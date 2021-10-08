open Core_kernel
open Prc

let floats_are_close_enough x y = Float.(abs (x -. y) < 1e-6)

module Naive_implementation = struct
  let tp (Dataset xs) c = List.count xs ~f:(fun (x, b) -> b && Float.(x >= c))
  let _tn (Dataset xs) c = List.count xs ~f:(fun (x, b) -> not b && Float.(x < c))
  let fp (Dataset xs) c = List.count xs ~f:(fun (x, b) -> not b && Float.(x >= c))
  let fn (Dataset xs) c = List.count xs ~f:(fun (x, b) -> b && Float.(x < c))
  let _recall d c =
    let tp = tp d c in
    float tp /. float (tp + fn d c)
  let precision d c =
    let tp = tp d c in
    float tp /. float (tp + fp d c)
  let average_precision (Dataset xs as d) =
    if List.is_empty xs then invalid_arg "average_precision estimator undefined on empty lists" ;
    let ys = List.filter_map xs ~f:(fun (x, b) -> if b then Some x else None) in
    let n = List.length ys in
    List.fold ys ~init:0. ~f:(fun acc y -> precision d y +. acc)
    /. float n
end

module Sklearn_implementation = struct
  let average_precision (Dataset xs) =
    let y_score = List.map xs ~f:fst |> Array.of_list |> Np.Numpy.vectorf in
    let y_true = List.map xs ~f:(fun (_, b) -> Bool.to_int b) |> Array.of_list |> Np.Numpy.vectori in
    Sklearn.Metrics.average_precision_score ~y_score ~y_true ()
end

let rec binormal_generator ~n rng =
  let alpha = Gsl.Randist.flat rng ~a:0.1 ~b:1. in
  let model = Binormal_model.make alpha in
  let Dataset xs as d = Binormal_model.simulation rng ~n model in
  if List.exists xs ~f:snd then d
  else binormal_generator ~n rng

let saturated_binormal_generator ~n rng =
  let clip a b x = Float.(if x < a then a else if x > b then b else x) in
  let Dataset xs = binormal_generator ~n rng in
  Dataset (List.map xs ~f:(fun (x, b) -> clip (-0.5) 1.5 x, b))

let test_implementations ~gen ~n ~show ~equal f g () =
  let rng = Gsl.Rng.(make (default ())) in
  let rec loop i =
    if i < n then
      let x = gen rng in
      if not (equal (f x) (g x)) then (
        Alcotest.fail (sprintf "Implementations differ on %s" (show x))
      )
      else loop (i + 1)
  in
  loop 0

type dataset = Prc.dataset = Dataset of (float * bool) list
[@@deriving show]

let average_precision_test gen =
  test_implementations
    ~gen ~show:show_dataset ~n:100 ~equal:floats_are_close_enough
    auc_average_precision
    Naive_implementation.average_precision

let average_precision_sklearn_test gen =
  test_implementations
    ~gen ~show:show_dataset ~n:100 ~equal:floats_are_close_enough
    auc_average_precision
    Naive_implementation.average_precision

let average_precision_test_suite f = [
  "average_precision on binormal model 3", `Quick, f (binormal_generator ~n:3) ;
  "average_precision on binormal model 10", `Quick, f (binormal_generator ~n:10) ;
  "average_precision on binormal model 100", `Quick, f (binormal_generator ~n:100) ;
  "average_precision on saturated binormal model 3", `Quick, f (saturated_binormal_generator ~n:3) ;
  "average_precision on saturated binormal model 10", `Quick, f (saturated_binormal_generator ~n:10) ;
  "average_precision on saturated binormal model 100", `Quick, f (saturated_binormal_generator ~n:100) ;
]

let naive_implementation_suite = average_precision_test_suite average_precision_test
let sklearn_suite = average_precision_test_suite average_precision_sklearn_test

let () =
  Alcotest.run "prc" [
    "test-against-naive-implementation", naive_implementation_suite ;
    "test-against-sklearn", sklearn_suite ;
  ]
