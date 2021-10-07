open Core_kernel
open Gsl

let sum a b ~f =
  let rec loop i acc =
    if i > b then acc
    else loop (i + 1) (acc +. f i)
  in
  loop a 0.

let logit p = Float.log (p /. (1. -. p))

let sigmoid x =
  let exp_x = Float.exp x in
  exp_x /. (1. +. exp_x)

type dataset = Dataset of (float * bool) list

let confusion_matrix_fold (Dataset d) ~init ~f =
  let xs = Array.of_list d in
  Array.sort xs ~compare:(fun (x, _) (y, _) -> Float.compare y x) ;
  let n = Array.length xs in
  let npos = Array.count xs ~f:snd in
  let nneg = n - npos in
  let rec loop i acc ~tp ~fp ~fn ~tn =
    if i <= n then
      let score, b = xs.(i - 1) in
      let tp = if b then tp + 1 else tp
      and fp = if b then fp else fp + 1
      and fn = if b then fn - 1 else fn
      and tn = if b then tn else tn - 1 in
      let acc =
        if i = n || Float.(score <> fst xs.(i)) then
          f ~tp ~fp ~fn ~tn ~score acc
        else acc
      in
      loop (i + 1) acc ~tp ~tn ~fp ~fn
    else acc
  in
  let tp = 0 and tn = nneg and fp = 0 and fn = npos in
  loop 1 (f ~tp ~fp ~fn ~tn ~score:Float.infinity init) ~tp ~fp ~tn ~fn

type confusion_matrix = {
  tp : int ;
  tn : int ;
  fp : int ;
  fn : int ;
}
[@@deriving sexp]

type confusion_matrix_series = (float * confusion_matrix) array
[@@deriving sexp]

let confusion_matrix_series d =
  confusion_matrix_fold d ~init:[] ~f:(fun ~tp ~fp ~fn ~tn ~score acc ->
      (score, { tp ; tn ; fp ; fn }) :: acc
    )
  |> List.rev
  |> Array.of_list

let%expect_test "performance curve 1" =
  let scores = [2.1 ; 1.2 ; 5.6 ; 0. ; 5.6] in
  let labels = [true ; false ; true ; false ; false] in
  let dataset = Dataset (List.zip_exn scores labels) in
  let series = confusion_matrix_series dataset in
  print_endline (Sexp.to_string_hum (sexp_of_confusion_matrix_series series)) ;
  [%expect "
    ((INF ((tp 0) (tn 3) (fp 0) (fn 2))) (5.6 ((tp 1) (tn 2) (fp 1) (fn 1)))
     (2.1 ((tp 2) (tn 2) (fp 1) (fn 0))) (1.2 ((tp 2) (tn 1) (fp 2) (fn 0)))
     (0 ((tp 2) (tn 0) (fp 3) (fn 0))))"]

let recall ~tp ~fn = float tp /. float (tp + fn)
let precision ~tp ~fp =
  if tp + fp = 0 then 1.
  else float tp /. float (tp + fp)

let operating_points d =
  confusion_matrix_fold d ~init:[] ~f:(fun ~tp ~fp ~fn ~tn:_ ~score acc ->
      (score, recall ~tp ~fn, precision ~tp ~fp) :: acc
    )
  |> List.rev

let auc_trapezoidal_lt d =
  let points = operating_points d in
  let n, r, pmin, pmax =
    let data =
      List.group points ~break:(fun (_, r1, _) (_, r2, _) ->
          Float.(r1 <> r2)
        )
      |> List.map ~f:(function
          | [] -> assert false
          | (_, r, _) :: _ as xs -> r, List.map xs ~f:trd3
        )
      |> Array.of_list
    in
    Array.length data,
    Array.map data ~f:fst,
    Array.map data ~f:(fun (_, ps) -> List.reduce_exn ps ~f:Float.min),
    Array.map data ~f:(fun (_, ps) -> List.reduce_exn ps ~f:Float.max)
  in
  sum 0 (n - 2) ~f:(fun i -> (pmin.(i) +. pmax.(i + 1)) /. 2. *. (r.(i + 1) -. r.(i)))

let decreasing_ties_groups (Dataset xs) =
  let rec loop closed_groups current_group points =
    match points, current_group with
    | [], None -> []
    | [], Some (s, npos, nneg) ->
      (s, npos, nneg) :: closed_groups
    | (s, b) :: t, None ->
      let npos, nneg = if b then 1, 0 else 0, 1 in
      loop closed_groups (Some (s, npos, nneg)) t
    | (s', b) :: t, Some (s, npos, nneg) ->
      if Float.equal s s' then
        let npos, nneg = if b then npos + 1, nneg else npos, nneg + 1 in
        loop closed_groups (Some (s, npos, nneg)) t
      else
        let npos', nneg' = if b then 1, 0 else 0, 1 in
        loop ((s, npos, nneg) :: closed_groups) (Some (s', npos', nneg')) t
  in
  let compare = Tuple.T2.compare ~cmp1:Float.ascending ~cmp2:Bool.ascending in
  List.sort xs ~compare
  |> loop [] None

let decreasing_ties_groups_test d =
  let groups = decreasing_ties_groups d in
  Sexplib.Sexp.pp Format.std_formatter ([%sexp_of: (float * int * int) list] groups)

let%expect_test "decreasing_ties_groups" =
  let data = Dataset [0., false; 1., false; 2., true; 0., false; 1., false; 2., true; 0., true; 1., false; 2., true; 0., true] in
  decreasing_ties_groups_test data ;
  [%expect "((2 3 0)(1 0 3)(0 2 2))"]

let%expect_test "decreasing_ties_groups 2" =
  let data = Dataset [(1.0070885317, true); (0.297475057516, false); (0.831050790341, false)] in
  decreasing_ties_groups_test data ;
  [%expect "((1.0070885317 1 0)(0.831050790341 0 1)(0.297475057516 0 1))"]

let auc_average_precision (Dataset xs as d) =
  if List.is_empty xs then invalid_arg "average_precision estimator undefined on empty lists" ;
  let npos, _, sum =
    decreasing_ties_groups d
    |> List.fold ~init:(0, 0, 0.) ~f:(fun (npos, nneg, sum) (_, p, n) ->
        let npos = npos + p and nneg = nneg + n in
        let prec = float npos /. float (npos + nneg) in
        let sum = float p *. prec +. sum in
        npos, nneg, sum
      )
  in
  sum /. float npos

let logit_confidence_interval ~alpha ~theta_hat ~n =
  let eta_hat = logit theta_hat in
  let tau_hat = (float n *. theta_hat *. (1. -. theta_hat)) ** (-0.5) in
  let delta = tau_hat *. Cdf.gaussian_Pinv ~sigma:1. ~p:(1. -. alpha /. 2.) in
  sigmoid (eta_hat -. delta),
  sigmoid (eta_hat +. delta)

module Binormal_model = struct
  type t = {
    mu_pos : float ;
    sigma_pos : float ;
    mu_neg : float ;
    sigma_neg : float ;
    alpha : float ;
  }

  let make ?(mu_pos = 1.) ?(sigma_pos = 1.) ?(mu_neg = 0.) ?(sigma_neg = 1.) alpha =
    { mu_pos ; mu_neg ; sigma_pos ; sigma_neg ; alpha }

  let simulation rng ~n { mu_pos ; mu_neg ; sigma_pos ; sigma_neg ; alpha } =
    let sample = List.init n ~f:(fun _ ->
        let mu, sigma, label = match Randist.bernoulli rng ~p:alpha with
          | 0 -> mu_neg, sigma_neg, false
          | 1 -> mu_pos, sigma_pos, true
          | _ -> assert false
        in
        mu +. Randist.gaussian rng ~sigma, label
      )
    in
    Dataset sample

  let phi ~mu ~sigma x = Cdf.gaussian_P ~x:(x -. mu) ~sigma
  let inv_phi ~mu ~sigma p = Cdf.gaussian_Pinv ~p ~sigma +. mu

  let precision p recall =
    let phi_neg = phi ~mu:p.mu_neg ~sigma:p.sigma_neg in
    let inv_phi_pos = inv_phi ~mu:p.mu_pos ~sigma:p.sigma_pos in
    let alpha = p.alpha in
    alpha *. recall
    /.
    (alpha *. recall
     +.
     (1. -. alpha) *. (1. -. phi_neg (inv_phi_pos (1. -. recall))))

  let curve ?(n = 100) p =
    let max_i = float (n - 1) in
    Array.init n ~f:(fun i ->
        let r = float i /. max_i in
        r, precision p r
      )

  let estimate (Dataset xs) =
    let select label =
      List.filter_map xs ~f:(fun (x, b) ->
          if Bool.(b = label) then Some x else None
        )
      |> Array.of_list
    in
    let x = select false in
    let y = select true in
    let len t = float (Array.length t) in
    Stats.{
      mu_pos = mean y ;
      mu_neg = mean x ;
      sigma_pos = sd y ;
      sigma_neg = sd x ;
      alpha = len y /. (len x +. len y) ;
    }

  let auc p =
    let n = 1_000 in
    let x = Array.init n ~f:(fun i -> float (i + 1) /. float n) in
    let y = Array.map x ~f:(precision p) in
    Stats.mean y
end
