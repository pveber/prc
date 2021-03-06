open Core_kernel
open Prc
open Gsl

module Plot = struct
  type t = {
    x : float array ;
    y : float array ;
    col : string option ;
    ty : [`Lines of int | `Points of int] ;
    label : string option ;
  }

  let lines ?col ?(lwd = 1) ?label points = {
    x = Array.map points ~f:fst ;
    y = Array.map points ~f:snd ;
    col ; ty = `Lines lwd ; label ;
  }

  let points ?col ?(pch = 19) ?label points = {
    x = Array.map points ~f:fst ;
    y = Array.map points ~f:snd ;
    col ; ty = `Points pch ; label ;
  }

  let plot_curve { x ; y ; col ; ty ; _ } =
    match ty with
    | `Lines lwd ->
      OCamlR_graphics.lines ~lwd ?col ~x ~y ()
    | `Points pch ->
      OCamlR_graphics.points ~pch ?col ~x ~y ()

  let recall_precision_plot ?main curves =
    OCamlR_graphics.plot
      ~plot_type:`Lines ?main
      ~xlab:"Recall" ~ylab:"Precision"
      ~xlim:(0., 1.) ~ylim:(0., 1.) ~x:[||] ~y:[||] () ;
    List.(iter (rev curves)) ~f:plot_curve
end

let recall_precision_curve ?col ?lwd ?label dataset =
  Prc.operating_points dataset
  |> List.map ~f:(fun (_, recall, precision) -> recall, precision)
  |> Array.of_list
  |> Plot.lines ?col ?lwd ?label

let curve_variability ?(sample_size = 1000) () =
  let rng = Rng.(make (default ())) in
  let alpha = 0.1 in
  let p = Binormal_model.make alpha in
  let samples = List.init 20 ~f:(fun _ -> Binormal_model.simulation ~n:sample_size rng p) in
  let empirical_curves =
    List.map samples ~f:(recall_precision_curve ~col:"grey")
  in
  let true_curve =
    Binormal_model.curve ~n:100 p
    |> Plot.lines ~col:"red" ~label:"True RP curve"
  in
  Plot.recall_precision_plot (true_curve :: empirical_curves)

let estimator_sampling_distribution ?(sigma = 1.) ?(alpha = 0.1) ?(sample_size = 30) () =
  let open OCamlR_base in
  let rng = Rng.(make (default ())) in
  let p = Binormal_model.make ~sigma_pos:sigma ~sigma_neg:sigma alpha in
  let true_auc = Binormal_model.auc p in
  let samples = List.init 1_000 ~f:(fun _ -> Binormal_model.simulation ~n:sample_size rng p) in
  let compute f =
    List.map samples ~f
    |> Numeric.of_list
    |> Numeric.to_sexp
  in
  let trapezoidal_lt = compute auc_trapezoidal_lt in
  let average_precision = compute auc_average_precision in
  let binormal = compute (fun sample -> Binormal_model.auc (Binormal_model.estimate sample)) in
  let l = List_.create [
      Some "binormal", binormal ;
      Some "trapezoidal", trapezoidal_lt ;
      Some "average_precision", average_precision ;
    ]
  in
  OCamlR_graphics.list_boxplot l ;
  OCamlR_graphics.abline ~h:true_auc ~lty:`dashed ~col:"red" ~lwd:2 ()

let confidence_interval_coverage meth sample_size =
  let rng = Rng.(make (default ())) in
  let model = Binormal_model.make 0.1 in
  let alpha = 0.05 in
  let true_auc = Binormal_model.auc model in
  let confidence_interval dataset = match meth with
    | `Logit ->
      let auc_estimate = auc_average_precision dataset in
      let n_pos = n_pos dataset in
      logit_confidence_interval ~alpha ~theta_hat:auc_estimate ~n_pos
    | `Bootstrap ->
      bootstrap_confidence_interval ~alpha Rng.(make (default ())) dataset ~f:auc_average_precision
  in
  let iteration _ =
    let dataset = Binormal_model.simulation rng ~n:sample_size model in
    let (lb, ub) = confidence_interval dataset in
    Float.(lb < true_auc && true_auc < ub)
  in
  Array.init 100 ~f:iteration
  |> Array.count ~f:Fn.id
