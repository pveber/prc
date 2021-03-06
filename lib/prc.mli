(**
Precision-Recall curve construction and AUC computation

References:
[1] The binormal assumption on precision-recall curves.
    Kay H. Brodersen, Cheng Soon Ong, Klaas E. Stephan and Joachim M. Buhmann

[2] Area Under the Precision-Recall Curve: Point Estimates and Confidence Intervals.
    Kendrick Boyd, Kevin H. Eng and C. David Page

[3] Precision-Recall-Gain Curves: PR Analysis Done Right.
    Peter A. Flach and Meelis Kull

[4] The Relationship Between Precision-Recall and ROC Curves.
    Jesse Davis and Mark Goadrich

[5] Realisable Classifiers: Improving Operating Performance on Variable Cost Problems.
    M.J.J. Scott, M. Niranjan, R.W. Prager
*)

type dataset = Dataset of (float * bool) list
(** Binary prediction scores with associated labels *)

val n_pos : dataset -> int
(** Number of positive items in the dataset *)

val operating_points :
  dataset ->
  (float * float * float) list
(** [operating_points d] computes the list of score threshold, recall
    and precision triplets, sorted by decreasing threshold. *)

val auc_trapezoidal_lt : dataset -> float
(** AUC lower triangular estimator (see [2] for reference) *)

val auc_average_precision : dataset -> float
(** AUC average precision (see [2] for reference) *)

val logit_confidence_interval :
  alpha:float ->
  theta_hat:float ->
  n_pos:int ->
  float * float
(** [logit_confidence_interval ~alpha ~theta_hat ~n] computes an
   asymptotically valid confidence interval at level 1 - [alpha], when
   the estimate [theta_hat] was obtained from a sample with [n_pos]
   positive observations. *)

val bootstrap_confidence_interval :
  ?niter:int ->
  alpha:float ->
  Gsl.Rng.t ->
  dataset ->
  f:(dataset -> float) ->
  (float * float)
(** [bootstrap_confidence_interval ?niter ~alpha rng d ~f] computes a
   bootstrap confidence interval at level 1 - [alpha] for the values
   produces by [f], using [n_iter] bootstrap iterations. *)

(** Binormal model

   A Gaussian mixture model for which the precision-recall curve can
   be computed explicitly (see [1])
 *)
module Binormal_model : sig
  type t = {
    mu_pos : float ;
    sigma_pos : float ;
    mu_neg : float ;
    sigma_neg : float ;
    alpha : float ;
  }

  val make :
    ?mu_pos:float ->
    ?sigma_pos:float ->
    ?mu_neg:float ->
    ?sigma_neg:float ->
    float ->
    t
  (** [make ?mu_pos ?sigma_pos ?mu_neg ?sigma_neg alpha] is a
     representation of the mixture of two Gaussian distributions of
     means [mu_pos] and [mu_neg] resp. and standard deviation
     [sigma_pos] and [sigma_neg] resp., while [alpha] is the
     proportion of positive samples in the mixture. *)

  val simulation :
    Gsl.Rng.t ->
    n:int ->
    t ->
    dataset

  val curve :
    ?n:int ->
    t ->
    (float * float) array

  val estimate : dataset -> t

  val auc : t -> float
end
