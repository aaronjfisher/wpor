#' Pseudo outcome functions
#' @export
pseudo_DR_single <- function(outcome,
                             treatment,
                             .pred_outcome_1_single,
                             .pred_outcome_0_single,
                             .pred_treatment,
                             .pred_control) {
  trt_num <- as.numeric(treatment == 1)

  pseudo <-
    (trt_num / .pred_treatment) * (outcome - .pred_outcome_1_single) -
    ((1 - trt_num) / .pred_control) * (outcome - .pred_outcome_0_single) +
    .pred_outcome_1_single - .pred_outcome_0_single

  pseudo
}

#' @export
#' @rdname pseudo_DR_single
pseudo_DR_separate <- function(outcome,
                               treatment,
                               .pred_outcome_1_separate,
                               .pred_outcome_0_separate,
                               .pred_treatment,
                               .pred_control) {
  trt_num <- as.numeric(treatment == 1)

  pseudo <-
    (trt_num / .pred_treatment) * (outcome - .pred_outcome_1_separate) -
    ((1 - trt_num) / .pred_control) * (outcome - .pred_outcome_0_separate) +
    .pred_outcome_1_separate - .pred_outcome_0_separate

  pseudo
}

#' @export
#' @rdname pseudo_DR_single
pseudo_plugin <- function(.pred_outcome_1_single, .pred_outcome_0_single) {
  .pred_outcome_1_single - .pred_outcome_0_single
}

#' @export
#' @rdname pseudo_DR_single
pseudo_U <- function(outcome,
                     treatment,
                     .pred_outcome_marginal,
                     .pred_treatment) {
  trt_num <- as.numeric(treatment == 1)

  pseudo <-
    (outcome - .pred_outcome_marginal) /
      (trt_num - .pred_treatment)

  pseudo
}

#' @export
#' @rdname pseudo_DR_single
pseudo_cov <- function(outcome,
                       treatment,
                       .pred_outcome_marginal,
                       .pred_treatment,
                       .pred_control) {
  trt_num <- as.numeric(treatment == 1)

  pseudo <-
    (outcome - .pred_outcome_marginal) *
      (trt_num - .pred_treatment) /
      (.pred_treatment * .pred_control)

  pseudo
}
