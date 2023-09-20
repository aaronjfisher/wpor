#' Pseudo outcome functions
#' @export
pseudo_DR <- function(outcome,
                      treatment,
                      .pred_outcome_1,
                      .pred_outcome_0,
                      .pred_treatment,
                      .pred_control) {
  trt_num <- as.numeric(treatment == 1)

  pseudo <-
    (trt_num / .pred_treatment) * (outcome - .pred_outcome_1) -
    ((1 - trt_num) / .pred_control) * (outcome - .pred_outcome_0) +
    .pred_outcome_1 - .pred_outcome_0

  pseudo
}

#' @export
#' @rdname pseudo_DR
pseudo_plugin <- function(.pred_outcome_1, .pred_outcome_0) {
  .pred_outcome_1 - .pred_outcome_0
}

#' @export
#' @rdname pseudo_DR
pseudo_U <- function(outcome,
                     treatment,
                     .pred_outcome_obs,
                     .pred_treatment) {
  trt_num <- as.numeric(treatment == 1)

  pseudo <-
    (outcome - .pred_outcome_obs) /
      (trt_num - .pred_treatment)

  pseudo
}

#' @export
#' @rdname pseudo_DR
pseudo_cov <- function(outcome,
                       treatment,
                       .pred_outcome_obs,
                       .pred_treatment) {
  trt_num <- as.numeric(treatment == 1)

  pseudo <-
    (outcome - .pred_outcome_obs) *
      (trt_num - .pred_treatment)

  pseudo
}
