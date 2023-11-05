#' Weighting functions
#' @export
weight_DR_X <- function(.pred_treatment, .pred_control) {
  .pred_treatment * .pred_control
}

#' @export
#' @rdname weight_DR_X
weight_U_X <- function(treatment,
                       .pred_treatment,
                       .pred_control) {
  (.pred_treatment * .pred_control)^2 /
    (.pred_treatment^3 + .pred_control^3)
}

#' @export
#' @rdname weight_DR_X
weight_DR_AX <- function(treatment,
                         .pred_treatment,
                         .pred_control) {
  ifelse(treatment == 1,
    .pred_treatment^2,
    .pred_control^2
  )
}

#' @export
#' @rdname weight_DR_X
weight_DR_alt_AX <- function(treatment,
                         .pred_treatment,
                         .pred_control) {
  ifelse(treatment == 1,
         .pred_treatment,
         .pred_control
  )
}

#' @export
#' @rdname weight_DR_X
weight_U_AX <- function(treatment,
                        .pred_treatment) {
  trt_num <- as.numeric(treatment == 1)
  (trt_num - .pred_treatment)^2
}

#' @export
#' @rdname weight_DR_X
weight_1 <- function(.pred_treatment) {
  rep(1, length(.pred_treatment))
}
