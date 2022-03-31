#' Estimate the diameter at a given height based on a fitted Bi (2000) taper equation.
#'
#' Estimates the diameter at a given height of a tree from the diameter at breast height, total height and the coefficients of the Bi taper function.
#'
#' @param dbh tree diameter at breast height, in centimeters.
#' @param h total tree height, in meters.
#' @param hi height at which the diameter will be calculated, in meters.
#' @param coef numerical vector containing seven coefficients of the Bi taper function.
#'
#' @return a numeric value indicating the diameter at the given height.
#'
#' @details the Bi (2000) variable-form taper function is represented mathematically by the following expression
#'
#' di ~ dbh * (log(sin((pi/2) * (hi/h)))/(log(sin((pi/2) * (1.3/h)))))^(b0 + b1 * sin((pi/2) * (hi/h)) + b2 * cos((3 * pi/2) * (hi/h)) + b3 * (sin((pi/2) * (hi/h))/(hi/h)) + b4 * dbh + b5 * (hi/h) * dbh^0.5 + b6 * (hi/h) * h^0.5)
#'
#' @examples
#'
#' library(dplyr)
#' library(minpack.lm)
#' library(timbeR)
#'
#' tree_scaling <- tree_scaling %>%
#' mutate(did = di/dbh,
#'        hih = hi/h)
#'
#' bi <-  nlsLM(di ~ taper_bi(dbh, h, hih, b0, b1, b2, b3, b4, b5, b6),
#' data=tree_scaling,
#' start=list(b0=1.8,b1=-0.2,b2=-0.04,b3=-0.9,b4=-0.0006,b5=0.07,b6=-.14))
#'
#' coef_bi <- coef(bi)
#'
#' dbh <- 25
#' h <- 20
#' hi <- 15
#'
#' bi_di(dbh, h, hi, coef_bi)
#'
#' @export
bi_di <- function(dbh, h, hi, coef){
  b0 <- coef[[1]]; b1 <- coef[[2]]; b2 <- coef[[3]]; b3 <- coef[[4]]; b4 <- coef[[5]]; b5 <- coef[[6]]; b6 <- coef[[7]]
  dbh * (log(sin((pi/2) * (hi/h)))/(log(sin((pi/2) * (1.3/h)))))^(b0 + b1 * sin((pi/2) * (hi/h)) + b2 * cos((3 * pi/2) * (hi/h)) + b3 * (sin((pi/2) * (hi/h))/(hi/h)) + b4 * dbh + b5 * (hi/h) * dbh^0.5 + b6 * (hi/h) * h^0.5)
}
