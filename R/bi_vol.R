#' Estimate the total or partial volume of the tree, based on a fitted Bi taper function.
#'
#' Estimates the total or partial volume of the tree from the diameter at breast height, total height, initial section height, final section height and coefficients of the Bi taper equation.
#'
#' @param dbh tree diameter at breast height, in centimeters.
#' @param h total tree height, in meters.
#' @param coef numerical vector containing seven coefficients of the Bi taper equation.
#' @param hi final height of the tree section whose volume will be calculated, in meters. Default is the total tree height (h).
#' @param h0 initial height of the tree section whose volume will be calculated, in meters. Default is 0 (ground height).
#'
#' @return a numeric value indicating the total or partial volume of the tree.
#'
#' @details the Bi variable-form taper function is represented mathematically by the following expression
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
#' bi <-  nlsLM(di ~ dbh*(log(sin((pi/2)*(hih)))/(log(sin((pi/2)*(1.3/h)))))**
#' (b0+b1*sin((pi/2)*(hih))+b2*cos((3*pi/2)*(hih))+b3*(sin((pi/2)*(hih))/(hih))+
#'   b4*dbh+b5*(hih)*dbh**0.5+b6*(hih)*h**0.5),
#' data=tree_scaling,
#' start=list(b0=1.8,b1=-0.2,b2=-0.04,b3=-0.9,b4=-0.0006,b5=0.07,b6=-.14))
#'
#' coef_bi <- coef(bi)
#'
#' dbh <- 25
#' h <- 20
#'
#' bi_vol(dbh, h, coef_bi)
#'
#' hi = 15
#' h0 = .2
#'
#' bi_vol(dbh, h, coef_bi, hi, h0)
#'
#' @export
bi_vol <- function(dbh, h, coef, hi, h0) {

  b0 <- coef[[1]]; b1 <- coef[[2]]; b2 <- coef[[3]]; b3 <- coef[[4]]; b4 <- coef[[5]]; b5 <- coef[[6]]; b6 <- coef[[7]]

  if (missing(hi)) {
    hi <- h
  }

  if (missing(h0)) {
    h0 <- 0
  }

  fbi <- function(dbh, h, hi, b0, b1, b2, b3, b4, b5, b6) {
    (pi/40000)*((
      dbh * (log(sin((pi/2) * (hi/h)))/(log(sin((pi/2) * (1.3/h)))))^(b0 + b1 * sin((pi/2) * (hi/h)) + b2 * cos((3 * pi/2) * (hi/h)) + b3 * (sin((pi/2) * (hi/h))/(hi/h)) + b4 * dbh + b5 * (hi/h) * dbh^0.5 + b6 * (hi/h) * h^0.5)
    )^2)
  }


  vbi <- function(dbh, h, hi, h0, b0, b1, b2, b3, b4, b5, b6) {
    stats::integrate(fbi, lower=h0, upper=hi,
              dbh=dbh,
              h=h,
              b0=b0,b1=b1,b2=b2,b3=b3,b4=b4,b5=b5,b6=b6)$val
  }

  vbi(dbh, h, hi, h0, b0, b1, b2, b3, b4, b5, b6)

}
