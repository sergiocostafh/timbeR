#' Estimate the total or partial volume of the tree, based on a fitted Kozak (1994) taper function.
#'
#' Estimates the total or partial volume of the tree from the diameter at breast height, total height, initial section height, final section height and coefficients of the Kozak (1994) taper equation.
#'
#' @param dbh tree diameter at breast height, in centimeters.
#' @param h total tree height, in meters.
#' @param coef numerical vector containing nine coefficients of the Kozak (1994) taper function.
#' @param p numerical value representing the first inflection point calculated in the segmented model of Max and Burkhart (1976).
#' @param hi final height of the tree section whose volume will be calculated, in meters. Default is the total tree height (h).
#' @param h0 initial height of the tree section whose volume will be calculated, in meters. Default is 0 (ground height).
#'
#' @return a numeric value indicating the total or partial volume of the tree.
#'
#' @details the Kozak (1994) variable-form taper function is represented mathematically by the following expression
#'
#' di ~ b0*(dbh**b1)*(b2**dbh)*((1-(hi/h)^0.5)/(1-p^0.5))^(b3+b4*(hi/h)^(1/4)+b5*((hi/h))^(1/3)+b6*((hi/h)^0.5)+b7*asin(1-(hi/h)^0.5)+b8*(1/((dbh/h)+1))+b9*h)
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
#' kozak.94 <-  nlsLM(di ~ taper_kozak.94(dbh, h, hih, b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, p),
#' data=tree_scaling,
#' start=list(b0=1.5,b1=1.5,b2=1,b3=-10,b4=50,b5=-10,b6=34,b7=10,b8=-1,b9=-0.01, p = .1))
#'
#' coef_kozak.94 <- coef(kozak.94)[-11]
#' p_kozak.94 <- coef(kozak.94)[11]
#'
#' dbh <- 25
#' h <- 20
#'
#' kozak.94_vol(dbh, h, coef_kozak.94, p_kozak.94)
#'
#' hi = 15
#' h0 = .2
#'
#' kozak.94_vol(dbh, h, coef_kozak.94, p_kozak.94, hi, h0)
#'
#' @export
kozak.94_vol <- function(dbh, h, coef, p, hi, h0) {

  b0 <- coef[[1]]; b1 <- coef[[2]]; b2 <- coef[[3]]; b3 <- coef[[4]]; b4 <- coef[[5]]; b5 <- coef[[6]]; b6 <- coef[[7]]; b7 <- coef[[8]]; b8 <- coef[[9]]; b9 <- coef[[10]]; p <- p[[1]]

  if (missing(hi)) {
    hi <- h
  }

  if (missing(h0)) {
    h0 <- 0
  }

  fkozak.94 <- function(dbh, h, hi, b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, p) {
    (pi/40000)*((
      b0*(dbh**b1)*(b2**dbh)*((1-(hi/h)^0.5)/(1-p^0.5))^(b3+b4*(hi/h)^(1/4)+b5*((hi/h))^(1/3)+b6*((hi/h)^0.5)+b7*asin(1-(hi/h)^0.5)+b8*(1/((dbh/h)+1))+b9*h)
    )^2)
  }

  vkozak.94 <- function(dbh, h, hi, h0, b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, p) {
    stats::integrate(fkozak.94, lower=h0, upper=hi,
                     dbh=dbh,
                     h=h,
                     b0=b0, b1=b1, b2=b2, b3=b3, b4=b4, b5=b5, b6=b6, b7=b7, b8=b8, b9=b9, p=p)$val
  }
  vkozak.94(dbh, h, hi, h0, b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, p)

}

