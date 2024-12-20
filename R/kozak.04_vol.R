#' Estimate the total or partial volume of the tree, based on a fitted Kozak (2004) taper function.
#'
#' Estimates the total or partial volume of the tree from the diameter at breast height, total height, initial section height, final section height and coefficients of the Kozak (2004) taper equation.
#'
#' @param dbh tree diameter at breast height, in centimeters.
#' @param h total tree height, in meters.
#' @param coef numerical vector containing eight coefficients of the Kozak taper equation.
#' @param p numerical value representing the first inflection point calculated in the segmented model of Max and Burkhart (1976).
#' @param hi final height of the tree section whose volume will be calculated, in meters. Default is the total tree height (h).
#' @param h0 initial height of the tree section whose volume will be calculated, in meters. Default is 0 (ground height).
#'
#' @return a numeric value indicating the total or partial volume of the tree.
#'
#' @details the Kozak (2004) variable-form taper function is represented mathematically by the following expression
#'
#' di ~ b0*(dbh^b1)*(h^b2)*((1-(hi/h)^(1/4))/(1-(p^(1/3))))^(b3*(hi/h)^4+b4*(1/exp(dbh/h))+b5*((1-(hi/h)^(1/4))/(1-(p^(1/3))))^0.1+b6*(1/dbh)+b7*(h^(1-(hi/h)^(1/3)))+b8*((1-(hi/h)^(1/4))/(1-(p^(1/3)))))
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
#' kozak.04 <- nlsLM(di ~ taper_kozak.04(dbh, h, hih, b0, b1, b2, b3, b4, b5, b6, b7, b8, p),
#'                start=list(b0=1.00,b1=.97,b2=.03,b3=.49,b4=-
#'                             0.87,b5=0.50,b6=3.88,b7=0.03,b8=-0.19, p = .1),
#'                data = tree_scaling,
#'                control = nls.lm.control(maxiter = 1000, maxfev = 2000)
#' )
#'
#' coef_kozak.04 <- coef(kozak.04)[-10]
#' p_kozak.04 <- coef(kozak.04)[10]
#'
#' dbh <- 25
#' h <- 20
#'
#' kozak.04_vol(dbh, h, coef_kozak.04, p_kozak.04)
#'
#' hi = 15
#' h0 = .2
#'
#' kozak.04_vol(dbh, h, coef_kozak.04, p_kozak.04, hi, h0)
#'
#' @export
kozak.04_vol <- function(dbh, h, coef, p, hi, h0) {

  b0 <- coef[[1]]; b1 <- coef[[2]]; b2 <- coef[[3]]; b3 <- coef[[4]]; b4 <- coef[[5]]; b5 <- coef[[6]]; b6 <- coef[[7]]; b7 <- coef[[8]]; b8 <- coef[[9]]; p <- p[[1]]

  if (missing(hi)) {
    hi <- h
  }

  if (missing(h0)) {
    h0 <- 0
  }

  fkozak.04 <- function(dbh, h, hi, b0, b1, b2, b3, b4, b5, b6, b7, b8, p) {
    (pi/40000)*((
      b0*(dbh^b1)*(h^b2)*((1-(hi/h)^(1/4))/(1-(p^(1/3))))^(b3*(hi/h)^4+b4*(1/exp(dbh/h))+b5*((1-(hi/h)^(1/4))/(1-(p^(1/3))))^0.1+b6*(1/dbh)+b7*(h^(1-(hi/h)^(1/3)))+b8*((1-(hi/h)^(1/4))/(1-(p^(1/3)))))
    )^2)
  }


  vkozak.04 <- function(dbh, h, hi, h0, b0, b1, b2, b3, b4, b5, b6, b7, b8, p) {
    stats::integrate(fkozak.04, lower=h0, upper=hi,
              dbh=dbh,
              h=h,
              b0=b0,b1=b1,b2=b2,b3=b3,b4=b4,b5=b5,b6=b6,b7=b7,b8=b8,p=p)$val
  }

  vkozak.04(dbh, h, hi, h0, b0, b1, b2, b3, b4, b5, b6, b7, b8, p)

}
