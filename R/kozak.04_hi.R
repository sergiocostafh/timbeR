#' Estimate the height at which a given diameter occurs in a tree, based on a fitted Kozak (2004) taper equation.
#'
#' Estimates the height at which a given diameter occurs in a tree, from the diameter at breast height, total height and coefficients of the Kozak (2004) taper function.
#'
#' @param dbh tree diameter at breast height, in centimeters.
#' @param h total tree height, in meters.
#' @param di diameter whose height of occurrence will be estimated, in centimeters.
#' @param coef numerical vector containing nine coefficients of the Kozak taper equation
#' @param p numerical value representing the first inflection point calculated in the segmented model of Max and Burkhart (1976).
#'
#' @return as numeric value indicating the height at which the given diameter occurs.
#'
#' @details the Kozak (2004) variable-form taper function is represented mathematically by the following expression
#'
#' di ~b0*(dbh^b1)*(h^b2)*((1-(hi/h)^(1/4))/(1-(p^(1/3))))^(b3*(hi/h)^4+b4*(1/exp(dbh/h))+b5*((1-(hi/h)^(1/4))/(1-(p^(1/3))))^0.1+b6*(1/dbh)+b7*(h^(1-(hi/h)^(1/3)))+b8*((1-(hi/h)^(1/4))/(1-(p^(1/3)))))
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
#' h <- 20
#' dbh <- 25
#' hi <- 15
#'
#' kozak.04_hi(dbh, h, hi, coef_kozak.04, p_kozak.04)
#'
#' @export
kozak.04_hi <- function(dbh, h, di, coef, p){
  b0 <- coef[1]; b1 <- coef[2]; b2 <- coef[3]; b3 <- coef[4]; b4 <- coef[5]; b5 <- coef[6]; b6 <- coef[7]; b7 <- coef[8]; b8 <- coef[9]
  fun_opt_hi <- function(hi){
    (b0*(dbh^b1)*(h^b2)*((1-(hi/h)^(1/4))/(1-(p^(1/3))))^(b3*(hi/h)^4+b4*(1/exp(dbh/h))+b5*((1-(hi/h)^(1/4))/(1-(p^(1/3))))^0.1+b6*(1/dbh)+b7*(h^(1-(hi/h)^(1/3)))+b8*((1-(hi/h)^(1/4))/(1-(p^(1/3))))) - di)^2
  }

  stats::optimise(fun_opt_hi,lower=0,upper=h,tol=0.0001)$minimum
}


