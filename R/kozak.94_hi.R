#' Estimate the height at which a given diameter occurs in a tree, based on a fitted Kozak (1994) taper equation.
#'
#' Estimates the height at which a given diameter occurs in a tree, from the diameter at breast height, total height and coefficients of the Kozak (1994) taper function.
#'
#' @param dbh tree diameter at breast height, in centimeters.
#' @param h total tree height, in meters.
#' @param di diameter whose height of occurrence will be estimated, in centimeters.
#' @param coef numerical vector containing nine coefficients of the Kozak (1994) taper function.
#' @param p numerical value representing the first inflection point calculated in the segmented model of Max and Burkhart (1976).
#'
#' @return a numeric value indicating the height at which the given diameter occurs.
#'
#' @details the Kozak (1994) variable-form taper function is represented mathematically by the following expression
#'
#' b0*(dbh**b1)*(b2**dbh)*((1-(hi/h)^0.5)/(1-p^0.5))^(b3+b4*(hi/h)^(1/4)+b5*((hi/h))^(1/3)+b6*((hi/h)^0.5)+b7*asin(1-(hi/h)^0.5)+b8*(1/((dbh/h)+1))+b9*h)
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
#' di <- 5
#'
#' kozak.94_hi(dbh, h, di, coef_kozak.94, p_kozak.94)
#'
#' @export
kozak.94_hi <- function(dbh, h, di, coef, p){
  b0 <- coef[[1]]; b1 <- coef[[2]]; b2 <- coef[[3]]; b3 <- coef[[4]]; b4 <- coef[[5]]; b5 <- coef[[6]]; b6 <- coef[[7]]; b7 <- coef[[8]]; b8 <- coef[[9]]; b9 <- coef[[10]]; p <- p[[1]]
  fun_opt_hi <- function(hi){
    (b0*(dbh**b1)*(b2**dbh)*((1-(hi/h)^0.5)/(1-p^0.5))^(b3+b4*(hi/h)^(1/4)+b5*((hi/h))^(1/3)+b6*((hi/h)^0.5)+b7*asin(1-(hi/h)^0.5)+b8*(1/((dbh/h)+1))+b9*h) - di)^2
  }

  stats::optimise(fun_opt_hi,lower=0,upper=h,tol=0.0001)$minimum
}













