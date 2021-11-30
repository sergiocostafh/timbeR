#' Estimate the diameter at a given height based on a fitted Kozak (2004) taper equation.
#'
#' Estimates the diameter at a given height of a tree from the diameter at breast height, total height and the coefficients of the Kozak (2004) taper function.
#'
#' @param dbh tree diameter at breast height, in centimeters.
#' @param h total tree height, in meters.
#' @param hi height at which the diameter will be calculated, in meters.
#' @param coef numerical vector containing nine coefficients of the Kozak taper function.
#' @param p numerical value representing the first inflection point calculated in the segmented model of Max and Burkhart (1976).
#'
#' @details the Kozak (2004) variable-form taper function is represented mathematically by the following expression
#'
#' di ~b0*(dbh^b1)*(h^b2)*((1-(hi/h)^(1/4))/(1-(p^(1/3))))^(b3*(hi/h)^4+b4*(1/exp(dbh/h))+b5*((1-(hi/h)^(1/4))/(1-(p^(1/3))))^0.1+b6*(1/dbh)+b7*(h^(1-(hi/h)^(1/3)))+b8*((1-(hi/h)^(1/4))/(1-(p^(1/3)))))
#'
#' @return a numeric value indicating the diameter at the given height.
#'
#' @export
kozak_di <- function(dbh, h, hi, coef, p){
  b0 <- coef[[1]]; b1 <- coef[[2]]; b2 <- coef[[3]]; b3 <- coef[[4]]; b4 <- coef[[5]]; b5 <- coef[[6]]; b6 <- coef[[7]]; b7 <- coef[[8]]; b8 <- coef[[9]]; p <- p[[1]]
  b0*(dbh^b1)*(h^b2)*((1-(hi/h)^(1/4))/(1-(p^(1/3))))^(b3*(hi/h)^4+b4*(1/exp(dbh/h))+b5*((1-(hi/h)^(1/4))/(1-(p^(1/3))))^0.1+b6*(1/dbh)+b7*(h^(1-(hi/h)^(1/3)))+b8*((1-(hi/h)^(1/4))/(1-(p^(1/3)))))
}
