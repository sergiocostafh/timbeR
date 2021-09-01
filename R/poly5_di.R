#' Estimate the diameter at a given height based on a 5th degree polynomial function.
#'
#' Estimates the diameter at a given height of a tree from the diameter at breast height, total height and the coefficients of the 5th degree polynomial function that describes the tree's taper.
#'
#' @param dbh tree diameter at breast height, in centimeters.
#' @param h total tree height, in meters.
#' @param hi height at which the diameter will be calculated, in meters.
#' @param coef numerical vector containing six coefficients of the 5th degree polynomial function that describes the tree's taper.
#'
#' @return a numeric value indicating the diameter at the given height.
#'
#' @export
poly5_di <- function(dbh, h, hi, coef){
  b0 <- coef[1]; b1 <- coef[2]; b2 <- coef[3]; b3 <- coef[4]; b4 <- coef[5]; b5 <- coef[6]
  hih <- hi/h
  (b0+b1*hih+b2*hih^2+b3*hih^3+b4*hih^4+b5*hih^5)*dbh
}



