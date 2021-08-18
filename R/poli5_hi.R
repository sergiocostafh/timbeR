#' Estimate the height at which a given diameter occurs in a tree, based on a 5th degree polynomial function.
#'
#' Estimates the height at which a given diameter occurs in a tree, from the diameter at breast height, total height and coefficients of the 5th degree polynomial function that describes the tree's taper.
#'
#' @param dbh tree diameter at breast height, in centimeters.
#' @param h total tree height, in meters.
#' @param di diameter whose height of occurrence will be estimated, in centimeters.
#' @param coef numerical vector containing six coefficients of the 5th degree polynomial function that describes the tree's taper.
#'
#' @return as numeric value indicating the height at which the given diameter occurs.
#'
#' @export
poli5_hi <- function(dbh, h, di, coef){
  b0 <- coef[1]; b1 <- coef[2]; b2 <- coef[3]; b3 <- coef[4]; b4 <- coef[5]; b5 <- coef[6]
  fun_opt_hi <- function(hi){
    ((b0+b1*(hi/h)+b2*(hi/h)^2+b3*(hi/h)^3+b4*(hi/h)^4+b5*(hi/h)^5)-(di/dbh))^2
  }

  stats::optimise(fun_opt_hi,lower=0,upper=h,tol=0.0001)$minimum
}


