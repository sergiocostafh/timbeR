#' Estimate the height at which a given diameter occurs in a tree, based on a fitted Bi taper equation.
#'
#' Estimates the height at which a given diameter occurs in a tree, from the diameter at breast height, total height and coefficients of the Bi taper function.
#'
#' @param dbh tree diameter at breast height, in centimeters.
#' @param h total tree height, in meters.
#' @param di diameter whose height of occurrence will be estimated, in centimeters.
#' @param coef numerical vector containing seven coefficients of the Bi taper equation
#'
#' @return as numeric value indicating the height at which the given diameter occurs.
#'
#' @details the Bi variable-form taper function is represented mathematically by the following expression
#'
#' di ~ dbh * (log(sin((pi/2) * (hi/h)))/(log(sin((pi/2) * (1.3/h)))))^(b0 + b1 * sin((pi/2) * (hi/h)) + b2 * cos((3 * pi/2) * (hi/h)) + b3 * (sin((pi/2) * (hi/h))/(hi/h)) + b4 * dbh + b5 * (hi/h) * dbh^0.5 + b6 * (hi/h) * h^0.5)
#'
#' @export
bi_hi <- function(dbh, h, di, coef){
  b0 <- coef[1]; b1 <- coef[2]; b2 <- coef[3]; b3 <- coef[4]; b4 <- coef[5]; b5 <- coef[6]; b6 <- coef[7]
  fun_opt_hi <- function(hi){
    (dbh * (log(sin((pi/2) * (hi/h)))/(log(sin((pi/2) * (1.3/h)))))^(b0 + b1 * sin((pi/2) * (hi/h)) + b2 * cos((3 * pi/2) * (hi/h)) + b3 * (sin((pi/2) * (hi/h))/(hi/h)) + b4 * dbh + b5 * (hi/h) * dbh^0.5 + b6 * (hi/h) * h^0.5) - di)^2
  }

  stats::optimise(fun_opt_hi,lower=0,upper=h,tol=0.0001)$minimum
}


