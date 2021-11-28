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
#' di ~b0*(dbh^b1)*(h^b2)*((1-hih^(1/4))/(1-(p^(1/3))))^(b3*hih^4+b4*(1/exp(dbh/h))+b5*((1-hih^(1/4))/(1-(p^(1/3))))^0.1+b6*(1/dbh)+b7*(h^(1-hih^(1/3)))+b8*((1-hih^(1/4))/(1-(p^(1/3)))))
#'
#' @export
kozak_vol <- function(dbh, h, coef, p, hi, h0) {

  b0 <- coef[1]; b1 <- coef[2]; b2 <- coef[3]; b3 <- coef[4]; b4 <- coef[5]; b5 <- coef[6]; b6 <- coef[7]; b7 <- coef[8]; b8 <- coef[9]

  if (missing(hi)) {
    hi <- h
  }

  if (missing(h0)) {
    h0 <- 0
  }

  fkozak <- function(dbh, h, hi, b0, b1, b2, b3, b4, b5, b6, b7, b8, p) {
    (pi/40000)*((
      b0*(dbh^b1)*(h^b2)*((1-hih^(1/4))/(1-(p^(1/3))))^(b3*hih^4+b4*(1/exp(dbh/h))+b5*((1-hih^(1/4))/(1-(p^(1/3))))^0.1+b6*(1/dbh)+b7*(h^(1-hih^(1/3)))+b8*((1-hih^(1/4))/(1-(p^(1/3)))))
    )^2)
  }


  vkozak <- function(dbh, h, hi, h0, b0, b1, b2, b3, b4, b5, b6, b7, b8, p) {
    integrate(fkozak, lower=h0, upper=hi,
              dbh=dbh,
              h=h,
              b0=b0,b1=b1,b2=b2,b3=b3,b4=b4,b5=b5,b6=b6,b7=b7,b8=b8,p=p)$val
  }

  vkozak(dbh, h, hi, h0, b0, b1, b2, b3, b4, b5, b6, b7, b8, p)

}
