#' Kozak (2004) Taper Function.
#'
#' @param dbh tree diameter at breast height, in centimeters.
#' @param h total tree height, in meters.
#' @param hih ratio between the height of the section (hi) and the total height (h) .
#' @param b0,b1,b2,b3,b4,b5,b6,b7,b8 model parameters.
#' @param p numerical value representing the first inflection point calculated in the segmented model of Max and Burkhart (1976).
#'
#' @return a numeric value indicating the diameter at the section.
#'
#' @references
#' Kozak, A. (2004). My last words on taper equations. The Forestry Chronicle, 80(4), 507-515.
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
#' kozak <-  nlsLM(di ~ taper_kozak(dbh, h, hih, b0, b1, b2, b3, b4, b5, b6, b7, b8, p),
#' data=tree_scaling,
#' start=list(b0=1.00,b1=.97,b2=.03,b3=.49,b4=-0.87,b5=0.50,b6=3.88,b7=0.03,b8=-0.19,p =.1))
#'
#' @export
taper_kozak <- function(dbh, h, hih, b0, b1, b2, b3, b4, b5, b6, b7, b8, p){
  b0*(dbh**b1)*(h**b2)*((1-hih**(1/4))/(1-(p^(1/3))))**(b3*hih**4+b4*(1/exp(dbh/h))+b5*((1-hih**(1/4))/(1-(p^(1/3))))**0.1+b6*
                                                          (1/dbh)+b7*(h**(1-
                                                                            hih**(1/3)))+b8*((1-hih**(1/4))/(1-(p^(1/3)))))
}
