#' Bi (2000) Taper Function.
#'
#' @param dbh tree diameter at breast height, in centimeters.
#' @param h total tree height, in meters.
#' @param hih ratio between the height of the section (hi) and the total height (h).
#' @param b0,b1,b2,b3,b4,b5,b6 model parameters.
#'
#' @return a numeric value indicating the diameter at the section.
#'
#' @references
#' Bi, H. (2000). Trigonometric variable-form taper equations for Australian eucalypts. Forest Science, 46(3), 397-409.
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
#' bi <-  nlsLM(di ~ taper_bi(dbh, h, hih, b0, b1, b2, b3, b4, b5, b6),
#' data=tree_scaling,
#' start=list(b0=1.8,b1=-0.2,b2=-0.04,b3=-0.9,b4=-0.0006,b5=0.07,b6=-.14))
#'
#' @export
taper_bi <- function(dbh, h, hih, b0, b1, b2, b3, b4, b5, b6){
  dbh*(log(sin((pi/2)*(hih)))/(log(sin((pi/2)*(1.3/h)))))**
     (b0+b1*sin((pi/2)*(hih))+b2*cos((3*pi/2)*(hih))+b3*(sin((pi/2)*(hih))/(hih))+
       b4*dbh+b5*(hih)*dbh**0.5+b6*(hih)*h**0.5)
}
