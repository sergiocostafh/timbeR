#' Estimate the total or partial volume of the tree, based on a 5th degree polynomial function that describes the taper of the tree.
#'
#' Estimates the total or partial volume of the tree from the diameter at breast height, total height, initial section height, final section height and coefficients of the 5th degree polynomial function that describes the tree's taper.
#'
#' @param dbh tree diameter at breast height, in centimeters.
#' @param h total tree height, in meters.
#' @param coef numerical vector containing six coefficients of the 5th degree polynomial function that describes the tree's taper.
#' @param hi final height of the tree section whose volume will be calculated, in meters. Default is the total tree height (h).
#' @param h0 initial height of the tree section whose volume will be calculated, in meters. Default is 0 (ground height).
#'
#' @return a numeric value indicating the total or partial volume of the tree.
#'
#' @export
poly5_vol <- function(dbh,h,coef,hi,h0){

  b0 <- coef[[1]]; b1 <- coef[[2]]; b2 <- coef[[3]]; b3 <- coef[[4]]; b4 <- coef[[5]]; b5 <- coef[[6]]

  if(missing(hi)){
    hi <- h
  }

  {

    if(missing(h0)){
      h0_vol <- 0
      }

    else{
      h0_vol <- pi*dbh^2/40000*((b0^2)*h0+b0*(b1/h)*h0^2+((2/3)*b0*(b2/h^2)+(1/3)*(b1/h)^2)*h0^3+((1/2)*b0*(b3/h^3)+(1/2)*(b1/h)*(b2/h^2))*h0^4+((2/5)*b0*(b4/h^4)+(2/5)*(b1/h)*(b3/h^3)+(1/5)*(b2/h^2)^2)*h0^5+((1/3)*b0*(b5/h^5)+(1/3)*(b1/h)*(b4/h^4)+(1/3)*(b2/h^2)*(b3/h^3))*h0^6+((2/7)*(b1/h)*(b5/h^5)+(2/7)*(b2/h^2)*(b4/h^4)+(1/7)*(b3/h^3)^2)*h0^7+((1/4)*(b2/h^2)*(b5/h^5)+(1/4)*(b3/h^3)*(b4/h^4))*h0^8+((2/9)*(b3/h^3)*(b5/h^5)+(1/9)*(b4/h^4)^2)*h0^9+(1/5)*(b4/h^4)*(b5/h^5)*h0^10+(1/11)*(b5/h^5)^2*h0^11)
      }

    (pi*dbh^2/40000*((b0^2)*hi+b0*(b1/h)*hi^2+((2/3)*b0*(b2/h^2)+(1/3)*(b1/h)^2)*hi^3+((1/2)*b0*(b3/h^3)+(1/2)*(b1/h)*(b2/h^2))*hi^4+((2/5)*b0*(b4/h^4)+(2/5)*(b1/h)*(b3/h^3)+(1/5)*(b2/h^2)^2)*hi^5+((1/3)*b0*(b5/h^5)+(1/3)*(b1/h)*(b4/h^4)+(1/3)*(b2/h^2)*(b3/h^3))*hi^6+((2/7)*(b1/h)*(b5/h^5)+(2/7)*(b2/h^2)*(b4/h^4)+(1/7)*(b3/h^3)^2)*hi^7+((1/4)*(b2/h^2)*(b5/h^5)+(1/4)*(b3/h^3)*(b4/h^4))*hi^8+((2/9)*(b3/h^3)*(b5/h^5)+(1/9)*(b4/h^4)^2)*hi^9+(1/5)*(b4/h^4)*(b5/h^5)*hi^10+(1/11)*(b5/h^5)^2*hi^11))-h0_vol

    }

  }
