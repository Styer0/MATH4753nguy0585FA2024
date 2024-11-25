#' @title the 95 Percent Confidence Interval
#'
#' @description
#' This function calculates a 95 Percent confidence interval for the mean of a single sample.
#'
#' @param x A numeric vector representing the sample.
#'
#' @return A numeric vector of length 2 containing the lower and upper bounds of the confidence interval.
#' @export
#'
#' @examples
#' myci(rnorm(30, mean=10, sd=12))
myci <- function(x){
  t=qt(0.975,24)
  ci=c()
  ci[1]=mean(x)-t*sd(x)/sqrt(25)
  ci[2]=mean(x)+t*sd(x)/sqrt(25)
  return(ci)
}
