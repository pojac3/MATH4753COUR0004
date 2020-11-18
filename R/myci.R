#' MATH4753COUR0004::myci()
#'
#'
#' myci() creates a 95\% confidence interval of the mean given a vector x.
#' This is like a much simplified version of the t.test() function,
#' obviously with much less functionality.
#'
#' @param x A vector of values which you would like to create
#' a 95\% confidence interval  of the mean for.
#'
#' @return This function returns a vector ci which holds 2 values:
#' ci[1] is the lower bound of the 95\% confidence interval of the mean, and
#' ci[2] is the upper bound of the 95\% confidence interval of the mean.
#'
#' @examples
#' y=c(3,4,5,6,7)
#' confidence_interval=myci(y)
#' confidence_interval
#'
myci<-function(x=c()) {
  t=qt(0.95,length(x))

  ci=c() # creating the vector which we will return
  ci[1]=mean(x)-t*sd(x)/sqrt(length(x))
  ci[2]=mean(x)+t*sd(x)/sqrt(length(x))
  return (ci)
}
