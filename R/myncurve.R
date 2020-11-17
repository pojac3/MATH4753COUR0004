#' MATH4753COUR0004::myncurve()
#'
#' Creates a graph of a normal curve with mean mu and standard deviation sigma.
#' On this plot, it will also plot the lower tail area under the curve beginning at
#' a, and print this value out to the console.
#'
#' @param mu The mean of this normal distribution. Defaults to 0
#' @param sigma The standard deviation of this normal distribution. Defaults to 1
#' @param a The lower tail probability which the function begins at.
#'
#' @return This function does not return any values. Rather, it simply prints out the lower tail
#' probability of the given curve beginning at the value a
#'
#' @examples
#' MATH4753COUR0004::myncurve(mu=10,sigma=5,a=6)
myncurve = function(mu=0, sigma=1,a){
  ### Draw a normal curve
  curve(dnorm(x,mean=mu,sd=sigma),xlim=c(mu-3*sigma, mu+3*sigma))

  # x values corresponding to the x - cords of points on the curve
  xcurve=seq(mu-3*sigma, a, length=1000)

  # Y values corresponding t0 the x values
  ycurve=dnorm(xcurve,mean=mu,sd=sigma)

  # Fill in the polygon with the given vertices
  polygon(c(mu-3*sigma,xcurve,a),c(0,ycurve,0),col="Red")

  # Put in the text with the appropriate area

  # Area
  prob=pnorm(a,mean=mu,sd=sigma)
  prob=round(prob,4)
  prob
}
