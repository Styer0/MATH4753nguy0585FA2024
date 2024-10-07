#' @title myncurve
#'
#' @param mu Mean of the normal distribution.
#' @param sigma Standard deviation of the distribution.
#' @param a The point up to which the area is calculated and shaded.
#'
#' @return A plot that shows a shaded area and the area itself
#' @export
#'
#' @examples
#' myncurve(5,10,3)
myncurve = function(mu, sigma, a) {
  # Plot the normal distribution curve
  curve(dnorm(x, mean = mu, sd = sigma),
        xlim = c(mu - 3 * sigma, mu + 3 * sigma),
        ylab = "Normal density",
        xlab = "x")

  # Shade the area from -∞ to a
  x_shade = seq(mu - 3 * sigma, a, length = 1000)
  y_shade = dnorm(x_shade, mean = mu, sd = sigma)
  polygon(c(x_shade, a), c(y_shade, 0), col = "pink")

  # Calculate the area (P(X <= a))
  area = pnorm(a, mean = mu, sd = sigma)

  # Return the results in a list
  return(list(mu = mu, sigma = sigma, area = area))
}
