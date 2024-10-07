#' @title The ntickets Function
#'
#' @description
#' Calculate the number of tickets to be sold when the number of seats in flight is N and the probability of a
#' "show" is p and gamma is the probability that the plane will be overbooked.
#'
#' @param N Numbers of seats
#' @param gamma The Probability of overbooking
#' @param p The Probability of a "show"
#'
#' @return Two Plots of discrete and continuous graphs,
#' numbers of tickets for discrete and continuous values,
#' N value, p value, and gamma value
#' @export
#'
#' @examples
#' ntickets(200, 0.02, 0.95)
ntickets = function(N,gamma,p){
  # sequence of n values
  nvalues = seq(N, floor(N+0.1* N), by = 1)

  # Objective function for discrete distribution
  objdis <- 1 - gamma - pbinom(q = N,
                               size = nvalues,
                               prob = p)

  # Objective function for continuous distribution
  objcon <- function(nvalues) {
    1 - gamma - pnorm(N + 0.5,
                      nvalues * p,
                      sqrt(nvalues * p * (1 - p)))
  }

  # Number of tickets sold for discrete
  nd = nvalues[which.min(abs(objdis))]

  # Number of tickets sold for continuous
  nc = optimize(f = function(x) abs(objcon(x)), interval = nvalues)$minimum

  # Discrete graph
  plot(x=nvalues, y = objdis, type = 'b',
       main = paste("Objective vs n to find optimal tickets sold\n(",nd,") gamma=",
                    gamma, "N=",N, "discrete"), ylab = "Objective", xlab="n", bg = "blue",pch=21)
  abline(v = nd, h = 0, col = "red")

  # Continuous graph
  curve(objcon(x), N, floor(N + 0.1 * N),
        main = paste("Objective vs n to find optimal tickets sold\n(",nc,") gamma=",
                     gamma, "N=", N, "continuous"), ylab = "Objective", xlab = "n")
  abline(v = nc, h = 0, col = "blue")

  #print list
  print(list(nd = nd, nc = nc, N = N, p = p, gamma = gamma))
}
