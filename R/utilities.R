### various utility functions



# map onto circular space -------------------------------------------------
map_to_circular <- function(data, unit){

  if(unit == "degrees"){
    circ <- ((2 * pi * data) / 360)
  }

  if(unit == "degrees_180"){
    circ <- (2 * pi * data) / 180
  }

  if(unit == "radians"){
    circ <- data
  }

  return(circ)

}




# degrees to radians ------------------------------------------------------
#' Transform degrees into radians
#'
#' A function to transform degrees into radians
#' @param deg Degree value to transform into radians
#' @source
#' The code has been adapted from Matlab code written by Paul Bays
#' (https://bayslab.com) published under GNU General Public License.
#' @noRd
degrees_to_radians <- function(deg){deg * pi / 180}



# radians to degrees ------------------------------------------------------
#' Transform radians into degrees
#'
#' A function to transform radians into degrees
#' @param rad Radian value to transform into degrees
#' @source
#' The code has been adapted from Matlab code written by Paul Bays
#' (https://bayslab.com) published under GNU General Public License.
#' @noRd
radians_to_degrees <- function(rad) {(rad * 180) / (pi)}



# wrapping radians onto circular space ------------------------------------------
# Map values onto circular space
# This function maps the input radians onto the circular space -pi to pi.
# @param data The data (in radians) to be mapped
# @source
# The code has been adapted from Matlab code written by Paul Bays
# (https://bayslab.com) published under GNU General Public License.
wrap <- function(data, bound = pi) {
  result <- ((data + bound) %% (bound * 2)) - bound
  return(result)
}



# calculate circular mean -------------------------------------------------
# Calculate the circular equivalent of the mean
# @source
# The code has been adapted from Matlab code written by Paul Bays
# (https://bayslab.com) published under GNU General Public License.
cmean <- function(x) {

  if(any(abs(x) > pi)) {
    stop("Error: Input values must be in radians, range -PI to PI'", call. = FALSE)
  }

  y <- atan2(sum(sin(x)), sum(cos(x)))
  return(y)
}



# calculate the circular SD -----------------------------------------------
# Calculate the circular equivalent of standard deviation
# @source
# The code has been adapted from Matlab code written by Paul Bays
# (https://bayslab.com) published under GNU General Public License.
cstd <- function(x) {

  if(any(abs(x) > pi)) {
    stop("Error: Input values must be in radians, range -PI to PI'", call. = FALSE)
  }

  if(NROW(x) == 1){
    x <- t(x)
  }

  r <- sqrt(sum(sin(x))^2 + sum(cos(x))^2) / NROW(x)
  y <- sqrt(-2 * log(r))
  return(y)
}



# generate random samples from vin mises ----------------------------------
#' Generates N random samples from a Von Mises distribution with mean mu and
#' concentration k
#' @param n An integer indicating how many values to generate
#' @param mu A numeric value indicating the location parameter of the von Mises
#' distribution to generate values from
#' @param kappa A numeric value indicating the concentration parameter of the
#' von Mises distribution
#' @examples
#' simulated_values <- randomvonmises(n = 100, mu = 0, kappa = 8.2)
#' @source
#' The code has been adapted from Matlab code written by Paul Bays
#' (https://bayslab.com) published under GNU General Public License.
#' @importFrom stats runif
#' @noRd
randomvonmises <- function(n, mu, kappa) {

  x = NULL

  if(kappa == 0) {
    x <- (runif(n) * 2 - 1) * pi
  }

  a <- 1 + (1 + 4 * (kappa ^ 2)) ^ 0.5
  b <- (a - (2 * a) ^ 0.5) / (2 * kappa)
  r <- (1 + b ^ 2) / (2 * b)
  obs <- 1

  while(obs <= n) {
    z = cos(pi * runif(1))
    f = (1 + r * z) / (r + z)
    c = kappa * (r - f)
    u = runif(1)

    if((c * (2 - c) - u > 0) | (log(c / u) + 1 - c >= 0)) {
      x[obs] = wrap(sign(runif(1) - 0.5) * acos(f) + mu)
      obs = obs + 1
    }

  }

  return(x)

}


# probability density function of von mises -------------------------------
#' Probability density function of the von Mises distribution.
#' Returns the probability density function for the von Mises distribution with
#' location parameter mu and concentration parameter kappa, evaluated at the
#' values in x (given in radians, from -pi to pi).
#' @param x a numeric value (or vector) of quantiles
#' @param mu a numeric value indicating the location parameter of the von Mises
#' distribution
#' @param kappa a numeric value indicating the concentration parameter of the
#' von Mises distribution
#' @examples
#'
#' # establish quantile values
#' x <- seq(from = -pi, to = pi, length.out = 100)
#'
#' # calculate pdf
#' y <- vonmisespdf(x = x, mu = 0, kappa = 8.2)
#'
#' @source
#' The code has been adapted from Matlab code written by Paul Bays
#' (https://bayslab.com) published under GNU General Public License.
#' @noRd
vonmisespdf <- function(x, mu, kappa) {
  p <- exp(kappa * cos(x - mu)) / (2 * pi * besselI(kappa, 0))
  return(p)
}



# obtain logarithmically spaced vectors -----------------------------------
# Obtain logarithmically spaced vectors
# logspace function for logarithmically spaced vectors
# @source
# The code has been adapted from Matlab code written by Paul Bays
# (https://bayslab.com) published under GNU General Public License.
logspace <- function(a, b, n){
  exp(log(10) * seq(a, b, length.out = n))
}



# trapz function ----------------------------------------------------------
# trapz function from the caTools package by Jarek Tuszynski
# @source
# The code has been adapted from Matlab code written by Paul Bays
# (https://bayslab.com) published under GNU General Public License.
trapz <- function(x, y) {
  idx <- 2:length(x)
  return (as.double( (x[idx] - x[idx-1]) %*% (y[idx] + y[idx-1])) / 2)
}



# Matlab's repmat function ------------------------------------------------
# Recreate Matlab's repmat function
# @source
# The code has been adapted from Matlab code written by Paul Bays
# (https://bayslab.com) published under GNU General Public License.
repmat = function(x, nn){

  mx <- NROW(x)
  nx <- NCOL(x)

  if(nn > 0){
    return(matrix(data = x, nrow = mx, ncol = nx * nn))
  } else {
    return(matrix(nrow = mx, ncol = nn))
  }
}



# inverse of A1 function --------------------------------------------------
# Inverse of A1 function.
# @source
# The code has been adapted from Matlab code written by Paul Bays
# (https://bayslab.com) published under GNU General Public License.
A1inv <- function(r) {

  if(0 <= r & r < 0.53) {
    k <- 2 * r + r ^ 3 + (5 * r ^ 5) / 6
  } else if(r < 0.85) {
    k <- -0.4 + 1.39 * r + 0.43 / (1 - r)
  } else {
    k <- 1 / (r ^ 3 - 4 * r ^ 2 + 3 * r)
  }

  return(k)

}



# standard deviation of k -------------------------------------------------
#' Standard deviation of von Mises kappa parameter
#' Returns the standard deviation of a wrapped normal distribution
#' corresponding to a von Mises concentration parameter of kappa
#'@param k A numeric value for concentration parameter kappa of the von
#' Mises distribution
#' @source
#' The code has been adapted from Matlab code written by Paul Bays
#' (https://bayslab.com) published under GNU General Public License.
#'
#' @examples
#' sd <- k2sd(8.4)
#'
#' @noRd
k2sd <- function(k){

  if(k == 0){
    s <- Inf
  } else if(is.infinite(k)){
    s <- 0
  } else {
    s <- sqrt(-2 * log(besselI(k, 1) / besselI(k, 0)))
  }

  return(s)

}



# standard deviation to k -------------------------------------------------
#' Translate from standard deviation to von Mises kappa parameter
#' Returns the von Mises concentration parameter kappa corresponding
#' to a standard deviation sd of a wrapped normal distribution
#' @param sd A numeric value for standard deviation
#' @examples
#' kappa <- sd2k(0.361)
#' @source
#' The code has been adapted from Matlab code written by Paul Bays
#' (https://bayslab.com) published under GNU General Public License.
#' @noRd
sd2k <- function(sd){

  r <- exp(-sd ^ 2 / 2)
  k <- 1 / (r ^ 3 - 4 * r ^ 2 + 3 * r)
  k[r < 0.85] <- -0.4 + 1.39 * r[r < 0.85] + 0.43 / (1 - r[r < 0.85])
  k[r < 0.53] <- 2 * r[r < 0.53] + r[r < 0.53]^3 + (5 * r[r < 0.53]^5) / 6

  return(k)
}



# calculate akiake's information criterion --------------------------------
aic <- function(ll, parms){
  value <- (-2 * ll) + (2 * parms)
  value <- round(value, 3)
  return(value)
}


# akiake's information criterion, corrected for n -------------------------
aic_c <- function(ll, parms, n){
  value <- (-2 * ll) + ((2 * parms) * (n / (n - parms - 1)))
  value <- round(value, 3)
  return(value)
}


# calculate bayesian information criterion --------------------------------
bic <- function(ll, parms, n){
  value <- (-2 * ll) + (parms * log(n))
  value <- round(value, 3)
  return(value)
}
