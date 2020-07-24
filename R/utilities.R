### various utility functions

# degrees to radians ------------------------------------------------------
#' Transform degrees into radians
#'
#' A function to transform degrees into radians
#' @param deg Degree value to transform into radians
#' #' @export
degrees_to_radians <- function(deg){deg * pi / 180}



# radians to degrees ------------------------------------------------------
#' Transform radians into degrees
#'
#' A function to transform radians into degrees
#' @param deg Radian value to transform into degrees
#' #' @export
radians_to_degrees <- function(rad) {(rad * 180) / (pi)}



# wrapping radians onto circular space ------------------------------------------
#' Map values onto circular space
#' This function maps the input radians onto the circular space -pi to pi.
#' @param data The data (in radians) to be mapped
#' #' @export
wrap <- function(data, bound = pi) {
  result <- ((data + bound) %% (bound * 2)) - bound
  return(result)
}



