# analyse behavioural precision ----------------------------------------------
#' Obtain measures of precision
#'
#' Returns participant-level data for precision estimates ready for inferential
#' analysis. Note that the function does not actually conduct the analysis.
#'
#' Precision is defined as the reciprocal of the circular standard deviation of
#' the response error corrected for guessing.
#'
#' This note is a TODO for later.
#'
#' @param data A data frame with columns containing: participant identifier
#' (declared via variable 'id_var'); the participants' response per trial ('response_var'); the
#' target value ('target_var'); and, if applicable, the set size of each
#' response ('set_size_var'), and the condition of each response
#' ('condition_var').
#' @param unit The unit of measurement in the data frame: "degrees"
#' (measurement is in degrees, from 0 to 360); "degrees_180 (measurement is in
#' degrees, but limited to 0 to 180); or "radians" (measurement is in radians,
#' from pi to 2 * pi, but could also be already in -pi to pi).
#' @param id_var The quoted column name coding for participant id. If the data is from
#' a single participant (i.e., there is no id column) set to NULL.
#' @param response_var The quoted column name coding for the participants' responses
#' @param target_var The quoted column name coding for the target value.
#' @param set_size_var The quoted column name (if applicable) coding for the set
#' size of each response.
#' @param condition_var The quoted column name (if applicable) coding for the
#' condition of each response.
#' @importFrom stats sd
#' @importFrom dplyr %>%
#' @importFrom dplyr summarise
#' @importFrom dplyr group_by
#' @importFrom graphics hist
#'
#'
#' @examples
#' # load an example data frame
#' data(bays2009_full)
#'
#' # calculate the precision per condition per set size
#' precision_data <- analyse_precision(data = bays2009_full,
#'                                     unit = "radians",
#'                                     condition_var = "delay",
#'                                     set_size_var = "set_size")
#'
#' @export
analyse_precision <- function(data,
                              unit = "degrees",
                              id_var = "id",
                              response_var = "response",
                              target_var = "target",
                              set_size_var = NULL,
                              condition_var = NULL){

  # get the list of participant ids
  ids <- unique(data[[id_var]])

  # calculate response error mapped onto circular space ----
  if(unit == "degrees"){
    response <- data[[response_var]] / 180 * pi
    target <- data[[target_var]] / 180 * pi
    data$error <- wrap(response - target)
  }

  if(unit == "degrees_180"){
    response <- data[[response_var]] / 90 * pi
    target <- data[[target_var]] / 90 * pi
    data$error <- wrap(response - target)
  }

  if(unit == "radians"){
    response <- data[[response_var]]
    target <- data[[target_var]]
    data$error <- wrap(response - target)
  }

  if(unit == "wrapped_radians"){
    data$error <- data$response
  }


  # find precision----

  # no set size or condition manipulation
  if(is.null(set_size_var) && is.null(condition_var)){

    if(!is.null(id_var)){
      final_data <- data %>%
        group_by(id) %>%
        summarise(precision = get_precision_single(error)[, 1],
                  bias = get_precision_single(error)[, 2])
    } else{
      final_data <- data %>%
        summarise(precision = get_precision_single(error)[, 1],
                  bias = get_precision_single(error)[, 2])
    }

  }

  # no set size manipulation but there is a condition manipulation
  if(is.null(set_size_var) && !is.null(condition_var)){

    data$condition <- as.factor(data[[condition_var]])

    if(!is.null(id_var)){
      final_data <- data %>%
        group_by(id, condition) %>%
        summarise(precision = get_precision_single(error)[, 1],
                  bias = get_precision_single(error)[, 2])
    } else{
      final_data <- data %>%
        group_by(condition) %>%
        summarise(precision = get_precision_single(error)[, 1],
                  bias = get_precision_single(error)[, 2])
    }

  }

  # set size manipulation, but no condition manipulation
  if(!is.null(set_size_var) && is.null(condition_var)){

    data$set_size <- data[[set_size_var]]

    if(!is.null(id_var)){
      final_data <- data %>%
        group_by(id, set_size) %>%
        summarise(precision = get_precision_single(error)[, 1],
                  bias = get_precision_single(error)[, 2])
    } else{
      final_data <- data %>%
        group_by(set_size) %>%
        summarise(precision = get_precision_single(error)[, 1],
                  bias = get_precision_single(error)[, 2])
    }

  }

  # both set size & condition manipulation
  if(!is.null(set_size_var) && !is.null(condition_var)){

    data$set_size <- data[[set_size_var]]
    data$condition <- as.factor(data[[condition_var]])

    if(!is.null(id_var)){
      final_data <- data %>%
        group_by(id, condition, set_size) %>%
        summarise(precision = get_precision_single(error)[, 1],
                  bias = get_precision_single(error)[, 2])
    } else{
      final_data <- data %>%
        group_by(condition, set_size) %>%
        summarise(precision = get_precision_single(error)[, 1],
                  bias = get_precision_single(error)[, 2])

    }
  }

  return(final_data)

}




# get precision of single condition ---------------------------------------
#' Obtain the precision of a single condition
#' @importFrom tidyr tibble
#' @source
#' The code has been adapted from Matlab code written by Paul Bays
#' (https://paulbays.com).
#' @export
get_precision_single <- function(error, target = 0) {

  if(any(abs(error) > pi) | any(abs(target) > pi)) {
    stop("Error: Input values must be in radians, range -PI to PI", call. = FALSE)
  }

  # transform error to column vector
  error <- tibble(error)

  #--- calculate precision
  n <- NROW(error)

  # expected precision under uniform distribution
  x <- logspace(-2, 2, 100)
  p0 <- trapz(x, n / (sqrt(x) * exp(x + (n * exp(-x)))))

  precision <- (1 / cstd(error)) - p0

  # Bias
  bias <- cmean(error)

  return(data.frame(precision = precision, bias = bias))
}

