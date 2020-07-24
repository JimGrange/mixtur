### plotting functions



# plot behavioral error --------------------------------------------------
#' Plot response error of behavioural data
#'
#'Function to plot the behavioural error data. Requires a data frame
#'that (at least) has target value data and participant response data.
#'
#' @param data A data frame with columns containing: participant identification
#' number ('id_var'); the participants' response per trial ('response_var');
#' the  target value ('target_var'); and, if applicable, the condition
#' ('condition_var').
#'@param unit The unit of measurement in the data frame: "degrees_360"
#'(measurement is in degrees, from 0 to 360); "degrees_180 (measurement is in
#'degrees, but limited to 0 to 180); "radians" (measurement is in radians,
#'from pi to 2 * pi); "wrapped_radians" (measurement is in radians, but
#'wrapped from -pi to pi)
#'@param id_var The column name coding for participant id.
#'@param response_var The column name coding for the participants' responses
#'@param target_var The column name coding for the target value
#'@param set_size_var The column name (if applicable) coding for the set
#'size of each response
#'@param condition_var If multiple conditions were presented, the column name
#'that codes for the current condition
#' @export
plot_error <- function(data,
                       unit = "degrees_360",
                       id_var = "id",
                       response_var = "response",
                       target_var = "target",
                       set_size_var = "set_size",
                       condition_var = "condition"){


  # establish the break points of the density plot
  break_points <- round(seq(from = -pi, to = pi, length.out = 18), 3)

  # get the list of participant ids
  ids <- unique(data[[id_var]])


  # calculate response error mapped onto circular space ----
  if(unit == "degrees_360"){
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
    data$error <- response - target
  }

  if(unit == "wrapped_radians"){
    data$error <- data$response
  }



# find mean error ---------------------------------------------------------
  if(is.null(set_size) && is.null(condition_var)){

    final_data <- data %>%
      group_by(id) %>%
      summarise(y = hist(error, breaks = break_points, plot = FALSE)$density,
                x = hist(error, breaks = break_points, plot = FALSE)$mids) %>%
      group_by(x) %>%
      summarise(mean_error = mean(y),
                se_error = (sd(y) / sqrt(length(y))))
  }



# plot the data -----------------------------------------------------------

ggplot(final_data, aes(x = x,
                       y = mean_error)) +
    geom_point() +
    geom_errorbar(aes(ymax = mean_error + se_error,
                      ymin = mean_error - se_error),
                  width = 0.05) +
    theme_bw() +
    scale_x_continuous(limits = c(-pi, pi))


return(data)

}
