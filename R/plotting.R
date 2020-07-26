### plotting functions



# plot behavioral error --------------------------------------------------
#' Plot response error of behavioural data
#'
#'Function to plot the response error in behavioural data. Requires a data
#'frame that (at least) has target value data and participant response data.
#'
#' @param data A data frame with columns containing: participant identifier
#' ('id_var'); the participants' response per trial ('response_var'); the
#' target value ('target_var'); and, if applicable, the set size of each
#' response ('set_size_var'), and the condition of each response
#' ('condition_var').
#'@param unit The unit of measurement in the data frame: "degrees"
#'(measurement is in degrees, from 0 to 360); "degrees_180 (measurement is in
#'degrees, but limited to 0 to 180); "radians" (measurement is in radians,
#'from pi to 2 * pi); "wrapped_radians" (measurement is in radians, but
#'wrapped from -pi to pi)
#'@param id_var The column name coding for participant id.
#'@param response_var The column name coding for the participants' responses
#'@param target_var The column name coding for the target value
#'@param set_size_var The column name (if applicable) coding for the set
#'size of each response
#'@param condition_var The column name (if applicable) coding for the
#'condition of each response
#'@examples
#'library(tidyverse)
#'data(example_data)
#'plot_error(example_data, condition_var = "condition")
#' @export
plot_error <- function(data,
                       unit = "degrees",
                       id_var = "id",
                       response_var = "response",
                       target_var = "target",
                       set_size_var = "NULL",
                       condition_var = "NULL"){


  # establish the break points of the density plot
  break_points <- round(seq(from = -pi, to = pi, length.out = 18), 3)

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



# find mean error ---------------------------------------------------------

  # no set size or condition manipulation
  if(set_size_var == "NULL" && condition_var == "NULL"){

    final_data <- data %>%
      group_by(id) %>%
      summarise(y = hist(error, breaks = break_points, plot = FALSE)$density,
                x = hist(error, breaks = break_points, plot = FALSE)$mids) %>%
      group_by(x) %>%
      summarise(mean_error = mean(y),
                se_error = (sd(y) / sqrt(length(y))))
  }

  # no set size manipulation but there is a condition manipulation
  if(set_size_var == "NULL" && condition_var != "NULL"){

    data$condition <- as.factor(data[[condition_var]])

    final_data <- data %>%
      group_by(id, condition) %>%
      summarise(y = hist(error, breaks = break_points, plot = FALSE)$density,
                x = hist(error, breaks = break_points, plot = FALSE)$mids) %>%
      group_by(condition, x) %>%
      summarise(mean_error = mean(y),
                se_error = (sd(y) / sqrt(length(y))))
  }


  # set size manipulation, but no condition manipulation
  if(set_size_var != "NULL" && condition_var == "NULL"){
    data$set_size <- data[[set_size_var]]

    final_data <- data %>%
      group_by(id, set_size) %>%
      summarise(y = hist(error, breaks = break_points, plot = FALSE)$density,
                x = hist(error, breaks = break_points, plot = FALSE)$mids) %>%
      group_by(set_size, x) %>%
      summarise(mean_error = mean(y),
                se_error = (sd(y) / sqrt(length(y))))
  }

  # both set size & condition manipulation
  if(set_size_var != "NULL" && condition_var != "NULL"){
    data$set_size <- data[[set_size_var]]
    data$condition <- as.factor(data[[condition_var]])

    final_data <- data %>%
      group_by(id, condition, set_size) %>%
      summarise(y = hist(error, breaks = break_points, plot = FALSE)$density,
                x = hist(error, breaks = break_points, plot = FALSE)$mids) %>%
      group_by(set_size, condition, x) %>%
      summarise(mean_error = mean(y),
                se_error = (sd(y) / sqrt(length(y))))
  }



# plot the data -----------------------------------------------------------


  # no set size or condition manipulation
  if(set_size_var == "NULL" && condition_var == "NULL"){

    plot <- ggplot(final_data, aes(x = x,
                                   y = mean_error)) +
      geom_point() +
      geom_errorbar(aes(ymax = mean_error + se_error,
                        ymin = mean_error - se_error),
                    width = 0.05) +
      theme_bw() +
      scale_x_continuous(limits = c(-pi, pi)) +
      scale_y_continuous(limits = c(0,
                                    max(final_data$mean_error) +
                                      max(final_data$se_error))) +
      labs(x = "Mean Error (Radians)",
           y = "Probability Density")

  }

  # no set size manipulation but there is a condition manipulation
  if(set_size_var == "NULL" && condition_var != "NULL"){

    plot <- ggplot(final_data, aes(x = x,
                                   y = mean_error)) +
      geom_point() +
      geom_errorbar(aes(ymax = mean_error + se_error,
                        ymin = mean_error - se_error),
                    width = 0.05) +
      theme_bw() +
      scale_x_continuous(limits = c(-pi, pi)) +
      scale_y_continuous(limits = c(0,
                                    max(final_data$mean_error) +
                                      max(final_data$se_error))) +
      labs(x = "Mean Error (Radians)",
           y = "Probability Density") +
      facet_wrap(vars(condition), ncol = 4)

    # rename the final_data frame
    colnames(final_data)[1] <- condition_var

    }


  # set size manipulation, but no condition manipulation
  if(set_size_var != "NULL" && condition_var == "NULL"){

    plot <- ggplot(final_data, aes(x = x,
                                   y = mean_error)) +
      geom_point() +
      geom_errorbar(aes(ymax = mean_error + se_error,
                        ymin = mean_error - se_error),
                    width = 0.05) +
      theme_bw() +
      scale_x_continuous(limits = c(-pi, pi)) +
      scale_y_continuous(limits = c(0,
                                    max(final_data$mean_error) +
                                      max(final_data$se_error))) +
      labs(x = "Mean Error (Radians)",
           y = "Probability Density") +
      facet_wrap(vars(set_size), ncol = 4)

    # rename the final_data frame
    colnames(final_data)[1] <- set_size_var

  }


  # both set size & condition manipulation
  if(set_size_var != "NULL" && condition_var != "NULL"){

    plot <- ggplot(final_data, aes(x = x,
                                   y = mean_error,
                                   group = condition)) +
      geom_point(aes(colour = condition)) +
      geom_errorbar(aes(ymax = mean_error + se_error,
                        ymin = mean_error - se_error,
                        colour = condition),
                    width = 0.05) +
      theme_bw() +
      scale_x_continuous(limits = c(-pi, pi)) +
      scale_y_continuous(limits = c(0,
                                    max(final_data$mean_error) +
                                      max(final_data$se_error))) +
      scale_colour_brewer(palette = "Dark2", name = condition_var) +
      guides(fill=guide_legend(title="New Legend Title")) +
      labs(x = "Mean Error (Radians)",
           y = "Probability Density") +
      facet_wrap(vars(condition, set_size))

    # rename the final_data frame
    colnames(final_data)[1] <- set_size_var
    colnames(final_data)[2] <- condition_var

  }

  # return the plot & the plot data
  return(list(plot = plot,
              plot_data = final_data))

}
