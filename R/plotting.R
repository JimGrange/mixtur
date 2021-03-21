### plotting functions


# plot summary statistic --------------------------------------------------
#' Plot summary statistics of behavioural data
#'
#' Function to plot model-free summary statistics of behavioural data. Users
#' can plot mean absolute error, resultant vector length, and precision of the
#' behavioural data.
#'
#'@param data A data frame with columns containing: participant identifier
#'('id_var'); the participants' response per trial ('response_var'); the
#'target value ('target_var'); and, if applicable, the set size of each
#'response ('set_size_var'), and the condition of each response
#'('condition_var').
#'@param statistic The summary statistic to plot. This can be set to
#'"mean_absolute_error", "resultant_vector_length", or "precision".
#'@param unit The unit of measurement in the data frame: "degrees"
#'(measurement is in degrees, from 0 to 360); "degrees_180 (measurement is in
#'degrees, but limited to 0 to 180); or "radians" (measurement is in radians,
#'from pi to 2 * pi, but could also be already in -pi to pi).
#'@param id_var The column name coding for participant id. If the data is from
#'a single participant (i.e., there is no id column) set to "NULL".
#'@param response_var The column name coding for the participants' responses.
#'@param target_var The column name coding for the target value.
#'@param set_size_var The column name (if applicable) coding for the set
#'size of each response.
#'@param condition_var The column name (if applicable) coding for the
#'condition of each response.
#'@param return_data A boolean (TRUE or FALSE) indicating whether the data for
#'the plot should be returned.
#'
#'@examples
#'data(example_data)
#'plot_summary_statistic(example_data, condition_var = "condition")
#'
#' @importFrom stats sd
#' @importFrom dplyr %>%
#' @importFrom dplyr summarise
#' @importFrom dplyr group_by
#' @importFrom dplyr rename
#' @importFrom graphics hist
#' @export
plot_summary_statistic <- function(data,
                                   statistic = "precision",
                                   unit = "degrees",
                                   id_var = "id",
                                   response_var = "response",
                                   target_var = "target",
                                   set_size_var = NULL,
                                   condition_var = NULL,
                                   return_data = FALSE){


  if(statistic == "precision"){
    plot <- plot_precision(data = data,
                   unit = unit,
                   id_var = id_var,
                   response_var = response_var,
                   target_var = target_var,
                   set_size_var = set_size_var,
                   condition_var = condition_var,
                   return_data = return_data)
  }

  if(statistic == "mean_absolute_error"){
    plot <- plot_mean_absolute_error(data = data,
                             unit = unit,
                             id_var = id_var,
                             response_var = response_var,
                             target_var = target_var,
                             set_size_var = set_size_var,
                             condition_var = condition_var,
                             return_data = return_data)
  }


  if(statistic == "resultant_vector_length"){
    plot <- plot_resultant_vector_length(data = data,
                                 unit = unit,
                                 id_var = id_var,
                                 response_var = response_var,
                                 target_var = target_var,
                                 set_size_var = set_size_var,
                                 condition_var = condition_var,
                                 return_data = return_data)
  }


  return(plot)

}





# plot resultant vector length --------------------------------------------

#' Plot resultant vector length of behavioural data
#'
#' Function to plot the resultant vector length of response error in
#' behavioural data. Requires a data frame that (at least) has target value
#' data and participant response data.
#'
#'@param data A data frame with columns containing: participant identifier
#'('id_var'); the participants' response per trial ('response_var'); the
#'target value ('target_var'); and, if applicable, the set size of each
#'response ('set_size_var'), and the condition of each response
#'('condition_var').
#'@param unit The unit of measurement in the data frame: "degrees"
#'(measurement is in degrees, from 0 to 360); "degrees_180 (measurement is in
#'degrees, but limited to 0 to 180); or "radians" (measurement is in radians,
#'from pi to 2 * pi, but could also be already in -pi to pi).
#'@param id_var The column name coding for participant id. If the data is from
#'a single participant (i.e., there is no id column) set to "NULL".
#'@param response_var The column name coding for the participants' responses.
#'@param target_var The column name coding for the target value.
#'@param set_size_var The column name (if applicable) coding for the set
#'size of each response.
#'@param condition_var The column name (if applicable) coding for the
#'condition of each response.
#'@param return_data A boolean (TRUE or FALSE) indicating whether the data for
#'the plot should be returned.
#'
#'@examples
#'data(example_data)
#'plot_resultant_vector_length(example_data, condition_var = "condition")
#'
#' @importFrom stats sd
#' @importFrom dplyr %>%
#' @importFrom dplyr summarise
#' @importFrom dplyr group_by
#' @importFrom dplyr rename
#' @importFrom graphics hist
#' @export
plot_resultant_vector_length <- function(data,
                                         unit = "degrees",
                                         id_var = "id",
                                         response_var = "response",
                                         target_var = "target",
                                         set_size_var = NULL,
                                         condition_var = NULL,
                                         return_data = FALSE){

  # add id column
  data$id <- data[[id_var]]

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



  # find mean_absolute_error----

  # no set size or condition manipulation
  if(is.null(set_size_var) && is.null(condition_var)){

    if(!is.null(id_var)){
      final_data <- data %>%
        group_by(id) %>%
        summarise(individual_value = get_resultant_vector_length(error)) %>%
        summarise(mean_value = mean(individual_value),
                  se_value = sd(individual_value) /
                    sqrt(length(individual_value)))
    } else{
      final_data <- data %>%
        summarise(individual_value = get_resultant_vector_length(error)) %>%
        summarise(mean_value = mean(individual_value),
                  se_value = sd(individual_value) /
                    sqrt(length(individual_value)))
    }
  }


  # no set size manipulation but there is a condition manipulation
  if(is.null(set_size_var) && !is.null(condition_var)){

    data$condition <- as.factor(data[[condition_var]])

    if(!is.null(id_var)){
      final_data <- data %>%
        group_by(id, condition) %>%
        summarise(individual_value = get_resultant_vector_length(error)) %>%
        group_by(condition) %>%
        summarise(mean_value = mean(individual_value),
                  se_value = sd(individual_value) /
                    sqrt(length(individual_value)))
    } else{
      final_data <- data %>%
        group_by(condition) %>%
        summarise(individual_value = get_resultant_vector_length(error)) %>%
        group_by(condition) %>%
        summarise(mean_value = mean(individual_value),
                  se_value = sd(individual_value) /
                    sqrt(length(individual_value)))
    }
  }


  # set size manipulation, but no condition manipulation
  if(!is.null(set_size_var) && is.null(condition_var)){

    data$set_size <- data[[set_size_var]]

    if(!is.null(id_var)){
      final_data <- data %>%
        group_by(id, set_size) %>%
        summarise(individual_value = get_resultant_vector_length(error)) %>%
        group_by(set_size) %>%
        summarise(mean_value = mean(individual_value),
                  se_value = sd(individual_value) /
                    sqrt(length(individual_value)))
    } else{
      final_data <- data %>%
        group_by(set_size) %>%
        summarise(individual_value = get_resultant_vector_length(error)) %>%
        group_by(set_size) %>%
        summarise(mean_value = mean(individual_value),
                  se_value = sd(individual_value) /
                    sqrt(length(individual_value)))
    }
  }


  # both set size & condition manipulation
  if(!is.null(set_size_var) && !is.null(condition_var)){

    data$set_size <- data[[set_size_var]]
    data$condition <- as.factor(data[[condition_var]])

    if(!is.null(id_var)){
      final_data <- data %>%
        group_by(id, condition, set_size) %>%
        summarise(individual_value = get_resultant_vector_length(error)) %>%
        group_by(set_size, condition) %>%
        summarise(mean_value = mean(individual_value),
                  se_value = sd(individual_value) /
                    sqrt(length(individual_value)))
    } else{
      final_data <- data %>%
        group_by(condition, set_size) %>%
        summarise(individual_value = get_resultant_vector_length(error)) %>%
        group_by(set_size, condition) %>%
        summarise(mean_value = mean(individual_value),
                  se_value = sd(individual_value) /
                    sqrt(length(individual_value)))
    }
  }


  # plot the data----

  # no set size or condition manipulation
  if(is.null(set_size_var) && is.null(condition_var)){
    plot <- "NULL"
  }


  # no set size manipulation but there is a condition manipulation
  if(is.null(set_size_var) && !is.null(condition_var)){

    plot <- ggplot(final_data, aes(x = condition,
                                   y = mean_value)) +
      geom_errorbar(aes(ymax = mean_value + se_value,
                        ymin = mean_value - se_value),
                    width = 0.00) +
      geom_point(size = 2.5) +
      theme_bw() +
      labs(x = condition_var,
           y = "Resultant Vector Length")

    # rename the final_data frame
    colnames(final_data)[1] <- condition_var

  }


  # set size manipulation but no condition manipulation
  if(!is.null(set_size_var) && is.null(condition_var)){

    # ensure set size is numeric
    final_data$set_size <- as.numeric(as.character(final_data$set_size))

    plot <- ggplot(final_data, aes(x = set_size,
                                   y = mean_value)) +
      geom_errorbar(aes(ymax = mean_value + se_value,
                        ymin = mean_value - se_value),
                    width = 0.00) +
      geom_point(size = 2.5) +
      theme_bw() +
      labs(x = "Set Size",
           y = "Resultant Vector Length")

    # rename the final_data frame
    colnames(final_data)[1] <- set_size_var

  }


  # both set size & condition manipulation
  if(!is.null(set_size_var) && !is.null(condition_var)){

    # ensure set size is numeric
    final_data$set_size <- as.numeric(as.character(final_data$set_size))

    # add some jitter to the plotting position
    pd = position_dodge(0.2)

    plot <- ggplot(final_data, aes(x = set_size,
                                   y = mean_value,
                                   group = condition)) +
      geom_errorbar(aes(ymax = mean_value + se_value,
                        ymin = mean_value - se_value,
                        colour = condition),
                    width = 0.00,
                    position = pd) +
      geom_point(aes(colour = condition),
                 position = pd,
                 size = 2.5) +
      theme_bw() +
      scale_colour_brewer(palette = "Dark2", name = condition_var) +
      labs(x = "Set Size",
           y = "Resultant Vector Length")

    # rename the final_data frame
    colnames(final_data)[1] <- set_size_var
    colnames(final_data)[2] <- condition_var

  }


  # return the plot & the plot data
  if(return_data == TRUE){
    return(list(plot = plot, data = final_data))
  } else {
    return(plot)
  }


}



# plot mean absolute error ------------------------------------------------

#' Plot mean absolute error of behavioural data
#'
#'Function to plot the mean absolute error of response error in behavioural
#'data. Requires a data frame that (at least) has target value data and
#'participant response data.
#'
#'@param data A data frame with columns containing: participant identifier
#'('id_var'); the participants' response per trial ('response_var'); the
#'target value ('target_var'); and, if applicable, the set size of each
#'response ('set_size_var'), and the condition of each response
#'('condition_var').
#'@param unit The unit of measurement in the data frame: "degrees"
#'(measurement is in degrees, from 0 to 360); "degrees_180 (measurement is in
#'degrees, but limited to 0 to 180); or "radians" (measurement is in radians,
#'from pi to 2 * pi, but could also be already in -pi to pi).
#'@param id_var The column name coding for participant id. If the data is from
#'a single participant (i.e., there is no id column) set to "NULL".
#'@param response_var The column name coding for the participants' responses.
#'@param target_var The column name coding for the target value.
#'@param set_size_var The column name (if applicable) coding for the set
#'size of each response.
#'@param condition_var The column name (if applicable) coding for the
#'condition of each response.
#'@param return_data A boolean (TRUE or FALSE) indicating whether the data for
#'the plot should be returned.
#'
#'@examples
#'data(example_data)
#'plot_mean_absolute_error(example_data, condition_var = "condition")
#'
#' @importFrom stats sd
#' @importFrom dplyr %>%
#' @importFrom dplyr summarise
#' @importFrom dplyr group_by
#' @importFrom dplyr rename
#' @importFrom graphics hist
#' @export
plot_mean_absolute_error <- function(data,
                                     unit = "degrees",
                                     id_var = "id",
                                     response_var = "response",
                                     target_var = "target",
                                     set_size_var = NULL,
                                     condition_var = NULL,
                                     return_data = FALSE){

  # add id column
  data$id <- data[[id_var]]

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



  # find mean_absolute_error----

  # no set size or condition manipulation
  if(is.null(set_size_var) && is.null(condition_var)){

    if(!is.null(id_var)){
      final_data <- data %>%
        group_by(id) %>%
        summarise(individual_value = get_mean_absolute_error(error)) %>%
        summarise(mean_value = mean(individual_value),
                  se_value = sd(individual_value) /
                    sqrt(length(individual_value)))
    } else{
      final_data <- data %>%
        summarise(individual_value = get_mean_absolute_error(error)) %>%
        summarise(mean_value = mean(individual_value),
                  se_value = sd(individual_value) /
                    sqrt(length(individual_value)))
    }
  }


  # no set size manipulation but there is a condition manipulation
  if(is.null(set_size_var) && !is.null(condition_var)){

    data$condition <- as.factor(data[[condition_var]])

    if(!is.null(id_var)){
      final_data <- data %>%
        group_by(id, condition) %>%
        summarise(individual_value = get_mean_absolute_error(error)) %>%
        group_by(condition) %>%
        summarise(mean_value = mean(individual_value),
                  se_value = sd(individual_value) /
                    sqrt(length(individual_value)))
    } else{
      final_data <- data %>%
        group_by(condition) %>%
        summarise(individual_value = get_mean_absolute_error(error)) %>%
        group_by(condition) %>%
        summarise(mean_value = mean(individual_value),
                  se_value = sd(individual_value) /
                    sqrt(length(individual_value)))
    }
  }


  # set size manipulation, but no condition manipulation
  if(!is.null(set_size_var) && is.null(condition_var)){

    data$set_size <- data[[set_size_var]]

    if(!is.null(id_var)){
      final_data <- data %>%
        group_by(id, set_size) %>%
        summarise(individual_value = get_mean_absolute_error(error)) %>%
        group_by(set_size) %>%
        summarise(mean_value = mean(individual_value),
                  se_value = sd(individual_value) /
                    sqrt(length(individual_value)))
    } else{
      final_data <- data %>%
        group_by(set_size) %>%
        summarise(individual_value = get_mean_absolute_error(error)) %>%
        group_by(set_size) %>%
        summarise(mean_value = mean(individual_value),
                  se_value = sd(individual_value) /
                    sqrt(length(individual_value)))
    }
  }


  # both set size & condition manipulation
  if(!is.null(set_size_var) && !is.null(condition_var)){

    data$set_size <- data[[set_size_var]]
    data$condition <- as.factor(data[[condition_var]])

    if(!is.null(id_var)){
      final_data <- data %>%
        group_by(id, condition, set_size) %>%
        summarise(individual_value = get_mean_absolute_error(error)) %>%
        group_by(set_size, condition) %>%
        summarise(mean_value = mean(individual_value),
                  se_value = sd(individual_value) /
                    sqrt(length(individual_value)))
    } else{
      final_data <- data %>%
        group_by(condition, set_size) %>%
        summarise(individual_value = get_mean_absolute_error(error)) %>%
        group_by(set_size, condition) %>%
        summarise(mean_value = mean(individual_value),
                  se_value = sd(individual_value) /
                    sqrt(length(individual_value)))
    }
  }


  # plot the data----

  # no set size or condition manipulation
  if(is.null(set_size_var) && is.null(condition_var)){
    plot <- "NULL"
  }


  # no set size manipulation but there is a condition manipulation
  if(is.null(set_size_var) && !is.null(condition_var)){

    plot <- ggplot(final_data, aes(x = condition,
                                   y = mean_value)) +
      geom_errorbar(aes(ymax = mean_value + se_value,
                        ymin = mean_value - se_value),
                    width = 0.00) +
      geom_point(size = 2.5) +
      theme_bw() +
      labs(x = condition_var,
           y = "Mean Absolute Error")

    # rename the final_data frame
    colnames(final_data)[1] <- condition_var

  }


  # set size manipulation but no condition manipulation
  if(!is.null(set_size_var) && is.null(condition_var)){

    # ensure set size is numeric
    final_data$set_size <- as.numeric(as.character(final_data$set_size))

    plot <- ggplot(final_data, aes(x = set_size,
                                   y = mean_value)) +
      geom_errorbar(aes(ymax = mean_value + se_value,
                        ymin = mean_value - se_value),
                    width = 0.00) +
      geom_point(size = 2.5) +
      theme_bw() +
      labs(x = "Set Size",
           y = "Mean Absolute Error")

    # rename the final_data frame
    colnames(final_data)[1] <- set_size_var

  }


  # both set size & condition manipulation
  if(!is.null(set_size_var) && !is.null(condition_var)){

    # ensure set size is numeric
    final_data$set_size <- as.numeric(as.character(final_data$set_size))

    # add some jitter to the plotting position
    pd = position_dodge(0.2)

    plot <- ggplot(final_data, aes(x = set_size,
                                   y = mean_value,
                                   group = condition)) +
      geom_errorbar(aes(ymax = mean_value + se_value,
                        ymin = mean_value - se_value,
                        colour = condition),
                    width = 0.00,
                    position = pd) +
      geom_point(aes(colour = condition),
                 position = pd,
                 size = 2.5) +
      theme_bw() +
      scale_colour_brewer(palette = "Dark2", name = condition_var) +
      labs(x = "Set Size",
           y = "Mean Absolute Error")

    # rename the final_data frame
    colnames(final_data)[1] <- set_size_var
    colnames(final_data)[2] <- condition_var

  }


  # return the plot & the plot data
  if(return_data == TRUE){
    return(list(plot = plot, data = final_data))
  } else {
    return(plot)
  }


}




# plot behavioral error --------------------------------------------------
#' Plot response error of behavioural data
#'
#'Function to plot the response error in behavioural data. Requires a data
#'frame that (at least) has target value data and participant response data.
#'
#'@param data A data frame with columns containing: participant identifier
#'('id_var'); the participants' response per trial ('response_var'); the
#'target value ('target_var'); and, if applicable, the set size of each
#'response ('set_size_var'), and the condition of each response
#'('condition_var').
#'@param unit The unit of measurement in the data frame: "degrees"
#'(measurement is in degrees, from 0 to 360); "degrees_180 (measurement is in
#'degrees, but limited to 0 to 180); or "radians" (measurement is in radians,
#'from pi to 2 * pi, but could also be already in -pi to pi).
#'@param id_var The column name coding for participant id. If the data is from
#'a single participant (i.e., there is no id column) set to "NULL".
#'@param response_var The column name coding for the participants' responses.
#'@param target_var The column name coding for the target value.
#'@param set_size_var The column name (if applicable) coding for the set
#'size of each response.
#'@param condition_var The column name (if applicable) coding for the
#'condition of each response.
#'@param return_data A boolean (TRUE or FALSE) indicating whether the data for
#'the plot should be returned.
#'
#'@examples
#'data(example_data)
#'plot_error(example_data, condition_var = "condition")
#'
#' @importFrom stats sd
#' @importFrom dplyr %>%
#' @importFrom dplyr summarise
#' @importFrom dplyr group_by
#' @importFrom dplyr rename
#' @importFrom graphics hist
#' @export
plot_error <- function(data,
                       unit = "degrees",
                       id_var = "id",
                       response_var = "response",
                       target_var = "target",
                       set_size_var = NULL,
                       condition_var = NULL,
                       return_data = FALSE){


  # establish the break points of the density plot
  break_points <- round(seq(from = -pi, to = pi, length.out = 18), 3)

  # get the list of participant ids
  ids <- unique(data[[id_var]])


  # calculate response error mapped onto circular space ------
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



  # find mean error ----

  # no set size or condition manipulation
  if(is.null(set_size_var) && is.null(condition_var)){

    if(!is.null(id_var)){
      final_data <- data %>%
        group_by(id) %>%
        summarise(y = hist(error, breaks = break_points, plot = FALSE)$density,
                  x = hist(error, breaks = break_points, plot = FALSE)$mids) %>%
        group_by(x) %>%
        summarise(mean_error = mean(y),
                  se_error = (sd(y) / sqrt(length(y))))
    } else{
      final_data <- data %>%
        summarise(y = hist(error, breaks = break_points, plot = FALSE)$density,
                  x = hist(error, breaks = break_points, plot = FALSE)$mids) %>%
        group_by(x) %>%
        summarise(mean_error = mean(y),
                  se_error = (sd(y) / sqrt(length(y))))
    }
  }


  # no set size manipulation but there is a condition manipulation
  if(is.null(set_size_var) && !is.null(condition_var)){

    data$condition <- as.factor(data[[condition_var]])

    if(!is.null(id_var)){
      final_data <- data %>%
        group_by(id, condition) %>%
        summarise(y = hist(error, breaks = break_points, plot = FALSE)$density,
                  x = hist(error, breaks = break_points, plot = FALSE)$mids) %>%
        group_by(condition, x) %>%
        summarise(mean_error = mean(y),
                  se_error = (sd(y) / sqrt(length(y))))
    } else{
      final_data <- data %>%
        group_by(condition) %>%
        summarise(y = hist(error, breaks = break_points, plot = FALSE)$density,
                  x = hist(error, breaks = break_points, plot = FALSE)$mids) %>%
        group_by(condition, x) %>%
        summarise(mean_error = mean(y),
                  se_error = (sd(y) / sqrt(length(y))))
    }
  }


  # set size manipulation, but no condition manipulation
  if(!is.null(set_size_var) && is.null(condition_var)){
    data$set_size <- data[[set_size_var]]

    if(!is.null(id_var)){
      final_data <- data %>%
        group_by(id, set_size) %>%
        summarise(y = hist(error, breaks = break_points, plot = FALSE)$density,
                  x = hist(error, breaks = break_points, plot = FALSE)$mids) %>%
        group_by(set_size, x) %>%
        summarise(mean_error = mean(y),
                  se_error = (sd(y) / sqrt(length(y))))
    } else{
      final_data <- data %>%
        group_by(set_size) %>%
        summarise(y = hist(error, breaks = break_points, plot = FALSE)$density,
                  x = hist(error, breaks = break_points, plot = FALSE)$mids) %>%
        group_by(set_size, x) %>%
        summarise(mean_error = mean(y),
                  se_error = (sd(y) / sqrt(length(y))))
    }
  }


  # both set size & condition manipulation
  if(!is.null(set_size_var) && !is.null(condition_var)){
    data$set_size <- data[[set_size_var]]
    data$condition <- as.factor(data[[condition_var]])

    if(!is.null(id_var)){
      final_data <- data %>%
        group_by(id, condition, set_size) %>%
        summarise(y = hist(error, breaks = break_points, plot = FALSE)$density,
                  x = hist(error, breaks = break_points, plot = FALSE)$mids) %>%
        group_by(set_size, condition, x) %>%
        summarise(mean_error = mean(y),
                  se_error = (sd(y) / sqrt(length(y))))
    } else{
      final_data <- data %>%
        group_by(condition, set_size) %>%
        summarise(y = hist(error, breaks = break_points, plot = FALSE)$density,
                  x = hist(error, breaks = break_points, plot = FALSE)$mids) %>%
        group_by(set_size, condition, x) %>%
        summarise(mean_error = mean(y),
                  se_error = (sd(y) / sqrt(length(y))))
    }
  }


  # plot the data ----

  # no set size or condition manipulation
  if(is.null(set_size_var) && is.null(condition_var)){

    plot <- ggplot(final_data, aes(x = x,
                                   y = mean_error)) +
      geom_errorbar(aes(ymax = mean_error + se_error,
                        ymin = mean_error - se_error),
                    width = 0.00) +
      geom_point() +
      theme_bw() +
      scale_x_continuous(limits = c(-pi, pi)) +
      scale_y_continuous(limits = c(0,
                                    max(final_data$mean_error) +
                                      max(final_data$se_error))) +
      labs(x = "Error (Radians)",
           y = "Probability Density")
    }


  # no set size manipulation but there is a condition manipulation
  if(is.null(set_size_var) && !is.null(condition_var)){

    plot <- ggplot(final_data, aes(x = x,
                                   y = mean_error)) +
      geom_errorbar(aes(ymax = mean_error + se_error,
                        ymin = mean_error - se_error),
                    width = 0.00) +
      geom_point() +
      theme_bw() +
      scale_x_continuous(limits = c(-pi, pi)) +
      scale_y_continuous(limits = c(0,
                                    max(final_data$mean_error) +
                                      max(final_data$se_error))) +
      labs(x = "Error (Radians)",
           y = "Probability Density") +
      facet_wrap(vars(condition), ncol = 4)

    # rename the final_data frame
    colnames(final_data)[1] <- condition_var

    }


  # set size manipulation, but no condition manipulation
  if(!is.null(set_size_var) && is.null(condition_var)){

    plot <- ggplot(final_data, aes(x = x,
                                   y = mean_error)) +
      geom_errorbar(aes(ymax = mean_error + se_error,
                        ymin = mean_error - se_error),
                    width = 0.00) +
      geom_point() +
      theme_bw() +
      scale_x_continuous(limits = c(-pi, pi)) +
      scale_y_continuous(limits = c(0,
                                    max(final_data$mean_error) +
                                      max(final_data$se_error))) +
      labs(x = "Error (Radians)",
           y = "Probability Density") +
      facet_wrap(vars(set_size), ncol = 4)

    # rename the final_data frame
    colnames(final_data)[1] <- set_size_var

  }


  # both set size & condition manipulation
  if(!is.null(set_size_var) && !is.null(condition_var)){

    # add position jitter to avoid over-plotting
    pd <- position_dodge(0.1)

    plot <- ggplot(final_data, aes(x = x,
                                   y = mean_error,
                                   group = condition)) +
      geom_errorbar(aes(ymax = mean_error + se_error,
                        ymin = mean_error - se_error,
                        colour = condition),
                    width = 0.00,
                    position = pd) +
      geom_point(aes(colour = condition),
                 position = pd) +
      theme_bw() +
      scale_x_continuous(limits = c(-pi, pi)) +
      scale_y_continuous(limits = c(0,
                                    max(final_data$mean_error) +
                                      max(final_data$se_error))) +
      scale_colour_brewer(palette = "Dark2", name = condition_var) +
      labs(x = "Error (Radians)",
           y = "Probability Density") +
      facet_wrap(vars(set_size))

    # rename the final_data frame
    colnames(final_data)[1] <- set_size_var
    colnames(final_data)[2] <- condition_var

  }


  # return the plot & the plot data
  if(return_data == TRUE){
    return(list(plot = plot, data = final_data))
  } else {
    return(plot)
  }

}



# plot behavioural precision ----------------------------------------------
#' Plot precision of behavioural data
#'
#'Function to plot the response precision of behavioural data. Requires a data
#'frame that (at least) has target value data and participant response data.
#'
#'@param data A data frame with columns containing: participant identifier
#'('id_var'); the participants' response per trial ('response_var'); the
#'target value ('target_var'); and, if applicable, the set size of each
#'response ('set_size_var'), and the condition of each response
#'('condition_var').
#'@param unit The unit of measurement in the data frame: "degrees"
#'(measurement is in degrees, from 0 to 360); "degrees_180 (measurement is in
#'degrees, but limited to 0 to 180); or "radians" (measurement is in radians,
#'from pi to 2 * pi, but could also be already in -pi to pi).
#'@param id_var The column name coding for participant id. If the data is from
#'a single participant (i.e., there is no id column) set to "NULL".
#'@param response_var The column name coding for the participants' responses
#'@param target_var The column name coding for the target value
#'@param set_size_var The column name (if applicable) coding for the set
#'size of each response
#'@param condition_var The column name (if applicable) coding for the
#'condition of each response
#'@param return_data A boolean (TRUE or FALSE) indicating whether the data for
#'the plot should be returned.
#'
#'@examples
#'library(tidyverse)
#'data(example_data)
#'plot_error(example_data, condition_var = "condition")
#'
#' @importFrom stats sd
#' @importFrom dplyr %>%
#' @importFrom dplyr summarise
#' @importFrom dplyr group_by
#' @importFrom graphics hist
#' @export
plot_precision <- function(data,
                           unit = "degrees",
                           id_var = "id",
                           response_var = "response",
                           target_var = "target",
                           set_size_var = NULL,
                           condition_var = NULL,
                           return_data = FALSE){

  # add id column
  data$id <- data[[id_var]]

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
                  bias = get_precision_single(error)[, 2]) %>%
        summarise(mean_precision = mean(precision),
                  se_precision = sd(precision) / sqrt(length(precision)),
                  mean_bias = mean(bias),
                  se_bias = sd(bias) / sqrt(length(bias)))
    } else{
      final_data <- data %>%
        summarise(precision = get_precision_single(error)[, 1],
                  bias = get_precision_single(error)[, 2]) %>%
        summarise(mean_precision = mean(precision),
                  se_precision = sd(precision) / sqrt(length(precision)),
                  mean_bias = mean(bias),
                  se_bias = sd(bias) / sqrt(length(bias)))
    }
  }


  # no set size manipulation but there is a condition manipulation
  if(is.null(set_size_var) && !is.null(condition_var)){

    data$condition <- as.factor(data[[condition_var]])

    if(!is.null(id_var)){
      final_data <- data %>%
        group_by(id, condition) %>%
        summarise(precision = get_precision_single(error)[, 1],
                  bias = get_precision_single(error)[, 2]) %>%
        group_by(condition) %>%
        summarise(mean_precision = mean(precision),
                  se_precision = sd(precision) / sqrt(length(precision)),
                  mean_bias = mean(bias),
                  se_bias = sd(bias) / sqrt(length(bias)))
    } else{
      final_data <- data %>%
        group_by(condition) %>%
        summarise(precision = get_precision_single(error)[, 1],
                  bias = get_precision_single(error)[, 2]) %>%
        group_by(condition) %>%
        summarise(mean_precision = mean(precision),
                  se_precision = sd(precision) / sqrt(length(precision)),
                  mean_bias = mean(bias),
                  se_bias = sd(bias) / sqrt(length(bias)))
    }
  }


  # set size manipulation, but no condition manipulation
  if(!is.null(set_size_var) && is.null(condition_var)){

    data$set_size <- data[[set_size_var]]

    if(!is.null(id_var)){
      final_data <- data %>%
        group_by(id, set_size) %>%
        summarise(precision = get_precision_single(error)[, 1],
                  bias = get_precision_single(error)[, 2]) %>%
        group_by(set_size) %>%
        summarise(mean_precision = mean(precision),
                  se_precision = sd(precision) / sqrt(length(precision)),
                  mean_bias = mean(bias),
                  se_bias = sd(bias) / sqrt(length(bias)))
    } else{
      final_data <- data %>%
        group_by(set_size) %>%
        summarise(precision = get_precision_single(error)[, 1],
                  bias = get_precision_single(error)[, 2]) %>%
        group_by(set_size) %>%
        summarise(mean_precision = mean(precision),
                  se_precision = sd(precision) / sqrt(length(precision)),
                  mean_bias = mean(bias),
                  se_bias = sd(bias) / sqrt(length(bias)))
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
                  bias = get_precision_single(error)[, 2]) %>%
        group_by(set_size, condition) %>%
        summarise(mean_precision = mean(precision),
                  se_precision = sd(precision) / sqrt(length(precision)),
                  mean_bias = mean(bias),
                  se_bias = sd(bias) / sqrt(length(bias)))
    } else{
      final_data <- data %>%
        group_by(condition, set_size) %>%
        summarise(precision = get_precision_single(error)[, 1],
                  bias = get_precision_single(error)[, 2]) %>%
        group_by(set_size, condition) %>%
        summarise(mean_precision = mean(precision),
                  se_precision = sd(precision) / sqrt(length(precision)),
                  mean_bias = mean(bias),
                  se_bias = sd(bias) / sqrt(length(bias)))
    }
}


  # plot the data----

  # no set size or condition manipulation
  if(is.null(set_size_var) && is.null(condition_var)){
    plot <- "NULL"
  }


  # no set size manipulation but there is a condition manipulation
  if(is.null(set_size_var) && !is.null(condition_var)){

    plot <- ggplot(final_data, aes(x = condition,
                                   y = mean_precision)) +
      geom_errorbar(aes(ymax = mean_precision + se_precision,
                        ymin = mean_precision - se_precision),
                    width = 0.00) +
      geom_point(size = 2.5) +
      theme_bw() +
      labs(x = condition_var,
           y = "Precision")

    # rename the final_data frame
    colnames(final_data)[1] <- condition_var

  }


  # set size manipulation but no condition manipulation
  if(!is.null(set_size_var) && is.null(condition_var)){

    # ensure set size is numeric
    final_data$set_size <- as.numeric(as.character(final_data$set_size))

    plot <- ggplot(final_data, aes(x = set_size,
                                   y = mean_precision)) +
      geom_errorbar(aes(ymax = mean_precision + se_precision,
                        ymin = mean_precision - se_precision),
                    width = 0.00) +
      geom_point(size = 2.5) +
      theme_bw() +
      labs(x = "Set Size",
           y = "Precision")

    # rename the final_data frame
    colnames(final_data)[1] <- set_size_var

  }


  # both set size & condition manipulation
  if(!is.null(set_size_var) && !is.null(condition_var)){

    # ensure set size is numeric
    final_data$set_size <- as.numeric(as.character(final_data$set_size))

    # add some jitter to the plotting position
    pd = position_dodge(0.2)

    plot <- ggplot(final_data, aes(x = set_size,
                                   y = mean_precision,
                                   group = condition)) +
      geom_errorbar(aes(ymax = mean_precision + se_precision,
                        ymin = mean_precision - se_precision,
                        colour = condition),
                    width = 0.00,
                    position = pd) +
      geom_point(aes(colour = condition),
                 position = pd,
                 size = 2.5) +
      theme_bw() +
      scale_colour_brewer(palette = "Dark2", name = condition_var) +
      labs(x = "Set Size",
           y = "Precision")

    # rename the final_data frame
    colnames(final_data)[1] <- set_size_var
    colnames(final_data)[2] <- condition_var

  }


  # return the plot & the plot data
  if(return_data == TRUE){
    return(list(plot = plot, data = final_data))
  } else {
    return(plot)
  }

}




# plot model fit ----------------------------------------------------------
#' Plot model fit against human error data (target errors)
#'
#'@param participant_data A data frame of the participant data, with columns
#'containing: participant identifier ('id_var'); the participants' response
#'per trial ('response_var'); the target value ('target_var'); and, if
#'applicable, the set size of each response ('set_size_var'), and the condition
#'of each response ('condition_var').
#'@param model_fit The model fit object to be plotted against participant data.
#'@param unit The unit of measurement in the data frame: "degrees"
#'(measurement is in degrees, from 0 to 360); "degrees_180 (measurement is in
#'degrees, but limited to 0 to 180); or "radians" (measurement is in radians,
#'from pi to 2 * pi, but could also be already in -pi to pi).
#'@param id_var The column name coding for participant id. If the data is from
#'a single participant (i.e., there is no id column) set to "NULL".
#'@param response_var The column name coding for the participants' responses
#'@param target_var The column name coding for the target value
#'@param set_size_var The column name (if applicable) coding for the set
#'size of each response
#'@param condition_var The column name (if applicable) coding for the
#'condition of each response
#'@param return_data A boolean (TRUE or FALSE) indicating whether the data for
#'the plot should be returned.
#'
#' @export
plot_model_fit <- function(participant_data,
                           model_fit,
                           unit = "degrees",
                           id_var = "id",
                           response_var = "response",
                           target_var = "target",
                           set_size_var = NULL,
                           condition_var = NULL){



  # check how many components in the model fit object
  if(is.null(participant_data$p_n)){
    components <- 2
  } else {
    components <- 3
  }

  # get the error data for the participant data
  human_error <- plot_error(participant_data,
                            id_var = id_var,
                            unit = unit,
                            response_var = response_var,
                            target_var = target_var,
                            set_size_var = set_size_var,
                            condition_var = condition_var,
                            return_data = TRUE)

  human_error <- human_error$data

  # no set size or condition manipulation
  if(is.null(set_size_var) && is.null(condition_var)){

    # get the mean K, p_t, and p_u parameters from the model fit
    # 2-component and 3-component model make same predictions for
    # target error
    mean_k <- mean(model_fit$K)
    mean_p_t <- mean(model_fit$p_t)

    if(components == 3){
      mean_p_u <- mean(model_fit$p_n) + mean(model_fit$p_u)
    } else {
      mean_p_u <- mean(model_fit$p_u)
    }


    # get the model predictions
    model_preds <- tibble(x = seq(-pi, pi, length.out = 1000),
                          y = vonmisespdf(x, 0, mean_k) * (mean_p_t) +
                            dunif(x, min = -pi, max = pi) * (mean_p_u))

    # plot the human data & model predictions
    plot <- ggplot(human_error, aes(x = x,
                                    y = mean_error)) +
      geom_line(data = model_preds,
                aes(x = x, y = y),
                alpha = 0.8,
                col = "#D95F02",
                lwd = 1.3) +
      geom_errorbar(aes(ymax = mean_error + se_error,
                        ymin = mean_error - se_error),
                    width = 0.00) +
      geom_point() +
      theme_bw() +
      labs(x = "Error (Radians)",
           y = "Probability Density")

  }


  # no set size manipulation but there is a condition manipulation
  if(is.null(set_size_var) && !is.null(condition_var)){
    human_error$condition <- human_error[[condition_var]]
    model_fit$condition <- model_fit[[condition_var]]

    # get the mean K, p_t, and p_u parameters from the model fit
    # 2-component and 3-component model make same predictions for
    # target error
    mean_k <- model_fit %>%
      group_by(condition) %>%
      summarise(mean_k = mean(K))
    mean_p_t <- model_fit %>%
      group_by(condition) %>%
      summarise(mean_p_t = mean(p_t))

    if(components == 3){
      mean_p_u <- model_fit %>%
        group_by(condition) %>%
        summarise(mean_p_u = mean(p_n) + mean(p_u))
    } else {
      mean_p_u <- model_fit %>%
        group_by(condition) %>%
        summarise(mean_p_u = mean(p_u))
    }


    # get the model predictions
    conditions <- unique(human_error$condition)

    for(i in 1:length(conditions)){

      level_k = mean_k %>%
        filter(condition == conditions[i]) %>%
        pull()

      level_p_t = mean_p_t %>%
        filter(condition == conditions[i]) %>%
        pull()

      level_p_u = mean_p_u %>%
        filter(condition == conditions[i]) %>%
        pull()

      level_preds <- tibble(x = seq(-pi, pi, length.out = 1000),
                            y = vonmisespdf(x, 0, level_k) * (level_p_t) +
                              dunif(x, min = -pi, max = pi) * (level_p_u))

      level_preds <- level_preds %>%
        mutate(condition = conditions[i])

      if(i == 1){
        model_preds <- level_preds
      } else {
        model_preds <- rbind(model_preds, level_preds)
      }
    }

    #---- plot the human data & model predictions
    plot <- ggplot(human_error, aes(x = x,
                                    y = mean_error)) +
      geom_line(data = model_preds,
                aes(x = x, y = y),
                alpha = 0.8,
                col = "#D95F02",
                lwd = 1.3) +
      geom_errorbar(aes(ymax = mean_error + se_error,
                        ymin = mean_error - se_error),
                    width = 0.00) +
      geom_point() +
      facet_wrap(vars(condition), ncol = 4) +
      theme_bw() +
      labs(x = "Error (Radians)",
           y = "Probability Density")

  }


  # set size manipulation, but no condition manipulation
  if(!is.null(set_size_var) && is.null(condition_var)){

    human_error$set_size <- human_error[[set_size_var]]
    model_fit$set_size <- model_fit[[set_size_var]]

    # get the mean K, p_t, and p_u parameters from the model fit
    # 2-component and 3-component model make same predictions for
    # target error
    mean_k <- model_fit %>%
      group_by(set_size) %>%
      summarise(mean_k = mean(K))
    mean_p_t <- model_fit %>%
      group_by(set_size) %>%
      summarise(mean_p_t = mean(p_t))

    if(components == 3){
      mean_p_u <- model_fit %>%
        group_by(set_size) %>%
        summarise(mean_p_u = mean(p_n) + mean(p_u))
    } else {
      mean_p_u <- model_fit %>%
        group_by(set_size) %>%
        summarise(mean_p_u = mean(p_u))
    }


    # get the model predictions
    set_sizes <- unique(model_fit$set_size)

    for(i in 1:length(set_sizes)){

      level_k = mean_k %>%
        filter(set_size == set_sizes[i]) %>%
        pull()

      level_p_t = mean_p_t %>%
        filter(set_size == set_sizes[i]) %>%
        pull()

      level_p_u = mean_p_u %>%
        filter(set_size == set_sizes[i]) %>%
        pull()

      level_preds <- tibble(x = seq(-pi, pi, length.out = 1000),
                            y = vonmisespdf(x, 0, level_k) * (level_p_t) +
                              dunif(x, min = -pi, max = pi) * (level_p_u))

      level_preds <- level_preds %>%
        mutate(set_size = set_sizes[i])

      if(i == 1){
        model_preds <- level_preds
      } else {
        model_preds <- rbind(model_preds, level_preds)
      }

    }


    #---- plot the human data & model predictions
    plot <- ggplot(human_error, aes(x = x,
                                    y = mean_error)) +
      geom_line(data = model_preds,
                aes(x = x, y = y),
                alpha = 0.8,
                col = "#D95F02",
                lwd = 1.3) +
      geom_errorbar(aes(ymax = mean_error + se_error,
                        ymin = mean_error - se_error),
                    width = 0.00) +
      geom_point() +
      facet_wrap(vars(set_size), ncol = 2) +
      theme_bw() +
      scale_colour_brewer(palette = "Dark2") +
      labs(x = "Error (Radians)",
           y = "Probability Density")

  }

  # set size manipulation and condition manipulation
  if(!is.null(set_size_var) && !is.null(condition_var)){

    human_error$set_size <- human_error[[set_size_var]]
    human_error$condition <- human_error[[condition_var]]
    model_fit$set_size <- model_fit[[set_size_var]]
    model_fit$condition <- model_fit[[condition_var]]

    # get the mean K, p_t, and p_u parameters from the model fit
    # 2-component and 3-component model make same predictions for
    # target error
    mean_k <- model_fit %>%
      group_by(set_size, condition) %>%
      summarise(mean_k = mean(K))
    mean_p_t <- model_fit %>%
      group_by(set_size, condition) %>%
      summarise(mean_p_t = mean(p_t))

    if(components == 3){
      mean_p_u <- model_fit %>%
        group_by(set_size, condition) %>%
        summarise(mean_p_u = mean(p_n) + mean(p_u))
    } else {
      mean_p_u <- model_fit %>%
        group_by(set_size, condition) %>%
        summarise(mean_p_u = mean(p_u))
    }


    # get the model predictions for each level
    set_sizes <- unique(model_fit$set_size)
    conditions <- unique(model_fit$condition)

    for(i in 1:length(set_sizes)){
      for(j in 1:length(conditions)){

        level_k <- mean_k %>%
          filter(set_size == set_sizes[i]) %>%
          filter(condition == conditions[j]) %>%
          pull()

        level_p_t <-  mean_p_t %>%
          filter(set_size == set_sizes[i]) %>%
          filter(condition == conditions[j]) %>%
          pull()

        level_p_u <-  mean_p_u %>%
          filter(set_size == set_sizes[i]) %>%
          filter(condition == conditions[j]) %>%
          pull()

        level_preds <- tibble(x = seq(-pi, pi, length.out = 1000),
                              y = vonmisespdf(x, 0, level_k) * (level_p_t) +
                                dunif(x, min = -pi, max = pi) * (level_p_u))

        level_preds <- level_preds %>%
          mutate(set_size = set_sizes[i]) %>%
          mutate(condition = conditions[j])

        if(i == 1 && j == 1){
          model_preds <- level_preds
        } else {
          model_preds <- rbind(model_preds, level_preds)
        }
      }
    }

    #---- plot the human data & model predictions
    human_error$condition <- as.factor(human_error[[condition_var]])
    model_preds$condition <- as.factor(model_preds$condition)

    plot <- ggplot(human_error, aes(x = x,
                                    y = mean_error,
                                    group = condition)) +
      geom_line(data = model_preds,
                aes(x = x,
                    y = y,
                    colour = condition),
                alpha = 0.8,
                lwd = 1) +
      geom_errorbar(aes(ymax = mean_error + se_error,
                        ymin = mean_error - se_error,
                        colour = condition),
                    width = 0.00) +
      geom_point(aes(colour = condition)) +
      facet_wrap(vars(set_size), ncol = 2) +
      theme_bw() +
      scale_colour_brewer(palette = "Dark2") +
      labs(x = "Error (Radians)",
           y = "Probability Density")


  }

  return(plot)
}




# plot model parameters --------------------------------------------------
#' Plot best-fitting parameters of model fit
#'
#'Function to plot the best-fitting parameters of either the 2-component or
#'3-component model. .
#'
#'@param model_fit The model fit object containing the parameters to be
#'plotted.
#'@param id_var The column name coding for participant id. If the data is from
#'a single participant (i.e., there is no id column) set to NULL.
#'@param set_size_var The column name (if applicable) coding for the set
#'size of each response.
#'@param condition_var The column name (if applicable) coding for the
#'condition of each response.
#'@param return_data A boolean (TRUE or FALSE) indicating whether the data for
#'the plot should be returned.
#'
#' @importFrom stats sd
#' @importFrom dplyr %>%
#' @importFrom dplyr summarise
#' @importFrom dplyr group_by
#' @importFrom dplyr rename
#' @importFrom graphics hist
#' @export
plot_parameters <- function(model_fit,
                            id_var = "id",
                            set_size_var = NULL,
                            condition_var = NULL,
                            return_data = FALSE){

  if(is.null(id_var)){
    return("Error: Only use this function for multiple participants.")
  }


  # check how many components in the model fit object
  if(is.null(model_fit$p_n)){
    components <- 2
  } else{
    components <- 3
  }


  # no set size or condition manipulation
  if(is.null(set_size_var) && is.null(condition_var)){

    # get plot data
    plot_data <- model_fit %>%
      pivot_longer(K:p_u,
                    names_to = "Parameter") %>%
      mutate(Parameter = as.factor(Parameter)) %>%
      group_by(Parameter) %>%
      summarise(mean_value = mean(value),
                se_value = sd(value) / sqrt(length(value)))


    # do the plot if it's the 2-component model
    if(components == 2){

      plot_data$Parameter <- factor(plot_data$Parameter,
                                    levels = c("K", "p_t", "p_u"))

      plot <- ggplot(plot_data, aes(x = Parameter,
                                    y = mean_value)) +
        geom_errorbar(aes(ymax = mean_value + se_value,
                          ymin = mean_value - se_value),
                      width = 0.00) +
        geom_point() +
        labs(y = "Mean Parameter Value") +
        facet_wrap(vars(Parameter), ncol = 3, scales = "free") +
        theme_bw()
    }


    # do the plot if it's the 3-component model
    if(components == 3){

      plot_data$Parameter <- factor(plot_data$Parameter,
                                    levels = c("K", "p_t", "p_n", "p_u"))

      plot <- ggplot(plot_data, aes(x = Parameter,
                                    y = mean_value)) +
        geom_errorbar(aes(ymax = mean_value + se_value,
                          ymin = mean_value - se_value),
                      width = 0.00) +
        geom_point() +
        labs(y = "Mean Parameter Value") +
        facet_wrap(vars(Parameter), ncol = 4, scales = "free") +
        theme_bw()

    }

  }


  # no set size manipulation but there is a condition manipulation
  if(is.null(set_size_var) && !is.null(condition_var)){

    model_fit$condition <- model_fit[[condition_var]]

    # get the plot data
    if(components == 2){
      plot_data <- model_fit %>%
        select(id_var, K, p_t, p_u, condition) %>%
        pivot_longer(K:p_u,
                     names_to = "Parameter") %>%
        group_by(condition, Parameter) %>%
        summarise(mean_value = mean(value),
                  se_value = sd(value) / sqrt(length(value)))
    }
    if(components == 3){
      plot_data <- model_fit %>%
        select(id_var, K, p_t, p_n, p_u, condition) %>%
        pivot_longer(K:p_u,
                     names_to = "Parameter") %>%
        group_by(condition, Parameter) %>%
        summarise(mean_value = mean(value),
                  se_value = sd(value) / sqrt(length(value)))
    }

    ## do the plot if it's the 2-component model

    # ensure condition is factor
    plot_data$condition <- as.factor(plot_data$condition)

    if(components == 2){
      plot_data$Parameter <- factor(plot_data$Parameter,
                                    levels = c("K", "p_t", "p_u"))
      plot <- ggplot(plot_data, aes(x = condition,
                                    y = mean_value)) +
        geom_errorbar(aes(ymax = mean_value + se_value,
                          ymin = mean_value - se_value),
                      width = 0.00) +
        geom_point() +
        labs(y = "Mean Parameter Value") +
        labs(x = condition_var) +
        facet_wrap(vars(Parameter), ncol = 3, scales = "free") +
        theme_bw()
    }


    ## do the plot if it's the 3-component model

    # ensure condition is factor
    plot_data$condition <- as.factor(plot_data$condition)

    if(components == 3){
      plot_data$Parameter <- factor(plot_data$Parameter,
                                    levels = c("K", "p_t", "p_n", "p_u"))
      plot <- ggplot(plot_data, aes(x = condition,
                                    y = mean_value)) +
        geom_errorbar(aes(ymax = mean_value + se_value,
                          ymin = mean_value - se_value),
                      width = 0.00) +
        geom_point() +
        labs(y = "Mean Parameter Value") +
        labs(x = condition_var) +
        facet_wrap(vars(Parameter), ncol = 4, scales = "free") +
        theme_bw()
    }


    ## rename columns to user-determined names
    participant_condition <- model_fit %>%
      select(all_of(condition_var))
    function_condition <- model_fit %>%
      select(condition)

    if(colnames(participant_condition) != colnames(function_condition)){
      model_fit <- model_fit %>%
        select(-condition)
    }

  }

  # set size manipulation, but no condition manipulation
  if(!is.null(set_size_var) && is.null(condition_var)){

    model_fit$set_size <- model_fit[[set_size_var]]

    # get the plot data
    if(components == 2){
      plot_data <- model_fit %>%
        select(id_var, K, p_t, p_u, set_size) %>%
        pivot_longer(K:p_u,
                     names_to = "Parameter") %>%
        group_by(set_size, Parameter) %>%
        summarise(mean_value = mean(value),
                  se_value = sd(value) / sqrt(length(value)))
    }
    if(components == 3){
      plot_data <- model_fit %>%
        select(id_var, K, p_t, p_n, p_u, set_size) %>%
        pivot_longer(K:p_u,
                     names_to = "Parameter") %>%
        group_by(set_size, Parameter) %>%
        summarise(mean_value = mean(value),
                  se_value = sd(value) / sqrt(length(value)))
    }

    ## do the plot if it's the 2-component model

    # ensure set size is numeric
    plot_data$set_size <- as.numeric(as.character(plot_data$set_size))

    if(components == 2){
      plot_data$Parameter <- factor(plot_data$Parameter,
                                    levels = c("K", "p_t", "p_u"))
      plot <- ggplot(plot_data, aes(x = set_size,
                                    y = mean_value)) ++
        geom_errorbar(aes(ymax = mean_value + se_value,
                          ymin = mean_value - se_value),
                      width = 0.00) +
        geom_point() +
        labs(y = "Mean Parameter Value") +
        labs(x = "Set Size") +
        facet_wrap(vars(Parameter), ncol = 3, scales = "free") +
        theme_bw()
    }


    ## do the plot if it's the 3-component model

    # ensure set size is numeric
    plot_data$set_size <- as.numeric(as.character(plot_data$set_size))

    if(components == 3){
      plot_data$Parameter <- factor(plot_data$Parameter,
                                    levels = c("K", "p_t", "p_n", "p_u"))
      plot <- ggplot(plot_data, aes(x = set_size,
                                    y = mean_value)) +
        geom_errorbar(aes(ymax = mean_value + se_value,
                          ymin = mean_value - se_value),
                      width = 0.00) +
        geom_point() +
        labs(y = "Mean Parameter Value") +
        labs(x = "Set Size") +
        facet_wrap(vars(Parameter), ncol = 4, scales = "free") +
        theme_bw()
    }

    ## rename columns to user-determined names
    participant_set_size <- model_fit %>%
      select(all_of(set_size_var))
    function_set_size <- model_fit %>%
      select(set_size)

    if(colnames(participant_set_size) != colnames(function_set_size)){
      model_fit <- model_fit %>%
        select(-set_size)
    }

  }


  # both set size & condition manipulation
  if(!is.null(set_size_var) && !is.null(condition_var)){

    model_fit$condition <- model_fit[[condition_var]]
    model_fit$set_size <- model_fit[[set_size_var]]

    # get the plot data
    if(components == 2){
      plot_data <- model_fit %>%
        select(id_var, K, p_t, p_u, set_size, condition) %>%
        pivot_longer(K:p_u,
                     names_to = "Parameter") %>%
        group_by(condition, set_size, Parameter) %>%
        summarise(mean_value = mean(value),
                  se_value = sd(value) / sqrt(length(value)))
    }
    if(components == 3){
      plot_data <- model_fit %>%
        select(id_var, K, p_t, p_n, p_u, set_size, condition) %>%
        pivot_longer(K:p_u,
                     names_to = "Parameter") %>%
        group_by(condition, set_size, Parameter) %>%
        summarise(mean_value = mean(value),
                  se_value = sd(value) / sqrt(length(value)))
    }

    ## do the plot if it's the 2-component model

    # ensure set size is numeric & condition is factor
    plot_data$set_size <- as.numeric(as.character(plot_data$set_size))
    plot_data$condition <- as.factor(plot_data$condition)

    if(components == 2){
      plot_data$Parameter <- factor(plot_data$Parameter,
                                    levels = c("K", "p_t", "p_u"))
      plot <- ggplot(plot_data, aes(x = set_size,
                                    y = mean_value)) +
        geom_errorbar(aes(ymax = mean_value + se_value,
                          ymin = mean_value - se_value),
                      width = 0.00) +
        geom_point() +
        scale_colour_brewer(palette = "Dark2", name = condition_var) +
        labs(x = "Set Size",
             y = "Mean Parameter Value") +
        facet_wrap(vars(Parameter), ncol = 3, scales = "free") +
        theme_bw()
    }


    ## do the plot if it's the 3-component model

    # ensure set size is numeric & condition is factor
    plot_data$set_size <- as.numeric(as.character(plot_data$set_size))
    plot_data$condition <- as.factor(plot_data$condition)

    if(components == 3){
      plot_data$Parameter <- factor(plot_data$Parameter,
                                    levels = c("K", "p_t", "p_n", "p_u"))
      pd <- position_dodge(0.3)
      plot <- ggplot(plot_data, aes(x = set_size,
                                    y = mean_value,
                                    group = condition)) +
        geom_errorbar(aes(ymax = mean_value + se_value,
                          ymin = mean_value - se_value,
                          colour = condition),
                      width = 0.00,
                      position = pd) +
        geom_point(aes(colour = condition),
                   position = pd) +
        scale_colour_brewer(palette = "Dark2", name = condition_var) +
        labs(x = "Set Size",
             y = "Mean Parameter Value") +
        facet_wrap(vars(Parameter), ncol = 4, scales = "free") +
        theme_bw()
    }


    ## rename columns to user-determined names

    # set size
    participant_set_size <- model_fit %>%
      select(all_of(set_size_var))
    function_set_size <- model_fit %>%
      select(set_size)

    if(colnames(participant_set_size) != colnames(function_set_size)){
      model_fit <- model_fit %>%
        select(-set_size)
    }

    # condition
    participant_condition <- model_fit %>%
      select(all_of(condition_var))
    function_condition <- model_fit %>%
      select(condition)

    if(colnames(participant_condition) != colnames(function_condition)){
      model_fit <- model_fit %>%
        select(-condition)
    }

  }

  # return the plot & the plot data
  if(return_data == TRUE){
    return(list(plot = plot, data = model_fit))
  } else {
    return(plot)
  }



}
