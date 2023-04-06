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
#'@param palette A character stating the preferred colour palette to use. To
#'see all available palettes, type ?scale_colour_brewer into the console.
#'
#'@return If \code{return_data} is set to \code{FALSE} (which it is by default),
#' the function returns a ggplot2 object visualising the summary statistic
#' averaged across participants (if applicable) per set-size (if applicable)
#' and condition (if applicable).
#'
#' @return If \code{return_data} is set to \code{TRUE}, the function returns a
#' list with two components:
#'
#'  \itemize{
#'  \item \code{plot:} The ggplot2 object.
#'  \item \code{data:} A data frame with the data used to generate the plot.
#'  }
#'
#'@examples
#' plot_summary_statistic(bays2009_full,
#'                       unit = "radians",
#'                       statistic = "precision",
#'                       set_size_var = "set_size",
#'                       condition_var = "duration")
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
                                   return_data = FALSE,
                                   palette = "Dark2"){


  if(statistic == "precision"){
    plot <- plot_precision(data = data,
                   unit = unit,
                   id_var = id_var,
                   response_var = response_var,
                   target_var = target_var,
                   set_size_var = set_size_var,
                   condition_var = condition_var,
                   return_data = return_data,
                   palette = palette)
  }

  if(statistic == "mean_absolute_error"){
    plot <- plot_mean_absolute_error(data = data,
                             unit = unit,
                             id_var = id_var,
                             response_var = response_var,
                             target_var = target_var,
                             set_size_var = set_size_var,
                             condition_var = condition_var,
                             return_data = return_data,
                             palette = palette)
  }


  if(statistic == "resultant_vector_length"){
    plot <- plot_resultant_vector_length(data = data,
                                 unit = unit,
                                 id_var = id_var,
                                 response_var = response_var,
                                 target_var = target_var,
                                 set_size_var = set_size_var,
                                 condition_var = condition_var,
                                 return_data = return_data,
                                 palette = palette)
  }


  return(plot)

}





# plot resultant vector length --------------------------------------------

#'  Plot resultant vector length of behavioural data
#'
#' Function to plot the resultant vector length of response error in
#'  behavioural data. Requires a data frame that (at least) has target value
#'  data and participant response data.
#'
#' @param data A data frame with columns containing: participant identifier
#' ('id_var'); the participants' response per trial ('response_var'); the
#' target value ('target_var'); and, if applicable, the set size of each
#' response ('set_size_var'), and the condition of each response
#' ('condition_var').
#' @param unit The unit of measurement in the data frame: "degrees"
#' (measurement is in degrees, from 0 to 360); "degrees_180 (measurement is in
#' degrees, but limited to 0 to 180); or "radians" (measurement is in radians,
#' from pi to 2 * pi, but could also be already in -pi to pi).
#' @param id_var The column name coding for participant id. If the data is from
#' a single participant (i.e., there is no id column) set to "NULL".
#' @param response_var The column name coding for the participants' responses.
#' @param target_var The column name coding for the target value.
#' @param set_size_var The column name (if applicable) coding for the set
#' size of each response.
#' @param condition_var The column name (if applicable) coding for the
# 'condition of each response.
#' @param return_data A boolean (TRUE or FALSE) indicating whether the data for
#' the plot should be returned.
#' @param palette A character stating the preferred colour palette to use. To
#' see all available palettes, type ?scale_colour_brewer into the console.
#'
#' @importFrom stats sd
#' @importFrom dplyr %>%
#' @importFrom dplyr summarise
#' @importFrom dplyr group_by
#' @importFrom dplyr rename
#' @importFrom graphics hist
#' @importFrom ggplot2 ggplot aes geom_errorbar labs geom_point theme_bw
#' position_dodge
#' @importFrom rlang .data
#' @noRd
plot_resultant_vector_length <- function(data,
                                         unit = "degrees",
                                         id_var = "id",
                                         response_var = "response",
                                         target_var = "target",
                                         set_size_var = NULL,
                                         condition_var = NULL,
                                         return_data = FALSE,
                                         palette = "Dark2"){

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
        group_by(.data$id) %>%
        summarise(individual_value =
                    get_resultant_vector_length(.data$error)) %>%
        summarise(mean_value = mean(.data$individual_value),
                  se_value = sd(.data$individual_value) /
                    sqrt(length(.data$individual_value)))
    } else{
      final_data <- data %>%
        summarise(individual_value =
                    get_resultant_vector_length(.data$error)) %>%
        summarise(mean_value = mean(.data$individual_value),
                  se_value = sd(.data$individual_value) /
                    sqrt(length(.data$individual_value)))
    }
  }


  # no set size manipulation but there is a condition manipulation
  if(is.null(set_size_var) && !is.null(condition_var)){

    data$condition <- as.factor(data[[condition_var]])

    if(!is.null(id_var)){
      final_data <- data %>%
        group_by(.data$id, .data$condition) %>%
        summarise(individual_value =
                    get_resultant_vector_length(.data$error)) %>%
        group_by(.data$condition) %>%
        summarise(mean_value = mean(.data$individual_value),
                  se_value = sd(.data$individual_value) /
                    sqrt(length(.data$individual_value)))
    } else{
      final_data <- data %>%
        group_by(.data$condition) %>%
        summarise(individual_value =
                    get_resultant_vector_length(.data$error)) %>%
        group_by(.data$condition) %>%
        summarise(mean_value = mean(.data$individual_value),
                  se_value = sd(.data$individual_value) /
                    sqrt(length(.data$individual_value)))
    }
  }


  # set size manipulation, but no condition manipulation
  if(!is.null(set_size_var) && is.null(condition_var)){

    data$set_size <- data[[set_size_var]]

    if(!is.null(id_var)){
      final_data <- data %>%
        group_by(.data$id, .data$set_size) %>%
        summarise(individual_value =
                    get_resultant_vector_length(.data$error)) %>%
        group_by(.data$set_size) %>%
        summarise(mean_value = mean(.data$individual_value),
                  se_value = sd(.data$individual_value) /
                    sqrt(length(.data$individual_value)))
    } else{
      final_data <- data %>%
        group_by(.data$set_size) %>%
        summarise(individual_value =
                    get_resultant_vector_length(.data$error)) %>%
        group_by(.data$set_size) %>%
        summarise(mean_value = mean(.data$individual_value),
                  se_value = sd(.data$individual_value) /
                    sqrt(length(.data$individual_value)))
    }
  }


  # both set size & condition manipulation
  if(!is.null(set_size_var) && !is.null(condition_var)){

    data$set_size <- data[[set_size_var]]
    data$condition <- as.factor(data[[condition_var]])

    if(!is.null(id_var)){
      final_data <- data %>%
        group_by(.data$id, .data$condition, .data$set_size) %>%
        summarise(individual_value =
                    get_resultant_vector_length(.data$error)) %>%
        group_by(.data$set_size, .data$condition) %>%
        summarise(mean_value = mean(.data$individual_value),
                  se_value = sd(.data$individual_value) /
                    sqrt(length(.data$individual_value)))
    } else{
      final_data <- data %>%
        group_by(.data$condition, .data$set_size) %>%
        summarise(individual_value =
                    get_resultant_vector_length(.data$error)) %>%
        group_by(.data$set_size, .data$condition) %>%
        summarise(mean_value = mean(.data$individual_value),
                  se_value = sd(.data$individual_value) /
                    sqrt(length(.data$individual_value)))
    }
  }


  # plot the data----

  # no set size or condition manipulation
  if(is.null(set_size_var) && is.null(condition_var)){
    plot <- "NULL"
  }


  # no set size manipulation but there is a condition manipulation
  if(is.null(set_size_var) && !is.null(condition_var)){

    plot <- ggplot(final_data, aes(x = .data$condition,
                                   y = .data$mean_value)) +
      geom_errorbar(aes(ymax = .data$mean_value + .data$se_value,
                        ymin = .data$mean_value - .data$se_value),
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

    plot <- ggplot(final_data, aes(x = .data$set_size,
                                   y = .data$mean_value)) +
      geom_errorbar(aes(ymax = .data$mean_value + .data$se_value,
                        ymin = .data$mean_value - .data$se_value),
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

    plot <- ggplot(final_data, aes(x = .data$set_size,
                                   y = .data$mean_value,
                                   group = .data$condition)) +
      geom_errorbar(aes(ymax = .data$mean_value + .data$se_value,
                        ymin = .data$mean_value - .data$se_value,
                        colour = .data$condition),
                    width = 0.00,
                    position = pd) +
      geom_point(aes(colour = .data$condition),
                 position = pd,
                 size = 2.5) +
      theme_bw() +
      scale_colour_brewer(palette = palette, name = condition_var) +
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

# Plot mean absolute error of behavioural data
#
#Function to plot the mean absolute error of response error in behavioural
#data. Requires a data frame that (at least) has target value data and
#participant response data.
#
#@param data A data frame with columns containing: participant identifier
#('id_var'); the participants' response per trial ('response_var'); the
#target value ('target_var'); and, if applicable, the set size of each
#response ('set_size_var'), and the condition of each response
#('condition_var').
#@param unit The unit of measurement in the data frame: "degrees"
#(measurement is in degrees, from 0 to 360); "degrees_180 (measurement is in
#degrees, but limited to 0 to 180); or "radians" (measurement is in radians,
#from pi to 2 * pi, but could also be already in -pi to pi).
#@param id_var The column name coding for participant id. If the data is from
#a single participant (i.e., there is no id column) set to "NULL".
#@param response_var The column name coding for the participants' responses.
#@param target_var The column name coding for the target value.
#@param set_size_var The column name (if applicable) coding for the set
#size of each response.
#@param condition_var The column name (if applicable) coding for the
#condition of each response.
#@param return_data A boolean (TRUE or FALSE) indicating whether the data for
#the plot should be returned.
#
#@examples
#data(bays2009_full)
#plot_mean_absolute_error(bays2009_full,
#                         unit = "radians",
#                         set_size_var = "set_size",
#                         condition_var = "duration")
#
#' @importFrom stats sd
#' @importFrom dplyr %>%
#' @importFrom dplyr summarise
#' @importFrom dplyr group_by
#' @importFrom dplyr rename
#' @importFrom graphics hist
#' @importFrom ggplot2 ggplot aes geom_errorbar labs geom_point theme_bw
#' position_dodge
#' @importFrom rlang .data
#' @noRd
plot_mean_absolute_error <- function(data,
                                     unit = "degrees",
                                     id_var = "id",
                                     response_var = "response",
                                     target_var = "target",
                                     set_size_var = NULL,
                                     condition_var = NULL,
                                     return_data = FALSE,
                                     palette = "Dark2"){

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
        group_by(.data$id) %>%
        summarise(individual_value = get_mean_absolute_error(.data$error)) %>%
        summarise(mean_value = mean(.data$individual_value),
                  se_value = sd(.data$individual_value) /
                    sqrt(length(.data$individual_value)))
    } else{
      final_data <- data %>%
        summarise(individual_value = get_mean_absolute_error(.data$error)) %>%
        summarise(mean_value = mean(.data$individual_value),
                  se_value = sd(.data$individual_value) /
                    sqrt(length(.data$individual_value)))
    }
  }


  # no set size manipulation but there is a condition manipulation
  if(is.null(set_size_var) && !is.null(condition_var)){

    data$condition <- as.factor(data[[condition_var]])

    if(!is.null(id_var)){
      final_data <- data %>%
        group_by(.data$id, .data$condition) %>%
        summarise(individual_value = get_mean_absolute_error(.data$error)) %>%
        group_by(.data$condition) %>%
        summarise(mean_value = mean(.data$individual_value),
                  se_value = sd(.data$individual_value) /
                    sqrt(length(.data$individual_value)))
    } else{
      final_data <- data %>%
        group_by(.data$condition) %>%
        summarise(individual_value = get_mean_absolute_error(.data$error)) %>%
        group_by(.data$condition) %>%
        summarise(mean_value = mean(.data$individual_value),
                  se_value = sd(.data$individual_value) /
                    sqrt(length(.data$individual_value)))
    }
  }


  # set size manipulation, but no condition manipulation
  if(!is.null(set_size_var) && is.null(condition_var)){

    data$set_size <- data[[set_size_var]]

    if(!is.null(id_var)){
      final_data <- data %>%
        group_by(.data$id, .data$set_size) %>%
        summarise(individual_value = get_mean_absolute_error(.data$error)) %>%
        group_by(.data$set_size) %>%
        summarise(mean_value = mean(.data$individual_value),
                  se_value = sd(.data$individual_value) /
                    sqrt(length(.data$individual_value)))
    } else{
      final_data <- data %>%
        group_by(.data$set_size) %>%
        summarise(individual_value = get_mean_absolute_error(.data$error)) %>%
        group_by(.data$set_size) %>%
        summarise(mean_value = mean(.data$individual_value),
                  se_value = sd(.data$individual_value) /
                    sqrt(length(.data$individual_value)))
    }
  }


  # both set size & condition manipulation
  if(!is.null(set_size_var) && !is.null(condition_var)){

    data$set_size <- data[[set_size_var]]
    data$condition <- as.factor(data[[condition_var]])

    if(!is.null(id_var)){
      final_data <- data %>%
        group_by(.data$id, .data$condition, .data$set_size) %>%
        summarise(individual_value = get_mean_absolute_error(.data$error)) %>%
        group_by(.data$set_size, .data$condition) %>%
        summarise(mean_value = mean(.data$individual_value),
                  se_value = sd(.data$individual_value) /
                    sqrt(length(.data$individual_value)))
    } else{
      final_data <- data %>%
        group_by(.data$condition, .data$set_size) %>%
        summarise(individual_value = get_mean_absolute_error(.data$error)) %>%
        group_by(.data$set_size, .data$condition) %>%
        summarise(mean_value = mean(.data$individual_value),
                  se_value = sd(.data$individual_value) /
                    sqrt(length(.data$individual_value)))
    }
  }


  # plot the data----

  # no set size or condition manipulation
  if(is.null(set_size_var) && is.null(condition_var)){
    plot <- "NULL"
  }


  # no set size manipulation but there is a condition manipulation
  if(is.null(set_size_var) && !is.null(condition_var)){

    plot <- ggplot(final_data, aes(x = .data$condition,
                                   y = .data$mean_value)) +
      geom_errorbar(aes(ymax = .data$mean_value + .data$se_value,
                        ymin = .data$mean_value - .data$se_value),
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

    plot <- ggplot(final_data, aes(x = .data$set_size,
                                   y = .data$mean_value)) +
      geom_errorbar(aes(ymax = .data$mean_value + .data$se_value,
                        ymin = .data$mean_value - .data$se_value),
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

    plot <- ggplot(final_data, aes(x = .data$set_size,
                                   y = .data$mean_value,
                                   group = .data$condition)) +
      geom_errorbar(aes(ymax = .data$mean_value + .data$se_value,
                        ymin = .data$mean_value - .data$se_value,
                        colour = .data$condition),
                    width = 0.00,
                    position = pd) +
      geom_point(aes(colour = .data$condition),
                 position = pd,
                 size = 2.5) +
      theme_bw() +
      scale_colour_brewer(palette = palette, name = condition_var) +
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
#' Plot response error of behavioural data relative to target values.
#'
#'Function to plot the response error in behavioural data relative to target
#'values. Requires a data frame that (at least) has target value data and
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
#'@param n_bins An integer controlling the number of cells / bins used in the
#'plot.
#'@param n_col An integer controlling the number of columns in the resulting
#'plot.
#'@param return_data A boolean (TRUE or FALSE) indicating whether the data for
#'the plot should be returned.
#'@param palette A character stating the preferred colour palette to use. To
#'see all available palettes, type ?scale_colour_brewer into the console.
#'@param scale_y_axis A vector of 2 elements stating the minimum and maximum
#'value to use for the y-axis in the plots.
#'
#'@return If \code{return_data} is set to \code{FALSE} (which it is by default),
#' the function returns a ggplot2 object visualising the density distribution
#' of response error averaged across participants (if applicable) per set-size
#' (if applicable) and condition (if applicable).
#'
#' @return If \code{return_data} is set to \code{TRUE}, the function returns a
#' list with two components:
#'
#'  \itemize{
#'  \item \code{plot:} The ggplot2 object.
#'  \item \code{data:} A data frame with the data used to generate the plot.
#'  }
#'
#'
#'@examples
#' plot_error(bays2009_full,
#'           unit = "radians",
#'           set_size_var = "set_size")
#'
#' @importFrom stats sd
#' @importFrom dplyr %>%
#' @importFrom dplyr summarise
#' @importFrom dplyr group_by
#' @importFrom dplyr rename
#' @importFrom graphics hist
#' @importFrom ggplot2 ggplot aes geom_errorbar labs geom_point scale_x_continuous scale_y_continuous
#' theme_bw facet_wrap position_dodge
#' @importFrom rlang .data
#' @export
plot_error <- function(data,
                       unit = "degrees",
                       id_var = "id",
                       response_var = "response",
                       target_var = "target",
                       set_size_var = NULL,
                       condition_var = NULL,
                       n_bins = 18,
                       n_col = 2,
                       return_data = FALSE,
                       palette = "Dark2",
                       scale_y_axis = NULL){

  condition <- NULL
  set_size <- NULL


  # establish the break points of the density plot
  break_points <- round(seq(from = -pi, to = pi, length.out = n_bins), 3)

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
        group_by(.data$id) %>%
        summarise(y = hist(.data$error, breaks = break_points, plot = FALSE)$density,
                  x = hist(.data$error, breaks = break_points, plot = FALSE)$mids) %>%
        group_by(.data$x) %>%
        summarise(mean_error = mean(.data$y),
                  se_error = (sd(.data$y) / sqrt(length(.data$y))))
    } else{
      final_data <- data %>%
        summarise(y = hist(.data$error, breaks = break_points, plot = FALSE)$density,
                  x = hist(.data$error, breaks = break_points, plot = FALSE)$mids) %>%
        group_by(.data$x) %>%
        summarise(mean_error = mean(.data$y),
                  se_error = (sd(.data$y) / sqrt(length(.data$y))))
    }
  }


  # no set size manipulation but there is a condition manipulation
  if(is.null(set_size_var) && !is.null(condition_var)){

    data$condition <- as.factor(data[[condition_var]])

    if(!is.null(id_var)){
      final_data <- data %>%
        group_by(.data$id, .data$condition) %>%
        summarise(y = hist(.data$error, breaks = break_points, plot = FALSE)$density,
                  x = hist(.data$error, breaks = break_points, plot = FALSE)$mids) %>%
        group_by(.data$condition, .data$x) %>%
        summarise(mean_error = mean(.data$y),
                  se_error = (sd(.data$y) / sqrt(length(.data$y))))
    } else{
      final_data <- data %>%
        group_by(.data$condition) %>%
        summarise(y = hist(.data$error, breaks = break_points, plot = FALSE)$density,
                  x = hist(.data$error, breaks = break_points, plot = FALSE)$mids) %>%
        group_by(.data$condition, .data$x) %>%
        summarise(mean_error = mean(.data$y),
                  se_error = (sd(.data$y) / sqrt(length(.data$y))))
    }
  }


  # set size manipulation, but no condition manipulation
  if(!is.null(set_size_var) && is.null(condition_var)){
    data$set_size <- data[[set_size_var]]

    if(!is.null(id_var)){
      final_data <- data %>%
        group_by(.data$id, .data$set_size) %>%
        summarise(y = hist(.data$error, breaks = break_points, plot = FALSE)$density,
                  x = hist(.data$error, breaks = break_points, plot = FALSE)$mids) %>%
        group_by(.data$set_size, .data$x) %>%
        summarise(mean_error = mean(.data$y),
                  se_error = (sd(.data$y) / sqrt(length(.data$y))))
    } else{
      final_data <- data %>%
        group_by(.data$set_size) %>%
        summarise(y = hist(.data$error, breaks = break_points, plot = FALSE)$density,
                  x = hist(.data$error, breaks = break_points, plot = FALSE)$mids) %>%
        group_by(.data$set_size, .data$x) %>%
        summarise(mean_error = mean(.data$y),
                  se_error = (sd(.data$y) / sqrt(length(.data$y))))
    }
  }


  # both set size & condition manipulation
  if(!is.null(set_size_var) && !is.null(condition_var)){
    data$set_size <- data[[set_size_var]]
    data$condition <- as.factor(data[[condition_var]])

    if(!is.null(id_var)){
      final_data <- data %>%
        group_by(.data$id, .data$condition, .data$set_size) %>%
        summarise(y = hist(.data$error, breaks = break_points, plot = FALSE)$density,
                  x = hist(.data$error, breaks = break_points, plot = FALSE)$mids) %>%
        group_by(.data$set_size, .data$condition, .data$x) %>%
        summarise(mean_error = mean(.data$y),
                  se_error = (sd(.data$y) / sqrt(length(.data$y))))
    } else{
      final_data <- data %>%
        group_by(.data$condition, .data$set_size) %>%
        summarise(y = hist(.data$error, breaks = break_points, plot = FALSE)$density,
                  x = hist(.data$error, breaks = break_points, plot = FALSE)$mids) %>%
        group_by(.data$set_size, .data$condition, .data$x) %>%
        summarise(mean_error = mean(.data$y),
                  se_error = (sd(.data$y) / sqrt(length(.data$y))))
    }
  }


  # plot the data ----

  # no set size or condition manipulation
  if(is.null(set_size_var) && is.null(condition_var)){


    if(is.null(scale_y_axis)){
      plot <- ggplot(final_data, aes(x = .data$x,
                                     y = .data$mean_error)) +
        geom_errorbar(aes(ymax = .data$mean_error + .data$se_error,
                          ymin = .data$mean_error - .data$se_error),
                      width = 0.00) +
        geom_point() +
        theme_bw() +
        scale_x_continuous(limits = c(-pi, pi)) +
        scale_y_continuous(limits = c(0,
                                      max(final_data$mean_error) +
                                        max(final_data$se_error))) +
        labs(x = "Error (Radians)",
             y = "Probability Density")
    } else {
      plot <- ggplot(final_data, aes(x = .data$x,
                                     y = .data$mean_error)) +
        geom_errorbar(aes(ymax = .data$mean_error + .data$se_error,
                          ymin = .data$mean_error - .data$se_error),
                      width = 0.00) +
        geom_point() +
        theme_bw() +
        scale_x_continuous(limits = c(-pi, pi)) +
        scale_y_continuous(limits = c(scale_y_axis[1], scale_y_axis[2])) +
        labs(x = "Error (Radians)",
             y = "Probability Density")
    }

    }


  # no set size manipulation but there is a condition manipulation
  if(is.null(set_size_var) && !is.null(condition_var)){

    if(is.null(scale_y_axis)){
      plot <- ggplot(final_data, aes(x = .data$x,
                                     y = .data$mean_error)) +
        geom_errorbar(aes(ymax = .data$mean_error + .data$se_error,
                          ymin = .data$mean_error - .data$se_error),
                      width = 0.00) +
        geom_point() +
        theme_bw() +
        scale_x_continuous(limits = c(-pi, pi)) +
        scale_y_continuous(limits = c(0,
                                      max(final_data$mean_error) +
                                        max(final_data$se_error))) +
        labs(x = "Error (Radians)",
             y = "Probability Density") +
        facet_wrap(vars(.data$condition), ncol = n_col)
    } else {
      plot <- ggplot(final_data, aes(x = .data$x,
                                     y = .data$mean_error)) +
        geom_errorbar(aes(ymax = .data$mean_error + .data$se_error,
                          ymin = .data$mean_error - .data$se_error),
                      width = 0.00) +
        geom_point() +
        theme_bw() +
        scale_x_continuous(limits = c(-pi, pi)) +
        scale_y_continuous(limits = c(scale_y_axis[1], scale_y_axis[2])) +
        labs(x = "Error (Radians)",
             y = "Probability Density") +
        facet_wrap(vars(.data$condition), ncol = n_col)
    }


    # rename the final_data frame
    colnames(final_data)[1] <- condition_var

    }


  # set size manipulation, but no condition manipulation
  if(!is.null(set_size_var) && is.null(condition_var)){

    if(is.null(scale_y_axis)){
      plot <- ggplot(final_data, aes(x = .data$x,
                                     y = .data$mean_error)) +
        geom_errorbar(aes(ymax = .data$mean_error + .data$se_error,
                          ymin = .data$mean_error - .data$se_error),
                      width = 0.00) +
        geom_point() +
        theme_bw() +
        scale_x_continuous(limits = c(-pi, pi)) +
        scale_y_continuous(limits = c(0,
                                      max(final_data$mean_error) +
                                      max(final_data$se_error))) +
        labs(x = "Error (Radians)",
             y = "Probability Density") +
        facet_wrap(vars(.data$set_size), ncol = n_col)
    } else {
      plot <- ggplot(final_data, aes(x = .data$x,
                                     y = .data$mean_error)) +
        geom_errorbar(aes(ymax = .data$mean_error + .data$se_error,
                          ymin = .data$mean_error - .data$se_error),
                      width = 0.00) +
        geom_point() +
        theme_bw() +
        scale_x_continuous(limits = c(-pi, pi)) +
        scale_y_continuous(limits = c(scale_y_axis[1], scale_y_axis[2])) +
        labs(x = "Error (Radians)",
             y = "Probability Density") +
        facet_wrap(vars(.data$set_size), ncol = n_col)
    }



    # rename the final_data frame
    colnames(final_data)[1] <- set_size_var

  }


  # both set size & condition manipulation
  if(!is.null(set_size_var) && !is.null(condition_var)){

    # add position jitter to avoid over-plotting
    pd <- position_dodge(0.1)

    if(is.null(scale_y_axis)){
      plot <- ggplot(final_data, aes(x = .data$x,
                                     y = .data$mean_error,
                                     group = .data$condition)) +
        geom_errorbar(aes(ymax = .data$mean_error + .data$se_error,
                          ymin = .data$mean_error - .data$se_error,
                          colour = .data$condition),
                      width = 0.00,
                      position = pd) +
        geom_point(aes(colour = .data$condition),
                   position = pd) +
        theme_bw() +
        scale_x_continuous(limits = c(-pi, pi)) +
        scale_y_continuous(limits = c(0,
                                      max(final_data$mean_error) +
                                        max(final_data$se_error))) +
        scale_colour_brewer(palette = palette, name = condition_var) +
        labs(x = "Error (Radians)",
             y = "Probability Density") +
        facet_wrap(vars(.data$set_size), ncol = n_col)
    } else {
      plot <- ggplot(final_data, aes(x = .data$x,
                                     y = .data$mean_error,
                                     group = .data$condition)) +
        geom_errorbar(aes(ymax = .data$mean_error + .data$se_error,
                          ymin = .data$mean_error - .data$se_error,
                          colour = .data$condition),
                      width = 0.00,
                      position = pd) +
        geom_point(aes(colour = .data$condition),
                   position = pd) +
        theme_bw() +
        scale_x_continuous(limits = c(-pi, pi)) +
        scale_y_continuous(limits = c(scale_y_axis[1], scale_y_axis[2])) +
        labs(x = "Error (Radians)",
             y = "Probability Density") +
        facet_wrap(vars(.data$set_size), ncol = n_col)
    }


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
# Plot precision of behavioural data
#
#Function to plot the response precision of behavioural data. Requires a data
#frame that (at least) has target value data and participant response data.
#
#@param data A data frame with columns containing: participant identifier
#('id_var'); the participants' response per trial ('response_var'); the
#target value ('target_var'); and, if applicable, the set size of each
#response ('set_size_var'), and the condition of each response
#('condition_var').
#@param unit The unit of measurement in the data frame: "degrees"
#(measurement is in degrees, from 0 to 360); "degrees_180 (measurement is in
#degrees, but limited to 0 to 180); or "radians" (measurement is in radians,
#from pi to 2 * pi, but could also be already in -pi to pi).
#@param id_var The column name coding for participant id. If the data is from
#a single participant (i.e., there is no id column) set to "NULL".
#@param response_var The column name coding for the participants' responses
#@param target_var The column name coding for the target value
#@param set_size_var The column name (if applicable) coding for the set
#size of each response
#@param condition_var The column name (if applicable) coding for the
#condition of each response
#@param return_data A boolean (TRUE or FALSE) indicating whether the data for
#the plot should be returned.
#
#' @importFrom stats sd
#' @importFrom dplyr %>%
#' @importFrom dplyr summarise
#' @importFrom dplyr group_by
#' @importFrom graphics hist
#' @importFrom ggplot2 ggplot aes geom_errorbar labs geom_point theme_bw
#' @importFrom rlang .data
#' @noRd
plot_precision <- function(data,
                           unit = "degrees",
                           id_var = "id",
                           response_var = "response",
                           target_var = "target",
                           set_size_var = NULL,
                           condition_var = NULL,
                           return_data = FALSE,
                           palette = "Dark2"){


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
        group_by(.data$id) %>%
        summarise(precision = get_precision_single(.data$error)[, 1],
                  bias = get_precision_single(.data$error)[, 2]) %>%
        summarise(mean_precision = mean(.data$precision),
                  se_precision = sd(.data$precision) /
                    sqrt(length(.data$precision)),
                  mean_bias = mean(.data$bias),
                  se_bias = sd(.data$bias) / sqrt(length(.data$bias)))
    } else{
      final_data <- data %>%
        summarise(precision = get_precision_single(.data$error)[, 1],
                  bias = get_precision_single(.data$error)[, 2]) %>%
        summarise(mean_precision = mean(.data$precision),
                  se_precision = sd(.data$precision) /
                    sqrt(length(.data$precision)),
                  mean_bias = mean(.data$bias),
                  se_bias = sd(.data$bias) / sqrt(length(.data$bias)))
    }
  }


  # no set size manipulation but there is a condition manipulation
  if(is.null(set_size_var) && !is.null(condition_var)){

    data$condition <- as.factor(data[[condition_var]])

    if(!is.null(id_var)){
      final_data <- data %>%
        group_by(.data$id, .data$condition) %>%
        summarise(precision = get_precision_single(.data$error)[, 1],
                  bias = get_precision_single(.data$error)[, 2]) %>%
        group_by(.data$condition) %>%
        summarise(mean_precision = mean(.data$precision),
                  se_precision = sd(.data$precision) /
                    sqrt(length(.data$precision)),
                  mean_bias = mean(.data$bias),
                  se_bias = sd(.data$bias) / sqrt(length(.data$bias)))
    } else{
      final_data <- data %>%
        group_by(.data$condition) %>%
        summarise(precision = get_precision_single(.data$error)[, 1],
                  bias = get_precision_single(.data$error)[, 2]) %>%
        group_by(.data$condition) %>%
        summarise(mean_precision = mean(.data$precision),
                  se_precision = sd(.data$precision) /
                    sqrt(length(.data$precision)),
                  mean_bias = mean(.data$bias),
                  se_bias = sd(.data$bias) / sqrt(length(.data$bias)))
    }
  }


  # set size manipulation, but no condition manipulation
  if(!is.null(set_size_var) && is.null(condition_var)){

    data$set_size <- data[[set_size_var]]

    if(!is.null(id_var)){
      final_data <- data %>%
        group_by(.data$id, .data$set_size) %>%
        summarise(precision = get_precision_single(.data$error)[, 1],
                  bias = get_precision_single(.data$error)[, 2]) %>%
        group_by(.data$set_size) %>%
        summarise(mean_precision = mean(.data$precision),
                  se_precision = sd(.data$precision) /
                    sqrt(length(.data$precision)),
                  mean_bias = mean(.data$bias),
                  se_bias = sd(.data$bias) / sqrt(length(.data$bias)))
    } else{
      final_data <- data %>%
        group_by(.data$set_size) %>%
        summarise(precision = get_precision_single(.data$error)[, 1],
                  bias = get_precision_single(.data$error)[, 2]) %>%
        group_by(.data$set_size) %>%
        summarise(mean_precision = mean(.data$precision),
                  se_precision = sd(.data$precision) /
                    sqrt(length(.data$precision)),
                  mean_bias = mean(.data$bias),
                  se_bias = sd(.data$bias) / sqrt(length(.data$bias)))
    }
  }


  # both set size & condition manipulation
  if(!is.null(set_size_var) && !is.null(condition_var)){

    data$set_size <- data[[set_size_var]]
    data$condition <- as.factor(data[[condition_var]])

    if(!is.null(id_var)){
      final_data <- data %>%
        group_by(.data$id, .data$condition, .data$set_size) %>%
        summarise(precision = get_precision_single(.data$error)[, 1],
                  bias = get_precision_single(.data$error)[, 2]) %>%
        group_by(.data$set_size, .data$condition) %>%
        summarise(mean_precision = mean(.data$precision),
                  se_precision = sd(.data$precision) /
                    sqrt(length(.data$precision)),
                  mean_bias = mean(.data$bias),
                  se_bias = sd(.data$bias) / sqrt(length(.data$bias)))
    } else{
      final_data <- data %>%
        group_by(.data$condition, .data$set_size) %>%
        summarise(precision = get_precision_single(.data$error)[, 1],
                  bias = get_precision_single(.data$error)[, 2]) %>%
        group_by(.data$set_size, .data$condition) %>%
        summarise(mean_precision = mean(.data$precision),
                  se_precision = sd(.data$precision) /
                    sqrt(length(.data$precision)),
                  mean_bias = mean(.data$bias),
                  se_bias = sd(.data$bias) / sqrt(length(.data$bias)))
    }
  }


  # plot the data----

  # no set size or condition manipulation
  if(is.null(set_size_var) && is.null(condition_var)){
    plot <- "NULL"
  }


  # no set size manipulation but there is a condition manipulation
  if(is.null(set_size_var) && !is.null(condition_var)){

    plot <- ggplot(final_data, aes(x = .data$condition,
                                   y = .data$mean_precision)) +
      geom_errorbar(aes(ymax = .data$mean_precision + .data$se_precision,
                        ymin = .data$mean_precision - .data$se_precision),
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

    plot <- ggplot(final_data, aes(x = .data$set_size,
                                   y = .data$mean_precision)) +
      geom_errorbar(aes(ymax = .data$mean_precision + .data$se_precision,
                        ymin = .data$mean_precision - .data$se_precision),
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

    plot <- ggplot(final_data, aes(x = .data$set_size,
                                   y = .data$mean_precision,
                                   group = .data$condition)) +
      geom_errorbar(aes(ymax = .data$mean_precision + .data$se_precision,
                        ymin = .data$mean_precision - .data$se_precision,
                        colour = .data$condition),
                    width = 0.00,
                    position = pd) +
      geom_point(aes(colour = .data$condition),
                 position = pd,
                 size = 2.5) +
      theme_bw() +
      scale_colour_brewer(palette = palette, name = condition_var) +
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
#'@param model A string indicating the model that was fit to the data. Currently
#' the options are "2_component", "3_component", "slots", and "slots_averaging".
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
#'@param n_bins An integer controlling the number of cells / bins used in the
#'plot of the behavioural data.
#'@param n_col An integer controlling the number of columns in the resulting
#'plot.
#'@param palette A character stating the preferred colour palette to use. To
#'see all available palettes, type ?scale_colour_brewer into the console.
#'
#'@return The function returns a ggplot2 object visualising the mean observed
#' response error density distribution across participants (if applicable)
#' per set-size (if applicable) and condition (if applicable) together with the
#' model predictions superimposed.
#'
#'
#'
#' @importFrom tidyr tibble
#' @importFrom dplyr %>% mutate
#' @importFrom ggplot2 ggplot aes geom_errorbar labs geom_point scale_x_continuous scale_y_continuous
#' theme_bw facet_wrap vars geom_line scale_colour_brewer
#' @importFrom stats dunif
#' @importFrom rlang .data
#' @export
plot_model_fit <- function(participant_data,
                           model_fit,
                           model,
                           unit = "degrees",
                           id_var = "id",
                           response_var = "response",
                           target_var = "target",
                           set_size_var = NULL,
                           condition_var = NULL,
                           n_bins = 18,
                           n_col = 2,
                           palette = "Dark2"){


  # error message if unsupported model called
  if(model != "2_component" &&
     model != "3_component" &&
     model != "slots" &&
     model != "slots_averaging"){
    stop("Unidentified model called. Check the 'model' argument in
         plot_model_fit", call. = FALSE)
  }

  #---- slots model plot
  if(model == "slots"){

    # no set size manipulation
    if(is.null(set_size_var)){
      stop("Slots models require a set size variable", call. = FALSE)
    }

    # get the error data for the participant data
    human_error <- plot_error(participant_data,
                              id_var = id_var,
                              unit = unit,
                              response_var = response_var,
                              target_var = target_var,
                              set_size_var = set_size_var,
                              condition_var = condition_var,
                              n_bins = n_bins,
                              return_data = TRUE,
                              palette = palette)

    human_error <- human_error$data

    #--- no condition manipulation
    if(is.null(condition_var)){

      # get the mean K and kappa parameters from the model fit
      mean_K <- mean(model_fit$K)
      mean_kappa <- mean(model_fit$kappa)

      #--- get the model predictions

      # multiple set-sizes
      if(!is.null(set_size_var)){
        set_sizes <- sort(unique(participant_data[[set_size_var]]))

        for(i in 1:length(set_sizes)){

          level_preds <- tibble(x = seq(-pi, pi, length.out = 1000),
                                y = 0)

          if(mean_K < set_sizes[i]){
            level_preds <- level_preds %>%
              mutate(y = ((mean_K / set_sizes[i]) *
                            vonmisespdf(.data$x, 0, mean_kappa)) +
                       ((1 - (mean_K / set_sizes[i])) *
                          dunif(.data$x, min = -pi, max = pi)))

          } else {
            level_preds <- level_preds %>%
              mutate(y = vonmisespdf(.data$x, 0, mean_kappa))
          }

          level_preds <- level_preds %>%
            mutate(set_size = set_sizes[i])

          if(i == 1){
            model_preds <- level_preds
          } else {
            model_preds <- rbind(model_preds, level_preds)
          }

        }
      }

      #--- plot the human data & model predictions
      plot <- ggplot(human_error, aes(x = .data$x,
                                      y = .data$mean_error)) +
        geom_line(data = model_preds,
                  aes(x = .data$x, y = .data$y),
                  alpha = 0.8,
                  col = "#D95F02",
                  lwd = 1.3) +
        geom_errorbar(aes(ymax = .data$mean_error + .data$se_error,
                          ymin = .data$mean_error - .data$se_error),
                      width = 0.00) +
        geom_point() +
        facet_wrap(vars(.data$set_size), ncol = n_col) +
        theme_bw() +
        scale_colour_brewer(palette = palette) +
        labs(x = "Error (Radians)",
             y = "Probability Density")
    }


    #--- condition manipulation
    if(!is.null(condition_var)){

      human_error$condition <- human_error[[condition_var]]
      model_fit$condition <- model_fit[[condition_var]]


      # get the mean K and kappa parameters from the model fit
      mean_K <- model_fit %>%
        group_by(.data$condition) %>%
        summarise(mean_K = mean(.data$K))

      mean_kappa <- model_fit %>%
        group_by(.data$condition) %>%
        summarise(mean_kappa = mean(.data$kappa))

      # get the model predictions
      conditions <- unique(human_error$condition)
      set_sizes <- sort(unique(participant_data[[set_size_var]]))

      for(i in 1:length(conditions)){
        for(j in 1:length(set_sizes)){

          level_K <- mean_K %>%
            filter(.data$condition == conditions[i]) %>%
            pull()

          level_kappa <- mean_kappa %>%
            filter(.data$condition == conditions[i]) %>%
            pull()

          level_preds <- tibble(x = seq(-pi, pi, length.out = 1000),
                                y = 0)

          if(level_K < set_sizes[j]){
            level_preds <- level_preds %>%
              mutate(y = ((level_K / set_sizes[j]) *
                            vonmisespdf(.data$x, 0, level_kappa)) +
                       ((1 - (level_K / set_sizes[j])) *
                          dunif(.data$x, min = -pi, max = pi)))

          } else {
            level_preds <- level_preds %>%
              mutate(y = vonmisespdf(.data$x, 0, level_kappa))
          }

          level_preds <- level_preds %>%
            mutate(condition = conditions[i],
                   set_size = set_sizes[j])

          if(i == 1 && j == 1){
            model_preds <- level_preds
          } else {
            model_preds <- rbind(model_preds, level_preds)
          }

        }
      }

      #--- plot the human data & model predictions
      human_error$condition <- as.factor(human_error[[condition_var]])
      model_preds$condition <- as.factor(model_preds$condition)

      plot <- ggplot(human_error, aes(x = .data$x,
                                      y = .data$mean_error,
                                      group = .data$condition)) +
        geom_line(data = model_preds,
                  aes(x = .data$x,
                      y = .data$y,
                      colour = .data$condition),
                  alpha = 0.8,
                  lwd = 1) +
        geom_errorbar(aes(ymax = .data$mean_error + .data$se_error,
                          ymin = .data$mean_error - .data$se_error,
                          colour = .data$condition),
                      width = 0.00) +
        geom_point(aes(colour = .data$condition)) +
        facet_wrap(vars(.data$set_size), ncol = n_col) +
        theme_bw() +
        scale_colour_brewer(palette = palette) +
        labs(x = "Error (Radians)",
             y = "Probability Density")

    }

  }

  #---- slots plus averaging model plot
  if(model == "slots_averaging"){


    # get the error data for the participant data
    human_error <- plot_error(participant_data,
                              id_var = id_var,
                              unit = unit,
                              response_var = response_var,
                              target_var = target_var,
                              set_size_var = set_size_var,
                              condition_var = condition_var,
                              n_bins = n_bins,
                              return_data = TRUE,
                              palette = palette)

    human_error <- human_error$data

    #---- no condition manipulation
    if(is.null(condition_var)){

      # get the mean K and kappa parameters from the model fit
      mean_K <- mean(model_fit$K)
      mean_kappa <- mean(model_fit$kappa)

      # get the model predictions
      set_sizes <- sort(unique(participant_data[[set_size_var]]))

      for(i in 1:length(set_sizes)){

        level_preds <- tibble(x = seq(-pi, pi, length.out = 1000),
                              y = 0)

        # work out density contributions
        p_high <- mean_K %% set_sizes[i] / set_sizes[i]
        kappa_high <- mean_kappa * (floor(mean_K / set_sizes[i]) + 1)
        kappa_low <- mean_kappa * floor(mean_K / set_sizes[i])
        p_guess <- 1 - mean_K / set_sizes[i]

        p_error_high <- vonmisespdf(level_preds$x, 0, kappa_high)
        p_error_low <- vonmisespdf(level_preds$x, 0, kappa_low)
        p_error_guess <- (p_guess * 1 / (2 * pi)) +
          ((1 - p_guess) * vonmisespdf(level_preds$x, 0, mean_kappa))

        if(set_sizes[i] <= mean_K){
          level_preds$y <- (p_high * p_error_high) +
            ((1 - p_high) * p_error_low)
        } else {
          level_preds$y <- p_error_guess
        }

        level_preds <- level_preds %>%
          mutate(set_size = set_sizes[i])

        if(i == 1){
          model_preds <- level_preds
        } else {
          model_preds <- rbind(model_preds, level_preds)
        }

      }

      #--- plot the human data & model predictions
      plot <- ggplot(human_error, aes(x = .data$x,
                                      y = .data$mean_error)) +
        geom_line(data = model_preds,
                  aes(x = .data$x, y = .data$y),
                  alpha = 0.8,
                  col = "#D95F02",
                  lwd = 1.3) +
        geom_errorbar(aes(ymax = .data$mean_error + .data$se_error,
                          ymin = .data$mean_error - .data$se_error),
                      width = 0.00) +
        geom_point() +
        facet_wrap(vars(.data$set_size), ncol = n_col) +
        theme_bw() +
        scale_colour_brewer(palette = palette) +
        labs(x = "Error (Radians)",
             y = "Probability Density")

    }


    #--- condition manipulation
    if(!is.null(condition_var)){

      human_error$condition <- human_error[[condition_var]]
      model_fit$condition <- model_fit[[condition_var]]

      # get the mean K and kappa parameters from the model fit
      mean_K <- model_fit %>%
        group_by(.data$condition) %>%
        summarise(mean_K = mean(.data$K))

      mean_kappa <- model_fit %>%
        group_by(.data$condition) %>%
        summarise(mean_kappa = mean(.data$kappa))

      # get the model predictions
      conditions <- unique(human_error$condition)
      set_sizes <- sort(unique(participant_data[[set_size_var]]))

      for(i in 1:length(conditions)){
        for(j in 1:length(set_sizes)){

          level_K <- mean_K %>%
            filter(.data$condition == conditions[i]) %>%
            pull()

          level_kappa <- mean_kappa %>%
            filter(.data$condition == conditions[i]) %>%
            pull()

          level_preds <- tibble(x = seq(-pi, pi, length.out = 1000),
                                y = 0)

          # work out density contributions
          p_high <- level_K %% set_sizes[j] / set_sizes[j]
          kappa_high <- level_kappa * (floor(level_K / set_sizes[j]) + 1)
          kappa_low <- level_kappa * floor(level_K / set_sizes[j])
          p_guess <- 1 - level_K / set_sizes[j]

          p_error_high <- vonmisespdf(level_preds$x, 0, kappa_high)
          p_error_low <- vonmisespdf(level_preds$x, 0, kappa_low)
          p_error_guess <- (p_guess * 1 / (2 * pi)) +
            ((1 - p_guess) * vonmisespdf(level_preds$x, 0, level_kappa))

          if(set_sizes[j] <= level_K){
            level_preds$y <- (p_high * p_error_high) +
              ((1 - p_high) * p_error_low)
          } else {
            level_preds$y <- p_error_guess
          }

          level_preds <- level_preds %>%
            mutate(condition = conditions[i],
                   set_size = set_sizes[j])

          if(i == 1 && j == 1){
            model_preds <- level_preds
          } else {
            model_preds <- rbind(model_preds, level_preds)
          }

        }
      }

      #--- plot the human data & model predictions
      human_error$condition <- as.factor(human_error[[condition_var]])
      model_preds$condition <- as.factor(model_preds$condition)

      plot <- ggplot(human_error, aes(x = .data$x,
                                      y = .data$mean_error,
                                      group = .data$condition)) +
        geom_line(data = model_preds,
                  aes(x = .data$x,
                      y = .data$y,
                      colour = .data$condition),
                  alpha = 0.8,
                  lwd = 1) +
        geom_errorbar(aes(ymax = .data$mean_error + .data$se_error,
                          ymin = .data$mean_error - .data$se_error,
                          colour = .data$condition),
                      width = 0.00) +
        geom_point(aes(colour = .data$condition)) +
        facet_wrap(vars(.data$set_size), ncol = n_col) +
        theme_bw() +
        scale_colour_brewer(palette = palette) +
        labs(x = "Error (Radians)",
             y = "Probability Density")

    }


  }

  #---- components model plot
  if(model == "2_component" || model == "3_component"){

    # check how many components in the model fit object
    if(is.null(model_fit$p_n)){
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
                              n_bins = n_bins,
                              return_data = TRUE,
                              palette = palette)

    human_error <- human_error$data

    # no set size or condition manipulation
    if(is.null(set_size_var) && is.null(condition_var)){

      # get the mean K, p_t, and p_u parameters from the model fit
      # 2-component and 3-component model make same predictions for
      # target error
      mean_k <- mean(model_fit$kappa)
      mean_p_t <- mean(model_fit$p_t)

      if(components == 3){
        mean_p_u <- mean(model_fit$p_n) + mean(model_fit$p_u)
      } else {
        mean_p_u <- mean(model_fit$p_u)
      }


      # get the model predictions
      model_preds <- tibble(x = seq(-pi, pi, length.out = 1000),
                            y = vonmisespdf(.data$x, 0, mean_k) * (mean_p_t) +
                              dunif(.data$x, min = -pi, max = pi) * (mean_p_u))

      # plot the human data & model predictions
      plot <- ggplot(human_error, aes(x = .data$x,
                                      y = .data$mean_error)) +
        geom_line(data = model_preds,
                  aes(x = .data$x, y = .data$y),
                  alpha = 0.8,
                  col = "#D95F02",
                  lwd = 1.3) +
        geom_errorbar(aes(ymax = .data$mean_error + .data$se_error,
                          ymin = .data$mean_error - .data$se_error),
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
        group_by(.data$condition) %>%
        summarise(mean_k = mean(.data$kappa))
      mean_p_t <- model_fit %>%
        group_by(.data$condition) %>%
        summarise(mean_p_t = mean(.data$p_t))

      if(components == 3){
        mean_p_u <- model_fit %>%
          group_by(.data$condition) %>%
          summarise(mean_p_u = mean(.data$p_n) + mean(.data$p_u))
      } else {
        mean_p_u <- model_fit %>%
          group_by(.data$condition) %>%
          summarise(mean_p_u = mean(.data$p_u))
      }


      # get the model predictions
      conditions <- unique(human_error$condition)

      for(i in 1:length(conditions)){

        level_k = mean_k %>%
          filter(.data$condition == conditions[i]) %>%
          pull()

        level_p_t = mean_p_t %>%
          filter(.data$condition == conditions[i]) %>%
          pull()

        level_p_u = mean_p_u %>%
          filter(.data$condition == conditions[i]) %>%
          pull()

        level_preds <- tibble(x = seq(-pi, pi, length.out = 1000),
                              y = vonmisespdf(.data$x, 0, level_k) * (level_p_t) +
                                dunif(.data$x, min = -pi, max = pi) * (level_p_u))

        level_preds <- level_preds %>%
          mutate(condition = conditions[i])

        if(i == 1){
          model_preds <- level_preds
        } else {
          model_preds <- rbind(model_preds, level_preds)
        }
      }

      #---- plot the human data & model predictions
      plot <- ggplot(human_error, aes(x = .data$x,
                                      y = .data$mean_error)) +
        geom_line(data = model_preds,
                  aes(x = .data$x, y = .data$y),
                  alpha = 0.8,
                  col = "#D95F02",
                  lwd = 1.3) +
        geom_errorbar(aes(ymax = .data$mean_error + .data$se_error,
                          ymin = .data$mean_error - .data$se_error),
                      width = 0.00) +
        geom_point() +
        facet_wrap(vars(.data$condition), ncol = n_col) +
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
        group_by(.data$set_size) %>%
        summarise(mean_k = mean(.data$kappa))
      mean_p_t <- model_fit %>%
        group_by(.data$set_size) %>%
        summarise(mean_p_t = mean(.data$p_t))

      if(components == 3){
        mean_p_u <- model_fit %>%
          group_by(.data$set_size) %>%
          summarise(mean_p_u = mean(.data$p_n) + mean(.data$p_u))
      } else {
        mean_p_u <- model_fit %>%
          group_by(.data$set_size) %>%
          summarise(mean_p_u = mean(.data$p_u))
      }


      # get the model predictions
      set_sizes <- unique(model_fit$set_size)

      for(i in 1:length(set_sizes)){

        level_k = mean_k %>%
          filter(.data$set_size == set_sizes[i]) %>%
          pull()

        level_p_t = mean_p_t %>%
          filter(.data$set_size == set_sizes[i]) %>%
          pull()

        level_p_u = mean_p_u %>%
          filter(.data$set_size == set_sizes[i]) %>%
          pull()

        level_preds <- tibble(x = seq(-pi, pi, length.out = 1000),
                              y = vonmisespdf(.data$x, 0, level_k) *
                                (level_p_t) +
                                dunif(.data$x, min = -pi, max = pi) *
                                (level_p_u))

        level_preds <- level_preds %>%
          mutate(set_size = set_sizes[i])

        if(i == 1){
          model_preds <- level_preds
        } else {
          model_preds <- rbind(model_preds, level_preds)
        }

      }


      #---- plot the human data & model predictions
      plot <- ggplot(human_error, aes(x = .data$x,
                                      y = .data$mean_error)) +
        geom_line(data = model_preds,
                  aes(x = .data$x, y = .data$y),
                  alpha = 0.8,
                  col = "#D95F02",
                  lwd = 1.3) +
        geom_errorbar(aes(ymax = .data$mean_error + .data$se_error,
                          ymin = .data$mean_error - .data$se_error),
                      width = 0.00) +
        geom_point() +
        facet_wrap(vars(.data$set_size), ncol = n_col) +
        theme_bw() +
        scale_colour_brewer(palette = palette) +
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
        group_by(.data$set_size, .data$condition) %>%
        summarise(mean_k = mean(.data$kappa))
      mean_p_t <- model_fit %>%
        group_by(.data$set_size, .data$condition) %>%
        summarise(mean_p_t = mean(.data$p_t))

      if(components == 3){
        mean_p_u <- model_fit %>%
          group_by(.data$set_size, .data$condition) %>%
          summarise(mean_p_u = mean(.data$p_n) + mean(.data$p_u))
      } else {
        mean_p_u <- model_fit %>%
          group_by(.data$set_size, .data$condition) %>%
          summarise(mean_p_u = mean(.data$p_u))
      }


      # get the model predictions for each level
      set_sizes <- unique(model_fit$set_size)
      conditions <- unique(model_fit$condition)

      for(i in 1:length(set_sizes)){
        for(j in 1:length(conditions)){

          level_k <- mean_k %>%
            filter(.data$set_size == set_sizes[i]) %>%
            filter(.data$condition == conditions[j]) %>%
            pull()

          level_p_t <-  mean_p_t %>%
            filter(.data$set_size == set_sizes[i]) %>%
            filter(.data$condition == conditions[j]) %>%
            pull()

          level_p_u <-  mean_p_u %>%
            filter(.data$set_size == set_sizes[i]) %>%
            filter(.data$condition == conditions[j]) %>%
            pull()

          level_preds <- tibble(x = seq(-pi, pi, length.out = 1000),
                                y = vonmisespdf(.data$x, 0, level_k) *
                                  (level_p_t) +
                                  dunif(.data$x, min = -pi, max = pi) *
                                  (level_p_u))

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

      plot <- ggplot(human_error, aes(x = .data$x,
                                      y = .data$mean_error,
                                      group = .data$condition)) +
        geom_line(data = model_preds,
                  aes(x = .data$x,
                      y = .data$y,
                      colour = .data$condition),
                  alpha = 0.8,
                  lwd = 1) +
        geom_errorbar(aes(ymax = .data$mean_error + .data$se_error,
                          ymin = .data$mean_error - .data$se_error,
                          colour = .data$condition),
                      width = 0.00) +
        geom_point(aes(colour = .data$condition)) +
        facet_wrap(vars(.data$set_size), ncol = n_col) +
        theme_bw() +
        scale_colour_brewer(palette = palette) +
        labs(x = "Error (Radians)",
             y = "Probability Density")


    }

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
#'@param model A string indicating the model that was fit to the data. Currently
#' the options are "2_component", "3_component", "slots", and "slots_averaging".
#'@param id_var The column name coding for participant id.
#'@param set_size_var The column name (if applicable) coding for the set
#'size of each response.
#'@param condition_var The column name (if applicable) coding for the
#'condition of each response.
#'@param n_col An integer controlling the number of columns in the resulting
#'plot.
#'@param return_data A boolean (TRUE or FALSE) indicating whether the data for
#'the plot should be returned.
#'@param palette A character stating the preferred colour palette to use. To
#'see all available palettes, type?scale_colour_brewer into the console.
#'
#'@return If \code{return_data} is set to \code{FALSE} (which it is by
#' default),the function returns a ggplot2 object visualising the mean model
#' parameters across participants (if applicable) per set-size (if applicable)
#' and condition (if applicable).
#'
#' @return If \code{return_data} is set to \code{TRUE}, the function returns a
#' list with two components:
#'
#'  \itemize{
#'  \item \code{plot:} The ggplot2 object.
#'  \item \code{data:} A data frame with the data used to generate the plot.
#'  }
#' @importFrom stats sd
#' @importFrom dplyr %>%
#' @importFrom dplyr summarise
#' @importFrom dplyr group_by
#' @importFrom dplyr rename
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr all_of
#' @importFrom graphics hist
#' @importFrom ggplot2 position_dodge
#' @importFrom rlang .data
#' @export
plot_model_parameters <- function(model_fit,
                                  model,
                                  id_var = "id",
                                  set_size_var = NULL,
                                  condition_var = NULL,
                                  n_col = 2,
                                  return_data = FALSE,
                                  palette = "Dark2"){

  if(is.null(id_var)){
    stop("id_var required", call. = FALSE)
  }

  #---- slots models
  if(model == "slots" || model == "slots_averaging"){

    # no condition manipulation
    if(is.null(condition_var)){

      # get plot data
      plot_data <- model_fit %>%
        pivot_longer(.data$K:.data$kappa,
                     names_to = "Parameter") %>%
        mutate(Parameter = as.factor(.data$Parameter)) %>%
        group_by(.data$Parameter) %>%
        summarise(mean_value = mean(.data$value),
                  se_value = sd(.data$value) / sqrt(length(.data$value)))

      # do the plot
      plot_data$Parameter <- factor(plot_data$Parameter,
                                    levels = c("K", "kappa"))

      plot <- ggplot(plot_data, aes(x = .data$Parameter,
                                    y = .data$mean_value)) +
        geom_errorbar(aes(ymax = .data$mean_value + .data$se_value,
                          ymin = .data$mean_value - .data$se_value),
                      width = 0.00) +
        geom_point() +
        labs(y = "Mean Parameter Value") +
        facet_wrap(vars(.data$Parameter), ncol = n_col, scales = "free") +
        theme_bw()
    }


      # there is a condition manipulation
      if(!is.null(condition_var)){

        model_fit$condition <- model_fit[[condition_var]]

        # get plot data
        plot_data <- model_fit %>%
          pivot_longer(.data$K:.data$kappa,
                       names_to = "Parameter") %>%
          group_by(.data$condition, .data$Parameter) %>%
          summarise(mean_value = mean(.data$value),
                    se_value = sd(.data$value) / sqrt(length(.data$value)))

        # do the plot
        plot_data$condition <- as.factor(plot_data$condition)
        plot_data$Parameter <- factor(plot_data$Parameter,
                                      levels = c("K", "kappa"))

        plot <- ggplot(plot_data, aes(x = .data$condition,
                                      y = .data$mean_value)) +
          geom_errorbar(aes(ymax = .data$mean_value + .data$se_value,
                            ymin = .data$mean_value - .data$se_value),
                        width = 0.00) +
          geom_point() +
          labs(y = "Mean Parameter Value") +
          facet_wrap(vars(.data$Parameter), ncol = n_col, scales = "free") +
          theme_bw()

        ## rename columns to user-determined names
        participant_condition <- model_fit %>%
          select(all_of(condition_var))
        function_condition <- model_fit %>%
          select(.data$condition)

        if(colnames(participant_condition) != colnames(function_condition)){
          model_fit <- model_fit %>%
            select(-.data$condition)
        }
      }

  }

  #---- components models
  if(model == "2_component" || model == "3_component"){

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
        pivot_longer(.data$kappa:.data$p_u,
                     names_to = "Parameter") %>%
        mutate(Parameter = as.factor(.data$Parameter)) %>%
        group_by(.data$Parameter) %>%
        summarise(mean_value = mean(.data$value),
                  se_value = sd(.data$value) / sqrt(length(.data$value)))


      # do the plot if it's the 2-component model
      if(components == 2){

        plot_data$Parameter <- factor(plot_data$Parameter,
                                      levels = c("kappa", "p_t", "p_u"))

        plot <- ggplot(plot_data, aes(x = .data$Parameter,
                                      y = .data$mean_value)) +
          geom_errorbar(aes(ymax = .data$mean_value + .data$se_value,
                            ymin = .data$mean_value - .data$se_value),
                        width = 0.00) +
          geom_point() +
          labs(y = "Mean Parameter Value") +
          facet_wrap(vars(.data$Parameter), ncol = n_col, scales = "free") +
          theme_bw()
      }


      # do the plot if it's the 3-component model
      if(components == 3){

        plot_data$Parameter <- factor(plot_data$Parameter,
                                      levels = c("kappa", "p_t", "p_n", "p_u"))

        plot <- ggplot(plot_data, aes(x = .data$Parameter,
                                      y = .data$mean_value)) +
          geom_errorbar(aes(ymax = .data$mean_value + .data$se_value,
                            ymin = .data$mean_value - .data$se_value),
                        width = 0.00) +
          geom_point() +
          labs(y = "Mean Parameter Value") +
          facet_wrap(vars(.data$Parameter), ncol = n_col, scales = "free") +
          theme_bw()

      }

    }


    # no set size manipulation but there is a condition manipulation
    if(is.null(set_size_var) && !is.null(condition_var)){

      model_fit$condition <- model_fit[[condition_var]]

      # get the plot data
      if(components == 2){
        plot_data <- model_fit %>%
          select(id_var, .data$kappa, .data$p_t, .data$p_u, .data$condition) %>%
          pivot_longer(.data$kappa:.data$p_u,
                       names_to = "Parameter") %>%
          group_by(.data$condition, .data$Parameter) %>%
          summarise(mean_value = mean(.data$value),
                    se_value = sd(.data$value) / sqrt(length(.data$value)))
      }
      if(components == 3){
        plot_data <- model_fit %>%
          select(id_var, .data$kappa, .data$p_t, .data$p_n, .data$p_u,
                 .data$condition) %>%
          pivot_longer(.data$kappa:.data$p_u,
                       names_to = "Parameter") %>%
          group_by(.data$condition, .data$Parameter) %>%
          summarise(mean_value = mean(.data$value),
                    se_value = sd(.data$value) / sqrt(length(.data$value)))
      }

      ## do the plot if it's the 2-component model

      # ensure condition is factor
      plot_data$condition <- as.factor(plot_data$condition)

      if(components == 2){
        plot_data$Parameter <- factor(plot_data$Parameter,
                                      levels = c("kappa", "p_t", "p_u"))
        plot <- ggplot(plot_data, aes(x = .data$condition,
                                      y = .data$mean_value)) +
          geom_errorbar(aes(ymax = .data$mean_value + .data$se_value,
                            ymin = .data$mean_value - .data$se_value),
                        width = 0.00) +
          geom_point() +
          labs(y = "Mean Parameter Value") +
          labs(x = condition_var) +
          facet_wrap(vars(.data$Parameter), ncol = n_col, scales = "free") +
          theme_bw()
      }


      ## do the plot if it's the 3-component model

      # ensure condition is factor
      plot_data$condition <- as.factor(plot_data$condition)

      if(components == 3){
        plot_data$Parameter <- factor(plot_data$Parameter,
                                      levels = c("kappa", "p_t", "p_n", "p_u"))
        plot <- ggplot(plot_data, aes(x = .data$condition,
                                      y = .data$mean_value)) +
          geom_errorbar(aes(ymax = .data$mean_value + .data$se_value,
                            ymin = .data$mean_value - .data$se_value),
                        width = 0.00) +
          geom_point() +
          labs(y = "Mean Parameter Value") +
          labs(x = condition_var) +
          facet_wrap(vars(.data$Parameter), ncol = n_col, scales = "free") +
          theme_bw()
      }


      ## rename columns to user-determined names
      participant_condition <- model_fit %>%
        select(all_of(condition_var))
      function_condition <- model_fit %>%
        select(.data$condition)

      if(colnames(participant_condition) != colnames(function_condition)){
        model_fit <- model_fit %>%
          select(-.data$condition)
      }

    }

    # set size manipulation, but no condition manipulation
    if(!is.null(set_size_var) && is.null(condition_var)){

      model_fit$set_size <- model_fit[[set_size_var]]

      # get the plot data
      if(components == 2){
        plot_data <- model_fit %>%
          select(id_var, .data$kappa, .data$p_t, .data$p_u, .data$set_size) %>%
          pivot_longer(.data$kappa:.data$p_u,
                       names_to = "Parameter") %>%
          group_by(.data$set_size, .data$Parameter) %>%
          summarise(mean_value = mean(.data$value),
                    se_value = sd(.data$value) / sqrt(length(.data$value)))
      }
      if(components == 3){
        plot_data <- model_fit %>%
          select(id_var, .data$kappa, .data$p_t, .data$p_n, .data$p_u,
                 .data$set_size) %>%
          pivot_longer(.data$kappa:.data$p_u,
                       names_to = "Parameter") %>%
          group_by(.data$set_size, .data$Parameter) %>%
          summarise(mean_value = mean(.data$value),
                    se_value = sd(.data$value) / sqrt(length(.data$value)))
      }

      ## do the plot if it's the 2-component model

      # ensure set size is numeric
      plot_data$set_size <- as.numeric(as.character(plot_data$set_size))

      if(components == 2){
        plot_data$Parameter <- factor(plot_data$Parameter,
                                      levels = c("kappa", "p_t", "p_u"))
        plot <- ggplot(plot_data, aes(x = .data$set_size,
                                      y = .data$mean_value)) ++
          geom_errorbar(aes(ymax = .data$mean_value + .data$se_value,
                            ymin = .data$mean_value - .data$se_value),
                        width = 0.00) +
          geom_point() +
          labs(y = "Mean Parameter Value") +
          labs(x = "Set Size") +
          facet_wrap(vars(.data$Parameter), ncol = n_col, scales = "free") +
          theme_bw()
      }


      ## do the plot if it's the 3-component model

      # ensure set size is numeric
      plot_data$set_size <- as.numeric(as.character(plot_data$set_size))

      if(components == 3){
        plot_data$Parameter <- factor(plot_data$Parameter,
                                      levels = c("kappa", "p_t", "p_n", "p_u"))
        plot <- ggplot(plot_data, aes(x = .data$set_size,
                                      y = .data$mean_value)) +
          geom_errorbar(aes(ymax = .data$mean_value + .data$se_value,
                            ymin = .data$mean_value - .data$se_value),
                        width = 0.00) +
          geom_point() +
          labs(y = "Mean Parameter Value") +
          labs(x = "Set Size") +
          facet_wrap(vars(.data$Parameter), ncol = n_col, scales = "free") +
          theme_bw()
      }

      ## rename columns to user-determined names
      participant_set_size <- model_fit %>%
        select(all_of(set_size_var))
      function_set_size <- model_fit %>%
        select(.data$set_size)

      if(colnames(participant_set_size) != colnames(function_set_size)){
        model_fit <- model_fit %>%
          select(-.data$set_size)
      }

    }


    # both set size & condition manipulation
    if(!is.null(set_size_var) && !is.null(condition_var)){

      model_fit$condition <- model_fit[[condition_var]]
      model_fit$set_size <- model_fit[[set_size_var]]

      # get the plot data
      if(components == 2){
        plot_data <- model_fit %>%
          select(id_var, .data$kappa, .data$p_t, .data$p_u, .data$set_size,
                 .data$condition) %>%
          pivot_longer(.data$kappa:.data$p_u,
                       names_to = "Parameter") %>%
          group_by(.data$condition, .data$set_size, .data$Parameter) %>%
          summarise(mean_value = mean(.data$value),
                    se_value = sd(.data$value) / sqrt(length(.data$value)))
      }
      if(components == 3){
        plot_data <- model_fit %>%
          select(id_var, .data$kappa, .data$p_t, .data$p_n, .data$p_u,
                 .data$set_size, .data$condition) %>%
          pivot_longer(.data$kappa:.data$p_u,
                       names_to = "Parameter") %>%
          group_by(.data$condition, .data$set_size, .data$Parameter) %>%
          summarise(mean_value = mean(.data$value),
                    se_value = sd(.data$value) / sqrt(length(.data$value)))
      }

      ## do the plot if it's the 2-component model

      # ensure set size is numeric & condition is factor
      plot_data$set_size <- as.numeric(as.character(plot_data$set_size))
      plot_data$condition <- as.factor(plot_data$condition)

      if(components == 2){
        plot_data$Parameter <- factor(plot_data$Parameter,
                                      levels = c("kappa", "p_t", "p_u"))
        plot <- ggplot(plot_data, aes(x = .data$set_size,
                                      y = .data$mean_value)) +
          geom_errorbar(aes(ymax = .data$mean_value + .data$se_value,
                            ymin = .data$mean_value - .data$se_value),
                        width = 0.00) +
          geom_point() +
          scale_colour_brewer(palette = palette, name = condition_var) +
          labs(x = "Set Size",
               y = "Mean Parameter Value") +
          facet_wrap(vars(.data$Parameter), ncol = n_col, scales = "free") +
          theme_bw()
      }


      ## do the plot if it's the 3-component model

      # ensure set size is numeric & condition is factor
      plot_data$set_size <- as.numeric(as.character(plot_data$set_size))
      plot_data$condition <- as.factor(plot_data$condition)

      if(components == 3){
        plot_data$Parameter <- factor(plot_data$Parameter,
                                      levels = c("kappa", "p_t", "p_n", "p_u"))
        pd <- position_dodge(0.3)
        plot <- ggplot(plot_data, aes(x = .data$set_size,
                                      y = .data$mean_value,
                                      group = .data$condition)) +
          geom_errorbar(aes(ymax = .data$mean_value + .data$se_value,
                            ymin = .data$mean_value - .data$se_value,
                            colour = .data$condition),
                        width = 0.00,
                        position = pd) +
          geom_point(aes(colour = .data$condition),
                     position = pd) +
          scale_colour_brewer(palette = palette, name = condition_var) +
          labs(x = "Set Size",
               y = "Mean Parameter Value") +
          facet_wrap(vars(.data$Parameter), ncol = n_col, scales = "free") +
          theme_bw()
      }


      ## rename columns to user-determined names

      # set size
      participant_set_size <- model_fit %>%
        select(all_of(set_size_var))
      function_set_size <- model_fit %>%
        select(.data$set_size)

      if(colnames(participant_set_size) != colnames(function_set_size)){
        model_fit <- model_fit %>%
          select(-.data$set_size)
      }

      # condition
      participant_condition <- model_fit %>%
        select(all_of(condition_var))
      function_condition <- model_fit %>%
        select(.data$condition)

      if(colnames(participant_condition) != colnames(function_condition)){
        model_fit <- model_fit %>%
          select(-.data$condition)
      }

    }
  }


  # return the plot & the plot data
  if(return_data == TRUE){
    return(list(plot = plot, data = model_fit))
  } else {
    return(plot)
  }



}
