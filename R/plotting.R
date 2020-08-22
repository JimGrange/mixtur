### plotting functions



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
      geom_point() +
      geom_errorbar(aes(ymax = mean_error + se_error,
                        ymin = mean_error - se_error),
                    width = 0.05) +
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
      geom_point() +
      geom_errorbar(aes(ymax = mean_error + se_error,
                        ymin = mean_error - se_error),
                    width = 0.05) +
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
      geom_point() +
      geom_errorbar(aes(ymax = mean_error + se_error,
                        ymin = mean_error - se_error),
                    width = 0.05) +
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
      geom_point(aes(colour = condition),
                 position = pd) +
      geom_errorbar(aes(ymax = mean_error + se_error,
                        ymin = mean_error - se_error,
                        colour = condition),
                    width = 0.05,
                    position = pd) +
      theme_bw() +
      scale_x_continuous(limits = c(-pi, pi)) +
      scale_y_continuous(limits = c(0,
                                    max(final_data$mean_error) +
                                      max(final_data$se_error))) +
      scale_colour_brewer(palette = "Dark2", name = condition_var) +
      guides(fill=guide_legend(title="New Legend Title")) +
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
      geom_point(size = 2.5) +
      geom_errorbar(aes(ymax = mean_precision + se_precision,
                        ymin = mean_precision - se_precision),
                    width = 0.05) +
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
      geom_point(size = 2.5) +
      geom_errorbar(aes(ymax = mean_precision + se_precision,
                        ymin = mean_precision - se_precision),
                    width = 0.05) +
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
      geom_point(aes(colour = condition),
                 position = pd,
                 size = 2.5) +
      geom_errorbar(aes(ymax = mean_precision + se_precision,
                        ymin = mean_precision - se_precision,
                        colour = condition),
                    width = 0.05,
                    position = pd) +
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
#'@param human_data A data frame of the participant data, with columns
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
plot_model_fit <- function(human_data,
                           model_fit,
                           unit = "degrees",
                           id_var = "id",
                           response_var = "response",
                           target_var = "target",
                           set_size_var = NULL,
                           condition_var = NULL){


  # get the error data for the participant data
  human_error <- plot_error(human_data,
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
    mean_p_u <- mean(model_fit$p_n) + mean(model_fit$p_u)

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
      geom_point() +
      geom_errorbar(aes(ymax = mean_error + se_error,
                        ymin = mean_error - se_error),
                    width = 0.05) +
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
    mean_p_u <- model_fit %>%
      group_by(condition) %>%
      summarise(mean_p_u = mean(p_n) + mean(p_u))

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
      geom_point() +
      geom_errorbar(aes(ymax = mean_error + se_error,
                        ymin = mean_error - se_error),
                    width = 0.05) +
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
    mean_p_u <- model_fit %>%
      group_by(set_size) %>%
      summarise(mean_p_u = mean(p_n) + mean(p_u))

    # get the model predictions
    set_sizes <- unique(data$set_size)

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
      geom_point() +
      geom_errorbar(aes(ymax = mean_error + se_error,
                        ymin = mean_error - se_error),
                    width = 0.05) +
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
    mean_p_u <- model_fit %>%
      group_by(set_size, condition) %>%
      summarise(mean_p_u = mean(p_n) + mean(p_u))

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
      geom_point(aes(colour = condition)) +
      geom_errorbar(aes(ymax = mean_error + se_error,
                        ymin = mean_error - se_error,
                        colour = condition),
                    width = 0.05) +
      facet_wrap(vars(set_size), ncol = 2) +
      theme_bw() +
      scale_colour_brewer(palette = "Dark2") +
      labs(x = "Error (Radians)",
           y = "Probability Density")


  }

  return(plot)
}
