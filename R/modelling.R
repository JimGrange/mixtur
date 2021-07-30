# fit the mixtur model ----------------------------------------------------
#' Fit the mixture model.
#'
#' This is the function called by the user to fit either the two- or three-
#' component mixture model.
#'
#' @source
#' The code for the 3-component model has been adapted from Matlab code
#' written by Paul Bays (https://bayslab.com) published under GNU General
#' Public License.
#' @param data A data frame with columns containing (at the very least)
#' trial-level participant response and target values This data can either be
#' in degrees (1-360 or 1-180) or radians. If the 3-component mixture model is
#' to be fitted to the data, the data frame also needs to contain the values
#' of all non-targets. In addition, the model can be fit to individual
#' individual participants, individual set-sizes, and individual additional
#' conditions; if the user wishes for this, then the data frame should have
#' columns coding for this information.
#'
#' @param model A string indicating the model to be fit to the data. Currently
#' the options are "2_component", "3_component", "slots", and "slots_averaging".
#'
#' @param unit A string indicating the unit of measurement in the data frame:
#' "degrees" (measurement is in degrees, from 1 to 360); "degrees_180
#' (measurement is in degrees, but limited to 1 to 180); or "radians"
#' (measurement is in radians, from pi to 2 * pi, but could also be already in
#' the range -pi to pi).
#'
#' @param id_var The quoted column name coding for participant id. If the data
#' is from a single participant (i.e., there is no id column) set to NULL.
#'
#' @param response_var The quoted column name coding for the participants'
#' responses
#'
#' @param target_var The quoted column name coding for the target value.
#'
#' @param non_target_var The quoted variable name common to all columns (if
#' applicable) storing non-target values. If the user wishes to fit the
#' 3-component mixture model, the user should have one column coding for each
#' non-target's value in the data frame. If there is more than one non-target,
#' each column name should begin with a common term (e.g., the "non_target"
#' term is common to the non-target columns "non_target_1", "non_target_2"
#' etc.), which should then be passed to the function via the
#' \code{non_target_var} variable.
#'
#' @param set_size_var The quoted column name (if applicable) coding for the
#' set size of each response.
#'
#' @param condition_var The quoted column name (if applicable) coding for the
#' condition of each response.
#'
#' @param return_fit If set to TRUE, the function will return the log-likelihood
#' of the model fit, Akiakie's Information Criterion (AIC), Bayesian Information
#' Criterion (BIC), as well as the number of trials used in the fit.
#'
#' @return Returns a data frame with best-fitting parameters per participant (if
#' applicable), set-size (if applicable), and condition (if applicable). If
#' \code{return_fit} was set to \code{TRUE}, the data frame will also include
#' the log-likelihood value and information criteria of the model fit.
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr select filter
#' @importFrom rlang :=
#'
#' @examples
#'
#' # load the example data
#' data <- bays2009_full
#'
#' # fit the 3-component mixture model ignoring condition
#' \donttest{
#' fit <- fit_mixtur(data = data,
#'                   model = "3_component",
#'                   unit = "radians",
#'                   id_var = "id",
#'                   response_var = "response",
#'                   target_var = "target",
#'                   non_target_var = "non_target",
#'                   set_size_var = "set_size",
#'                   condition_var = NULL)
#'}
#'
#' @export
fit_mixtur <- function(data,
                       model = "3_component",
                       unit = "degrees",
                       id_var = "id",
                       response_var = "response",
                       target_var = "target",
                       non_target_var = NULL,
                       set_size_var = NULL,
                       condition_var = NULL,
                       return_fit = FALSE){

  # set the fit method (removed as an option to the user)
  fit_method <- "GD"

  # error message if unsupported model called
  if(model != "2_component" &&
     model != "3_component" &&
     model != "slots" &&
     model != "slots_averaging"){
    stop("Unidentified model called. Check the 'model' argument in
         fit_mixtur", call. = FALSE)
  }

  # get the non-target column names, if applicable
  if(!is.null(non_target_var)){
    non_target_cols <- data %>%
      select(contains(non_target_var)) %>%
      colnames()
  }

  # if 3-component model is called but non-target columns not provided,
  # tell the user to provide them
  if(model == "3_component" && is.null(non_target_var)){
    stop("The 3-component model requires non-target values", call. = FALSE)
  }


  # change degrees to radians
  if(unit == "degrees"){
    data[[response_var]] <- data[[response_var]] / 180 * pi
    data[[target_var]] <- data[[target_var]] / 180 * pi

     if(!is.null(non_target_var)){
      data[, non_target_cols] <- data[, non_target_cols] / 180 * pi
    }
  }

  # change degrees_180 to radians
  if(unit == "degrees_180"){
    data[[response_var]] <- data[[response_var]] / 90 * pi
    data[[target_var]] <- data[[target_var]] / 90 * pi

    if(!is.null(non_target_var)){
      data[, non_target_cols] <- data[, non_target_cols] / 90 * pi
    }
  }

  if(unit == "radians"){
    data <- data
  }

  # print message to user
  message("Model fit running. Please wait...")


  #---- fitting the slots models
  if(model == "slots" || model == "slots_averaging"){

    # return an error message if there is not a set size column
    if(is.null(set_size_var)){
      stop("Slots model requires a set size variable.", call. = FALSE)
    }

    # no condition manipulation
    if(is.null(condition_var)){

      # perform the model fit
      fit <- fit_level_slots(data,
                             model = model,
                             id_var = id_var,
                             response_var = response_var,
                             target_var = target_var,
                             set_size_var = set_size_var,
                             return_fit = return_fit,
                             fit_method = fit_method)

    }


    # there is a condition manipulation
    if(!is.null(condition_var)){

      # get the list of conditions
      data$condition <- data[[condition_var]]
      conditions <- unique(data[, "condition"])

      # loop over each condition
      for(i in 1:length(conditions)){


        # get the current level's data
        level_data <- data %>%
          filter(.data$condition == conditions[i])

        # fit the model to this condition
        level_fit <- fit_level_slots(level_data,
                                     model = model,
                                     id_var = id_var,
                                     response_var = response_var,
                                     target_var = target_var,
                                     set_size_var = set_size_var,
                                     return_fit = return_fit,
                                     fit_method = fit_method)

        # add the condition level to the parameter data frame
        level_fit <- level_fit %>%
          mutate(condition = conditions[i])

        # stitch data together
        if(i == 1){
          fit <- level_fit
        } else {
          fit <- rbind(fit, level_fit)
        }

      }

      # rename columns
      fit <- fit %>%
        rename(!!condition_var:=.data$condition)

    }
  }

  #---- fitting the components models
  if(model == "2_component" || model == "3_component"){

    # no set size or condition manipulation
    if(is.null(set_size_var) && is.null(condition_var)){

      # get the set size of the level
      if(!is.null(non_target_var)){
        level_set_size <- length(non_target_cols) + 1
      } else {
        level_set_size <- 1
      }

      # perform the model fit
      fit <- fit_level_components(data,
                                  model = model,
                                  id_var = id_var,
                                  response_var = response_var,
                                  target_var = target_var,
                                  non_target_var = non_target_var,
                                  set_size = level_set_size,
                                  return_fit = return_fit,
                                  fit_method = fit_method)
    }



    # no set size manipulation but there is a condition manipulation
    if(is.null(set_size_var) && !is.null(condition_var)){

      # get the list of conditions
      data$condition <- data[[condition_var]]
      conditions <- unique(data[, "condition"])

      # loop over each condition
      for(i in 1:length(conditions)){

        # get the current level's data
        level_data <- data %>%
          filter(.data$condition == conditions[i])

        # get the set size of the level
        if(!is.null(non_target_var)){
          level_set_size <- length(non_target_cols) + 1
        } else {
          level_set_size <- 1
        }

        # fit the model to this condition
        level_fit <- fit_level_components(level_data,
                                          model = model,
                                          id_var = id_var,
                                          response_var = response_var,
                                          target_var = target_var,
                                          non_target_var = non_target_var,
                                          set_size = level_set_size,
                                          return_fit = return_fit,
                                          fit_method = fit_method)

        level_fit <- level_fit %>%
          mutate(condition = conditions[i])

        # stitch data together
        if(i == 1){
          fit <- level_fit
        } else {
          fit <- rbind(fit, level_fit)
        }
      }

      # rename columns
      fit <- fit %>%
        rename(!!condition_var:=.data$condition)
    }


    # set size manipulation, but no condition manipulation
    if(!is.null(set_size_var) && is.null(condition_var)){

      # get the list of set_sizes
      data$set_size <- data[[set_size_var]]
      set_sizes <- unique(data[[set_size_var]])

      # loop over each set size
      for(i in 1:length(set_sizes)){

        # get the current set size's data
        level_data <- data %>%
          filter(.data$set_size == set_sizes[i])

        # fit the model to this set size
        if(set_sizes[i] == 1){
          level_fit <- fit_level_components(level_data,
                                            model = model,
                                            id_var = "id",
                                            response_var = "response",
                                            target_var = "target",
                                            non_target_var = NULL,
                                            set_size = 1,
                                            return_fit = return_fit,
                                            fit_method = fit_method)

          level_fit <- level_fit %>%
            mutate(set_size = set_sizes[i])

        } else{
          level_fit <- fit_level_components(level_data,
                                            model = model,
                                            id_var = "id",
                                            response_var = "response",
                                            target_var = "target",
                                            non_target_var = non_target_var,
                                            set_size = set_sizes[i],
                                            return_fit = return_fit,
                                            fit_method = fit_method)

          level_fit <- level_fit %>%
            mutate(set_size = set_sizes[i])
        }

        # stitch data together
        if(i == 1){
          fit <- level_fit
        } else {
          fit <- rbind(fit, level_fit)
        }
      }

      # rename columns
      fit <- fit %>%
        rename(!!set_size_var:=.data$set_size)
    }


    # both set size & condition manipulation
    if(!is.null(set_size_var) && !is.null(condition_var)){

      # get the list of set sizes
      data$set_size <- data[[set_size_var]]
      set_sizes <- unique(data[[set_size_var]])

      # get the list of conditions
      data$condition <- data[[condition_var]]
      conditions <- unique(data[, "condition"])

      # loop over each set size & condition
      for(i in 1:length(set_sizes)){
        for(j in 1:length(conditions)){

          # get the current level's data
          level_data <- data %>%
            filter(.data$set_size == set_sizes[i]) %>%
            filter(.data$condition == conditions[j])

          # fit the model to this set size & condition
          if(set_sizes[i] == 1){
            level_fit <- fit_level_components(level_data,
                                              model = model,
                                              id_var = id_var,
                                              response_var = response_var,
                                              target_var = target_var,
                                              non_target_var = NULL,
                                              set_size = 1,
                                              return_fit = return_fit,
                                              fit_method = fit_method)

            level_fit <- level_fit %>%
              mutate(set_size = set_sizes[i],
                     condition = conditions[j])
          } else{
            level_fit <- fit_level_components(level_data,
                                              model = model,
                                              id_var = id_var,
                                              response_var = response_var,
                                              target_var = target_var,
                                              non_target_var = non_target_var,
                                              set_size = set_sizes[i],
                                              return_fit = return_fit,
                                              fit_method = fit_method)

            level_fit <- level_fit %>%
              mutate(set_size = set_sizes[i],
                     condition = conditions[j])
          }

          # stitch data together
          if(i == 1 && j == 1){
            fit <- level_fit
          } else {
            fit <- rbind(fit, level_fit)
          }
        }
      }

      # rename columns
      fit <- fit %>%
        rename(!!condition_var:=.data$condition) %>%
        rename(!!set_size_var:=.data$set_size)
    }

  }



  # print message to user
  message("Model fit finished.")


  # add AIC, AIC_C, and BIC if requested
  if(return_fit == TRUE){

    if(model == "2_component" || model == "slots" || model == "slots_averaging"){
      n_parms <- 2
    } else {
      n_parms <- 3
    }

    fit <- fit %>%
      mutate(aic = aic(.data$LL, n_parms),
             aic_c = aic_c(.data$LL, n_parms, .data$n),
             bic = bic(.data$LL, n_parms, .data$n))

  }


  return(fit)

}


# fit slots model to a single level ---------------------------------------
# Fit slots model to a single level. This fits both the slots model and the
# slots plus averaging model.
#
# This wrapper function is called by the \code{fit_mixtur} function to fit the
# models to a single level from the data frame. It is not expected that this
# function be called by the user.
#
#' @importFrom dplyr %>%
#' @importFrom dplyr pull
#' @importFrom dplyr rename
#' @importFrom rlang .data
fit_level_slots <- function(data,
                            model,
                            id_var,
                            response_var,
                            target_var,
                            set_size_var,
                            return_fit,
                            fit_method){

  # get the participant ids
  id <- data %>%
    pull(id_var)

  # move each participant data to separate items in list
  l <- split(data, id)

  # initiate data frame to store parameters
  parms <- data.frame(id = FALSE, K = FALSE, kappa = FALSE,
                      LL = FALSE, n = FALSE)

  # loop over every participant
  for(i in seq_along(l)){

    # get the current participant's data
    df <- as.data.frame.list(l[i], col.names = colnames(l[i]))

    # get the relevant values
    slot_data <- df %>%
      select(response_var, target_var, set_size_var) %>%
      rename(response = response_var,
             target = target_var,
             set_size = set_size_var)


    #pass the data to the fit function
    fit <- fit_slots_gd(model = model,
                        slot_data,
                        return.ll = return_fit)

    # store the parameters
    id <- as.character(df[1, id_var])
    parms[i, 1] <- id

    if(return_fit == TRUE){
      parms[i, 2:3] <- round(fit$parameters, 3)
      parms[i, 4] <- round(fit$LL, 3)
      parms[i, 5] <- nrow(df)
    } else{
      parms[i, 2:3] <- round(fit, 3)
    }

  }

  if(return_fit == TRUE){
    return(parms)
  } else {
    parms <- parms %>% select(-.data$LL, -.data$n)
    return(parms)
  }

}


# fit slots model ---------------------------------------------------------
# fit the slots model via gradient descent.
#
# This is the function that is called by the wrapper function
# \code{fit_level_slots}. It is not expected that this function be called by
# the user.
#' @importFrom stats optim
fit_slots_gd <- function(model,
                         slot_data,
                         return.ll = TRUE){



  # number of trials
  n <- NROW(slot_data)

  # set starting parameters
  K <- c(1, 2, 3, 4)
  kappa <- c(1, 10, 100)

  # set minimum & maximum parameter values
  min_parms <- c(0, 0)
  max_parms <- c(Inf, Inf)


  # initialise log likelihood
  log_lik = Inf

  # iterate over all starting parameters and conduct model fit
  for(i in seq_along(K)){
    for(j in seq_along(kappa)){

      start_parms <- c(K[i],
                       kappa[j])

      if(model == "slots"){
        est_list <- optim(par = start_parms,
                          fn = slots_model_pdf_gd,
                          data = slot_data,
                          min_parms = min_parms,
                          max_parms = max_parms,
                          method = "Nelder-Mead",
                          control = list(parscale = c(1, 5)))
      }

      if(model == "slots_averaging"){
        est_list <- optim(par = start_parms,
                          fn = slots_model_averaging_pdf_gd,
                          data = slot_data,
                          min_parms = min_parms,
                          max_parms = max_parms,
                          method = "Nelder-Mead",
                          control = list(parscale = c(1, 5)))
      }



      if(est_list$value < log_lik & !is.nan(est_list$value)) {

        log_lik <- est_list$value
        parameters <- c(est_list$par[1],
                        est_list$par[2])
        parameters <- round(parameters, 3)

      }
    }
  }

  if(return.ll == TRUE) {
    return(list(parameters = parameters, LL = -log_lik))
  } else {
    return(parameters)
  }

}


# slots plus averaging likelihood function --------------------------------
# Calculate the likelihood function of the slots plus averaging model fitting
# via gradient descent (nelder-mead routine via optim function).
#
# It is not expected that this function be called by the user.
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom rlang .data
slots_model_averaging_pdf_gd <- function(data,
                                         parms,
                                         min_parms,
                                         max_parms){

  # check bounds on parameter values
  if ((min(parms - min_parms) < 0) | (min(max_parms - parms) < 0)){
    return(.Machine$double.xmax)
  }

  # extract parameters
  K <- parms[1]
  kappa <- parms[2]

  # calculate response error
  data <- data %>%
    mutate(error = wrap(.data$target - .data$response))

  # calculate negative log-likelihood
  data <- data %>%
    mutate(p_high = K %% .data$set_size / .data$set_size,
           kappa_high = kappa * (floor(K / .data$set_size) + 1),
           kappa_low = kappa * floor(K / .data$set_size),
           p_error_high = vonmisespdf(.data$error, 0, .data$kappa_high),
           p_error_low = vonmisespdf(.data$error, 0, .data$kappa_low),
           p_guess = 1 - K / .data$set_size,
           p_error_guess = (.data$p_guess * 1 / (2 * pi)) +
             ((1 - .data$p_guess) * vonmisespdf(.data$error, 0, kappa))) %>%
    mutate(p_error = case_when(set_size <= K ~ (p_high * p_error_high) +
                                 ((1 - p_high) * p_error_low),
                               TRUE ~ p_error_guess))


  ll <- -sum(log(data$p_error))

  if(ll == Inf || ll == -Inf ||  is.na(ll)){
    return(.Machine$double.xmax)
  } else{
    return(ll)
  }

}


# slots model likelihood function -----------------------------------------
# Calculate the likelihood function of the slots model fitting via
# gradient descent (nelder-mead routine via optim function).
#
# It is not expected that this function be called by the user.
#
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom rlang .data
slots_model_pdf_gd <- function(data,
                               parms,
                               min_parms,
                               max_parms){

  # check bounds on parameter values
  if ((min(parms - min_parms) < 0) | (min(max_parms - parms) < 0)){
    return(.Machine$double.xmax)
  }

  # extract parameters
  K <- parms[1]
  kappa <- parms[2]

  # calculate response error
  data <- data %>%
    mutate(error = wrap(.data$target - .data$response))

  # calculate negative log likelihood
  d <- data %>%
    mutate(p_memory = K / .data$set_size,
           p_error_memory = vonmisespdf(.data$error, 0, kappa),
           p_guess = 1 - K / .data$set_size,
           p_error_guess =  1 / (2 * pi)) %>%
    mutate(p_error = case_when(K < .data$set_size ~ (p_memory * p_error_memory) +
                                 ((1 - p_memory) * p_error_guess),
                               TRUE ~ p_error_memory))

  ll <- -sum(log(d$p_error))

  if(ll == Inf || ll == -Inf || is.na(ll)){
    return(.Machine$double.xmax)
  } else{
    return(ll)
  }

}




# fit components model to a single level ----------------------------------
# Fit components model to a single level.
#
# This wrapper function is called by the \code{fit_mixtur} function to fit the
# models to a single level from the data frame. It is not expected that this
# function be called by the user.
#
#' @importFrom dplyr %>%
#' @importFrom dplyr pull
#' @importFrom dplyr starts_with
#' @importFrom rlang .data
fit_level_components <- function(data,
                                 model,
                                 id_var = "id",
                                 response_var = "response",
                                 target_var = "target",
                                 non_target_var,
                                 set_size = 1,
                                 return_fit = FALSE,
                                 fit_method){

  if(set_size == 1){
    non_target_var <- NULL
  }

  # get the participant ids
  id <- data %>%
    pull(id_var)

  # move each participant data to separate items in list
  l <- split(data, id)

  # initiate data frame to store parameters
  if(model == "2_component" || model == "3_component"){
    parms <- data.frame(id = FALSE, kappa = FALSE, p_t = FALSE, p_n = FALSE,
                        p_u = FALSE, LL = FALSE, n = FALSE)
  }


  # loop over every participant
  for(i in seq_along(l)) {

    # get the current participant's data
    df <- as.data.frame.list(l[i], col.names = colnames(l[i]))

    # get the response and the target values
    response <- as.matrix(df[, response_var])
    target <- as.matrix(df[, target_var])


    #--- pass the data to the fit function

    #-- fit the 2-component model
    if(model == "2_component"){

      if(fit_method == "EM"){
        fit <- fit_components_em(response,
                                 target,
                                 return.ll = return_fit)
      }
      if(fit_method == "GD"){
        fit <- fit_components_gd(response,
                                 target,
                                 return.ll = return_fit)
      }

    }

    #-- fit the 3-component model
    # if the 3-component model is called, pass non-target info to fit
    # only if set size is greater than one (i.e., there is non-target info)
    if(model == "3_component"){

      # get the non-target values for this set size if set size is above one
      if(set_size > 1){
        non_target_cols <- df %>%
          select(starts_with(non_target_var)) %>%
          colnames()

        non_targets <- as.matrix(df[, non_target_cols])
        colnames(non_targets) <- NULL
        non_targets <- as.matrix(non_targets[, 1:(set_size - 1)])
      } else {
        non_targets <- NULL
      }

      if(fit_method == "EM"){
        if(is.null(non_target_var)) {
          fit <- fit_components_em(response,
                                   target,
                                   return.ll = return_fit)
        } else {
          fit <- fit_components_em(response,
                                   target,
                                   non_targets,
                                   return.ll = return_fit)
        }
      }

      if(fit_method == "GD"){
        if(is.null(non_target_var)) {
          fit <- fit_components_gd(response,
                                   target,
                                   return.ll = return_fit)
        } else {
          fit <- fit_components_gd(response,
                                   target,
                                   non_targets,
                                   return.ll = return_fit)
        }
      }
    }

    # store the parameters
    if(model == "2_component"){

      id <- as.character(df[1, id_var])
      parms[i, 1] <- id

      if(return_fit == TRUE){
        parms[i, 2:5] <- round(fit$parameters, 3)
        parms[i, 6] <- round(fit$LL, 3)
        parms[i, 7] <- nrow(df)
      } else{
        parms[i, 2:5] <- round(fit, 3)
      }
    }


    if(model == "3_component"){

      id <- as.character(df[1, id_var])
      parms[i, 1] <- id

      if(return_fit == TRUE){

        parms[i, 2:5] <- round(fit$parameters, 3)
        parms[i, 6] <- round(fit$LL, 3)
        parms[i, 7] <- nrow(df)
      } else{
        parms[i, 2:5] <- round(fit, 3)
      }
    }

  }

  # remove p_n column from 2-component fits
  if(model == "2_component"){
    parms <- parms %>% select(-.data$p_n)
  }


  if(return_fit == TRUE){
    return(parms)
  } else {
    parms <- parms %>% select(-.data$LL, -.data$n)
    return(parms)
  }

}



# fit components model gd -----------------------------------------------
# Fit the components model using gradient descent (nelder-mead routine via
# optim function).

# This is the function that is called by the wrapper function
# \code{fit_level}. It is not expected that this function be called by the
# user.
#' @importFrom stats optim
fit_components_gd <- function(response,
                              target,
                              non_targets = replicate(NROW(response), 0),
                              return.ll = TRUE) {

  # check the data is in correct shape
  if(NCOL(response) > 2 | NCOL(target) > 1 | NROW(response) != NROW(target) |
     (any(non_targets != 0) & NROW(non_targets) != NROW(response) |
      NROW(non_targets) != NROW(target))) {
    stop("fit_model error: Input not correctly dimensioned", call. = FALSE)
  }

  # number of trials
  n <- NROW(response)

  # number of non-targets
  nn <- ifelse(any(non_targets != 0), NCOL(non_targets), 0)

  # set starting parameters
  kappa <- c(1, 10, 100)
  N <- c(0.01, 0.1, 0.4)
  U <- c(0.01, 0.1, 0.4)

  if(nn == 0){
    N <- 0
  }

  # initialise log likelihood
  log_lik = Inf

  # iterate over all starting parameters and conduct model fit
  for(i in seq_along(kappa)) {
    for(j in seq_along(N)) {
      for(k in seq_along(U)) {

        start_parms <- c(kappa[i],
                        N[j],
                        U[k])

        est_list <- optim(par = start_parms,
                          fn = components_model_pdf_gd,
                          response = response,
                          target = target,
                          non_targets = non_targets,
                          method = "Nelder-Mead",
                          control = list(parscale = c(1, 0.1, 0.1)))


        if (est_list$value < log_lik & !is.nan(est_list$value) ) {
          log_lik <- est_list$value
          parameters <- c(est_list$par[1],
                          1 - est_list$par[2] - est_list$par[3],
                          est_list$par[2],
                          est_list$par[3])
          parameters <- round(parameters, 3)
        }
      }
    }
  }


  if(return.ll == TRUE) {
    return(list(parameters = parameters, LL = -log_lik))
  } else {
    return(parameters)
  }
}



# components model likelihood function gd  ---------------------------------
# Calculate the likelihood function of the mixture model.
#
# It is not expected that this function be called by the user.
components_model_pdf_gd <- function(response,
                                    target,
                                    non_targets,
                                    start_parms = NULL,
                                    min_parms,
                                    max_parms) {

  # extract the parameters
  parms <- c(start_parms[1],
             1 - start_parms[2] - start_parms[3],
             start_parms[2],
             start_parms[3])

  if(is.null(non_targets)){
    non_targets <- replicate(NROW(response), 0)
  }

  # check the data is in correct shape
  if(NCOL(response) > 2 | NCOL(target) > 1 | NROW(response) != NROW(target) |
     (any(non_targets != 0) & NROW(non_targets) != NROW(response) |
      NROW(non_targets) != NROW(target))) {
    stop("likelihood error: Input not correctly dimensioned", call. = FALSE)
  }

  # check parameters are valid in terms of min and max values
  if((!(is.null(parms))) &
     (any(parms[1] < 0, parms[2:4] < 0,
          parms[2:4] > 1, abs(sum(parms[2:4]) - 1) > 10 ^ - 6))) {
    return(.Machine$double.xmax)
  }


  # set maximum iterations & LL acceptable
  max_iter <- 10^4
  max_dLL <- 10^-4

  # get the number of trials
  n <- NROW(response)

  # get the number of non-targets present
  nn <- ifelse(any(non_targets != 0), NCOL(non_targets), 0)

  # set default starting parameter if not provided, else assign starting
  # parameters to parameter variables
  if(is.null(parms)) {
    kappa <- 5
    p_t <- 0.5
    p_n <- ifelse(nn > 0, 0.3, 0)
    p_u <- 1 - p_t - p_n
  } else {
    kappa <- parms[1];
    p_t <- parms[2]
    p_n <- parms[3];
    p_u <- parms[4]
  }

  # calculate response error from target value
  error <- wrap(response - target)

  # if present, calculate response error from non-targets
  if(nn > 0){
    non_target_error <- wrap(repmat(response, nn) - non_targets)
  } else {
    non_target_error <- repmat(response, nn)
  }

  # initialise likelihood and fit routine values
  LL <- 0
  dLL <- 1

  # get the weight contributions of target and guess responses to performance
  w_t <- p_t * vonmisespdf(error, 0, kappa)
  w_g <- p_u * replicate(n, 1) / (2 * pi)

  # if present, get the weight contribution of non-target responses
  # to performance
  if(nn == 0){
    w_n <- matrix(nrow = NROW(non_target_error),
                  ncol = NCOL(non_target_error))
  } else {
    w_n <- p_n/nn * vonmisespdf(non_target_error, 0, kappa)
  }

  # calculate log likelihood of model
  weights <- rowSums(cbind(w_t, w_g, w_n))
  ll <- -sum(log(weights))


  if(ll == Inf || ll == -Inf || is.na(ll)){
    return(.Machine$double.xmax)
  } else {
    return(ll)
  }
}


# fit components model via EM ---------------------------------------------
# Fit the components model via expectation maximisation.
#
# This is the function that is called by the wrapper function
# \code{fit_level}. It is not expected that this function be called by the
# user.
fit_components_em <- function(response,
                              target,
                              non_targets = replicate(NROW(response), 0),
                              return.ll = TRUE) {

  # check the data is in correct shape
  if(NCOL(response) > 2 | NCOL(target) > 1 | NROW(response) != NROW(target) |
     (any(non_targets != 0) & NROW(non_targets) != NROW(response) |
      NROW(non_targets) != NROW(target))) {
    stop("fit_model error: Input not correctly dimensioned", call. = FALSE)
  }

  # number of trials
  n <- NROW(response)

  # number of non-targets
  nn <- ifelse(any(non_targets != 0), NCOL(non_targets), 0)

  # set starting parameters
  kappa <- c(1, 10, 100)
  N <- c(0.01, 0.1, 0.4)
  U <- c(0.01, 0.1, 0.4)

  if(nn == 0){
    N <- 0
  }

  # initialise log likelihood
  log_lik = -Inf

  # iterate over all starting parameters and conduct model fit
  for(i in seq_along(kappa)) {
    for(j in seq_along(N)) {
      for(k in seq_along(U)) {
        est_list <- components_model_pdf_em(response = response,
                                            target = target,
                                            non_targets = non_targets,
                                            start_parms = c(kappa[i],
                                                            1 - N[j] - U[k],
                                                            N[j], U[k]))

        if (est_list$ll > log_lik & !is.nan(est_list$ll) ) {
          log_lik <- est_list$ll
          parameters <- round(est_list$parameters, 3)
        }
      }
    }
  }

  if(return.ll == TRUE) {
    return(list(parameters = parameters, LL = log_lik))
  } else {
    return(parameters)
  }
}



# components model likelihood function em  --------------------------------
# Calculate the likelihood function of the components model fitting via
# expectation maximisation.
#
# It is not expected that this function be called by the user.
components_model_pdf_em <- function(response,
                                    target,
                                    non_targets,
                                    start_parms = NULL) {

  if(is.null(non_targets)){
    non_targets <- replicate(NROW(response), 0)
  }

  # check the data is in correct shape
  if(NCOL(response) > 2 | NCOL(target) > 1 | NROW(response) != NROW(target) |
     (any(non_targets != 0) & NROW(non_targets) != NROW(response) |
      NROW(non_targets) != NROW(target))) {
    stop("likelihood error: Input not correctly dimensioned", call. = FALSE)
  }

  # check parameters are valid
  if((!(is.null(start_parms))) &
     (any(start_parms[1] < 0, start_parms[2:4] < 0,
          start_parms[2:4] > 1, abs(sum(start_parms[2:4]) - 1) > 10 ^ - 6))) {
    stop("likelihood error: Invalid model parameters", call. = FALSE)
  }

  # set maximum iterations & LL acceptable
  max_iter <- 10^4
  max_dLL <- 10^-4

  # get the number of trials
  n <- NROW(response)

  # get the number of non-targets present
  nn <- ifelse(any(non_targets != 0), NCOL(non_targets), 0)

  # set default starting parameter if not provided, else assign starting
  # parameters to parameter variables
  if(is.null(start_parms)) {
    kappa <- 5
    p_t <- 0.5
    p_n <- ifelse(nn > 0, 0.3, 0)
    p_u <- 1 - p_t - p_n
  } else {
    kappa <- start_parms[1];
    p_t <- start_parms[2]
    p_n <- start_parms[3];
    p_u <- start_parms[4]
  }

  # calculate response error from target value
  error <- wrap(response - target)

  # if present, calculate response error from non-targets
  if(nn > 0){
    non_target_error <- wrap(repmat(response, nn) - non_targets)
  } else {
    non_target_error <- repmat(response, nn)
  }

  # initialise likelihood and fit routine values
  LL <- 0
  dLL <- 1
  iter <- 0

  # iterate to minimise log likelihood
  while(TRUE) {
    iter <- iter + 1

    # get the weight contributions of target and guess responses to performance
    w_t <- p_t * vonmisespdf(error, 0, kappa)
    w_g <- p_u * replicate(n, 1) / (2 * pi)

    # if present, get the weight contribution of non-target responses
    # to performance
    if(nn == 0){
      w_n <- matrix(nrow = NROW(non_target_error),
                    ncol = NCOL(non_target_error))
    } else {
      w_n <- p_n/nn * vonmisespdf(non_target_error, 0, kappa)
    }

    # calculate log likelihood of model
    weights <- rowSums(cbind(w_t, w_g, w_n))
    dLL <- LL - sum(log(weights))
    LL <- sum(log(weights))

    if(abs(dLL) < max_dLL | iter > max_iter | is.nan(dLL)) {
      break
    }

    # calculate p_t, p_n, and p_u
    p_t <- sum(w_t / weights) / n
    p_n <- sum(rowSums(w_n) / weights) / n
    p_u <- sum(w_g / weights) / n

    # improve parameter values via expectation maximisation
    rw <- c((w_t / weights), (w_n / repmat(weights, nn)))
    S <- c(sin(error), sin(non_target_error))
    C <- c(cos(error), cos(non_target_error))
    r <- c(sum(sum(S * rw)), sum(sum(C * rw)))

    if(sum(sum(rw, na.rm = TRUE)) == 0) {
      kappa <- 0
    } else {
      R <- sqrt(sum(r ^ 2)) / sum(sum(rw))
      kappa <- A1inv(R)
    }

    if(n <= 15) {
      if(kappa < 2) {
        kappa <- max(kappa - 2 / (n * kappa), 0)
      } else {
        kappa <- kappa * (n - 1) ^ 3 / (n ^ 3 + n)
      }
    }
  }

  # return parameter values & log likelihood
  if(iter > max_iter) {
    warning('likelihood function:MaxIter','Maximum iteration limit exceeded.',
            call. = FALSE)
    return_parms <- c(NaN, NaN, NaN, NaN)
    LL <- NaN
  } else {
    return_parms <- data.frame(kappa = kappa, p_t = p_t, p_n = p_n, p_u = p_u)
  }

  return(list(parameters = return_parms,
              ll = LL))

}
