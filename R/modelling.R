# fit the mixtur model ----------------------------------------------------
#' Fit the mixture model.
#'
#' This is the function called by the user to fit either the two- or three-
#' component mixture model.
#'
#' @param data A data frame with columns containing (at the very least)
#' trial-level participant response and target values This data can either be
#' in degrees (1-360 or 1-180) or radians. If the 3-component mixture model is
#' to be fitted to the data, the data frame also needs to contain the values
#' of all non-targets. In addition, the model can be fit to individual
#' individual participants, individual set-sizes, and individual additional
#' conditions; if the user wishes for this, then the data frame should have
#' columns coding for this information.
#'
#' @param components A numeric value indicating whether the 2-component or
#' 3-component mixture model should be fitted to the data.
#'
#' @param unit A character indicating the unit of measurement in the data frame:
#' "degrees" (measurement is in degrees, from 1 to 360); "degrees_180
#' (measurement is in degrees, but limited to 1 to 180); or "radians"
#' (measurement is in radians, from pi to 2 * pi, but could also be already in
#' the range -pi to pi).
#'
#' @param id_var The quoted column name coding for participant id. If the data is from
#' a single participant (i.e., there is no id column) set to NULL.
#'
#' @param response_var The quoted column name coding for the participants' responses
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
#' @param set_size_var The quoted column name (if applicable) coding for the set
#' size of each response.
#'
#' @param condition_var The quoted column name (if applicable) coding for the
#' condition of each response.
#'
#' @param return_fit If set to TRUE, the function will return the negative
#' log-likelihood of the model fit.
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr select
#'
#' @examples
#'
#' # load the example data
#' data <- bays2009_full
#'
#' # fit the 3-component mixture model ignoring condition
#' fit <- fit_mixtur(data = data,
#'                   components = 3,
#'                   unit = "radians",
#'                   id_var = "id",
#'                   response_var = "response",
#'                   target_var = "target",
#'                   non_target_var = "non_target",
#'                   set_size_var = "set_size",
#'                   condition_var = NULL)
#'
#' @export
fit_mixtur <- function(data,
                       components = 3,
                       unit = "degrees",
                       id_var = "id",
                       response_var = "response",
                       target_var = "target",
                       non_target_var = NULL,
                       set_size_var = NULL,
                       condition_var = NULL,
                       return_fit = FALSE){

  # get the non-target column names, if applicable
  if(!is.null(non_target_var)){
    non_target_cols <- data %>%
      select(contains(non_target_var)) %>%
      colnames()
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

  # TODO: Radians not in range -pi to pi

  # print message to user
  print("Model fit running. Please wait...")


  # no set size or condition manipulation
  if(is.null(set_size_var) && is.null(condition_var)){

    # get the set size of the level
    if(!is.null(non_target_var)){
      level_set_size <- length(non_target_cols) + 1
    } else {
      level_set_size <- 1
    }

    # perform the model fit
    fit <- fit_level(data,
                     components = components,
                     id_var = id_var,
                     response_var = response_var,
                     target_var = target_var,
                     non_target_var = non_target_var,
                     set_size = level_set_size,
                     return_fit = return_fit)
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
        filter(condition == conditions[i])

      # get the set size of the level
      if(!is.null(non_target_var)){
        level_set_size <- length(non_target_cols) + 1
      } else {
        level_set_size <- 1
      }

      # fit the model to this condition
      level_fit <- fit_level(level_data,
                             components = components,
                             id_var = id_var,
                             response_var = response_var,
                             target_var = target_var,
                             non_target_var = non_target_var,
                             set_size = level_set_size,
                             return_fit = return_fit)

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
      rename(!!condition_var:=condition)
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
        filter(set_size == set_sizes[i])

      # fit the model to this set size
      if(set_sizes[i] == 1){
        level_fit <- fit_level(level_data,
                               components = components,
                               id_var = "id",
                               response_var = "response",
                               target_var = "target",
                               non_target_var = NULL,
                               set_size = 1,
                               return_fit = return_fit)

        level_fit <- level_fit %>%
          mutate(set_size = set_sizes[i])

      } else{
        level_fit <- fit_level(level_data,
                               components = components,
                               id_var = "id",
                               response_var = "response",
                               target_var = "target",
                               non_target_var = non_target_var,
                               set_size = set_sizes[i],
                               return_fit = return_fit)

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
      rename(!!set_size_var:=set_size)
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
          filter(set_size == set_sizes[i]) %>%
          filter(condition == conditions[j])

        # fit the model to this set size & condition
        if(set_sizes[i] == 1){
          level_fit <- fit_level(level_data,
                                 components = components,
                                 id_var = id_var,
                                 response_var = response_var,
                                 target_var = target_var,
                                 non_target_var = NULL,
                                 set_size = 1,
                                 return_fit = return_fit)

          level_fit <- level_fit %>%
            mutate(set_size = set_sizes[i],
                   condition = conditions[j])
        } else{
          level_fit <- fit_level(level_data,
                                 components = components,
                                 id_var = id_var,
                                 response_var = response_var,
                                 target_var = target_var,
                                 non_target_var = non_target_var,
                                 set_size = set_sizes[i],
                                 return_fit = return_fit)

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
      rename(!!condition_var:=condition) %>%
      rename(!!set_size_var:=set_size)
  }

  # print message to user
  print("Model fit finished.")


  # remove p_n column if the user was fitting the 2-component model
  if(components == 2){
    fit <- fit %>%
      select(-p_n)
  }

  return(fit)

}



# fit model to a single level ---------------------------------------------
#' Fit model to a single level.
#'
#' This wrapper function is called by the \code{fit_mixtur} function to fit the
#' models to a single level from the data frame. It is not expected that this
#' function be called by the user.
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr pull
#'
#' @export
fit_level <- function(data,
                      components = 3,
                      id_var = "id",
                      response_var = "response",
                      target_var = "target",
                      non_target_var,
                      set_size = 1,
                      return_fit = FALSE){

  if(is.null(non_target_var)){
    non_target_var <- NULL
  }

  # get the participant ids
  id <- data %>%
    pull(id_var)

  # move each participant data to separate items in list
  l <- split(data, id)

  # initiate data frame to store parameters
  parms <- data.frame(id = FALSE, K = FALSE, p_t = FALSE, p_n = FALSE,
                      p_u = FALSE, LL = FALSE)

  # loop over every partiipant
  for(i in seq_along(l)) {

    # get the current participant's data
    df <- as.data.frame.list(l[i], col.names = colnames(l[i]))

    # get the response and the target values
    response <- as.matrix(df[, response_var])
    target <- as.matrix(df[, target_var])



    #--- pass the data to the fit function

    # if the 2-component model is called, don't pass non-target info to fit
    if(components == 2){
      fit <- fit_model(response,
                       target,
                       return.ll = return_fit)
    }

    # if the 3-component model is called, pass non-target info to fit
    # only if set size is greater than one (i.e., there is non-target info)
    if(components == 3){

      # get the non-target values for this set size if set size is above one
      if(set_size > 1){
        non_target_cols <- df %>%
          select(starts_with(non_target_var)) %>%
          colnames()

        non_targets <- as.matrix(df[, non_target_cols])
        colnames(non_targets) <- NULL
        non_targets <- as.matrix(non_targets[, 1:(set_size - 1)])
      }

      if(is.null(non_target_var)) {
        fit <- fit_model(response,
                         target,
                         return.ll = return_fit)
      } else {
        fit <- fit_model(response,
                         target,
                         non_targets,
                         return.ll = return_fit)
      }
    }

    # store the parameters
    id <- as.character(df[1, id_var])
    parms[i, 1] <- id

    if(return_fit == TRUE){
      parms[i, 2:5] <- round(fit$parameters, 3)
      parms[i, 6] <- round(fit$LL, 3)
    } else{
      parms[i, 2:5] <- round(fit, 3)
    }

  }

  if(return_fit == TRUE){
    return(parms)
  } else {
    parms <- parms %>% select(-LL)
    return(parms)
  }


}



# fit model ---------------------------------------------------------------
#' Fit the mixture model.
#'
#' This is the function that is called by the wrapper function
#' \code{fit_level}. It is not expected that this function be called by the
#' user.
#'
#' @export
fit_model <- function(response,
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
  K <- c(1, 10, 100)
  N <- c(0.01, 0.1, 0.4)
  U <- c(0.01, 0.1, 0.4)

  if(nn == 0){
    N <- 0
    }

  # initialise log likelihood
  log_lik = -Inf

  # iterate over all starting parmeters and conduct model fit
  for(i in seq_along(K)) {
    for(j in seq_along(N)) {
      for(k in seq_along(U)) {
        est_list <- likelihood_function(response = response,
                                        target = target,
                                        non_targets = non_targets,
                                        start_parms = c(K[i], 1 - N[j] - U[k],
                                                        N[j], U[k]))
          # est_list <- JV10_function(X = response, Tg = target,
          #                           NT = non_targets,
          #                           B_start = c(K[i], 1 - N[j] - U[k],
          #                                      N[j], U[k]))
        if (est_list$ll > log_lik & !is.nan(est_list$ll) ) {
          log_lik <- est_list$ll
          parameters <- est_list$parameters
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



# likelihood function -----------------------------------------------------
#' Calculate the likelihood function of the mixture model.
#'
#' It is not expected that this function be called by the user.
#'
#' @export
likelihood_function <- function(response,
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
  # parameters to parameter variabls
  if(is.null(start_parms)) {
    K <- 5
    p_t <- 0.5
    p_n <- ifelse(nn > 0, 0.3, 0)
    p_u <- 1 - p_t - p_n
  } else {
    K <- start_parms[1];
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
  iter <- 1

  # iterate to minimise log likelihood
  while(TRUE) {
    iter <- iter + 1

    # get the weight contributions of target and guess responses to performance
    w_t <- p_t * vonmisespdf(error, 0, K)
    w_g <- p_u * replicate(n, 1) / (2 * pi)

    # if present, get the weight contribution of non-target responses
    # to performance
    if(nn == 0){
      w_n <- matrix(nrow = NROW(non_target_error),
                    ncol = NCOL(non_target_error))
    } else {
      w_n <- p_n/nn * vonmisespdf(non_target_error, 0, K)
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


    # improve parameter values
    rw <- c((w_t / weights), (w_n / repmat(weights, nn)))
    S <- c(sin(error), sin(non_target_error))
    C <- c(cos(error), cos(non_target_error))
    r <- c(sum(sum(S * rw)), sum(sum(C * rw)))

    if(sum(sum(rw, na.rm = TRUE)) == 0) {
      K <- 0
    } else {
      R <- sqrt(sum(r ^ 2)) / sum(sum(rw))
      K <- A1inv(R)
    }

    if(n <= 15) {
      if(K < 2) {
        K <- max(K - 2 / (n * K), 0)
      } else {
        K <- K * (n - 1) ^ 3 / (n ^ 3 + n)
      }
    }
  }

  # return parameter values & log likelihood
  if(iter > max_iter) {
    warning('likelihood function:MaxIter','Maximum iteration limit exceeded.', call. = FALSE)
    return_parms <- c(NaN, NaN, NaN, NaN)
    LL <- NaN
  } else {
    return_parms <- data.frame(K = K, p_t = p_t, p_n = p_n, p_u = p_u)
  }

  return(list(parameters = return_parms,
              ll = LL))

}


# model comparison --------------------------------------------------------
#' Conduct formal model comparison of the 2-component and 3-component model.
#'
#' This function will fit both the 2- and 3-component models to the data
#' and provide an assessment of which fits best via Akiake's Information
#' Criterion (AIC).
#'
#' @param data A data frame with columns containing (at the very least)
#' trial-level participant response and target values This data can either be
#' in degrees (1-360 or 1-180) or radians. If the 3-component mixture model is
#' to be fitted to the data, the data frame also needs to contain the values
#' of all non-targets. In addition, the model can be fit to individual
#' individual participants, individual set-sizes, and individual additional
#' conditions; if the user wishes for this, then the data frame should have
#' columns coding for this information.
#'
#' @param unit A character indicating the unit of measurement in the data frame:
#' "degrees" (measurement is in degrees, from 1 to 360); "degrees_180
#' (measurement is in degrees, but limited to 1 to 180); or "radians"
#' (measurement is in radians, from pi to 2 * pi, but could also be already in
#' the range -pi to pi).
#'
#' @param id_var The quoted column name coding for participant id. If the data is from
#' a single participant (i.e., there is no id column) set to NULL.
#'
#' @param response_var The quoted column name coding for the participants' responses
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
#' @param set_size_var The quoted column name (if applicable) coding for the set
#' size of each response.
#'
#' @param condition_var The quoted column name (if applicable) coding for the
#'
#'@return \code{ll_2} The log-likelihood values for the 2-component model.
#'@return \code{ll_3} The log-likelihood values for the 3-component model.
#'@return \code{aic_2} Akaike's information criterion for the 2-component
#'model.
#'@return \code{aic_3} Akaike's information criterion for the 2-component
#'model.
#'@return \code{aic_difference}. The difference of AIC values between models.
#'Calculated as aic_2 minus aic_3, positive values indicate better fit for
#'the 3-component model.
#'
#'@importFrom dplyr %>%
#'@importFrom dplyr rename
#'@importFrom dplyr mutate
#'
#' @export
#'
model_comparison <- function(data,
                             unit = "degrees",
                             id_var = "id",
                             response_var = "response",
                             target_var = "target",
                             non_target_var,
                             set_size_var = NULL,
                             condition_var = NULL){

  # get the non-target column names
  non_target_cols <- data %>%
    select(contains(non_target_var)) %>%
    colnames()

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

  # TODO: Radians not in range -pi to pi

  # print message to user
  print("Model fit running. Please wait...")

  # no set size or condition manipulation
  if(is.null(set_size_var) && is.null(condition_var)){

    # get the set size of the level
    level_set_size <- length(non_target_cols) + 1

    # fit the 2-component model
    fit_2 <- fit_level(data,
                       components = 2,
                       id_var = id_var,
                       response_var = response_var,
                       target_var = target_var,
                       non_target_var = NULL,
                       set_size = level_set_size,
                       return_fit = TRUE)

    # fit the 3-component model
    fit_3 <- fit_level(data,
                       components = 3,
                       id_var = id_var,
                       response_var = response_var,
                       target_var = target_var,
                       non_target_var = non_target_var,
                       set_size = level_set_size,
                       return_fit = TRUE)

    # collate together into a final data frame
    final_data <- data.frame(id = fit_2$id,
                             ll_2 = fit_2$LL,
                             ll_3 = fit_3$LL,
                             aic_2 = aic(fit_2$LL, 2),
                             aic_3 = aic(fit_3$LL, 3))
    final_data <- final_data %>%
      mutate(aic_difference = aic_2 - aic_3)

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
        filter(condition == conditions[i])

      # get the set size of the level
      if(!is.null(non_target_var)){
        level_set_size <- length(non_target_cols) + 1
      } else {
        level_set_size <- 1
      }

      # fit the 2-component model
      level_fit_2 <- fit_level(level_data,
                               components = 2,
                               id_var = id_var,
                               response_var = response_var,
                               target_var = target_var,
                               non_target_var = NULL,
                               set_size = level_set_size,
                               return_fit = TRUE)
      level_fit_2 <- level_fit_2 %>%
        mutate(condition = conditions[i])

      # fit the 3-component model
      level_fit_3 <- fit_level(level_data,
                               components = 3,
                               id_var = id_var,
                               response_var = response_var,
                               target_var = target_var,
                               non_target_var = non_target_var,
                               set_size = level_set_size,
                               return_fit = TRUE)
      level_fit_3 <- level_fit_3 %>%
        mutate(condition = conditions[i])

      # stitch data together
      if(i == 1){
        fit_2 <- level_fit_2
        fit_3 <- level_fit_3
      } else {
        fit_2 <- rbind(fit_2, level_fit_2)
        fit_3 <- rbind(fit_3, level_fit_3)
      }

    }

    # rename columns
    fit_2 <- fit_2 %>%
      rename(!!condition_var:=condition)

    fit_3 <- fit_3 %>%
      rename(!!condition_var:=condition)

    # collate together into a final data frame
    final_data <- data.frame(id = fit_2$id,
                             condition = fit_2[[condition_var]],
                             ll_2 = fit_2$LL,
                             ll_3 = fit_3$LL,
                             aic_2 = aic(fit_2$LL, 2),
                             aic_3 = aic(fit_3$LL, 3))
    final_data <- final_data %>%
      mutate(aic_difference = aic_2 - aic_3) %>%
      rename(!!condition_var:=condition)

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
        filter(set_size == set_sizes[i])

      # fit the 2-component model
      level_fit_2 <- fit_level(level_data,
                               components = 2,
                               id_var = id_var,
                               response_var = response_var,
                               target_var = target_var,
                               non_target_var = NULL,
                               set_size = set_sizes[i],
                               return_fit = TRUE)
      level_fit_2 <- level_fit_2 %>%
        mutate(set_size = set_sizes[i])

      # fit the 3-component model
      if(set_sizes[i] == 1){
        level_fit_3 <- fit_level(level_data,
                                 components = 3,
                                 id_var = id_var,
                                 response_var = response_var,
                                 target_var = target_var,
                                 non_target_var = NULL,
                                 set_size = 1,
                                 return_fit = TRUE)
      } else {
        level_fit_3 <- fit_level(level_data,
                                 components = 3,
                                 id_var = id_var,
                                 response_var = response_var,
                                 target_var = target_var,
                                 non_target_var = non_target_var,
                                 set_size = set_sizes[i],
                                 return_fit = TRUE)
      }
      level_fit_3 <- level_fit_3 %>%
        mutate(set_size = set_sizes[i])

      # stitch data together
      if(i == 1){
        fit_2 <- level_fit_2
        fit_3 <- level_fit_3
      } else {
        fit_2 <- rbind(fit_2, level_fit_2)
        fit_3 <- rbind(fit_3, level_fit_3)
      }

    }

    # rename columns
    fit_2 <- fit_2 %>%
      rename(!!set_size_var:=set_size)

    fit_3 <- fit_3 %>%
      rename(!!set_size_var:=set_size)

    # collate together into a final data frame
    final_data <- data.frame(id = fit_2$id,
                             set_size = fit_2[[set_size_var]],
                             ll_2 = fit_2$LL,
                             ll_3 = fit_3$LL,
                             aic_2 = aic(fit_2$LL, 2),
                             aic_3 = aic(fit_3$LL, 3))
    final_data <- final_data %>%
      mutate(aic_difference = aic_2 - aic_3) %>%
      rename(!!set_size_var:=set_size)
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
          filter(set_size == set_sizes[i]) %>%
          filter(condition == conditions[j])

        # fit the 2-component model
        if(set_sizes[i] == 1){
          level_fit_2 <- fit_level(level_data,
                                   components = 2,
                                   id_var = id_var,
                                   response_var = response_var,
                                   target_var = target_var,
                                   non_target_var = NULL,
                                   set_size = 1,
                                   return_fit = TRUE)
        } else {
          level_fit_2<- fit_level(level_data,
                                  components = 2,
                                  id_var = id_var,
                                  response_var = response_var,
                                  target_var = target_var,
                                  non_target_var = NULL,
                                  set_size = set_sizes[i],
                                  return_fit = TRUE)
        }
        level_fit_2 <- level_fit_2 %>%
          mutate(set_size = set_sizes[i],
                 condition = conditions[j])

        # fit the 3-component model
        if(set_sizes[i] == 1){
          level_fit_3 <- fit_level(level_data,
                                   components = 3,
                                   id_var = id_var,
                                   response_var = response_var,
                                   target_var = target_var,
                                   non_target_var = NULL,
                                   set_size = 1,
                                   return_fit = TRUE)
        } else {
          level_fit_3<- fit_level(level_data,
                                  components = 3,
                                  id_var = id_var,
                                  response_var = response_var,
                                  target_var = target_var,
                                  non_target_var = non_target_var,
                                  set_size = set_sizes[i],
                                  return_fit = TRUE)
        }
        level_fit_3 <- level_fit_3 %>%
          mutate(set_size = set_sizes[i],
                 condition = conditions[j])

        # stitch data together
        if(i == 1){
          fit_2 <- level_fit_2
          fit_3 <- level_fit_3
        } else {
          fit_2 <- rbind(fit_2, level_fit_2)
          fit_3 <- rbind(fit_3, level_fit_3)
        }

      }
    }

    # rename columns
    fit_2 <- fit_2 %>%
      rename(!!condition_var:=condition) %>%
      rename(!!set_size_var:=set_size)

    fit_3 <- fit_3 %>%
      rename(!!condition_var:=condition) %>%
      rename(!!set_size_var:=set_size)

    # collate together into a final data frame
    final_data <- data.frame(id = fit_2$id,
                             set_size = fit_2[[set_size_var]],
                             condition = fit_2[[condition_var]],
                             ll_2 = fit_2$LL,
                             ll_3 = fit_3$LL,
                             aic_2 = aic(fit_2$LL, 2),
                             aic_3 = aic(fit_3$LL, 3))
    final_data <- final_data %>%
      mutate(aic_difference = aic_2 - aic_3) %>%
      rename(!!set_size_var:=set_size,
             !!condition_var:=condition)

  }


  # print message to user
  print("Model fit finished.")

  return(final_data)

}
