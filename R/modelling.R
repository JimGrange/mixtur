# fit the mixtur model ----------------------------------------------------
#' Fit the mixture model. This is the function to be called by the user
#' @importFrom dplyr %>%
#' @importFrom dplyr select
#' @export
fit_mixtur <- function(data,
                       unit = "degrees",
                       id_var = "id",
                       response_var = "response",
                       target_var = "target",
                       non_target_var = "NULL",
                       set_size_var = "NULL",
                       condition_var = "NULL"){

  # get the non-target column names, if applicable
  if(non_target_var != "NULL"){
    non_target_cols <- data %>%
      select(contains(non_target_var)) %>%
      colnames()
  }

  # change degrees to radians
  if(unit == "degrees"){
    data[[response_var]] <- data[[response_var]] / 180 * pi
    data[[target_var]] <- data[[target_var]] / 180 * pi

     if(non_target_var != "NULL"){
      data[, non_target_cols] <- data[, non_target_cols] / 180 * pi
    }

  }

  # change degrees_180 to radians
  if(unit == "degrees_180"){
    data[[response_var]] <- data[[response_var]] / 90 * pi
    data[[target_var]] <- data[[target_var]] / 90 * pi

    if(non_target_var != "NULL"){
      data[, non_target_cols] <- data[, non_target_cols] / 90 * pi
    }

  }

  if(unit == "radians"){
    data <- data
  }

  # TODO: Radians not in range -pi to pi



  # no set size or condition manipulation
  if(set_size_var == "NULL" && condition_var == "NULL"){

    # perform the model fit
    fit <- fit_level(data)

  }


  # no set size manipulation but there is a condition manipulation
  if(set_size_var == "NULL" && condition_var != "NULL"){
  }

  # set size manipulation, but no condition manipulation
  if(set_size_var != "NULL" && condition_var == "NULL"){

    # get the list of set_sizes
    data$set_size <- data[[set_size_var]]
    set_sizes <- unique(data[[set_size_var]])

    # loop over each set size
    for(i in 1:length(set_sizes)){

      # get the current set size's data
      set_data <- data %>%
        filter(set_size == set_sizes[i])

      # fit the model to this set size
      if(set_sizes[i] == 1){
        set_fit <- fit_level(set_data,
                             id_var = "id",
                             response_var = "response",
                             target_var = "target",
                             non_target_var = NULL)
        set_fit <- set_fit %>%
          mutate(set_size = set_sizes[i])
      } else{
        set_fit <- fit_level(set_data,
                             id_var = "id",
                             response_var = "response",
                             target_var = "target",
                             non_target_var = "non_target",
                             set_size = set_sizes[i])
        set_fit <- set_fit %>%
          mutate(set_size = set_sizes[i])
      }


      # stitch data together
      if(i == 1){
        fit <- set_fit
      } else {
        fit <- rbind(fit, set_fit)
      }
    }
  }

  # both set size & condition manipulation
  if(set_size_var != "NULL" && condition_var != "NULL"){

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
                                 id_var = id_var,
                                 response_var = response_var,
                                 target_var = target_var,
                                 non_target_var = NULL
                                 )
          level_fit <- level_fit %>%
            mutate(set_size = set_sizes[i],
                   condition = conditions[j])
        } else{
          level_fit <- fit_level(level_data,
                                 id_var = id_var,
                                 response_var = response_var,
                                 target_var = target_var,
                                 non_target_var = "non_target",
                                 set_size = set_sizes[i])
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
  }

  return(fit)

}




# fit model to a single level ---------------------------------------------
#' fit model to a single level
#' @importFrom dplyr %>%
#' @importFrom dplyr pull
#' @export
fit_level <- function(data,
                      id_var = "id",
                      response_var = "response",
                      target_var = "target",
                      non_target_var = "non_target",
                      set_size = 1){


  # get the participant ids
  id <- data %>%
    pull(id_var)

  # move each participant data to separate items in list
  l <- split(data, id)

  # initiate data frame to store parameters
  parms <- data.frame(id = FALSE, K = FALSE, p_t = FALSE, p_n = FALSE,
                      p_u = FALSE)

  # loop over every partiipant
  for(i in seq_along(l)) {

    # get the current participant's data
    df <- as.data.frame.list(l[i], col.names = colnames(l[i]))

    # get the response and the target values
    response <- as.matrix(df[, response_var])
    target <- as.matrix(df[, target_var])

    # get the non-target values for this set size if set size is above one

    if(set_size > 1){
      non_target_cols <- df %>%
        select(starts_with(non_target_var)) %>%
        colnames()

      non_targets <- as.matrix(df[, non_target_cols])
      colnames(non_targets) <- NULL
      non_targets <- as.matrix(non_targets[, 1:(set_size - 1)])
    }


    #--- pass the data to the fit function

    # if there are no targets, just fit the 2-component model, else
    # fit the 3-component model
    if(is.null(non_target_var)) {
      fit <- fit_model(response,
                       target,
                       return.ll = FALSE)
    } else {

      # fit the model
      fit <- fit_model(response,
                       target,
                       non_targets,
                       return.ll = FALSE)
    }

    # store the parameters
    id <- as.character(df[1, id_var])
    parms[i, 1] <- id
    parms[i, 2:5] <- round(fit, 3)
  }

  return(parms)

}





# fit model ---------------------------------------------------------------
#' fit model
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
#' likelihood function
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
