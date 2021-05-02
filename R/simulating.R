# simulate data from the models
#' Generate simulated data from mixture models
#' TODO: Documentation!!
#' @export
simulate_mixtur <- function(n_trials,
                            model,
                            kappa,
                            p_u,
                            p_n,
                            K,
                            set_size){

  # error message if unsupported model called
  if(model != "2_component" &&
     model != "3_component" &&
     model != "slots" &&
     model != "slots_averaging"){
    stop("Unidentified model called. Check the 'model' argument in
         simulate_mixtur", call. = FALSE)
  }

  # error message if more than one kappa and K value provided to slots models
  if(model == "slots" || model == "slots_avaeraging"){
    if(length(K) > 1 || length(kappa) > 1){
      stop("For slots models, only provide one value for K and kappa",
           call. = FALSE)
    }
  }

  if(model == "2_component"){
    simulated_data <- simulate_components(n_trials = n_trials,
                                          kappa = kappa,
                                          p_u = p_u,
                                          set_size = set_size)
  }

  if(model == "3_component"){
    simulated_data <- simulate_components(n_trials = n_trials,
                                          kappa = kappa,
                                          p_u = p_u,
                                          p_n = p_n,
                                          set_size = set_size)
  }

  if(model == "slots" || model == "slots_averaging"){
    simulated_data <- simulate_slots(n_trials = n_trials,
                                     model = model,
                                     K = K,
                                     kappa = kappa,
                                     set_size = set_size)
  }

  return(simulated_data)

}

# simulate data from the slots models -------------------------------------
# simulate the slots model. This function not to be called by the user
#' @importFrom dplyr %>%
#' @importFrom dplyr select
#' @importFrom dplyr bind_rows
#' @importFrom dplyr sample_n
simulate_slots <- function(n_trials,
                           model = "slots",
                           K,
                           kappa,
                           set_size = c(1, 2, 4, 8),
                           min_angle_distance = 20){

  # print message to user
  print("Simulating data. Please wait...")

  # work out the number of trials to simulate per set size
  trials_per_set_size <- numeric(length(set_size))
  remainder <- n_trials %% length(set_size)

  for(i in 1:length(trials_per_set_size)){
    if(i == 1){
      trials_per_set_size[i] <- floor(n_trials / length(set_size)) + remainder
    } else {
      trials_per_set_size[i] <- floor(n_trials / length(set_size))
    }
  }

  # loop over each requested set size
  for(i in 1:length(set_size)){

    # create the trial structure to present to the model
    trial_data <- get_angles(n_trials = trials_per_set_size[i],
                             set_size = set_size[i],
                             memory_distance = min_angle_distance)

    # transform the angles to circular space
    trial_data <- round(wrap(trial_data / 180 * pi), 3)

    # add the set size and a blank column to hold the model response
    trial_data <- trial_data %>%
      mutate(set_size = set_size[i],
             response = 0)



    #---- simulate the individual responses

    # generate random numbers to aid the mixture simulation
    # (it's quicker to generate these all at once rather than trial-by-trial)
    rand_num <- runif(trials_per_set_size[i], 0, 1)

    #--- slots model
    if(model == "slots"){
      for(j in 1:trials_per_set_size[i]){

        # if capacity is greater than set size, respond to target value
        if(K >= trial_data$set_size[j]){
          trial_data$response[j] <- round(randomvonmises(1,
                                                         trial_data$target[j],
                                                         kappa), 3)
        }

        # if capacity is lower than set size, respond via a mixture of
        # target responses and uniform guessing
        if(K < trial_data$set_size[j]){

          # probability of responding to target value
          # (capacity divided by sample size)
          p_target <- K / trial_data$set_size[j]

          if(rand_num[j] <= p_target){
            trial_data$response[j] <- round(randomvonmises(1,
                                                           trial_data$target[j],
                                                           kappa), 3)
          } else{
            trial_data$response[j] <- round(runif(1, -pi, pi), 3)
          }
        }

      }

    }

    #--- slots plus averaging model
    if(model == "slots_averaging"){

      for(j in 1:trials_per_set_size[i]){

        # if capacity is greater than the set size...
        if(K >= trial_data$set_size[j]){

          # calculate the probability of encoding target with multiple
          # quanta/slots
          p_high <- K %% trial_data$set_size[j] / trial_data$set_size[j]

          # if the target is encoded with multiple quanta/slots...
          if(rand_num[j] <= p_high){

            # work out how many quanta/slots it will receive...
            kappa_high <- kappa * (floor(K / trial_data$set_size[j]) + 1)

            # simulate the encoding with that many quanta/slots...
            trial_data$response[j] <- randomvonmises(1,
                                                     trial_data$target[j],
                                                     kappa_high)

          } else{

            # if this is not the case, encode with kappa_low
            kappa_low <- kappa * floor(K / trial_data$set_size[j])
            trial_data$response[j] <- randomvonmises(1,
                                                     trial_data$target[j],
                                                     kappa_low)
          }

        } else{

          # if capacity is smaller than set size...

          # work out the probability of guessing
          p_guess <- 1 - (K / trial_data$set_size[j])

          # if we are guessing....
          if(rand_num[j] <= p_guess){

            # select a random radian from -pi to pi
            trial_data$response[j] <- runif(1, -pi, pi)
          } else {

            # otherwise, encode with kappa of one quanta
            trial_data$response[j] <- randomvonmises(1,
                                                     trial_data$target[j],
                                                     kappa)
          }

        }

        trial_data$response[j] <- round(trial_data$response[j], 3)
      }
    }

    # store the data for each set size
    if(i == 1){
      sim_data <- trial_data
    } else {
      sim_data <- bind_rows(sim_data, trial_data)
    }

  }

  if(max(set_size) > 1){
    sim_data <- sim_data %>%
      mutate(id = 1) %>%
      select(id,
             set_size,
             target,
             response,
             contains("non_target"))
  } else{
    sim_data <- sim_data %>%
      mutate(id = 1) %>%
      select(id,
             set_size,
             target,
             response)
  }

  # randomise the trial order
  sim_data <- sim_data %>%
    sample_n(n_trials)


  # print message to user
  print("Simulating data finished.")

  return(sim_data)

}




# simulate data from the component models -----------------------------------
# simulate the mixture model. This function not to be called by the user
#' @importFrom dplyr %>%
#' @importFrom dplyr select
#' @importFrom dplyr near
#' @importFrom dplyr bind_rows
#' @importFrom dplyr contains
#' @importFrom dplyr sample_n
simulate_components <- function(n_trials,
                                kappa,
                                p_u,
                                p_n = NULL,
                                set_size = 4,
                                min_angle_distance = 20){

  # print message to user
  print("Simulating data. Please wait...")

    # check that p_u (or p_u + p_n) do not exceed 1
  if(is.null(p_n)){
    if(max(p_u) > 1){
      stop("All p_u parameters must be less than 1",
           call. = FALSE)
    }
  }

  if(!is.null(p_n)){
    if(max(p_n + p_u) > 1){
      stop("The sum of each p_n and p_u must be less than 1",
           call. = FALSE)
    }
  }

  # check that if the 3-component model is to be simulated that set_sizes
  # are greater than 1
  if(!is.null(p_n)){
    if(max(set_size) == 1){
      stop("The 3-component model requires a set size larger than 1",
           call. = FALSE)
    }
  }

  # check number of parameters match number of set sizes to simulate
  if(length(kappa) != length(set_size) && length(p_u) != length(set_size)){
    stop("The number of model parameters do not match number of set sizes
         requested",
         call. = FALSE)
  }

  if(!is.null(p_n)){
    if(length(p_n) != length(set_size)){
      stop("The number of model parameters do not match number of set sizes
           requested",
           call. = FALSE)
    }
  }

  # work out the number of trials to simulate per set size
  trials_per_set_size <- numeric(length(set_size))
  remainder <- n_trials %% length(set_size)

  for(i in 1:length(trials_per_set_size)){
    if(i == 1){
      trials_per_set_size[i] <- floor(n_trials / length(set_size)) + remainder
    } else {
      trials_per_set_size[i] <- floor(n_trials / length(set_size))
    }
  }


  # calculate p_t from parameters
  if(is.null(p_n)){
    p_t <- 1 - p_u
  } else{
    p_t <- 1 - p_u - p_n
  }

  # loop over each set size requested
  for(i in 1:length(set_size)){

    # create the trial structure to present to the model
    trial_data <- get_angles(n_trials = trials_per_set_size[i],
                             set_size = set_size[i],
                             memory_distance = min_angle_distance)

    # transform the angles to circular space
    trial_data <- round(wrap(trial_data / 180 * pi), 3)

    # get the model response
    if(is.null(p_n)){
      model_data <- get_component_response(trial_data,
                                           set_size = set_size[i],
                                           kappa = kappa[i],
                                           p_t = p_t[i],
                                           p_u = p_u[i],
                                           p_n = 0)
    } else {
      model_data <- get_component_response(trial_data,
                                           set_size = set_size[i],
                                           kappa = kappa[i],
                                           p_t = p_t[i],
                                           p_u = p_u[i],
                                           p_n = p_n[i])
    }

    model_data <- model_data %>%
      mutate(set_size = set_size[i])

    if(i == 1){
      sim_data <- model_data
    } else{
      sim_data <- bind_rows(sim_data, model_data)
    }

  }

  if(max(set_size) > 1){
    sim_data <- sim_data %>%
      select(.data$id,
             set_size,
             .data$target,
             .data$response,
             contains("non_target"))
  } else{
    sim_data <- sim_data %>%
      select(.data$id,
             set_size,
             .data$target,
             .data$response)
  }

  # randomise the trial order
  sim_data <- sim_data %>%
    sample_n(n_trials)

  # print message to user
  print("Simulating data finished.")

  return(sim_data)
}




# get the mixtur model's responses ----------------------------------------
# get the mixtur model's responses
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr slice
#' @importFrom dplyr relocate
#' @importFrom rlang .data
get_component_response <- function(trial_data, set_size, kappa, p_t, p_n, p_u){

  # add relevant column to the trial data frame
  if(set_size == 1){
    model_data <- trial_data %>%
      mutate(response = 0) %>%
      select(.data$target, .data$response)
  } else{
    model_data <- trial_data %>%
      mutate(response = 0) %>%
      select(.data$target, .data$response, contains("non_target"))
  }

  # generate random numbers to aid the mixture simulation
  # (it's quicker to generate these all at once rather than trial-by-trial)
  rand_num <- runif(nrow(trial_data), 0, 1)

  # loop over each trial
  for(i in 1:nrow(trial_data)){

    # get the target value, and a vector of all non-target values
    target <- trial_data$target[i]

    if(set_size > 1){
      distractors <- trial_data[i, ] %>%
        select(contains("non_target")) %>%
        slice() %>%
        unlist(.data, use.names = FALSE)
    }

    #--- generate the model response based on probability

    if(set_size == 1){

      # select the response centered on target with concentration k
      if(rand_num[i] <= p_t){
        model_data$response[i] <- round(randomvonmises(1, target, kappa), 3)
      } else{
        model_data$response[i] <- round(runif(1, -pi, pi), 3)
      }
    }

    if(set_size > 1){

      # select the response centered on target with concentration k
      if(rand_num[i] <= p_t){
        model_data$response[i] <- round(randomvonmises(1, target, kappa), 3)
      }

      # select the response based on a random uniform guess
      if(rand_num[i] > p_t && rand_num[i] <= p_t + p_u){
        model_data$response[i] <- round(runif(1, -pi, pi), 3)
      }

      # select the response centered on one non-target with concentration k
      # if n_stim > 1
      if(rand_num[i] > p_t + p_u){

        # select a random distractor
        if(length(distractors) > 1){
          trial_nt <- base::sample(distractors, 1)
          model_data$response[i] <- round(randomvonmises(1, trial_nt, kappa),
                                          3)
        } else {
          trial_nt <- distractors
          model_data$response[i] <- round(randomvonmises(1, trial_nt, kappa),
                                          3)
        }

      }
    }
  }

  # add id column
  model_data <- model_data %>%
    mutate(id = 1) %>%
    relocate(.data$id)

  return(model_data)

}



# simulate data from the mixture models with fixed angle separation---------------

# simulate the mixture model with fixed angle separation.
# This is NOT the function to be called by the user
#' @importFrom dplyr %>%
#' @importFrom dplyr select
#' @importFrom dplyr near
fixed_angle_simulate_mixtur <- function(n_trials,
                                        kappa,
                                        p_t,
                                        p_n,
                                        p_u,
                                        set_size = 4,
                                        min_angle_distance = 20){


  # print message to user
  print("Simulating data. Please wait...")

  # check that p_t, p_n, and p_u sum to 1
  if(is.null(p_n)){
    if(dplyr::near((p_t + p_u), 1)){
      stop("p_t and p_u do not sum to 1.", call. = FALSE)
    }
  }

  if(!is.null(p_n)){
    if(dplyr::near((p_t + p_n + p_u), 1) != TRUE){
      stop("p_t, p_n, and p_u do not sum to 1.", call. = FALSE)
    }
  }



  # create the trial structure to present to the model
  trial_data <- get_fixed_angles(n_trials = n_trials,
                                 set_size = set_size,
                                 memory_distance = min_angle_distance)

  # transform the angles to circular space
  trial_data <- round(wrap(trial_data / 180 * pi), 3)

  # get the model response
  if(is.null(p_n)){
    model_data <- get_component_response(trial_data,
                                         set_size = set_size,
                                         kappa = kappa,
                                         p_t = p_t,
                                         p_u = p_u,
                                         p_n = 0)

  } else {
    model_data <- get_component_response(trial_data,
                                         set_size = set_size,
                                         kappa = kappa,
                                         p_t = p_t,
                                         p_u = p_u,
                                         p_n = p_n)
  }

  # print message to user
  print("Simulating data finished.")

  return(model_data)
}
