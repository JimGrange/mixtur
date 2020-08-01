
# simulate data from the mixture models -----------------------------------


#' simulate the mixture model. This is the function to be called by the user
#' @importFrom dplyr %>%
#' @importFrom dplyr select
#' @export
simulate_mixtur <- function(n_trials,
                            K,
                            p_t,
                            p_n,
                            p_u,
                            set_size = 4,
                            min_angle_distance = 40){


  # print message to user
  print("Simulating data. Please wait...")

  # check that p_t, p_n, and p_u sum to 1
  if(p_n == "NULL"){
    if((p_t + p_u) != 1){
      stop("error: p_t and p_u do not sum to 1.", call. = FALSE)
    }
  }

  if(p_n != "NULL"){
    if((p_t + p_u + p_n) != 1){
      stop("error: p_t, p_n, and p_u do not sum to 1.", call. = FALSE)
    }
  }



  # create the trial structure to present to the model
  trial_data <- get_angles(n_trials = n_trials,
                           set_size = set_size,
                           memory_distance = min_angle_distance)

  # transform the angles to circular space
  trial_data <- round(wrap(trial_data / 180 * pi), 3)

  # get the model response
  if(p_n == "NULL"){
    model_data <- get_model_response(trial_data,
                                     set_size = set_size,
                                     K = K,
                                     p_t = p_t,
                                     p_u = p_u,
                                     p_n = 0)

  } else {
    model_data <- get_model_response(trial_data,
                                     set_size = set_size,
                                     K = K,
                                     p_t = p_t,
                                     p_u = p_u,
                                     p_n = n)
  }

  # print message to user
  print("Simulating data finished.")

  return(model_data)
}




# get the mixtur model's responses ----------------------------------------
#' get the mixtur model's responses
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr slice
#' @export
get_model_response <- function(trial_data, set_size, K, p_t, p_n, p_u){

  # add relevant column to the trial data frame
  if(set_size == 1){
    model_data <- trial_data %>%
      mutate(response = 0) %>%
      select(target, response)
  } else{
    model_data <- trial_data %>%
      mutate(response = 0) %>%
      select(target, response, contains("non_target"))
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
        unlist(., use.names = FALSE)
    }

    #--- generate the model response based on probability

    # select the response centered on target with concentration k
    if(rand_num[i] <= p_t){
      model_data$response[i] <- round(target + randomvonmises(1, 0, K), 3)
    }

    # select the response based on a random uniform guess
    if(rand_num[i] > p_t && rand_num[i] <= p_t + p_u){
      model_data$response[i] <- round(runif(1, -pi, pi), 3)
    }

    # select the response centered on one non-target with concentration k
    # if n_stim > 1
    if((rand_num[i] > p_t + p_u) & set_size > 1){

      # select a random distractor
      trial_nt <- base::sample(distractors, 1)
      model_data$response[i] <- round(trial_nt + randomvonmises(1, 0, K), 3)
    }

  }

  # add id column
  model_data <- model_data %>%
    mutate(id = 1) %>%
    relocate(id)

  return(model_data)

}

