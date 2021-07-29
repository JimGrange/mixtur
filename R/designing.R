# get a matrix of target & non-target angles ------------------------------
# Get a matrix of target and non-target angles
get_angles <- function(n_trials, set_size = 4, memory_distance = 20){

  if(set_size > 8){
    return("ERROR: Only use set sizes from 1 to 8")
  }

  # data frame to store trial information in
  if(set_size == 1){
    angles <- data.frame(target = FALSE)
  }

  if(set_size == 2){
    angles <- data.frame(target = FALSE, non_target_1 = FALSE)
  }

  if(set_size == 3){
    angles <- data.frame(target = FALSE,
                         non_target_1 = FALSE,
                         non_target_2 = FALSE)
  }

  if(set_size == 4){
    angles <- data.frame(target = FALSE,
                         non_target_1 = FALSE,
                         non_target_2 = FALSE,
                         non_target_3 = FALSE)
  }

  if(set_size == 5){
    angles <- data.frame(target = FALSE,
                         non_target_1 = FALSE,
                         non_target_2 = FALSE,
                         non_target_3 = FALSE,
                         non_target_4 = FALSE)
  }

  if(set_size == 6){
    angles <- data.frame(target = FALSE,
                         non_target_1 = FALSE,
                         non_target_2 = FALSE,
                         non_target_3 = FALSE,
                         non_target_4 = FALSE,
                         non_target_5 = FALSE)
  }

  if(set_size == 7){
    angles <- data.frame(target = FALSE,
                         non_target_1 = FALSE,
                         non_target_2 = FALSE,
                         non_target_3 = FALSE,
                         non_target_4 = FALSE,
                         non_target_5 = FALSE,
                         non_target_6 = FALSE)
  }

  if(set_size == 8){
    angles <- data.frame(target = FALSE,
                         non_target_1 = FALSE,
                         non_target_2 = FALSE,
                         non_target_3 = FALSE,
                         non_target_4 = FALSE,
                         non_target_5 = FALSE,
                         non_target_6 = FALSE,
                         non_target_7 = FALSE)
  }


  # loop over n_trials and populate angles
  for(i in 1:n_trials){

    # set the distance to zero
    d <- 0

    # repeat this loop until the minimum distance between angles
    # is greater than our minimum memory_distance
    while(d < memory_distance){
      distances <- NULL
      angle <- runif(set_size, 0, 360)
      for(j in 1:length(angle)){
        for(k in 1:length(angle)){
          if(j == k){
            distances <- c(distances, 360)
          } else{
            distances <- c(distances, (angle[j] - angle[k]) %% 360)
          }
        }
      }

      # find the minimum angle difference between memoranda.
      # the loop breaks when this value is greater than memory_distance
      d <- min(distances)

    }

    # store the colour angles
    angles[i, ] <- round(angle, 0)

  }
  return(angles)
}





# get a matrix of target & non-target angles ------------------------------
# Get a matrix of target and non-target angles that are a fixed distance
get_fixed_angles <- function(n_trials, set_size = 4, memory_distance = 20){

  if((set_size != 1) & (set_size != 2) & (set_size !=3) &
     (set_size != 4) & (set_size != 6) & (set_size != 8)){
    return("ERROR: Only use 1, 2, 4, 6, or 8 stimuli!")
  }

  # data frame to store trial information in
  if(set_size == 1){
    angles <- data.frame(target = FALSE)
  }

  if(set_size == 2){
    angles <- data.frame(target = FALSE, non_target_1 = FALSE)
  }

  if(set_size == 3){
    angles <- data.frame(target = FALSE,
                         non_target_1 = FALSE,
                         non_target_2 = FALSE)
  }

  if(set_size == 4){
    angles <- data.frame(target = FALSE,
                         non_target_1 = FALSE,
                         non_target_2 = FALSE,
                         non_target_3 = FALSE)
  }

  if(set_size == 6){
    angles <- data.frame(target = FALSE,
                         non_target_1 = FALSE,
                         non_target_2 = FALSE,
                         non_target_3 = FALSE,
                         non_target_4 = FALSE,
                         non_target_5 = FALSE)
  }

  if(set_size == 8){
    angles <- data.frame(target = FALSE,
                         non_target_1 = FALSE,
                         non_target_2 = FALSE,
                         non_target_3 = FALSE,
                         non_target_4 = FALSE,
                         non_target_5 = FALSE,
                         non_target_6 = FALSE,
                         non_target_7 = FALSE)
  }

  # generate all randon angles for first stimulus (quicker than doing it in the loop)
  all_angles <- runif(n_trials, 0, 360)

  # loop over n_trials and populate angles
  for(i in 1:n_trials){

    # randomly generate the first angle
    first_angle <- all_angles[i]

    # create the array of all angles
    colour <- c(first_angle,
                (first_angle + memory_distance) %% 360,
                (first_angle + (memory_distance * 2)) %% 360,
                (first_angle + (memory_distance * 3)) %% 360)

    # store the colour angles
    angles[i, ] <- round(colour, 0)


  }
  return(angles)
}
