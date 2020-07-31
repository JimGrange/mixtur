# get a matrix of target & non-target angles ------------------------------
# Get a matrix of target and non-target angles
#' @export
get_angles <- function(n_trials, set_size = 4, memory_distance = 40){

  if((set_size != 1) & (set_size != 2) & (set_size !=3) &
     (set_size != 4) & (set_size != 6)){
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
