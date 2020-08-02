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









# # set the radius of the circle
# radius <- 60
#
# # set where the circle should be centered
# circle_center_a <- 20
# circle_center_b <- 38
# # establish a blank data frame to collect the x- and y-axis coordinates
# # to then sample the a and b parameters in CIE-lab space
# lab_space <- data.frame(
#   a = numeric(360),
#   b = numeric(360)
# )
#
# # loop over every degree
# for(i in 1:360){
#
#   # get the current angle in radians rather than degrees
#   degree_in_radians <- degrees_to_radians(i)
#
#   # get the value for the x-axis (the a parameter in lab space)
#   lab_space$a[i] <- circle_center_a + radius * cos(degree_in_radians)
#
#   # get the value for the y-axis (the b parameter in lab space)
#   lab_space$b[i] <- circle_center_b + radius* sin(degree_in_radians)
#
# }
#
# # do the plot (a circle centered on our circle_center parameter)
# plot(lab_space, ylim = c(-100, 100), xlim = c(-100, 100))
# abline(h = 0)
# abline(v = 0)
#
#
# ### plot the CIE lab colour space
# library(grDevices)
#
# luminance <- 70
# a_parameters <- -128:128
# b_parameters <- -128:128
#
# par(bg = "grey")
# plot(NULL, xlim = c(-128, 128), ylim = c(-128, 128),
#      xlab = "a", ylab = "b")
#
# for(a in a_parameters){
#   for(b in b_parameters){
#
#     colour <- convertColor(c(luminance, a, b),
#                            from = "Lab",
#                            to = "sRGB",
#                            clip = TRUE)
#
#     points(x = a, y = b, pch = 19, col = rgb(colour))
#
#   }
# }
#
# # overlay the circle template
# points(lab_space, pch = 19)
# abline(h = 0)
# abline(v = 0)
#
#
# ### cut this circle out
# par(bg = "grey")
# plot(NULL, xlim = c(-128, 128), ylim = c(-128, 128))
#
# for(i in 1:nrow(lab_space)){
#
#   a <- lab_space$a[i]
#   b <- lab_space$b[i]
#
#   colour <- convertColor(c(luminance, a, b),
#                          from = "Lab",
#                          to = "sRGB",
#                          clip = TRUE)
#
#   points(x = a, y = b, pch = 19, cex = 2, col = rgb(colour))
#
#
# }
# abline(h = 0)
# abline(v = 0)
#
# rescale <- function(x){
#   return((x * 2) - 1)
# }
#
# revert <- function(x){
#   return((x + 1) / 2)
# }
#
#
# #https://www.easyrgb.com/en/convert.php#inputFORM
#
# rgb <- seq(from = 0, to = 1, by = 0.001)
# rescaled <- rescale(rgb)
# reverted <- revert(rescaled)
#
# rescaled_rgb <- data.frame(rgb = rgb,
#                            rescaled = rescaled,
#                            reverted = reverted)
#
#
# y <- convertColor(c(53, -20, 0),
#                   from = "Lab",
#                   to = "sRGB",
#                   clip = TRUE)




