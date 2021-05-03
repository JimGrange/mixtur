# load required library
library(R.matlab)
library(tidyverse)

# import the data
data <- readMat("bays_data.mat")

# declare some functions
radians_to_degrees <- function(rad) {(rad * 180) / (pi)}
degrees_to_radians <- function(deg){deg * pi / 180}

# only use certain columns
bays_data <- data.frame(
  id = data$subject,
  set_size = data$n.items,
  duration = data$delay,
  response = data$response,
  target = data$target,
  non_target_1 = data$nontarget[, 1],
  non_target_2 = data$nontarget[, 2],
  non_target_3 = data$nontarget[, 3],
  non_target_4 = data$nontarget[, 4],
  non_target_5 = data$nontarget[, 5]
)

# transform NaNs into NAs
bays_data$non_target_1[is.nan(as.numeric(bays_data$non_target_1))] <- NA
bays_data$non_target_2[is.nan(as.numeric(bays_data$non_target_2))] <- NA
bays_data$non_target_3[is.nan(as.numeric(bays_data$non_target_3))] <- NA
bays_data$non_target_4[is.nan(as.numeric(bays_data$non_target_4))] <- NA
bays_data$non_target_5[is.nan(as.numeric(bays_data$non_target_5))] <- NA


# # loop over rows and translate radians to degrees
# for(i in 1:nrow(bays_data)){
#
#   if(bays_data$response[i] < 0){
#     bays_data$response[i] <- abs(bays_data$response[i]) + pi
#   }
#   bays_data$response[i] <- radians_to_degrees(bays_data$response[i])
#
#   if(bays_data$target[i] < 0){
#     bays_data$target[i] <- abs(bays_data$target[i]) + pi
#   }
#   bays_data$target[i] <- radians_to_degrees(bays_data$target[i])
#
#   if((!is.na(bays_data$non_target_1[i])) && (bays_data$non_target_1[i] < 0)){
#     bays_data$non_target_1[i] <- abs(bays_data$non_target_1[i]) + pi
#   }
#   bays_data$non_target_1[i] <- radians_to_degrees(bays_data$non_target_1[i])
#
#   if((!is.na(bays_data$non_target_2[i])) && (bays_data$non_target_2[i] < 0)){
#     bays_data$non_target_2[i] <- abs(bays_data$non_target_2[i]) + pi
#   }
#   bays_data$non_target_2[i] <- radians_to_degrees(bays_data$non_target_2[i])
#
#   if((!is.na(bays_data$non_target_3[i])) && (bays_data$non_target_3[i] < 0)){
#     bays_data$non_target_3[i] <- abs(bays_data$non_target_3[i]) + pi
#   }
#   bays_data$non_target_3[i] <- radians_to_degrees(bays_data$non_target_3[i])
#
#   if((!is.na(bays_data$non_target_4[i])) && (bays_data$non_target_4[i] < 0)){
#     bays_data$non_target_4[i] <- abs(bays_data$non_target_4[i]) + pi
#   }
#   bays_data$non_target_4[i] <- radians_to_degrees(bays_data$non_target_4[i])
#
#   if((!is.na(bays_data$non_target_5[i])) && (bays_data$non_target_5[i] < 0)){
#     bays_data$non_target_5[i] <- abs(bays_data$non_target_5[i]) + pi
#   }
#   bays_data$non_target_5[i] <- radians_to_degrees(bays_data$non_target_5[i])
# }

# round degrees to 3 decimal places
bays_data[, 3:10] <- round(bays_data[, 3:10], 3)

# only select a snippet of the data for simple overview
bays2009_sample <- bays_data %>%
  filter(set_size == 4) %>%
  filter(duration == 500) %>%
  select(-set_size, -duration, -non_target_4, -non_target_5)


# save data file
save(bays2009_sample, file = "bays2009_sample.rda", compress='bzip2')
