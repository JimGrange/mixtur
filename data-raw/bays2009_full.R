# load required library
library(R.matlab)

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

# round degrees to 2 decimal places
bays_data[, 3:10] <- round(bays_data[, 3:10], 3)

bays2009_full <- bays_data

# save data file
save(bays2009_full, file = "bays2009_full.rda", compress='bzip2')
