# data from experiment 1 of Oberauer & Lin (2017)
# avaioable online here: https://osf.io/j24wb/

# read in the data
data <- read.delim("oberauer_2017.dat", header = FALSE)

# name the columns (see data descriptor on OSF link)
colnames(data) <- c("id",
                    "session",
                    "trial_number",
                    "trial_number_design",
                    "set_size",
                    "target",
                    "target_location",
                    "non_target_1",
                    "non_target_1_location",
                    "non_target_2",
                    "non_target_2_location",
                    "non_target_3",
                    "non_target_3_location",
                    "non_target_4",
                    "non_target_4_location",
                    "non_target_5",
                    "non_target_5_location",
                    "non_target_6",
                    "non_target_6_location",
                    "non_target_7",
                    "non_target_7_location",
                    "response")

# select relevant columns
data <- data %>%
  select(id,
         set_size,
         response,
         target,
         non_target_1,
         non_target_2,
         non_target_3,
         non_target_4,
         non_target_5,
         non_target_6,
         non_target_7)


# add NAs to non_target columns when set sizes less than 8
for(i in 1:nrow(data)){

  if(data$set_size[i] == 1){
    data$non_target_1[i] <- NA
    data$non_target_2[i] <- NA
    data$non_target_3[i] <- NA
    data$non_target_4[i] <- NA
    data$non_target_5[i] <- NA
    data$non_target_6[i] <- NA
    data$non_target_7[i] <- NA
  }

  if(data$set_size[i] == 2){
    data$non_target_2[i] <- NA
    data$non_target_3[i] <- NA
    data$non_target_4[i] <- NA
    data$non_target_5[i] <- NA
    data$non_target_6[i] <- NA
    data$non_target_7[i] <- NA
  }

  if(data$set_size[i] == 3){
    data$non_target_3[i] <- NA
    data$non_target_4[i] <- NA
    data$non_target_5[i] <- NA
    data$non_target_6[i] <- NA
    data$non_target_7[i] <- NA
  }

  if(data$set_size[i] == 4){
    data$non_target_4[i] <- NA
    data$non_target_5[i] <- NA
    data$non_target_6[i] <- NA
    data$non_target_7[i] <- NA
  }

  if(data$set_size[i] == 5){
    data$non_target_5[i] <- NA
    data$non_target_6[i] <- NA
    data$non_target_7[i] <- NA
  }

  if(data$set_size[i] == 6){
    data$non_target_6[i] <- NA
    data$non_target_7[i] <- NA
  }

  if(data$set_size[i] == 7){
    data$non_target_7[i] <- NA
  }
}

# save data file
oberauer_2017 <- data
save(oberauer_2017, file = "oberauer_2017.rda", compress = "bzip2")
