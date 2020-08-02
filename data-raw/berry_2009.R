# downloaded from https://osf.io/8wzgq/
library(tidyverse)
berry_2009 <- read.csv("berry_2009.csv")

berry_2009 <- berry_2009 %>%
  select(id, condition, target_ori, response_ori, non_target_1, non_target_2)

save(berry_2009, file = "berry_2009.rda")


