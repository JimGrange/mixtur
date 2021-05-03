# downloaded from https://osf.io/8wzgq/
library(tidyverse)
berry_2019 <- read.csv("berry_2019.csv")

berry_2019 <- berry_2019 %>%
  select(id, condition, target_ori, response_ori, non_target_1, non_target_2)

save(berry_2019, file = "berry_2019.rda", compress='bzip2')


