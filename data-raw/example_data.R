data <- read.csv("example_data.csv")

data <- data %>%
  rename(non_target_1 = nt1,
         non_target_2 = nt2,
         non_target_3 = nt3) %>%
  mutate(trim_cond = paste(sequence, condition, sep = "_")) %>%
  filter(trim_cond != "repetition_mixed") %>%
  select(-trim_cond) %>%
  filter(cuing_interval == "short") %>%
  select(-cuing_interval) %>%
  filter(id != "21_short",
         id != "22_short",
         id != "23_short",
         id != "25_short",
         id != "26_short") %>%
  select(-task, -sequence) %>%
  mutate(id = str_replace_all(id, pattern = "_short*", replacement = "")) %>%
  mutate(condition = fct_recode(condition, "condition_a" = "pure",
                                "condition_b" = "mixed"))

data[, 3:7] <- round(data[, 3:7], 0)

save(data, file = "example_data.rda")


