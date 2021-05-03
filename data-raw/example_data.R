example_data <- read.csv("example_data.csv")

example_data <- example_data %>%
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
  mutate(condition = fct_recode(condition, "a" = "mixed",
                                "b" = "pure"))

example_data[, 3:7] <- round(example_data[, 3:7], 0)

save(example_data, file = "example_data.rda", compress='xz')


