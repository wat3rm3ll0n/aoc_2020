# Part 1
library(tidyverse)

input <- read_tsv("day_2/input.txt", col_names = FALSE)

input %>%
  mutate(lower_bound = str_extract(),
         upper_bound = str_extract(),
         rule = str_extract(),
         code = str_extract(),
         valid = str_detect()) 