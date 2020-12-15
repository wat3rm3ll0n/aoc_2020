# Part 1
library(tidyverse)

input <- read_lines("day_2/input.txt")

passwords <- str_match(input, "([0-9]+)-([0-9]+)\\s([a-z]+):\\s([a-z]+)") %>%
  as_tibble() %>%
  rename(pw_code = "V1",
         pw_input1 = "V2",
         pw_input2 = "V3",
         pw_char = "V4",
         pw = "V5") %>%
  mutate(pw_input1 = as.numeric(pw_input1),
         pw_input2 = as.numeric(pw_input2))

passwords %>%
  mutate(count = map2_int(.x = pw, 
                      .y = pw_char,
                      .f = ~ str_count(.x, .y)
                      ),
         valid = if_else(count >= pw_input1 & count <= pw_input2,
                   "Valid",
                   "Invalid")) %>%
  group_by(valid) %>%
  count()

# Part 2

test <- passwords %>%
  mutate(possible_positions = map2(.x = pw,
                                   .y = pw_char,
                                   .f = ~str_locate_all(.x, .y))) %>%
  unnest(possible_positions)
  
