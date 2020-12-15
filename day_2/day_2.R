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
         valid = count >= pw_input1 & count <= pw_input2) %>%
  group_by(valid) %>%
  count()

# Part 2

passwords %>%
  mutate(possible_positions = map2(.x = pw,
                                   .y = pw_char,
                                   .f = ~str_locate_all(.x, .y)) %>%
                              map(~unlist(pluck(.x, 1)[,1])),
         pw_input1_test = map2_lgl(.x = pw_input1,
                               .y = possible_positions,
                               .f = ~ .x %in% unlist(.y)),
         pw_input2_test = map2_lgl(.x = pw_input2,
                               .y = possible_positions,
                               .f = ~ .x %in% unlist(.y)),
         valid = pw_input1_test != pw_input2_test) %>%
  group_by(valid) %>%
  count()
