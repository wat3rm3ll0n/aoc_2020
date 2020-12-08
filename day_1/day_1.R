# Part 1

library(tidyverse)

input_x <- scan("day_1/input.txt")
input_y <- input_x

grid <- expand_grid(input_x, input_y)

grid <- grid %>%
  mutate(combo = input_x + input_y) %>%
  filter(combo == 2020) %>%
  mutate(multiply = input_x * input_y) %>%
  distinct(multiply)

# Part 2

input_z <- input_y

grid <- expand_grid(input_x, input_y, input_z)

grid <- grid %>%
  mutate(combo = input_x + input_y + input_z) %>%
  filter(combo == 2020) %>%
  mutate(multiply = input_x * input_y * input_z) %>%
  distinct(multiply)
