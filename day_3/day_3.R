library(tidyverse)

## Part 1

input <- read_lines("day_3/input.txt")

trees <- function(input, right, down){

  row_length <- as.numeric(unique(str_count(input)))

  trees <- input %>%  
            str_locate_all("\\#") %>%
            map(~unlist(pluck(.x)[,1])) %>%
            tibble(trees = ., 
                   zero_index = seq_along(.)-1,
                   down_index = zero_index %% down == 0) %>%
            filter(down_index) %>%
            mutate(right_index = seq_along(trees)-1) %>%
            mutate(right_position = ((right*right_index)+1) %% row_length,
                   right_position_mod = if_else(right_position == 0,
                                            row_length,
                                            right_position),
                   hits = map2_lgl(.x = trees,
                                   .y = right_position_mod,
                                   .f = ~.y %in% unlist(.x)
                                   )
                   ) %>%
            group_by(hits) %>%
            count() %>%
            pluck("n", 2) %>%
            as.numeric()
  
  print(trees)
}

trees(input, 3, 1)

## Part 2

trees(input, 1, 1) * 
  trees(input, 3, 1) * 
  trees(input, 5, 1) * 
  trees(input, 7, 1) * 
  trees(input, 1, 2)




