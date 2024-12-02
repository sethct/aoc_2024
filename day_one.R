library(tidyverse)

#----------#
# Part One #
#----------#

# Read in data
day_one_raw <- read.csv('day_one.csv')

# split out columns
list_one <- sort(day_one_raw$list_one, decreasing = F) 
list_two <- sort(day_one_raw$list_two, decreasing = F) 

# join together
day_one_sorted <- data.frame(listone = list_one,
                             listtwo = list_two)

# calculate difference
day_one_distance <- day_one_sorted |>
  mutate(difference = abs(list_two - list_one))

# sum difference
ans_p1 <- sum(day_one_distance$difference)

#----------#
# Part Two #
#----------#

# count appearances of list 1 values in list 2
part_two <- day_one_sorted |>
  count(listtwo) |>        
  right_join(data.frame(listone = unique(day_one_sorted$listone)), by = c("listtwo" = "listone")) |>
  rename(Count = n) |>
  replace_na(list(Count = 0))

# calculate similarity score
part_two_mult <- part_two |>
  mutate(mult = listtwo * Count)

# sum similarity score
ans_p2 = sum(part_two_mult$mult)
