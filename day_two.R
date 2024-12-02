library(tidyverse)
library(httr)

# Function to get puzzle input
get_puzzle_input <- function(day, year = 2024) {
  url <- paste0("https://adventofcode.com/", year, "/day/", day, "/input")
  response <- GET(
    url,
    add_headers(Cookie = paste0("session=", session_cookie))
  )
  
  if (response$status_code == 200) {
    return(content(response, "text"))
  } else {
    stop("Failed to retrieve input. Check your session cookie and day.")
  }
}

# Get input for Day 2 of 2024
day2_input <- get_puzzle_input(2)

day_two_unlist <- unlist(strsplit(day2_input, "\n"))

day_two_raw <- data.frame(day_two_unlist)

# Split the column into individual elements by space
split_values <- strsplit(as.character(day_two_raw$day_two_unlist), " ")

# Find the maximum number of columns needed
max_cols <- max(sapply(split_values, length))

# Add NA for rows with fewer values and convert to a dataframe
split_df <- do.call(rbind, lapply(split_values, function(x) {
  length(x) <- max_cols
  x
}))

# Convert to a dataframe with proper column names
split_df <- data.frame(split_df, stringsAsFactors = FALSE)
colnames(split_df) <- paste0("Column", seq_len(ncol(split_df)))

# Convert columns to numeric (if needed)
split_df <- data.frame(lapply(split_df, as.numeric))

day_two_input_final <- split_df

#----------#
# Part One #
#----------#

library(purrr)

# Function to check if a row is safe
is_safe <- function(row) {
  # Remove NA values from the row
  row_clean <- na.omit(row)
  
  # Check if there are enough values (at least 2 for difference calculation)
  if(length(row_clean) < 2) {
    return(FALSE)
  }
  
  # Compute differences between consecutive elements
  inc <- diff(row_clean)
  
  # Check if all differences are within the valid set of values {1, 2, 3} or {-1, -2, -3}
  if (all(inc %in% c(1, 2, 3)) || all(inc %in% c(-1, -2, -3))) {
    return(TRUE)
  }
  return(FALSE)
}


# Step 1: Convert each row to a numeric vector and apply is_safe
safe_count_1 <- sum(apply(day_two_input_final, 1, function(row) is_safe(row)))

# Output the result
print(paste("Number of safe reports:", safe_count_1))

#----------#
# Part Two #
#----------#

# Step 2: Second count (checking removing one element from each row)
safe_count_2 <- sum(apply(day_two_input_final, 1, function(row) {
  any(sapply(1:length(row), function(i) is_safe(row[-i])))
}))

# Output the second result
print(paste("Number of safe reports after removing one element:", safe_count_2))
