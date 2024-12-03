library(httr)

# Get Data
session_cookie <- "53616c7465645f5f65b141f6ee517e862066edbc51531079e69ef768c0b3bc67ab19f5e30b24c21cb8c4959891c909dd0ece883fffed61c82ecb29f176eda104"

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

# Get input for Day 3 of 2024
day3_input <- get_puzzle_input(3)

#--------#
# Part 1 #
#--------#

# RegEx function
extract <- function(text) {
  # Define the regex pattern to match mul(number,number)
  pattern <- "mul\\((-?\\d+(\\.\\d+)?),(-?\\d+(\\.\\d+)?)\\)"
  
  # Use gregexpr to find all matches in the text
  matches <- gregexpr(pattern, text, perl = TRUE)
  
  # Extract the matches using regmatches
  result <- regmatches(text, matches)
  
  # Flatten the list of matches
  matches <- unlist(result)
  
  # Extract and calculate the product of the numbers in each match
  sum_result <- sum(sapply(matches, function(match) {
    # Extract the numbers using another regex
    numbers <- as.numeric(unlist(regmatches(match, gregexpr("-?\\d+(\\.\\d+)?", match))))
    # Multiply the two numbers
    prod(numbers)
  }))
  
  # Return the sum of all products
  return(sum_result)
}

part_one <- extract(day3_input)

#----------#
# Part Two #
#----------#

process_instructions <- function(text) {
  # Define regex patterns
  mul_pattern <- "mul\\((-?\\d+(\\.\\d+)?),(-?\\d+(\\.\\d+)?)\\)"
  control_pattern <- "do\\(\\)|don't\\(\\)"
  all_pattern <- paste(mul_pattern, control_pattern, sep = "|")
  
  # Extract all instructions (mul, do, and don't) in order
  instructions <- unlist(regmatches(text, gregexpr(all_pattern, text, perl = TRUE)))
  
  # Initialize state and sum
  enabled <- TRUE  # Mul instructions are enabled by default
  total_sum <- 0   # Sum of valid mul products
  
  # Process each instruction
  for (instr in instructions) {
    if (grepl("do\\(\\)", instr)) {
      enabled <- TRUE  # Enable future mul instructions
    } else if (grepl("don't\\(\\)", instr)) {
      enabled <- FALSE  # Disable future mul instructions
    } else if (grepl(mul_pattern, instr) && enabled) {
      # Extract numbers from mul(...)
      numbers <- as.numeric(unlist(regmatches(instr, gregexpr("-?\\d+(\\.\\d+)?", instr))))
      # Calculate product and add to the total sum
      total_sum <- total_sum + prod(numbers)
    }
  }
  
  # Return the final sum
  return(total_sum)
}


result <- process_instructions(day3_input)
