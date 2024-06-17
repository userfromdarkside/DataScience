rankall <- function(outcome, num = "best") {
  # Read outcome data
  outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  # Define the mapping for outcome to column name
  valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
  outcome_columns <- c("heart attack" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
                       "heart failure" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
                       "pneumonia" = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
  
  # Validate outcome
  if (!(outcome %in% valid_outcomes)) {
    stop("invalid outcome")
  }
  
  # Select the correct column based on the outcome
  outcome_column <- outcome_columns[[outcome]]
  
  # Initialize a list to store results for each state
  results <- list()
  
  # Get unique states
  states <- unique(outcome_data$State)
  
  # Process each state
  for (state in states) {
    # Filter data for the given state
    state_data <- subset(outcome_data, State == state)
    
    # Convert the outcome column to numeric
    state_data[[outcome_column]] <- suppressWarnings(as.numeric(state_data[[outcome_column]]))
    
    # Remove rows with NA values in the outcome column
    state_data <- state_data[!is.na(state_data[[outcome_column]]), ]
    
    # Sort the data by the outcome column and then by hospital name
    sorted_data <- state_data[order(state_data[[outcome_column]], state_data$Hospital.Name), ]
    
    # Determine the ranking to return
    if (num == "best") {
      rank_num <- 1
    } else if (num == "worst") {
      rank_num <- nrow(sorted_data)
    } else if (is.numeric(num)) {
      rank_num <- as.numeric(num)
    } else {
      rank_num <- NA
    }
    
    # Handle out of range ranks
    if (is.na(rank_num) || rank_num > nrow(sorted_data) || rank_num <= 0) {
      hospital_name <- NA
    } else {
      hospital_name <- sorted_data$Hospital.Name[rank_num]
    }
    
    # Add the result to the list
    results[[state]] <- data.frame(hospital = hospital_name, state = state)
  }
  
  # Combine results into a single data frame
  result_df <- do.call(rbind, results)
  
  # Sort by state
  result_df <- result_df[order(result_df$state), ]
  
  return(result_df)
}

# Sample function calls
print(rankall("heart attack", 20))
print(rankall("pneumonia", "worst"))
print(rankall("heart failure", 10))
