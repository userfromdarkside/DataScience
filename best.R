best <- function(state, outcome) {
  # Read outcome data
  outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  # Valid outcomes and corresponding column indices
  valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
  outcome_columns <- c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
                       "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
                       "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
  names(outcome_columns) <- valid_outcomes
  
  # Check if state and outcome are valid
  if (!(state %in% outcome_data$State)) {
    stop("invalid state")
  }
  
  if (!(outcome %in% valid_outcomes)) {
    stop("invalid outcome")
  }
  
  # Get the column name for the specified outcome
  outcome_col <- outcome_columns[outcome]
  
  # Filter data for the given state
  state_data <- subset(outcome_data, State == state)
  
  # Convert outcome column to numeric (suppress warnings for NAs)
  state_data[[outcome_col]] <- suppressWarnings(as.numeric(state_data[[outcome_col]]))
  
  # Remove rows with NA in the outcome column
  state_data <- state_data[!is.na(state_data[[outcome_col]]), ]
  
  # Sort by outcome column and then by Hospital.Name
  sorted_data <- state_data[order(state_data[[outcome_col]], state_data$Hospital.Name), ]
  
  # Return the hospital name with the lowest 30-day mortality rate
  best_hospital <- sorted_data$Hospital.Name[1]
  
  return(best_hospital)
}

# Sample function calls
print(best("TX", "heart attack"))
print(best("TX", "heart failure"))
print(best("MD", "heart attack"))
print(best("MD", "pneumonia"))

# Error handling
print(best("BB", "heart attack"))  # Expected: Error "invalid state"
print(best("NY", "hert attack"))   # Expected: Error "invalid outcome"

