rankhospital <- function(state, outcome, num = "best") {
  # Read outcome data
  outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  # Define the mapping for outcome to column name
  valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
  outcome_columns <- c("heart attack" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
                       "heart failure" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
                       "pneumonia" = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
  
  # Validate state
  if (!(state %in% outcome_data$State)) {
    stop("invalid state")
  }
  
  # Validate outcome
  if (!(outcome %in% valid_outcomes)) {
    stop("invalid outcome")
  }
  
  # Select the correct column based on the outcome
  outcome_column <- outcome_columns[outcome]
  
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
    num <- 1
  } else if (num == "worst") {
    num <- nrow(sorted_data)
  } else if (!is.numeric(num) || num <= 0 || num > nrow(sorted_data)) {
    return(NA)
  }
  
  # Return the hospital name with the given rank
  ranked_hospital <- sorted_data$Hospital.Name[num]
  
  return(ranked_hospital)
}

# Sample function calls
print(rankhospital("TX", "heart failure", 4))  # Expected: "DETAR HOSPITAL NAVARRO"
print(rankhospital("MD", "heart attack", "worst"))  # Expected: Hospital with the worst rate in MD for heart attack
print(rankhospital("MN", "heart attack", 5000))  # Expected: NA (rank exceeds number of hospitals)

