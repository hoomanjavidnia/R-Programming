best <- function(state, outcome) {
    # ============================================
    # = John Hopkins Data Science Specialization =
    # = R Programming                            =
    # = Week 4                                   =
    # = Programming Assignment 3                 =
    # = August 2015                              =
    # = Hooman Javidnia                          =
    # ============================================ 

  	# Read outcome data
  	# I am using readr package for reading the csv files
  	library(readr)
    # I don't need to read the whole file as the decision on best hospital is
    # only based on the mortality rates for "heart attack", "heart failure", and
    # "pneumonia". We also need the name of the hospital and the state. These
    # are located in the following columns of the CSV file:
    # Hospital Name: column 2 (character string)
    # State: column 7 (factor)
    # Mortality rate from heart attack: column 11 (double)
    # Mortality rate from heart failure: column 17 (double)
    # Mortality rate from pneumonia: column 23 (double)
    
    # There is a variable in R which has all the abbreviations of states. I will
    # use this variable for checking if the name of the variable passed is valid
    # or not. Also, it will be passed to read_csv file to read the states as a 
    # factor.
  
    # Check  if the state argument is a valid one. There are probably better
    # ways to do this, but I am just using a simple if.
    
    if (!state %in% state.abb) {
      stop("Invalid state")
    }
    
    # Check if the outcome is one of the valid conditions in the list
    conditions <- c("heart attack", "heart failure", "pneumonia")
    # convert the argument to lower case just in case the user has written the
    # name of the condition using upper case letters or a mixture of upper case
    # and lower case.
    if (!tolower(outcome) %in% conditions)
    
    
 #   outcome <- read_csv(, col_names = TRUE,
 #                            col_types = list(col_date(), col_double(),
#                                              col_double(), col_integer()))
    
    # check that state and outcome are valid
	
    # return hospital name in that state with lowest 30-day death rate
}