rankhospital <- function(state, outcome, num = "best") {
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
    

    all.outcome.data <- read_csv("outcome-of-care-measures.csv", 
                        col_names = TRUE, 
                        col_types = "_c____c___d_____d_____d_______________________", 
                        na = "NA")
    # Now that the CSV file is read, I am going to convert the state column to
    # factor:
    all.outcome.data$State <- as.factor(all.outcome.data$State)

    # check that state and outcome are valid
    # =====================
    # = I M P O R T A N T =
    # =====================
    # This assumes that there are no mistakes in the recording of the names of
    # the states in the data file and that there is at least one hospital from
    # every state in the data file.
    
    # Extract all the states in the State column of the data frame:
    all.states <- unique(all.outcome.data$State)
    # Check if the state argument is a valid one. There are probably better
    # ways to do this, but I am just using a simple if to throw an error
    # message.
    if (!state %in% all.states) {
        stop("Invalid state")
    }

    # Check if the outcome is one of the valid conditions in the list
    conditions <- c("heart attack", "heart failure", "pneumonia")

    # convert the argument to lower case just in case the user has written the
    # name of the condition using upper case letters or a mixture of upper case
    # and lower case.
    if (!tolower(outcome) %in% conditions) {
        stop("Invalid outcome")
    }
    
    # The above two ifs will take care of the invalid conditions and invalid
    # states. If we get to this point, it means we have the correct state and 
    # correct outcome.

    # return hospital name in that state with lowest 30-day death rate
    # Before starting doing anything, I am going to sort the data frame by state
    # first and then by the name of the hospital
    all.outcome.data <- all.outcome.data[order(all.outcome.data$State, 
                                               all.outcome.data$`Hospital Name`), ]
    # The reason I reordered the data frame is that this way if there is a tie, 
    # the first occurence of the min is the answer we are looking for.
    # First, let's find out which one of the conditions we are looking at:
    condition.index <- grep(outcome, conditions)
    # We will need only 2 columns of the data frame: Name of the Hospital
    # and the column corresponding to the condition we are interested in.
    # And we are only interested in  the hospitals in the given state:
    all.outcome.data <- all.outcome.data[all.outcome.data$State == state,
                                         c(1, condition.index + 2)]
    # Up to here, the code is similar to what we had in best.R. Since I want
    # either the best, or worst, or "num" of bests from the top, I can sort the
    # list of hospitals.
    sorted.outcome.data <- all.outcome.data[order(all.outcome.data[[2]], all.outcome.data[[1]]), ]
    sorted.outcome.data <- sorted.outcome.data[complete.cases(sorted.outcome.data), ]
    if (num == "best") {
        output <- head(sorted.outcome.data[[1]], 1)
    } else if (num == "worst") {
        output <- tail(sorted.outcome.data[[1]], 1)
    } else if (is.numeric(num)) {
        if (num > nrow(sorted.outcome.data)) {
            output <- "NA"
        } else {
            output <- sorted.outcome.data[[num, 1]]
        }
    } else {
        stop("invalid argument")
    }
    
    return(output) 
    # Return hospital name in that state with the given rank
    # 30-day death rate
}
