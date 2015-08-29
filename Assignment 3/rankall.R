rankall <- function(outcome, num = "best") {
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
    

    data <- read_csv("outcome-of-care-measures.csv", 
                        col_names = TRUE, 
                        col_types = "_c____c___d_____d_____d_______________________", 
                        na = "NA")
    # Now that the CSV file is read, I am going to convert the state column to
    # factor:
    data$State <- as.factor(data$State)

    # check that state and outcome are valid
    # =====================
    # = I M P O R T A N T =
    # =====================
    # This assumes that there are no mistakes in the recording of the names of
    # the states in the data file and that there is at least one hospital from
    # every state in the data file.
    
    # # Extract all the states in the State column of the data frame:
#     all.states <- unique(data$State)
#     # Check if the state argument is a valid one. There are probably better
#     # ways to do this, but I am just using a simple if to throw an error
#     # message.
#     if (!state %in% all.states) {
#         stop("Invalid state")
#     }

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
    
    # I am trying to avoid using for loops and I think that is the intension of
    # the course instructors. One solution I found is using split() function to
    # split the data frame based on the state. This will return a list of data
    # frames for each state. Then I will apply a function similar to 
    # rankhospital.R to the elements of the list.
    
    # Subset the columns of the data frame relavent to the given condition:
    # First, let's find out which one of the conditions we are looking at:
    condition.index <- grep(outcome, conditions)
    # We will need 3 columns of the data frame: Name of the Hospital, State,
    # and the column corresponding to the outcome we are investigating.
    data <- data[, c(1,2, condition.index + 2)]
    # Splittng the "data" data frame based on the State column which is a factor
    split.data <- split(data, data[[2]], drop = FALSE)
    # Now, I am going to define a function which will do the main part of the
    # calculation. The input to this function is a data frame (df) and num,
    # which can take the values of "best", "worst", or a numeric value.
    find.outcome <- function(df, num) {
        # Function find.outcome finds the desired outcome in a given data frame.
        # First the data frame df has to be sorted based on its 3rd column
        # which is the mortality rate for a given condition, and then 1st column
        # which is the name of the hospital. Note that at this point the NAs
        # are still part of each data frame.
        df <- df[order(df[[3]], df[[1]]), ]
        # Remove all NA's.
        df <- df[complete.cases(df), ]
        # Now that df is sorted and there are no NAs, I can extract the desired
        # outcome based on the value of the num.
        if (num == "best") {
            output <- head(df[[1]], 1)
        } else if (num == "worst") {
            output <- tail(df[[1]], 1)
        } else if (is.numeric(num)) {
            if (num > nrow(df)) {
                output <- "NA"
            } else {
                output <- df[[num, 1]]
            }
        } else {
            stop("invalid argument")
        }
        return(output)
    }
    # Now, split.data is a list, where each element is data frame.
    # I am applying find.outcome function on every element of the list.
    # It should return a list with the name of the Hospital for the given which 
    # has best outcome for the condition specfified.
    results <- lapply(split.data, find.outcome, num)
    # Return a data frame with the hospital names and the
    # (abbreviated) state name
    return(data.frame(hospital = unlist(results), state = names(results)))
}
