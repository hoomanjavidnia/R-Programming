pollutantmean <- function(directory, pollutant, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)
    ## NOTE: Do not round the result!
    
    ## End of original comments
    
    # ============================================
    # = John Hopkins Data Science Specialization =
    # = R Programming                            =
    # = Week 2                                   =
    # = Programming Assignment 1                 =
    # = August 2015                              =
    # = Hooman Javidnia                          =
    # ============================================ 
  
    # For this exercise, I have several things that I should consider: 
    # 1. How to get the names of all files in the destination folder
    # 2. How to make sure that the name of the pollutants are properly passed
    # to the function and raise error messages
  
    # Issue 1: One solution that I have found is using 
    # `list.files()' function. It will generate a list with the names of all
    # files in a given directory that match a given pattern like "*.csv"
    # If we pass the id of a station to this list, 
    # the names of the associated csv files will be returned.
    
    # Issue 2: In order to convert a text string to lower case, the tolower(x)
    # function can be used.
  
    # In terms of general idea behind the program, there are two methodologies:
    # * As we iterate through requested files we put everything in a dataframe
    #   and then take out the NAs and calculate the mean value using available
    #   functions.
    # * The second method is more barebones and we just read the csv files into
    #   vectors and take out the NAs and add them up and calculate the mean.
    # I will try both methods and see how each can be implemented.
    # --------------------------------------------------------------------------
  
  # Here the assumption is that the user will enter the path of the parent
  # directory of where the data is located and that the directory that contains
  # the CSV files is named specdata.
  # A better way of doing this would be to look for the directory within working
  # directory that contains the data and set it appropriately.
  # setwd(directory)
  # Probably there is no need to return the files full name, but I will return
  # them anyway.
  # Is there a directory called specdata in directory?
#   subdir.in.directory <- list.dirs(path = directory, full.names = FALSE)
#   if (grep("specdata", subdir.in.directory) == "specdata") {
#     setwd(paste(directory,"/specdata", sep = ""))
#   } else {
#     stop("Data directory does not exist here.")
#   }
  files.in.directory <- list.files(path = directory, pattern = "*.csv",
                                   full.names = TRUE)
  # I am going to use "readr" package to read the csv files.
  library("readr")
  # The idea is to loop over the id numbers and read the data in each csv file 
  # into a data frame. Then depending on which pollutant is selected, the
  # corresponding column from the data frame is saved in a vector, exclusing the
  # NAs. Once we are done with the loop, we should have a big (or not too big)
  # vector with all the reading of the specified pollutant in the vector.
  
  # First we have to initialize an empty vector:
  all.pollutants <- numeric(length = 0L)
  # Make sure commen misspelling of the pollutant is handled properly:
  pollutant <- tolower(pollutant)
  for (i in id) {
    # current.file is a data frame which contains the contents of the i-th file
    # in the id vector.
    # The column types are specified so that the read_csv's reliance on first
    # 100 rows doesn't throw off the detection of column types.
    current.file <- read_csv(files.in.directory[i], col_names = TRUE,
                             col_types = list(col_date(), col_double(),
                                         col_double(), col_integer()))
    # Now that the file has been read we want to extract all !NA values from the
    # data frame and put it in the vector all.pollutant.
    # In the following line, the NAs are taken care of and then put into the 
    # all.pollutant vector.
    # temp <- current.file[complete.cases(current.file[[pollutant]]), pollutant]
    # Another options is just to take the column of the specified pollutant with
    # all the NAs in it and take care of the NAs when calculating the mean.
    temp <- current.file[, pollutant]
    all.pollutants <- c(all.pollutants, temp)
  }
  mean(all.pollutants, na.rm = TRUE)
}