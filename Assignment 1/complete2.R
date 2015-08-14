complete2 <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  ## End of original comments
  
  # ============================================
  # = John Hopkins Data Science Specialization =
  # = R Programming                            =
  # = Week 2                                   =
  # = Programming Assignment 1                 =
  # = August 2015                              =
  # = Hooman Javidnia                          =
  # ============================================ 
  
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
  # Here we calculate the number of csv files in the data directory
  number.of.files <- length(files.in.directory)
  # I am going to use "readr" package to read the csv files.
  library("readr")
  # The idea is to loop over the id numbers and read the data in each csv file 
  # into a data frame. Then depending on which pollutant is selected, the
  # corresponding column from the data frame is saved in a vector, exclusing the
  # NAs. Once we are done with the loop, we should have a big (or not too big)
  # vector with all the reading of the specified pollutant in the vector.
  
  # First we have to initialize an empty data frame:
  # Here I have initialized the data.frame to have as many rows as the number 
  # of files in the data directory. All will be filled with NAs.
  # df <- data.frame(matrix(nrow = number.of.files, ncol = 2))
  # Assign names to the columns of the data frame df
  # names(df) <- c("id", "nobs")
  # df <- data.frame(id = integer(), nobs = integer(), stringsAsFactors = FALSE)
  id.vector = rep(NA, number.of.files)
  nobs.vector = rep(NA, number.of.files)
  j = 1
  for (i in id) {
    # current.file is a data frame which contains the contents of the i-th file
    # in the id vector.
    # The column types are specified so that the read_csv's reliance on first
    # 100 rows doesn't throw off the detection of column types.
    current.file <- read_csv(files.in.directory[i], col_names = TRUE,
                             col_types = list(col_date(), col_double(),
                                              col_double(), col_integer()))
    # Now that we have the content of i-th file, we make a complete case of 
    # the current data frame and count number of complete cases.
    current.file <- current.file[complete.cases(current.file), ]
    number.of.complete.case <- nrow(current.file)
    id.vector[j] = i
    nobs.vector[j] = number.of.complete.case
    j <- j+1
    # df <- rbind(df, data.frame(id = i, nobs = number.of.complete.case))
    # df[i, ] <- c(i, number.of.complete.case)
  }
  id.vector <- id.vector[!is.na(id.vector)]
  nobs.vector <- nobs.vector[!is.na(nobs.vector)]
  df <- data.frame(id = id.vector, nobs = nobs.vector)
  # df <- df[complete.cases(df), ]
  # The data frame at this point will have row lable that are coming from the 
  # original data frame, i.e., if some rows are dropped because they weren't 
  # complete cases (i.e., they had NAs), the cleaned up data frame will still
  # use labels from the original. The following command sets the lables to a 
  # sequence from 1 to number of rows in the data frame.
  # row.names(df) <- NULL
  return(df)
}