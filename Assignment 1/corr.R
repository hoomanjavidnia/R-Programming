corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  ## NOTE: Do not round the result!
  
  # ============================================
  # = John Hopkins Data Science Specialization =
  # = R Programming                            =
  # = Week 2                                   =
  # = Programming Assignment 1                 =
  # = August 2015                              =
  # = Hooman Javidnia                          =
  # ============================================ 
  
  # First I have to find which stations meet the threshold criteria of a given
  # number of complete cases. For this I can use the complete() function that
  # was developed previously.
  all.complete.cases <- complete(directory)
  
  # In the following line all rows which have a second column "nobs" greater
  # greater than threshold are selected, then the first column which shows the
  # id of the files are returned in a vector.
  matching.criteria <- all.complete.cases[all.complete.cases[, 2] > threshold, 1]
  
  # Similar to pollutantmean() function the list of all filed in the directory
  # is put into a vector:
  files.in.directory <- list.files(path = directory, pattern = "*.csv",
                                   full.names = TRUE)
  
  # All we have to do now is to loop over the file ids contained in 
  # matching.criteria vector and read the contents of those CSV files and 
  # and calculate the correlation.
  
  # We have to initialize a vector that the correlations between nitrate and 
  # sulfate for each file will be saved in this file.
  correlations <- numeric(length = 0L)
  
  # define a counter to step into the correlation vector
  # j <- 1
  # Load readr library
  library(readr)
  for (i in matching.criteria) {
    current.file <- read_csv(files.in.directory[i], col_names = TRUE,
                             col_types = list(col_date(), col_double(),
                                              col_double(), col_integer()))
    # Now we remove all NAs and extract all observations where both pollutants
    # were measured. Only columns 2 and 3 are kept as we don't need the other
    # columns
    current.file <- current.file[complete.cases(current.file), 2:3]
    correlations <- c(correlations, cor(current.file[1], current.file[2]))
    # correlations[j] <- cor(current.file[1], current.file[2])
    # j <- j + 1
  }
  return(correlations)
}