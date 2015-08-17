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
    outcome <- read_csv(, col_names = TRUE,
                             col_types = list(col_date(), col_double(),
                                              col_double(), col_integer()))
	# check that state and outcome are valid
	
	# return hospital name in that state with lowest 30-day death rate
      
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
      df <- rbind(df, data.frame(id = i, nobs = number.of.complete.case))	
}