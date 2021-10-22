## Item 1

pollutantMean <- function(directory, pollutant, id = 1:332){
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of  the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate"
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  specdata_list <- list.files(path = directory, pattern = ".csv")
  ## creates a vector of .csv files from the directory
  x <- numeric()
  ##empty numeric vector
  
  for (i in id){
    ##for loop to access and run all the monitor IDs
    
    data <- read.csv(specdata_list[i])
    ##to read/access every .csv files run from specdata_list
    
    x <- c(x, data[[pollutant]])
    ##updates the vector x with the data from pollutant
  }
  mean(x,na.rm = TRUE)
  ##calculates the mean of the data from x while ignoring missing values NA
}

pollutantMean("C:/Users/User/Desktop/Intro-to-Github/specdata", "sulfate", 1:332)
#prints the mean of the "sulfate" pollutant
pollutantMean("C:/Users/User/Desktop/Intro-to-Github/specdata", "nitrate", 1:332)
#prints the mean of the "nitrate" pollutant


########

## Item 2
complete <- function(directory, pollutant, id = 1:332){
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of  the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate"
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  specdata_list <- list.files(path = directory, pattern = ".csv")
  ## creates a vector of .csv files from the directory
  no_observations <- numeric()
  ##empty numeric vector to store the data for no of observations
  
  for(i in id){
  ##for loop to access and run all the monitor IDs
    data <- read.csv(specdata_list[i])
    ##to read/access every .csv files run from specdata_list
    
    data_sum <- sum(complete.cases(data))
    ## computes the sum of the no. of observations disregarding missing values
    no_observations <- c(no_observations, data_sum)
    # combines the arguments from the summed no. of observations
  }
  
  data.frame(id, no_observations)
  ## return a data frame where the first column is the monitor ID of the file
  ## and the second column is the no. of complete cases
  
}

complete("C:/Users/User/Desktop/Intro-to-Github/specdata", 1:332)
## prints out the data frame

########

## Item 3

corr <- function(directory, threshold = 0){
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1, initialized to 0
  ##indicating the no. of completely observed observations
  ## required to compute the correlation between nitrate and sulfate 
  
  specdata_list <- list.files(path = directory, pattern = ".csv")
  ## creates a vector of .csv files from the directory
  d_frame <- complete(directory)
  ## turns implicit missing values into explicit missing values 
  ## and returns a data frame
  monitor_id <- d_frame[d_frame["no_observations"] > threshold, ]$id
  ## selects variables greater than the threshold limit  
  ## it also selects the rows representing the monitor ID
  vector_corr <- numeric()
  ## creates an empty numeric vector to store the correlation
  
  for(i in monitor_id){
    data <- read.csv(specdata_list[i])
    ## reads the .csv files accessed by specdata_list
    compl_cases <- data[complete.cases(data), ]
    ## lists the data sets without the missing values
    ## and storing it in compl_cases
    vector_corr <- c(vector_corr, cor(compl_cases$nitrate, compl_cases$sulfate))
    ## calculates the correlation of nitrate and sulfate (disregarding NA values)
    ## from the files and storing the result in the vector
  }
  return(vector_corr)
  ## returns a numeric vector of correlations from the monitor
  ## that meet the threshold requirement
}

corr("C:/Users/User/Desktop/Intro-to-Github/specdata", 1:332)
## prints our the correlation of nitrate and sulfate


########

## Item 4

outcome <- read.csv('outcome-of-care-measures.csv', colClasses = "character")
## reads the csv file containing the data
## and specifying that each variable is a character class
head(outcome)
## returns the data from the data frame 
outcome[, 11] <- as.numeric(outcome[, 11])
## coerces objects of type numeric from the data column 11 of outcome dataset
head(outcome)
hist(
    outcome[, 11], 
     main = " 30-day Mortality Rates from Heart Attack", 
     col = "darkgoldenrod1")
## generates a histogram plotting the 30-day mortality rates for heart attack 
## with the title and color modified

