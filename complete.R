complete <- function(directory, id = 1:332) {
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
  
  data = data.frame()
  do = data.frame()
  ss = data.frame()
  
  for ( i in id) {
    
    if (i < 10) 
      fn = paste("specdata/00", i, ".csv", sep="")
    else if (i < 100)
      fn = paste("specdata/0", i, ".csv", sep="")
    else if (i < 1000)
      fn = paste("specdata/", i, ".csv", sep="")
    #print(fn)
    data = read.csv(fn, header=TRUE)
    ss = subset(data, !is.na(sulfate) & !is.na(nitrate), select= c("ID"))
    do = rbind(do, c(i, nrow(ss)))
    #do = rbind(do, c(i, nrow(na.omit(data))))
  }
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  colnames(do) <- c("id","nobs")
  do
}