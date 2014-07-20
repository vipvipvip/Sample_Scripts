pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  data = data.frame()
  for ( i in id) {
    
    if (i < 10) 
      fn = paste("specdata/00", i, ".csv", sep="")
    else if (i < 100)
      fn = paste("specdata/0", i, ".csv", sep="")
    else if (i < 1000)
      fn = paste("specdata/", i, ".csv", sep="")
    ##print(fn)
    data = rbind(data,read.csv(fn, header=TRUE))
    ##print(data)
  }
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  formatC(round(mean(data[,pollutant], trim=0, na.rm=TRUE),3))
  
}
