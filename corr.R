corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  options(digits = 4)
  data = data.frame()
  do = data.frame()
  ss = c()
  ni = c()
  cr = c()
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  for (f in list.files("specdata", "*.csv", full.names=TRUE)) {
      #print(f)
    data = read.csv(f, header=TRUE)
    ss = subset(data, !is.na(sulfate) & !is.na(nitrate), select= c("sulfate"))
    ni = subset(data, !is.na(sulfate) & !is.na(nitrate), select= c("nitrate"))
    #print(ss)
    if (nrow(ss) > threshold) {
      #print(paste(f, " = ", nrow(ss), sep=""))
      #cr = as.numeric(append(cr, formatC(round(cor(ss, ni),6))))
      cr = as.numeric(append(cr, cor(ss,ni)))
    }
  }
  ## Return a numeric vector of correlations
  as.numeric(cr)
}