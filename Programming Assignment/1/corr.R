# Programming assignment 1
# Yohan Robinson
# 2015-12-20
#

# ------- Part 3 -------
#

corr <- function(directory, threshold = integer(1000000)) {
  #read filenames from directory
  filelist <- list.files(path = directory, full.names = TRUE)
  #create empty variables
  readdata <- data.frame()
  rd <- matrix()  
  n <- 0
  for (i in 1:332) {
    if (as.data.frame(complete(directory,i)$nobs[1])>=threshold){
      n <- n+1
      readdata <- read.csv(filelist[i], header=TRUE)
      rd[n] <- cor(readdata$sulfate,readdata$nitrate, use="pairwise.complete.obs")
    }
  }
  data.frame(rd)
  return(rd)
}

