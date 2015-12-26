# Programming assignment 1
# Yohan Robinson
# 2015-12-20
#

# ------- PART 2 -------

complete <- function(directory, id = 1:332) {
  #read filenames from directory
  filelist <- list.files(path = directory, full.names = TRUE)
  #create empty variables
  readdata <- data.frame()
  completecases <- data.frame()
  nobs <- data.frame();
  #loopsearch complete cases
  for (i in id) { 
    readdata <- read.csv(filelist[i],header=TRUE)
    nobs <- sum(complete.cases(readdata))
    #create new row with id and nobs
    completecases <- rbind(completecases, data.frame(i,nobs))
  }
  return(completecases)
}

