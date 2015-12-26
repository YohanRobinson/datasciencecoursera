# Programming assignment 1
# Yohan Robinson
# 2015-12-20
#
rm(list=ls()) 

# ------- PART 1 -------
# Function for creating character filename from integer value



# Function for pollutant mean of monitors with the id numbers in the vector "id"
pollutantmean <- function(directory=character,pollutant=c("sulfate","nitrate"), id=vector(1:332)){
    makefilename <- function (directory, fn=integer(1:332)){
      idname <- c("0","0","0")
      if (fn>99) idname[1] <- as.integer(fn/100)
      if (fn>9) idname[2] <- as.integer((fn - (as.integer(idname[1])*100))/10)
      idname[3] <- fn - (as.integer(idname[1])*100) - (as.integer(idname[2])*10)
      filename <- paste0(directory,"/",idname[1],idname[2],idname[3],".csv")
      return(filename)
    }
    rd <- data.frame()
    for (i in 1:length(id)){
          readdata <- read.csv(makefilename(directory, id[i]),header=T)
          rd <- rbind(rd,readdata)
    }
    rd <- subset(rd,!is.na(rd[,pollutant]))
    return(mean(as.numeric(rd[,pollutant])))
}

# test
pollutantmean("specdata", "sulfate", 1:10)    #4.064
pollutantmean("specdata","nitrate",70:72)     #1.706
pollutantmean("specdata", "nitrate")      #1.281



# ------- PART 2 -------
#
# Function of number of complete cases
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

complete <- function(directory, id = 1:332) {
  makefilename <- function (directory, fn=integer(1:332)){
    idname <- c("0","0","0")
    if (fn>99) idname[1] <- as.integer(fn/100)
    if (fn>9) idname[2] <- as.integer((fn - (as.integer(idname[1])*100))/10)
    idname[3] <- fn - (as.integer(idname[1])*100) - (as.integer(idname[2])*10)
    filename <- paste0(directory,"/",idname[1],idname[2],idname[3],".csv")
    return(filename)
  }
  cc <- data.frame(length(id),2)
    colnames(cc) <- (c("id", "nobs"))
  #Function loading complete cases of a monitor with ID=idnr 
  nobs <- function(idnr = 1:332) {
    readdata <- read.csv(makefilename(directory, idnr),header=T)
    # remove rows with NA
    readdata <- readdata[-which(is.na(readdata[,2:3])==T), ]
    # return number of rows in complete-cases dataframe
    return(nrow(readdata))
  }
  for (i in 1:length(id)){
    cc[i,1] <- id[i]
    cc[i,2] <- nobs(id[i])
  }
  cc <- as.data.frame(cc)
  return(cc)
}

# test
complete("specdata", 1)                     # 117
complete("specdata", c(2, 4, 8, 10, 12))    # 1041, 474, 192, 148, 96
complete("specdata", 3)                     # 243
complete("specdata", 30:25)                   

# ------- Part 3 -------
#

corr <- function(directory, threshold = integer) {
  makefilename <- function (directory, fn=integer(1:332)){
    idname <- c("0","0","0")
    if (fn>99) idname[1] <- as.integer(fn/100)
    if (fn>9) idname[2] <- as.integer((fn - (as.integer(idname[1])*100))/10)
    idname[3] <- fn - (as.integer(idname[1])*100) - (as.integer(idname[2])*10)
    filename <- paste0(directory,"/",idname[1],idname[2],idname[3],".csv")
    return(filename)
  }
  rd <- matrix()  
  n <- 0
  for (i in 1:332) {
    if (as.list(complete(directory,i)$nobs[1])>=threshold){
      n <- n+1
      readdata <- read.csv(makefilename(directory, i),header=T)
      rd[n] <- cor(readdata$sulfate,readdata$nitrate, use="pairwise.complete.obs")
    }
  }
  data.frame(rd)
  return(rd)
}


#test
cr <- corr("specdata", 150)
head(cr)    ## [1] -0.01896 -0.14051 -0.04390 -0.06816 -0.12351 -0.07589
summary(cr)

source("http://d396qusza40orc.cloudfront.net/rprog%2Fscripts%2Fsubmitscript1.R")
submit()
source("submitscript1.R")
submit()
