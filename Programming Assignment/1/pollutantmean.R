# Programming assignment 1
# Yohan Robinson
# 2015-12-20
#
rm(list=ls()) 

# ------- PART 1 -------
# Function for creating character filename from integer value



# Function for pollutant mean of monitors with the id numbers in the vector "id"
pollutantmean <- function(directory,pollutant=c("sulfate","nitrate"), id=1:332){
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


library(KernSmooth)
