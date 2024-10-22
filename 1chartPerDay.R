#importing file
library(readxl)
library(janitor)
library(ggplot2)
library(tidyr)

file <- read_excel("/Users/samzimmer/Desktop/pigs/Data/file2.xlsx")
#ORIENTING DATA IN FRAME
firstLine <- which(file == "Date")
myData <- file[-(0:firstLine+1),]
#naming columns by data names, rather than original headers
colnames(myData) <- file[7,]

#CONVERTING UNITS TO MAKE SENSE
#date and time columns
myData$Date <- as.integer(myData$Date)
myData$Time <- as.numeric(myData$Time)
myData$Time <- convert_to_datetime(myData$Date + myData$Time)
myData$Date <- myData$Time
colnames(myData)[colnames(myData) == "Time"] <- "RelTime"
colnames(myData)[colnames(myData) == "Standing [t]"] <- "Standing [%]"
colnames(myData)[colnames(myData) == "Lying [t]"] <- "Lying [%]"
#standing and lying columns
myData$`Standing [%]` <- as.numeric(myData$`Standing [%]`)
myData$`Lying [%]` <- as.numeric(myData$`Lying [%]`)
#(multiplied by number to convert to minutes, then divided by 60 to get percent)
myData$`Standing [%]` <- myData$`Standing [%]` * 1440
myData$`Lying [%]` <- myData$`Lying [%]` * 1440


#SETTING UP START TIME
#TO CHOOSE, EDIT startTime
startTime = "09-29 00:00"
#DELETING DATA BEFORE START TIME
firstLine <- grep(startTime ,myData$RelTime) - 1
myData <- myData[-(0:firstLine),]
#SETTING TIME ACCORDING TO START TIME
myData$RelTime <- 0:(nrow(myData)-1)

#STACKED BAR CHART SHOWING STANDING AND LYING TIME
breaks <- 24
standingLying <- myData[1441:2880,c(1,2,4:5)]
chartTitle <- substr(standingLying$Date[1],1,10)
standingLying$RelTime <- cut(standingLying$RelTime, breaks, include.lowest = TRUE, labels= 1:breaks)
standingLying2 <- standingLying %>% gather("state", "Minutes", 3:4)
ggplot(standingLying2, aes(x = RelTime, y = Minutes, fill = state)) + geom_col() + labs(title=chartTitle, x = "Time of Day (hr)", y = "Minutes")
