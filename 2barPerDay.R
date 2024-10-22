#importing file
library(readxl)
library(janitor)
library(ggplot2)
library(tidyr)
library(dplyr)

file <- read_excel("/Users/samzimmer/Desktop/pigs/Data/file3.xlsx")
info <- read.csv("/Users/samzimmer/Desktop/pigs/morgansowdatarev.csv")
#ORIENTING DATA IN FRAME
firstLine <- which(file == "Date")
myData <- file[-(0:firstLine+1),]
#naming columns by data names, rather than original headers
colnames(myData) <- file[7,]

#CONVERTING UNITS TO MAKE SENSE
#date and time columns
myData$Date <- as.integer(myData$Date)
myData$Time <- as.numeric(myData$Time)
myData$`Motion Index` <- as.numeric(myData$`Motion Index`)
myData$Date <- convert_to_datetime(myData$Date + myData$Time)
myData <- myData[c(1,3:7)]
colnames(myData)[colnames(myData) == "Standing [t]"] <- "Standing [%]"
colnames(myData)[colnames(myData) == "Lying [t]"] <- "Lying [%]"
colnames(myData)[colnames(myData) == "Motion Index"] <- "MotionIndex"

#REMOVING FIRST PARTIAL DAY
startTime <- as.Date(substr(myData$Date[1],1,10)) + 1
firstLine <- which(myData$Date == startTime)
  
#REMOVING LAST PARTIAL DAY
endTime <- as.Date(substr(myData$Date[nrow(myData)],1,10))
lastLine <- which(myData$Date == endTime) - 1

#SELECTING WORKING DATA
workingData <- myData[firstLine:lastLine,1:2]

#PULL PIG'S INFO
#finding icetag
iceTag <- substr(colnames(file)[3],6,8)
pigInfo <- which(info$`IceTag_.` == iceTag)

chartTitle <- paste("iceTag:", iceTag, "\nID:",info$ID[pigInfo],
                    "\nCrate:", info$Crate[pigInfo], 
                    "\nGroup:", info$Group[pigInfo], 
                    "\nRoom:", info$Room[pigInfo], 
                    "\nTreatment:",info$Tx[pigInfo],
                    "\nDates:",startTime, "-", endTime-1)

#STACKED BAR CHART SHOWING STANDING AND LYING TIME
breaks <- as.numeric(difftime(endTime, startTime, units = "days"))
workingData$Date <- cut(workingData$Date, breaks, include.lowest = TRUE, labels= 1:breaks)
workingData%>%group_by(Date)%>%summarise_all(funs(sum))

ggplot(workingData) + geom_col(aes(x = Date, y = MotionIndex)) + 
  geom_vline(xintercept=2.5,lwd=1,colour="red") +
  geom_text(aes(x=2.5, label="\nCrate Opens", y=5000), colour="red", angle=90) +
  labs(title = chartTitle, x = "Day", y = "Cumulative Motion Index")

#TO DO:
#DELETE ONE FROM END DATE
#CHANGE LABELS TO DATE
#LABEL CRATE OPENING AND FARROW DATE