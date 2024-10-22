#TO USE CODE:
# Run four functions: loadPackages,makeGraph,loadFiles,printFiles
# Run functions at bottom of screen: loadFiles, printFiles

loadPackages <- function() {
  library(readxl)
  library(janitor)
  library(ggplot2)
  library(tidyr)
  library(dplyr)
  library(ggrepel)
}
makeGraph <- function(theFile) {
  dataStart <- which(theFile[1] == "Date")
  print(dim(theFile))
  myData <- theFile[-(0:dataStart+1),]
  #naming columns by data names, rather than original headers
  colnames(myData) <- theFile[dataStart,]
  
  #CONVERTING UNITS TO MAKE SENSE
  #date and time columns
  myData$Time <- as.numeric(myData$Time)
  myData$Steps <- as.numeric(myData$Steps)
  myData <- myData[c(2:7)]

  #ADJUSTING FOR START TIME OF 10AM
  workingData <- myData[,c(1,5)]
  difference <- (myData$Time[1]-(10/24))*1440
  if (difference<0) {
    firstLine <- which(workingData$Time==(10/24))[1]
    workingData <- workingData[firstLine:(1440*6+firstLine-1),]
  } else if (difference>0) {
    fillDayTime <- c(600:(600+difference-1)/1440)
    fillDaySteps <-c(rep(0,times=length(fillDayTime)))
    fillDay <<- data.frame(Time=fillDayTime,Steps=fillDaySteps)
    workingData <- rbind(fillDay,workingData)
  } else {
    workingData <- myData[1:(1440*6),1:2]
  }
  workingData <- workingData[1:(1440*6),]
  row.names(workingData) <- 1:nrow(workingData)
  workingData$Time <- as.numeric(workingData$Time)
  indexOfHours <- which((workingData$Time*1440)%%60==0)[c(1,7,13,19,25)]
  hours <- c("10:00", "16:00","22:00","04:00","10:00")
  workingData$Time <- convert_to_datetime(workingData$Time)
  workingData$Minutes <- c(rep(1:1440,times=6))
  workingData$Day <- rep(2:7,each=1440)
  
  #creating day 2
  #order data and apply cumsum
  workingData$Steps <- ave(workingData$Steps,workingData$Day, FUN=cumsum)
  #add group row
  workingData$Group <- c(rep("2-3",2880),rep("4-5",2880),rep("6-7",2880))
  #add label row
  workingData$Label <- rep(NA, times=1440*6)
  workingData$Label[c(1300+1440*(0:5))] <- paste("Day",2:7)
  
  
  #PULL PIG'S INFO
  pigStartDate <- convert_to_datetime(as.integer(theFile[3,3]))
  pigIceTag <- substr(colnames(theFile)[3],6,8)
  info$Start_Date <- as.Date(info$Start_Date,"%m/%d/%Y")
  info$End_Date <- as.Date(info$End_Date,"%m/%d/%Y")
  pigInfo <- which(info$Start_Date == pigStartDate & info$`IceTag_.` == pigIceTag)
  
  
  chartTitle <- paste("iceTag:", info$`IceTag_.`[pigInfo], "\nID:",info$ID[pigInfo],
                      "\nCrate:", info$Crate[pigInfo], 
                      "\nGroup:", info$Group[pigInfo], 
                      "\nRoom:", info$Room[pigInfo], 
                      "\nTreatment:",info$Tx[pigInfo],
                      "\nDates:",convert_to_datetime(as.integer(theFile[8,1])), "-", convert_to_datetime(as.integer(theFile[8,1]))+60*60*24*6)
  
  return(ggplot(workingData,aes(x=Minutes,y=Steps,group=Day, color = Group)) + 
           geom_line() +
           geom_text_repel(aes(label=Label)) +
           theme(legend.position = "none") +
           scale_x_continuous(breaks=c(indexOfHours),labels=hours)+
           labs(title = chartTitle, x = "Time of Day", y = "Cumulative Steps", colour="")
  )
}

printFiles <- function() {
  file01 <<- as.data.frame(read_excel("/Users/samzimmer/Desktop/pigs/Data/file01.xlsx",n_max=1440*7))
  file02 <<- as.data.frame(read_excel("/Users/samzimmer/Desktop/pigs/Data/file02.xlsx",n_max=1440*7))
  file03 <<- as.data.frame(read_excel("/Users/samzimmer/Desktop/pigs/Data/file03.xlsx",n_max=1440*7))
  file04 <<- as.data.frame(read_excel("/Users/samzimmer/Desktop/pigs/Data/file04.xlsx",n_max=1440*7))
  file05 <<- as.data.frame(read_excel("/Users/samzimmer/Desktop/pigs/Data/file05.xlsx",n_max=1440*7))
  file06 <<- as.data.frame(read_excel("/Users/samzimmer/Desktop/pigs/Data/file06.xlsx",n_max=1440*7))
  file07 <<- as.data.frame(read_excel("/Users/samzimmer/Desktop/pigs/Data/file07.xlsx",n_max=1440*7))
  file08 <<- as.data.frame(read_excel("/Users/samzimmer/Desktop/pigs/Data/file08.xlsx",n_max=1440*7))
  file09 <<- as.data.frame(read_excel("/Users/samzimmer/Desktop/pigs/Data/file09.xlsx",n_max=1440*7))
  file10 <<- as.data.frame(read_excel("/Users/samzimmer/Desktop/pigs/Data/file10.xlsx",n_max=1440*7))
  file11 <<- as.data.frame(read_excel("/Users/samzimmer/Desktop/pigs/Data/file11.xlsx",n_max=1440*7))
  info <<- read.csv("/Users/samzimmer/Desktop/pigs/morgansowdatarev.csv")
  pdf("/Users/samzimmer/Desktop/pigs/Visualizations/steps.pdf")
  print("Files loaded")
  
  print(makeGraph(file01))
  print(makeGraph(file02))
  print(makeGraph(file03))
  print(makeGraph(file04))
  print(makeGraph(file05))
  print(makeGraph(file06))
  print(makeGraph(file07))
  print(makeGraph(file08))
  print(makeGraph(file09))
  print(makeGraph(file10))
  print(makeGraph(file11))
  dev.off()
  print("Graphs made")
}
loadPackages()
printFiles()
