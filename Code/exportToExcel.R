loadPackages <- function() {
  library(readxl)
  library(writexl)
  library(janitor)
  library(ggplot2)
  library(tidyr)
  library(dplyr)
  library(ggrepel)
}
makePigTable  <- function(theFile) {
  #PULL PIG'S INFO
  pigStartDate <- convert_to_datetime(as.integer(theFile[3,3]))
  pigIceTag <- substr(colnames(theFile)[3],6,8)
  info$Start_Date <- as.Date(info$Start_Date,"%m/%d/%Y")
  info$End_Date <- as.Date(info$End_Date,"%m/%d/%Y")
  pigInfo <- which(info$Start_Date == pigStartDate & info$`IceTag_.` == pigIceTag)
  
  dataStart <- which(theFile[1] == "Date")
  myData <- theFile[-(0:dataStart+1),]
  #naming columns by data names, rather than original headers
  colnames(myData) <- theFile[dataStart,]
  colnames(myData) <- list("Date","Time","MotionIndex","Standing","Lying","Steps","LyingBouts")
  
  #ADJUSTING FOR START TIME OF 10AM
  myData$Time <- as.numeric(myData$Time)
  difference <- (myData$Time[1]-(10/24))*1440
  if (difference<0) {
    firstLine <- which(myData$Time==(10/24))[1]
    myData <- myData[firstLine:(1440*6+firstLine-1),]
  } else if (difference>0) {
    fillDayTime <- c(600:(600+difference-1)/1440)
    fillDayDate <- c(rep(myData$Date[1],times=length(fillDayTime)))
    fillDayRest <- c(rep(0,times=length(fillDayTime)))
    fillDay <<- data.frame(Date=fillDayDate,
                           Time=fillDayTime,
                           MotionIndex = fillDayRest,
                           Standing = fillDayRest,
                           Lying = fillDayRest,
                           Steps = fillDayRest,
                           LyingBouts = fillDayRest)
    myData <- rbind(fillDay,myData)
    myData <- myData[1:(1440*6),]
  } else {
    myData <- myData[1:(1440*6),]
  }
  
  #converting date to readable form
  myData$Date <- as.numeric(myData$Date) + as.numeric(myData$Time)
  myData <- myData[,c(1,3:7)]
  myData$Date <- convert_to_datetime(myData$Date)
  #converting standing and lying to readable format
  myData$Standing <- as.numeric(myData$Standing)*1440
  myData$Lying <- as.numeric(myData$Lying)*1440
  #fixing row index
  row.names(myData) <- 1:nrow(myData)
  myData$pigID <- c(info$ID[pigInfo])
  return (myData)
}
loadFiles <- function() {
  info <<- read.csv("/Users/samzimmer/Desktop/pigs/morgansowdatarev.csv")
  file01 <<- makePigTable(read_excel("/Users/samzimmer/Desktop/pigs/Data/file01.xlsx",n_max=1440*7))
  file02 <<- makePigTable(read_excel("/Users/samzimmer/Desktop/pigs/Data/file02.xlsx",n_max=1440*7))
  file03 <<- makePigTable(read_excel("/Users/samzimmer/Desktop/pigs/Data/file03.xlsx",n_max=1440*7))
  file04 <<- makePigTable(read_excel("/Users/samzimmer/Desktop/pigs/Data/file04.xlsx",n_max=1440*7))
  file05 <<- makePigTable(read_excel("/Users/samzimmer/Desktop/pigs/Data/file05.xlsx",n_max=1440*7))
  file06 <<- makePigTable(read_excel("/Users/samzimmer/Desktop/pigs/Data/file06.xlsx",n_max=1440*7))
  file07 <<- makePigTable(read_excel("/Users/samzimmer/Desktop/pigs/Data/file07.xlsx",n_max=1440*7))
  file08 <<- makePigTable(read_excel("/Users/samzimmer/Desktop/pigs/Data/file08.xlsx",n_max=1440*7))
  file09 <<- makePigTable(read_excel("/Users/samzimmer/Desktop/pigs/Data/file09.xlsx",n_max=1440*7))
  file10 <<- makePigTable(read_excel("/Users/samzimmer/Desktop/pigs/Data/file10.xlsx",n_max=1440*7))
  file11 <<- makePigTable(read_excel("/Users/samzimmer/Desktop/pigs/Data/file11.xlsx",n_max=1440*7))
  files <<- list(file01,file02,file03,file04,file05,file06,file07,file08,file09,file10,file11)
  print("Files loaded and formatted")
}
timeColumn <<- substr(convert_to_datetime((600:2039)/1440),12,16)
tableDay <- function(theFile) {
  pigID <- theFile$pigID[1]
  print(pigID)
  pigInfo <- which(info$ID == pigID)
  print(pigInfo)
  PigInfoHolder <- data.frame(PigInfo1 = c(paste(colnames(info),": "),rep("",times=1440-17)),
                              PigInfo2 = c(unname(unlist(info[pigInfo,])),rep("",times=1440-17)))
  PigInfoHolder <- unite(PigInfoHolder, col='PigInfo', c("PigInfo1", "PigInfo2"), sep='')
  
  theFile$Day <- rep(paste("Day",2:7,sep=""),each=1440)
  MotionIndexHolder <- data.frame(MotionIndex = theFile$MotionIndex,
                                  Day = theFile$Day,
                                  Index = rep(1:1440,times=6))
  MotionIndexHolder <- spread(MotionIndexHolder, key = Day, value = MotionIndex)
  MotionIndexHolder$Index <- timeColumn
  colnames(MotionIndexHolder)[1] <- "Time"
  MotionIndexHolder[2:7] <- sapply(MotionIndexHolder[2:7],cumsum)
  MotionIndexHolder$PigInfo <- PigInfoHolder$PigInfo
  
  StandingHolder <- data.frame(Standing = theFile$Standing,
                               Day = theFile$Day,
                               Index = rep(1:1440,times=6))
  StandingHolder <- spread(StandingHolder, key = Day, value = Standing)
  StandingHolder$Index <- timeColumn
  colnames(StandingHolder)[1] <- "Time"
  StandingHolder[2:7] <- sapply(StandingHolder[2:7],cumsum)
  StandingHolder$PigInfo <- PigInfoHolder$PigInfo
  
  LyingHolder <- data.frame(Lying = theFile$Lying,
                            Day = theFile$Day,
                            Index = rep(1:1440,times=6))
  LyingHolder <- spread(LyingHolder, key = Day, value = Lying)
  LyingHolder$Index <- timeColumn
  colnames(LyingHolder)[1] <- "Time"
  LyingHolder[2:7] <- sapply(LyingHolder[2:7],cumsum)
  LyingHolder$PigInfo <- PigInfoHolder$PigInfo
  
  StepsHolder <- data.frame(Steps = theFile$Steps,
                            Day = theFile$Day,
                            Index = rep(1:1440,times=6))
  StepsHolder <- spread(StepsHolder, key = Day, value = Steps)
  StepsHolder$Index <- timeColumn
  colnames(StepsHolder)[1] <- "Time"
  StepsHolder[2:7] <- sapply(StepsHolder[2:7],cumsum)
  StepsHolder$PigInfo <- PigInfoHolder$PigInfo

  LyingBoutsHolder <- data.frame(LyingBouts = theFile$LyingBouts,
                                 Day = theFile$Day,
                                 Index = rep(1:1440,times=6))
  LyingBoutsHolder <- spread(LyingBoutsHolder, key = Day, value = LyingBouts)
  LyingBoutsHolder$Index <- timeColumn
  colnames(LyingBoutsHolder)[1] <- "Time"
  LyingBoutsHolder[2:7] <- sapply(LyingBoutsHolder[2:7],cumsum)
  LyingBoutsHolder$PigInfo <- PigInfoHolder$PigInfo
  
  #NEED THESE VARIABLES TO GO GLOBAL
  
  assign(paste("MotionIndex",pigID,sep=""), MotionIndexHolder, envir = as.environment(".GlobalEnv"))
  assign(paste("Standing",pigID,sep=""), StandingHolder, envir = as.environment(".GlobalEnv"))
  assign(paste("Lying",pigID,sep=""), LyingHolder, envir = as.environment(".GlobalEnv"))
  assign(paste("Steps",pigID,sep=""), StepsHolder, envir = as.environment(".GlobalEnv"))
  assign(paste("LyingBouts",pigID,sep=""), LyingBoutsHolder, envir = as.environment(".GlobalEnv"))
}
exportTables <- function() {
  MotionIndexList <- list(MotionIndex55471,MotionIndex55489,MotionIndex55503,MotionIndex55504,MotionIndex55508,MotionIndex55515,MotionIndex55516,MotionIndex55524,MotionIndex55526,MotionIndex55527,MotionIndex55536)
  StandingList <- list(Standing55471,Standing55489,Standing55503,Standing55504,Standing55508,Standing55515,Standing55516,Standing55524,Standing55526,Standing55527,Standing55536)
  LyingList <- list(Lying55471,Lying55489,Lying55503,Lying55504,Lying55508,Lying55515,Lying55516,Lying55524,Lying55526,Lying55527,Lying55536)
  StepsList <- list(Steps55471,Steps55489,Steps55503,Steps55504,Steps55508,Steps55515,Steps55516,Steps55524,Steps55526,Steps55527,Steps55536)
  LyingBoutsList <- list(LyingBouts55471,LyingBouts55489,LyingBouts55503,LyingBouts55504,LyingBouts55508,LyingBouts55515,LyingBouts55516,LyingBouts55524,LyingBouts55526,LyingBouts55527,LyingBouts55536)
  
  write_xlsx(MotionIndexList,"/Users/samzimmer/Desktop/pigs/Tables/MotionIndex.xlsx")
  write_xlsx(StandingList,"/Users/samzimmer/Desktop/pigs/Tables/Standing.xlsx")
  write_xlsx(LyingList,"/Users/samzimmer/Desktop/pigs/Tables/Lying.xlsx")
  write_xlsx(StepsList,"/Users/samzimmer/Desktop/pigs/Tables/Steps.xlsx")
  write_xlsx(LyingBoutsList,"/Users/samzimmer/Desktop/pigs/Tables/LyingBouts.xlsx")
}
loadPackages()
loadFiles()
lapply(files,tableDay)
exportTables()



# TO DO:
# export the frames to global variables
# export variables to excel