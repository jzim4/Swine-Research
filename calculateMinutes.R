#importing file
file <- read.csv(file.choose())
#select data, ENTER DESIRED START INTO "", IF NONE, SAY "Date"
firstLine <- which(file == "Date")
myData <- file[-(0:firstLine+1),]
#naming columns by data names, rather than original headers
colnames(myData) <- file[7,]
#resetting indecies for rows
rownames(myData) <- 1:nrow(myData)

#converting Time from analog clock to minutes FROM MIDNIGHT ON DAY 1
#TO DO HERE: 
# ACCOUNT FOR NEW DATE
inMinutes <- function(date, time) {
  splitDate <- strsplit(date, split = "/")
  splitTime <- strsplit(time, split = ":")
  minutes <- as.integer(splitTime[[1]][1]) * 60 + as.integer(splitTime[[1]][2])
  return(minutes)
}

myData2$Time <- lapply(myData2$Time, inMinutes)

#converting standing and lying to percentages
myData2 <- myData
