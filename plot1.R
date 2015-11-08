##
## This function will create/replace a graphic file plot1.png
## showing a histogram for the Global Active Power value
## based on information contained in 
## https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip
## The function assumes the text file household_power_consumption.txt
## has been extracted from the .zip file and it's located
## in the working directory.
## The function also uses the package "lubridate", so it assumes
## it is already installed
##

plot1 <- function() {
  ## Read file
  myData <-read.table("household_power_consumption.txt", header= TRUE, sep=";")
  ## Subset only the two dates to be used in the analysis
  z<-myData$Date == "1/2/2007" | myData$Date == "2/2/2007"
  myNewData<-myData[z,]
  ## Remove the original data from memory
  rm(myData)
  
  ## Reformat date and time columns into one single column
  ## As suggested in https://class.coursera.org/exdata-034/forum/thread?thread_id=17
  library(lubridate)
  myNewData$DateFormat<-dmy_hms(paste(myNewData$Date, myNewData$Time))
  
  ## Reformat the numeric columns needed for all the plots
  ## in the analysis
  myNewData$GlobalActivePower_num <-as.double(as.character(myNewData$Global_active_power))
  myNewData$SubMetering1_num<-as.numeric(as.character(myNewData$Sub_metering_1))
  myNewData$SubMetering2_num<-as.numeric(as.character(myNewData$Sub_metering_2))
  myNewData$SubMetering3_num<-as.numeric(as.character(myNewData$Sub_metering_3))
  myNewData$Voltage_num<-as.numeric(as.character(myNewData$Voltage))
  myNewData$GlobalReactivePower_num<-as.numeric(as.character(myNewData$Global_reactive_power))
  
  ## Generate plot in the png grahic device
  png("plot1.png",width=480,height=480)
  par(mar=c(4,6,4,1))
  hist(myNewData$GlobalActivePower_num,main="Global Active Power", xlab="Global Active Power (kilowatts)", col="RED")
  dev.off()
}
