##
## This function will create/replace a graphic file plot3.png
## showing a plot of the Sub Metering Energy values vs Date
## based on information contained in 
## https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip
## The function assumes the text file household_power_consumption.txt
## has been extracted from the .zip file and it's located
## in the working directory.
## The function also uses the package "lubridate", so it assumes
## it is already installed
##

plot3 <- function() {
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
  
  ## Change the local to English, so the names of the days in
  ## the plot will be in English and not the locale of the
  ## current installation
  ## As suggested in https://class.coursera.org/exdata-034/forum/thread?thread_id=33
  curr_locale <- Sys.getlocale("LC_TIME")
  Sys.setlocale("LC_TIME","en_US.UTF-8")
  
  ## Generate plot in the png grahic device
  png("plot3.png",width=480,height=480)
  par(mar=c(4,6,4,1))
  with(myNewData,plot(DateFormat,SubMetering1_num,type="l",ylab="Energy sub metering",xlab=""))
  lines(myNewData$DateFormat,myNewData$SubMetering2_num,type="l",col="RED")
  lines(myNewData$DateFormat,myNewData$SubMetering3_num,type="l",col="BLUE")
  legend(x="topright",c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),col=c("BLACK","RED","BLUE"),lty=c(1, 1, 1))
  dev.off()
  
  ## Change the locale back to the original one
  Sys.setlocale("LC_TIME",curr_locale)
}
