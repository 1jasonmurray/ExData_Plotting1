plot3 <- function() {
	
	## The purpose of this function is to generate a graph showing values
	## of sub metered power for the dates 2007-02-01 and 2007-02-02 from 
	## the following data source:
	## https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip
	
	
	## Create directories and download data if necessary
	fileURL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
	dataDir <- "./data"
	dataZIP <- paste(dataDir, "/household_power_consumption.zip", sep = "")
	dataFile <- paste(dataDir, "/household_power_consumption.txt", sep = "")
	
	if (!dir.exists(dataDir)) {dir.create(dataDir)}
	if (!file.exists(dataFile)) {
		download.file(fileURL, dataZIP) 
		unzip(dataZIP,exdir = dataDir)
		}
	
	
	## Import data, subset based on dates, and combine Date/Time to one field 
	## DateTime and drop original fields
	powerData <- read.csv(dataFile, sep = ";", na.strings = "?",stringsAsFactors = FALSE)
	powerData <- filter(powerData, Date == "1/2/2007" | Date == "2/2/2007") %>%
		mutate(DateTime = as.POSIXct(paste(Date, Time), format = "%d/%m/%Y %T")) %>%
		select(DateTime,Global_active_power:Sub_metering_3)
	
	
	
	
	## Generate plot to file as png
	plot3File <- paste(dataDir,"/plot3.png", sep = "")
	png(plot3File)
	with(powerData, plot(DateTime, Sub_metering_1, type = "l", xlab = "", ylab = "Energy sub metering"))
	with(powerData, lines(DateTime, Sub_metering_2, col = "red"))
	with(powerData, lines(DateTime, Sub_metering_3, col = "blue"))
	legend("topright",c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lty = 1, col = c("black", "red", "blue"))
	dev.off()
	
	
}
