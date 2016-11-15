plot1 <- function() {
	
	## The purpose of this function is to generate a graph showing a histogram
	## of global active power for the dates 2007-02-01 and 2007-02-02 from 
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
	plot1File <- paste(dataDir,"/plot1.png", sep = "")
	png(plot1File)
	hist(powerData$Global_active_power, col="red", main = "Global Active Power", xlab = "Global Active Power (kilowatts)")
	dev.off()
	
	
	
	
	
	
	
	
}