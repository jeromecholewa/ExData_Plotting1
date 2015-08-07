plot3 <- function() {

        electric <- create_small_needed_file()

        # first call with plot creates the plot
        # and corrects the labels x and y
        with(electric, plot(Time, Sub_metering_1, type = "l",
                            ylab = "Energy sub metering",
                            xlab = ""))

        # 2 calls with "points" using other columns and  different colors
        with(electric, points(Time, Sub_metering_2, type = "l",
                            col = "red"))
        with(electric, points(Time, Sub_metering_3, type = "l",
                              col = "blue"))

        # adding legends
        legend("topright", col = c("black", "red", "blue"),
               legend = c("Sub_metering_1", "Sub_metering_2",
                          "Sub_metering_3"), lwd = 2,
               y.intersp = 1, xjust = 0,
               cex = .8)


        #copy to desired device (=png file) and close png device
        dev.copy(png, file = "plot3.png")
        dev.off()
}


# this function extracts only the necessary 1880 lines from the big file
# and coerces the Date and Time column into date and POSIXct format
# respectively
create_small_needed_file <- function() {
setwd("~/Documents/Education/Coursera/Exploratory_Data_Analysis/Week1/electric-project1/")
exploratory_dir <- getwd()

electric_explore  <- read.table("household_power_consumption.txt", nrows = 20, sep = ";", header = T)

colunam <- colnames(electric_explore)

colclasses  <- c("character", "character", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric")

# based on analysis of the head + tail + certain parts of the file
# using read.table(....  nrows = 200), then (nrows = 2100000, skip = 2075000) to get the end of the file
# then (nrows = 2000, skip = xxx), I found out that the needed dates were
# around after row 66620 and the next 3000 rows or so.
electric_need  <- read.table("household_power_consumption.txt", nrows = 2910, skip = 66620, sep = ";", header = F, colClasses = colclasses,  col.names = colunam)

# then I sorted only the needed rows and confirmed I got all 2880 rows
# since 2880 minutes are 2 days
electric_final  <- electric_need[electric_need$Date == "1/2/2007" |  electric_need$Date == "2/2/2007",]

# formating dates as dates (instead of characters)
electric_final$Date  <- as.Date(electric_final$Date, "%d/%m/%Y")

# formating times as real times (instead of characters), but for that we
# need to add the correct date in front of the time, otherwise it will be
# the correct time but today's date!!
electric_final$Time  <- as.POSIXct(strptime(paste(electric_final$Date, electric_final$Time, sep = " "), "%Y-%m-%d %H:%M:%S"))

electric_final

}