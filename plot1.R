# Exploratory Data Analysis.
# Week 1 Project.
# Plot 1.
# Jai Jeffryes

# Create data directory.
if (!dir.exists("./Data")) {dir.create("./Data")}

# Download the dataset.
DLurl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
DLfile <- "./Data/household_power_consumption.zip"
Logfile <- "./Data/Download.log"
Overwrite <- FALSE

if (!file.exists(DLfile) | Overwrite == TRUE) {
        download.file(DLurl, DLfile)
        DLdate <- date()
        
        DLfileMsg <- paste0("Download file: ", DLfile)
        DLurlMsg <- paste0("URL: ", DLurl)
        DLdateMsg <- paste0("Download date: ", DLdate)
        
        cat(paste(" ", DLfileMsg, DLurlMsg, DLdateMsg, sep = "\n"),
            file = Logfile, sep = "\n", append = TRUE)
} else {
        print("File exists. Download skipped.")
}

# Extract it from the archive.
if (!file.exists("./Data/household_power_consumption.txt")) {
        unzip(DLfile, exdir = "./Data/")
}

# Data file
power_file <- "./Data/household_power_consumption.txt"

# Get the file line count.
# Get the classes. Use this when you read the whole table.
library(stringi)
cmd <- "wc"
line_count <- system(paste(cmd, power_file), intern = TRUE)
line_count <- as.numeric(stri_extract_first(line_count, regex = "\\d+"))
line_count # 2,075,260

initial <-  read.table(power_file, sep = ";", header = TRUE, nrows = 100, na.strings = "?")
# The Date and Time variables will later be read as character vectors.
classes <- gsub("factor", "character", sapply(initial, class))

# Read lines of the input file. Pick off the dataset we want by catting those
# lines to a new file.
power_file_filter <- "./Data/household_power_consumption_filter.txt"

# Only split the file if we haven't done it yet for another plot.
if (!file.exists(power_file_filter)) {
        theInput <- readLines(power_file, n = -1) # Change to n = -1 for whole file.
        minDate <- as.Date("2007-02-01")
        maxDate <- as.Date("2007-02-02")
        
        # Send header to out.
        cat(theInput[1], file = power_file_filter, sep = "\n", append = TRUE)
        
        # Continue with line 2.
        theResult <- lapply(theInput[-1], function(x) {
                inLine <- strsplit(x[[1]], ";")
                inDate <- as.Date(inLine[[1]][1], "%d/%m/%Y")
                
                if (inDate >= minDate & inDate <= maxDate) { # send the line to the new file.
                        cat(x, file = power_file_filter, sep = "\n", append = TRUE)
                        catCount <- catCount + 1
                }
        })
}

# Count the lines in the filtered file.
cmd <- "wc"
power_filter_line_count <- system(paste(cmd, power_file_filter), intern = TRUE)
power_filter_line_count <- as.numeric(stri_extract_first(power_filter_line_count, regex = "\\d+"))

# Report the line counts of the files.
print(paste0("Power file line count, ", power_file, ": ", line_count))
print(paste0("Power filtered file line count", power_file_filter, ": ", power_filter_line_count))

# Make a line estimate for fast reading.
n_rows <- 1.1 * power_filter_line_count

# Read data.
power_consumption <-  read.table(power_file_filter, sep = ";", header = TRUE, nrows = n_rows,
                                 na.strings = "?", colClasses = classes)

# Convert date and time.
power_consumption$DateTime <- strptime(paste(power_consumption$Date, power_consumption$Time),
                                       "%d/%m/%Y %T")

# Ready to plot.
# Set up and open output file.
plotFile <- "plot1.png"
plotWidth <- 480
plotHeight <- 480
png(filename = plotFile, width = plotWidth, height = plotHeight)

# Plot.
hist(power_consumption$Global_active_power,
     main = "Global Active Power",
     xlab = "Global Active Power (kilowatts)",
     col = "red")

# Close file.
dev.off()
