###########
# Exploratory Data Analysis
# -------
# Course Project 1: plot4.R
#
# Assignment Info:
# https://github.com/rdpeng/ExData_Plotting1
#
# Work by: Allen Hammock
#  Github: brainvat
# Project: https://github.com/brainvat/ExData_Plotting1/
#
# Usage:
#
#   Source this file in your R environment to execute.
#   Code will download data files if uncompressed source
#   data is not found in the current working directory.
#   Output of PNG file will be saved to current working
#   directory.
#
#   Examine other data outputs with str(res) if you are
#   executing this code in a REPL like RStudio.
#
#   Skip down to main() function to see where the execution
#   really starts.
#
#######

#####
# Utility Functions
##

# logging
log <- function(..., file="logfile.txt") {
    cat(paste(..., "\n", sep=""))
}

# load dependencies and warn the user
load_library <- function(lib, halt=FALSE) {
    if (!(lib %in% rownames(installed.packages()))) {
        if (halt) {
            stop("Please install the package '", lib, "' and try this script again.", sep="")
        } else {
            return(FALSE)
        }
    } else {
        library(lib, character.only=TRUE, warn.conflicts = FALSE)
        return(TRUE)
    }
}

# make a repeating string, inspired by the makeNstr from the Hmisc package
# modified to work here without any external library
makeNstr <- function(str, num) {
    if (num == 0) return("")
    paste(rep(str, num), collapse="")
}

# print numbers with separators
pretty_num <- function(n, mark = ",") {
    return(format(n, big.mark = mark))
}

# create a horizontal rule
hr <- function(l=60) {
    return(paste(makeNstr("-", l), "\n", sep=""))
}

# create a header wrapped in horizontal rules
h <- function(..., space_above=0, space_below=0, rule=60) {
    log(makeNstr("\n", space_above), hr(l=rule), ..., "\n", hr(l=rule), makeNstr("\n", space_below))
}

# estimate memory usage in bytes for a data frame from file on disk
# if you just use the defaults, you get back the estimated size of 1 row
estimate_memory_size <- function(file, n_samples = 100, n_rows = 100, pretty = FALSE) {
    
    # for pretty printing, optional
    pretty_lib <- load_library("gdata")
    
    val <- 0
    if (file.exists(file)) {
        sample <- read.table(file, nrows = n_samples)
        val <- n_rows * object.size(sample)[1]
    }
    
    if (pretty) {
        if (pretty_lib) {
            return(humanReadable(val))
        } else {
            return(paste(pretty_num(val), " bytes", sep=""))
        }        
    } else {
        return(val)
    }
}

# read lines from text file, optionally testing each line with fun(line)
# to decide whether to keep it or not

read_matching_lines <- function(file, test_function = function(l) TRUE, debug = TRUE) {
    if (!file.exists(file)) {
        return("")
    }
    
    con  <- file(file, open = "r")
    my.data <- character()
    count <- 0
    if (debug) {
        cat("X")
    }
    
    while (length(one.line <- readLines(con, n = 1, warn = FALSE)) > 0) {
        count <- count + 1
        if (debug) {
            
            if (count %% 2 == 0) {
                cat("\b-")
            } else if (count %% 3 == 0) {
                cat("\b/")
            } else {
                cat("\b|")
            }
        }
        if (test_function(one.line)) {
            my.data <- c(my.data, one.line)
        }
    }
    if (debug) {
        log("\nRead ", pretty_num(count), " lines from file")
    }
    
    close(con)
    return(my.data)
}

# download and extract files from Internet

get_data <- function(data_url, zipfile, data_file) {
    # Skip if we have already have the data file
    if (!file.exists(data_file)) {
        h("Download and Extract Source Data")
        
        # Skip download if we already have the zip file
        if (!file.exists(zipfile)) {
            log("Downloading compressed data set from ", data_url)
            res <- download.file(url=data_url, destfile=zipfile, method="curl", mode="w")
        }
        
        # Double-check that download completed and decompress
        if (!file.exists(zipfile)) {
            stop("Unable to download the data from the Internet.\n\n")
        } else {
            log("Extracting compressed data to ", data_file)      
            res <- unzip(zipfile=zipfile,overwrite=FALSE)
            if (!file.exists(data_file)) {
                stop("ERROR. Problem decompressing zip file\n\n")
            } 
        }
    }
    return(TRUE)
}

make_graph <- function(lines, png_file = "output.png", watermark = FALSE) {
    
    days <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
    data <- sapply(X=lines,FUN = function(x) strsplit(x, ";"), simplify = "array")
    data <- data.frame(matrix(unlist(data), nrow = length(data), byrow = TRUE), stringsAsFactors = FALSE)
    names(data) <- c("Date", "Time", "Global_active_power", "Global_reactive_power", "Voltage", "Global_intensity", "Sub_metering_1", "Sub_metering_2", "Sub_metering_3")
    data <- cbind(data, DateTime = strptime(paste(data$Date, data$Time, sep = " "), "%d/%m/%Y %H:%M:%S"))
    data <- cbind(data, DayOfWeek = days[as.POSIXlt(data$DateTime)$wday])
    data$Sub_metering_1 <- as.numeric(data$Sub_metering_1)
    data$Sub_metering_2 <- as.numeric(data$Sub_metering_2)
    data$Sub_metering_3 <- as.numeric(data$Sub_metering_3)
    data$Global_active_power <- as.numeric(data$Global_active_power)
    data$Global_reactive_power <- as.numeric(data$Global_reactive_power)
    data$Voltage <- as.numeric(data$Voltage)
    
    # using base graphics system, produce the histogram
    # match Title, X and Y labels, and bar colors
    # to look like figure/unnamed-chunk-2.png
    
    png(filename = png_file, height = 480, width = 480)
    par(mfrow = c(2,2))
    
    # 1, 1 
    plot(data$Global_active_power ~ data$DateTime, ylab="Global Active Power", type="l", xlab = "")

    # 1, 2 
    plot(data$Voltage ~ data$DateTime, ylab="Voltage", type="l", xlab = "datetime")
    
    # 2, 1 
    plot(data$Sub_metering_1 ~ data$DateTime, ylab="Energy sub metering", type="l", xlab = "")
    lines(data$Sub_metering_2 ~ data$DateTime, col = "red")
    lines(data$Sub_metering_3 ~ data$DateTime, col = "blue")
    legend("topright", lty = 1, bty = "n", legend = c("Sub_metering_1","Sub_metering_2", "Sub_metering_3"), col = c("black", "blue","red"))

    # 2, 2 
    plot(data$Global_reactive_power ~ data$DateTime, ylab="Global_reactive_power", type="l", xlab = "datetime")
    
    watermark <- watermark && load_library("RCurl", halt = FALSE) && load_library("png")
    if (watermark) {
        cat("Let's do this!\n")
        me.png.url <- "https://avatars3.githubusercontent.com/u/7524494?v=2&s=460"
        me.png <- readPNG(getURLContent(me.png.url))
        tmp <- par('usr')
        rasterImage(me.png, tmp[1]*0.25, tmp[3]*0.25, tmp[2]*0.25, tmp[4]*0.25)
    }
    dev.off()
    
    if (file.exists(png_file)) {
        log("Successfully created ", png_file)
    } else {
        stop("ERROR. Unable to create ", png_file)
    }
    
    return(list(lines=lines, data=data, graphinfo=NULL))
}

#####
# Main program
##

main <- function(global_data = NULL) {
    
    # download the data if has not already been retrieved
    # this saves a lot of time if you are running this
    # script over and over, for example during development
    
    data_url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
    zipfile <- "household_power_consumption.zip"
    data_file <- "household_power_consumption.txt"
    png_file <- "plot4.png"
    data_row_count <- 2075259
    line_match_func <- function(l) grepl(pattern="^0?[12]\\/2\\/2007", x = l)
    
    h("Download and Extract Data")
    get_data(data_url = data_url, zipfile = zipfile, data_file = data_file )
    log("Decompressed data file found in current working directory")
    log("Estimated memory usage to read ", pretty_num(data_row_count), " records from ", data_file, " would be ", estimate_memory_size(data_file, n_rows = data_row_count, pretty = TRUE))
    log("Reading data file, skipping lines outside of required date range (this may take a while)")
    
    # since data processing takes so long, when developing this
    # script it's useful to skip this process if you know
    # it's working fine
    
    if (!is.null(global_data)) {
        log("Using global data called on main()")
        lines <- global_data[["lines"]]
    } else {
        lines <- read_matching_lines(data_file, test_function = line_match_func)
        log("Read in ", pretty_num(length(lines)), " matching rows of data from file")        
    }
    
    # Now we can extract just the column we want to produce
    # the histogram
    
    h("Download and Extract Data")
    log("Producting histogram from Global Active Power observations")    
    res <- make_graph(lines, png_file = png_file)
    
    # return the lines, the data vector, and the
    # histogram object back to the global environment
    # so we can inspect it, very useful during development
    
    return(res)
}

#res <- main()

# run it again, but without all of the tedious waiting
# to load the original data file!
#
# my_res <- main(res)
# my_res <- make_graph(res[["lines"]], png_file = "my_res.png")
