#####
# Utility Functions
##

# logging
log <- function(..., file="logfile.txt") {
  cat(paste(..., "\n", sep=""))
}

# load dependencies and warn the user
load_library <- function(lib) {
  if (!(lib %in% rownames(installed.packages()))) {
    stop("Please install the package '", lib, "' and try this script again.", sep="")
  } else {
    library(lib, character.only=TRUE)
  }
}

# make a repeating string, inspired by the makeNstr from the Hmisc package
# modified to work here without any external library
makeNstr <- function(str, num) {
  if (num == 0) return("")
  paste(rep(str, num), collapse="")
}

# create a horizontal rule
hr <- function(l=60) {
  return(paste(makeNstr("-", l), "\n", sep=""))
}

# create a header wrapped in horizontal rules
h <- function(..., space_above=0, space_below=0, rule=60) {
  log(makeNstr("\n", space_above), hr(l=rule), ..., "\n", hr(l=rule), makeNstr("\n", space_below))
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
}

#####
# Main program
##

main <- function() {

  # download the data if has not already been retrieved
  data_url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
  zipfile <- "household_power_consumption.zip"
  data_file <- "household_power_consumption.txt"
  png_file <- "plot2.png"

  get_data(data_url = data_url, zipfile = zipfile, data_file = data_file )
  h("Process Extracted Data")
  
  png(filename = png_file, height = 480, width = 480)
  hist(rnorm(1000))
  dev.off()
  
  if (file.exists(png_file)) {
    log("Successfully created ", png_file)
  } else {
    stop("ERROR. Unable to create ", png_file)
  }
}

main()
