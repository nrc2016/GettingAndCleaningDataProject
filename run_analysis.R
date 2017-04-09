required.packages <- c("downloader", "data.table")
data.directory <- "data"
zip.filename <- paste(data.directory, "dataset.zip", sep="/")
data.set.url <- "https://d396qusza40orc.cloudfront.net/getdata/projectfiles/UCI HAR Dataset.zip"

check_required_package_installed <- function (package.name) {
  if(!require(package.name, character.only = T)) {
    install.packages(package.name)
    if(!require(package.name, character.only = T)) {
      return(FALSE)
    }
  } 
  
  return(TRUE)
}

check_required_packages_installed <- function (required.packages) {
  print("...... Start of checking required packages ...")
  
  result <- lapply(required.packages, check_required_package_installed)
  not.loaded.library <- which(result != TRUE)
  if (length(not.loaded.library)==0) {
    print("...... End of checking required packages.")
    return(TRUE)
  }
  
  error.string <- lapply(not.loaded.library, FUN=function(x) {
    return(paste("...... ERROR: Package ", required.packages[x], " cannot be loaded or installed.", sep=""))
  })
  error.string <- unlist(error.string)

  return(error.string)
}

download_data_set <- function(url, zip.file) {
  print("...... Start of downloading data set ...")
  download(url, zip.file)
  print("...... End of downloading data set.")
}

load_data_set <- function (url, zip.file, ext.dir) {
  print("... Start of loading data set ...")
  if(!file.exists(zip.file)) {
    download_data_set(url, zip.file)
    if(!file.exists(zip.file)) {
      return(FALSE)
    }
  }
  unzip(zip.file, exdir=ext.dir)
  print("... End of loading data set.")
}

run_analysis <- function () {
  print("Start of analysis...")
  
  # check required packages installed
  error.message <- check_required_packages_installed(required.packages)
  if (error.message != TRUE) {
    print(error.message)
    return(FALSE)
  }
  
  # load the data
  error.message <- load_data_set(data.set.url, zip.filename, data.directory)
  if (error.message == FALSE) {
    print("ERROR: Cannot either download or load the data set.")
    return(FALSE)
  }
  
  # format variable names
  
  # join the data sets
  
  # provide averages and standard 
  
  # write output
  
  print("... End of analysis.")
}