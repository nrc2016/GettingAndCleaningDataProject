##############################################################################
#
# Filename: run_analysis.R
#
# This file defines the functions required to download, merge, tidy, and write
# data sets for the UCI HAR Data Set. This file was written for the Getting
# and Cleaning Data course.
#
# Functions:
# - check_required_package_installed
# - check_required_packages_installed
# - download_data_set
# - unzip_data_set
# - load_and_filter_features
# - load_activities
# - join_and_filter_data_sets
# - calculate_means_activities_subjects
# - run_analysis
#
# Date: April 9, 2017
#
#
# Thank you for your time.
#
##############################################################################

required.packages <- c("downloader", "data.table", "dplyr")
data.directory <- "data"
zip.filename <- paste(data.directory, "dataset.zip", sep="/")
data.set.url <- "https://d396qusza40orc.cloudfront.net/getdata/projectfiles/UCI HAR Dataset.zip"
data.set.output.1.filename = paste(data.directory, "tidy_merged_data_set.txt", sep="/")
data.set.output.2.filename = paste(data.directory, "tidy_mean_data_set.txt", sep="/")

features.filename <- paste(data.directory, "UCI HAR Dataset", "features.txt", sep="/")
activities.filename <- paste(data.directory, "UCI HAR Dataset", "activity_labels.txt", sep="/")

train.filename <- paste(data.directory, "UCI HAR Dataset", "train", "X_train.txt", sep="/")
test.filename <- paste(data.directory, "UCI HAR Dataset", "test", "X_test.txt", sep="/")
data.set.filenames <- c(train.filename, test.filename)

train.label.filename <- paste(data.directory, "UCI HAR Dataset", "train", "y_train.txt", sep="/")
test.label.filename <- paste(data.directory, "UCI HAR Dataset", "test", "y_test.txt", sep="/")
data.set.label.filenames <- c(train.label.filename, test.label.filename)

train.subject.filename <- paste(data.directory, "UCI HAR Dataset", "train", "subject_train.txt", sep="/")
test.subject.filename <- paste(data.directory, "UCI HAR Dataset", "test", "subject_test.txt", sep="/")
data.set.subject.filenames <- c(train.subject.filename, test.subject.filename)

##############################################################################
#
# Function: check_required_package_installed
#
# This function checks if the required package is installed and if not attempts
# to download and install it.
#
# Args:
# - package,name
#
# Returns:
# - TRUE if the package can be installed and loaded
# - FALSE if the package can not be loaded
#
##############################################################################

check_required_package_installed <- function (package.name) {
  if(!require(package.name, character.only = T)) {
    install.packages(package.name)
    if(!require(package.name, character.only = T)) {
      return(FALSE)
    }
  } 
  
  return(TRUE)
}

##############################################################################
#
# Function: check_required_packages_installed
#
# This function checks if all required packages can be loaded.
#
# Args:
# - list of required packages
#
# Returns:
# - TRUE: if all packages can be loaded
# - Error Message: if one or more packages cannot be loaded.
#
##############################################################################

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

##############################################################################
#
# Function: download_data_set
#
# This function downloads the data set from the internet and saves it locally.
#
# Args:
# - url: URL of the data set zip file
# - zip.file: zip file name
#
# Returns:
#
##############################################################################

download_data_set <- function(url, zip.file) {
  print("...... Start of downloading data set ...")
  download(url, zip.file)
  print("...... End of downloading data set.")
}

##############################################################################
#
# Function: unzip_data_set
#
# This function tests to see if the zip file has been previously downloaded
# and downloads it if it does not exist locally. The function unzipps the zip
# file in the directory specified.
#
# Args:
# - url: URL of the data set zip file
# - zip.file: zip file name
# - ext.dir: directory for extracting the zip file
#
# Returns:
# TRUE: if no problems downloading or unzipping the data set.
# FALSE: if problem downloading or unzipping the data set
#
##############################################################################

unzip_data_set <- function (url, zip.file, ext.dir) {
  print("... Start of unzipping data set ...")
  if(!file.exists(zip.file)) {
    download_data_set(url, zip.file)
    if(!file.exists(zip.file)) {
      return(FALSE)
    }
  }
  unzip(zip.file, exdir=ext.dir)
  print("... End of unzipping data set.")
  
  return(TRUE)
}

##############################################################################
#
# Function: load_and_filter_features
#
# This function loads the features files and keeps only the mean and standard
# deviation measures.
#
# Args:
# - filename: features file name
#
# Returns:
# data frame of selected features
#
##############################################################################

load_and_filter_features <- function(filename) {
  print("...... Start of load and filter features ...")
  
  # load the features file
  features.df <- read.table(filename, col.names=c("position", "feature"))
  
  # keep only the relevant features
  selected.features.df <- subset(features.df, grepl('-(mean|std)\\(', features.df$feature))
  
  # format selected features
  selected.features.df$feature <- tolower(selected.features.df$feature)
  selected.features.df$feature <- gsub("[-()]", "", selected.features.df$feature)
  
  print("...... End of load and filter features.")
  
  return(selected.features.df)
}

##############################################################################
#
# Function: load_activities
#
# This function loads the activity descriptions.
#
# Args:
# - filename: activity file name
#
# Returns:
# data frame of activity descriptors and ids
#
##############################################################################

load_activities <- function(filename) {
  print("...... Start of load activities ...")
  
  activities <- read.table(filename, col.names=c("id", "description"))
  
  print("...... End of load activities.")
  
  return(activities)
}

##############################################################################
#
# Function: join_and_filter_data_sets
#
# This function joins the different data sets including the subject ids and 
# activity descriptions. Also, the function keeps only the relevant features
# outlined in the features argument.
#
# Args:
# - data.filenames: list of data set filenames
# - label.filenames: list of data set label filenames
# - subject.filenames: list of data set subject filenames
# - features: list of feature descritors and ids
# - activities: list of activities and ids
#
# Returns:
# data frame of joined and filtered UCI HAR data
#
##############################################################################

join_and_filter_data_sets <- function(data.filenames, 
                                      label.filenames,
                                      subject.filenames,
                                      features, 
                                      activities) {
  print("...... Start of join and filter data sets ...")
  
  df.final = NULL
  
  for(i in 1:length(data.filenames)) {
    df.1 <- read.table(data.filenames[i])
    df.1 <- df.1[, features$position]
    colnames(df.1) <- features$feature
    
    df.2 <- read.table(label.filenames[i], col.names=c("id"))
    df.2 <- left_join(df.2, activities)
    colnames(df.2)[2]  = "label"
    
    df.3 <- read.table(subject.filenames[i], col.names=c("subjectid"))

    df <- cbind(df.3, df.1, df.2)
    df <- df[, -which(colnames(df) == "id")]
    
    if(is.null(df.final)) {
      df.final = df
    } else {
      df.final <- rbind(df.final, df)
    }
  }
  
  df.final$label <- as.factor(tolower(df.final$label))
  df.final$subjectid <- as.factor(df.final$subjectid)

  print("...... End of join and filter data sets.")
  return(df.final)
}

##############################################################################
#
# Function: calculate_means_activities_subjects
#
# This function calculates the averages for the activities and subjects.
#
# Args:
# - df: tidy data frame of UCI HAR data
#
# Returns:
# tidy data frame of average measurements for activities and subjects
#
##############################################################################

calculate_means_activities_subjects <- function (df) {
  print("...... Start of calculate means for activities and subjects ...")
  
  result <- aggregate(df[, 2:67], list(df$label, df$subjectid), mean)
  colnames(result)[1:2] = c("label", "subjectid")
  result <- result[order(c(result$label, result$subjectid)),]
  
  print("...... End of calculate means for activities and subjects.")
  
  return(result)
}

##############################################################################
#
# Function: run_analysis
#
# This function is the mainline for the analyisis of the UCI HAR data. It
# checks for required packages to be loaded and loads supporting meta-data
# related to features, activity labels, and test subject ids. The function then
# merges all the different data sets including subject data and activity labels.
# Once a tidy data set is generated, another tidy data set is created that
# calculates the average for activities and subjects. Both tidy data sets are
# written locally.
#
# Args:
#
# Returns:
#
##############################################################################

run_analysis <- function () {
  print("Start of analysis...")
  
  # check required packages installed
  error.message <- check_required_packages_installed(required.packages)
  if (error.message != TRUE) {
    print(error.message)
    return(FALSE)
  }
  
  # download and unzip the data set
  error.message <- unzip_data_set(data.set.url, zip.filename, data.directory)
  if (error.message == FALSE) {
    print("ERROR: Cannot either download or load the data set.")
    return(FALSE)
  }
  
  # load and filter features in data set to include mean and standard deviation
  # for each measurement
  features <- load_and_filter_features(features.filename)
  
  # load activity labels
  activities <- load_activities(activities.filename)

  # join and tidy the data sets into one data set
  final.1.df <- join_and_filter_data_sets(data.set.filenames,
                                          data.set.label.filenames,
                                          data.set.subject.filenames,
                                          features,
                                          activities)
  
  # create tidy data set of the averages for each activity and subject
  final.2.df <- calculate_means_activities_subjects(final.1.df)

  # write output
  print("...... Start of writing output files ...")
  write.table(final.1.df, file=data.set.output.1.filename, row.names=F)
  write.table(final.2.df, file=data.set.output.2.filename, row.names=F)
  print("...... End of writing output files.")
  
  print("... End of analysis.")
}

# run the analysis
run_analysis()