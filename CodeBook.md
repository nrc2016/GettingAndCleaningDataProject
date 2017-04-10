---
title: "CodeBook"
output: html_document
---

# Getting and Cleaning Data (Coursera Data Science Specialization) Project Codebook

## Original Data

Fitness tracking companies like FitBit, Nike, and Jawbone Up are interested in developing data analytic capabilites to provide value added services to their customers. The data set used for the course project was collected by a Samsung Galaxy S II for the purposes of identifying six activities, walking, walking up stairs, walking downstairs, sitting, standing, and laying. Accelerometer and gyroscope data was collected and processed into two different types of data sets, training and testing. Each of these types have three files: subject id, activity label, and phone measurements and each of these three files contain the same number of observations. For more information on the data set see the features_info.txt in the zipped file.


## Data Sets

### Raw Data Set

The activity_labels.txt meta data file contained the ids and descriptors for activities listed above. The features_info.txt also contained meta data for column position and labels for features in the phone measurement data. This was used to identify relevant features to keep in the desired data set. Only mean and standard deviation features were identified using the following regular expression:

-(mean|std)\\(

The regular expression matched 66 of the original 561 featres and the matching features were kept along with two other additional features that were created. The first additional feature was a text label for the activity type and the second additional feature was a text type for identifying the subject identity.

Finally, the two types of data, training and testing, were merged together to form one data set. The resulting data set was formatted as described below.

### Tidy Data Set 1

The joined data set was further formatted in the following ways:

- feature names were converted to lower case
- special characters, ",", "(", and ")" were removed from the feature names
- label and subjectid feature values were converted to factors

This tidy joined data set was used below in calculating the averages for activities and subjects in the experiment.

### Tidy Data Set 2

After the averages for the identified measurements were calculated for the different activities and subject grouping, the following formatting was performed:

- grouping features were changed to original feature names, label and subjectid
- data set was ordered according to activity then subject id.

