########Final Project########

# This file export the first and last 8th of the acoustic_data column to csv's

# Load working directory
setwd("C:/Users/ADRC/Documents/MATH 189")

#Load dependencies
library(data.table)

#Load original data File
df<-fread("train.csv")

#Remove existing CSVs
file.remove("train_X_1.csv")
file.remove("train_X_2.csv")
file.remove("train_X_1.csv")
file.remove("train_X_2.csv")

# acoustic_data
#Separate into two parts: First and Last 8th
df2<-df[1:(nrow(df)/8),1]
df3<-df[(nrow(df)-nrow(df2)):nrow(df),1]
fwrite(df2, "train_X_1.csv")
rm("df2")
fwrite(df3, "train_X_2.csv")
rm("df3")

# time_to_failure
#Separate into two parts: First and Last 8th
df2<-df[1:(nrow(df)/8),2]
df3<-df[(nrow(df)-nrow(df2)):nrow(df),2]
fwrite(df2, "train_Y_1.csv")
rm("df2")
fwrite(df3, "train_Y_2.csv")
rm("df3")

