########Final Project########

# This file export runs a chi square test to check for normality of acoustic
# signal distribution 

#Set Working Directory 
setwd("C:/Users/ADRC/Documents/MATH 189")

#Set memory limit
memory.limit(size=56000)

#Load dependencies
library(data.table)
library(ggplot2)
library(matrixStats)
library(gridExtra)
library(speedglm)

# Load  first dataset 
df1<-fread("train_X_1.csv")

# Plot histogram 
ggplot(df1, aes(x=acoustic_data)) + geom_histogram(binwidth=1) +xlim(-20,20)


######################
#Tests for Normality##
######################

# Chi square test
#Put into proportion table first 

#Create vector of culmulative proababilities for quantile
probability<-c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)

#Obtain quantiles 
a<-quantile(df1$acoustic_data, probability)
b<-as.numeric(a[1:length(a)])

#Create proportion table
proportion_table<-as.table(as.numeric(table(cut(df1$acoustic_data, breaks=c(-Inf,0,2,3,4,5,6,7,9, Inf)))))

#Perform Chi Square Goodness of Fit
chisq.test(proportion_table, p=c(0.1, 0.1, 0.1, 0.1, 0.2, 0.1, 0.1, 0.1, 0.1))

#The data does not follow a normal distribtion 



# K-S Test 
#Generate Normal distributed numbers
normal_generates<-rnorm(nrow(df1), mean(df1$acoustic_data), sd(df1$acoustic_data))

#Perform KS test 
ks.test(df1$acoustic_data, normal_generates)
#P Value is low, so not normal




# Shapiro Wilks test for normality
#Pull 5000 samples to allow the test to work 
ad_sample<-sample(df1$acoustic_data, replace=F, 5000)
shapiro.test(ad_sample)
# Not normal becuase p-values are very small 






##################################
#Flag the values that are extreme#
##################################

#Use 3*sD to filter out extreme vaues 
sd<-sd(df1$acoustic_data)
df1$spike<-ifelse(df1$acoustic_data >= 3*sd |df1$acoustic_data <= -3*sd, 1, 0)

subset_nospikes<-subset(df1$acoustic_data,df1$spike==0)
# Plot without spikes
ggplot(subset(df1, df1$spike==0), aes(x=acoustic_data)) + geom_histogram(binwidth=1) +xlim(-35,35) + xlab("Acoustic Signal") +
  ggtitle("Distribution of Acoustic Signals") + theme(plot.title = element_text(hjust = 0.5))
  





#Tests for Normality#


# Chi square test
#Put into proportion table first 

#Create vector of culmulative proababilities for quantile
probability<-c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)

#Obtain quantiles 
a<-quantile(subset_nospikes, probability)
b<-as.numeric(a[1:length(a)])

#Create proportion table
proportion_table<-as.table(as.numeric(table(cut(subset_nospikes, breaks=c(-Inf,0,2,3,4,5,6,7,9, Inf)))))

#Perform Chi Square Goodness of Fit
chisq.test(proportion_table, p=c(0.1, 0.1, 0.1, 0.1, 0.2, 0.1, 0.1, 0.1, 0.1))

#The data does not follow a normal distribtion 



# K-S Test 
#Generate Normal distributed numbers
normal_generates<-rnorm(length(subset_nospikes), mean(subset_nospikes), sd(subset_nospikes))

#Perform KS test 
ks.test(subset_nospikes, normal_generates)
#P Value is low, so not normal




# Shapiro Wilks test for normality
#Pull 5000 samples to allow the test to work 
ad_sample<-sample(subset_nospikes, replace=F, 5000)
shapiro.test(ad_sample)
# Not normal becuase p-values are very small 







############################################################
#Let's look at the corresponding times for the flagged ones#
############################################################


#Bind Corresponding Y data frame
df1<-fread("train_X_1.csv")
df2<-fread("train_Y_1.csv")
df3<-cbind(df1, df2)
rm("df2")
df2<-df3
rm("df3")
rm("df1")

# # Plot of times that are spiked
# spiked_times_p<-ggplot(subset(df2, df2$spike==1), aes(x=time_to_failure)) + geom_histogram(binwidth=0.1) +xlim(0, 1)
# 
# # Plot of times that are not spiked
# unspiked_times_p<-ggplot(subset(df2, df2$spike==0), aes(x=time_to_failure)) + geom_histogram()
# 
# grid.arrange(spiked_plot, unspiked_plot)





#############################################
#####Regression Analysis on Leading Times####
#############################################

#Window size = 4100

#Memory management
df1<-fread("train_X_1.csv")
#df2<-fread("train_Y_1.csv")
#df3<-cbind(df1, df2)
rm("df2")
#df2<-df3
rm("df3")
#rm("df1")

#Let's create a new data frame of the first n values and store it in df3
df3<-df1[1:10000000]

#Remove df2
rm("df1")
rm("df2")
#Sample every 10
df3<-df3[seq(1, nrow(df3), 10)]

#n is the number of rows in df3
n<-nrow(df3)

#Initialize dataframe in order to use it for regression 
#So we make j + 1 columns such that j is the window size (4100 in this case) and + 1 for the time to failure value
#The number of rows is n-(j-1) = n-j+1
j<-410

#Take subset from j+1 to end of df3
df3<-df3[411:nrow(df3)]

#Plot theses values 
# Bring in the y values 
df2<-fread("train_Y_1.csv")
plot_matrix<-cbind(df3, df2[411:n])

ggplot(plot_matrix, aes(x=acoustic_data, y=time_to_failure)) + geom_point() +
  ggtitle("Acoustic Signal vs Time to Failure") + ylab("Time to Failure (Seconds)") + xlab("Acoustic Signals") + theme(plot.title = element_text(hjust = 0.5))
ggsave("myplot.png")

#Change the vector into matrix form by row
df_4<-as.data.frame(matrix(df3$acoustic_data,nrow = 2420,ncol = j, byrow = TRUE))


#Remove df3
rm("df3")

#Add y column from j:n
df2<-fread("train_Y_1.csv")

#Sample every 10
df2<-df2[seq(1, nrow(df2), 10)]

#Take out the first 410 entries 
df2<-df2[411:nrow(df2)]

#Column bind it with the master data frame
df_4<-cbind(df_4, df2[1:nrow(df_4)])



#rename variable columns to be Xj
for(i in 1:j){eval(parse(text=paste0("
                                     colnames(df_4)[i] <- paste0('X','",i,"')
                                     ")))
}

#Rename the last column (dependent variable for regression)
colnames(df_4)[ncol(df_4)] <- "X411"



#Create formula for i:j
formula<-""
for(i in 1:j){eval(parse(text=paste0("
                                     formula<-paste0(formula, ' X','",i," +')
                                     ")))
}

#Remove unecessary characters
formula<-substring(formula, 2, nchar(formula)-2)

#Add dependent variable to formula X(j+1)
formula <- paste0("X411 ~ ", formula)

#Rename columns in dataframe before running regression 
lm<-lm(formula, data=df_4)

#Average residual
mean(resid(lm))

#Bind residuals into dataframe to plot 
df_4$lm_residual<-resid(lm)


#RESIDUAL PLOT
#Log
ggplot(df_4, aes(x=X411, y=lm_residual)) + geom_point() +
  ggtitle("Residual Plot") + ylab("Residuals") + xlab("Time to Failure (Seconds)") + theme(plot.title = element_text(hjust = 0.5))
  


#Clear memory 
rm("df_4")
rm("df2")



##### Create dataframe for prediction #####

#Memory management
df1<-fread("train_X_1.csv")

#Create prediction data frame
prediction_df<-df1[9999901:12500000]

#Sample every 10
prediction_df<-prediction_df[seq(1, nrow(prediction_df), 10)]

#Window size after sampling 
j<-410

#Take subset from j+1 to end of df3
prediction_df<-prediction_df[411:nrow(prediction_df)]

#Change the vector into matrix form by row
prediction_df<-as.data.frame(matrix(prediction_df$acoustic_data,nrow = 2420,ncol = j, byrow = TRUE))

#rename variable columns
for(i in 1:410){eval(parse(text=paste0("
                                       colnames(prediction_df)[i] <- paste0('X','",i,"')
                                       ")))
}

#Bring back time to failure for corresponding values 
df2<-fread("train_Y_1.csv")

df2<-df2[9999901:12500000]

#Sample every 10
df2<-df2[seq(1, nrow(df2), 10)]



#Now predict on this subset and get the MSE
#the range for the thing you're subtracting is n+4100:2*n
rm("df_4")
rm("df1")
mean((predict(lm, prediction_df) - df2$time_to_failure[1:2420])^2)
#79.99945



