
###
##Part a)

#Loading the green_tripdata_2015-09 dataset into taxi_df

library(data.table)

taxi_df <- fread("https://s3.amazonaws.com/nyc-tlc/trip+data/green_tripdata_2015-09.csv")


##Part b)

# Reporting number of rows and columns in the dataframe taxi_df

number_of_row=nrow(taxi_df)

number_of_column=ncol(taxi_df)

### DATA CLEANING

min(taxi_df$Trip_distance)
# We can observe that the minimum value of trip distance is 0 which can not be possible, hence we will remove all those
#values from our dataframe for which we have trip distance as 0.

min(taxi_df$Fare_amount)
# The minimum value of fare amount is -475, it shows that there are certain values for which the fare amount is negative. We will remove 
#all those values from our dataframe.

min(taxi_df$Tip_amount)
# The minimum value of fare amount is -50, it shows that there are certain values for which the tip amount is negative. We will remove 
#all those values from our dataframe.

min(taxi_df$Total_amount)
# The minimum value of total amount is also -475, it shows that there are certain values for which the total amount is negative. We will remove 
#all those values from our dataframe.

#Now we will filter our dataframe based on these values which are mentioned above

#nrow(taxi_df)
#min(taxi_df$Tip_amount)

taxi_df<-taxi_df[with(taxi_df, Trip_distance >0), ]
taxi_df<-taxi_df[with(taxi_df, Fare_amount >=2.5), ]
taxi_df<-taxi_df[with(taxi_df, Tip_amount >=0), ]

nrow(taxi_df)

###
##Part a)

#Plotting a Histogram using ggplot

install.packages("ggplot2")
library("ggplot2")

#dev.off()---use this function in case ggplot does not work after loading

ggplot(data=taxi_df, aes(taxi_df$Trip_distance))+
  geom_histogram( binwidth =1,color = "red", fill = "blue") + xlim(0, 30)+ labs(title="Histogram for Trip distance", x="Distance", y="Count")

ggplot(data=taxi_df, aes(taxi_df$Trip_distance))+
  geom_density( color = "red", fill = "blue" ) + xlim(0, 30)+ labs(title="Density plot for Trip distance", x="Distance", y="Count")


##Part b)

#The Histogram obtained for "Trip distance" is right skewed. It can be observed that
#the mean is larger than the median.

#Hypothesis:The trips are not normally distributed hence they cannot be random. It seems that maximum
#number of trips happen at certain time i.e. during peak hours.


###
##Part a)

# Fetching hours from pickup time and adding these hours into a new column "hours" of taxi_df.
#NOTE: The 0th hour is corresponding to 12 AM

pickup_time <- as.POSIXlt(taxi_df$lpep_pickup_datetime)
taxi_df$hours<-pickup_time$hour

#Making 2 new columns which have values of trip day and trip week according to month

taxi_df$day_of_trip=pickup_time$mday

#I am assuming that a week is from Sunday to Saturday

taxi_df$week_of_month[taxi_df$day_of_trip >=1 & taxi_df$day_of_trip <= 5]<-1
taxi_df$week_of_month[taxi_df$day_of_trip >=6 & taxi_df$day_of_trip <= 12]<-2
taxi_df$week_of_month[taxi_df$day_of_trip >=13 & taxi_df$day_of_trip <= 19]<-3
taxi_df$week_of_month[taxi_df$day_of_trip >=20 & taxi_df$day_of_trip <= 26]<-4
taxi_df$week_of_month[taxi_df$day_of_trip >=27 & taxi_df$day_of_trip <= 30]<-5

#calculating and plotting mean and median by hours

mean_trip_distance= aggregate(taxi_df$Trip_distance, list(hours=taxi_df$hours), mean)
plot(mean_trip_distance,col = 'blue', pch = 19, cex = 0.5,main = "Mean distance based on hour",
     xlab = "Hours",
     ylab = "Mean",type='l')

median_trip_distance= aggregate(taxi_df$Trip_distance, list(hours=taxi_df$hours), median)
plot(median_trip_distance,col = 'red', pch = 19, cex = 0.5,main = "Median distance based on hour",
     xlab = "Hours",
     ylab = "Median",type='l')

##Part b)

#creating a new column which has the value 'Yes' if the ratecodeid is 2(for JFK) or 3(for Newark) and 'No' for rest of the values.

taxi_df$airport_pick_or_drop[taxi_df$RateCodeID==2 | taxi_df$RateCodeID==3]<-'Yes'
taxi_df$airport_pick_or_drop[taxi_df$RateCodeID==1 | taxi_df$RateCodeID==4 | taxi_df$RateCodeID==5 | taxi_df$RateCodeID==6]<-'No'

library(plyr)
count(taxi_df$airport_pick_or_drop)

# using count from library 'plyr' we can observe that a total of 4280 trips originated or ended at new york airports


average_fair= aggregate(taxi_df$Fare_amount, list(taxi_df$airport_pick_or_drop),mean,drop=FALSE)

#the average_fair from or to airports is 53.24755

average_tip_amount= aggregate(taxi_df$Tip_amount, list(taxi_df$airport_pick_or_drop),mean,drop=FALSE)

#average tip for trips from and to airports is 5.395044


###

##Part a)


#Calculating tip percent and storing all those values in a new column 'taxi_percent' in taxi_df

taxi_df$tip_percent=round(100*taxi_df$Tip_amount/taxi_df$Total_amount,2)

##Part b)

# Building a model which predict the tip percentge based on the values of Total amount, Trip duration and Trip distance

# Calculating the total duration of the trip in minutes

pickup_time = as.POSIXct(taxi_df$lpep_pickup_datetime,format='%Y-%m-%d %H:%M:%S')
drop_time= as.POSIXct(taxi_df$Lpep_dropoff_datetime,format='%Y-%m-%d %H:%M:%S')
taxi_df$trip_duration= round((drop_time-pickup_time)/60,2)
taxi_df$trip_duration= as.numeric(taxi_df$trip_duration)

# Now as few of the values in trip duration were 0 We will remove these values from our dataset.

taxi_df<-taxi_df[with(taxi_df, trip_duration >0), ]

#Creating a traning and testing data set for model prediction

#Training data set having 1000000 number of rows

train_data=taxi_df[1:1000000]

# Testing data set having rest of the rows from taxi_df

test_data=taxi_df[1000001:1468366]

#creating a variable test count which contains value of tip percent from testing dataset
test_counts= test_data$tip_percent

#Using lm() for prediction
prediction_model = lm(train_data$tip_percent ~ train_data$Total_amount + train_data$trip_duration + train_data$Trip_distance , data = train_data)

summary(prediction_model)
# Looking at the values obatined from Summary function we can obsereve how the value of tip percent changes when we use total amount,
#trip duration and trip distance as predictor variables.

plot(train_data$tip_percent ~ train_data$Total_amount + train_data$trip_duration + train_data$Trip_distance )

#Predicting the model
p = predict(prediction_model, test_data)

#Plotting the predicted model
plot(p - test_counts)

#Also plotting a histogram to showcase the tip percentage

ggplot(data=taxi_df, aes(taxi_df$tip_percent)) + 
  geom_histogram(bins=50)+labs( x = "Tip in Percentage", y = "count")


###

##Part a)

#calculating the average speed in miles per hour and adding that value in our dataframe

taxi_df$average_speed = round(taxi_df$Trip_distance/(taxi_df$trip_duration/60),2)

#Now as the maximum speed should be below or equal to 25mph in NYC "http://nymag.com/daily/intelligencer/2014/11/things-to-know-about-nycs-new-speed-limit.html".
#We will clean the data based on the average speed.

taxi_df<-taxi_df[with(taxi_df, average_speed <= 25), ]

##Part b)

#HYPOTHESIS TESING

#Finding the averrage speed according to the week of month

average_speed_weekwise= aggregate(taxi_df$average_speed, list(week=taxi_df$week_of_month),mean)

#Hypothesis Testing using Paired student t-test. We are using this method as we are concerned with average speeds for each week.

#Let's define null and alternate hypothesis

#Null Hypothesis(H0): Average speed for each week is same and true difference in mean is zero for both samples.

#Alternate Hypothesis(H1): Average speeds are different for different week and true difference is not zero for the samples.

#making samples of 1st 100 elements from each week

week1_sample=taxi_df$average_speed[taxi_df$week_of_month==1][1:100]
week2_sample=taxi_df$average_speed[taxi_df$week_of_month==2][1:100]
week3_sample=taxi_df$average_speed[taxi_df$week_of_month==3][1:100]
week4_sample=taxi_df$average_speed[taxi_df$week_of_month==4][1:100]
week5_sample=taxi_df$average_speed[taxi_df$week_of_month==5][1:100]

#performing Hypothesis testing for week1 and week2 sample
t.test(week1_sample,week2_sample, paired=TRUE)

#performing Hypothesis testing for week1 and week3 sample
t.test(week1_sample,week3_sample, paired=TRUE)

#performing Hypothesis testing for week1 and week4 sample
t.test(week1_sample,week4_sample, paired=TRUE)

#performing Hypothesis testing for week1 and week5 sample
t.test(week1_sample,week5_sample, paired=TRUE)

#performing Hypothesis testing for week2 and week3 sample
t.test(week2_sample,week3_sample, paired=TRUE)

#performing Hypothesis testing for week2 and week4 sample
t.test(week2_sample,week4_sample, paired=TRUE)

#performing Hypothesis testing for week2 and week5 sample
t.test(week2_sample,week5_sample, paired=TRUE)

#performing Hypothesis testing for week3 and week4 sample
t.test(week3_sample,week4_sample, paired=TRUE)

#performing Hypothesis testing for week3 and week5 sample
t.test(week3_sample,week5_sample, paired=TRUE)

#performing Hypothesis testing for week4 and week5 sample
t.test(week4_sample,week5_sample, paired=TRUE)

#After performing all the above mentioned tests it is evident that p-value for all the test is greater than 0.05 and hence
# we can REJECT NULL HYPOTHESIS in favor of Alternative Hypothesis.

#Conclusion: The average speeds of NYC green taxi is different for each week.

# Plotting a boxplot for the average speed based on each week.

boxplot(taxi_df$average_speed~taxi_df$week_of_month,data=taxi_df, col=(c("gold","darkgreen")),main="Boxplot for average speed on week", 
        xlab="Week", ylab="Speed in miles per hour")


#part c)

#Plotting a boxplot for the average speed based on each hour of day

boxplot(taxi_df$average_speed~taxi_df$hours,data=taxi_df, col=(c("gold","darkgreen")),main="Boxplot for average speed on hours", 
        xlab="Hours per day", ylab="Speed in miles per hour")

#HYPOTHESIS TESTING

#Hypothesis Testing using One-way Analysis of Variance (ANOVA). We are using this method as we are concerned with average speeds for each hour.

#Let's define null and alternate hypothesis

#Null Hypothesis(H0): Average speed for each hour is same and true difference in mean is zero.

#Alternate Hypothesis(H1): Average speeds are different for different hour and true difference is not zero for the samples.

model_on_hours <- lm(taxi_df$average_speed~taxi_df$hours,data=taxi_df)

summary(model_on_hours)

anova(model_on_hours)

#Using the ANOVA test it is clear that the speeds are different for each hour. Hence we will reject Null Hypothesis in favor of
#Alertnate Hypothesis.

#Using the boxplot we can also infer that the speed is maximum at 5 AM and gradually decreases from there. The speed starts increasing slowly 
#after 6 PM.


######## THE END
