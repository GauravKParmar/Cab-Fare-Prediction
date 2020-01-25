rm(list = ls(all=T))
setwd("C:/Users/GAURAV/Desktop/Edwisor/Projects/Cab Fare Prediction")
getwd()

list.of.packages = (c("geosphere","lubridate","corrplot","tree","randomForest","gbm"))

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

train<-read.csv("train_cab.csv",header=TRUE,
                colClasses=c("fare_amount"="character",
                             "pickup_datetime"="character",
                             "dropoff_longitude"="numeric",
                             "pickup_longitude"="numeric",
                             "dropoff_latitude"="numeric",
                             "pickup_latitude"="numeric",
                             "passenger_count"="numeric"),
                stringsAsFactors = F)

test = read.csv(file = "test.csv", header = TRUE,
                colClasses=c("pickup_datetime"="character",
                             "dropoff_longitude"="numeric",
                             "pickup_longitude"="numeric",
                             "dropoff_latitude"="numeric",
                             "pickup_latitude"="numeric",
                             "passenger_count"="numeric"),
                stringsAsFactors = F)

head(train)
head(test)

str(train)
str(test)

train$fare_amount = as.numeric(train$fare_amount)
train$pickup_datetime = as.character(train$pickup_datetime)
train$pickup_longitude = as.numeric(train$pickup_longitude)
train$pickup_latitude = as.numeric(train$pickup_latitude)
train$dropoff_longitude = as.numeric(train$dropoff_longitude)
train$dropoff_latitude = as.numeric(train$dropoff_latitude)
train$passenger_count = as.numeric(train$passenger_count)

test$pickup_datetime = as.character(test$pickup_datetime)
test$pickup_longitude = as.numeric(test$pickup_longitude)
test$pickup_latitude = as.numeric(test$pickup_latitude)
test$dropoff_longitude = as.numeric(test$dropoff_longitude)
test$dropoff_latitude = as.numeric(test$dropoff_latitude)
test$passenger_count = as.numeric(test$passenger_count)

apply(train, 2, function(x){sum(is.na(x))})
apply(test, 2, function(x){sum(is.na(x))})

train = na.omit(train)

summary(train)

#Counting negative fare values
sum(train$fare_amount < 0)

#Dropping negative fare values
train = train[!(train$fare_amount < 0),]

#Checking values of passenger count
table(train$passenger_count)
table(test$passenger_count)

# Assuming a cab has maximum 6 seats. So removing values above 6.
train = train[!(train$passenger_count > 6),]
# Since there are two values 1.3 and 0.12, we drop them as well.
train = train[!(train$passenger_count == 1.3),]
train = train[!(train$passenger_count == 0.12),]
# 0 passengers are outliers, as fare amount cannot be charged if there are no passengers.
train = train[!(train$passenger_count == 0),]

table(train$passenger_count)
table(test$passenger_count)

str(train)

boxplot(train$fare_amount)

table(train$fare_amount)

train = train[!(train$fare_amount > 400),]

boxplot(train$fare_amount)

# Latitude ranges from -90 to 90
# Longitude ranges from -180 to 180

train = train[!((train$pickup_latitude < -90) | (train$pickup_latitude > 90)),]
train = train[!((train$dropoff_latitude < -90) | (train$dropoff_latitude > 90)),]
train = train[!((train$pickup_longitude < -180) | (train$pickup_longitude > 180)),]
train = train[!((train$dropoff_longitude < -180) | (train$dropoff_longitude > 180)),]

test = test[!((test$pickup_latitude < -90) | (test$pickup_latitude > 90)),]
test = test[!((test$dropoff_latitude < -90) | (test$dropoff_latitude > 90)),]
test = test[!((test$pickup_longitude < -180) | (test$pickup_longitude > 180)),]
test = test[!((test$dropoff_longitude < -180) | (test$dropoff_longitude > 180)),]

summary(train)

library(geosphere)

train$haversine_distance = distHaversine(cbind(train$pickup_longitude, train$pickup_latitude), 
                                         cbind(train$dropoff_longitude, train$dropoff_latitude))/1000
test$haversine_distance = distHaversine(cbind(test$pickup_longitude, test$pickup_latitude),
                                        cbind(test$dropoff_longitude, test$dropoff_latitude))/1000


head(train)
head(test)

library(lubridate)

train$pickup_datetime = ymd_hms(train$pickup_datetime)
train$year = as.factor(year(train$pickup_datetime))
train$month = as.factor(month(train$pickup_datetime))
train$day = as.factor(day(train$pickup_datetime))
train$dayOfWeek = as.factor(wday(train$pickup_datetime))
train$hour = as.factor(hour(train$pickup_datetime))

test$pickup_datetime = ymd_hms(test$pickup_datetime)
test$year = as.factor(year(test$pickup_datetime))
test$month = as.factor(month(test$pickup_datetime))
test$day = as.factor(day(test$pickup_datetime))
test$dayOfWeek = as.factor(wday(test$pickup_datetime))
test$hour = as.factor(hour(test$pickup_datetime))

train$pickup_datetime = NULL
test$pickup_datetime = NULL

train = na.omit(train)

head(train)
head(test)

# Does number of passengers affect the fare?
plot(train$passenger_count, train$fare_amount)
#From the graph we can see that single passengers are the most frequent travellers, 
#and the highest fare also seems to come from cabs which carry just 1 passenger.

# Does the date and time of pickup  affect the fare?
plot(train$day, train$fare_amount)
#The fare throughout the month is mostly uniform.
plot(train$hour, train$fare_amount)

# Does the day of week affect the fare?
plot(train$dayOfWeek, train$fare_amount)
# Thursday has good amount of fare rides.

# Does the distance affect the fare?
train[((train$haversine_distance>200) & (train$fare_amount!=0)),]

# As you can see from the DF above, the abnormally high distances are due to 
# either the pickup or dropoff co-ordinates being incorrect or 0. 
# However, since all these values have fares, I do not wish to drop them as they contain crucial data. 
# Instead, I will replace the initial distance values with distance values calculated using the fare using the following formula
# distance = (fare_amount - 2.5)/1.56
# After looking the valid co-ordinates on google map, we find that the data is from U.S locations. 
# Google search gave me the following price for cab = 2.5(baseprice) + 1.56/km

train[((train$haversine_distance>200) & (train$fare_amount!=0)),]$haversine_distance = 
  apply(train[((train$haversine_distance>200) & (train$fare_amount!=0)),], 1, function(x) (as.numeric(x[["fare_amount"]])-2.50)/1.56)

# Rows where distance is 0 and fare is 0
train[((train$haversine_distance == 0) & (train$fare_amount == 0)),]

# Rows where distance is 0 and fare is less than $2.50.
train[((train$haversine_distance == 0) & (train$fare_amount < 2.50)),]

# Rows where distance is not 0 but fare is 0
train[((train$haversine_distance != 0) & (train$fare_amount == 0)),]
# There is only one row where distance is not 0 and fare is 0. Hence, imputing fare value using distance formula.
train[((train$haversine_distance != 0) & (train$fare_amount == 0)),]$fare_amount = 
  2.50 + 1.56 * train[((train$haversine_distance != 0) & (train$fare_amount == 0)),]$haversine_distance

# Rows where distance is 0 and fare is not 0
train[((train$haversine_distance == 0) & (train$fare_amount != 0)),]

# Lets impute them with distance formula.
train[((train$haversine_distance == 0) & (train$fare_amount != 0)),]$haversine_distance = 
  apply(train[((train$haversine_distance == 0) & (train$fare_amount != 0)),], 1, function(x) (as.numeric(x[["fare_amount"]])-2.50)/1.56)

# Lets check if still we have 0 distance values.
train[train$haversine_distance==0,]

#From table we can see distance value is 0 and fare is 2.5, because the pickup and dropoff coordinates are same or they are 0.
#Hence we will remove them from the data.
train = train[!(train$haversine_distance==0),]

#Deleting from test data as well.
test = test[!(test$haversine_distance==0),]

#Univariate Analysis
hist(train$fare_amount, breaks = 200, main = "Fare Amount Distribution", xlab = "Fare Amount")
hist(train$pickup_latitude, main = "Pickup Latitude Distribution", xlab = "Pickup Latitude")
hist(train$pickup_longitude, main = "Pickup Longitude Distribution", xlab = "Pickup Longitude")
hist(train$dropoff_latitude, main = "Dropoff Latitude Distribution", xlab = "Dropoff Latitude")
hist(train$dropoff_longitude, main = "Dropoff Longitude Distribution", xlab = "Dropoff Longitude")
hist(train$haversine_distance, breaks = 100, main = "Distance Distribution", xlab = "Distance")

#Bivariate Analysis
plot(train$year, train$fare_amount)
# Based on the scatterplot, in year 2009 and 2013 there were rides which got high fare_amount and very low on year 2015
plot(train$month, train$fare_amount)
# Based on the scatterplot,We can see Jan month fare amount is very high and low in July month.
plot(train$dayOfWeek, train$fare_amount)
# Based on the scatterplot,We can see that Tuesday ride has the highest fare_amount
plot(train$hour, train$fare_amount)
# Rides taken during 8 pm to 9 pm gives highest fare_amount
plot(train$passenger_count, train$fare_amount)
# Single passengers are frequent travellers
plot(train$haversine_distance, train$fare_amount)
# We can see as the distance increases fare amount also increases

#Feature Selection
str(train)
str(test)

#Since we have calculated the distance we can drop all the latitudes and longitudes
train[c("pickup_longitude","pickup_latitude","dropoff_longitude","dropoff_latitude")] = NULL
test[c("pickup_longitude","pickup_latitude","dropoff_longitude","dropoff_latitude")] = NULL

numeric = c('fare_amount', 'passenger_count', 'haversine_distance')
categorical = c("year","month", "day", "dayOfWeek", "hour")

corr = round(cor(train[numeric]), 2)

library(corrplot)
corrplot(corr, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)

# Anova Test

# Anova Test is performed between categorical independent variables & fare_amount(continuous target variable) 
summary(aov(fare_amount~year, data = train))
summary(aov(fare_amount~month, data = train))
summary(aov(fare_amount~day, data = train))
summary(aov(fare_amount~dayOfWeek, data = train))
summary(aov(fare_amount~hour, data = train))

# From the anova result, we can observe Day ,DayOfWeek
# has p value > 0.05, so delete these variables not consider in model.

train$day = NULL
train$dayOfWeek = NULL
test$day = NULL
test$dayOfWeek = NULL

# Feature Scaling
library(ggplot2)
# Consider distance variable
ggplot(train, aes(x = haversine_distance)) + geom_density(fill="blue")
ggplot(test, aes(x = haversine_distance)) + geom_density(fill="blue")
# The distance variable is right skewed we can reduce this skewness using log transformation
train$haversine_distance = log(train$haversine_distance)
test$haversine_distance = log(test$haversine_distance)
# After log transformation
ggplot(train, aes(x = haversine_distance)) + geom_density(fill="blue")
ggplot(test, aes(x = haversine_distance)) + geom_density(fill="blue")

# Lets check for fare amount variable as well
ggplot(train, aes(x = fare_amount)) + geom_density(fill="blue")
# The fare amount variable is right skewed as well.
train$fare_amount = log(train$fare_amount)
# After log transformation
ggplot(train, aes(x = fare_amount)) +  geom_density(fill="blue")
# We can now observe after applying log function, the data is normalized.


# Model Development
set.seed(42)
train_index = sample(1:nrow(train),0.8*nrow(train))

X = train[train_index,]
y = train[-train_index,]

####### Linear Regression ########
lm_model = lm(fare_amount ~., data = X)
summary(lm_model)

predictions_LR = predict(lm_model, y[,-1])
rmse_LR = sqrt(mean((y$fare_amount-predictions_LR)^2))
rmse_LR
r_squared_LR = summary(lm_model)$adj.r.squared
r_squared_LR

####### Decision Tree #########
require(tree)
dt_model = tree(fare_amount~., data = X)
summary(dt_model)

predictions_DT = predict(dt_model, y[,-1])
rmse_DT = sqrt(mean((y$fare_amount-predictions_DT)^2))
rmse_DT
r_squared_DT = 1 - sum((y$fare_amount-predictions_DT)^2)/sum((y$fare_amount-mean(y$fare_amount))^2)
r_squared_DT

####### Random Forest #########
library(randomForest)
rf_model = randomForest(fare_amount~., data = X, importance=TRUE, ntree=100)

predictions_RF = predict(rf_model, y[,-1])
rmse_RF = sqrt(mean((y$fare_amount-predictions_RF)^2))
rmse_RF
r_squared_RF = 1 - sum((y$fare_amount-predictions_RF)^2)/sum((y$fare_amount-mean(y$fare_amount))^2)
r_squared_RF

####### Gradient Boosting ########
library(gbm)
gb_model = gbm(fare_amount~., data = X, n.trees = 500, interaction.depth = 2)

predictions_GB = predict(gb_model, y[,-1], n.trees = 500)
rmse_GB = sqrt(mean((y$fare_amount-predictions_GB)^2))
rmse_GB
r_squared_GB = 1 - sum((y$fare_amount-predictions_GB)^2)/sum((y$fare_amount-mean(y$fare_amount))^2)
r_squared_GB

########## Summary of all models ########
models = c("Linear Regression", "Decision Tree", "Random Forest", "Gradient Boosting")
RMSE = c(rmse_LR, rmse_DT, rmse_RF, rmse_GB)
r2 = c(r_squared_LR, r_squared_DT, r_squared_RF, r_squared_GB)

results = data.frame(models, RMSE, r2)
results

########## Prediction of fare from provided test dataset #############
#We have already cleaned and processed our test dataset along with our training dataset. 
#Hence we will be predicting using gradient boosting model

gb_model = gbm(fare_amount~., data = train, n.trees = 500, interaction.depth = 2)

fare_predictions = predict(gb_model, test, n.trees = 500)
test$predicted_fares = fare_predictions
test

write.csv(x = test, file = "test_R.csv", row.names = F)
