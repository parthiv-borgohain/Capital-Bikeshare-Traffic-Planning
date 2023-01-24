library(ggplot2)
library(tree)
library(kknn)
library(MASS)
# install.packages('Hmisc')
library(Hmisc)
library(gtable)
library(gridExtra)
# Let's read the trip data CSV -

trip_data_2011_2012 <- read.csv('data/hour.csv')

# Let's factorize all required columns first

trip_data_2011_2012$season <- as.factor(trip_data_2011_2012$season)
trip_data_2011_2012$yr <- as.factor(trip_data_2011_2012$yr)
trip_data_2011_2012$mnth <- as.factor(trip_data_2011_2012$mnth)
trip_data_2011_2012$hr <- as.factor(trip_data_2011_2012$hr)
trip_data_2011_2012$holiday <- as.factor(trip_data_2011_2012$holiday)
trip_data_2011_2012$weekday <- as.factor(trip_data_2011_2012$weekday)
trip_data_2011_2012$workingday <- as.factor(trip_data_2011_2012$workingday)
trip_data_2011_2012$weathersit <- as.factor(trip_data_2011_2012$weathersit)


# Splitting data into year 2011 and 2012 datasets (used in Exploratory Data Analysis)
trip_data_2011 <- trip_data_2011_2012[trip_data_2011_2012$yr==0,]
trip_data_2012 <- trip_data_2011_2012[trip_data_2011_2012$yr==1,]


#Let's look at a summary
Hmisc::describe(trip_data_2011_2012)

# From this summary, we can see no values are missing in any of the columns

head(trip_data_2011_2012)

# Let's first look at distribution of rides per season per type of user

# Calculate means to be displayed on boxplot
means_agg_2011 <- aggregate(registered ~  season, trip_data_2011, mean)
means_cas_2011 <- aggregate(casual ~  season, trip_data_2011, mean)

means_agg_2012 <- aggregate(registered ~  season, trip_data_2012, mean)
means_cas_2012 <- aggregate(casual ~  season, trip_data_2012, mean)
# Round off the means
means_agg_2011$registered <- round(means_agg_2011$registered, 2)
means_cas_2011$casual <- round(means_cas_2011$casual, 2)

means_agg_2012$registered <- round(means_agg_2012$registered, 2)
means_cas_2012$casual <- round(means_cas_2012$casual, 2)



# For 2011

plt_2011_season_reg <- ggplot(data = trip_data_2011, aes(x=season, y=registered, fill=season)) +
  geom_boxplot(outlier.colour="red", outlier.shape=16,
               outlier.size=1, notch=FALSE) +
  geom_text(data = means_agg_2011, aes(label = registered)) +
  scale_fill_manual(values=c("#FEF8FA", "#D6F1C6","#C9F1FD","#F9CC87"))

plt_2011_season_reg


plt_2011_season_cas <- ggplot(data = trip_data_2011, aes(x=season, y=casual, fill=season)) +
  geom_boxplot(outlier.colour="red", outlier.shape=16,
               outlier.size=1, notch=FALSE)+
  geom_text(data = means_cas_2011, aes(label = casual)) +
  scale_fill_manual(values=c("#FEF8FA", "#D6F1C6","#C9F1FD","#F9CC87"))

plt_2011_season_cas



# For 2012


plt_2012_season_reg <- ggplot(data = trip_data_2012, aes(x=season, y=registered, fill=season)) +
  geom_boxplot(outlier.colour="red", outlier.shape=16,
               outlier.size=1, notch=FALSE) +
  geom_text(data = means_agg_2012, aes(label = registered)) +
  scale_fill_manual(values=c("#FEF8FA", "#D6F1C6","#C9F1FD","#F9CC87"))
plt_2012_season_reg


plt_2012_season_cas <- ggplot(data = trip_data_2012, aes(x=season, y=casual, fill=season)) +
  geom_boxplot(outlier.colour="red", outlier.shape=16,
               outlier.size=1, notch=FALSE)+
  geom_text(data = means_cas_2012, aes(label = casual)) +
  scale_fill_manual(values=c("#FEF8FA", "#D6F1C6","#C9F1FD","#F9CC87"))

plt_2012_season_cas


grid.arrange(plt_2011_season_reg, plt_2011_season_cas, plt_2012_season_reg, plt_2012_season_cas, nrow=2, ncol=2)



#ggplot defines an outlier by default as something that's > 1.5*IQR from the borders of the box.


# Let's look at the day of the week wise distribution of rides of casuals and registered 
# users over different seasons

#get only season, day of week and no. of rides

season_1 <- trip_data_2011_2012[trip_data_2011_2012$season=='1',][c('yr','season', 'weekday', 'casual','registered')]
season_2 <- trip_data_2011_2012[trip_data_2011_2012$season=='2',][c('yr','season', 'weekday', 'casual','registered')]
season_3 <- trip_data_2011_2012[trip_data_2011_2012$season=='3',][c('yr','season', 'weekday', 'casual','registered')]
season_4 <- trip_data_2011_2012[trip_data_2011_2012$season=='4',][c('yr','season', 'weekday', 'casual','registered')]




# Winter 

s1_reg <- ggplot(data=aggregate(registered ~  weekday, season_1, mean), aes(x=weekday, y=registered)) +
  geom_point(stat='identity') +
  geom_line(aes(group='weekday'),size=1, linetype='dotdash')
#s1_reg


s1_cas <- ggplot(data=aggregate(casual ~  weekday, season_1, mean), aes(x=weekday, y=casual)) +
  geom_point(stat='identity') +
  geom_line(aes(group='weekday'),size=1, linetype='dotdash')
#s1_cas



# Spring
s2_reg <- ggplot(data=aggregate(registered ~  weekday, season_2, mean), aes(x=weekday, y=registered)) +
  geom_point(stat='identity') +
  geom_line(aes(group='weekday'),size=1, linetype='dotdash')
#s2_reg


s2_cas <- ggplot(data=aggregate(casual ~  weekday, season_2, mean), aes(x=weekday, y=casual)) +
  geom_point(stat='identity') +
  geom_line(aes(group='weekday'),size=1, linetype='dotdash')
#s2_cas



# Summer
s3_reg <- ggplot(data=aggregate(registered ~  weekday, season_3, mean), aes(x=weekday, y=registered)) +
  geom_point(stat='identity') +
  geom_line(aes(group='weekday'),size=1, linetype='dotdash')
#s3_reg


s3_cas <- ggplot(data=aggregate(casual ~  weekday, season_3, mean), aes(x=weekday, y=casual)) +
  geom_point(stat='identity') +
  geom_line(aes(group='weekday'),size=1, linetype='dotdash')
#s3_cas



# Fall
s4_reg <- ggplot(data=aggregate(registered ~  weekday, season_4, mean), aes(x=weekday, y=registered)) +
  geom_point(stat='identity') +
  geom_line(aes(group='weekday'),size=1, linetype='dotdash')
#s4_reg


s4_cas <- ggplot(data=aggregate(casual ~  weekday, season_4, mean), aes(x=weekday, y=casual)) +
  geom_point(stat='identity') +
  geom_line(aes(group='weekday'),size=1, linetype='dotdash')
#s4_cas

grid.arrange(s1_reg, s1_cas, s2_reg, s2_cas, s3_reg, s3_cas, s4_reg, s4_cas, nrow=4, ncol=2)

# Reset plot grid
par(mfrow=c(1, 1))

# Enough variation to consider predicting them separately


# Let's do a simple lin reg on predicting reg rides based on casual rides




regCas <- lm(registered~casual, data = trip_data_2011_2012)
summary(regCas)

plot(trip_data_2011_2012$casual,trip_data_2011_2012$registered)
abline(regCas)




# One hot encoding
# We need to encode all categorical predictors (One Hot Encoding) so that we can pass them as 
# inputs to our regression models
# install.packages(c("mltools","data.table"))

library(mltools)
library(data.table)

# Encoding all predictors but removing the first two columns - 
# 1) instant - This is just a serial no. counter so not needed
# 2) dteday - The date of the observation. This is not needed as we are already considering yr, 
# month, weekday and hour as predictors

newdata <- one_hot(as.data.table(trip_data_2011_2012[c(-1,-2)]))


# Let's look a summary of this data
Hmisc::describe(newdata)

# It has 64 predictors since each unique value of our categorical variables was encoded as a separate
# column (predictor)



# Let's split the dataset for registered users and casual users
# We remove columns 'casual' and 'cnt' because we are currently only concerned with predicting
# rides by registered users and we don't need the total count (rides by registered+rides by casuals)
reg = newdata[,-c('casual','cnt','season_4','yr_1', 'mnth_12','hr_23','holiday_1', 'weekday_6','workingday_0', 'workingday_1', 'weathersit_4')]


# Let's first split the dataset into training

set.seed(123)

# Let's take 15000 out of ~17k samples for training 

n = dim(reg)[1]
tr = sample(1:n,15000)

newdata_tr = reg[tr]
newdata_test = reg[-tr]

# Now, let's run a multiple linear regression with all predictors
reg_mlr <- lm(registered~., data=newdata_tr)




summary(reg_mlr)


# We can see 9 co-efficients were not selected because of singularities.
# That's okay. It simply means that those co-efficients were a linear combination of other co-efficients
# and were effectively not providing any new information, hence they were ignored.


plot(reg_mlr)


# We see a more or less random spread of residuals and we get a good adjusted r square value as well.
# Let's try to predict the values on our test set now

reg_pred_mlr <- predict(reg_mlr, newdata=newdata_test)

plot(newdata_test$registered, reg_pred_mlr)
abline(1,1)


rmse.mlr <- sqrt(mean((reg_pred_mlr-newdata_test$registered)^2)/(nrow(newdata_test)-43-1))


# Okay we have around 68% accuracy :D
# That means we have room to improve yay!
# The residuals form a pattern that looks like a parabola meaning we are making systematic errors.


# Calculate fraction of observations where rides per hour exceed 400

n_out <- nrow(newdata[newdata$registered>400])
fraction = n_out/nrow(newdata)

# Around 7% of all the hours in 2011 and 2012 experienced more than 400 rides by registered users.


# Let's see if we can train a less complicated model using the step forward method -

null = lm(registered~1, data=reg[tr,])
#regForward = step(null, scope=formula(reg_mlr), direction="forward", k=log(length(tr))) 

#regBack = step(reg_mlr, direction="backward", k=log(length(tr)))



# Let's try running a random forest model

library(tree)

library(randomForest)

#bag.reg <- randomForest(registered ~ ., data = reg,
 #                          subset = tr, mtry = 7, importance = TRUE)


trip_data_2011_2012_sub <- trip_data_2011_2012[sample(1:nrow(trip_data_2011_2012), 5000),]


tree.reg <- tree(registered~.-instant-dteday-casual-cnt, trip_data_2011_2012_sub)


train = sample(1:nrow(trip_data_2011_2012_sub),4000)
random.reg <- randomForest(registered~.-instant-dteday-casual-cnt, data = trip_data_2011_2012_sub,
                           subset = train, mtry = 4, importance = TRUE)
summary(random.reg)


yhat.reg <- predict(random.reg, newdata = trip_data_2011_2012_sub[-train, ])
plot(yhat.reg, trip_data_2011_2012_sub[-train, ]$registered)
abline(1,1)
mse.reg = sqrt(mean((yhat.reg-trip_data_2011_2012_sub[-train, ]$registered)^2)/(1000-12-1))
plot(yhat.reg, residuals)
