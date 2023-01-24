#--------------------------------------------------
#Bike Share
#Import libraries
library(ggplot2)
library(tree)
library(kknn)
library(MASS)
library(Hmisc)
library(gtable)
library(gridExtra)
library(randomForest)
library(rpart)
library(rpart.plot)
library(gbm)
#----------# Let's read the trip data CSV -------------------------
trip_data_2011_2012 <- read.csv('hour.csv')


#----------Let's factorize all required columns first--------------
trip_data_2011_2012$season <- as.factor(trip_data_2011_2012$season)
trip_data_2011_2012$yr <- as.factor(trip_data_2011_2012$yr)
trip_data_2011_2012$mnth <- as.factor(trip_data_2011_2012$mnth)
trip_data_2011_2012$hr <- as.factor(trip_data_2011_2012$hr)
trip_data_2011_2012$holiday <- as.factor(trip_data_2011_2012$holiday)
trip_data_2011_2012$weekday <- as.factor(trip_data_2011_2012$weekday)
trip_data_2011_2012$workingday <- as.factor(trip_data_2011_2012$workingday)
trip_data_2011_2012$weathersit <- as.factor(trip_data_2011_2012$weathersit)
str(trip_data_2011_2012) #Check the structure of the dataset

#---------------------Modifying Data-----------------------------
library(mltools)
library(data.table)
newdata <- as.data.table(trip_data_2011_2012[c(-1,-2)])
reg = newdata[newdata$registered < 500,-c('casual','cnt', 'yr')]
#We're removing the outliers that is over 500, and taking out casual, count and year since we're only predicting for registered users and years is out of context
cas = newdata[newdata$casual < 500,-c('registered','cnt', 'yr')]
#Likewise, we're taking out registered to predict casuals


#Set the seed to make partition reproducible
set.seed(99)
#Let's divide the data into three sets since we have a fairly large dataset (17379 observations)
n = nrow(reg) 
n1 = floor(n/2)
n2 = floor(n/4)
n3 = n-n1-n2
ii = sample(1:n,n) 
#Training Set
reg_train = reg[ii[1:n1], ] 
cas_train = cas[ii[1:n1], ] 
#Validation Set
reg_val = reg[ii[n1+1:n2], ]
cas_val = cas[ii[n1+1:n2], ] 
#Testing Set
reg_test = reg[ii[n1+n2+1:n3], ] 
cas_test = cas[ii[n1+n2+1:n3], ] 









#------------------------------------------------------------------------
#------------------------------------------------------------------------
#------------------------------------------------------------------------
#-------------------------REGISTERED PREDICTIONS-------------------------
#------------------------------------------------------------------------
#------------------------------------------------------------------------
#------------------------------------------------------------------------



#-------------Use RPart to fit big tree------------------------------------
big.tree = rpart(registered ~.,method="anova",data=reg_train,
                 control=rpart.control(minsplit=5,cp=.001)) 
#CP value determines if it is worth it to split or not
#Greater CP, stricter partition. Lower CP, partition is more flexible 
nbig = length(unique(big.tree$where))
cat('size of big tree: ',nbig,'\n') #Size of big tree = 128
par(mfrow=c(1,1))
plotcp(big.tree)

#Fit on training set, predict on validation set for vector of cp

cpvec = big.tree$cptable[,"CP"] #cp values to try
ntree = length(cpvec) #number of cv values = number of trees fit.
iltree = rep(0,ntree) #in-sample loss
oltree = rep(0,ntree) #out-of-sample loss
sztree = rep(0,ntree) #size of each tree
for(i in 1:ntree) {
  if((i %% 10)==0) cat('tree i: ',i,'\n')
  temptree = prune(big.tree,cp=cpvec[i])
  sztree[i] = length(unique(temptree$where))
  iltree[i] = sum((reg_train$registered-predict(temptree))^2)
  ofit = predict(temptree,reg_val)
  oltree[i] = sum((reg_val$registered-ofit)^2)
}
#Let's see the out of sample loss and in sample loss
oltree=sqrt(oltree/nrow(reg_val)); iltree = sqrt(iltree/nrow(reg_train))
mean(oltree)
mean(iltree)

#Plot the losses
rgl = range(c(iltree,oltree))
plot(range(sztree),rgl,type='n',xlab='tree size',ylab='loss')
points(sztree,iltree,pch=15,col='red')
points(sztree,oltree,pch=16,col='blue')
legend("topright",legend=c('in-sample','out-of-sample'),lwd=3,col=c('red','blue'))


#Prune the tree using the cp value that gave us the lowest loss out of sample
iitree = which.min(oltree)
thetree = prune(big.tree,cp=cpvec[iitree])
thetreepred = predict(thetree,reg_val)
plot(thetreepred, reg_val$registered)
abline(0,1)
tree.rmse = sqrt(sum((reg_val$registered - thetreepred)^2)/nrow(reg_val))
cat('Pruned Tree RMSE for validation set : ',tree.rmse,'\n')
#So we get ~59.855 for a pruned tree



#------------------Random Forest------------------------------------

#First fit random forest on training set, get loss on validation using values of mtry and ntrees

set.seed(1)
p=ncol(reg_train)-1
mtryv = c(p,round(sqrt(p))) 
ntreev = c(100, 500)
parmrf = expand.grid(mtryv,ntreev)
colnames(parmrf)=c('mtry','ntree')
nset = nrow(parmrf)
olrf = rep(0,nset)
ilrf = rep(0,nset)
rffitv = vector('list',nset)
for(i in 1:nset) {
  cat('doing rf ',i,' out of ',nset,'\n')
  temprf = randomForest(registered~.,data=reg_train,mtry=parmrf[i,1],ntree=parmrf[i,2], maxnodes= 1000)
  ifit = predict(temprf)
  ofit=predict(temprf,newdata=reg_val)
  olrf[i] = sum((reg_val$registered-ofit)^2)
  ilrf[i] = sum((reg_train$registered-ifit)^2)
  rffitv[[i]]=temprf
}

#For simplicity sake only 100 and 500 trees are used, but change this later to 1000 and 5000 

ilrf = round(sqrt(ilrf/nrow(reg_train)),3); olrf = round(sqrt(olrf/nrow(reg_val)),3)

print(cbind(parmrf,olrf,ilrf))
#So random forest is giving us a minimum out of sample loss of 49.596 when we use all the predictors (bagging) and 500 trees




#Fit random forest on train+validation, predict on test
reg_train_val = rbind(reg_train, reg_val)
rf.reg = randomForest(registered ~., reg_train_val, mtry = 11, ntree = 500, importance = TRUE)
rf.reg #With random forest we are able to explain 82.28% of the variance in registered users using all the predictors
yhat.rf = predict(rf.reg, newdata = reg_test)

rf.rmse = sqrt(sum((reg_test$registered - yhat.rf)^2)/nrow(reg_test))
cat('Random Forest RMSE in Testing Set : ',rf.rmse,'\n')
plot(reg_test$registered, yhat.rf,xlab='Test Registered',ylab='Prediction Registered')
abline(0,1,col='red',lwd=2)
#We can see that it is underestimating for the higher values

#Let's see the variance importance
importance(rf.reg)
varImpPlot(rf.reg)
#Clearly hour is the most impactful variable





#------------------Bagging------------------------------------
#Let's try bagging separately with different numbers of trees
bag.reg = randomForest(registered~., reg_train, mtry = 11, ntree = 100, importance = TRUE)
bag.reg
#So this is the same as random forest with all the variables
#We should probably increase the number of trees but for simplicity sake just use 100

bag.reg2 = randomForest(registered~., reg_train, mtry = 11, ntree = 50, importance = TRUE)
bag.reg2


bag.reg3 = randomForest(registered~., reg_train, mtry = 11, ntree = 500, importance = TRUE)
bag.reg3




#And let's fit it on the testing set using both training + validation

reg_train_val = rbind(reg_train, reg_val)
bag.reg = randomForest(registered ~., reg_train_val, mtry = 11, ntree = 100, importance = TRUE)
bag.reg #With bagging we are able to explain 82.66% of the variance in registered users using all the predictors
yhat.rf = predict(bag.reg, newdata = reg_test)

rf.rmse = sqrt(sum((reg_test$registered - yhat.rf)^2)/nrow(reg_test))
cat('Bagging  RMSE in Testing Set : ',rf.rmse,'\n')
plot(reg_test$registered, yhat.rf,xlab='Test Registered',ylab='Prediction Registered')
abline(0,1,col='red',lwd=2)





#------------------Boosting------------------------------------

set.seed(1)
idv = c(2,5,10)
ntv = c(1000,5000)
lamv=c(.001,.05,.2)
parmb = expand.grid(idv,ntv,lamv)
colnames(parmb) = c('tdepth','ntree','lam')
print(parmb)
nset = nrow(parmb)
olb = rep(0,nset)
ilb = rep(0,nset)
bfitv = vector('list',nset)
for(i in 1:nset) {
  cat('doing boost ',i,' out of ',nset,'\n')
  tempboost = gbm(registered~.,data=reg_train,distribution='gaussian',
                  interaction.depth=parmb[i,1],n.trees=parmb[i,2],shrinkage=parmb[i,3])
  ifit = predict(tempboost,n.trees=parmb[i,2])
  ofit=predict(tempboost,newdata=reg_val,n.trees=parmb[i,2])
  olb[i] = sum((reg_val$registered - ofit)^2)
  ilb[i] = sum((reg_train$registered - ifit)^2)
  bfitv[[i]]=tempboost
}

ilb = round(sqrt(ilb/nrow(reg_train)),3); olb = round(sqrt(olb/nrow(reg_val)),3)

print(cbind(parmb,olb,ilb))
#We get minimum loss of 48.801 when ntree = 1000, lamda is 0.05 and depth = 10




#Now fit the best boosting on train and validation then get fit, RMSE on test

ntrees=1000
reg_train_val = rbind(reg_train, reg_val)


boost.reg <- gbm(registered ~ ., data = reg_train_val, distribution = "gaussian", 
                 interaction.depth = 10, n.trees = ntrees, shrinkage = 0.05)

summary(boost.reg)
plot(boost.reg, i="hr")

yhat.boost = predict(boost.reg, reg_test, ntrees = 1000)
boost.rmse = sqrt(sum((reg_test$registered - yhat.boost)^2)/nrow(reg_test))
cat('Boosting RMSE: ',boost.rmse,'\n')
#So now our RMSE is at 46.24659
plot(reg_test$registered, yhat.boost ,xlab='Test Registered',ylab='Boosting Prediction')
abline(0,1,col='red',lwd=2)

#So Boosting will give us the least RMSE














#------------------------------------------------------------------------
#------------------------------------------------------------------------
#------------------------------------------------------------------------
#-------------------------CASUAL PREDICTIONS-----------------------------
#------------------------------------------------------------------------
#------------------------------------------------------------------------
#------------------------------------------------------------------------



#-------------Use RPart to fit big tree------------------------------------
big.tree2 = rpart(casual ~.,method="anova",data=cas_train,
                  control=rpart.control(minsplit=5,cp=.0005)) 
#CP value determines if it is worth it to split or not
#Greater CP, stricter partition. Lower CP, partition is more flexible 
nbig = length(unique(big.tree2$where))
cat('size of big tree: ',nbig,'\n') #Size of big tree 2 = 116
par(mfrow=c(1,1))
plotcp(big.tree2)

#Fit on training set, predict on validation set for vector of cp

cpvec = big.tree2$cptable[,"CP"] #cp values to try
ntree = length(cpvec) #number of cv values = number of trees fit.
iltree = rep(0,ntree) #in-sample loss
oltree = rep(0,ntree) #out-of-sample loss
sztree = rep(0,ntree) #size of each tree
for(i in 1:ntree) {
  if((i %% 10)==0) cat('tree i: ',i,'\n')
  temptree = prune(big.tree2,cp=cpvec[i])
  sztree[i] = length(unique(temptree$where))
  iltree[i] = sum((cas_train$casual-predict(temptree))^2)
  ofit = predict(temptree,cas_val)
  oltree[i] = sum((cas_val$casual-ofit)^2)
}
#Let's see the out of sample loss and in sample loss
oltree=sqrt(oltree/nrow(cas_val)); iltree = sqrt(iltree/nrow(cas_train))
mean(oltree)
mean(iltree)

#Plot the losses
rgl = range(c(iltree,oltree))
plot(range(sztree),rgl,type='n',xlab='tree size',ylab='loss')
points(sztree,iltree,pch=15,col='red')
points(sztree,oltree,pch=16,col='blue')
legend("topright",legend=c('in-sample','out-of-sample'),lwd=3,col=c('red','blue'))


#Prune the tree using the cp value that gave us the lowest loss out of sample
iitree = which.min(oltree)
thetree = prune(big.tree2,cp=cpvec[iitree])
thetreepred = predict(thetree,cas_val)
plot(thetreepred, cas_val$casual)
abline(0,1)
tree.rmse = sqrt(sum((cas_val$casual - thetreepred)^2)/nrow(cas_val))
cat('Pruned Tree RMSE for validation set : ',tree.rmse,'\n')
#So we get out of sample loss of 21.9601 for a pruned tree




#------------------Random Forest------------------------------------

#First fit random forest on training set, get loss on validation using values of mtry and ntrees

set.seed(1)
p=ncol(cas_train)-1
mtryv = c(p,round(sqrt(p))) 
ntreev = c(100, 500)
parmrf = expand.grid(mtryv,ntreev)
colnames(parmrf)=c('mtry','ntree')
nset = nrow(parmrf)
olrf = rep(0,nset)
ilrf = rep(0,nset)
rffitv = vector('list',nset)
for(i in 1:nset) {
  cat('doing rf ',i,' out of ',nset,'\n')
  temprf = randomForest(casual~.,data=cas_train,mtry=parmrf[i,1],ntree=parmrf[i,2], maxnodes= 1000)
  ifit = predict(temprf)
  ofit=predict(temprf,newdata=cas_val)
  olrf[i] = sum((cas_val$casual-ofit)^2)
  ilrf[i] = sum((cas_train$casual-ifit)^2)
  rffitv[[i]]=temprf
}

#For simplicity sake only 100 and 500 trees are used, but change this later to 1000 and 5000 

ilrf = round(sqrt(ilrf/nrow(cas_train)),3); olrf = round(sqrt(olrf/nrow(cas_val)),3)

print(cbind(parmrf,olrf,ilrf))
#So random forest is giving us a minimum out of sample loss of 17.917 when we use all the predictors (bagging) and 100 trees




#Fit random forest on train+validation, predict on test
cas_train_val = rbind(cas_train, cas_val)
rf.cas = randomForest(casual ~., cas_train_val, mtry = 11, ntree = 100, importance = TRUE)
rf.cas #With random forest we are able to explain 87.71% of the variance in casual users using all the predictors
yhat.rf = predict(rf.cas, newdata = cas_test)

rf.rmse = sqrt(sum((cas_test$casual - yhat.rf)^2)/nrow(cas_test))
cat('Random Forest RMSE in Testing Set : ',rf.rmse,'\n')
plot(cas_test$casual, yhat.rf,xlab='Test Casuals',ylab='Prediction Casuals')
abline(0,1,col='red',lwd=2)
#We can see that it is underestimating for the higher values

#Let's see the variance importance
importance(rf.reg)
varImpPlot(rf.reg)
#Clearly hour is the most impactful variable here as well






#------------------Bagging------------------------------------
#Let's try bagging separately with different numbers of trees
bag.cas = randomForest(casual~., cas_train, mtry = 11, ntree = 100, importance = TRUE)
bag.cas
#So this is the same as random forest with all the variables
#We should probably increase the number of trees but for simplicity sake just use 100

bag.cas2 = randomForest(casual~., cas_train, mtry = 11, ntree = 50, importance = TRUE)
bag.cas2


bag.cas3 = randomForest(casual~., cas_train, mtry = 11, ntree = 500, importance = TRUE)
bag.cas3




#And let's fit it on the testing set using both training + validation

cas_train_val = rbind(cas_train, cas_val)
bag.cas = randomForest(casual ~., cas_train_val, mtry = 11, ntree = 100, importance = TRUE)
bag.reg #With bagging we are able to explain 86.37% of the variance in casual users using all the predictors
yhat.rf = predict(bag.cas, newdata = cas_test)

rf.rmse = sqrt(sum((cas_test$casual - yhat.rf)^2)/nrow(cas_test))
cat('Bagging  RMSE in Testing Set : ',rf.rmse,'\n')
plot(cas_test$casual, yhat.rf,xlab='Test Casuals',ylab='Prediction Casuals')
abline(0,1,col='red',lwd=2)





#------------------Boosting------------------------------------

set.seed(1)
idv = c(2,5,10)
ntv = c(1000,5000)
lamv=c(.001,.05,.2)
parmb = expand.grid(idv,ntv,lamv)
colnames(parmb) = c('tdepth','ntree','lam')
print(parmb)
nset = nrow(parmb)
olb = rep(0,nset)
ilb = rep(0,nset)
bfitv = vector('list',nset)
for(i in 1:nset) {
  cat('doing boost ',i,' out of ',nset,'\n')
  tempboost = gbm(casual~.,data=cas_train,distribution='gaussian',
                  interaction.depth=parmb[i,1],n.trees=parmb[i,2],shrinkage=parmb[i,3])
  ifit = predict(tempboost,n.trees=parmb[i,2])
  ofit=predict(tempboost,newdata=cas_val,n.trees=parmb[i,2])
  olb[i] = sum((cas_val$casual - ofit)^2)
  ilb[i] = sum((cas_train$casual - ifit)^2)
  bfitv[[i]]=tempboost
}

ilb = round(sqrt(ilb/nrow(cas_train)),3); olb = round(sqrt(olb/nrow(cas_val)),3)

print(cbind(parmb,olb,ilb))
#We get minimum loss of 17.423 when ntree = 1000, lamda is 0.05 and depth = 10




#Now fit the best boosting on train and validation then get fit, RMSE on test

ntrees=1000
cas_train_val = rbind(cas_train, cas_val)


boost.cas <- gbm(casual ~ ., data = cas_train_val, distribution = "gaussian", 
                 interaction.depth = 10, n.trees = ntrees, shrinkage = 0.05)

summary(boost.cas)
plot(boost.cas, i="hr")

yhat.boost = predict(boost.cas, cas_test, ntrees = 1000)
boost.rmse = sqrt(sum((cas_test$casual - yhat.boost)^2)/nrow(cas_test))
cat('Boosting RMSE: ',boost.rmse,'\n')
#So now our RMSE is at 17.12937
plot(cas_test$casual, yhat.boost ,xlab='Test Casuals',ylab='Boosting Casuals')
abline(0,1,col='red',lwd=2)

#So bagging will give us the least RMSE 


