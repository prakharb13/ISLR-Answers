## ----setup, include=FALSE----
knitr::opts_chunk$set(echo = FALSE)
knitr::purl()


## ---------------------------
rm(list=ls())
library(MASS)
library(ggplot2)
?Boston


## ---------------------------
pairs(Boston[,1:5],col='red',pch=21)


## ---------------------------
pairs(Boston[,6:10],col='red',pch=21)


## ---------------------------
pairs(Boston[,11:13],col='red',pch=21)


## ---------------------------
pairs(Boston[,c(1,6,7,8,9,10)],col='red',pch=21)


## ---------------------------
pairs(Boston[,c(1,11,12,13)],col='red',pch=21)


## ---------------------------
cor(Boston$crim,Boston[-c(1)])

par(mfrow=c(2,4))

plot(Boston$crim,Boston$zn,col='red')
plot(Boston$crim,Boston$nox,col='red')
plot(Boston$crim,Boston$rm,col='red')
plot(Boston$crim,Boston$age,col='red')
plot(Boston$crim,Boston$ptratio,col='red')
plot(Boston$crim,Boston$black,col='red')
plot(Boston$crim,Boston$lstat,col='red')
plot(Boston$crim,Boston$medv,col='red')




## ---------------------------
Boston[Boston$crim>(mean(Boston$crim)*2*sd(Boston$crim)),]
cat('sd:',sd(Boston$crim),'mean:',mean(Boston$crim),
    'range:',range(Boston$crim)) 


## ---------------------------
Boston[Boston$tax>(mean(Boston$tax)*2*sd(Boston$tax)),]
cat('sd:',sd(Boston$tax),'mean:',mean(Boston$tax),
    'range:',range(Boston$tax)) 


## ---------------------------
Boston[Boston$ptratio>(mean(Boston$ptratio)*2*sd(Boston$ptratio)),]
cat('sd:',sd(Boston$ptratio),'mean:',mean(Boston$ptratio),
    'range:',range(Boston$ptratio)) 


## ---------------------------
table(Boston$chas)


## ---------------------------
median(Boston$ptratio)


## ---------------------------
Boston[Boston$medv==min(Boston$medv),]

## ---------------------------
sapply(Boston,range)


## ---------------------------
dim(Boston[Boston$rm>7,])[1]


## ---------------------------
Boston[Boston$rm>8,]


## ---------------------------
model1 <- lm(crim~zn, Boston)
summary(model1)


plot(Boston$zn,Boston$crim,col='red')
abline(model1$coefficients[1],model1$coefficients[2])


## ---------------------------
model2 <- lm(crim~indus, Boston)
summary(model2)


plot(Boston$indus,Boston$crim,col='red')
abline(model2$coefficients[1],model2$coefficients[2])


## ---------------------------
model3 <- lm(crim~chas, Boston)
summary(model3)


plot(Boston$chas,Boston$crim,col='red')
abline(model3$coefficients[1],model3$coefficients[2])


## ---------------------------
model4 <- lm(crim~nox, Boston)
summary(model4)


plot(Boston$nox,Boston$crim,col='red')
abline(model4$coefficients[1],model4$coefficients[2])


## ---------------------------
model5 <- lm(crim~rm, Boston)
summary(model5)


plot(Boston$rm,Boston$crim,col='red')
abline(model5$coefficients[1],model5$coefficients[2])


## ---------------------------
model6 <- lm(crim~age, Boston)
summary(model6)


plot(Boston$age,Boston$crim,col='red')
abline(model6$coefficients[1],model6$coefficients[2])


## ---------------------------
model7 <- lm(crim~dis, Boston)
summary(model7)


plot(Boston$dis,Boston$crim,col='red')
abline(model7$coefficients[1],model7$coefficients[2])


## ---------------------------
model8 <- lm(crim~rad, Boston)
summary(model8)


plot(Boston$rad,Boston$crim,col='red')
abline(model8$coefficients[1],model8$coefficients[2])


## ---------------------------
model9 <- lm(crim~tax, Boston)
summary(model9)


plot(Boston$tax,Boston$crim,col='red')
abline(model9$coefficients[1],model9$coefficients[2])


## ---------------------------
model10 <- lm(crim~ptratio, Boston)
summary(model10)


plot(Boston$ptratio,Boston$crim,col='red')
abline(model10$coefficients[1],model10$coefficients[2])


## ---------------------------
model11 <- lm(crim~black, Boston)
summary(model11)


plot(Boston$black,Boston$crim,col='red')
abline(model11$coefficients[1],model11$coefficients[2])


## ---------------------------
model12 <- lm(crim~lstat, Boston)
summary(model12)


plot(Boston$lstat,Boston$crim,col='red')
abline(model12$coefficients[1],model12$coefficients[2])


## ---------------------------
model13 <- lm(crim~medv, Boston)
summary(model13)


plot(Boston$medv,Boston$crim,col='red')
abline(model13$coefficients[1],model13$coefficients[2])


## ---------------------------
mlp = lm(crim~.,data=Boston)
summary(mlp)


## ---------------------------
univariate_coeff <- c(model1$coefficients[2],model2$coefficients[2],model3$coefficients[2],model4$coefficients[2],model5$coefficients[2],model6$coefficients[2],model7$coefficients[2],model8$coefficients[2],model9$coefficients[2],model10$coefficients[2],model11$coefficients[2],model12$coefficients[2],model13$coefficients[2])

multivariate_coeff <- c(mlp$coefficients)

plot(univariate_coeff,multivariate_coeff[(2:14)],xlab='Univariate coeff', ylab='Multivariate coeff',col='red')


## ---------------------------
poly_model1 = lm(crim ~ poly(zn, 3, raw = TRUE), data = Boston) 
summary(poly_model1)


## ---------------------------
poly_model1 = lm(crim ~ poly(indus, 3, raw = TRUE), data = Boston) 
summary(poly_model1)


## ---------------------------
poly_model1 = lm(crim ~ poly(chas, 3, raw = TRUE), data = Boston) 
summary(poly_model1)


## ---------------------------
poly_model1 = lm(crim ~ poly(nox, 3, raw = TRUE), data = Boston) 
summary(poly_model1)


## ---------------------------
poly_model1 = lm(crim ~ poly(rm, 3, raw = TRUE), data = Boston) 
summary(poly_model1)


## ---------------------------
poly_model1 = lm(crim ~ poly(age, 3, raw = TRUE), data = Boston) 
summary(poly_model1)


## ---------------------------
poly_model1 = lm(crim ~ poly(dis, 3, raw = TRUE), data = Boston) 
summary(poly_model1)


## ---------------------------
poly_model1 = lm(crim ~ poly(rad, 3, raw = TRUE), data = Boston) 
summary(poly_model1)


## ---------------------------
poly_model1 = lm(crim ~ poly(tax, 3, raw = TRUE), data = Boston) 
summary(poly_model1)


## ---------------------------
poly_model1 = lm(crim ~ poly(ptratio, 3, raw = TRUE), data = Boston) 
summary(poly_model1)


## ---------------------------
poly_model1 = lm(crim ~ poly(black, 3, raw = TRUE), data = Boston) 
summary(poly_model1)


## ---------------------------
poly_model1 = lm(crim ~ poly(lstat, 3, raw = TRUE), data = Boston) 
summary(poly_model1)


## ---------------------------
poly_model1 = lm(crim ~ poly(medv, 3, raw = TRUE), data = Boston) 
summary(poly_model1)


## ---------------------------
library(ISLR)
?College


## ---------------------------
sample_size = sample(1:dim(College)[1],.75*nrow(College))
College$Private_num = ifelse(College$Private=='Yes',1,0)
train = College[sample_size,]
test = College[-sample_size,]

ind_cols = c("Private_num","Accept","Enroll","Top10perc","Top25perc"
             ,"F.Undergrad","P.Undergrad"
             ,"Outstate","Room.Board"
             ,"Books","Personal"
             ,"PhD","Terminal"
             ,"S.F.Ratio","perc.alumni"
             ,"Expend","Grad.Rate") 



## ---------------------------
set.seed(45)
model = lm(Apps~Private_num+Accept+Enroll+Top10perc+Top25perc+F.Undergrad+P.Undergrad+Outstate+Room.Board+Books+Personal+PhD+Terminal+S.F.Ratio+perc.alumni+Expend+Grad.Rate,data=train)
summary(model)
plot(model)

predicted = predict(model,test[-c(1,2)])

rmse = sqrt(mean((test$Apps-predicted)^2))
cat('RMSE on test Dataset:',rmse)


## ---------------------------
library(glmnet)
set.seed(45)
train_x = model.matrix(~.-1,train[,ind_cols])
model = cv.glmnet(train_x,train$Apps,data=train,alpha=0)
summary(model)
plot(model)
test_x = model.matrix(~.-1,test[,ind_cols])
predicted = predict(model,test_x)
rmse = sqrt(mean((test$Apps-predicted)^2))
cat('RMSE on test Dataset:',rmse)


## ---------------------------
set.seed(45)
train_x = model.matrix(~.-1,train[,ind_cols])
model = cv.glmnet(train_x,train$Apps,data=train,alpha=1)
summary(model)
plot(model)
test_x = model.matrix(~.-1,test[,ind_cols])
predicted = predict(model,test_x)
rmse = sqrt(mean((test$Apps-predicted)^2))
cat('RMSE on test Dataset:',rmse)

coefficients = coef(model, 'lambda.min')
length_nonzero_coeff = length(coefficients[which(coefficients!=0)])


## ---------------------------
set.seed(45)
library(pls)
pcr_model <- pcr(Apps~Private_num+Accept+Enroll+Top10perc+Top25perc+F.Undergrad+P.Undergrad+Outstate+Room.Board+Books+Personal+PhD+Terminal+S.F.Ratio+perc.alumni+Expend+Grad.Rate,data=train,  scale=TRUE, validation='CV')
summary(pcr_model)
plot(pcr_model)
validationplot(pcr_model , val.type="MSEP",col='red')

predicted = predict(pcr_model,test[,ind_cols],ncomp=7)
rmse = sqrt(mean((test$Apps-predicted)^2))
cat('RMSE on test Dataset:',rmse)


## ---------------------------
set.seed(45)
library(pls)

plsr_model <- plsr(Apps~Private_num+Accept+Enroll+Top10perc+Top25perc+F.Undergrad+P.Undergrad+Outstate+Room.Board+Books+Personal+PhD+Terminal+S.F.Ratio+perc.alumni+Expend+Grad.Rate,data=train,  scale=TRUE, validation='CV')
summary(plsr_model)
plot(plsr_model)

validationplot(plsr_model , val.type="MSEP",col='red')

predicted = predict(plsr_model,test[,ind_cols],ncomp=7)
rmse = sqrt(mean((test$Apps-predicted)^2))
cat('RMSE on test Dataset:',rmse)


## ---------------------------
sample_size = sample(1:dim(Boston)[1],.75*nrow(Boston))
train = Boston[sample_size,]
test = Boston[-sample_size,]

ind_cols = c("zn","indus","chas","nox","rm","age","dis","rad",  "tax","ptratio","black","lstat","medv") 


## ---------------------------
set.seed(45)
model = lm(crim~.,data=train)
summary(model)
plot(model)
predicted = predict(model,test[,ind_cols])
rmse = sqrt(mean((test$crim-predicted)^2))
cat('RMSE on test Dataset:',rmse)


## ---------------------------
set.seed(45)
train_x = model.matrix(~.-1,train[,ind_cols])
model = cv.glmnet(train_x,train$crim,data=train,alpha=0)
summary(model)
plot(model)
test_x = model.matrix(~.-1,test[,ind_cols])
predicted = predict(model,test_x)
rmse = sqrt(mean((test$crim-predicted)^2))
cat('RMSE on test Dataset:',rmse)


## ---------------------------
set.seed(45)
train_x = model.matrix(~.-1,train[,ind_cols])
model = cv.glmnet(train_x,train$crim,data=train,alpha=1)
summary(model)
plot(model)

test_x = model.matrix(~.-1,test[,ind_cols])
predicted = predict(model,test_x)
rmse = sqrt(mean((test$crim-predicted)^2))
cat('RMSE on test Dataset:',rmse)


## ---------------------------
set.seed(45)
pcr_model <- pcr(crim~.,data=train,  scale=TRUE, validation='CV')
plot(pcr_model)

predicted = predict(pcr_model,test[,ind_cols],ncomp=7)
rmse = sqrt(mean((test$crim-predicted)^2))
cat('RMSE on test Dataset:',rmse)


## ---------------------------
set.seed(45)
plsr_model <- plsr(crim~.,data=train,  scale=TRUE, validation='CV')
plot(plsr_model)
predicted = predict(plsr_model,test[,ind_cols],ncomp=7)
rmse = sqrt(mean((test$crim-predicted)^2))
cat('RMSE on test Dataset:',rmse)


## ---------------------------
set.seed(45)
summary(Weekly)

cor(Weekly[,1:length(Weekly)-1])

pairs(Weekly[,1:8],col='red')

par(mfrow=c(2,4))
hist(Weekly$Lag1,main='Lag1',col='red')
hist(Weekly$Lag2,main='Lag2',col='red')
hist(Weekly$Lag3,main='Lag3',col='red')
hist(Weekly$Lag4,main='Lag4',col='red')
hist(Weekly$Lag5,main='Lag5',col='red')
hist(Weekly$Volume,main='Volume',col='red')
hist(Weekly$Today,main='Today',col='red')
hist(Weekly$Year,main='Year',col='red')


## ---------------------------
set.seed(45)
library(glmnet)

logistic_model <- glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5,data=Weekly,family=binomial)
summary(logistic_model)
plot(logistic_model)


## ---------------------------
predicted_probs = predict(logistic_model,type='response')
predicted = ifelse(predicted_probs<0.5,'Down','Up')

print(table(Weekly$Direction,predicted))
cat("\n")
cat('Overal fraction of correct prediction:',(564+49)/(49+41+435+564))


## ---------------------------
set.seed(45)
library(glmnet)
train = Weekly[Weekly['Year']<=2008,]
test =  Weekly[Weekly['Year']>2008,]

logistic_model <- glm(Direction~Lag2,data=train,family=binomial)
summary(logistic_model)

predicted_probs = predict(logistic_model,test['Lag2'],type='response')
predicted = ifelse(predicted_probs<0.5,'Down','Up')

print(table(test$Direction,predicted))
cat("\n")
cat('Overal fraction of correct prediction:',(56+9)/(5+9+34+56))


## ---------------------------
set.seed(45)
library(kknn)
knn_model <- kknn(Direction~Lag2,train=train,test=test,k=1)

table(knn_model$fitted.values,test$Direction)
cat("\n")
cat('Overal fraction of correct prediction:',(30+22)/(21+31+30+22))


## ---------------------------
set.seed(45)

knn_model <- kknn(Direction~Lag2,train=train,test=test,k=3)

table(knn_model$fitted.values,test$Direction)
cat("\n")
cat('Overal fraction of correct prediction:',(16+42)/(16+27+19+42))


## ---------------------------
set.seed(45)

knn_model <- kknn(Direction~Lag2,train=train,test=test,k=6)

table(knn_model$fitted.values,test$Direction)
cat("\n")
cat('Overal fraction of correct prediction:',(16+40)/(16+27+21+40))


## ---------------------------
set.seed(45)
library(glmnet)
train = Weekly[Weekly['Year']<=2008,]
test =  Weekly[Weekly['Year']>2008,]

logistic_model <- glm(Direction~Lag2+Lag2*Lag2,data=train,family=binomial)

predicted_probs = predict(logistic_model,test['Lag2'],type='response')
predicted = ifelse(predicted_probs<0.5,'Down','Up')

print(table(test$Direction,predicted))
cat("\n")
cat('Overal fraction of correct prediction:',(56+9)/(5+9+34+56))


## ---------------------------
set.seed(45)
library(glmnet)
train = Weekly[Weekly['Year']<=2008,]
test =  Weekly[Weekly['Year']>2008,]

logistic_model <- glm(Direction~Lag2*Lag2,data=train,family=binomial)

predicted_probs = predict(logistic_model,test['Lag2'],type='response')
predicted = ifelse(predicted_probs<0.5,'Down','Up')

print(table(test$Direction,predicted))
cat("\n")
cat('Overal fraction of correct prediction:',(56+9)/(5+9+34+56))


## ---------------------------
set.seed(45)
library(ISLR)
sample_size = sample(1:dim(Carseats)[1],.75*nrow(Carseats))
train = Carseats[sample_size,]
test = Carseats[-sample_size,]


## ---------------------------
library(tree)

regression_tree_model <- tree(Sales~.,data=train)
summary(regression_tree_model)

plot(regression_tree_model)
text(regression_tree_model,pretty=0,cex=0.65)

yhat = predict(regression_tree_model,test[,-c(1)])
cat("\n")
cat('MSE on test Dataset:',mean((yhat - test$Sales)^2))


## ---------------------------
library(tree)
cv_tree_model <- cv.tree(regression_tree_model)
plot(cv_tree_model,col='red')


## ---------------------------
library(tree)
prune_tree_model <- prune.tree(regression_tree_model,best = 5)

yhat = predict(prune_tree_model,test[,-c(1)])
cat("\n")
cat('MSE on test Dataset:',mean((yhat - test$Sales)^2))


## ---------------------------
library(randomForest)
rf_model <- randomForest(Sales~.,data=train,mtry=10,importance=TRUE)
rf_model

yhat = predict(rf_model,test[,-c(1)])
cat("\n")
cat('MSE on test Dataset:',mean((yhat - test$Sales)^2))
cat("\n")
importance(rf_model)


## ---------------------------
set.seed(42)
library(randomForest)
rf_model1 <- randomForest(Sales~.,data=train,mtry=10,importance=TRUE)
rf_model2 <- randomForest(Sales~.,data=train,mtry=5,importance=TRUE)
rf_model3 <- randomForest(Sales~.,data=train,mtry=sqrt(10),importance=TRUE)
rf_model4 <- randomForest(Sales~.,data=train,mtry=10/4,importance=TRUE)

cat("Model 1: m=10")
importance(rf_model1)
cat("Model 1: m=5")
importance(rf_model2)
cat("Model 1: m=sqrt(10)")
importance(rf_model3)
cat("Model 1: m=10/4")
importance(rf_model4)


yhat1 = predict(rf_model1,test[,-c(1)])
yhat2 = predict(rf_model2,test[,-c(1)])
yhat3 = predict(rf_model3,test[,-c(1)])
yhat4 = predict(rf_model4,test[,-c(1)])
cat("\n")
cat('MSE on test Dataset: rf_model1:',mean((yhat1 - test$Sales)^2))
cat("\n")
cat('MSE on test Dataset: rf_model1',mean((yhat2 - test$Sales)^2))
cat("\n")
cat('MSE on test Dataset: rf_model1',mean((yhat3 - test$Sales)^2))
cat("\n")
cat('MSE on test Dataset: rf_model1',mean((yhat4 - test$Sales)^2))
cat("\n")


## ---------------------------
train = Caravan[1:1000,]
test = Caravan[1001:5822,]


## ---------------------------
library(gbm)
set.seed(45)

train$Purchase1 = rep(0,length(train$Purchase))
train$Purchase1[train$Purchase=='Yes'] = 1

train$AVRAAUT  = NULL
train$PVRAAUT  = NULL

gbm_boost=gbm(Purchase1~.-Purchase, data=train,       distribution="bernoulli", n.trees=1000, shrinkage=.01)
summary(gbm_boost)


## ---------------------------
test$Purchase1 = rep(0,length(test$Purchase))
test$Purchase1[test$Purchase=='Yes'] = 1

test$AVRAAUT  = NULL
test$PVRAAUT  = NULL

yhat=predict(gbm_boost ,newdata =test[-c(84,85)],type='response')
predicted = rep(0,length(test$Purchase1))
predicted[yhat>0.2] = 1

table(test$Purchase1,predicted)

cat("\n")
cat("Fraction of people predicted to make a purchase and actually make one:", 34/(34+123))
cat("\n")
cat("Accuracy:", (4410+34)/(4410+34+123+255))


## ---------------------------
set.seed(45)

train = Caravan[1:1000,]
test = Caravan[1001:5822,]

train$Purchase1 = rep(0,length(train$Purchase))
train$Purchase1[train$Purchase=='Yes'] = 1

train$AVRAAUT  = NULL
train$PVRAAUT  = NULL
train$Purchase = NULL

logistic = glm(Purchase1~., data=train,family='binomial')

test$Purchase1 = rep(0,length(test$Purchase))
test$Purchase1[test$Purchase=='Yes'] = 1

test$AVRAAUT  = NULL
test$PVRAAUT  = NULL
test$Purchase = NULL

yhat=predict(logistic ,newdata =test,type='response')
predicted = rep(0,length(test$Purchase1))
predicted[yhat>0.2] = 1

table(test$Purchase1,predicted)

cat("\n")
cat("Fraction of people predicted to make a purchase and actually make one:", 58/(58+350))
cat("\n")
cat("Accuracy:", (4183+58)/(58+231+4183+350))


## ---------------------------
beauty_df = read.csv('BeautyData.csv')


regression_model = lm(CourseEvals~.,data=beauty_df)
summary(regression_model)

plot(regression_model)

cor(beauty_df)


## ---------------------------
midcity = read.csv('MidCity.csv')
midcity$Nbhd = as.factor(midcity$Nbhd) 
midcity$Brick_num = ifelse(midcity$Brick=='Yes',1,0) 
set.seed(45)
linear_regression_model = lm(Price~SqFt+Bedrooms+Offers+Bathrooms+Nbhd+Brick_num,data=midcity)

summary(linear_regression_model)


## ---------------------------
summary(linear_regression_model)


## ---------------------------
set.seed(45)

nonlinear_regression_model = lm(Price~SqFt+Bedrooms+Offers+Bathrooms+Nbhd+Brick_num+Brick_num*Nbhd,data=midcity)

summary(nonlinear_regression_model)


## ---------------------------
set.seed(45)
midcity$Nbhd = as.numeric(midcity$Nbhd)
midcity$Nbhd_3 = ifelse(midcity$Nbhd==3,1,0)

linear_regression_model = lm(Price~SqFt+Bedrooms+Offers+Bathrooms+Nbhd_3+Brick_num,data=midcity)

summary(linear_regression_model)


## ---------------------------
###standardize the train x's
library(nnet)
set.seed(45)
minv = rep(0,13)
maxv = rep(0,13)
bostonsc = Boston
for(i in 1:13) {
minv[i] = min(Boston[[i+1]])
maxv[i] = max(Boston[[i+1]])
bostonsc[[i+1]] = (Boston[[i+1]]-minv[i])/(maxv[i]-minv[i])
}
final_size = NULL
final_decay = NULL

size_list = c(3,5,7,10)
decay_list = c(.1,.001,.0001)
k_folds = 5

n_train = dim(Boston)[1]
n0 = round(n_train/k_folds, 0)

out_MSE = matrix(0, k_folds)
train_cv_df = data.frame(matrix(nrow=0, ncol=3))


for(size in size_list)
  
  for (decay in decay_list){
  {
  
  used = NULL
  set = 1:n_train
  
  for(j in 1:k_folds){
    
    if(n0<length(set)){val = sample(set,n0)}
    if(n0>=length(set)){val=set}
    
    model = nnet(crim~.,bostonsc[-val,],size=size,decay=decay,trace=F)
    bostonsc_test = bostonsc[,-c(1)]
    prediction = predict(model, bostonsc[val,])

    
    out_MSE[j] = mean((prediction - (bostonsc[val,]$crim))^2)

    
    used = union(used,val)
    set = (1:n_train)[-used]  
  }
  avg_rmse_os = mean(out_MSE)
  final_size = size
  final_decay = decay
  
  train_cv_df = rbind(train_cv_df,cbind(avg_rmse_os,final_size,final_decay))
  
  }
  }
cat("\n","\n")
cat('Min MSE:',min(train_cv_df$avg_rmse_os),' and Size:',
train_cv_df[which(train_cv_df$avg_rmse_os==min(train_cv_df$avg_rmse_os)),]$final_size
,' and Decay:',
train_cv_df[which(train_cv_df$avg_rmse_os==min(train_cv_df$avg_rmse_os)),]$final_decay)

train_cv_df['hyparameter'] = paste(train_cv_df$final_size, train_cv_df$final_decay, sep="_")

plot(train_cv_df$avg_rmse_os,train_cv_df$hyperparameter,xlab='hyperparameters',ylab='avg_rmse_oos',main='Comparison of Avg RMSE vs Hyperparameter',col='red')

