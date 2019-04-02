getwd()
data=read.csv("D:\\VIT Workship\\missionPositive\\ML\\Bike-Sharing-Dataset\\hour.csv",header=TRUE)
View(data)

library(caTools)
set.seed(123)
attach(data)

split = sample.split(data$fare_amount,SplitRatio = 0.75)
training_set = subset(data,split == TRUE)
test_set = subset(data,split == FALSE)

#--------------------------SIMPLE LINEAR REGRESSION-------------------

r = lm(formula = data$cnt ~ data$season + data$holiday + data$weathersit + data$atemp + data$hum + data$windspeed) 
summary(r)
y_pred = predict(r,newdata = test_set)
y_pred

#regressor1 = lm(formula = data$fare_amount ~ data$cnt +data$distance,data = training_set)
#summary(regressor1)f
#y_pred1 = predict(regressor,newdata = data$test_set)
#y_pred1

#regressor2 = lm(formula = data$fare_amount ~ data$season+data$holiday+data$weathersit+data$atemp+data$hum+data$windspeed,data = training_set)
#summary(regressor2)
#y_pred2 = predict(regressor2,newdata = data$test_set)

library(ggplot2)
#Season vs Count
ggplot() +
  geom_point(aes(x = training_set$season,y = training_set$cnt),
             colour = 'red')+
  geom_line(aes(x = data$season,y = predict(r,newdata = data)),
            colour = 'blue')+
  ggtitle('Season vs Count (Training set')+
  xlab('Season')+
  ylab('Count')

ggplot() +
  geom_point(aes(x = test_set$season,y = test_set$cnt),
             colour = 'red')+
  geom_line(aes(x = data$season,y = predict(r,newdata = data)),
            colour = 'blue')+
  ggtitle('Season vs Count (Test set')+
  xlab('Season')+
  ylab('Count')

#Weathersit vs Count
ggplot() +
  geom_point(aes(x = training_set$weathersit,y = training_set$cnt),
             colour = 'red')+
  geom_line(aes(x = data$weathersit,y = predict(r,newdata = data)),
            colour = 'blue')+
  ggtitle('Weathersit vs Count (Training set')+
  xlab('Weathersit')+
  ylab('Count')

ggplot() +
  geom_point(aes(x = test_set$weathersit,y = test_set$cnt),
             colour = 'red')+
  geom_line(aes(x = data$weathersit,y = predict(r,newdata = data)),
            colour = 'blue')+
  ggtitle('Weathersit vs Count (Test set')+
  xlab('Weathersit')+
  ylab('Count')

#Weekday vs Count

ggplot() +
  geom_point(aes(x = training_set$weekday,y = training_set$cnt),
             colour = 'red')+
  geom_line(aes(x = data$weekday,y = predict(r,newdata = data)),
            colour = 'blue')+
  ggtitle('Weekday vs Count (Training set')+
  xlab('Weekday')+
  ylab('Count')

ggplot() +
  geom_point(aes(x = test_set$weekday,y = test_set$cnt),
             colour = 'red')+
  geom_line(aes(x = data$weekday,y = predict(r,newdata = data)),
            colour = 'blue')+
  ggtitle(' vs Count (Test set')+
  xlab('Weekday')+
  ylab('Count')

#atemp vs Count
ggplot() +
  geom_point(aes(x = training_set$atemp,y = training_set$cnt),
             colour = 'red')+
  geom_line(aes(x = data$atemp,y = predict(r,newdata = data)),
            colour = 'blue')+
  ggtitle('atemp vs Count (Training set')+
  xlab('atemp')+
  ylab('Count')

ggplot() +
  geom_point(aes(x = test_set$atemp,y = test_set$cnt),
             colour = 'red')+
  geom_line(aes(x = data$atemp,y = predict(r,newdata = data)),
            colour = 'blue')+
  ggtitle('atemp vs Count (Test set')+
  xlab('atemp')+
  ylab('Count')

#humidity vs sount

ggplot() +
  geom_point(aes(x = training_set$hum,y = training_set$cnt),
             colour = 'red')+
  geom_line(aes(x = data$hum,y = predict(r,newdata = data)),
            colour = 'blue')+
  ggtitle('Humidity vs Count (Training set')+
  xlab('Humidity')+
  ylab('Count')

ggplot() +
  geom_point(aes(x = test_set$hum,y = test_set$cnt),
             colour = 'red')+
  geom_line(aes(x = data$hum,y = predict(r,newdata = data)),
            colour = 'blue')+
  ggtitle('Humidity vs Count (Test set')+
  xlab('Humidity')+
  ylab('Count')

#windspeed vs count

ggplot() +
  geom_point(aes(x = test_set$windspeed,y = test_set$cnt),
             colour = 'red')+
  geom_line(aes(x = data$windspeed,y = predict(r,newdata = data)),
            colour = 'blue')+
  ggtitle('Windspeed vs Count (Test set')+
  xlab('Windspeed')+
  ylab('Count')

#-----------------MULTIPLE LINEAR REGRESSION-----------------

m_regressor = lm(formula = data$cnt ~ data$season + data$holiday + data$workingday + data$weathersit + data$atemp + data$hum + data$windspeed, data = training_set)
summary(m_regressor)

y_multi_pred = predict(m_regressor,newdata = test_set)
y_multi_pred


#--------------------LOGISTIC REGRESSION------------------------

dataset = data.frame(data$season,data$holiday,data$cnt,data$base_price)
View(dataset)

library(caTools)

set.seed(123)

split = sample.split(dataset$data.base_price,SplitRatio = 0.75)
train = subset(dataset,split==TRUE)
test = subset(dataset,split==FALSE)

train[,1] = scale(train[,1])
train[,3] = scale(train[,3])

test[,1] = scale(test[,1])
test[,3] = scale(test[,3])

#train[,4] = ifelse(train$data.base_price>4,1,0)
#test[,4] = ifelse(test$data.base_price==4,0,1)

classifier = glm(formula = train$data.holiday ~ .,family = binomial,data = train)
prob_predict = predict(classifier,type = "response",newdata = test[,-2])
prob_predict
y_glm_pred = ifelse(prob_predict > 0.025,1,0)
y_glm_pred

cm = table(test[,2],y_glm_pred)
cm

library(pROC)
roc(test[,2],y_glm_pred,plot = TRUE)
#-----------------LINEAR DISCIMINANT ANALYSIS--------------------
library(BBmisc)

pairs(dataset[1:4],data = dataset)
cor(dataset[1:4])
attach(dataset)

plot(dataset$data.season,dataset$data.base_price)
plot(dataset$data.holiday,dataset$data.base_price)
plot(dataset$data.cnt,dataset$data.base_price)

library(psych)
library(MASS)

pairs.panels(train[1:4],gap = 0,bg = c('green','red')[dataset$data.base_price],pch = 22)

linear<-lda(dataset$data.season~.,data = dataset)
linear
attributes(linear)
library(pROC)
roc(test[,2],y_multi_pred,plot = TRUE)
#-----------------------NEURAL NETWORKS----------------------

require(neuralnet)
nn = neuralnet(cnt~season+mnth+holiday+workingday,data = datann,hidden = 1, act.fct = "logistic",linear.output = FALSE)
plot(nn)

pred_nn = compute(nn,test_set)
pred_nn$net.result

prob <- pred_nn$net.result
pred <- ifelse(prob>0.5, 1, 0)
pred

result = data.frame(actual = test_set$cnt, prediction = pred_nn$net.result)

attach(result)
actual = test_set$cnt
prediction = pred
table(actual,prediction)

roc(actual,prediction,plot = TRUE)


#--------------------------SMOOTHING SPLINE------------------------------
fit1 = smooth.spline(cnt,season,df = 16)
plot(cnt,season,col = "grey",xlab = cnt, ylab = season)
#points(cnt.grid,predict(r,newdata = list(cnt = cnt.grid)),col = "darkgreen",lwd = 2,type = 1)
lines(fit1,col = "red",lwd = 2)
legend("topright",c("Smoothing Spline with 16 df","Cubic Spline"),col = c("red","darkgreen"),lwd = 2)

fit2 = smooth.spline(cnt,season,cv = TRUE)
fit2
plot(cnt,season,col="grey")
lines(fit2,lwd = 2, col = "purple")
legend("topright",("Smoothing Splines with 6.78 df selected by CV"),col="purple",lwd=2)

#----------------BOOSTING---------------------------
library(caret)
set.seed(1143)

validation <- training_set[1:100,]

mdl3 <- train(cnt ~ . + I(atemp^2) + I(hr^2), data=training_set[1:100,], method='gbm', verbose=F)
p3 <- predict(mdl3, validation)
p3[p3<0] <- 0
s3 <- sqrt(sum((p3-validation$cnt)^2))
s3

library(Cubist)
set.seed(1143)
mdl4 <- train(cnt ~ . + I(atemp^2) + I(hr^2), data=training_set[1:50,], method='cubist')
p4 <- predict(mdl4, validation)
p4[p4<0] <- 0
s4 <- sqrt(sum((p4-validation$cnt)^2))
s4


