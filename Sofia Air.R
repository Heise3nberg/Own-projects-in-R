setwd("C:\\Users\\Georgi\\Desktop\\Data science\\R-working\\case_study")

library(dplyr)
library(lubridate)
library(imputeTS)
library(stringr)
library(ggplot2)
library(leaps)
library(psych)

library(tseries)
library(forecast)
library(MASS)
library(tree)
library(randomForest)

library(mgcv)
library(binaryLogic)
library(TSA)
library(boot)

library(plotrix)
library(synthpop)
library(class)
library(caret)
library(leaps)
library(scorecard)
library(Information)
library(tidymodels)
library(Metrics)
library(ranger)

#Data prep-------------------------------------------------------------------------------------------------------------------------------------------
#Bulk import of data
files = list.files("C:\\Users\\Georgi\\Desktop\\Data science\\R-working\\case_study") #create a list of files to import

data = lapply(files,read.csv,na.strings = c("","NA"," ",-9999),stringsAsFactors = F)  #import the list

#add the name of the file for each csv
names(data) = files 

#get the data tables
data2017 = data$st9572_2017.csv
data2018 = data$st9572_2018.csv
weather = data$weather.csv

#Check trends
windows()
ggplot()+
    geom_line(data = data2017,aes(x = as.integer(rownames(data2017)),y = P1eu),size=1.5)

windows()
ggplot()+
    geom_line(data = data2018,aes(x = as.integer(rownames(data2018)),y = P1eu),size=1.5)

#Check for duplicates
sum(duplicated(data2017$time))
sum(duplicated(data2018$time))

#Check for NAs
colSums(is.na(data2017))
colSums(is.na(data2018)) #there are a total if 316 NAs

#Visualize missing data to check if there are any patterns
missing17 = data2017 %>%
    filter(is.na(P1eu)==T) %>%
    mutate(day = wday(time,label=T),hour = hour(time))

#missing weekdays 2017
table(missing17$day) #wednesday and sunday tend to have more missing

#missing hour 2017
table(missing17$hour) #11.00 has a lot more missing vars 

#missing for 2018
missing18 = data2018 %>%
    filter(is.na(P1eu)==T) %>%
    mutate(day = wday(time,label=T),hour = hour(time))

table(missing18$day) #no signifficant pattern
table(missing18$hour) #11.00 and 00.00 

#Deal with missing values
#Omit NAs
data2018 = na.omit(data2018)
data2017 = na.omit(data2017)

#mix dataframes together
newdata = bind_rows(data2017,data2018)

#investigate formats
sapply(newdata,class)

#transform time column
newdata$time = ymd_hms(newdata$time)

#extract date
newdata$year = year(newdata$time)
newdata$month = month(newdata$time)
newdata$day = day(newdata$time)

#create a date column:
newdata$date = make_date(year = newdata$year, month = newdata$month,day = newdata$day)

#remove redundant columns
names(newdata)
newdata = subset(newdata,select = -c(time, year , month, day,X))

#aggregate data by day
newdata = newdata %>%
    group_by(date) %>%
    summarize(pm10 = mean(P1eu))

#Get dates in weather dataset
weather$date = make_date(year = weather$year,month = weather$Month,day = weather$day)

#Obtain the cleaned dataset
dataset = left_join(newdata,weather,by = "date")

#remove redundant columns
names(dataset)
dataset = subset(dataset,select = -c(year,Month,day))

View(dataset)

#Check missing
colSums(is.na(dataset))

#remove missing columns
aux = c("PRCPMAX","PRCPMIN","VISIB")

dataset = dataset[,!names(dataset) %in% aux]

#summary
summary(dataset$pm10)

#Interpolation to fill in missing observations
dataset[c(3:(ncol(dataset)))] = sapply(dataset[c(3:(ncol(dataset)))]
                                       ,na_interpolation,option="linear")

#Check missing again
colSums(is.na(dataset))

#PM10 trend
windows()
ggplot()+
    geom_line(data = dataset,aes(x = as.integer(rownames(dataset)),y = pm10),size=1.5)

#distribution
windows()
hist(dataset$pm10,xlab = "rowindex",main="Distribution of pm10")

#Feature engineering---------------------------------------------------------------------------------------------------------------------------------
#analyze correlations
windows()
cor.plot(dataset[-1])

#remove dew point as it is mutually correlated with temperature
aux = c("DPMAX"  , "DPAVG" ,"DPMIN")

dataset = dataset[,!names(dataset) %in% aux]

#stationarity test
windows()
acf(dataset$pm10,main = "ACF of pm10") #slow decline in autocor, data looks stationary

windows()
pacf(dataset$pm10,main = "PACF of pm10")

#dickey-fuller test
adf = adf.test(dataset$pm10)
print(adf)

#estimate the order of an arima model
Arima = auto.arima(dataset$pm10)
print(Arima)

#Create lagged variable
dataset$lagged1 = dplyr::lag(dataset$pm10,n=1)

#moving average component
wdw = 4
dataset$MA = NA

for (i in 4:(nrow(dataset))) {
    
    dataset$MA[i] = mean(dataset$pm10[i:(i-wdw)])
}
dataset$MA[which(is.na(dataset$MA==T))] = mean(dataset$pm10[is.na(dataset$MA)==T])

#binning wind speed:
dataset$no_wind = ifelse(dataset$sfcWindMIN==0,1,0)

#Binnig humidity
dataset$humidity = ifelse(dataset$RHMAX==100,1,0)

#Binning PRCP
dataset$precip = ifelse(dataset$PRCPAVG==0,1,0)

#synergystic effect of wind speed, humidity and precipitation:
dataset$D = dataset$precip * dataset$humidity * dataset$no_wind

#wind ratio
dataset$C = lag(dataset$sfcWindAVG,n=1) * dataset$sfcWindAVG

#lagged wind ratio
dataset$R = dataset$lagged1/dataset$C

#athmospheric pressure and temperature synergy
dataset$syn1 = dataset$PSLAVG * dataset$TASAVG

#athmospheric pressure and humidity
dataset$syn2 = dataset$PSLAVG * dataset$RHAVG

#max pressure and temperature synergy
dataset$syn3 = dataset$PSLMAX * dataset$TASMAX

#max pressure and humidity synergy
dataset$syn4 = dataset$PSLMAX * dataset$RHMAX

#add days of the week variable
dataset$wday = weekdays(dataset$date)

dataset$wday = factor(dataset$wday,levels = 
                          c("Monday" ,"Tuesday", "Wednesday","Thursday" ,
                            "Friday","Saturday","Sunday"))

unique(dataset$wday)
#visualize weekday
windows()
ggplot(dataset, aes(x = wday, y = pm10)) + 
    geom_boxplot(fill = "gray90", color = "gray40", outlier.colour = "#00a2b1") +
    scale_y_continuous(expand = c(0,0), limits=c(0, max(dataset$pm10)*1.05))

#daily average chart:
daily = dataset %>%
        group_by(wday) %>%
        summarize(avg = mean(pm10))


windows()
ggplot(daily,aes(x=wday,y=avg)) + 
    geom_bar(stat = "identity",fill="darkgreen")


#add weekend variables
dataset$weekend = ifelse(dataset$wday=="Sunday" | dataset$wday=="Saturday",1,0)

#conver wday to numeric variables:
new_df = dataset[,"wday"]

new_df$wday = factor(new_df$wday, exclude = NULL)
new_df = model.matrix(~.-1, data = new_df[, "wday"])
new_df = as.data.frame(new_df)

colnames(new_df) = c("Monday" ,"Tuesday", "Wednesday","Thursday"
                     ,"Friday","Saturday","Sunday")

#sanity check:
new_df$day = dataset$wday
View(new_df)

#remove rendundant variable:
new_df = new_df[-which(names(new_df)=="day")]

#add encoded weekdays as varibles:
dataset = bind_cols(dataset,new_df)

aux = c("day", "wday")

#Create a variable for month
dataset$month = month(dataset$date)

#switch 12 with 0
dataset$month[which(dataset$month==12)] = 0

#remove the missing 1st row
dataset = na.omit(dataset)

#estimate best predictive subset
subs = regsubsets(pm10~. ,data=dataset[-1],nvmax=30,method="exhaustive")

summary(subs)$which
summary(subs)$adjr2

windows()
plot(summary(subs)$adjr2,type="l",col="blue") #optimal subset of features is 18-20

#remove redundant variables:
aux = c("TASAVG",  "RHMAX", "RHMIN", "PRCPAVG","sfcWindMAX","sfcWindMIN","PSLMAX" ,"PSLMIN")

dataset = dataset[,!names(dataset) %in% aux]

#test correlation for the new dataset
windows()
cor.plot(dataset[-c(which(names(dataset)=="date"),which(names(dataset)=="wday"))])

#remove redundant variables:
aux = c("syn2",  "syn3", "weekend","wday")

dataset = dataset[,!names(dataset) %in% aux]

#remove date
rownames(dataset) = dataset$date

dataset = subset(dataset, select = -date)

windows()
cor.plot(dataset[-c(17:23)])
#Modeling-------------------------------------------------------------------------------------------------------------------------------------------
#Train test split
set.seed(1)
train = sample(nrow(dataset),round(nrow(dataset)*3/4))

#Create tree to test varibles
eq1 = tree(pm10~TASMAX+TASMIN+sfcWindAVG+PSLAVG+no_wind+D+syn1+
                                 R+MA,data = dataset,subset=train)
#visualize tree
windows()
plot(eq1)
text(eq1)

summary(eq1)

#tree residuals
windows()
hist(summary(eq1)$residuals)

#Prune tree
aux1 = cv.tree(eq1)

aux1$size
aux1$dev #~8 features are most optimal

peq1 = prune.tree(eq1,best=7)

windows()
plot(peq1)
text(peq1)

#Random forest
eq2 = randomForest(pm10~TASMAX + TASMIN   + sfcWindAVG + PSLAVG + lagged1 + no_wind +  R  +D+syn1,
                   data = dataset, subset = train, importance = T,ntree = 25000,mtry=4) #mtry=4  TASMAX + TASMIN   + sfcWindAVG + PSLAVG + lagged1 + no_wind +  R  +D+syn1

windows()
varImpPlot(eq2,type=1)

#ARIMAX
#remove PSLAVG  lagged1 and syn1 from the model: , 2:13 are the original values
xreg = as.matrix(dataset[train,c("TASMAX","TASMIN","sfcWindAVG","PSLAVG" ,"no_wind" ,"humidity" ,"precip","D",
                                 "R","syn1","lagged1")])

xregt = as.matrix(dataset[-train,c("TASMAX","TASMIN","sfcWindAVG","PSLAVG" ,"no_wind" ,"humidity" ,"precip","D",
                                   "R","syn1","lagged1")])

eq3 = auto.arima(dataset[train, "pm10"], xreg = xreg)

pred = forecast(eq3, xreg = xregt)

summary(eq3)
pred$model

pred_eq3 = predict(eq3,newxreg=xregt)

tpred = predict(eq3,newxreg=xreg)

#ARIMAX residuals
windows()
hist(eq3$residuals)

#linear model for benchmarking
eq4 = lm(pm10~TASMAX + TASMIN   + PSLAVG  + no_wind +  R  +D+MA
         +Wednesday,data = dataset,subset=train)

summary(eq4)
#look at residuals
windows()
hist(summary(eq4)$residuals)

#plot train/test samples
windows()
plot(dataset$pm10[-train],type="l",col="blue")

windows() #plot training sample
plot(dataset$pm10[train],type="l",col="blue")

#define train control for k-fold cross validation################################################################
train_control = trainControl(method="cv", number=8)
#Fit model
model = train(pm10~TASMAX + TASMIN   + PSLAVG  + no_wind +  R  +D+lagged1+syn1
              ,data = dataset, trControl = train_control, method = "rf")

#Summarise Results
print(model)

(MSEeq5_in = mean((dataset$pm10[train]-predict(model,newdata = dataset[train,]))^2))
(MSEeq5_out = mean((dataset$pm10[-train]-predict(model,newdata = dataset[-train,]))^2))

# Validattion and benchmarking of results-----------------------------------------------------------------------------------------------------------------------------
#test error on training set:
(MSEeq1_in = mean((dataset$pm10[train]-predict(eq1,newdata = dataset[train,]))^2))
#test error outside training set:
(MSEeq1 = mean((dataset$pm10[-train]-predict(eq1,newdata = dataset[-train,]))^2)) #the model is overfit  due to many features

#random forrest in sample error:
(MSEeq2_in = mean((dataset$pm10[train]-predict(eq2,newdata = dataset[train,]))^2))

#random forrest out of sample error:
(MSEeq2 = mean((dataset$pm10[-train]-predict(eq2,newdata = dataset[-train,]))^2))

#dynamic regression in sample error:
(MSEeq3 = mean((dataset$pm10[train]-tpred$pred)^2))

#dynamic regression out of sample  error
(MSEeq3 = mean((dataset$pm10[-train]-pred_eq3$pred)^2))

#linear regression in sample error:
(MSEeq4_in = mean((dataset$pm10[train]-predict(eq4,newdata = dataset[train,]))^2))

#linear regression out of sample  error
(MSEeq4 = mean((dataset$pm10[-train]-predict(eq4,newdata = dataset[-train,]))^2))

#naive forecast for benchmarking
n1MSEeq2_out = mean((dataset$pm10[-train]-mean(dataset$pm10))^2) #using the mean as a forecast

n2MSEeq_out = mean((dataset$pm10[-train]-dataset$lagged1[-train])^2) #using yesterdays value for a forecast

#pruned tree in sample:
(MSEpeq1_in = mean((dataset$pm10[train]-predict(peq1,newdata = dataset[train,]))^2))

#pruned tree out of sample:
(MSEpeq1 = mean((dataset$pm10[-train]-predict(peq1,newdata = dataset[-train,]))^2))

#Visualization of Arimax results
a = pred_eq3$pred
b = dataset$pm10[-train]
c = seq(1:72)

prediction = data.frame("actual"=b,"predicted"=a)
prediction$index = c

windows()
ggplot() + 
    geom_line(data = prediction, aes(x = index, y = actual), color = "blue",size=2) +
    geom_line(data = prediction, aes(x = index, y = predicted), color = "red",size=2)

#Visualization of Random Forest results
e = predict(eq2,newdata = dataset[-train,])
f = dataset$pm10[-train]
g = seq(1:72)

prediction1 = data.frame("actual"=f,"predicted"=e)
prediction1$index = g

windows()
ggplot() + 
    geom_line(data = prediction1, aes(x = index, y = actual), color = "blue",size=2) +
    geom_line(data = prediction1, aes(x = index, y = predicted), color = "red",size=2)

#Classification-----------------------------------------------------------------------------------------------------------------------------------------
#Data prep
#binning pm10 for classification purposes
dataset = dataset %>%
    mutate(bpm10 = case_when(
        dataset$pm10 <= 50 ~ 0,
        dataset$pm10 > 50 ~ 1
    ))

#remove old variable
dataset = subset(dataset,
                 select = -c(pm10))

#rearange:
dataset = dataset[c(ncol(dataset),1:ncol(dataset)-1)]

#Check target variable distribution
sum(dataset$bpm10 == 0)
sum(dataset$bpm10 == 1)

#Information analysis
IV = create_infotables(data=dataset, y="bpm10", bins = 3)
print((IV$Summary),
      row.names=FALSE)

#Graph balance in the target variable
lbs = c("OK",
        "Dirty")
colors = c("green",
           "lightblue")
daysinfo = c(Acceptable = sum(dataset$bpm10 == 0),
             Dirty = sum(dataset$bpm10 == 1))
windows()
pie3D(daysinfo,
      labels = lbs,
      main = "Daily Air Quality",
      col = colors) #We have a relatively sparse sample

sum(dataset$bpm10 == 1) / sum(dataset$bpm10 == 0) #13% of the records are > 50: 
                                                  #this will be problematic when we split the sample

#Create synthetic observations to deal with the sparcity issue:
ones = filter(dataset,bpm10 == 1)

synthetic = syn(ones, method = "cart",visit.sequence = (1:ncol(dataset)),m = 4)

synthetic2 = synthetic$syn

ones = rbind(synthetic2[[1]],
             synthetic2[[2]])

#merge data
syn_dataset = rbind(dataset,ones)

#a quick peak at the new values:
sum(syn_dataset$bpm10 == 0)
sum(syn_dataset$bpm10 == 1)

#Visualize balanced sample
daysinfo = c(Acceptable = sum(syn_dataset$bpm10 == 0),
             Dirty = sum(syn_dataset$bpm10 == 1))
windows()
pie3D(daysinfo,
      labels = lbs,
      main = "Daily Air Quality",
      col = colors)

#Classification Modelling and benchmarking--------------------------------------------------------------------------------------------------------------------------:
#train/test split
set.seed(1)
syn_train = sample(nrow(syn_dataset),round(nrow(syn_dataset)*3/4))

#Sampling for original
target_train = dataset[train, 1]
target_test = dataset[-train, 1]

#Sampling for synthethic
syn_target_train = as.data.frame(syn_dataset[syn_train, 1])
syn_target_test = syn_dataset[-syn_train, 1]

#switch datatype from numeric to factor
dataset$bpm10 = factor(dataset$bpm10,levels=c(0,1))
syn_dataset$bpm10 = factor(syn_dataset$bpm10,levels=c(0,1))

#Normalization of the variables:
normalize = function(x) {
    return ((x - min(x)) / (max(x) - min(x))) }

data_n = as.data.frame(lapply(syn_dataset[2:ncol(syn_dataset)],normalize))

data_n$bpm10 = syn_dataset$bpm10

#Random forest model with normalized synthetic features:
eq5 = randomForest(bpm10~TASMAX + TASMIN   + sfcWindAVG + PSLAVG + lagged1 + no_wind +R +D,
                   data = data_n, subset = syn_train, importance = T,ntree = 25000,mtry=4)

#Conf matrix random forest:
cf5 = confusionMatrix(table(predict(eq5, newdata = data_n[-syn_train,], type = "class"),syn_target_test$bpm10),dnn = c('Predicted', 'Actual'))

windows()
varImpPlot(eq5)

#Random forest model with generic features:
eq6 = randomForest(bpm10~TASMAX+TASMIN  + sfcWindAVG + PSLAVG + lagged1 + no_wind +R +D,
                   data = dataset, subset = train, importance = T,ntree = 50000,mtry=4)

#Conf matrix random forest:
confusionMatrix(table(predict(eq6, newdata = dataset[-train,], type = "class"),target_test$bpm10))

windows()
varImpPlot(eq6)

#Random forest model with synthetic features(but not normalized):
eq7 = randomForest(bpm10~TASMAX + TASMIN   + sfcWindAVG + PSLAVG + lagged1 + no_wind +R +D,
                   data = syn_dataset, subset = syn_train, importance = T,ntree = 50000,mtry=4)


#Conf matrix random forest:
cf7 = confusionMatrix(table(predict(eq7, newdata = syn_dataset[-syn_train,], type = "class"),syn_target_test$bpm10))

windows()
varImpPlot(eq7)

#Logistic regression model for benchmarking:
eq8 = glm(bpm10~TASMAX + TASMIN   + PSLAVG  + no_wind +  R  +D+MA
          +Wednesday,data = syn_dataset,family = "binomial",subset = syn_train)
eq8
summary(eq8)

Estimated_Probability = predict(eq8,type="response",newdata = syn_dataset[-syn_train,])
Real_probability = syn_target_test$bpm10
Score = data.frame(Estimated_prob = Estimated_Probability , Real = Real_probability)

Score$Estimated = ifelse(Score$Estimated_prob>=0.5,1,0)

#Conf matrix logistic reg:
confusionMatrix(table(Score$Estimated,Score$Real)) #fix confusion matrix diagonals

#Knn algorithm for benchmarking
#pick features:
data_knn = as.data.frame(lapply(syn_dataset[c(2,3,5,6,7,9,12,14,15)],normalize)) #picked features are: TASMAX TASMIN sfcWindAVG PSLAVG  lagged1  no_wind R D syn1

x_train = data_knn[syn_train, ]
x_test = data_knn[-syn_train, ]

kNN = knn(train = x_train,test = x_test,cl = syn_target_train$bpm10,k=sqrt(nrow(x_train)))

confusionMatrix(table(kNN,syn_target_test$bpm10))

#Show results of matrix of eq7:
print(cf5$byClass)

windows()
barplot(cf5$byClass)

#Baseline prob (TER)
length(syn_dataset$bpm10[syn_dataset$bpm10 == 1])/nrow(syn_dataset)

#conf matrix visualization:-----
draw_confusion_matrix <- function(cm) {
  
  layout(matrix(c(1,1,2)))
  par(mar=c(2,2,2,2))
  plot(c(100, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
  title('CONFUSION MATRIX', cex.main=2)
  
  # create the matrix 
  rect(150, 430, 240, 370, col='#3F97D0')
  text(195, 435, 'Class1', cex=1.2)
  rect(250, 430, 340, 370, col='#F7AD50')
  text(295, 435, 'Class2', cex=1.2)
  text(125, 370, 'Predicted', cex=1.3, srt=90, font=2)
  text(245, 450, 'Actual', cex=1.3, font=2)
  rect(150, 305, 240, 365, col='#F7AD50')
  rect(250, 305, 340, 365, col='#3F97D0')
  text(140, 400, 'Class1', cex=1.2, srt=90)
  text(140, 335, 'Class2', cex=1.2, srt=90)
  
  # add in the cm results 
  res <- as.numeric(cm$table)
  text(195, 400, res[1], cex=1.6, font=2, col='white')
  text(195, 335, res[2], cex=1.6, font=2, col='white')
  text(295, 400, res[3], cex=1.6, font=2, col='white')
  text(295, 335, res[4], cex=1.6, font=2, col='white')
  
  # add in the specifics 
  plot(c(100, 0), c(100, 0), type = "n", xlab="", ylab="", main = "DETAILS", xaxt='n', yaxt='n')
  text(10, 85, names(cm$byClass[1]), cex=1.2, font=2)
  text(10, 70, round(as.numeric(cm$byClass[1]), 3), cex=1.2)
  text(30, 85, names(cm$byClass[2]), cex=1.2, font=2)
  text(30, 70, round(as.numeric(cm$byClass[2]), 3), cex=1.2)
  text(50, 85, names(cm$byClass[5]), cex=1.2, font=2)
  text(50, 70, round(as.numeric(cm$byClass[5]), 3), cex=1.2)
  text(70, 85, names(cm$byClass[6]), cex=1.2, font=2)
  text(70, 70, round(as.numeric(cm$byClass[6]), 3), cex=1.2)
  text(90, 85, names(cm$byClass[7]), cex=1.2, font=2)
  text(90, 70, round(as.numeric(cm$byClass[7]), 3), cex=1.2)
  
  # add in the accuracy information 
  text(30, 35, names(cm$overall[1]), cex=1.5, font=2)
  text(30, 20, round(as.numeric(cm$overall[1]), 3), cex=1.4)
  text(70, 35, names(cm$overall[2]), cex=1.5, font=2)
  text(70, 20, round(as.numeric(cm$overall[2]), 3), cex=1.4)
}  


windows()
draw_confusion_matrix(cf5)





