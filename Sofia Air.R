setwd("C:\\Users\\Georgi\\Desktop\\Data science\\R-working\\case_study")

library(dplyr)
library(lubridate)
library(imputeTS)
library(stringr)
library(ggplot2)
library(leaps)
library(psych)

library(MASS)
library(tree)
library(randomForest)

library(mgcv)

#Data prep---------------------------------------------------------------------------------------------------------------------------------------
#Bulk import of data
files = list.files("C:\\Users\\Georgi\\Desktop\\Data science\\R-working\\case_study")

data = lapply(files,read.csv,na.strings = c("","NA"," ",-9999),stringsAsFactors = F)

names(data) = files

data2017 = data$st9421_2017.csv
data2018 = data$st9421_2018.csv
weather = data$weather.csv

#Check distributions
windows()
ggplot(data2017,aes(x=data2017$P1eu)) +
  geom_density()

windows()
ggplot(data2018,aes(x=data2018$P1eu)) +
  geom_density()

#Count number of missig and number of duplicates
sum(duplicated(data2017$time))
sum(duplicated(data2018$time))

colSums(is.na(data2017))
colSums(is.na(data2018))

#Visualize missing data to check if there are any patterns
missing17 = data2017 %>%
  filter(is.na(P1eu)==T) %>%
  mutate(day = wday(time,label=T),hour = hour(time))

table(missing17$day)
table(missing17$hour)

missing18 = data2018 %>%
  filter(is.na(P1eu)==T) %>%
  mutate(day = wday(time,label=T),hour = hour(time))

table(missing18$day) 
table(missing18$hour)

#Deal with missing values
x = median(data2017$P1eu,na.rm = T)
y = median(data2018$P1eu,na.rm = T)

#data2017$P1eu = ifelse(is.na(data2017$P1eu)==T,x,data2017$P1eu)

#data2018$P1eu = ifelse(is.na(data2018$P1eu)==T,y,data2018$P1eu)
data2018 = na.omit(data2018)
data2017 = na.omit(data2017)

#mix dataframes together
newdata = bind_rows(data2017,data2018)

newdata$time = ymd_hms(newdata$time)

newdata$year = year(newdata$time)
newdata$month = month(newdata$time)
newdata$day = day(newdata$time)

newdata$date = make_date(year = newdata$year, month = newdata$month,day = newdata$day)

newdata = newdata[-c(2,4,5,6)]

windows()
ggplot(newdata,aes(x=P1eu)) +
  geom_density()

#Summarize by day
newdata = newdata %>%
  group_by(date) %>%
  summarize(pm10 = mean(P1eu))

newdata = newdata[c(2,1)]

#Get dates in weather dataset
weather$date = make_date(year = weather$year,month = weather$Month,day = weather$day)

#Obtain the cleaned dataset
dataset = left_join(newdata,weather,by = "date")

dataset = dataset[-c(3:5)]

dataset = dataset[c(2,1:ncol(dataset))]

dataset = dataset[-3]

View(dataset)

#Check missing
colSums(is.na(dataset))

#remove missing columns
aux = c("PRCPMAX","PRCPMIN","VISIB")

dataset = dataset[,!names(dataset) %in% aux]

#Interpolation
summary(dataset)

dataset[c(3:(ncol(dataset)))] = sapply(dataset[c(3:(ncol(dataset)))]
                                       ,na_interpolation,option="linear")

#trend
windows()
ggplot()+
  geom_line(data = dataset,aes(x = as.integer(rownames(dataset)),y = pm10))

#Feature engineering------------------------------------------------------------------------------------------------------
windows()
cor.plot(dataset[-1])

#PCA for temperature-dew
PC_a = prcomp(dataset[3:8])

names(PC_a)

summary(PC_a) #95% of variance absorbed

PC_a$rotation 

dataset$temperature = PC_a$x[,1]

#PCA for pressure
PC_b = prcomp(dataset[15:17])


summary(PC_b) #93 % of variance absorbed

PC_b$rotation 

dataset$pressure = PC_b$x[,1]

#PCA for wind
PC_c = prcomp(dataset[6:8])


summary(PC_c) #96 % of variance absorbed

PC_c$rotation 

dataset$wind = PC_c$x[,1]

#Remove unnecessary values
names(dataset)

aux = c("TASMAX"  ,  "TASAVG" , "TASMIN"  ,  "DPMAX"  , "DPAVG" ,"DPMIN")

dataset = dataset[,!names(dataset) %in% aux]

aux = c("PSLMAX"    , "PSLAVG"  ,  "PSLMIN" )

dataset = dataset[,!names(dataset) %in% aux]

aux = c("sfcWindMAX", "sfcWindAVG","sfcWindMIN")

dataset = dataset[,!names(dataset) %in%aux]


#Binnig humidity
dataset$humidity = ifelse(dataset$RHMAX==100,1,0)

aux = c( "RHMAX"  ,   "RHAVG"   , "RHMIN" )

dataset = dataset[,!names(dataset) %in% aux]

#Binning PRCP
dataset$precip = ifelse(dataset$PRCPAVG==0,0,1)

aux = "PRCPAVG"

dataset = dataset[,!names(dataset) %in% aux]

#stationarity test
windows()
acf(dataset$pm10)

adf = adf.test(dataset$pm10)
print(adf)

#Lagged variables

dataset$lagged1 = dplyr::lag(dataset$pm10,n=1)

#add synergies

dataset$syn1 = dataset$pressure*dataset$temperature #athmospheric pressure and temperature synergy

#test correlation for new dataset
windows()
cor.plot(dataset[-1])

#Check plots
windows()
ggplot(dataset,aes(x=dataset$pressure,y=dataset$pm10))+
  geom_point(size=2)

windows()
ggplot(dataset,aes(x=dataset$temperature,y=dataset$pm10))+
  geom_point(size=2)

windows()
ggplot(dataset,aes(x=dataset$wind,y=dataset$pm10))+
  geom_point(size=2)

#remove date
rownames(dataset) = dataset$date

dataset = dataset[-1]

#Modeling------------------------------------------------------------------------------------------------------------------------
#Train test split
set.seed(25)
train = sample(nrow(dataset),round(nrow(dataset)*3/4))

#create tree
eq1 = tree(pm10~.,data = dataset,subset=train)

#visualize tree
windows()
plot(eq1)
text(eq1)

summary(eq1)

windows()
hist(summary(eq1)$residuals)

#Prune tree
aux1 = cv.tree(eq1)

aux1$size
aux1$dev

peq1 = prune.tree(eq1,best=5)

windows()
plot(peq1)
text(peq1)

names(dataset)
#Random forest
#Random forest can't work with missing values
dataset$lagged1[1] = x

eq2 = randomForest(pm10~lagged1 + wind + pressure + temperature + precip +
                     humidity+syn1,
                   data = dataset, subset = train, importance = T,nTree = 50000)


#eq2 = randomForest(pm10~lagged1+wind+pressure+temperature+PRCPAVG,
                   #data = dataset, subset = train, importance = T)

windows()
varImpPlot(eq2,type=1)

# Validate results
#test error on training set:
(MSEeq1_in = mean((dataset$pm10[train]-predict(eq1,newdata = dataset[train,]))^2))
#test error outside training set:
(MSEeq1 = mean((dataset$pm10[-train]-predict(eq1,newdata = dataset[-train,]))^2))

(MSEpeq1 = mean((dataset$pm10[-train]-predict(peq1,newdata = dataset[-train,]))^2))

#random forrest in sample error:
(MSEeq2_in = mean((dataset$pm10[train]-predict(eq2,newdata = dataset[train,]))^2))

#random forrest out of sample error:
(MSEeq2 = mean((dataset$pm10[-train]-predict(eq2,newdata = dataset[-train,]))^2))

sqrt(MSEeq2)

summary(eq2)

#Visualization of randomForest results
a = predict(eq2,newdata = dataset[-train,])
b = dataset$pm10[-train]
c = seq(1:71)

prediction = data.frame("actual"=b,"predicted"=a)

prediction$index = c

windows()
ggplot() + 
  geom_line(data = prediction, aes(x = index, y = actual), color = "blue",size=2) +
  geom_line(data = prediction, aes(x = index, y = predicted), color = "red",size=2)

#Try logarithmic values:::



#Check linear model
eq4 = glm(pm10~lagged1 + wind + pressure + temperature +
           humidity+syn1 + precip,data = dataset,subset=train)

summary(eq4)

#regression in sample error:
(MSEeq4_in = mean((dataset$pm10[train]-predict(eq4,newdata = dataset[train,]))^2))

#regression out of sample  error
(MSEeq4 = mean((dataset$pm10[-train]-predict(eq4,newdata = dataset[-train,]))^2))

cv10 = cv.glm(dataset,eq4,K=10)

print(cv10$delta[1])

#Visualization of regression results
a = predict(eq4,newdata = dataset[-train,])
b = dataset$pm10[-train]
c = seq(1:71)

prediction = data.frame("actual"=b,"predicted"=a)

prediction$index = c

windows()
ggplot() + 
  geom_line(data = prediction, aes(x = index, y = actual), color = "blue",size=2) +
  geom_line(data = prediction, aes(x = index, y = predicted), color = "red",size=2)





