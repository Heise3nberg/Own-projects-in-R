# Load libraries
library(foreign)
library(tibble) 
library(dplyr)

library(lubridate)
library(imputeTS)
library(stats)
library(psych)
library(dendextend)
library(nFactors)
library(DescTools)

library(stringr)
library(SnowballC)
library(quanteda)
library(GPArotation)
library(ggplot2)
library(arm)

rm(list = ls())
setwd("C:\\Users\\Georgi\\Desktop\\Data science\\R-working")
load("nn.RData")

#Import the needed data from the module EP and DN from SHARE research files
incl = c("mergeid","country","dn003_","dn010_","ep005_","ep205e","dn010_","dn042_")

EP = dds$ep[,names(dds$ep) %in% incl]
DN = dds$dn[,names(dds$dn) %in% incl]

#Explore the cleaned data
View(DN)
View(EP)

View(dds$ep)
View(dds$dn)

table(EP$ep205e)

colSums(is.na(EP))

#Extract ID and Salary
a = as.data.frame(EP[c(1,4)])

a = na.omit(a)

#a = as.data.frame(a$ep205e[-which(is.na(a$ep205e)==T)])
#Get a quick peek at the variables
colnames(a) = c("mergeid","Salary")

#Get rid of NAs and null values
for (i in 1:nrow(a)) {  
    
    if (a$Salary[i] ==  "Refusal")
        a$Salary[i] = 0
}

for (i in 1:nrow(a)) {  
    
    if (a$Salary[i] ==  "Don't know")
        a$Salary[i] = 0
}

a$Salary[which(a$Salary==0)] = NA

a = na.omit(a)

#Set correct data type
a$Salary = as.numeric(as.character(a$Salary))

c = lapply(a$Salary,class)

unique(c)

windows()
hist(a$Salary,breaks = 30,main = "Salary distribution",xlim = c(400,200000))

#Merge tables and create the tables for the analytics
aux = left_join(EP,DN[c(1,3,4,5)],by="mergeid")

als = left_join(a,aux,by="mergeid")

#Check missing values
colSums(is.na(als))

#Unfortunately we'll have to get rid of the education variable, because most of the information is missing :(
als = als[-c(5,7)]

colnames(als) = c("personid","salary","country","jobstatus","Age","gender")

#Calculate age
sapply(als,class)

als$Age = as.numeric(as.character(als$Age))

for (i in 1:length(als$Age)) { 
    als$Age[i] = 2020 - als$Age[i]
}

table(als$jobstatus)

#Income analysis----------------------------------------------------------------------

inc = als

#Cleanse useless values
inc = lapply(inc, function(x) {
    gsub("Retired", NA, x)})

inc = lapply(inc, function(x) {
    gsub("Unemployed", NA, x)})

inc = lapply(inc, function(x) {
    gsub("Homemaker", NA, x)})

inc = lapply(inc, function(x) {
    gsub("Permanently sick or disabled", NA, x)})

inc = lapply(inc, function(x) {
    gsub("Other", NA, x)})

inc = lapply(inc, function(x) {
    gsub("NA", NA, x)})


inc = as.data.frame(inc)

inc = na.omit(inc)

sapply(inc,class)

#Check for duplicates
sum((duplicated(inc$personid)==T))

inc$salary = as.numeric(as.character(inc$salary))

#Check for discrepancies
inc$salary[which(inc$salary <= 0)]

which(inc$salary <= 0)

inc = inc[-56,]

#Begin Exploratory analysis----------------------------------------------------------------
#Plot 
boxplot(
    inc$salary ~ inc$gender,
    xlab = "gender",
    ylab = "pay",
    col = c("tan1", "steelblue2", "lightpink2"),
    main = "Salaries for genders"
)

#Maybe it will be a good idea to check the outliers
#Analyze outliers
q = quantile(inc$salary,probs = c(0.01,0.99))

print(q)

inc$Outliers = ifelse(inc$salary<=q[1] | inc$salary>=q[2],"outlier","")

inc_outliers = inc[which(inc$Outliers=="outlier"),]

View(inc_outliers)

table(inc_outliers$gender)

#Calculated average monthly payment for males and females
gender_pay = inc  %>%
    group_by(gender) %>%
    summarize(Pay = (mean(salary)/12))

print(paste("The average pay for women is:",gender_pay$Pay[1]))
print(paste("The average pay for men is:",gender_pay$Pay[2]))

#The 1st percentile is mainly consisten of men, we will anaylize that later
#Remove unnecessary columns
inc = inc[-c(4,7)]

#How important are Age, Gender and Country for the salary
inc1 = inc

#Encode gender and coutry values with numbers for factor analysis
table(inc1$country)

#Remaining data features 11 EU countries, generate a function to encode them:
inc1$country =  match(inc1$country, unique(inc1$country))

#Now to encode Male/Female with 0/1
inc1$gender = as.numeric(inc1$gender)

for (i in 1:nrow(inc1)) {if (inc1$gender[i]==2)
    inc1$gender[i] = 0}

sapply(inc1, class)
inc1$Age = as.numeric(as.character(inc1$Age))

#Factor analysis
c = cor(inc1[2:ncol(inc1)])

cor.plot(c)

EV = eigen(c)

AP = parallel(subject = nrow(inc1[,2:5]), var = ncol(inc1[,2:5]),rep = 100,cent = .05)

NS = nScree(x = EV$values, aparallel = AP$eigen$qevpea)

windows()
plotnScree(NS)

#Principal component analysis in order to better determine the number of underlying factors

inc2 = as.data.frame(scale(inc1[2:5]),center=T,scale=T)

PC = prcomp(inc2[2:4])

names(PC)
summary(PC) #There are some latent variables in the dataset, but none of them is too significant

#2 or 3 significant factors can be found in the data, one with 1.4EV and one with 1.2EV

fa = fa(inc1[2:5],nfactors = 3) #One factor which equals 18% of the variation the dataset
print(fa)

windows()
fa.diagram(fa)

fa$loadings
fa$weights #Age and gender are most significant for salary

#Now lets examine how salaries differ by gender
#Wage distribution modeling----------------------------------------------------

table(inc1$gender) # we have 388 males and 496 females

male_salaries  = data.frame(Salary=rep(0,1000))
female_salaries = data.frame(Salary=rep(0,1000))

for (i in 1:nrow(inc1)) {
    if (inc1$gender[i]==0)
        inc1$salary[i] -> male_salaries$Salary[i]
    else inc1$salary[i] -> female_salaries$Salary[i]
}

male_salaries$Salary[which(male_salaries$Salary==0)] = NA
female_salaries$Salary[which(female_salaries$Salary==0)] = NA

male_salaries = na.omit(male_salaries)
female_salaries = na.omit(female_salaries)

qqnorm(male_salaries$Salary,plot.it=TRUE, datax=TRUE)
qqnorm(female_salaries$Salary,plot.it=TRUE, datax=TRUE)

windows()
hist(male_salaries$Salary,breaks = 20,main = "Male salary distribution") #Outliers are much more likely in male distribution
windows()
hist(female_salaries$Salary,breaks = 20,main = "Female salary distribution") # We have a similar to poisson distribution

#Analyze positive outliers 
q1 = quantile(inc1$salary,probs = (0.975))

print(q1)

inc1$Outliers = ifelse(inc1$salary>=q1,"outlier","")

inc1_outliers = inc[which(inc1$Outliers=="outlier"),]

table(inc1_outliers$gender)

View(inc1_outliers)

#Transform to normal distributions
num_w =  496
num_m = 388

mu_w = gender_pay$Pay[1]
mu_m = gender_pay$Pay[2]

sig_w = sd(female_salaries$Salary)/12
sig_m = sd(male_salaries$Salary)/12

dist_w = rnorm(num_w,mu_w,sig_w)
dist_m = rnorm(num_m,mu_m,sig_m)

windows()
hist(dist_w,freq=T,breaks = 20,main = "Feale salary distribution")
windows()
hist(dist_m,freq=T,breaks = 20,main = "Male salary distribution") #Check outliers in male distribution again

wages = c(male_salaries$Salary, female_salaries$Salary)

gender = rep(c(0, 1), c(num_m, num_w))

print(cor(wages,gender))
print(cor(inc1$salary,inc1$country))

eq1 = lm(formula = wages ~ gender)
summary(eq1) #The average woman will make 7670 EUR per year less than the average male, or 639 EUR per month(all monetary amounts are in Euro)

#Calculate average male and female salary by country
by_country = inc %>%
    group_by(country,gender) %>%
    summarize(Pay = (mean(salary)/12))

cols = c("pink","blue")

windows()
barplot(by_country$Pay,names.arg = paste(by_country$gender,by_country$country,sep = "/"),las=2,col=cols,main="Gender pay difference")

#Calculate averages if we remove ourliers:
#Remove the 1st percentile
print(q)

inc$Outliers = ifelse(inc$salary<=q[1] | inc$salary>=q[2],"outlier","")

inc = inc[-which(inc$Outliers=="outlier"),]

gender_pay2 = inc %>%
    group_by(gender) %>%
    summarize(Pay = (mean(salary)/12))

View(gender_pay2)

#ANOVA by country and gender, with the outliers uncluded

anova = aov(salary~country + gender + Age,data = inc1)
summary(anova) #helps us to see directly the relavative weight of country, age and gender to the salary (F value)