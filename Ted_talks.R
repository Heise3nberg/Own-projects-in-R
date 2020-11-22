setwd("C:\\Users\\Georgi\\Desktop\\Data science\\R-working")

library(lubridate)
library(dplyr)
library(imputeTS)
library(stats)
library(psych)
library(dendextend)
library(nFactors)
library(DescTools)

library(sentimentr)
library(stringr)
library(SnowballC)
library(quanteda)
library(GPArotation)
library(ngram)
library(ggplot2)
library(ggfortify)

tedmain = read.csv("ted_main.csv",na.strings = c(""," ","NA"), stringsAsFactors = FALSE)
trans = read.csv("transcripts.csv",na.strings = c(""," ","NA"), stringsAsFactors = FALSE)

# Summarize the variable class for each data frame
mainclass = data.frame(names(tedmain),rapply(tedmain,class))

colnames(mainclass) = c("Varname","Varclass")

(length(unique(tedmain$name)))
(length(unique(tedmain$url)))

transclass = data.frame(names(trans),rapply(trans,class))

colnames(transclass)=c("Varname","Varclass")

(length(unique(trans$url)))

which(duplicated(trans$url)==T)

#Remove duplicats
trans = trans[-(which(duplicated(trans$url)==T)),]

#Identify matches and differences
url = list()

url$both = intersect(tedmain$url,trans$url)
(length(url$both)) 
url$tedmain = setdiff(tedmain$url,trans$url)
(length(url$tedmain)) 

#Merge dataframes
mergted = merge(trans,tedmain,by = "url")

#test count extr with Ken Robinson: Do schools kill creativity?

x = tedmain[1,]

# Derive the ratings attached to this video
rating = str_extract_all(x$ratings, "\\{[^{}]+\\}")

rating = unlist(rating)

rating = substring(rating, 2, nchar(rating)-1)

rating = strsplit(rating, split="\\, |\\: ")

krrt = data.frame(sapply(rating,"[",4),sapply(rating,"[",6))

colnames(krrt) = c("name","count")

sapply(krrt,class)

#Set data types
krrt$name = as.character(krrt$name)

krrt$count = as.numeric(as.character(krrt$count))

krrt = krrt[order(krrt$count,decreasing = T),]

#Extract all ratings

ratings_list = list() 

for (i in 1:nrow(mergted)){
  
  rating = str_extract_all(mergted$ratings[i], "\\{[^{}]+\\}")
  rating = unlist(rating)
  rating = substring(rating, 2, nchar(rating)-1)
  rating = strsplit(rating, split="\\, |\\: ")
  ratings_list[[i]] = data.frame(name = as.character(sapply(rating,"[",4)),count = 
                                   as.numeric(sapply(rating,"[",6)))

  ratings_list[[i]]$name = gsub("'","",ratings_list[[i]]$name)
  ratings_list[[i]] = ratings_list[[i]][order(ratings_list[[i]]$name),]
  
}

#Bind ratings with the merged dataframe
mergted[,(ncol(mergted)+1):(ncol(mergted)+14)]=NA
colnames(mergted)[19:ncol(mergted)] = ratings_list[[1]]$name

#Double loop

for (j in 1:14) {
  for(i in 1:nrow(mergted)){
    mergted[i,(18+j)] = ratings_list[[i]]$count[j]  
      
  }
  
}

#Proceed with FA-----------------------------
r = cor(mergted[,19:32])

windows()
cor.plot(r,numbers=T,las=2)

EV = eigen(cor(mergted[,19:32]))

AP = parallel(subject = nrow(mergted[,19:32]),var = ncol(mergted[,19:32]),
              rep = 100,cent = .05)

NS = nScree(x = EV$values, aparallel = AP$eigen$qevpea)

windows()
plotnScree(NS)

#There are 3 main factors
a = alpha(mergted[,19:32],check.keys=T)

print(a)

fa = fa(mergted[,19:32], nfactors = 3)

print(fa)

windows()
fa.diagram(fa)

fa$loadings
fa$weights

newdata = mergted[,c(9,17,19:32)]

#Group factors together
MR1 = c("Inspiring","Courageous","Beautiful","Persuasive","Informative") #Excellent video

MR2 = c("Unconvincing","Obnoxious","Confusing","Longwinded","OK") #Good video

MR3 = c("Ingenious","Fascinating","Jaw-dropping","Funny") #Bad video

#newdata$Excellent = sum(newdata[,"MR1"])

#Obtain "grades" for videos
newdata$Excellent = (0.7*newdata[,3]) + (0.9*newdata[,5]) + (0.4*newdata[,8]) + (0.9*newdata[,10]) + (0.5*newdata[,15])

newdata$Bad = (0.7*newdata[,4]) + (0.6*newdata[,12]) + (0.8*newdata[,13]) + (0.5*newdata[,14]) + (0.9*newdata[,16])

newdata$Good = (0.6*newdata[,6]) + (0.4*newdata[,7]) + (0.9*newdata[,9]) + (0.5*newdata[,11])

#Create dataframe for clustering
cluster_data = newdata[-c(3:16)]

colnames(cluster_data)[3:5] = c("Excellent","Bad","Good")

cluster_data$title = gsub("â€™","'s",newdata$title)

#switch title with author name

summary(cluster_data[3:5])

sapply(cluster_data,class)

#Clustering
#########################################################################---------------

cluster_data$Ratio = cluster_data[, "Excellent"] / cluster_data[, "Bad"]

cluster_data$Ratio[which(!is.finite(cluster_data$Ratio))] = 0

#PCA by Excellent and Bad ratings
pc_data = as.data.frame(scale(cluster_data[3:4]),center=T,scale=T)

PC = prcomp(pc_data)

names(PC)
summary(PC)

PC$x = -PC$x

PC$rotation = -PC$rotation

cluster_data2 = cluster_data[2]

#Derive "satisfaction index"
#We will use the 1st PC to make a FINAL CLUSTERRING(it carries 85% of the information)

cluster_data2$Satisfaction_index = PC$x[,1]

cluster_data2$Exc_Bad_ratio = cluster_data["Exc_Bad_ratio"]

View(cluster_data2)

cluster_data2$title = as.factor(cluster_data2$title)

cluster_data$title = as.factor(cluster_data$title)

colnames(cluster_data2[3]) = "Exc_Bad_Ratio"


#Remove outliers

out = quantile(cluster_data$Ratio,probs = c(0.025, 0.975))

cluster_data$outliers = ifelse(cluster_data$Ratio<= out[1]|cluster_data$Ratio>= out[2],"outlier",cluster_data$Ratio)

cluster_data = cluster_data[-which(cluster_data$outliers == "outlier"),]

cluster_data = cluster_data[-7]

cluster_data$title = as.factor(cluster_data$title)


km2 = kmeans(cluster_data$Ratio,centers = 3, nstart = 10, iter.max = 100)

plot(km2$centers)

windows()
plot(cluster_data$Ratio,cluster_data$title,pch=19,col=km2$cluster)

cluster_data$Clusters = km2$cluster

sapply(cluster_data,class)

#Put the 3 clusters in separate dataframes

bad_videos = cluster_data[which(cluster_data$Clusters == 1),]

impecable_videos = cluster_data[which(cluster_data$Clusters == 2),]

average_videos = cluster_data[which(cluster_data$Clusters == 3),]

#Average number of words per title per cluster

bad_videos$title = as.character(bad_videos$title)
impecable_videos$title = as.character(impecable_videos$title)
average_videos$title = as.character(average_videos$title)

n_words = wordcount(bad_videos$title, sep = " ", count.function = sum)
average_words = n_words/nrow(bad_videos)
print(paste("Bad videos avg words per title:",average_words))

n_words2 = wordcount(impecable_videos$title, sep = " ", count.function = sum)
average_words2 = n_words2/nrow(impecable_videos)
print(paste("Impecable videos avg words per title:",average_words2))

n_words3 = wordcount(average_videos$title, sep = " ", count.function = sum)
average_words3 = n_words3/nrow(average_videos)
print(paste("Average videos avg words per title:",average_words3))

#Analyze most common words in the BAD cluster

titles_list = list() 

titles_list = word(bad_videos$title)

unique_words = unique(titles_list)

count_words = rep(0, length(unique_words))


for (i in 1:length(unique_words)) {
  count_words[i] = sum(titles_list == unique_words[i])
}

top_10_order = order(count_words, decreasing = TRUE)[1:10]

top_10_freqs = sort(count_words, decreasing = TRUE)[1:10]

top_10_words = unique_words[top_10_order]

print(top_10_words)

windows()
barplot(top_10_freqs, border = NA, names.arg = top_10_words,
        las = 2, ylim = c(0,100))


#Analyze most common words in the impecable cluster
titles_list2 = list() 

titles_list2 = word(impecable_videos$title)

unique_words2 = unique(titles_list2)

count_words2 = rep(0, length(unique_words2))

for (i in 1:length(unique_words2)) {
  count_words2[i] = sum(titles_list2 == unique_words2[i])
}

top_10_order2 = order(count_words2, decreasing = TRUE)[1:10]

top_10_freqs2 = sort(count_words2, decreasing = TRUE)[1:10]

top_10_words2 = unique_words2[top_10_order2]

print(top_10_words2)

windows()
barplot(top_10_freqs2, border = NA, names.arg = top_10_words2,
        las = 2, ylim = c(0,100))


#Analyze most common words in the average cluster

titles_list3 = list() 

titles_list3 = word(average_videos$title)

unique_words3 = unique(titles_list3)

count_words3 = rep(0, length(unique_words3))


for (i in 1:length(unique_words3)) {
  count_words3[i] = sum(titles_list3 == unique_words3[i])
}

top_10_order3 = order(count_words3, decreasing = TRUE)[1:10]

top_10_freqs3 = sort(count_words3, decreasing = TRUE)[1:10]

top_10_words3 = unique_words3[top_10_order3]

print(top_10_words3)

windows()
barplot(top_10_freqs3, border = NA, names.arg = top_10_words3,
        las = 2, ylim = c(0,100))


#Sentiment analysis----------------------------
#Get video
dep = tedmain[1647,]

dep = merge(dep,trans,by = "url")

rating2 = str_extract_all(dep$ratings, "\\{[^{}]+\\}")
rating2 = unlist(rating2)
rating2 = substring(rating2, 2, nchar(rating2)-1)
rating2 = strsplit(rating2, split="\\, |\\: ")

depression = data.frame(sapply(rating2,"[",4),sapply(rating2,"[",6))

colnames(depression) = c("Name","Count")

depression$Name =  as.character(depression$Name)
depression$Count = as.numeric(as.character(depression$Count))

depression = depression[order(depression$Count,decreasing = T),]

#sentiments

sent = get_sentences(dep$transcript)

sent = unlist(sent)

depsent = sentiment(sent)

windows()
plot(depsent$sentiment,type="l",col="blue",lwd=2)

summary(depsent$sentiment)

#Sentiment words
sentwords = extract_sentiment_terms(sent)

depneg = unlist(sentwords$negative)

depnegs = wordStem(depneg)

depnegs = data.frame(table(depnegs))

depnegs = depnegs[order(depnegs$Freq,decreasing=T),]


deppos = unlist(sentwords$positive)

deppos = wordStem(deppos)

deppos = data.frame(table(deppos))

deppos = deppos[order(deppos$Freq,decreasing=T),]


























