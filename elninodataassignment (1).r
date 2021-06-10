library(dplyr)
library(tidyverse)
library(ggpubr)
library(Hmisc)
library(psych)
library(ggplot2)

#reading dataset
dataset<-read.table("tao-all2.dat", na.strings=".")
names(dataset) <- c("obs","year","month","day","date","latitude",
                    "longitude","zon.winds","mer.winds","humidity","air_temp",
                    "s.s.temp")
head(dataset,10)

#data validation
stopifnot(ncol(dataset)==12)
stopifnot(nrow(dataset)==178080)

#checking if data contains na values to check if cleaning is required
row.has.na <- apply(dataset, 1, function(x){any(is.na(x))})
sum(row.has.na)

#sum(row.has.na) is 0 so there are no nan values. performing further cleaning of missing values
str(dataset)
dataset$longitude <- dataset$longitude + ifelse(dataset$longitude<0, 360, 0)
dataset <- na.omit(dataset)

head(dataset)

#converting non numeric columns to numeric now
dataset[, c(8:12)] <- sapply(dataset[, c(8:12)], as.numeric)


str(dataset)
summary(dataset)
#calculating the column wise variance
sapply(dataset, function(x) c(sum=sum(x), var=var(x), sd=sd(x)))

cor(dataset, method = c("pearson"))
#corelation matrix here describe
#datavisualization with scatterplot of 100 rows
ggplot(dataset[1:100,], aes(x=air_temp,y=longitude, colour = s.s.temp)) + geom_point(position = position_dodge(width = .3))
ggplot(dataset[1:100,], aes(x=air_temp,y=latitude, colour = s.s.temp)) + geom_point(position = position_dodge(width = .3))


ggplot(dataset,aes(y=dataset$air_temp)) + 
  geom_boxplot(color="lightblue", notch=TRUE, fill="azure", outlier.color="red", outlier.shape=20,outlier.size=4) +
  ylab("Air temperature") +
  scale_x_discrete() +
  ggtitle("Boxplot of Air Temperature")

ggplot(dataset,aes(x=air_temp,y=longitude)) + 
  geom_point(color = "blue") +
  xlab("Air.Temp") +
  ylab("Longitude") +
  ggtitle("Scatterplot of Air.Temp") +
  geom_smooth(method = "lm",color = "red") +
  geom_smooth(color = "orange")

