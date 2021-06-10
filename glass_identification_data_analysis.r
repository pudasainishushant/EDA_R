#Data Preparation
glassdf<-read.table("glass.data", fileEncoding="UTF-8", sep = ",")

names(glassdf) <- c("Id","RI","Na","Mg","Al","Si","K","Ca","Ba","Fe","Class")
head(glassdf,5)
#Data Validation

#checking the number of column and rows is equal to the number mentioned in the metadata and getting the summary to compare with the one given in glass.tag file. 
sum(is.na(glassdf)) # checking if there is na value 
stopifnot(ncol(glassdf)==11)
stopifnot(nrow(glassdf)==214)
summary(glassdf[1:9]) 
#above attributes matches the information given in the meta data so it is a valid dataset
install.packages("GGally")
install.packages("ggpubr")
#box plot to visualize the data and see if outliers are present.
library(GGally)
library(ggpubr)
ggboxplot(glassdf, x = "Class", y = c("Na", "Mg","Al","Si","K","Ca","Ba"),
          merge = FALSE, palette = "jco")
cor(glassdf)
install.packages("psych")
library(psych)
pairs.panels(glassdf[2:10], stars = TRUE)
#box plot shows some outliers present in the data that needs further cleaning and pairs pannel shows the types of distribution of the features of the data

##performing MNOVA test
# Absense of univariate or multivariate outliers.
# Multivariate normality.

table(glassdf$Class)
#Above table shows minimum of 9 data >number of class. Thus, the first assumptions is verified
install.packages("tidyverse")
install.packages("rstatix")
install.packages("car")
install.packages("broom")
library(tidyverse)
library(rstatix)
library(car)
library(broom)
#removing outliers if any
glassdf %>%
  group_by(Class) %>%
  identify_outliers(Fe)
boxplot(glassdf[2:9])$out

cleanedDf<-glassdf
cleanedDf<- glassdf[-which(glassdf$Na %in% outliersNa),]
cleanedDf
# MANOVA test
e_manova <- manova(cbind(Na, Mg) ~ as.factor(Class), data = glassdf)
summary(e_manova)
summary.aov(e_manova)

