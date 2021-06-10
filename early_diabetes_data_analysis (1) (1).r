
###Reading dataframe
diabetesDf<- read.csv("diabetes_data_upload.csv")
###Printing top 10 data
head(diabetesDf,10)
###Printing bottom 10 data for analysis
tail(diabetesDf,10)
###coverting the character to numeric form for easy data wrangling
diabetesDf<-transform(diabetesDf, Gender=ifelse((Gender == "Female"), 0, 1))
diabetesDf<- transform(diabetesDf,Polyuria=ifelse((Polyuria=="No"),0,1))
diabetesDf<-transform(diabetesDf,Polydipsia=ifelse((Polydipsia=="No"),0,1))
diabetesDf<-transform(diabetesDf,sudden.weight.loss=ifelse((sudden.weight.loss=="No"),0,1))
diabetesDf<-transform(diabetesDf, weakness=ifelse((weakness == "Female"), 0, 1))
diabetesDf<-transform(diabetesDf,Polyphagia=ifelse((Polyphagia=="No"),0,1))
diabetesDf<-transform(diabetesDf,Genital.thrush=ifelse((Genital.thrush=="No"),0,1))
diabetesDf<-transform(diabetesDf,visual.blurring=ifelse((visual.blurring=="No"),0,1))
diabetesDf<-transform(diabetesDf,Itching=ifelse((Itching=="No"),0,1))
diabetesDf<-transform(diabetesDf,Irritability=ifelse((Irritability=="No"),0,1))
diabetesDf<-transform(diabetesDf,delayed.healing=ifelse((delayed.healing=="No"),0,1))
diabetesDf<-transform(diabetesDf,partial.paresis=ifelse((partial.paresis=="No"),0,1))
diabetesDf<-transform(diabetesDf,muscle.stiffness=ifelse((muscle.stiffness=="No"),0,1))
diabetesDf<-transform(diabetesDf,Alopecia=ifelse((Alopecia=="No"),0,1))
diabetesDf<-transform(diabetesDf,Obesity=ifelse((Obesity=="No"),0,1))

diabetesDf<-transform(diabetesDf,class=ifelse((class=="Negative"),0,1))
head(diabetesDf,9)
install.packages("corrr")
stats::cor(diabetesDf[0:17], method = "pearson")
####seems like correlation between the features are less , that means each features are different from each other and has effect on the output. so keeping all features except weakness
newdf<-subset(diabetesDf, select = -c(weakness) )
head(newdf,5)
###seeing the variance
library(dplyr)
newdf %>% summarise_if(is.numeric, var)
install.packages("psych")
library(psych)
####for visualization pair plots have been plotted below. For better figure I would plot only 4 coloums and see relation between the data. Age seems to follow normal distribution while other features are mainly categorical
pairs.panels(newdf[0:4], stars = TRUE)

