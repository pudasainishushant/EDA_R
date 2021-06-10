
hcvDf <- read.table("hcvdat0.csv",sep=",",header=TRUE)
head(hcvDf,10)

###droping the index X
newdf<-subset(hcvDf, select = -c(X))
head(newdf,5)

###converting non numeric columns to numeric

newdf<-transform(newdf,Sex=ifelse((Sex=="m"),0,1))

myFunction <- function(x){
  my_category <- x[1][1]
  value = 0
  if (my_category=='0=BloodDonor'){
    value = 0
  }else if(my_category=='0s=suspect Blood Donor'){
    value = 1
  }else if(my_category=="1=Hepatitis"){
    value = 2
  }else if(my_category=="2=Fibrosis"){
    value = 3
  }else if(my_category=="3=Cirrhosis"){
    value = 4
  }
  return (value)
  #further values ignored (if there are more than 2 columns)
  value <- if(a==b) a + b else b - a
  #or more complicated stuff
  return(value)
}

newdf$class <- apply(newdf, 1, myFunction)

### the number of data of each category is not equal from the above analysis. so removing certain data from category 0 to balance the dataset
trimmedDf<-newdf[500:615,]

### removing the na values
row.has.na <- apply(trimmedDf, 1, function(x){any(is.na(x))})

sum(row.has.na)

trimmedDf <- trimmedDf[!row.has.na,]

head(trimmedDf)
###further no category column is needed so dropping category column
trimmedDf<-subset(trimmedDf, select = -c(Category))

head(trimmedDf)
###reindexing the dataframe

row.names(trimmedDf)<-NULL

head(trimmedDf,1)

###checking the correlation between different categorical variables with the response variable

install.packages("corrr")
cor(trimmedDf, method = c("pearson"))
####here PROT and ALB are highly correlated,ALP and GGT are also highly corellated so we can keep one and discard the other.
#discarding ALB and GGT
trimmedDf<-subset(trimmedDf, select = -c(ALB,GGT))
install.packages("psych")
library(psych)
pairs.panels(trimmedDf[0:10], stars = TRUE)
summary(trimmedDf)

### performing multiclass classification on the data
install.packages("xgboost")
library("xgboost")

train_index <- sample(1:nrow(trimmedDf), nrow(trimmedDf)*0.75)
# Full data set
data_variables <- as.matrix(trimmedDf[,-1])
data_label <- trimmedDf[,"class"]
data_matrix <- xgb.DMatrix(data = as.matrix(trimmedDf), label = data_label)
# split train data and make xgb.DMatrix
train_data   <- data_variables[train_index,]
train_label  <- data_label[train_index]
train_matrix <- xgb.DMatrix(data = train_data, label = train_label)
# split test data and make xgb.DMatrix
test_data  <- data_variables[-train_index,]
test_label <- data_label[-train_index]
test_matrix <- xgb.DMatrix(data = test_data, label = test_label)

###fitting the model and using k-fold for the error estimation..

numberOfClasses <- length(unique(trimmedDf$class))
xgb_params <- list("objective" = "multi:softprob",
                   "eval_metric" = "mlogloss",
                   "num_class" = numberOfClasses)
nround    <- 50 # number of XGBoost rounds
cv.nfold  <- 5

# Fit cv.nfold * cv.nround XGB models and save OOF predictions
cv_model <- xgb.cv(params = xgb_params,
                   data = train_matrix, 
                   nrounds = nround,
                   nfold = cv.nfold,
                   verbose = FALSE,
                   prediction = TRUE)
###Assess Out-of-Fold Prediction Error

OOF_prediction <- data.frame(cv_model$pred) %>%
  mutate(max_prob = max.col(., ties.method = "last"),
         label = train_label + 1)
head(OOF_prediction)

###confusion matrix
install.packages("caret")
library("caret") 
install.packages('e1071', dependencies=TRUE)

# confusion matrix
confusionMatrix(factor(OOF_prediction$max_prob),
                factor(OOF_prediction$label),
                mode = "everything")

