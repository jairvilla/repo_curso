# PSG Analysis
# Pre-processing data ----
library(klaR); library(MASS); library(caret);
library(ggplot2), library(dplyr), library(tidyverse)

# load dataset 
data1 <- dtreat_original

# Explore dataset ----
names(data1)
summary(data1) 

# Selecting main features ----
library(dplyr)
data1 <- select(data1, sexo, edad, bmi, 9:12, 15:25, 29:31)

# Replace variables names 
# names(data1)[1] <-"gender"
library(data.table)
setnames(data1, old=c("ind_arousal_psg","i_desatura_psg","sato2_media_psg", "hora_cpap_1m", 
                     "hora_cpap_3m", "hora_cpap_6m" ), new=c("arousal", "desat", 
                      "sao2", "res1", "res3", "res6"))
# Remove columns 
data2 <- select(data1, -(19:21))
names(data2)

# Dealing missing values ----
# 1- using sapply
sapply(data1, function(x) sum(is.na(x)))

# 2- dplyr function  
library(tidyr)
data1 %>% 
summarise_all(funs(sum(is.na(.)))) %>% 
  gather %>% 
  ggplot(aes(x = reorder(key, value), y = value)) + geom_bar(stat = "identity") +
  coord_flip() +
  xlab("variable") +
  ylab("Absolute number of missings")

# Impute values to missing values (MICE)  ----
# MICE algorithm 
library(mice); library(VIM); library(lattice)
# Explanation Mice code ----
# To impute the missing values, mice package use an algorithm in a such a way that use information 
# from other variables in the dataset to predict and impute the missing values. Therefore, you may
# not want to use a certain variable as predictors. For example, the ID variable does not have any predictive value.

# To skip a imputation variable
# meth[c("gender", "depresion", "ansiedad", "hta, cardio", "enf_neuro", "enf_respira")]=""

## Remove responses from dataset.
# data2 <-(data1)[,1:10] ## Just have 10 predictors variables (just numeric predictor)
data2 <- select(data1, -(19:21))
data2 <- select(data2, "edad", "bmi", "epworth", "eq50", 6:7, 14:18) # Creat new dataset with only numerical variables 
imp <-mice(data2, m=5, printFlag=FALSE, maxit = 5, seed=145)
summary(imp)
# The output imp contains m=5 completed datasets. Each dataset can be analysed

# Completed datasets (observed and imputed), for example the second one, can be extracted by
# Select an iteration (4)
data2 <- complete(imp,5)
data.frame(colSums(is.na(data2)))
names(data2)
dim(data4)
# Check for implausible imputations (values that are clearly impossible, e.g. negative values for bmi)
# The imputations, for example for epworth, are stored as
imp$imp$iah_psg

# Back a dataset (data1)
data1 <- cbind(data2, data1[,19:21], data1[,8:13])
names(data1)

## We can inspect the distributions of the original and the imputed data:
## scatterplot (of epworth and iah_total) for each imputed dataset
xyplot(imp,  epworth ~ iah_psg | .imp, pch = 20, cex = 1.4)

## pmm stands for predictive mean matching, default method of mice() for imputation of continuous incomplete variables; for each missing value, pmm finds a set of observed values with the closest predicted mean 
# as the missing one and imputes the missing values by a random draw from that set. 
imp$method

# Modifica la distrubucion de los predictors???
summary(dtreat1)





# Classify outcomes---- ----
data1$res1.class <- with(data1, ifelse(res1>="4","cumple", "nocumple"))
data1$res2.class <- with(data1, ifelse(res3>="4", "cumple", "nocumple"))
data1$res6.class <- with(data1, ifelse(res6>="4", "cumple", "no cumple"))
names(data1)
#  Converting to factor variable (commorbidities)-----
data1$depresion<- as.factor(data1$depresion)
data1$ansiedad<- as.factor(data1$ansiedad)
data1$hta<- as.factor(data1$hta)
data1$enf_neuro<- as.factor(data1$enf_neuro)
data1$enf_respira<- as.factor(data1$enf_respira)
data1$cardio<- as.factor(data1$cardio)

data1 <- select(data1, -res2.class)
#--Converting to factor the response
data1$res1.class<-as.factor(data1$res1.class)
data1$res3.class<-as.factor(data1$res2.class)
data1$res6.class<-as.factor(data1$res6.class)
str(data1)


# ------------------------------------------ Naives bayes model ----------------------------------------
set.seed(156)
inTrain  <- createDataPartition(y=data1$res6.class,p=0.8, list=FALSE) # res1.class = cumple/ no cumple
training <- data1[inTrain,]
testing <- data1[-inTrain,]
dim(training); dim(testing)
names(training)

# To define training control ----
train_control <- trainControl(method="cv", number=10)
# fix the parameters of the algorithm
# grid <- expand.grid(.fL=c(0), .usekernel=c(FALSE), .adjust=c(1))

# To Create Naives Bayes Model
model_nb1  <- train(res6.class ~  + + + , data=training, set.seed(156), trControl=train_control, method="nb")
names()
# model_nb1
# summary(model_nb1)
# model_nb1$finalModel
# names(model_nb1)
# model_nb1$x        # copy of the original data 
# model_nb1$results# Summaries results 

# Predict test outcomes using naive Bayes model
pred.nb1 <- predict(model_nb1,testing)
data.frame(predicted=c(pred.nb1), actual=c(testing$res1.class))
table(testing$res1.class)

#table(pred.nb1, testing$res1.class, dnn=c("Actual", "Predicted"))
confusionMatrix(data= pred.nb1, reference = testing$res1.class)

