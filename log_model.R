# Logistical Regression and Feature selection 
# by J-villanueva

# Load libraries ----
library(haven);library(datasets); library(caret);library(readxl)
library(plyr);library(dplyr); library(ggplot2); library(tidyverse) 
library(data.table); library(tidyr); library(ggpubr)

## Preparing dataset ----
#  Exploring data --
# -----------------------------------------------
str(data1)  # borrar esto
# Esto sera una prueba. ç
names(data)

#-----------------------------------------------

names(data1)
summary(data1)
head(data1, n=10)
str(data1)

# Missing data Count --
data.frame (colSums(is.na(data1))) # Total Count NA's

#-- Converting to factor variable (commorbidities)--
data1$depresion<- as.factor(data1$depresion)
data1$ansiedad<- as.factor(data1$ansiedad)
data1$hta<- as.factor(data1$hta)
data1$enf_neuro<- as.factor(data1$enf_neuro)
data1$enf_respira<- as.factor(data1$enf_respira)
data1$cardio<- as.factor(data1$cardio)
# - responses as factors
data1$iah.class  <- factor(data1$iah.class)
data1$res1.class <- factor(data1$res1.class)
data1$res3.class <- factor(data1$res3.class)
data1$res6.class <- factor(data1$res6.class)

#-- Handling Missing data --
data.frame(colSums(is.na(data1)))

## Imputation with MICE package----
library(mice); library(VIM); library(lattice)
names(dtreat_mix)

## Remove responses from dataset----
data1 <- data1[,1:11] ## Just have 10 predictors variables (just numeric predictor)
imp     <- mice(data1, m=5, printFlag=FALSE, maxit = 5, seed=0905)
summary (imp)
# The output imp contains m=5 completed datasets. Each dataset can be analysed
# Completed datasets (observed and imputed), for example the second one, can be extracted by
# Select an iteration (4)
data1_4 <- complete(imp,4)
summary (data1_4)
data1_5 <- complete(imp,5)
# Check NA values 
data.frame(colSums(is.na(data1)))

# Check for implausible imputations (values that are clearly impossible, e.g. negative values for bmi)
# The imputations, for example for epworth, are stored as
imp$imp$epworth

# We can inspect the distributions of the original and the imputed data:
## scatterplot (of epworth and iah_total) for each imputed dataset
xyplot(imp,  epworth ~ iah_total | .imp, pch = 20, cex = 1.4)

#pmm stands for predictive mean matching, default method of mice() for imputation of continous incomplete variables; for each missing value, pmm finds a set of observed values with the closest predicted mean 
# as the missing one and imputes the missing values by a random draw from that set. 
imp$method

## To verify Hypoythesis t-test (significance)
# Modifica la distrubucion de los predictors???
# Evaluate significant differences, p-value 
t.test(data1$tc90_total, data1$tc90_total, method="paired") # p-value= 0.95
t.test(data1$sao2_total, data1$sao2_total, method="paired") # p-value= 0.95
t.test(data1$doi_total, data1$doi_total, method="paired")   # p-value= 
t.test(data1$epworth, data1$epworth, method="paired")      # p-value = 0.8902
t.test(data1$eq50, data1$eq50, method="paired")
t.test(data1$bmi, data1$bmi, method="paired")
t.test(data1$iah_total, data1$iah_total, method="paired")
t.test(data1$asda, data1$asda, method="paired")

# Merge response variables and  predictors --
# dtreat1 contains to imputed values, wei must include comorbidities and response from dtreat_mix
names(data1)
data1 <-cbind(data1, data1[, 12:17], data1[, 18:20])

library(rio)
export(data1, "dtreat1_mod.txt")

#-- Assing classes to responses and ahi -----
# iah.class, 0>iah=<15 = Mild, iah[15 -30] = mod, iah>30 = severe.
dtreat1$iah.class <- with(dtreat1, ifelse(iah_total >= 0 & iah_total < 15, "mild", 
                          ifelse(iah_total >= 15 & iah_total < 30, "mod", "sev")))
??rattle
# converter NA values from response1 as 0 ----
dtreat1$response1[is.na(dtreat1$response1)] <- 0
dtreat1$response3[is.na(dtreat1$response3)] <- 0
dtreat1$response6[is.na(dtreat1$response6)] <- 0

# response_1
dtreat1$res1.class <- with(dtreat1, ifelse(response1 >= 0.1 & response1 < 4, "nocumple", 
                                           ifelse(response1 >= 4 & response1 < 15, "cumple","notreat")))
# response3
dtreat1$res3.class <- with(dtreat1, ifelse(response3 >= 0.1 & response3 < 4, "nocumple", 
                                           ifelse(response3 >= 4 & response3 < 15, "cumple","notreat")))
# response6
dtreat1$res6.class <- with(dtreat1, ifelse(response6 >= 0.1 & response6 < 4, "nocumple", 
                                           ifelse(response6 >= 4 & response6 < 15, "cumple","notreat")))
# responses variables as factors
dtreat1$iah.class  <-as.factor(dtreat1$iah.class)
dtreat1$res1.class <- as.factor(dtreat1$res1.class)
dtreat1$res3.class <- as.factor(dtreat1$res3.class)
dtreat1$res6.class <- as.factor(dtreat1$res6.class)
#str(dtreat1)
#dtreat1<-subset(dtreat1, select=-c(1))

colSums(is.na(dataMasa))

# Binaring the outcome
names(data2.imp)
data2.imp$res_bin <- ifelse(data2.imp$res1.class=="cumple",0,1)

table(data2.imp$res1.class)

# Asignar la referencia en las clase (con esta dtreat has two levels: cumple o no cumple)----
data1$rel.res1<-relevel(data1$res1.class, ref = "cumple")
data1$rel.res3<-relevel(data1$res3.class, ref = "cumple")
data1$rel.res6<-relevel(data1$res6.class, ref = "cumple")
str(data1)


##-------------------------------- Logistic Regression Model -------------------------------####
# Data set 
library(dplyr)
data4 <-select(data1, 1:16,41:44)
names(data4)
names()
# Asignar 1 o 0 a la outcome variable ----
data2.imp$res1.bin <- with(data2.imp, ifelse(res1.class=="cumple", 0, 1))
data2.imp$res1.bin <- as.factor(data2.imp$res1.bin)
data2$res1.bin     <- relevel(data1$res1.bin, ref = "1") # relevel 
names(data_sel)
table(data1$iah.class)
# Remove res1.class ----
data3 <- data2.imp[,-18] # remove res1.bin
names (data3)
dim   (data3)

# Building model
set.seed(345)
# split data set 
split <- round(nrow(data3)*.70)  
train <- data3[1:split,]

# Create test --
test  <- data3[(split + 1):nrow(data3),]
dim(test); dim(train)

# Full logistic regression model 
glm.model <- glm(res1.class ~ ., family = binomial,data= train)  
coef(glm.model) 
summary(glm.model)

# Predictions full model 
cl.glm  <- predict(glm.model, test, type="response")  # Probabilidad de que sea 1(no cumple)
pr.glm  <- predict(glm.model, test, type="link")      # output logit function 

# Apply a Threshold to clasify 
class.pre <- ifelse(cl.glm > 0.5, "nocumple", "cumple")
out2      <- data.frame(prob =cl.glm, pred=class.pre, actual=test$res1.class)# data frame (pred , actual)
head(out2, 20)
class.pre <-factor(class.pre)

# Confusion Matrix 
confusionMatrix(class.pre, reference=test$res1.class)
mean(class.pre==test$res1.bin)  # Accurracy 

# --------------------- logistic model for taking account the select variables ---------
# Building model
set.seed(789)
names(data1)
# selecting important variables 
library(dplyr)
data_imp <-select(data1, "eq50", "age", "ind_arousal_total","fosq","res1.class") 
data_imp <-data1[,c(3, 14, 10, 16,59)]

# split data set 
split    <- round(nrow(data_imp)*.70)  
train    <- data_imp[1:split,]

# Create test --
test  <- data_imp[(split + 1):nrow(data_imp),]
dim(test); dim(train)
str(train)

# Full logistic regression model 
glm.model2 <- glm(res1.class ~ ., family = binomial,data= train)  
coef(glm.model2) 
summary(glm.model2)
glm.model2$fitted.values

# Predictions full model 
cl.glm2  <- predict(glm.model2, test, type="response")  # Probabilidad de que sea 1(no cumple)
pr.glm2  <- predict(glm.model2, test, type="link")

# Apply a Threshold to clasify 
class.pre <- ifelse(cl.glm2 > 0.50, "nocumple", "cumple")
out2      <- data.frame(cl.glm2, pred=class.pre, actual=test$res1.class)# data frame (pred , actual)
head(out2,20)
class.pre <-factor(class.pre)
# Confusion Matrix 
confusionMatrix(class.pre, reference=test$res1.class)
mean(class.pre==test$res1.bin)  # Accurracy 

# ROC Curve ----
# compute the cutoff for classificaton from probabilities into classes ---- 

library(pROC) 
roc1 <- roc(response=train$res1.class, predictor= glm.model$fitted.values) # Draw ROC curve, response must be class, predictor must be numeric.
auc1 = auc(roc1)
print(auc1)

# Find threshold that minimizes error, here the cut-off to classify the new observations could be cumple or not. 
e       <-cbind(roc1$thresholds,roc1$sensitivities+roc1$specificities)
cut_off <-subset(e,e[,2]==max(e[,2]))[1]
best_t  <-cat(" The best value of cut-off for classifier is ", cut_off)
# The best cutoff is the value on the curve that maximizes sensitivity and
# minimizes (1-specificity).

#Plot ROC Curve
curve1 <- plot (1-roc1$specificities,roc1$sensitivities,type="l",
                ylab="Sensitivity",xlab="1-Specificity",col="blue",lwd=2, 
                main ="ROC Curve for train data", sub = )
                abline(a=0,b=1, col="red")
                par(adj = 0.5)                  # adjust subtitle in the middle 
                text (0.5,0.5, paste("AUC=",round(roc1$auc*100, 2)), pos=4, col="black", cex=0.9)
                grid()

#---------------------------Features selection: Apply backward ----------------- 
library(tidyverse);library(MASS)
library(caret); library(dplyr)

step.model <- glm.model %>% stepAIC (glm.model, direction=c( "backward"))
coef(step.model) # subset of the most contributive  predictor
summary(step.model)
step.model$iter
# prediction with step model 
step.glm  <- predict(step.model, test, type="response") 
step.pred <- ifelse(step.glm > 0.5, "1", "0")

# Matrix confusion 
mean(step.pred==test$res1.bin)
confusionMatrix(step.pred, reference=test$res1.bin)
# Apply varimp function 
varImp(glm.model)

# Using var Imp function 
varImp(glm.model)  # z-value > 2, significant predictors for the model. 


#-------------------------------------------------------
# note: AIC vs subset size K 
plot1 <-data.frame(aic=c(194.05, 192.05, 190.05, 188.12, 186.19,184.47,182.85,181.27,179.63,
                         178.71,177.87,176.47), k=c(1,2,3,4,5,6,7,8,9,10,11,12))
library(ggplot2)
qplot(x=k, y=aic, data=plot1, geom=c("point", "line"), xlim = c(0, 12), ylim=c(170, 197), 
      xlab="Subset Size K", ylab="AIC (Akaike Information Criterion)")
#----------------------------------------------------------

# Calculate RMSE on training ()
sqrt(sum((model1$fitted.values- train$res1.class)^2))  ## revisar para logistic regression. (on training)
sqrt(sum((model1$fitted.values- test$res1.class)^2))  ## debes comprobar con trainih
# we can perform an ANOVA Chi-square test to check the overall effect of variables on the dependent variable.
anova(model1, model2, test ="Chisq")


## Information gain-------------------
# which factors are relevant for model?
library(FSelector)
weights <- information.gain(response1~., data1)
print   (weights)
subset  <- cutoff.k(weights, 2)
f       <- as.simple.formula(subset, "response1")
print(f)

# Fselector
res <- gain.ratio(g~., data)
# Plot to find relation among variables
plot(sleep.data, pch=16, col = "blue", main = "scatterplot")

