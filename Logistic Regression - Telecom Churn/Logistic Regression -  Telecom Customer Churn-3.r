# install.packages("Hmisc")
# install.packages("tidyverse")
# install.packages("dplyr")
# install.packages("lubridate")
# install.packages("pastecs")
# install.packages("outliers")
# install.packages("corrplot") 
# install.packages("rcompanion") 
# install.packages("nortest")
# install.packages("ggplot2")
# install.packages("sqldf")
# install.packages("tseries")
# install.packages("forecast")
# install.packages("xlsx")
# install.packages("naniar")
# install.packages("caret")
# install.packages("fastDummies")
# install.packages("FSelector")
# install.packages("party")
# install.packages("randomForest")
# install.packages("FactoMineR")
# install.packages("factoextra")
# install.packages("e1071")
# install.packages("ROCR")
# install.packages("MASS")
# install.packages("ResourceSelection")
# install.packages("Metrics")

library(Hmisc)
library(dplyr)
library(lubridate)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(forcats)
library(scales)
library(tidyr)
library(pastecs)
library(outliers)
library(corrplot)
library(rcompanion)
library(nortest)
library(ggplot2)
library(sqldf)
library(xlsx)
library(naniar)
library(caret)
library(MASS)
library(fastDummies)
library(FSelector)
library(party)
library(randomForest)
library(FactoMineR)
library(factoextra)
library(e1071)
library(ROCR)
library(MASS)
library(ResourceSelection)
library(Metrics)

cc <- read.csv("C:\\Data_Science\\Certifications\\AnalytixLabs\\Data Science in R\\Data Science with R - Final Projects\\Logistic Regression - Telecom Churn\\Proactive Attrition Management-Logistic Regression Case Study.csv")
head(cc,5)

NROW(cc)
NCOL(cc)

str(cc)

summary(cc)

describe(cc)

# Plot a histogram on all numeric columsn to see the distribution
# Some columns will have only few distinct values. These can later be classifed as categorical variables.

num_data <-cc[, sapply(cc, is.numeric)] 
for (i in 1:length(names(num_data))){
  print(i)
  hist( num_data[i], main='hist', breaks=20, prob=TRUE)
}

str(cc)

#Change column names to lowercase for ease of use
colnames(cc) <- tolower(colnames(cc))

# Drop Customerid column as its not needed
cc <- subset(cc,select = -c(customer))

# Drop csa column as its not needed
cc <- subset(cc,select = -c(csa)) 

# Drop churndep column as its not needed(we'll use churn instead)
cc <- subset(cc,select = -c(churndep)) 

#Convert the predicted variable to categorical
cc$calibrat <- as.factor(cc$calibrat)
cc$churn <- as.factor(cc$churn)

# Remove Retcall, Retaccpt, & retcalls from your final model. 
# As these variables are for actual churners, as mentioned in the business problem. 
cc <- subset(cc,select = -c(retcall,retcalls,retaccpt)) 

colnames(cc) 

head(cc,2)

str(cc)

# Convert categorical variables from Dummy variables
# Drop the Dummy variables in place of categorical variables

cc1 <- cc %>% mutate(marry = ifelse(marryun == 1,"Unknown",ifelse(marryyes==1,"Married",ifelse(marryno==1,"Unmarried","NA"))))  %>% 
              mutate(mail = ifelse(mailord == 1,"Mail-Order",ifelse(mailres==1,"Mail-Offer",ifelse(mailflag==1,"No-Mail","NA")))) %>% 
              mutate(cell = ifelse(newcelly == 1,"CellPhone",ifelse(newcelln==1,"No-CellPhone","NA"))) %>% 
              mutate(occupation = ifelse(occprof == 1,"Professional",ifelse(occcler==1,"Clerk",ifelse(occcrft==1,"Craftsman",ifelse(occstud==1,"Student",ifelse(occhmkr==1,"Homemaker",ifelse(occret==1,"Retired",ifelse(occself==1,"Self-Employed","NA")))))))) %>% 
              mutate(prizm = ifelse(prizmrur == 1,"Rural",ifelse(prizmub==1,"Suburban",ifelse(prizmtwn==1,"Town","NA")))) %>% 
              mutate(credit = ifelse(credita == 1,"A",ifelse(creditaa==1,"AA",ifelse(creditb==1,"B",ifelse(creditc ==1,"C",ifelse(creditde==1,"DE",ifelse(creditgy==1,"GY",ifelse(creditz==1,"Z","No_rating")))))))) 

cc1$marry <- as.factor(cc1$marry)
cc1$mail <- as.factor(cc1$mail)
cc1$cell <- as.factor(cc1$cell)
cc1$occupation <- as.factor(cc1$occupation)
cc1$prizm <- as.factor(cc1$prizm)
cc1$credit <- as.factor(cc1$credit)

cc1 <- subset(cc1,select = -c(marryun,marryyes,marryno,mailord,mailres,mailflag,newcelly,newcelln,occprof,occcler,occcrft,occstud,occhmkr,occret,occself,prizmrur,prizmub,prizmtwn,credita,creditaa,creditb,creditc,creditde,creditgy,creditz)) 
cc <- cc1

str(cc)

# Histogram of numeric variables

num_data <-cc[, sapply(cc, is.numeric)] 
for (i in 1:length(names(num_data))){
  print(i)
  hist( num_data[i], main='hist', breaks=20, prob=TRUE)
}

# Numeric variables
cc_num <- dplyr::select_if(cc, is.numeric)
num_var <- names(cc_num)

head(cc_num,5)
str(cc_num)

# Categorical variables
cc_cat <- dplyr::select_if(cc, is.factor)
cat_var <- names(cc_cat)

str(cc_cat)
head(cc_cat)

# Statistics function for Numeric variables

mystats_num = function(x) {
  nmiss=sum(is.na(x))
  c = class(x)
  a = x[!is.na(x)]
  m = mean(a,na.rm = T)
  med=median(a,na.rm = T)
  n = length(a)
  s = sd(a,na.rm = T)
  min = min(a,na.rm = T)
  q1=quantile(a,0.25,na.rm = T)
  q2=quantile(a,0.5,na.rm = T)
  q3=quantile(a,0.75,na.rm = T)
  p99=quantile(a,0.99,na.rm = T)
  p5=quantile(a,0.05,na.rm = T)
  p95=quantile(a,0.95,na.rm = T) 
  max = max(a,na.rm = T)
  UC = m+3*s
  LC = m-3*s
  outlier_flag = (max>p95 || min<p5) 
  return(c(class=c,n=n, nmiss=nmiss, outlier_flag=outlier_flag, mean=m,median=med, stdev=s,min = min,
           q1=q1,q2=q2,q3=q3,p99=p99,p5=p5,p95=p95,max=max,UC=UC,LC=LC ))
}

#Statistics function for Categorical variables

mystats_cat=function(x){
    Var_Type=class(x)
    n=length(x)
    nmiss=sum(is.na(x))
    return( c(Var_Type=Var_Type, n=n,nmiss=nmiss))
  }

# Save the stats in a file
num_stats <- data.frame(apply(cc_num,2,FUN = mystats_num))
write.csv(num_stats,"C:\\Data_Science\\Certifications\\AnalytixLabs\\Data Science in R\\Data Science with R - Final Projects\\Logistic Regression - Telecom Churn\\NumericStats.csv")

cat_stats <- t(data.frame(apply(cc_cat,2,FUN = mystats_cat))) 
write.csv(cat_stats,"C:\\Data_Science\\Certifications\\AnalytixLabs\\Data Science in R\\Data Science with R - Final Projects\\Logistic Regression - Telecom Churn\\CategoricalStats.csv")#View(cat_stats)


cc_num$setprc[cc_num$setprc==0] <- NA
cc_num$income[cc_num$income==0] <- NA

# User-Defined function to find NAs for cc dataframe

findMissingValues <- function(x) {
na_cnt <- colSums(is.na(x))
columns <- colnames(x)   
na_percent <- apply(x, 2, function(col)sum(is.na(col))*100/length(col))
tot_cnt <- apply(x, 2, function(col)length(col))
res <- data.frame(columns,tot_cnt,na_cnt,na_percent)
print(res[order(res$na_percent),]) 
                 
list_na <- colnames(x)[ apply(x, 2, anyNA) ]

gg <- ggplot(data = res, mapping = aes(x = na_percent, y = columns)) + geom_point(alpha = 1, aes(color = columns))
print(list_na) 
gg              
                
}            

findMissingValues(cc_num)

# income and setprc have 25% and 56% missing values, so we will drop these features

cc_num <- subset(cc_num,select=-c(income,setprc))

#Mean value treatment for Numerical variables
#Store the columns with NA values in list_na
#Find the mean values of such columns

list_na <- colnames(cc_num)[ apply(cc_num, 2, anyNA) ]
list_na

average_missing <- apply(cc_num[,colnames(cc_num) %in% list_na], 2, mean, na.rm =  TRUE)
average_missing

#Impute Missing data with the Mean or Median

cc_num$revenue[is.na(cc_num$revenue)] <- 58.8539614010814
cc_num$mou[is.na(cc_num$mou)] <- 525.728392370572
cc_num$recchrge[is.na(cc_num$recchrge)] <- 46.8764916491367
cc_num$directas[is.na(cc_num$directas)] <- 0.894801146390705
cc_num$overage[is.na(cc_num$overage)] <- 40.0953598000875
cc_num$roam[is.na(cc_num$roam)] <- 1.22152616792083

cc_num$changem[is.na(cc_num$changem)] <- -10.8464614076122
cc_num$changer[is.na(cc_num$changer)] <- -1.20592557941739
cc_num$phones[is.na(cc_num$phones)] <- 1.80861695239704
cc_num$models[is.na(cc_num$models)] <- 1.56179095234074

#EQUPDAYS cannot have decimal values, so we round off to a whole number
cc_num$eqpdays[is.na(cc_num$eqpdays)] <- 380
cc_num$age1[is.na(cc_num$age1)] <- 31.3751128175007
cc_num$age2[is.na(cc_num$age2)] <- 21.1577152844434


# No missing values as expected for Categorical variables

findMissingValues(cc_cat)

# Create a function that will find and cap the outliers.
# An outlier is considered if it is below the (Q1 – 1.5*IQR) or above (Q3 + 1.5*IQR)
# Or, an outlier is considered if it is below the 5th percentile or above 95th percentile
# We will cap the low and high outliers with 5th and 95th percentile values respectively

HandleOutlier <- function(x,names){
  
   m1 <- mean(x, na.rm = T) 
   cat("\n Mean with outliers:\n", round(m1, 4))
   OutVals = boxplot(x, main="Boxplot with outliers",horizontal = TRUE,col = "red")$out
   cat("\n Pre-treatment Outliers:\n", OutVals)
   
   which(x %in% OutVals)    
#   hist(x, main="Histogram with outliers", xlab=NA, ylab=NA, col="red",)
#   plot(x,xlab="Plot with Outliers", ylab="values", pch="*", col="red", cex=2)

# Method 1: Capping with 5th and 95th percentiles for outliers defined by IQR 
    
#   qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
#   caps <- quantile(x, probs=c(.05, .95), na.rm = T)
#   H <- 1.5 * IQR(x, na.rm = T)
#   x[x < (qnt[1] - H)] <- caps[1]
#   x[x > (qnt[2] + H)] <- caps[2]
    
# Method 2: Capping outliers with 5th and 95th percentiles 
    quantiles <- quantile(x, c(.05, .95), na.rm = T)
    x[ x < quantiles[1] ] <- quantiles[1]
    x[ x > quantiles[2] ] <- quantiles[2]
    
# Method 3: We can replace the outliers with NAs and later impute NAs using MICE or kNN  
    
#  y[x < (qnt[1] - H)] <- NA
#  y[x > (qnt[2] + H)] <- NA
    
# Method 4: Imputation with mean / median / mode
    
#  x[x < (qnt[1] - H)] <- median(x, na.rm = T)
#  x[x > (qnt[2] + H)] <- median(x, na.rm = T)
#  x[x < (qnt[1] - H)] <- mean(x, na.rm = T)
#  x[x > (qnt[2] + H)] <- mean(x, na.rm = T)
#  x[x < (qnt[1] - H)] <- mode(x, na.rm = T)
#  x[x > (qnt[2] + H)] <- mode(x, na.rm = T)
    
   m2 <- mean(x, na.rm = T)
   cat("\n Mean without outliers:\n", round(m2, 4))
   OutVals = boxplot(x, main="Boxplot without outliers",horizontal = TRUE,col = "green")$out
      
   cat("\n Post-treatment Outliers:\n", OutVals)
    
   which(x %in% OutVals)
#   hist(x, main="Histogram without outliers", xlab=NA, ylab=NA, col="green")
#   plot(x,xlab="Plot without Outliers", ylab="values", pch="*", col="green", cex=2)

    return(x)
}

# Find and Apply outlier treatment to all Numeric variables

cc_num <- data.frame(lapply(cc_num,HandleOutlier))  

head(cc_num,5)
head(cc_cat,5)

# List of categorical and numeric variables
num_var <- names(cc_num)
cat_var <- names(cc_cat)

cc<-cbind.data.frame(cc_num,cc_cat)
str(cc)

# We can run Chi-Squred test for all columns with churndep and check the p-values 
# Need to find a better way to loop through all the columns in the dataframe

chisq.test(cc$churn, cc$marry, correct=FALSE)
chisq.test(cc$churn, cc$mail, correct=FALSE)
chisq.test(cc$churn, cc$cell, correct=FALSE)
chisq.test(cc$churn, cc$occupation, correct=FALSE)
chisq.test(cc$churn, cc$prizm, correct=FALSE)
chisq.test(cc$churn, cc$credit, correct=FALSE)

chisqmatrix <- function(x) {
  names = colnames(x);  num = length(names)
  m = matrix(nrow=num,ncol=num,dimnames=list(names,names))
  for (i in 1:(num-1)) {
    for (j in (i+1):num) {
      m[i,j] = chisq.test(x[,i],x[,j],)$p.value
    }
  }
  return (m)
}

mat <- chisqmatrix(cc_cat)
mat

# As an alternative, we can use the chi.squared function from FSelector package
# This package provides functions for selecting attributes from a given dataset. Attribute subset selection is the process of identifying and removing as much of the irrelevant and redundant information as possible.
# CRAN page: http://cran.r-project.org/web/packages/FSelector/index.html
# The result is equal to Cramer's V coefficient between source attributes and destination attribute.
# Here we filter out the top 10 columns.

p <- FSelector::chi.squared(churn~., cc_cat)
print(p)
cutoff.k(p, 4)

# Drop non significant categorical columns
cc_cat <- subset(cc_cat,select=c(marry,mail,prizm,credit,calibrat,churn))

# Combine the categorical and numeric datasets
cc<-cbind.data.frame(cc_num,cc_cat)
str(cc)

set.seed(100)
rPartMod <- train(churn ~ ., data=cc, method="rpart")
rpartImp <- varImp(rPartMod)
print(rpartImp)

# We will keep only the top 20 important features in the final dataset + calibrat + churn

cc <- subset(cc,select=c(eqpdays,months,mou,recchrge,refurb,setprcm,phones,webcap,models,changem,outcalls,rv,directas,overage,children,ownrent,calibrat,churn))

par(mfrow = c(2,4))

boxplot(eqpdays~churn, ylab="eqpdays", xlab= "churn", col="light blue",data = cc)
boxplot(months~churn, ylab="months", xlab= "churn", col="light blue",data = cc)
boxplot(mou~churn, ylab="mou", xlab= "churn", col="light blue",data = cc)
boxplot(recchrge~churn, ylab="recchrge", xlab= "churn", col="light blue",data = cc)
boxplot(refurb~churn, ylab="refurb", xlab= "churn", col="light blue",data = cc)
boxplot(setprcm~churn, ylab="setprcm", xlab= "churn", col="light blue",data = cc)
boxplot(phones~churn, ylab="phones", xlab= "churn", col="light blue",data = cc)
boxplot(webcap~churn, ylab="webcap", xlab= "churn", col="light blue",data = cc)

write.csv(cc, file="C:\\Data_Science\\Certifications\\AnalytixLabs\\Data Science in R\\Data Science with R - Final Projects\\Logistic Regression - Telecom Churn\\telecom_final.csv")

# Check data distribution
# Plot histograms for all the continuous numeric variables in cc

num_data <-cc[, sapply(cc, is.numeric)] 
for (i in 1:length(names(num_data))){
  print(i)
  hist( num_data[i], main='hist', breaks=20, prob=TRUE)
}

# Here we see that except 'changem' the columns are either moderately skewed or highly skewed
# We will transform some of these variables using log, sqrt and cuberoot

e1071::skewness(cc$eqpdays)
e1071::skewness(cc$months)
e1071::skewness(cc$mou)
e1071::skewness(cc$recchrge)
e1071::skewness(cc$phones)
e1071::skewness(cc$models)
e1071::skewness(cc$changem)
e1071::skewness(cc$outcalls)
e1071::skewness(cc$directas)
e1071::skewness(cc$overage)

# the new skewness after transformation is between  -0.5 and +0.5, so the data is approximately symmetric.
# mou

ggplot(data = cc, aes(mou)) + geom_histogram()
sqrt.mou <- sqrt(cc$mou)
ggplot(data = cc, aes(sqrt.mou)) + geom_histogram()
e1071::skewness(sqrt.mou)


# eqpdays
# skewness is close to 0, the data is approximately symmetrical,
ggplot(data = cc, aes(eqpdays)) + geom_histogram()
sqrt.eqpdays <- sqrt(cc$eqpdays)
ggplot(data = cc, aes(sqrt.eqpdays)) + geom_histogram()
e1071::skewness(sqrt.eqpdays)

# months
# skewness is close to 0, the data is approximately symmetrical,
ggplot(data = cc, aes(months)) + geom_histogram()
log.months <- log1p(cc$months)
ggplot(data = cc, aes(log.months)) + geom_histogram()
e1071::skewness(log.months)

# recchrge
# skewness is between -0.5 and 0.5, the data is approximately symmetrical,
ggplot(data = cc, aes(recchrge)) + geom_histogram()
sqrt.recchrge <- sqrt(cc$recchrge)
ggplot(data = cc, aes(sqrt.recchrge)) + geom_histogram()
e1071::skewness(sqrt.recchrge)

# phones
# skewness is between +1 and +0.5 so the data is moderately skewed

ggplot(data = cc, aes(phones)) + geom_histogram()
log.phones <- log(cc$phones)
ggplot(data = cc, aes(log.phones)) + geom_histogram()
e1071::skewness(log.phones)

# models
# skewness is between +1 and +0.5 so the data is moderately skewed

ggplot(data = cc, aes(models)) + geom_histogram()
log.models <- log(cc$models)
ggplot(data = cc, aes(log.models)) + geom_histogram()
e1071::skewness(log.models)

# outcalls
# skewness is between -0.5 and +0.5, the data is approximately symmetric.

ggplot(data = cc, aes(outcalls)) + geom_histogram()
sqrt.outcalls <- sqrt(cc$outcalls)
ggplot(data = cc, aes(sqrt.outcalls)) + geom_histogram()
e1071::skewness(sqrt.outcalls)

# directas
# skewness is between -0.5 and +0.5, the data is approximately symmetric.

ggplot(data = cc, aes(directas)) + geom_histogram()
cbrt.directas <- (cc$directas)^(1/3)
ggplot(data = cc, aes(cbrt.directas)) + geom_histogram()
e1071::skewness(cbrt.directas)

# overage
# skewness is between -0.5 and +0.5, the data is approximately symmetric.

ggplot(data = cc, aes(overage)) + geom_histogram()
cbrt.overage <- (cc$overage)^(1/3)
ggplot(data = cc, aes(cbrt.overage)) + geom_histogram()
e1071::skewness(cbrt.overage)

# Add these transformed columns to the dataset inplace of the original columns

cc$sqrt.mou <- sqrt.mou
cc$sqrt.eqpdays <- sqrt.eqpdays
cc$log.months <- log.months
cc$sqrt.recchrge <- sqrt.recchrge
cc$log.phones <- log.phones
cc$log.models <- log.models
cc$sqrt.outcalls<- sqrt.outcalls
cc$cbrt.directas <- cbrt.directas
cc$cbrt.overage <- cbrt.overage

cc <- subset(cc,select=-c(mou,eqpdays,months,recchrge,phones,models,outcalls,directas,overage))

# Build two datasets one for model calibration and another for validation
# Calibration dataset to be used to create the model and predict churndep for validation dataset
cc_cal <- cc %>% subset(calibrat==1)
cc_val <- cc %>% subset(calibrat==0)

# Drop the calibrat column since it's no longer needed
cc_cal <- subset(cc_cal,select=-c(calibrat))
cc_val <- subset(cc_val,select=-c(calibrat))

# Not dividing calibration into2 parts. Will use full calibration for model building and test it on validation dataset
# Divide the calibration dataset into 2 parts: training and test datasets in 85:15 ratio
#set.seed(222)
#ind <- sample(2,nrow(cc_cal),replace = TRUE,prob = c(0.75,0.25))
#cc_cal_train <- cc_cal[ind==1,]
#cc_cal_test <- cc_cal[ind==2,]

nrow(cc_cal)
ncol(cc_cal)

nrow(cc_val)
ncol(cc_val)

# We see in calibration dataset 50% of the datasets are positives cases and 50% are negative cases
# So there is no class imbalance in calibration

prop.table(table(cc_cal$churn))
prop.table(table(cc_val$churn))

log.model <- glm(churn ~ .,family=binomial, data=cc_cal)
print(summary(log.model))

# Including only those columns that have atleast 1 star (statisically significant)
log.model <- glm(churn ~ refurb+webcap+changem+ownrent+sqrt.mou+sqrt.eqpdays+log.months+log.phones+sqrt.outcalls+cbrt.overage  ,family=binomial, data=cc_cal)
print(summary(log.model))

# Turn of any warnings (retcall1 is NA so you may get a warning like "prediction from a rank-deficient fit may be misleading"
options(warn=-1)
pred <- predict(log.model, newdata = cc_val, type='response')

y_pred_num <- ifelse(pred > 0.7702652, 1, 0)
y_predicted <- factor(y_pred_num, levels=c(0, 1))
y_actual <- cc_val$churn

cat("Prediction Accuracy %:", mean(y_predicted == y_actual) * 100)

pred1 <- ROCR::prediction(pred, cc_val$churn)
acc.perf = performance(pred1, measure = "acc")
plot(acc.perf)

# Calculate Optimal Acuracy and Cutoff
# Then you can go forth and threshold your model using the cutoff for (in hopes) maximum accuracy in your test data.
ind = which.max(slot(acc.perf, "y.values")[[1]])
acc = slot(acc.perf, "y.values")[[1]][ind]
cutoff = slot(acc.perf, "x.values")[[1]][ind]
print(c(accuracy= acc, cutoff = cutoff))

glmnetcm <- confusionMatrix(y_predicted, y_actual)
glmnetcm

step.model <- log.model %>% stepAIC(trace = TRUE)
step.model
summary(step.model)

# On Calibration  dataset
options(warn=-1) 
pred_train <- predict(step.model, newdata = cc_cal,type='response')

y_pred_num <- ifelse(pred_train > 0.7702652 , 1, 0)
y_predicted <- factor(y_pred_num, levels=c(0, 1))
y_actual <- cc_cal$churn

cat("Prediction Accuracy %:", mean(y_predicted == y_actual) * 100 ) 

# Turn of any warnings
# On Validation dataset
options(warn=-1) 
pred_test <- predict(step.model, newdata = cc_val, type='response')

y_pred_num <- ifelse(pred_test > 0.7702652 , 1, 0)
y_predicted <- factor(y_pred_num, levels=c(0, 1))
y_actual <- cc_val$churn

cat("Prediction Accuracy %:", mean(y_predicted == y_actual) * 100 ) 

pred1 <- ROCR::prediction(pred_test, cc_val$churn)
acc.perf = performance(pred1, measure = "acc")
plot(acc.perf)

# Calculate Optimal Acuracy and Cutoff
# Then you can go forth and threshold your model using the cutoff for (in hopes) maximum accuracy in your test data.
ind = which.max( slot(acc.perf, "y.values")[[1]] )
acc = slot(acc.perf, "y.values")[[1]][ind]
cutoff = slot(acc.perf, "x.values")[[1]][ind]
print(c(accuracy= acc, cutoff = cutoff))

glmnetcm <- confusionMatrix(y_predicted, y_actual)
glmnetcm

log.model$aic
step.model$aic

AUC <- Metrics::auc(cc_val$churn,y_pred_num)
AUC

prediction <- prediction(predictions = pred_test,labels = cc_val$churn)
perf <- performance(prediction,"tpr","fpr")
plot(perf,lwd=2,col='blue',main="ROC Curve")
abline(a=0, b= 1)

ks.test <- performance(pred1, "tpr", "fpr")
test.ks <- max(attr(ks.test, "y.values")[[1]] - (attr(ks.test, "x.values")[[1]]))
test.ks

pred_train <- predict(step.model, newdata = cc_cal, type='response')
pred2 <- ROCR::prediction(pred_train, cc_cal$churn)

ks.train <- performance(pred2, "tpr", "fpr")
train.ks <- max(attr(ks.train, "y.values")[[1]] - (attr(ks.train, "x.values")[[1]]))
train.ks

gain.chart_val <- function(n) {
    score <- runif(n)
    y <- (runif(n) < score)
    plot(performance(prediction(pred_test, cc_val$churn), "tpr", "rpp"), lwd = 2, main = paste("N =", n))
    lines(ecdf((rank(-score)[y == T]) / n), verticals = T, do.points = F, col = "red", lwd = 2)
} 

gain.chart_cal <- function(n) {
    score <- runif(n)
    y <- (runif(n) < score)
    plot(performance(prediction(pred_train, cc_cal$churn), "tpr", "rpp"), lwd = 2, main = paste("N =", n))
    lines(ecdf((rank(-score)[y == T]) / n), verticals = T, do.points = F, col = "red", lwd = 2)
} 

set.seed(1)
par(mfrow = c(2, 2))

gain.chart_cal(10)
gain.chart_cal(100)
gain.chart_cal(1000)
gain.chart_cal(10000)

# Save the Gain Charts to a PDF
dev.print(pdf, "C:\\Data_Science\\Certifications\\AnalytixLabs\\Data Science in R\\Data Science with R - Final Projects\\Logistic Regression - Telecom Churn\\Gains_Chart_Calibration.pdf")

set.seed(1)
par(mfrow = c(2, 2))

gain.chart_val(10)
gain.chart_val(100)
gain.chart_val(1000)
gain.chart_val(10000)

# Save the Gain Charts to a PDF
dev.print(pdf, "C:\\Data_Science\\Certifications\\AnalytixLabs\\Data Science in R\\Data Science with R - Final Projects\\Logistic Regression - Telecom Churn\\Gains_Chart_Validation.pdf")

lift.chart_val <- function(n) {
    score <- runif(n)
    y <- (runif(n) < score)
    plot(performance(prediction(pred_test, cc_val$churn), "lift", "rpp"), lwd = 2, main = paste("N =", n))
    lines(ecdf((rank(-score)[y == T]) / n), verticals = T, do.points = F, col = "red", lwd = 2)
}

set.seed(1)
par(mfrow = c(2, 2))

lift.chart_val(10)
lift.chart_val(100)
lift.chart_val(1000)
lift.chart_val(10000)

# Save the Lift Charts to a PDF
dev.print(pdf, "C:\\Data_Science\\Certifications\\AnalytixLabs\\Data Science in R\\Data Science with R - Final Projects\\Logistic Regression - Telecom Churn\\Lift_Chart_Validation.pdf")

lift.chart_cal <- function(n) {
    score <- runif(n)
    y <- (runif(n) < score)
    plot(performance(prediction(pred_train, cc_cal$churn), "lift", "rpp"), lwd = 2, main = paste("N =", n))
    lines(ecdf((rank(-score)[y == T]) / n), verticals = T, do.points = F, col = "red", lwd = 2)
}

set.seed(1)
par(mfrow = c(2, 2))

lift.chart_cal(10)
lift.chart_cal(100)
lift.chart_cal(1000)
lift.chart_cal(10000)

# Save the Lift Charts to a PDF
dev.print(pdf, "C:\\Data_Science\\Certifications\\AnalytixLabs\\Data Science in R\\Data Science with R - Final Projects\\Logistic Regression - Telecom Churn\\Lift_Chart_Calibration.pdf")

# Append probability columns to the datasets
# This is needed for the Decile analysis

cc_val$probs <- pred_test
cc_cal$probs <- pred_train

# We first group the probabilities by quartiles and run the program to make 10 bins for the probabilities

# Testing
decLocations <- quantile(cc_val$probs, probs = seq(0.1,0.9,by=0.1))
cc_val$decile <- findInterval(cc_val$probs,c(-Inf,decLocations, Inf))

#Training
decLocations <- quantile(cc_cal$probs, probs = seq(0.1,0.9,by=0.1))
cc_cal$decile <- findInterval(cc_cal$probs,c(-Inf,decLocations, Inf))

# Check the Deciles created

#Validation
summary(cc_val$decile)
xtabs(~decile,cc_val)

#Calibration
summary(cc_cal$decile)
xtabs(~decile,cc_cal)

Validation_DA <- sqldf("select decile Decile,  min(probs) as Min_prob, max(probs) as Max_prob,  sum(churn) as Bad_cnt, 
                           (count(decile)-sum(churn)) as Good_cnt from cc_val
                           group by decile
                     order by decile desc")

Calibration_DA <- sqldf("select decile Decile,  min(probs) as Min_prob, max(probs) as Max_prob,  sum(churn) as Bad_cnt, 
                           (count(decile)-sum(churn)) as Good_cnt from cc_cal
                           group by decile
                     order by decile desc")

Calibration_DA
Validation_DA

# Save the Decile Analysis Reports
write.csv(Validation_DA,"C:\\Data_Science\\Certifications\\AnalytixLabs\\Data Science in R\\Data Science with R - Final Projects\\Logistic Regression - Telecom Churn\\Validation_DA.csv")
write.csv(Calibration_DA,"C:\\Data_Science\\Certifications\\AnalytixLabs\\Data Science in R\\Data Science with R - Final Projects\\Logistic Regression - Telecom Churn\\Calibration_DA.csv")

sum <- summary(step.model)$coefficients
write.csv(sum,"C:\\Data_Science\\Certifications\\AnalytixLabs\\Data Science in R\\Data Science with R - Final Projects\\Logistic Regression - Telecom Churn\\model_summary.csv")

tree.model <- ctree(churn ~ ., cc_cal)
plot(tree.model, type='simple')

p1 <- predict(tree.model, cc_val)

y_predicted <- factor(p1)
y_actual <- cc_val$churn

cat("Prediction Accuracy %:", mean(y_predicted == y_actual) * 100 )

glmnetcm <- confusionMatrix(y_predicted, y_actual)
glmnetcm

rfModel <- randomForest(churn ~ ., cc_cal)
print(rfModel)

pred_rf <- predict(rfModel, cc_val)
y_predicted <- factor(pred_rf)
y_actual <- cc_val$churn
cat("Prediction Accuracy %:", mean(y_predicted == y_actual) * 100)

caret::confusionMatrix(y_predicted, y_actual)

# Random Forest Error Rate
# We use this plot to help us determine the number of trees. 
# As the number of trees increases, the OOB error rate decreases, and then becomes almost constant. 
# We are not able to decrease the OOB error rate after about 100 trees.

plot(rfModel)

# The 17th column is churn, 1st parameter is the vector of predictor variables and 2nd parameter is the predicted variable.

#t <- tuneRF(cc_cal_train[, -17], cc_cal_train[, 17], stepFactor = 0.5, plot = TRUE, ntreeTry = 100, trace = TRUE, improve = 0.05)

# Memory Error, commenting it out for now due to memory issues
#memory.limit()
#memory.limit(size=3000)

#rfModel_new <- randomForest(churn ~ ., data=cc_cal_train , ntree = 100, mtry = 2, importance = TRUE, proximity = TRUE)
#print(rfModel_new)

#pred_rf_new <- predict(rfModel_new, cc_cal_test)
#caret::confusionMatrix(pred_rf_new, cc_cal_test$churndep)

#Random Forest Feature Importance

randomForest::varImpPlot(rfModel, sort=T, n.var = 10, main = 'Top 10 Feature Importance')

imp <- as.data.frame(varImp(log.model))
imp <- data.frame(columns = rownames(imp), overall = imp$Overall)
imp <- imp[order(imp$overall,decreasing = T),]
imp

write.csv(imp,"C:\\Data_Science\\Certifications\\AnalytixLabs\\Data Science in R\\Data Science with R - Final Projects\\Logistic Regression - Telecom Churn\\var_imp.csv")

# Variable Importance: Draw a Bar Chart

ggplot(imp, aes(x=columns, y=overall))  + 
  geom_bar(stat="identity", width=.5, fill="tomato3") + 
  labs(title="Ordered Bar Chart", 
       subtitle="Variables vs Importance", 
       caption="source: varimp") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

saveRDS(step.model, "C:\\Data_Science\\Certifications\\AnalytixLabs\\Data Science in R\\Data Science with R - Final Projects\\Logistic Regression - Telecom Churn\\LogisticRegression.rda")
saveRDS(tree.model, "C:\\Data_Science\\Certifications\\AnalytixLabs\\Data Science in R\\Data Science with R - Final Projects\\Logistic Regression - Telecom Churn\\DecisionTree.rda")
saveRDS(rfModel, "C:\\Data_Science\\Certifications\\AnalytixLabs\\Data Science in R\\Data Science with R - Final Projects\\Logistic Regression - Telecom Churn\\RandomForest.rda")

svm.model = svm(formula = churn ~ .,  
                 data = cc_cal, 
                 type = 'C-classification', 
                 kernel = 'linear')  

pred_svm <- predict(svm.model, cc_val)
y_predicted <- factor(pred_svm)
y_actual <- cc_val$churn
cat("Prediction Accuracy %:", mean(y_predicted == y_actual) * 100)

caret::confusionMatrix(y_predicted, y_actual)

saveRDS(svm.model, "C:\\Data_Science\\Certifications\\AnalytixLabs\\Data Science in R\\Data Science with R - Final Projects\\Logistic Regression - Telecom Churn\\SVM.rda")

par(mfrow = c(2,5))

boxplot(sqrt.eqpdays~churn, ylab="eqpdays", xlab= "churn", col="light blue",data = cc)
boxplot(cbrt.overage~churn, ylab="overage", xlab= "churn", col="light blue",data = cc)
boxplot(changem~churn, ylab="changem", xlab= "churn", col="light blue",data = cc)
boxplot(sqrt.mou~churn, ylab="sqrt.mou", xlab= "churn", col="light blue",data = cc)
boxplot(refurb~churn, ylab="refurb", xlab= "churn", col="light blue",data = cc)
boxplot(ownrent~churn, ylab="ownrent", xlab= "churn", col="light blue",data = cc)
boxplot(log.phones~churn, ylab="phones", xlab= "churn", col="light blue",data = cc)
boxplot(log.months~churn, ylab="log.months", xlab= "churn", col="light blue",data = cc)
boxplot(webcap~churn, ylab="webcap", xlab= "churn", col="light blue",data = cc)
boxplot(sqrt.outcalls~churn, ylab="sqrt.outcalls", xlab= "churn", col="light blue",data = cc)
