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
# install.packages('DMwR')
# install.packages('ROCR')
# install.packages("ROSE")

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
library(DMwR)
library(ROCR)
library(ROSE)

web <- read.csv("C:\\Data_Science\\Certifications\\AnalytixLabs\\Data Science in R\\Data Science with R - Final Projects\\Classification - ECommerce Case Study\\train.csv")
head(web,5)

NROW(web)
NCOL(web)

str(web)

summary(web)

describe(web)

# Plot a histogram on all numeric columns to see the distribution
# Some columns will have only few distinct values. These can later be classifed as categorical variables.

num_data <-web[, sapply(web, is.numeric)] 
for (i in 1:length(names(num_data))){
  print(i)
  hist( num_data[i], main='hist', breaks=20, prob=TRUE)
}

# #Check which numeric columns can be renamed as Categorical
# Check for unique values
# Exclude the uniqueid column
apply(web[,2:32],2,unique)

#Convert the predicted variable to categorical
web$target <- as.factor(web$target)
colnames(web) 

web_num <- dplyr::select_if(web, is.numeric)
num_var <- names(web_num)

head(web_num,5)
str(web_num)

web_cat <- dplyr::select_if(web, is.factor)
cat_var <- names(web_cat)

str(web_cat)
head(web_cat)

# Statistics for Numeric variables

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
  max = max(a,na.rm = T)
  UC = m+3*s
  LC = m-3*s
  outlier_flag= max>1.5*(p99)
  return(c(class=c,n=n, nmiss=nmiss, outlier_flag=outlier_flag, mean=m,median=med, stdev=s,min = min,
           q1=q1,q2=q2,q3=q3,p99=p99,max=max, UC=UC, LC=LC ))
}

#Statistics for Categorical variables

mystats_cat=function(x){
    Var_Type=class(x)
    n=length(x)
    nmiss=sum(is.na(x))
    return( c(Var_Type=Var_Type, n=n,nmiss=nmiss))
  }

# Save the stats in a file
num_stats <- data.frame(apply(web_num,2,FUN = mystats_num))
write.csv(num_stats,"C:\\Data_Science\\Certifications\\AnalytixLabs\\Data Science in R\\Data Science with R - Final Projects\\Classification - ECommerce Case Study\\Numeric_Stats.csv")

cat_stats <- data.frame(apply(web_cat,2,FUN = mystats_cat))
write.csv(cat_stats,"C:\\Data_Science\\Certifications\\AnalytixLabs\\Data Science in R\\Data Science with R - Final Projects\\Classification - ECommerce Case Study\\Categorical_Stats.csv")


str(web_num)
str(web_cat)

# Replace all "-1" with NA
web_num <- replace(web_num, web_num =="-1", NA)
web_cat <- replace(web_cat, web_cat =="-1", NA)

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

findMissingValues(web_num)

findMissingValues(web_cat)

#These columns have very high NA percentage between 50% and 99%.
#So, we will drop these columns from numeric list
#No missing values in categorical list

web_num <- subset(web_num,select=-c(page1_exits,page2_exits,page3_exits,page4_exits,page5_exits,page6_exits))

findMissingValues(web_num)

# Create a function that will find and cap the outliers.
# An outlier is considered if it is below the (Q1 – 1.5*IQR) or above (Q3 + 1.5*IQR)
# We will cap the low and high outliers with 5th and 95th percentile values respectively

HandleOutlier <- function(x,names){
  
   m1 <- mean(x, na.rm = T) 
   cat("\n Mean with outliers:\n", round(m1, 4))
   OutVals = boxplot(x, main="Boxplot with outliers",horizontal = TRUE,col = "red")$out
   cat("\n Pre-treatment Outliers:\n", OutVals)
   which(x %in% OutVals)    
#   hist(x, main="Histogram with outliers", xlab=NA, ylab=NA, col="red",)
#   plot(x,xlab="Plot with Outliers", ylab="values", pch="*", col="red", cex=2)

#  Method 1: Capping with 5th and 95th percentiles  
    
   qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
   caps <- quantile(x, probs=c(.05, .95), na.rm = T)
   H <- 1.5 * IQR(x, na.rm = T)
   x[x < (qnt[1] - H)] <- caps[1]
   x[x > (qnt[2] + H)] <- caps[2]
    
#  Method 2: We can replace the outliers with NAs and later impute NAs using MICE or kNN  
    
#  y[x < (qnt[1] - H)] <- NA
#  y[x > (qnt[2] + H)] <- NA
    
#  Method 3: Imputation with mean / median / mode
    
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

## Find and Apply outlier treatment to all Numeric variables
web_num <- data.frame(lapply(web_num,HandleOutlier))

cormat <- cor(web_num,use="pairwise.complete.obs")
corplot <- corrplot(cormat, method = "number", tl.cex = 0.75)
corplot

# Some columns have NAs because the Standard Deviation of these columns is 0 after outlier handling
# Let's drop these columns.
sd(web_num$page1_top)
sd(web_num$visited_page1)

web_num <- subset(web_num,select = -c(page1_top, visited_page1,page2_top,visited_page2,page4_top,visited_page4,page5_top,visited_page5))

# Check if any columns still have SD=0 after outlier handling
apply(web_num,2,sd) %>% sort()

# Regenerate the correlation matrix. There should not be any NA values
cormat <- cor(web_num,use="pairwise.complete.obs")
corplot <- corrplot(cormat, method = "number", tl.cex = 0.75)
corplot

highlyCorrelated <- caret::findCorrelation(cormat, cutoff=0.7, verbose = TRUE, names = TRUE)
highlyCorrelated

#Remove columns with HIGH correlation
web_num <- subset(web_num, select = -c(metric4,page3_top,metric1,dayHourMinute))

cormat <- cor(web_num,use="pairwise.complete.obs")
corplot <- corrplot(cormat, method = "number", tl.cex = 0.75)
corplot

# Now check again, all correlations should be less than 0.7
highlyCorrelated1 <- caret::findCorrelation(cormat, cutoff=0.7, verbose = TRUE, names = TRUE)
highlyCorrelated1

str(web_num)

# There are no Zero or near-Zero variances
# So we will use a variance cutoff
nearZeroVar(web_num, saveMetrics=TRUE)

# We are going to use the var function to check the variance of the different columns
apply(web_num, 2, var) %>% sort()

# Let's find which variables have less than 0.1 as variance
which(apply(web_num, 2, var) <= 0.1)

# Remove columns with less than 0.1 as variance
web_num <- subset(web_num,select=-c(visited_page3))

apply(web_num, 2, var) %>% sort()

# We can run Chi-Squred test for all columns with churndep and check the p-values 
# Need to find a better way to loop through all the columns in the dataframe

chisq.test(web_cat$target, web_cat$region, correct=FALSE)
chisq.test(web_cat$target, web_cat$sourceMedium, correct=FALSE)
chisq.test(web_cat$target, web_cat$device, correct=FALSE)
chisq.test(web_cat$target, web_cat$country, correct=FALSE)

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

mat <- chisqmatrix(web_cat)
mat

# p-values of churn vs other categorical variables
mat[1,] %>% sort() 

# As an alternative, we can use the chi.squared function from FSelector package
# This package provides functions for selecting attributes from a given dataset. Attribute subset selection is the process of identifying and removing as much of the irrelevant and redundant information as possible.
# CRAN page: http://cran.r-project.org/web/packages/FSelector/index.html
# The result is equal to Cramer's V coefficient between source attributes and destination attribute.
# Here we filter out the top 10 columns.

p <- FSelector::chi.squared(target~., web_cat)
print(p)
cutoff.k(p, 4)

# List of categorical and numeric variables
num_var <- names(web_num)
cat_var <- names(web_cat)

web<-cbind.data.frame(web_num,web_cat)
str(web)

write.csv(web, file="C:\\Data_Science\\Certifications\\AnalytixLabs\\Data Science in R\\Data Science with R - Final Projects\\Classification - ECommerce Case Study\\web_final.csv")

# Divide the web dataset into 2 parts; training and test datasets in 80:20 ratio
set.seed(222)
ind <- sample(2,nrow(web),replace = TRUE,prob = c(0.8,0.2))
web_train <- web[ind==1,]
web_test <- web[ind==2,]

nrow(web_train)
ncol(web_train)

nrow(web_test)
ncol(web_test)

# Almost 90% of the datasets are positive cases and 10% are negative cases
# So, there is some class imbalance

prop.table(table(web_train$target))
prop.table(table(web_test$target))

set.seed(9560)
up_train <- upSample(x = web_train[, -ncol(web_train)],
                         y = web_train$target)
table(up_train$Class)

up_train$target <- up_train$Class
up_train <- subset(up_train, select =-c(Class))

set.seed(9560)
smote_train <- DMwR::SMOTE(target ~ ., data  = web_train)                         
table(smote_train$target) 

rose_train <- ROSE(target ~ ., data = web_train, seed = 1)$data
table(rose_train$target)

# We will exclude the categorical columns region,sourceMedium and country because they have too many levels.
# Logistic Regression becomes very slow with these categorical variables.

log.model <- glm(target ~ metric2+metric6+metric3+metric5+binary_var1+binary_var2+page6_top+visited_page6+device, family=binomial, data=smote_train) 
print(summary(log.model))  

pred <- predict(log.model, newdata = web_test, type='response')
y_pred_num <- ifelse(pred > 0.5, 1, 0)
y_predicted <- factor(y_pred_num, levels=c(0, 1)) 
y_actual <- web_test$target
cat("Prediction Accuracy %:", mean(y_predicted == y_actual) * 100) 

## Computing a simple ROC curve (x-axis: fpr, y-axis: tpr)

pred1 <- ROCR::prediction(pred, web_test$target)
perf <- performance(pred1,"tpr","fpr")
plot(perf,col=rainbow(10))

## precision/recall curve (x-axis: recall, y-axis: precision)
perf1 <- performance(pred1, "prec", "rec")
plot(perf1,col=rainbow(10))

## sensitivity/specificity curve (x-axis: specificity,y-axis: sensitivity)
perf1 <- performance(pred1, "sens", "spec")
plot(perf1,col=rainbow(10))

auc.perf = performance(pred1, measure = "auc")
print(sprintf("Area Under Curve(AUC) %f", as.numeric(auc.perf@y.values)))

acc.perf = performance(pred1, measure = "acc")
plot(acc.perf)

# Calculate Optimal Acuracy and Cutoff
# Then you can go forth and threshold your model using the cutoff for (in hopes) maximum accuracy in your test data.
ind = which.max( slot(acc.perf, "y.values")[[1]] )
acc = slot(acc.perf, "y.values")[[1]][ind]
cutoff = slot(acc.perf, "x.values")[[1]][ind]
print(c(accuracy= acc, cutoff = cutoff))

conmat <- confusionMatrix(y_predicted, y_actual)
conmat

# Sensitivity (also known as recall) and Pos Pred Value(also known as precision). 
F1 <- (2 * 0.9708 * 0.8236) / (0.9708 + 0.8236)
F1

saveRDS(log.model, "C:\\Data_Science\\Certifications\\AnalytixLabs\\Data Science in R\\Data Science with R - Final Projects\\Classification - ECommerce Case Study\\LogisticRegression.rda")

tree.model <- ctree(target ~ metric2+metric6+metric3+metric5+binary_var1+binary_var2+page6_top+visited_page6+device, rose_train)
plot(tree.model, type='simple')

p1 <- predict(tree.model, web_test)
y_predicted <- factor(p1)
y_actual <- web_test$target
cat("Prediction Accuracy %:", mean(y_predicted == y_actual) * 100) 

glmnetcm <- confusionMatrix(y_predicted, y_actual)
glmnetcm

saveRDS(tree.model, "C:\\Data_Science\\Certifications\\AnalytixLabs\\Data Science in R\\Data Science with R - Final Projects\\Classification - ECommerce Case Study\\DecisionTree.rda")

# Memory Error, commenting out for now
memory.limit()
memory.limit(size=100000)

# Random Forest is giving memory issues without ntree option, so limiting the number of trees to 100

rfModel <- randomForest(target ~ metric2+metric6+metric3+metric5+binary_var1+binary_var2+page6_top+visited_page6+device, ntree=100, rose_train)
print(rfModel)

plot(rfModel)

#Variable Importance
varImp(rfModel)

## Important variables according to the model
varImpPlot(rfModel,  
           sort = T,
           n.var=25,
           main="Variable Importance")

pred_rf <- predict(rfModel, web_test)
y_predicted <- factor(pred_rf)
y_actual <- web_test$target
cat("Prediction Accuracy %:", mean(y_predicted == y_actual) * 100)

conmat <- confusionMatrix(y_predicted, y_actual)
conmat

saveRDS(rfModel, "C:\\Data_Science\\Certifications\\AnalytixLabs\\Data Science in R\\Data Science with R - Final Projects\\Classification - ECommerce Case Study\\RandomForest.rda")

# Here, I have removed couple of features(page6_top,visited_page6,device) to get an 81% accuracy rate
NBModel <- naiveBayes(target ~ metric2+metric6+metric3+metric5+binary_var1+binary_var2, smote_train)
print(NBModel)

pred_nb <- predict(NBModel, web_test)
y_predicted <- factor(pred_nb)
y_actual <- web_test$target
cat("Prediction Accuracy %:", mean(y_predicted == y_actual) * 100)

conmat <- confusionMatrix(y_predicted, y_actual)
conmat

saveRDS(NBModel, "C:\\Data_Science\\Certifications\\AnalytixLabs\\Data Science in R\\Data Science with R - Final Projects\\Classification - ECommerce Case Study\\NaiveBayes.rda")

# Read the test.csv into R
test <- read.csv("C:\\Data_Science\\Certifications\\AnalytixLabs\\Data Science in R\\Data Science with R - Final Projects\\Classification - ECommerce Case Study\\test.csv")
head(test,2)

# Predict target using RandomForest model
pred_rf <- predict(rfModel, test)
predict_target <- factor(pred_rf) 

#Combine the test dataset and predict_target column
#Select only 2 columns :unique_id and predicted_y
res <- cbind(test,predict_target)
res <- subset(res, select=c(unique_id,predict_target))

#Save the resultset into a CSV file
write.csv(res,"C:\\Data_Science\\Certifications\\AnalytixLabs\\Data Science in R\\Data Science with R - Final Projects\\Classification - ECommerce Case Study\\results.csv")
