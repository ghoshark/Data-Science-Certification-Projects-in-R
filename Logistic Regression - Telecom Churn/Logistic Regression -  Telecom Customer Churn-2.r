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

colnames(cc) 

head(cc,2)

str(cc)

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
cc_num$eqpdays[is.na(cc_num$eqpdays)] <- 380.265630718126
cc_num$age1[is.na(cc_num$age1)] <- 31.3751128175007
cc_num$age2[is.na(cc_num$age2)] <- 21.1577152844434

cc_num$setprc[is.na(cc_num$setprc)] <- 82.5827008247289
cc_num$income[is.na(cc_num$income)] <- 5.77769855714205

# No missing values as expected

findMissingValues(cc_cat)

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
cc_num <- data.frame(lapply(cc_num,HandleOutlier))

head(cc_num,5)
head(cc_cat,5)

cormat <- cor(cc_num,use="pairwise.complete.obs")
corplot <- corrplot(cormat, method = "number", tl.cex = 0.75)
corplot

# Some columns have NAs because the Standard Deviation of these columns is 0 after outlier handling
# Let's drop these columns.
sd(cc_num$callfwdv)
sd(cc_num$refer)

cc_num <- subset(cc_num,select = -c(callfwdv, creditgy,creditz,prizmrur,occcler,occcrft,occstud,occhmkr,occret,occself,mailflag,retcalls,retaccpt,refer,mcycle,creditad,retcall))

# Check if any columns still have SD=0 after outlier handling
apply(cc_num,2,sd) %>% sort()


# Regenerate the correlation matrix. There should not be any NA values
cormat <- cor(cc_num,use="pairwise.complete.obs")
corplot <- corrplot(cormat, method = "number", tl.cex = 0.75)
corplot

highlyCorrelated <- caret::findCorrelation(cormat, cutoff=0.75, verbose = TRUE, names = TRUE)
highlyCorrelated

#Remove columns with HIGH correlation
# Exclude income
cc_num <- subset(cc_num, select = -c(mou,opeakvce,mourec,dropblk,ownrent,creditcd,mailres,phones,models,uniqsubs))

cormat <- cor(cc_num,use="pairwise.complete.obs")
corplot <- corrplot(cormat, method = "number", tl.cex = 0.75)
corplot

# Now check again, all correlations should be less than 0.75
highlyCorrelated1 <- caret::findCorrelation(cormat, cutoff=0.75, verbose = TRUE, names = TRUE)
highlyCorrelated1

str(cc_num)

# There are no Zero or near-Zero variances
# So we will use a variance cutoff
nearZeroVar(cc_num, saveMetrics=TRUE)

# We are going to use the var function to check the variance of the different columns
apply(cc_num, 2, var) %>% sort()

# Let's find which variables have less than 1 as variance
which(apply(cc_num, 2, var) <= 1)

# Remove columns with less than 1 as variance
cc_num <- subset(cc_num,select=-c(setprcm,incmiss,newcelln,newcelly,pcown,travel,mailord,marryno,marryyes,marryun,occprof,rv,truck,webcap,refurb,prizmtwn,prizmub,creditde,creditc,creditb,creditaa,credita,children,actvsubs,threeway))

str(cc_num)

# List of categorical and numeric variables
num_var <- names(cc_num)
cat_var <- names(cc_cat)

cc<-cbind.data.frame(cc_num,cc_cat)
str(cc)

set.seed(100)
rPartMod <- train(churn ~ ., data=cc, method="rpart")
rpartImp <- varImp(rPartMod)
print(rpartImp)

# We will keep only the important features in the final dataset + calibrat + churn
cc <- subset(cc,select=c(eqpdays,months,changem,recchrge,setprc,incalls,changer,overage,calibrat,churn))

par(mfrow = c(2,4))

boxplot(eqpdays~churn, ylab="eqpdays", xlab= "churn", col="light blue",data = cc)
boxplot(months~churn, ylab="months", xlab= "churn", col="light blue",data = cc)
boxplot(changem~churn, ylab="changem", xlab= "churn", col="light blue",data = cc)
boxplot(recchrge~churn, ylab="recchrge", xlab= "churn", col="light blue",data = cc)
boxplot(setprc~churn, ylab="setprc", xlab= "churn", col="light blue",data = cc)
boxplot(incalls~churn, ylab="incalls", xlab= "churn", col="light blue",data = cc)
boxplot(overage~churn, ylab="overage", xlab= "churn", col="light blue",data = cc)
boxplot(changer~churn, ylab="changer", xlab= "churn", col="light blue",data = cc)

write.csv(cc, file="C:\\Data_Science\\Certifications\\AnalytixLabs\\Data Science in R\\Data Science with R - Final Projects\\Logistic Regression - Telecom Churn\\telecom_final.csv")

# Build two datasets one for model calibration and another for validation
# Calibration dataset to be used to create the model and predict churndep for validation dataset
cc_cal <- cc %>% subset(calibrat==1)
cc_val <- cc %>% subset(calibrat==0)

# Drop the calibrat column since it's no longer needed
cc_cal <- subset(cc_cal,select=-c(calibrat))
cc_val <- subset(cc_val,select=-c(calibrat))

# Divide the calibration dataset into 2 parts: training and test datasets in 85:15 ratio
set.seed(222)
ind <- sample(2,nrow(cc_cal),replace = TRUE,prob = c(0.75,0.25))
cc_cal_train <- cc_cal[ind==1,]
cc_cal_test <- cc_cal[ind==2,]

nrow(cc_cal_train)
ncol(cc_cal_train)

nrow(cc_cal_test)
ncol(cc_cal_test)

nrow(cc_val)
ncol(cc_val)

# Almost 50% of the datasets are positives cases and 49% are negative cases
# So there is no class imbalance
prop.table(table(cc_cal_train$churn))
prop.table(table(cc_cal_test$churn))

log.model <- glm(churn ~ .,family=binomial, data=cc_cal_train)
print(summary(log.model))

# Turn of any warnings (retcall1 is NA so you may get a warning like "prediction from a rank-deficient fit may be misleading"
options(warn=-1)
pred <- predict(log.model, newdata = cc_cal_test, type='response')

y_pred_num <- ifelse(pred > 0.488506, 1, 0)
y_predicted <- factor(y_pred_num, levels=c(0, 1))
y_actual <- cc_cal_test$churn

cat("Prediction Accuracy %:", mean(y_predicted == y_actual) * 100 )

pred1 <- ROCR::prediction(pred, cc_cal_test$churn)
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

log.model2 <- stepAIC(log.model)
summary(log.model2)

log.model$aic
log.model2$aic

tree.model <- ctree(churn ~ ., cc_cal_train)
plot(tree.model, type='simple')

p1 <- predict(tree.model, cc_cal_test)

y_predicted <- factor(p1)
y_actual <- cc_cal_test$churn

cat("Prediction Accuracy %:", mean(y_predicted == y_actual) * 100 )

glmnetcm <- confusionMatrix(y_predicted, y_actual)
glmnetcm

rfModel <- randomForest(churn ~ ., cc_cal_train)
print(rfModel)

pred_rf <- predict(rfModel, cc_cal_test)
y_predicted <- factor(pred_rf)
y_actual <- cc_cal_test$churn
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

saveRDS(log.model, "C:\\Data_Science\\Certifications\\AnalytixLabs\\Data Science in R\\Data Science with R - Final Projects\\Logistic Regression - Telecom Churn\\LogisticRegression.rda")
saveRDS(tree.model, "C:\\Data_Science\\Certifications\\AnalytixLabs\\Data Science in R\\Data Science with R - Final Projects\\Logistic Regression - Telecom Churn\\DecisionTree.rda")
saveRDS(rfModel, "C:\\Data_Science\\Certifications\\AnalytixLabs\\Data Science in R\\Data Science with R - Final Projects\\Logistic Regression - Telecom Churn\\RandomForest.rda")

svm.model = svm(formula = churn ~ .,  
                 data = cc_cal_train, 
                 type = 'C-classification', 
                 kernel = 'linear')  

pred_svm <- predict(svm.model, cc_cal_test)
y_predicted <- factor(pred_svm)
y_actual <- cc_cal_test$churn
cat("Prediction Accuracy %:", mean(y_predicted == y_actual) * 100)

caret::confusionMatrix(y_predicted, y_actual)

saveRDS(svm.model, "C:\\Data_Science\\Certifications\\AnalytixLabs\\Data Science in R\\Data Science with R - Final Projects\\Logistic Regression - Telecom Churn\\SVM.rda")

par(mfrow = c(2,4))

boxplot(eqpdays~churn, ylab="eqpdays", xlab= "churn", col="light blue",data = cc)
boxplot(months~churn, ylab="months", xlab= "churn", col="light blue",data = cc)
boxplot(changem~churn, ylab="changem", xlab= "churn", col="light blue",data = cc)
boxplot(recchrge~churn, ylab="recchrge", xlab= "churn", col="light blue",data = cc)
boxplot(setprc~churn, ylab="setprc", xlab= "churn", col="light blue",data = cc)
boxplot(incalls~churn, ylab="incalls", xlab= "churn", col="light blue",data = cc)
boxplot(overage~churn, ylab="overage", xlab= "churn", col="light blue",data = cc)
boxplot(changer~churn, ylab="changer", xlab= "churn", col="light blue",data = cc)
