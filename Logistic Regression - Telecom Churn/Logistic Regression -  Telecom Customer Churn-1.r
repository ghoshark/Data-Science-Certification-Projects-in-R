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
# install.packages("e1071")

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
library(e1071)

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

# #Check which numeric columns can be renamed as Categorical
# Check for unique values
apply(cc,2,unique)

cc$RETCALL <- as.factor(cc$RETCALL)
cc$SETPRCM <- as.factor(cc$SETPRCM)
cc$CREDITAD <- as.factor(cc$CREDITAD)
cc$MCYCLE <- as.factor(cc$MCYCLE)
cc$INCOME <- as.factor(cc$INCOME)
cc$INCMISS <- as.factor(cc$INCMISS)
cc$NEWCELLN <- as.factor(cc$NEWCELLN)
cc$NEWCELLY <- as.factor(cc$NEWCELLY)
cc$RETACCPT <- as.factor(cc$RETACCPT)
cc$RETCALLS <- as.factor(cc$RETCALLS)
cc$CREDITCD <- as.factor(cc$CREDITCD)
cc$PCOWN <- as.factor(cc$PCOWN)
cc$TRAVEL <- as.factor(cc$TRAVEL)
cc$MAILFLAG <- as.factor(cc$MAILFLAG)
cc$MAILRES <- as.factor(cc$MAILRES)
cc$MAILORD <- as.factor(cc$MAILORD)
cc$MARRYNO <- as.factor(cc$MARRYNO)
cc$MARRYYES <- as.factor(cc$MARRYYES)
cc$MARRYUN  <- as.factor(cc$MARRYUN)
cc$OWNRENT <- as.factor(cc$OWNRENT)
cc$OCCSELF <- as.factor(cc$OCCSELF)
cc$OCCRET <- as.factor(cc$OCCRET)
cc$OCCHMKR <- as.factor(cc$OCCHMKR)
cc$OCCSTUD <- as.factor(cc$OCCSTUD)
cc$OCCCRFT <- as.factor(cc$OCCCRFT)
cc$OCCCLER <- as.factor(cc$OCCCLER)
cc$OCCPROF <- as.factor(cc$OCCPROF)
cc$RV <- as.factor(cc$RV)
cc$TRUCK <- as.factor(cc$TRUCK)
cc$WEBCAP <- as.factor(cc$WEBCAP)
cc$REFURB <- as.factor(cc$REFURB)
cc$PRIZMTWN <- as.factor(cc$PRIZMTWN)
cc$PRIZMUB <- as.factor(cc$PRIZMUB)
cc$PRIZMRUR <- as.factor(cc$PRIZMRUR)
cc$CREDITZ <- as.factor(cc$CREDITZ)
cc$CREDITGY <- as.factor(cc$CREDITGY)
cc$CREDITDE <- as.factor(cc$CREDITDE)
cc$CREDITC <- as.factor(cc$CREDITC)
cc$CREDITB <- as.factor(cc$CREDITB)
cc$CREDITAA <- as.factor(cc$CREDITAA)
cc$CREDITA <- as.factor(cc$CREDITA)
cc$CHILDREN <- as.factor(cc$CHILDREN)
cc$CALIBRAT <- as.factor(cc$CALIBRAT)
cc$CHURNDEP <- as.factor(cc$CHURNDEP)


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

# Expected 31047 missing values for "churndep" in categorical list

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
   hist(x, main="Histogram with outliers", xlab=NA, ylab=NA, col="red",)
   plot(x,xlab="Plot with Outliers", ylab="values", pch="*", col="red", cex=2)

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
   hist(x, main="Histogram without outliers", xlab=NA, ylab=NA, col="green")
   plot(x,xlab="Plot without Outliers", ylab="values", pch="*", col="green", cex=2)

   return(x)
}

## Find and Apply outlier treatment to all Numeric variables
cc_num <- data.frame(lapply(cc_num,HandleOutlier))
#cc_num_bkp <-cc_num 

head(cc_num,5)
head(cc_cat,5)

cormat <- cor(cc_num,use="pairwise.complete.obs")
corplot <- corrplot(cormat, method = "number", tl.cex = 0.75)
corplot

# Some columns have NAs because the Standard Deviation of these columns is 0 after outlier handling
# Let's drop these columns.
sd(cc_num$callfwdv)
sd(cc_num$refer)

cc_num <- subset(cc_num,select = -c(callfwdv, refer))

# Check if any columns still have SD=0 after outlier handling
apply(cc_num,2,sd) %>% sort()


# Regenerate the correlation matrix. There should not be any NA values
cormat <- cor(cc_num,use="pairwise.complete.obs")
corplot <- corrplot(cormat, method = "number", tl.cex = 0.75)
corplot

highlyCorrelated <- caret::findCorrelation(cormat, cutoff=0.75, verbose = TRUE, names = TRUE)
highlyCorrelated

#Remove columns with HIGH correlation
cc_num <- subset(cc_num, select = -c(mou,peakvce,opeakvce,dropblk,phones,uniqsubs))

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

# Let's find which variables have less than 10 as variance
which(apply(cc_num, 2, var) <= 10)

# Remove columns with less than 10 as variance
cc_num <- subset(cc_num,select=-c(directas,roam,custcare,threeway,callwait,actvsubs,models))

# cc_num <- subset(cc_num,select=c(eqpdays,changem,mourec,overage,setprc))

# We can run Chi-Squred test for all columns with churndep and check the p-values 
# Need to find a better way to loop through all the columns in the dataframe
chisq.test(cc_cat$churn, cc_cat$children, correct=FALSE)
chisq.test(cc_cat$churn, cc_cat$income, correct=FALSE)

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

# p-values of churn vs other categorical variables
mat[1,] %>% sort() 

# Select the columns with p-values < 0.05 plus calibrat and churn
# cc_cat <- subset(cc_cat,select=c(retcall, retcalls, webcap, creditde, retaccpt, setprcm, refurb, mailres, mailord, creditaa, credita, marryun, creditb, creditc, incmiss, marryno, income, ownrent, prizmrur, creditcd, prizmtwn, prizmtwn, newcelly, occret, occprof, calibrat, churn ))

# As an alternative, we can use the chi.squared function from FSelector package
# This package provides functions for selecting attributes from a given dataset. Attribute subset selection is the process of identifying and removing as much of the irrelevant and redundant information as possible.
# CRAN page: http://cran.r-project.org/web/packages/FSelector/index.html
# The result is equal to Cramer's V coefficient between source attributes and destination attribute.
# Here we filter out the top 10 columns.

p <- FSelector::chi.squared(churn~., cc_cat)
print(p)
cutoff.k(p, 11)

# Select the top 10 columns + the churn and calibrat columns in the categorical dataset
cc_cat <- subset(cc_cat,select=c(retcalls,retcall,webcap,creditde,retaccpt,setprcm,refurb,mailres,mailord,income,calibrat,churn))

head(cc_cat,2)

# List of categorical and numeric variables
num_var <- names(cc_num)
cat_var <- names(cc_cat)

cc<-cbind.data.frame(cc_num,cc_cat)
str(cc)

write.csv(cc, file="C:\\Data_Science\\Certifications\\AnalytixLabs\\Data Science in R\\Data Science with R - Final Projects\\Logistic Regression - Telecom Churn\\telecom_final.csv")

# Build two datasets one for model calibration and another for validation
# Calibration dataset to be used to create the model and predict churndep for validation dataset
cc_cal <- cc %>% subset(calibrat==1)
cc_val <- cc %>% subset(calibrat==0)

# Drop the calibrat column since it's no longer needed
cc_cal <- subset(cc_cal,select=-c(calibrat))
cc_val <- subset(cc_val,select=-c(calibrat))

# Divide the calibration dataset into 2 parts; training and test datasets in 75:25 ratio
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

y_pred_num <- ifelse(pred > 0.5, 1, 0)
y_predicted <- factor(y_pred_num, levels=c(0, 1))
y_actual <- cc_cal_test$churn

cat("Prediction Accuracy %:", mean(y_predicted == y_actual) * 100 )

glmnetcm <- confusionMatrix(y_predicted, y_actual)
glmnetcm

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

# The 27th column is churndep, 1st parameter is the vector of predictor variables and 2nd parameter is the predicted variable.

t <- tuneRF(cc_cal_train[, -27], cc_cal_train[, 27], stepFactor = 0.5, plot = TRUE, ntreeTry = 100, trace = TRUE, improve = 0.05)

# Memory Error, commenting out for now
memory.limit()
memory.limit(size=25000)

#rfModel_new <- randomForest(churn ~ ., data=cc_cal_train , ntree = 100, mtry = 2, importance = TRUE, proximity = TRUE)
#print(rfModel_new)

#pred_rf_new <- predict(rfModel_new, cc_cal_test)
#caret::confusionMatrix(pred_rf_new, cc_cal_test$churndep)

#Random Forest Feature Importance

# randomForest::varImpPlot(rfModel_new, sort=T, n.var = 10, main = 'Top 10 Feature Importance')

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

print(svm.model)
