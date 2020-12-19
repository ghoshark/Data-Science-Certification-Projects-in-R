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

cc <- read.csv("C:\\Data_Science\\Certifications\\AnalytixLabs\\Data Science in R\\Data Science with R - Final Projects\\Linear Regression - Credit Card Spend\\Credit Card Data.csv")
head(cc,5)

NROW(cc)
NCOL(cc)

str(cc)

summary(cc)

describe(cc)

#Distribution of primary and secondary spends

par(mfrow=c(1,2))
hist(cc$cardspent, col = "lightgreen")
hist(cc$card2spent, col = "skyblue")

#Creating a new Derived Variable "TotalCreditSpend" which is a summation of primary and secondary spend
cc$total_spend = cc$cardspent + cc$card2spent

#Distribution of total_spend
#Log Transforming dependent variable to make it a Normal Distribution

par(mfrow=c(1,2))
hist(cc$total_spend, col = "lightgreen")
hist(log(cc$total_spend),col = "skyblue")

cc$ln_Total_spend <- log(cc$total_spend)

# Drop the total_spend variable
cc <- subset(cc,select = -c(total_spend))


cc <- subset(cc,select = -c(cardspent,card2spent,ï..custid,birthmonth,carditems,card2items))
names(cc)

# Numeric variables
cc_num <- subset(cc,select = -c(region,townsize,gender,agecat,edcat,jobcat,union,
                                       empcat,retire,inccat,default,jobsat,marital,spousedcat,
                                       homeown,hometype,address,addresscat,cars,carown,cartype,
                                       carcatvalue,carbought,carbuy,commute,commutecat,commutecar,
                                       commutemotorcycle,commutecarpool,commutebus,commuterail,
                                       commutepublic,commutebike,commutewalk,commutenonmotor,
                                       telecommute,reason,polview,polparty,polcontrib,vote,card,
                                       cardtype,cardbenefit,cardfee,cardtenure,cardtenurecat,card2,
                                       card2type,card2benefit,card2fee,card2tenure,card2tenurecat,
                                       active,bfast,churn,tollfree,equip,callcard,wireless,multline,
                                       voice,pager,internet,callid,callwait,forward,confer,ebill,
                                       owntv,ownvcr,owndvd,owncd,ownpda,ownpc,ownipod,owngame,ownfax,
                                       news,response_01,response_02,response_03))

num_var <- names(cc_num)
cc_num <- as.data.frame(lapply(cc_num,as.numeric))

head(cc_num,5)
str(cc_num)

# Categorical variables
cc_cat <- subset(cc,select = c(region,townsize,gender,agecat,edcat,jobcat,union,
                                    empcat,retire,inccat,default,jobsat,marital,spousedcat,
                                    homeown,hometype,address,addresscat,cars,carown,cartype,
                                    carcatvalue,carbought,carbuy,commute,commutecat,commutecar,
                                    commutemotorcycle,commutecarpool,commutebus,commuterail,
                                    commutepublic,commutebike,commutewalk,commutenonmotor,
                                    telecommute,reason,polview,polparty,polcontrib,vote,card,
                                    cardtype,cardbenefit,cardfee,cardtenure,cardtenurecat,card2,
                                    card2type,card2benefit,card2fee,card2tenure,card2tenurecat,
                                    active,bfast,churn,tollfree,equip,callcard,wireless,multline,
                                    voice,pager,internet,callid,callwait,forward,confer,ebill,
                                    owntv,ownvcr,owndvd,owncd,ownpda,ownpc,ownipod,owngame,ownfax,
                                    news,response_01,response_02,response_03))

cat_var <- names(cc_cat)
cc_cat <- as.data.frame(lapply(cc_cat,as.factor))


str(cc_cat)
head(cc_cat)

str(cc_cat)

# There are some missing strings like #NULL! that should be replaced by "NA"
# Write out all the offending strings
# Later we will handle all such NAs together
na_strings <- c("#NULL!", "NA", "N A", "N / A", "N/A", "N/ A", "Not Available", "NOt available")
cc_num <- cc_num %>% naniar::replace_with_na_all(condition = ~.x %in% na_strings)

cc_num$lntollmon <- NULL
cc_num$lntollten <- NULL
cc_num$lnequipmon <- NULL
cc_num$lnequipten <- NULL
cc_num$lncardmon <- NULL
cc_num$lncardten <- NULL
cc_num$lnwiremon <- NULL
cc_num$lnwireten <- NULL


names(cc_num)

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
write.csv(num_stats,"C:\\Data_Science\\Certifications\\AnalytixLabs\\Data Science in R\\Data Science with R - Final Projects\\Linear Regression - Credit Card Spend\\NumericStats.csv")

cat_stats <- t(data.frame(apply(cc_cat,2,FUN = mystats_cat)))
write.csv(cat_stats,"C:\\Data_Science\\Certifications\\AnalytixLabs\\Data Science in R\\Data Science with R - Final Projects\\Linear Regression - Credit Card Spend\\CategoricalStats.csv")#View(cat_stats)

# User-Defined function to find NAs for cc dataframe
# There will be few NA values that we introduced in previous step

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

cc_num$lncreddebt[is.na(cc_num$lncreddebt)] <- 269.990398079616
cc_num$lnothdebt[is.na(cc_num$lnothdebt)] <- 285.403880776155
cc_num$commutetime[is.na(cc_num$commutetime)] <- 17.4029611844738
cc_num$longten[is.na(cc_num$longten)] <- 2211.45987592556
cc_num$lnlongten[is.na(cc_num$lnlongten)] <- 436.341004602762
cc_num$cardten[is.na(cc_num$cardten)] <- 267.27631052421

# No missing values in categorical list

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

## Find and Apply outlier treatment to all Numeric variables
cc_num <- data.frame(lapply(cc_num,HandleOutlier))


head(cc_num,5)

# List of categorical and numeric variables
num_var <- names(cc_num)
cat_var <- names(cc_cat)

cc<-cbind.data.frame(cc_num,cc_cat)
str(cc)

cormat <- cor(cc_num,use="pairwise.complete.obs")
corplot <- corrplot(cormat, method = "number", tl.cex = 0.75)
corplot

# pets_saltfish and pets_reptiles have NAs because the Standard Deviation of these columns is 0 after outlier handling
# Let's drop these 2 columns
cc_num <- subset(cc_num,select = -c(pets_saltfish, pets_reptiles))

cormat <- cor(cc_num,use="pairwise.complete.obs")
corplot <- corrplot(cormat, method = "number", tl.cex = 0.75)
corplot

# Find attributes that are highly correlated (ideally >0.75)
highlyCorrelated <- caret::findCorrelation(cormat, cutoff=0.75, verbose = TRUE, names = TRUE)
highlyCorrelated

#Carefully read the output from the findCorrelation() function call; it wants to eliminate rows 4, 5, and col 19. 
#But it wants to eliminate 4 because it’s correlated with 5 and 5 because it’s correlated with 19. 
#If we remove column 5, 4 won’t be strongly correlated with anything anymore! 
#So, it is important to watch what your code is doing, in this case it doesn’t look like 4 is necessary to remove.
# Also, total_spend correlates strongly with ln_Total_spend, so it needs to be dropped
cc_num <- subset(cc_num, select = -c(income,tenure,lnlongten,othdebt,lnlongmon,creddebt,tollten,wiremon,equipten,pets))

# Check for High Correlation now
# But lninc is still highly correlated with column 15, so need to drop it

cormat <- cor(cc_num,use="pairwise.complete.obs")
highlyCorrelated <- caret::findCorrelation(cormat, cutoff=0.75, verbose = TRUE, names = TRUE)
highlyCorrelated

# Drop row 4 (income)
cc_num <- subset(cc_num, select = -c(lninc))

# Check for High Correlation
# As expected, all correlations are <= 0.75 

cormat <- cor(cc_num,use="pairwise.complete.obs")
highlyCorrelated <- caret::findCorrelation(cormat, cutoff=0.75, verbose = TRUE, names = TRUE)
highlyCorrelated

# Plot a Correlation Matrix
corplot <- corrplot(cormat, method = "number", tl.cex = 0.75)
corplot

# List of categorical and numeric variables
num_var <- names(cc_num)
cat_var <- names(cc_cat)

cc<-cbind.data.frame(cc_num,cc_cat)
str(cc)

NCOL(cc_num)
NCOL(cc_cat)
names(cc)

names(cc_cat)


# Performing the One-Way ANOVA and selecting variables with p value < 0.05 (5%)

full.model <- aov(ln_Total_spend ~ region+townsize+gender+agecat+edcat+jobcat+union+empcat+retire+inccat+default+jobsat+marital+spousedcat+homeown+hometype+address+addresscat+cars+carown+cartype+carcatvalue+carbought+carbuy+commute+commutecat+commutecar+commutemotorcycle+commutecarpool+commutebus+commuterail+commutepublic+commutebike+commutewalk+commutenonmotor+telecommute+reason+polview+polparty+polcontrib+vote+card+cardtype+cardbenefit+cardfee+cardtenure+cardtenurecat+card2+card2type+card2benefit+card2fee+card2tenure+card2tenurecat+active+bfast+churn+tollfree+equip+callcard+wireless+multline+voice+pager+internet+callid+callwait+forward+confer+ebill+owntv+ownvcr+owndvd+owncd+ownpda+ownpc+ownipod+owngame+ownfax+news+response_01+response_02+response_03, data = cc) 
summary(full.model)

# From the full categorical list we select only those columns that are statistically significant or with p value < 0.05 (5%)

full.model <- aov(ln_Total_spend ~ region+gender+agecat+edcat+jobcat+empcat+retire+inccat+carown+reason+card+card2+internet+ownvcr+owndvd, data = cc) 
summary(full.model)

step.model <- stepAIC(full.model,direction = "both")
summary(step.model)
step.model$anova

#The Akaike information criterion (AIC) is an estimator of the relative quality of statistical models for a given set of data
#From the Stepwise regression, we get the model with least AIC.

#Step:  AIC=-6932.9
#ln_Total_spend ~ region + gender + edcat + retire + inccat + carown + reason + card + card2 + internet + ownvcr + owndvd

par(mfrow=c(2,2))
plot(step.model,col="blue")

step.model

cc_cat <- subset(cc_cat,select = c(region, gender, edcat, retire, inccat, carown, reason, card, card2, internet, ownvcr, owndvd))

full.model.lm <- lm(ln_Total_spend ~. ,data = cc_num)
summary(full.model.lm)

step.model.lm <- stepAIC(full.model.lm, direction = "both")
summary(step.model.lm)
step.model.lm$anova

par(mfrow=c(2,2))
plot(step.model.lm,col="blue")

shortlistedVars <- names(unlist(step.model.lm[[1]])) 
shortlistedVars <- shortlistedVars[!shortlistedVars %in% "(Intercept)"] # remove intercept

print(shortlistedVars)

step.model.lm

cc_num <- subset(cc_num,select = c(age,ed,employ,debtinc,lncreddebt,lnothdebt,pets_birds,carvalue))

ggplot(cc, aes(x=age, y=ln_Total_spend)) + geom_point(col="darkblue") + geom_smooth(method=lm)

ggplot(cc, aes(x=ed, y=ln_Total_spend)) + geom_point(col="red") + geom_smooth(method=lm)

ggplot(cc, aes(x=employ, y=ln_Total_spend)) + geom_point(col="magenta") + geom_smooth(method=lm)

ggplot(cc, aes(x=debtinc, y=ln_Total_spend)) + geom_point(col="orange") + geom_smooth(method=lm) 

ggplot(cc, aes(x=lncreddebt, y=ln_Total_spend)) + geom_point(col="black") + geom_smooth(method=lm)

ggplot(cc, aes(x=lnothdebt, y=ln_Total_spend)) + geom_point(col="pink") + geom_smooth(method=lm)

ggplot(cc, aes(x=pets_birds, y=ln_Total_spend)) + geom_point(col="skyblue") + geom_smooth(method=lm)

ggplot(cc, aes(x=carvalue, y=ln_Total_spend)) + geom_point(col="maroon") + geom_smooth(method=lm)

cc_final <- cbind.data.frame(cc_cat,cc_num,ln_total_spend=cc$ln_Total_spend)
write.csv(cc_final, file="C:\\Data_Science\\Certifications\\AnalytixLabs\\Data Science in R\\Data Science with R - Final Projects\\Linear Regression - Credit Card Spend\\credit_card_final.csv")


NCOL(cc_final)
head(cc_final,5)

set.seed(222)
ind <- sample(2,nrow(cc_final),replace = TRUE,prob = c(0.75,0.25))
cc_train <- cc_final[ind==1,]
cc_test <- cc_final[ind==2,]

nrow(cc_train)
nrow(cc_test)

ncol(cc_train)
ncol(cc_test)


full.model.lm2 <- lm(ln_total_spend ~ .,data = cc_train)
summary(full.model.lm2)


step.model.lm2 <- stepAIC(full.model.lm2,direction="both")
summary(step.model.lm2)
step.model.lm2$anova

par(mfrow=c(2,2))
plot(step.model.lm2,col="darkblue")

step.model.lm2

#From the Stepwise AIC, we see that this model has the least value of AIC.

#Step:  AIC=-5180.54
#ln_total_spend ~ gender + edcat + retire + inccat + reason + card + card2 + internet + ownvcr + ed + employ + lnothdebt

nrow(cc_train)

cc_train$cooksd <- cooks.distance(step.model.lm2)

# Plot the Cook's Distance using the traditional 4/n criteria
sample_size <- nrow(cc_train)
plot(cc_train$cooksd, pch="*", cex=1.5, main="Influential Obs by Cooks Distance")  # plot Cook's distance
abline(h = 4/sample_size, col="red", lwd=2)  # add a cutoff line

cc_train1 <- subset(cc_train,cooksd<(4/3768))

NROW(cc_train1)

final_model1 <- lm(ln_total_spend ~ gender + edcat + retire + inccat + reason + card + card2 + internet + ownvcr + ed + employ + lnothdebt,data = cc_train1)
summary(final_model1)


cc_train1$cooksd1 <- cooks.distance(final_model1)

# Plot the Cook's Distance using the traditional 4/n criteria
sample_size <- nrow(cc_train1)
plot(cc_train1$cooksd1, pch="*", cex=1.5, main="Influential Obs by Cooks Distance")  # plot Cook's distance
abline(h = 4/sample_size, col="red", lwd=2)  # add a cutoff line

cc_train2 <- subset(cc_train1,cooksd1<(4/3607))

nrow(cc_train2)

final_model2 <- lm(ln_total_spend ~ gender+edcat+retire+inccat+reason+card+card2+internet+ownvcr+ed + employ + lnothdebt,data = cc_train2)
summary(final_model2)

# No more outliers with Cook's Distance greater than 4/Sample Size

subset(cc_train2,cooksd1>(4/3520))

plot(cc_train2$cooksd1, pch="*", cex=1.5, main="Influential Obs by Cooks Distance")  # plot Cook's distance
abline(h = 4/3520, col="red", lwd=2)  # add a cutoff line

plot(final_model2, pch=1, col="cyan", lty=1, lwd=1, which=1)

#In the current case, the red trend line is almost at zero except slightly towards the left side, due to few outliers presence.
#Although few of the points( “167”, “1200”, “3394”) ar far from 0, none of the values are outside the range of -2 and +2.

plot(final_model2, pch=1, col="cyan", lty=1, lwd=1, which=2)

#In the current case, points form a line in the middle of the graph, but tend to deviate from the diagonal line in both the upper and lower extremities. 
#Plot behaviour like this, means that the tails are lighter (have smaller values) than what would be expected under the standard modeling assumptions (of the Normal distribution). 
#Again the observations that can be noticed in the tails are “167”, “1200”, “3394”.

plot(final_model2, pch=1, col="cyan", lty=1, lwd=1, which=3)

#In the current case, the red trend line is almost horizontal.

plot(final_model2, pch=1, col="cyan", lty=1, lwd=1, which=5)

#In the current case, the red trend line is almost horizontal.

# rownames(cc_train2) gives the row number
# Exclude the 3 row numbers: 167,1200 and 3394
# The new recordcount should be 3529-3 = 3517
cc_train3 <- cc_train2[!(rownames(cc_train2) %in% c(167,1200,3394)),]

nrow(cc_train3)


final_model3 <- lm(ln_total_spend ~ gender+edcat+retire+inccat+reason+card+card2+internet+ownvcr+ed+employ+lnothdebt,data = cc_train3)
summary(final_model3)
# Removing outliers, model fits only slightly better than the previous: F statistic and R-squared values of final_model3 are higher than final_model2 ones. 

par(mfrow = c(2,2)) # display a unique layout for all graphs
plot(final_model3,col="darkblue")

# Plots still shows some tails, but they seem to be more acceptable.

AIC(final_model2)
AIC(final_model3)

BIC(final_model2)
BIC(final_model3)

# AIC and BIC values of final_model3 are lower than final_model2 ones. 
# Generally, small values correspond to models with a low test error, so it's a confirmation that final_model3 fits better than final_model2.

car::vif(final_model3)

# Most variables have VIF of about 1 except edcat(vif=16) and ed(vif=15.89)
# So we will remove these variables from our model

final_model3 <- lm(ln_total_spend ~ gender+retire+inccat+reason+card+card2+internet+ownvcr+employ+lnothdebt,data = cc_train3)
summary(final_model3)
# Removing outliers, model fits only slightly better than the previous: F statistic and R-squared values of final_model3 are higher than final_model2 ones. 

# Check again for multicollinearity
# No more variables showing high VIF value
car::vif(final_model3)

# Training
pred_train <- predict(final_model3,newdata = cc_train)
cc_train <- cbind(cc_train,pred_spend=pred_train)

# Test
pred_test <- predict(final_model3,newdata = cc_test)
cc_test <- cbind(cc_test,pred_spend=pred_test)

actuals_preds <- data.frame(cbind(actual=exp(cc_test$ln_total_spend), predicted=exp(cc_test$pred_spend)) )

#Export the actual and predicted values to a CSV file
write.csv(actuals_preds,"C:\\Data_Science\\Certifications\\AnalytixLabs\\Data Science in R\\Data Science with R - Final Projects\\Linear Regression - Credit Card Spend\\actuals_predictions.csv")

correlation_accuracy <- cor(actuals_preds)
corrplot(correlation_accuracy, method = "number", tl.cex = 0.75)

min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
cat("Min Max Accuracy%: ", round(min_max_accuracy*100,3))
cat("\n")
mape <- mean(abs((actuals_preds$predicted - actuals_preds$actual))/actuals_preds$actual)  
cat("MAPE%: ", round(mape*100,3)) 

saveRDS(final_model3, "C:\\Data_Science\\Certifications\\AnalytixLabs\\Data Science in R\\Data Science with R - Final Projects\\Linear Regression - Credit Card Spend\\LinearRegression.rda")

# readRDS("C:\\Data_Science\\Certifications\\AnalytixLabs\\Data Science in R\\Data Science with R - Final Projects\\Linear Regression - Credit Card Spend\\LinearRegression.rda")

dec_loc <- quantile(cc_train$pred_spend,probs = seq(0.1,0.9,by=0.1))
cc_train$decile <- findInterval(cc_train$pred_spend,c(-Inf,dec_loc,+Inf))


cc_train_decile <- sqldf("select decile,count(decile) as Count,
                   avg(ln_total_spend) as Actual_spend,
                   avg(pred_spend) as Predicted_spend
                   from cc_train
                   group by decile
                   order by decile desc")

write.csv(cc_train_decile,"C:\\Data_Science\\Certifications\\AnalytixLabs\\Data Science in R\\Data Science with R - Final Projects\\Linear Regression - Credit Card Spend\\train_decile.csv")


dec_loc <- quantile(cc_test$pred_spend,probs = seq(0.1,0.9,by=0.1))
cc_test$decile <- findInterval(cc_test$pred_spend,c(-Inf,dec_loc,+Inf))


cc_test_decile <- sqldf("select decile,count(decile) as Count,
                   avg(ln_total_spend) as Actual_spend,
                   avg(pred_spend) as Predicted_spend
                   from cc_test
                   group by decile
                   order by decile desc")

write.csv(cc_test_decile,"C:\\Data_Science\\Certifications\\AnalytixLabs\\Data Science in R\\Data Science with R - Final Projects\\Linear Regression - Credit Card Spend\\test_decile.csv")


summary(final_model3)

imp <- as.data.frame(varImp(final_model3))
imp <- data.frame(columns = rownames(imp), overall = imp$Overall)
imp <- imp[order(imp$overall,decreasing = T),]
imp

write.csv(imp,"C:\\Data_Science\\Certifications\\AnalytixLabs\\Data Science in R\\Data Science with R - Final Projects\\Linear Regression - Credit Card Spend\\var_imp.csv")

# Variable Importance: Draw a Bar Chart

ggplot(imp, aes(x=columns, y=overall))  + 
  geom_bar(stat="identity", width=.5, fill="tomato3") + 
  labs(title="Ordered Bar Chart", 
       subtitle="Variables vs Importance", 
       caption="source: varimp") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))
