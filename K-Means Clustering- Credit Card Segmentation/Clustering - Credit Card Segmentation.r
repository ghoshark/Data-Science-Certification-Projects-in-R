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
# install.packages("dummies")
# install.packages("ggfortify")
# install.packages("rgl")
# install.packages("factoextra")
# install.packages("FactoMineR")
# install.packages("cluster")
# install.packages("NbClust")
# install.packages("ggiraphExtra")
# install.packages("GGally")
# install.packages("ggExtra")
# install.packages("vegan")
# install.packages('psych')
# install.packages('GPArotation')
# install.packages("tables")

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
library(dummies)
library(ggfortify)
library(rgl)
library(factoextra)
library(RColorBrewer)
library(FactoMineR)
library(cluster)
library(NbClust)
library(ggiraphExtra)
library(GGally)
library(ggExtra)
library(vegan)
library(psych)
library(GPArotation)
library(tables)

cc <- read.csv("C:\\Data_Science\\Certifications\\AnalytixLabs\\Data Science in R\\Data Science with R - Final Projects\\K-Means Clustering- Credit Card Segmentation\\CC GENERAL.csv")
head(cc,5)

NROW(cc)
NCOL(cc)

str(cc)

summary(cc)

describe(cc)

# Drop the CUSTID column since it wont be required
cc <- subset(cc,select=-c(CUST_ID))

names(cc)

# Statistics for Numeric variables
# All variables are numeric in this dataset

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

# Save the statistics in a file
stats <- data.frame(apply(cc,2,FUN = mystats_num))
stats
write.csv(stats,"C:\\Data_Science\\Certifications\\AnalytixLabs\\Data Science in R\\Data Science with R - Final Projects\\K-Means Clustering- Credit Card Segmentation\\NumericStats.csv")

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

# MINIMUM_PAYMENTS has 313 or 3.5% missing values. CREDIT_LIMIT has 1 or 0.01% missing values.
findMissingValues(cc)

#Mean value treatment for Numerical variables
#Store the columns with NA values in list_na
#Find the mean values of such columns

list_na <- colnames(cc)[ apply(cc, 2, anyNA) ]
list_na

average_missing <- apply(cc[,colnames(cc) %in% list_na], 2, mean, na.rm =  TRUE)
average_missing

#Impute Missing data with the Mean or Median

cc$CREDIT_LIMIT[is.na(cc$CREDIT_LIMIT)] <- 4494.44945036462
cc$MINIMUM_PAYMENTS[is.na(cc$MINIMUM_PAYMENTS)] <- 864.206542305083

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
cc <- data.frame(lapply(cc,HandleOutlier))

cc$MONTHLY_AVG_PURCHASE <- (cc$PURCHASES / cc$TENURE)

cc$CASH_ADVANCE_AMT <- (cc$CASH_ADVANCE / cc$TENURE)

head(cc,2)

# Histograms for CASH_ADVANCE_AMT and MONTHLY_AVG_PURCHASE

p <- ggplot(cc, aes(x=CASH_ADVANCE_AMT)) + geom_histogram(color="black", fill="darkblue",binwidth=5) +
     geom_vline(aes(xintercept=mean(CASH_ADVANCE_AMT)),color="red", linetype="dashed", size=1)
p
q <- ggplot(cc, aes(x=MONTHLY_AVG_PURCHASE)) + geom_histogram(color="black", fill="darkblue",binwidth=5) + 
     geom_vline(aes(xintercept=mean(MONTHLY_AVG_PURCHASE)),color="red", linetype="dashed", size=1)
q

cc <- cc %>% mutate(PURCHASE_TYPE = case_when((ONEOFF_PURCHASES==0)&(INSTALLMENTS_PURCHASES==0) ~ "none",
                                           (ONEOFF_PURCHASES>0)&(INSTALLMENTS_PURCHASES>0) ~ "oneoff_installment",
                                           (ONEOFF_PURCHASES==0)&(INSTALLMENTS_PURCHASES>0) ~ "installment",
                                           (ONEOFF_PURCHASES>0)&(INSTALLMENTS_PURCHASES==0) ~ "oneoff"))
head(cc,5)

df <- sqldf('select PURCHASE_TYPE, count(*) as COUNT from cc group by PURCHASE_TYPE')
df

# PURCHASE_TYPE count
p <-ggplot(df, aes(PURCHASE_TYPE, COUNT)) + geom_bar(stat = "identity", aes(fill = PURCHASE_TYPE))
p

#Both the metrics are already there in the data.
head(cc$MONTHLY_AVG_PURCHASE)
head(cc$CASH_ADVANCE_TRX)

cc$LIMIT_USAGE <- (cc$BALANCE / cc$CREDIT_LIMIT)

head(cbind(cc$BALANCE,cc$CREDIT_LIMIT,cc$LIMIT_USAGE))

# Histogram for LIMIT_USAGE
p <- ggplot(cc, aes(x=LIMIT_USAGE)) + geom_histogram(color="black", fill="darkred",binwidth=0.25)
p

cc$PAY_MINPAY <- (cc$PAYMENTS / cc$MINIMUM_PAYMENTS)

head(cbind(cc$PAY_MINPAY,cc$PAYMENTS,cc$MINIMUM_PAYMENTS))

# Histogram for PAY_MINPAY
p <- ggplot(cc, aes(x=PAY_MINPAY)) + geom_histogram(color="black", fill="darkred",binwidth=10)
p

# Create DUMMY variables for categorical variable PURCHASE_TYPE                   
cc <- dummies::dummy.data.frame(cc, sep = ".")
str(cc)

cc %>% 
  gather(Attributes, value, 1:18) %>% 
  ggplot(aes(x=value)) +
  geom_histogram(fill = "lightblue2", color = "black") + 
  facet_wrap(~Attributes, scales = "free_x") +
  labs(x = "Value", y = "Frequency")

# Correlation Matrix shows that many features are co-related so applying dimensionality reduction will help negating multi-colinearity in data

corrm <- cor(cc,use="pairwise.complete.obs")
corrm

eigen(corrm)$values 

scree(corrm, factors=T, pc=T, main="scree plot", hline=NULL, add=FALSE)   

# Coming up with other useful values such as cumulative eigenvalue, percentage variance and cumulative percentage variance.

eigen_values <- mutate(data.frame(eigen(corrm)$values)
                       ,cum_sum_eigen=cumsum(eigen.corrm..values)
                       , pct_var=eigen.corrm..values/sum(eigen.corrm..values)
                       , cum_pct_var=cum_sum_eigen/sum(eigen.corrm..values))
eigen_values

write.csv(eigen_values, "C:\\Data_Science\\Certifications\\AnalytixLabs\\Data Science in R\\Data Science with R - Final Projects\\K-Means Clustering- Credit Card Segmentation\\EigenValues.csv")  ### EXPORTING EIGEN VALUE SUMMARY

# Another way of calculating the number of factors is using `Psych` package’s `fa.parallel` function to execute parallel analysis. 
# Here we specify the data frame and factor method (`minres` in our case). 
# Run the following to find acceptable number of factors and generate the `scree plot`:

parallel <- fa.parallel(cc, fm = 'minres', fa = 'fa')

FA_7 <- fa(corrm,nfactors = 7,rotate = "oblimin",fm="minres")
print(FA_7)
#Now we need to consider the loadings more than 0.3 and not loading on more than one factor. Note that negative values are acceptable here. 
#So let’s first establish the cut off to improve visibility:
print(FA_7$loadings,cutoff = 0.3)

fa.diagram(FA_7)

FA_8 <- fa(corrm,nfactors = 8,rotate = "oblimin",fm="minres")
print(FA_8)
#Now we need to consider the loadings more than 0.3 and not loading on more than one factor. Note that negative values are acceptable here. 
#So let’s first establish the cut off to improve visibility:
print(FA_8$loadings,cutoff = 0.3)

fa.diagram(FA_8)

# Sort the loadings
# List out the objects
# Save the loadings file

FA_SORT<-fa.sort(FA_8) 
ls(FA_SORT) 
load1 <- FA_SORT$loadings
write.csv(load1, "C:\\Data_Science\\Certifications\\AnalytixLabs\\Data Science in R\\Data Science with R - Final Projects\\K-Means Clustering- Credit Card Segmentation\\Loadings.csv") 

# Decided not to include the KPI's into my solution

var <- c("PURCHASES",
"CASH_ADVANCE",
"PURCHASES_FREQUENCY",
"LIMIT_USAGE",
"BALANCE",
"PAY_MINPAY",
"ONEOFF_PURCHASES",
"CASH_ADVANCE_AMT",
"PURCHASES_INSTALLMENTS_FREQUENCY",
"CREDIT_LIMIT"
)

cc_fa_final <- cc[var]

# Standardizing the final data
cc_fa_final <- scale(cc_fa_final)

write.csv(cc_fa_final,"C:\\Data_Science\\Certifications\\AnalytixLabs\\Data Science in R\\Data Science with R - Final Projects\\K-Means Clustering- Credit Card Segmentation\\FinalVariables.csv")

#Building clusters using k-means clustering 

cluster_7 <- kmeans(cc_fa_final,7)
cluster_8 <- kmeans(cc_fa_final,8)

cc_new <-cbind(cc,km_clust_7=cluster_7$cluster ,km_clust_8=cluster_8$cluster)

head(cc_new,3)

cluster::clusplot(cc_fa_final, cluster_7$cluster, color = TRUE, lines =6, labels = 2)

cluster::clusplot(cc_fa_final, cluster_8$cluster, color = TRUE, lines =6, labels = 2)

# Centroid Plot against 1st 2 discriminant functions
fpc::plotcluster(cc_fa_final, cluster_7$cluster)
fpc::plotcluster(cc_fa_final, cluster_8$cluster)

#Converting into factors

cc_new$km_clust_7=factor(cc_new$km_clust_7)
cc_new$km_clust_8=factor(cc_new$km_clust_8)
cc_new <- data.frame(cc_new)

profile <- tables::tabular(1+PURCHASES+CASH_ADVANCE+PURCHASES_FREQUENCY+LIMIT_USAGE+BALANCE+PAY_MINPAY+ONEOFF_PURCHASES+CASH_ADVANCE_AMT+PURCHASES_INSTALLMENTS_FREQUENCY+CREDIT_LIMIT ~ mean+(mean*km_clust_7)+(mean*km_clust_8),data=cc_new)

profile1 <- as.matrix(profile)
profile1 <- data.frame(profile1)
head(profile1)

write.csv(profile1,"C:\\Data_Science\\Certifications\\AnalytixLabs\\Data Science in R\\Data Science with R - Final Projects\\K-Means Clustering- Credit Card Segmentation\\profile1.csv")

profile<-tabular(1~length+(length*km_clust_7)+(length*km_clust_8),data=cc_new)
profile2<-as.matrix(profile)
profile2<-data.frame(profile2)
head(profile2)

write.csv(profile2,"C:\\Data_Science\\Certifications\\AnalytixLabs\\Data Science in R\\Data Science with R - Final Projects\\K-Means Clustering- Credit Card Segmentation\\profile2.csv")

# Naively apply principal components analysis to raw data and plot
pc <- princomp(cc)
plot(pc)
ggplot2::autoplot(pc,col = "darkblue")

plot(pc, type='l')

# First component dominates greatly. What are the loadings?
# pc1 is CREDIT_LIMIT and PAYMENTS
# pc2 is CREDIT_LIMIT, PAYMENTS and ONEOFF_PURCHASES                         
summary(pc) # pc1 has > 62% variance, pc1+pc2 has > 77% variance
loadings(pc) 

# verify by plotting variance of columns
# var is for variance function
mar <- par()$mar
par(mar=mar+c(0,5,0,0))
par(mfrow=c(1,2))
barplot(sapply(cc, var), horiz=T, las=1, cex.names=0.8,col = "darkred")
barplot(sapply(cc, var), horiz=T, las=1, cex.names=0.8, log='x',col = "darkgreen")
par(mar=mar)

# Scale
cc2 <- data.frame(scale(cc))
# Verify variance is uniform
plot(sapply(cc2,var))

# verify the variances are now equal by plotting variance of columns
# var is for variance function (regular scaling on the left, logarithmic on the right)

mar <- par()$mar
par(mar=mar+c(0,5,0,0))
par(mfrow=c(1,2))
barplot(sapply(cc2, var), horiz=T, las=1, cex.names=0.8,col = "darkred")
barplot(sapply(cc2, var), horiz=T, las=1, cex.names=0.8, log='x',col = "darkgreen")
par(mar=mar)

# Now apply principal components analysis to scaled data and plot
pc <- princomp(cc2,score=TRUE)
plot(pc)
plot(pc, type='l')
ggplot2::autoplot(pc,col = "darkblue")

summary(pc) # pc1 has > 28% variance, pc1+pc2..pc7 has > 80% variance
loadings(pc) 

res.pca <- PCA(cc2,  graph = FALSE)
# Visualize eigenvalues/variances
fviz_screeplot(res.pca, addlabels = TRUE, ylim = c(0, 50))

# Extract the results for variables
var <- get_pca_var(res.pca)
# Contributions of variables to PC1
fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)
# Contributions of variables to PC2
fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)
# Contributions of variables to PC3
fviz_contrib(res.pca, choice = "var", axes = 3, top = 10)
# Contributions of variables to PC4
fviz_contrib(res.pca, choice = "var", axes = 4, top = 10)
# Contributions of variables to PC5
fviz_contrib(res.pca, choice = "var", axes = 5, top = 10)

# Control variable colors using their contributions to the principle axis
fviz_pca_var(res.pca, col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping
             ) + theme_minimal() + ggtitle("Variables - PCA")

# Get principal component vectors using prcomp instead of princomp
pc <- prcomp(cc2)
# First for 5 principal components
comp <- data.frame(pc$x[,1:5])
# Plot
plot(comp, pch=16, col=rgb(0,0.5,0.5,0.5))

#Check the priciple components
plot(pc)

# Multi 3D plot using RGL package

# plot3d(comp$PC1, comp$PC2, comp$PC3)
# plot3d(comp$PC1, comp$PC3, comp$PC4)

#memory.limit()
memory.limit(size=56000)

# The default distance computed is the Euclidean.
# But get_dist() also supports distances described in equations 2-4 above plus others.

#distance <- factoextra::get_dist(cc)
#factoextra::fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

set.seed(31)
# function to compute total within-cluster sum of squares
fviz_nbclust(comp, kmeans, method = "wss", k.max = 12) + theme_minimal() + ggtitle("The Elbow Method")

# The bend indicates that additional clusters beyond 6 have little value.

# This gives a warning  message: did not converge in 10 iterations"Warning message:
# Commenting this out for now
#gap_stat <- cluster::clusGap(comp, FUN = kmeans, nstart = 30, K.max = 10, B = 50)
#fviz_gap_stat(gap_stat) + theme_minimal() + ggtitle("fviz_gap_stat: Gap Statistic")

# The silhouette plot shows the that the silhouette coefficient was highest when k = 5
# This method suggests optimal number of clusters as 5
fviz_nbclust(comp, kmeans, method = "silhouette", k.max = 12) + theme_minimal() + ggtitle("The Silhouette Plot")

set.seed(13)

cal_fit2 <- vegan::cascadeKM(comp, 1, 10, iter = 1000)
plot(cal_fit2, sortg = TRUE, grpmts.plot = TRUE)

calinski.best2 <- as.numeric(which.max(cal_fit2$results[2,]))
cat("Calinski criterion optimal number of clusters:", calinski.best2, "\n")

# Compute k-means clustering with k = 6
set.seed(123)
final6 <- kmeans(comp, 6, nstart = 25)
fviz_cluster(final6, data = comp) + theme_minimal() + ggtitle("k = 6")
saveRDS(final6, "C:\\Data_Science\\Certifications\\AnalytixLabs\\Data Science in R\\Data Science with R - Final Projects\\K-Means Clustering- Credit Card Segmentation\\K-means_cluster6.rda")

# Compute k-means clustering with k = 7
set.seed(123)
final7 <- kmeans(comp, 7, nstart = 25)
fviz_cluster(final7, data = comp) + theme_minimal() + ggtitle("k = 7")
saveRDS(final7, "C:\\Data_Science\\Certifications\\AnalytixLabs\\Data Science in R\\Data Science with R - Final Projects\\K-Means Clustering- Credit Card Segmentation\\K-means_cluster7.rda")

cc2 %>%
  mutate(Cluster = final6$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean") 

cc_df <- as.data.frame(cc2) %>% rownames_to_column()

cluster_pos <- as.data.frame(final6$cluster) %>% rownames_to_column()

colnames(cluster_pos) <- c("rowname", "cluster")

cc_final <- inner_join(cluster_pos, cc_df, by="rowname")

ggiraphExtra::ggRadar(cc_final[-1], aes(group = cluster), rescale = FALSE, legend.position = "none", size = 1, interactive = FALSE, use.label = TRUE) + facet_wrap(~cluster) + scale_y_discrete(breaks = NULL) + # don't show ticks
theme(axis.text.x = element_text(size = 7)) + scale_fill_manual(values = rep("#1c6193", nrow(cc_final))) +
scale_color_manual(values = rep("#1c6193", nrow(cc_final))) +
ggtitle("Credit Card Attributes")

cc_df <- as.data.frame(cc2)
cc_df$cluster <- final6$cluster
cc_df$cluster <- as.character(cc_df$cluster)

GGally::ggpairs(cc_df, 1:6, mapping = ggplot2::aes(color = cluster, alpha = 0.5), 
        diag = list(continuous = wrap("densityDiag")), 
        lower=list(continuous = wrap("points", alpha=0.9)))

# Plot specific graphs from previous matrix with scatterplot

g <- ggplot(cc_df, aes(x = BALANCE_FREQUENCY, y = ONEOFF_PURCHASES, color = cluster)) +
        geom_point() +
        theme(legend.position = "bottom")

ggExtra::ggMarginal(g, type = "histogram", bins = 20, color = "grey", fill = "blue")

# Plot specific graphs from previous matrix with scatterplot

b <- ggplot(cc_df, aes(x = INSTALLMENTS_PURCHASES, y = PURCHASES_INSTALLMENTS_FREQUENCY, color = cluster)) +
        geom_point() +
        theme(legend.position = "bottom")
ggExtra::ggMarginal(b, type = "histogram", bins = 20, color = "grey", fill = "blue")

# Plot specific features against each of the 4 clusters
ggplot(cc_df, aes(x = cluster, y = BALANCE_FREQUENCY)) + 
        geom_boxplot(aes(fill = cluster))

ggplot(cc_df, aes(x = cluster, y = INSTALLMENTS_PURCHASES)) + 
        geom_boxplot(aes(fill = cluster))

ggplot(cc_df, aes(x = cluster, y = ONEOFF_PURCHASES_FREQUENCY)) + 
        geom_boxplot(aes(fill = cluster))

ggplot(cc_df, aes(x = cluster, y = PURCHASES_INSTALLMENTS_FREQUENCY)) + 
        geom_boxplot(aes(fill = cluster))

# Parallel coordinate plots allow us to put each feature on seperate column and lines connecting each column

ggparcoord(data = cc_df, columns = 1:6, groupColumn = 6, alphaLines = 0.4, title = "Parallel Coordinate Plot for the Credit Card Spend", scale = "globalminmax", showPoints = TRUE) + theme(legend.position = "bottom")
