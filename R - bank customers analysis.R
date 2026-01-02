#Reference about the dataset used in this analysis:
#It was given to me by my statistics professor

setwd("C:/Users/tomma/OneDrive/Desktop/STATISTICS FOR MANAGEMENT")

Bank_customers_data <- read.csv("C:/Users/tomma/OneDrive/Desktop/STATISTICS FOR MANAGEMENT/Bank_customers_data.csv", sep=";")
View(Bank_customers_data)

#ABSOLUTE FREQUENCY AND RELATIVE FREQUENCY:

ages <- c(Bank_customers_data$Age)
abs_freq_ages <- table(ages)
abs_freq_ages 
rel_freq_ages <- prop.table(abs_freq_ages)
rel_freq_ages

experience <- c(Bank_customers_data$Experience)
abs_freq_exp <- table(experience)
abs_freq_exp
rel_freq_exp <- prop.table(abs_freq_exp)
rel_freq_exp

income <- c(Bank_customers_data$Income)
abs_freq_inc <- table(income)
abs_freq_inc
rel_freq_inc <- prop.table(abs_freq_inc)
rel_freq_inc

family <- c(Bank_customers_data$Family)
abs_freq_fam <- table(family)
abs_freq_fam
rel_freq_fam <- prop.table(abs_freq_fam)
rel_freq_fam

ccavg <- c(Bank_customers_data$CCAvg)
abs_freq_ccavg <- table(ccavg)
abs_freq_ccavg
rel_freq_ccavg <- prop.table(abs_freq_ccavg)
rel_freq_ccavg

education <- c(Bank_customers_data$Education)
abs_freq_edu <- table(education)
abs_freq_edu
rel_freq_edu <- prop.table(abs_freq_edu)
rel_freq_edu

mortgage <- c(Bank_customers_data$Mortgage)  
abs_freq_mort <- table(mortgage)
abs_freq_mort
rel_freq_mort <- prop.table(abs_freq_mort)
rel_freq_mort

loan <- c(Bank_customers_data$Personal.Loan)
abs_freq_loan <- table(loan)
abs_freq_loan
rel_freq_loan <- prop.table(abs_freq_loan)
rel_freq_loan

#MEAN AND MEDIAN:

mean_ages <- mean(ages)
mean_ages
median_ages <- median(ages)
median_ages

mean_exp <- mean(experience)
mean_exp
median_exp <- median(experience)
median_exp

mean_inc <- mean(income)
mean_inc
median_inc <- median(income)
median_inc

mean_fam <- mean(family)
mean_fam
median_fam <- median(family)
median_fam

mean_ccavg <- mean(ccavg)
mean_ccavg
median_ccavg <- median(ccavg)
median_ccavg

mean_edu <- mean(education)
mean_edu
median_edu <- median(education)
median_edu

mean_mort <- mean(mortgage)      
mean_mort
median_mort <- median(mortgage)
median_mort

mean_loan <- mean(loan)
mean_loan
median_loan <- median(loan)
median_loan

#MODE:

#In R, the mode() function does not compute the statistical mode (i.e., the most frequent value 
#in a vector). Instead, the mode() function returns the storage mode of an object 
#(e.g., "numeric," "character," etc.). To compute the mode (most frequent value) in a statistical 
#sense, we need to use a custom function or other methods, as R doesn't have a built-in function 
#for this.

#Custom function:
find_mode <- function(x) {
  uniq_values <- unique(x)
  uniq_values[which.max(tabulate(match(x, uniq_values)))]
}

#Computing the modes of the variables:

mode_ages <- find_mode(ages)
print(mode_ages)

mode_exp <- find_mode(experience)
print(mode_exp)

mode_inc <- find_mode(income)
print(mode_inc)

mode_fam <- find_mode(family)
print(mode_fam)

mode_ccavg <- find_mode(ccavg)
print(mode_ccavg)

mode_edu <- find_mode(education)
print(mode_edu)

mode_mort <- find_mode(mortgage)   
print(mode_mort)

mode_loan <- find_mode(loan)
print(mode_loan)

#QUARTILES:

Q1_ages <- quantile(ages, 0.25)  
Q3_ages <- quantile(ages, 0.75)
Q1_ages   
Q3_ages

Q1_exp <- quantile(experience, 0.25)  
Q3_exp <- quantile(experience, 0.75)
Q1_exp   
Q3_exp

Q1_inc <- quantile(income, 0.25)  
Q3_inc <- quantile(income, 0.75)
Q1_inc   
Q3_inc

Q1_fam <- quantile(family, 0.25)  
Q3_fam <- quantile(family, 0.75)
Q1_fam   
Q3_fam

Q1_ccavg <- quantile(ccavg, 0.25)  
Q3_ccavg <- quantile(ccavg, 0.75)
Q1_ccavg  
Q3_ccavg

Q1_edu <- quantile(education, 0.25)  
Q3_edu <- quantile(education, 0.75)
Q1_edu   
Q3_edu

Q1_mort <- quantile(mortgage, 0.25)  
Q3_mort <- quantile(mortgage, 0.75)
Q1_mort   
Q3_mort

Q1_loan <- quantile(loan, 0.25)  
Q3_loan <- quantile(loan, 0.75)
Q1_loan   
Q3_loan

#BOXPLOTS:

boxplot(ages, 
        main = "Box Plot of Ages",  
        ylab = "Values",            
        col = "lightblue",                  
        border = "darkblue",                
        notch = TRUE,                       
        outline = TRUE) 

boxplot(experience, 
        main = "Box Plot of Experience",  
        ylab = "Values",            
        col = "lightblue",                  
        border = "darkblue",                
        notch = TRUE,                       
        outline = TRUE) 

boxplot(income, 
        main = "Box Plot of Income",  
        ylab = "Values",            
        col = "lightblue",                  
        border = "darkblue",                
        notch = TRUE,                       
        outline = TRUE) 

boxplot(family, 
        main = "Box Plot of Family",  
        ylab = "Values",            
        col = "lightblue",                  
        border = "darkblue",                
        notch = TRUE,                       
        outline = TRUE) 

boxplot(ccavg, 
        main = "Box Plot of CCavg",  
        ylab = "Values",            
        col = "lightblue",                  
        border = "darkblue",                
        notch = TRUE,                       
        outline = TRUE) 

boxplot(education, 
        main = "Box Plot of Education",  
        ylab = "Values",            
        col = "lightblue",                  
        border = "darkblue",                
        notch = TRUE,                       
        outline = TRUE) 

boxplot(mortgage, 
        main = "Box Plot of Mortgage",  
        ylab = "Values",            
        col = "lightblue",                  
        border = "darkblue",                
        notch = TRUE,                       
        outline = TRUE,
        ) 

boxplot(loan, 
        main = "Box Plot of Loan",  
        ylab = "Values",            
        col = "lightblue",                  
        border = "darkblue",                
        notch = TRUE,                       
        outline = TRUE) 

#VARIANCE AND STANDARD DEVIATION:

var_ages <- var(ages)
var_ages
std_ages <- sd(ages)
std_ages

var_exp <- var(experience)
var_exp
std_exp <- sd(experience)
std_exp

var_inc <- var(income)
var_inc
std_inc <- sd(income)
std_inc

var_fam <- var(family)
var_fam
std_fam <- sd(family)
std_fam

var_ccavg <- var(ccavg)
var_ccavg
std_ccavg <- sd(ccavg)
std_ccavg

var_edu <- var(education)
var_edu
std_edu <- sd(education)
std_edu

var_mort <- var(mortgage)
var_mort
std_mort <- sd(mortgage)
std_mort

var_loan <- var(loan)
var_loan
std_loan <- sd(loan)
std_loan

#GINI IMPURITY INDEX:

class_counts_ages <- table(ages)
class_proportions_ages <- class_counts_ages / length(ages)
gini_impurity_ages <- 1 - sum(class_proportions_ages^2)
gini_impurity_ages

class_counts_exp <- table(experience)
class_proportions_exp <- class_counts_exp / length(experience)
gini_impurity_exp <- 1 - sum(class_proportions_exp^2)
gini_impurity_exp

class_counts_inc <- table(income)
class_proportions_inc <- class_counts_inc / length(income)
gini_impurity_inc <- 1 - sum(class_proportions_inc^2)
gini_impurity_inc

class_counts_fam <- table(family)
class_proportions_fam <- class_counts_fam / length(family)
gini_impurity_fam <- 1 - sum(class_proportions_fam^2)
gini_impurity_fam

class_counts_ccavg <- table(ccavg)
class_proportions_ccavg <- class_counts_ccavg / length(ccavg)
gini_impurity_ccavg <- 1 - sum(class_proportions_ccavg^2)
gini_impurity_ccavg

class_counts_edu <- table(education)
class_proportions_edu <- class_counts_edu / length(education)
gini_impurity_edu <- 1 - sum(class_proportions_edu^2)
gini_impurity_edu

class_counts_mort <- table(mortgage)
class_proportions_mort <- class_counts_mort / length(mortgage)
gini_impurity_mort <- 1 - sum(class_proportions_mort^2)
gini_impurity_mort

class_counts_loan <- table(loan)
class_proportions_loan <- class_counts_loan / length(loan)
gini_impurity_loan <- 1 - sum(class_proportions_loan^2)
gini_impurity_loan

#PEARSON'S CHI-SQUARE INDEX:
#A larger 𝜒2 value generally indicates a stronger association between the variables.

contingency_table_ages_inc <- table(ages, income)
contingency_table_ages_inc
chi_square_ages_inc <- chisq.test(contingency_table_ages_inc)
chi_square_ages_inc

contingency_table_exp_inc <- table(experience, income)
contingency_table_exp_inc
chi_square_exp_inc <- chisq.test(contingency_table_exp_inc)
chi_square_exp_inc

contingency_table_edu_inc <- table(education, income)
contingency_table_edu_inc
chi_square_edu_inc <- chisq.test(contingency_table_edu_inc)
chi_square_edu_inc

contingency_table_fam_ccavg <- table(family, ccavg)
contingency_table_fam_ccavg
chi_square_fam_ccavg <- chisq.test(contingency_table_fam_ccavg)
chi_square_fam_ccavg

contingency_table_fam_loan <- table(family, loan)
contingency_table_fam_loan
chi_square_fam_loan <- chisq.test(contingency_table_fam_loan)
chi_square_fam_loan

#PEARSON'S CORRELATION COEFFICIENT:

pearson_correlation_ages_inc <- cor(ages, income, method = "pearson")  
pearson_correlation_ages_inc

pearson_correlation_exp_inc <- cor(experience, income, method = "pearson")
pearson_correlation_exp_inc

pearson_correlation_edu_inc <- cor(education, income, method = "pearson")
pearson_correlation_edu_inc

pearson_correlation_fam_ccavg <- cor(family, ccavg, method = "pearson")
pearson_correlation_fam_ccavg

pearson_correlation_fam_loan <- cor(family, loan, method = "pearson")   
pearson_correlation_fam_loan

#TABLES WITH GENERAL STATS:

#To create a data frame to store the summary statistics >> it doesn't work, idk why 
#summary_stats <- data.frame(
#  Variable = c("Age", "Experience", "Income", "Family", "CCavg", "Education", "Mortgage", "Loan"),
#  Mean = c(mean_ages, mean_exp, mean_inc, mean_fam, mean_ccavg, mean_edu, mean_mort, mean_loan),
#  Q1 = c(Q1_ages, Q1_exp, Q1_inc, Q1_fam, Q1_ccavg, Q1_edu, Q1_mort, Q1_loan),
#  Median = c(median_ages, median_exp, median_inc, median_fam, median_ccavg, median_edu, median_mort, median_loan),
#  Q3 = c(Q3_ages, Q3_exp, Q3_inc, Q3_fam, Q3_ccavg, Q3_edu, Q3_mort, Q3_loan),
#  Mode = c(mode_ages, mode_exp, mode_inc, mode_fam, mode_ccavg, mode_edu, mode_mort, mode_loan),
#  Variance = c(var_ages, var_exp, var_inc, var_fam, var_ccavg, var_edu, var_mort, var_loan),
#  Std_Dev = c(std_ages, std_exp, std_inc, std_fam, std_ccavg, std_edu, std_mort, std_loan),
#  Gini_Index = c(gini_impurity_ages, gini_impurity_exp, gini_impurity_inc, gini_impurity_fam, gini_impurity_ccavg, gini_impurity_edu, gini_impurity_mort, gini_impurity_loan),
#)
#print(summary_stats)

#single tables:

table_ages <- data.frame(
  Statistic = c("Q1", "Median", "Q3","Mean","Mode","Variance", "St.Dev", "Gini Index"),
  Ages = c(Q1_ages, median_ages, Q3_ages, mean_ages, mode_ages, var_ages, std_ages, gini_impurity_ages),
  )
print(table_ages)

table_exp <- data.frame(
  Statistic = c("Q1", "Median", "Q3","Mean","Mode", "Variance", "St.Dev", "Gini Index"),
  Value = c(Q1_exp, median_exp, Q3_exp, mean_exp, mode_exp, var_exp, std_exp, gini_impurity_exp)
)
print(table_exp)

table_inc <- data.frame(
  Statistic = c("Q1", "Median", "Q3","Mean","Mode", "Variance", "St.Dev", "Gini Index"),
  Value = c(Q1_inc, median_inc, Q3_inc, mean_inc, mode_inc, var_inc, std_inc, gini_impurity_inc)
)
print(table_inc)

table_fam <- data.frame(
  Statistic = c("Q1", "Median", "Q3","Mean","Mode", "Variance", "St.Dev", "Gini Index"),
  Value = c(Q1_fam, median_fam, Q3_fam, mean_fam, mode_fam, var_fam, std_fam, gini_impurity_fam)
)
print(table_fam)

table_ccavg <- data.frame(
  Statistic = c("Q1", "Median", "Q3","Mean","Mode", "Variance", "St.Dev", "Gini Index"),
  Value = c(Q1_ccavg, median_ccavg, Q3_ccavg, mean_ccavg, mode_ccavg, var_ccavg, std_ccavg, gini_impurity_ccavg)
)
print(table_ccavg)

table_edu <- data.frame(
  Statistic = c("Q1", "Median", "Q3","Mean","Mode", "Variance", "St.Dev", "Gini Index"),
  Value = c(Q1_edu, median_edu, Q3_edu, mean_edu, mode_edu, var_edu, std_edu, gini_impurity_edu)
)
print(table_edu)

table_mort <- data.frame(
  Statistic = c("Q1", "Median", "Q3","Mean","Mode", "Variance", "St.Dev", "Gini Index"),
  Value = c(Q1_mort, median_mort, Q3_mort, mean_mort, mode_mort, var_mort, std_mort, gini_impurity_mort)
)
print(table_mort)

table_loan <- data.frame(
  Statistic = c("Q1", "Median", "Q3","Mean","Mode", "Variance", "St.Dev", "Gini Index"),
  Value = c(Q1_loan, median_loan, Q3_loan, mean_loan, mode_loan, var_loan, std_loan, gini_impurity_loan)
)
print(table_loan)

#K-MEANS:

kmeans_ages <- kmeans(ages, centers = 5)
kmeans_ages$cluster
kmeans_ages$centers
plot(Bank_customers_data$Age, xlab = "ID", ylab = "Age",
     main = "K-means Clustering Ages", col = kmeans_ages$cluster, pch=19)

kmeans_exp <- kmeans(experience, centers = 5)
kmeans_exp$cluster
kmeans_exp$centers
plot(Bank_customers_data$Experience, xlab = "ID", ylab = "Experience",
     main = "K-means Clustering Experience", col = kmeans_exp$cluster, pch=19)

kmeans_inc <- kmeans(income, centers = 5)
kmeans_inc$cluster
kmeans_inc$centers
plot(Bank_customers_data$Income, xlab = "ID", ylab = "Income",
     main = "K-means Clustering Income", col = kmeans_inc$cluster, pch=19)

kmeans_fam <- kmeans(family, centers = 4) #not 5 because there was more clusters then distinct points
kmeans_fam$cluster
kmeans_fam$centers
plot(Bank_customers_data$Family, xlab = "ID", ylab = "Family",
     main = "K-means Clustering Family", col = kmeans_fam$cluster, pch=19)

kmeans_ccavg <- kmeans(ccavg, centers = 5)
kmeans_ccavg$cluster
kmeans_ccavg$centers
plot(Bank_customers_data$CCAvg, xlab = "ID", ylab = "CCavg",
     main = "K-means Clustering CCavg", col = kmeans_ccavg$cluster, pch=19)

kmeans_edu <- kmeans(education, centers = 3) #we have only 3 possible values
kmeans_edu$cluster
kmeans_edu$centers
plot(Bank_customers_data$Education, xlab = "ID", ylab = "Education",
     main = "K-means Clustering Education", col = kmeans_edu$cluster, pch=19)

kmeans_mort <- kmeans(mortgage, centers = 5)
kmeans_mort$cluster
kmeans_mort$centers
plot(Bank_customers_data$Mortgage, xlab = "ID", ylab = "Mortgage",
     main = "K-means Clustering Mortgage", col = kmeans_mort$cluster, pch=19)

kmeans_loan <- kmeans(loan, centers = 5)
kmeans_loan$cluster
kmeans_loan$centers
plot(Bank_customers_data$Personal.Loan, xlab = "ID", ylab = "Loan",
     main = "K-means Clustering Loan", col = kmeans_loan$cluster, pch=19)

#HIERARCHICAL CLUSTERING:

distance_matrix_ages <- dist(ages)
hclust_ages <- hclust(distance_matrix_ages, method = "average")
plot(hclust_ages, main = "Hierarchical Clustering Dendrogram - Age", xlab = "ID", 
     sub = "", ylab = "Height")
clusters_ages <- cutree(hclust_ages, k = 5)
print(clusters_ages)

distance_matrix_exp <- dist(experience)
hclust_exp <- hclust(distance_matrix_exp, method = "average")
plot(hclust_exp, main = "Hierarchical Clustering Dendrogram - Experience", xlab = "ID", 
     sub = "", ylab = "Height")
clusters_exp <- cutree(hclust_exp, k = 5)
print(clusters_exp)

distance_matrix_inc <- dist(income)
hclust_inc <- hclust(distance_matrix_inc, method = "average")
plot(hclust_inc, main = "Hierarchical Clustering Dendrogram - Income", xlab = "ID", 
     sub = "", ylab = "Height")
clusters_inc <- cutree(hclust_inc, k = 5)
print(clusters_inc)

distance_matrix_fam <- dist(family)
hclust_fam <- hclust(distance_matrix_fam, method = "average")
plot(hclust_fam, main = "Hierarchical Clustering Dendrogram - Family", xlab = "ID", 
     sub = "", ylab = "Height")
clusters_fam <- cutree(hclust_fam, k = 5)
print(clusters_fam)

distance_matrix_ccavg <- dist(ccavg)
hclust_ccavg <- hclust(distance_matrix_ccavg, method = "average")
plot(hclust_ccavg, main = "Hierarchical Clustering Dendrogram - CCavg", xlab = "ID", 
     sub = "", ylab = "Height")
clusters_ccavg <- cutree(hclust_ccavg, k = 5)
print(clusters_ccavg)

distance_matrix_edu <- dist(education)
hclust_edu <- hclust(distance_matrix_edu, method = "average")
plot(hclust_edu, main = "Hierarchical Clustering Dendrogram - Education", xlab = "ID", 
     sub = "", ylab = "Height")
clusters_edu <- cutree(hclust_edu, k = 5)
print(clusters_edu)

distance_matrix_mort <- dist(mortgage)
hclust_mort <- hclust(distance_matrix_mort, method = "average")
plot(hclust_mort, main = "Hierarchical Clustering Dendrogram - Mortgage", xlab = "ID", 
     sub = "", ylab = "Height")
clusters_mort <- cutree(hclust_mort, k = 5)
print(clusters_mort)

distance_matrix_loan <- dist(loan)
hclust_loan <- hclust(distance_matrix_loan, method = "average")
plot(hclust_loan, main = "Hierarchical Clustering Dendrogram - Loan", xlab = "ID", 
     sub = "", ylab = "Height")
clusters_loan <- cutree(hclust_loan, k = 5)
print(clusters_loan)

#PCA: 

data_pca <- data.frame(
  Bank_customers_data[, -1]     #remove the first column of the data
)

scaled_data <- scale(data_pca)

pca_result <- prcomp(scaled_data, center = TRUE, scale. = TRUE)

summary(pca_result)

biplot(pca_result, main = "PCA Biplot", cex = c(0.3,0.6))  #first value for observation and second value for variables

#THE END