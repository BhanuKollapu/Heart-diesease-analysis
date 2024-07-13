data<-read.csv(file.choose(),header=TRUE)
data

# Display the first few rows of data
head(data)

#Display the number of rows and columns
nrow(data)
ncol(data)


# Understanding the datatype of dataset
str(data)

# Summary
summary(data)
names(data)

################### Handle nul values######################
colSums(is.na(data))
any(is.na(data))

data<- na.omit(data)
data

#######################Removing columns###############################
data<-subset(data,select = -dataset)
data
##############################Duplicates###########################
sum(duplicated(data))
#there is no duplicates in dataset

boxplot(data$id)

#################################Outliers##############################
boxplot(data$age)
bboxplot(data$trestbps)
boxplot(data$chol)
boxplot(data$fbs)
boxplot(data$thalch)
boxplot(data$exang)
boxplot(data$oldpeak)
boxplot(data$ca)
boxplot(data$taget)


z_score<-scale(data$age)
outliers<-which(abs(z_score)>3)
outliers
removeoutliers<-which(abs(z_score)<3)
removeoutliers
boxplot(removeoutliers)

z_score<-scale(data$trestbps)
outliers<-which(abs(z_score)>3)
outliers
removeoutliers<-which(abs(z_score)<3)
removeoutliers
boxplot(removeoutliers)

z_score<-scale(data$chol)
outliers<-which(abs(z_score)>3)
outliers
removeoutliers<-which(abs(z_score)<3)
removeoutliers
boxplot(removeoutliers)

z_score<-scale(data$fbs)
outliers<-which(abs(z_score)>3)
outliers
removeoutliers<-which(abs(z_score)<3)
removeoutliers
boxplot(removeoutliers)

z_score<-scale(data$thalch)
outliers<-which(abs(z_score)>3)
outliers
removeoutliers<-which(abs(z_score)<3)
removeoutliers
boxplot(removeoutliers)

z_score<-scale(data$exang)
outliers<-which(abs(z_score)>3)
outliers
removeoutliers<-which(abs(z_score)<3)
removeoutliers
boxplot(removeoutliers)

z_score<-scale(data$oldpeak)
outliers<-which(abs(z_score)>3)
outliers
removeoutliers<-which(abs(z_score)<3)
removeoutliers
boxplot(removeoutliers)

z_score<-scale(data$ca)
outliers<-which(abs(z_score)>3)
outliers
removeoutliers<-which(abs(z_score)<3)
removeoutliers
boxplot(removeoutliers)

##############################Tranformation####################################
data$sex[data$sex == 'Male']<- 1
data$sex[data$sex=='Female']<-0
View(data)

data$fbs[data$fbs == 'TRUE']<- 1
data$fbs[data$fbs=='FALSE']<-0
View(data)

data$exang[data$exang == 'TRUE']<- 1
data$exang[data$exang=='FALSE']<-0
View(data)
##############################Standardization##################################
# Calculate mean and standard deviation
mean_value <- mean(data$age)
sd_value <- sd(data$age)

# Standardize the column
data$standardized_age <- (data$age - mean_value) / sd_value
data$standardized_age


#############################Normalization####################################
# Calculate minimum and maximum values
min_value <- min(data$age)
max_value <- max(data$age)

# Normalize the column
data$normalized_age<- (data$age) / (max_value - min_value)
data$normalized_age

#####################Data Visualization#######################################
# Summary statistics for numerical columns
summary(data[c("age", "trestbps", "chol", "thalch", "oldpeak")])

# Visualizing distributions and relationships
# Example: Histogram for age
library(ggplot2)
ggplot(data, aes(x = age)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Age")

# Histogram for oldpeak
ggplot(data, aes(x = oldpeak))+
  geom_histogram(binwidth = 1, fill = "red", color = "yellow") +
  labs(title = "Disribution of oldpeak ")

# Scatterplot of age vs cholesterol
ggplot(data, aes(x = age, y = chol)) +
  geom_point(color = "darkgreen", alpha = 0.6) +
  labs(title = "Age vs Cholesterol", x = "Age", y = "Cholesterol")
# Scatterplot of age vs oldpeak
ggplot(data, aes(x = age,, y = oldpeak)) +
  geom_point(color = "yellowgreen", alpha = 0.9) +
  labs(title = "Age vs Oldpeak", x ="Age", y = "Oldpeak")

# Boxplot of age by sex
ggplot(data, aes(x = sex, y = age, fill = factor(sex))) +
  geom_boxplot() +
  labs(title = "Age Distribution by Sex", x = "Sex", y = "Age")

# Example: Visualizing heart disease target variable
ggplot(data, aes(x = factor(target), fill = factor(target))) +
  geom_bar() +
  labs(title = "Heart Disease Distribution", x = "Target", y = "Count")

# Example: Scatterplot matrix for selected columns
selected_cols <- data[, c("age", "trestbps", "chol", "thalch", "oldpeak")] ##The c() function in R is used to create a vector of column names to be selected from the data dataframe.
pairs(selected_cols)

######################Adding column to dataset#############################'
data$oldpeople <-data$age> 60
View(data)
