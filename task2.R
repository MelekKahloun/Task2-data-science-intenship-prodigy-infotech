library(tidyverse)
library(ggplot2)
library(dplyr)
library(corrplot)

titanic <- read.csv("C:\\Users\\lenovo\\Downloads\\prodigy\\task2\\train.csv")

head(titanic)
summary(titanic)


# Handle missing values
# Impute missing 'Age' with the median age
titanic$Age[is.na(titanic$Age)] <- median(titanic$Age, na.rm = TRUE)

titanic$Embarked[is.na(titanic$Embarked)] <- 'S'

titanic <- titanic[!is.na(titanic$Fare), ]


titanic$FamilySize <- titanic$SibSp + titanic$Parch + 1

titanic$AgeGroup <- cut(titanic$Age, breaks = c(0, 12, 18, 35, 60, 100),
                        labels = c("Child", "Teen", "Adult", "Middle Age", "Senior"))

titanic$FareGroup <- cut(titanic$Fare, breaks = quantile(titanic$Fare, probs = seq(0, 1, 0.25)),
                         include.lowest = TRUE, labels = c("Low", "Medium", "High", "Very High"))

# Exploratory Data Analysis (EDA)

# Plot 1: Count of Survival
p1 <- ggplot(titanic, aes(x = factor(Survived))) + 
    geom_bar(fill = 'blue') + 
    labs(title = "Count of Survival", x = "Survived", y = "Count")

# Plot 2: Distribution of Age
p2 <- ggplot(titanic, aes(x = Age)) + 
    geom_histogram(binwidth = 5, fill = 'skyblue', color = 'black') + 
    labs(title = "Distribution of Age", x = "Age", y = "Count")

# Plot 3: Boxplot of Age by Passenger Class
p3 <- ggplot(titanic, aes(x = factor(Pclass), y = Age)) + 
    geom_boxplot(fill = 'orange') + 
    labs(title = "Boxplot of Age by Passenger Class", x = "Passenger Class", y = "Age")

# Plot 4: Survival Rate by Gender
p4 <- ggplot(titanic, aes(x = factor(Sex), fill = factor(Survived))) + 
    geom_bar(position = "dodge") + 
    labs(title = "Survival Rate by Gender", x = "Gender", y = "Count")

# Plot 5: Survival Rate by Passenger Class
p5 <- ggplot(titanic, aes(x = factor(Pclass), fill = factor(Survived))) + 
    geom_bar(position = "dodge") + 
    labs(title = "Survival Rate by Passenger Class", x = "Passenger Class", y = "Count")

# Plot 6: Survival Rate by Age Group
p6 <- ggplot(titanic, aes(x = AgeGroup, fill = factor(Survived))) + 
    geom_bar(position = "dodge") + 
    labs(title = "Survival Rate by Age Group", x = "Age Group", y = "Count")

# Plot 7: Survival Rate by Family Size
p7 <- ggplot(titanic, aes(x = FamilySize, fill = factor(Survived))) + 
    geom_bar(position = "dodge") + 
    labs(title = "Survival Rate by Family Size", x = "Family Size", y = "Count")

# Plot 8: Survival Rate by Fare Group
p8 <- ggplot(titanic, aes(x = FareGroup, fill = factor(Survived))) + 
    geom_bar(position = "dodge") + 
    labs(title = "Survival Rate by Fare Group", x = "Fare Group", y = "Count")

# Convert categorical variables to numeric for correlation analysis
titanic$Sex <- as.numeric(factor(titanic$Sex))
titanic$Embarked <- as.numeric(factor(titanic$Embarked))
titanic$Pclass <- as.numeric(factor(titanic$Pclass))
titanic$Survived <- as.numeric(factor(titanic$Survived))

# Select relevant columns for correlation
corr_data <- titanic %>% select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare, FamilySize)

# Compute correlation matrix
corr_matrix <- cor(corr_data, use = "complete.obs")

# Plot the correlation heatmap
corrplot(corr_matrix, method = "color", type = "upper", tl.col = "black", tl.srt = 45)

# Display all plots
print(p1)
print(p2)
print(p3)
print(p4)
print(p5)
print(p6)
print(p7)
print(p8)




