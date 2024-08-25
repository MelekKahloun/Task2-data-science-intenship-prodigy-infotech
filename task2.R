library(tidyverse)
library(ggplot2)
library(dplyr)
library(corrplot)

titanic <- read.csv("C:\\Users\\lenovo\\Downloads\\prodigy\\task2\\train.csv")

head(titanic)
summary(titanic)

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

titanic$Sex <- as.numeric(factor(titanic$Sex))
titanic$Embarked <- as.numeric(factor(titanic$Embarked))

corr_data <- titanic %>% select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare)

corr_matrix <- cor(corr_data, use = "complete.obs")

# Plot the correlation heatmap
corrplot(corr_matrix, method = "color", type = "upper", tl.col = "black", tl.srt = 45)

# Plot 4: Survival Rate by Gender
p4 <- ggplot(titanic, aes(x = Sex, fill = factor(Survived))) + 
  geom_bar(position = "dodge") + 
  labs(title = "Survival Rate by Gender", x = "Gender", y = "Count")

# Plot 5: Survival Rate by Passenger Class
p5 <- ggplot(titanic, aes(x = factor(Pclass), fill = factor(Survived))) + 
  geom_bar(position = "dodge") + 
  labs(title = "Survival Rate by Passenger Class", x = "Passenger Class", y = "Count")

# Display all plots
print(p1)
print(p2)
print(p3)
print(p4)
print(p5)



