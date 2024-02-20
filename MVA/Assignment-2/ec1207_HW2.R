library(readr)
library(stats)
library(ggplot2)

# Loading Dataset
titanic=read_csv('C:/Users/Admin/Desktop/titanic.csv')

# Checking the structure of dataset
str(titanic)

# Summary statistics
summary(titanic)

head(titanic)

# converting the numeric survive indicator to factor/categorical
titanic$Survived=ifelse(titanic$Survived==1,'Yes','No')
titanic$Survived=as.factor(titanic$Survived)

head(titanic)

# Converting categorical columns from int to factors
titanic$Pclass=as.factor(titanic$Pclass)
titanic$SibSp=as.factor(titanic$SibSp)
titanic$Parch=as.factor(titanic$Parch)

head(titanic)

sum(is.na(titanic$Age))
# from the above line we can see that 'Age' column has 177 missing values
# We can choose either mean or median method to fillin the values
# Mean might not give accurate results, can fill the values with median.

titanic$Age[is.na(titanic$Age)] <- round(median(titanic$Age, 
                                                na.rm = TRUE))

sum(is.na(titanic$Age))
# Now there are no missing values in Age column

# Exploratory Data Analysis

#Univariate Analysis

#Question
#How any passengers are travelling in each class?

ggplot(data=titanic, aes(x=Pclass, fill = Pclass)) + 
  geom_bar(position = "dodge") + 
  geom_text(stat='count', aes(label=..count..), position = position_dodge(0.9),vjust=-0.2) +
  ylab("Number of Passengers")

# We can see most of the passengers are from class 3 (cheapest of all)
# Suprisingly there are more passengers in Class-1, than compared to Class-2

# Question
# How many were survived?

ggplot(data=titanic, aes(x=Survived, fill = Survived)) + 
  geom_bar(position = "dodge") + 
  geom_text(stat='count', aes(label=..count..), position = position_dodge(0.9),vjust=-0.2) +
  ylab("Number of Passengers")

# Here we can see there are 342 that are survived, and 549 passengers who lost their lives

#Question
# How is the price ranged?
ggplot(data=titanic, aes(x=Fare,)) + 
  geom_histogram(binwidth = 15) +
  xlab("Fare")

# The tickets fare are consistent with the ticket class for sure, as the highest number of ticket purchased is the cheapest one offered to board the Titanic.

# BiVariate Analysis
# Question
# Which class passengers has high survival chance
ggplot(titanic, aes(x=Pclass,fill=Survived))+ geom_bar(position = "dodge") + geom_text(stat='count',aes(label=..count..),position = position_dodge(0.9),vjust=-0.2) +
  ylab("Number of Passengers") + xlab("Passenger Class")

# From the chart we can conclude that people who paid more ie, Class-1 had much better chance of survival as compared to others.

# Question
# what ticket was selected by what age of passengers?
ggplot(titanic) + geom_freqpoly(mapping = aes(x = Age, color = Pclass), binwidth = 2.5) +
  ylab("Frequency")

# HEre, we can see that most of the passengers irrespective of ticket class are almost of similar age (approx 30)

# Age Distribution by passenger class and sex
ggplot(titanic, aes(x=factor(Pclass), y=Age, fill=Sex)) +
  geom_boxplot() +
  facet_grid(Sex ~ .) +
  scale_fill_manual(values=c("lightblue", "pink"), name="Sex") +
  labs(title="Age Distribution by Passenger Class and Sex",
       x="Passenger Class",
       y="Age") +
  theme_minimal()

# This faceted boxplot also shows almost similar that most of the passengers
# were from age 20-40 including all the ticket classes.


# Boxplot of fare and survived status
boxplot(Fare ~ Survived, data=titanic, main="Fare by Survival Status", xlab="Survived (0 = No, 1 = Yes)", ylab="Fare", col="lightgreen")
# Comparison of Age and Fare
plot(titanic$Age, titanic$Fare, main="Age vs. Fare", xlab="Age", ylab="Fare", col="blue")



# Scatterplot matrix for the numerical columns in the data
numeric_vars <- titanic[, c("Survived", "Pclass", "Age", "SibSp", "Parch", "Fare")]
pairs(numeric_vars)

# Question
# What percent of people were survived that were embarked at different ports
ggplot(titanic, aes(x=factor(Survived), fill=factor(Survived))) +
  geom_bar() +
  facet_grid(Pclass ~ Embarked) +
  scale_fill_manual(values=c("lightblue", "pink"), name="Survived") +
  labs(title="Survival Counts by Passenger Class and Embarked Port",
       x="Survived (0 = No, 1 = Yes)",
       y="Count") +
  theme_minimal()

# HEre we can conclude that passengers that embarked at port Southampton wer the ones who survivied more and also they are the ones who lost their lives

library(plotly)

#3D scatterplot using plotly
plot_ly(data = titanic, x = ~Age, y = ~Fare, z = ~Survived, color = ~Survived, colors = c("pink", "lightblue")) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = "Age"),
                      yaxis = list(title = "Fare"),
                      zaxis = list(title = "Survived")))





