
# Alex Gromadzkio
# Titanic Homework Assignment

library(tidyverse)
library(readr)  # Provides "read_csv" function
library(dplyr)  # Allows for nicer display of data frame

setwd("~/UVA/DSI/Fall 2018/SYS/Mining in R/Homework/Homework 0/sys6018-competition-titanic/TitanicData")

original_data <- read_csv("train.csv")
training_data <- original_data
testing_data <- read_csv("test.csv")

# mean(as.numeric(training_data$Age, na.rm=TRUE)) #<---- how to handle this?
# seems that the most missing data is cabin, age, or embarked (R bootcamp)
# just going to get rid of cabin, seems like i can logically (explained later on, especially since i don't know specifics
    # of cabin tiers)

plot(training_data,pch=20,cex=.2)
training_data2 <- mutate(training_data, 
                  Cabin2 = if_else(is.nan(Cabin), true = 0, false = 1),
                  Age2 = if_else(is.nan(Age), true = 0, false = 1),
                  SibSp2 = if_else(is.nan(SibSp), true = 0, false = 1))


training_data

# =========Variables in original data and some heuristics============
# NOTE: categotical (non-continuous) is indicated by CAT
# -PassengerId -- just to keep track of them in original ordering, should not be included in model
# Pclass -- CAT ticket class, likely correlated with the type of cabin they were in, so potential heteroskedasticity
# -Name -- Should not be included
# Sex -- CAT  Could be relevant if the "WOMEN AND CHILDREN ONLY" thing from James Cameron's interpretation was real
# Age -- CAT Mothers and children thing again
# SibSp -- CAT siblings and spouses aboard the Titanic (this might be insignificant without 
          # considering additional interaction term between SibSp and Sex)
# Parch -- CAT similar to the above variable, it is likely to discern that a 4 year old is a child and not a parent,
          # but it is difficult to say for sure what predicted impact this might have
# -Ticket -- ticket number, should not be included
# Fare -- passenger fare is almost assuredly heteroskedasticity, but this might be better indicator than class
# Cabin -- CAT cabin number probably relates to the ticket class.  
          # only include if ticket data not broken down or decide on fare
# Embarked -- CAT do each of the towns in this categorical variable have significantly different demographic backgrounds?
          # again, this is an interaction term thing that we never covered how to do in R

#==============Attempt====================
training_data$Pclass <- factor(training_data$Pclass)
training_data$Sex <- factor(training_data$Sex)
training_data$Age <- factor(training_data$Age)
training_data$SibSp <- factor(training_data$SibSp)
training_data$Parch <- factor(training_data$Parch)
training_data$Cabin <- factor(training_data$Cabin)
training_data$Embarked <- factor(training_data$Embarked)

# For the first iteration of the model, I assume 
# that room number and passenger class likely were correlated (passenger class simpler to deal with so removed Room)


# NOTE: many of the entries are not entirely complete (missing one or more fields)

training_glm1 <- glm(Survived ~ .-Name -Ticket -Cabin, data=training_data)
summary(training_glm1)

# My intuition is telling me that without being able to use any interaction terms, 
  #I should get rid of some potentially similar variables.
    # First however, lets just assume that without any other background the destinations were the same

training_glm2 <- glm(Survived ~ .-Name -Ticket -Cabin -Embarked, data=training_data)
summary(training_glm2)

# how about something else... multitasking? sure why not.  trying ticket price vs class and the family status variables

training_glm3 <- glm(Survived ~ .-Name -Ticket -Cabin -Embarked -Pclass -SibSp -Parch, data=training_data)
summary(training_glm3)

