
# Alex Gromadzki
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

# training_data2 <- mutate(training_data,                                       # why doesn't this work?
#                   Cabin2 = if_else(is.nan(Cabin), true = 0, false = 1),
#                   Age2 = if_else(is.nan(Age), true = 0, false = 1),

training_data2 <- mutate(training_data,
                        Cabin2 = if_else(Cabin=="NA", true = 0, false = 1, missing = 0),
                        Age2 = if_else(Age=="NA", true = 0, false = 1, missing = 0))
# this sets all the cases where there is a value equal to one and all of the ones without a value equal to zero



# in order to use all of the NA's, I am going to calculate the mean age of all present and insert that so as not
# to skew the coefficients in one direction or another


all_with_ages <- training_data2[(training_data2$Age2 == 1),]
m.ages <- mean(all_with_ages$Age)

training_data3 <- mutate(training_data,
                         Cabin = if_else(Cabin=="NA", true = "NONE", false = Cabin, missing = "NONE"),
                         Age = if_else(Age=="NA", true = m.ages, false = Age, missing = m.ages))



# is.na(NA)



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
training_data3$Pclass <- factor(training_data3$Pclass)
training_data3$Sex <- factor(training_data3$Sex)
training_data3$Age <- factor(training_data3$Age)
training_data3$SibSp <- factor(training_data3$SibSp)
training_data3$Parch <- factor(training_data3$Parch)
training_data3$Cabin <- factor(training_data3$Cabin)
training_data3$Embarked <- factor(training_data3$Embarked)

# For the first iteration of the model, I assume 
# that room number and passenger class likely were correlated (passenger class simpler to deal with so removed Room)


# NOTE: many of the entries are not entirely complete (missing one or more fields)

training_glm1 <- glm(Survived ~ .-Name -Ticket -Cabin, data=training_data3)
summary(training_glm1)

# My intuition is telling me that without being able to use any interaction terms, 
  #I should get rid of some potentially similar variables.
    # First however, lets just assume that without any other background the destinations were the same

training_glm2 <- glm(Survived ~ .-Name -Ticket -Cabin -Embarked, data=training_data3)
summary(training_glm2)

# how about something else... multitasking? sure why not.  trying ticket price vs class and the family status variables

training_glm3 <- glm(Survived ~ .-Name -Ticket -Cabin -Embarked -Pclass -SibSp -Parch, data=training_data3)
summary(training_glm3)

