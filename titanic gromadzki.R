
# Alex Gromadzki
# Titanic Homework Assignment

library(tidyverse)
library(readr)  # Provides "read_csv" function
library(dplyr)  # Allows for nicer display of data frame

setwd("~/UVA/DSI/Fall 2018/SYS/Mining in R/Homework/Homework 0/sys6018-competition-titanic/TitanicData")

original_data <- read_csv("train.csv")


# this sets all the cases where there is a value equal to one and all of the ones without a value equal to zero

# seems that the most missing data is cabin or embarked
# just going to get rid of cabin, seems like i can logically (explained later on, especially since i don't know specifics
# of cabin tiers)


# ====== Full ========

full_data <- mutate(original_data,
                        # Cabin2 = if_else(Cabin=="NA", true = 0, false = 1, missing = 0),
                        Age2 = if_else(Age=="NA", true = 0, false = 1, missing = 0))

# in order to use all of the NA's, I am going to calculate the mean age of all present and insert that so as not
# to skew the coefficients in one direction or another


full_ages <- full_data[(full_data$Age2 == 1),]
m.ages <- mean(full_ages$Age)

 
full_data <- mutate(full_data,
                         # Cabin = if_else(Cabin=="NA", true = "NONE", false = Cabin, missing = "NONE"),
                         Age = if_else(Age=="NA", true = m.ages, false = Age, missing = m.ages))

full_data$Age2 <- NULL


# ====== Testing ========

testing_data <- read_csv("test.csv")


temp.testing_data <- mutate(testing_data,
                        # Cabin2 = if_else(Cabin=="NA", true = 0, false = 1, missing = 0),
                        Age2 = if_else(Age=="NA", true = 0, false = 1, missing = 0))

test_all_with_ages <- temp.testing_data[(temp.testing_data$Age2 == 1),]
test_m.ages <- mean(test_all_with_ages$Age)

testing_data <- mutate(testing_data,
                         # Cabin = if_else(Cabin=="NA", true = "NONE", false = Cabin, missing = "NONE"),
                         Age = if_else(Age=="NA", true = test_m.ages, false = Age, missing = test_m.ages))


# ====== Training Subset ========
sub <- sample(1:891,size=445)
training_data <- original_data

training_data2 <- training_data[sub,]     # Select subset for cross-validation

temp.training_data2 <- mutate(training_data2,
                            # Cabin2 = if_else(Cabin=="NA", true = 0, false = 1, missing = 0),
                            Age2 = if_else(Age=="NA", true = 0, false = 1, missing = 0))

training_all_with_ages <- temp.training_data2[(temp.training_data2$Age2 == 1),]
train2_m.ages <- mean(training_all_with_ages$Age)

training_data2 <- mutate(training_data2,
                       # Cabin = if_else(Cabin=="NA", true = "NONE", false = Cabin, missing = "NONE"),
                       Age = if_else(Age=="NA", true = train2_m.ages, false = Age, missing = train2_m.ages))


# ====== Validation Subset ========

validation_data <- training_data[-sub,]

temp.validation_data <- mutate(validation_data,
                              # Cabin2 = if_else(Cabin=="NA", true = 0, false = 1, missing = 0),
                              Age2 = if_else(Age=="NA", true = 0, false = 1, missing = 0))

validation_all_with_ages <- temp.validation_data[(temp.validation_data$Age2 == 1),]
validation_m.ages <- mean(validation_all_with_ages$Age)

validation_data <- mutate(validation_data,
                         # Cabin = if_else(Cabin=="NA", true = "NONE", false = Cabin, missing = "NONE"),
                         Age = if_else(Age=="NA", true = validation_m.ages, false = Age, missing = validation_m.ages))





# I realized that I should probably just drop cabin since there are more unique rooms in testing.  did not
# alter the above code involving cabin, given that it might be useful later

full_data$Cabin <- NULL
testing_data$Cabin <- NULL
training_data$Cabin <- NULL
training_data2$Cabin <- NULL
validation_data$Cabin <- NULL






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

training_data2$Pclass <- factor(training_data2$Pclass)
training_data2$Sex <- factor(training_data2$Sex)
# training_data2$SibSp <- factor(training_data2$SibSp)    # while intuitively I would leave make these factors (discrete)
# training_data2$Parch <- factor(training_data2$Parch)    # just going to keep these continuous for now
# training_data2$Cabin <- factor(training_data2$Cabin)
training_data2$Embarked <- factor(training_data2$Embarked)
training_data2$Name <- NULL
training_data2$Ticket <- NULL

validation_data$Pclass <- factor(validation_data$Pclass)
validation_data$Sex <- factor(validation_data$Sex)
# validation_data$SibSp <- factor(validation_data$SibSp)    # from above
# validation_data$Parch <- factor(validation_data$Parch)
# validation_data$Cabin <- factor(validation_data$Cabin)
validation_data$Embarked <- factor(validation_data$Embarked)
validation_data$Name <- NULL
validation_data$Ticket <- NULL


testing_data$Pclass <- factor(testing_data$Pclass)
testing_data$Sex <- factor(testing_data$Sex)
# testing_data$SibSp <- factor(testing_data$SibSp)      # taken from above
# testing_data$Parch <- factor(testing_data$Parch)
# testing_data$Cabin <- NULL                                      # did this above
testing_data$Embarked <- factor(testing_data$Embarked)
training_data2$Name <- NULL
training_data2$Ticket <- NULL

full_data$Pclass <- factor(full_data$Pclass)
full_data$Sex <- factor(full_data$Sex)
# full_data$SibSp <- factor(full_data$SibSp)    # while intuitively I would leave make these factors (discrete)
# full_data$Parch <- factor(full_data$Parch)    # just going to keep these continuous for now
# full_data$Cabin <- factor(full_data$Cabin)
full_data$Embarked <- factor(full_data$Embarked)
full_data$Name <- NULL
full_data$Ticket <- NULL

# For the first iteration of the model, I assume 
# that room number and passenger class likely were correlated (passenger class simpler to deal with so removed Room)


training_glm1 <- glm(Survived ~ .-PassengerId, data=training_data2)
summary(training_glm1)

# My intuition is telling me that without being able to use any interaction terms, 
  #I should get rid of some potentially similar variables.
    # First however, lets just assume that without any other background the destinations were the same

training_glm2 <- glm(Survived ~ .-PassengerId -Embarked, data=training_data2)
summary(training_glm2)

# how about something else... multitasking? sure why not.  trying ticket price vs class and a family status variable

training_glm3 <- glm(Survived ~ .-PassengerId -Embarked -Pclass -Parch, data=training_data2)
summary(training_glm3)

training_glm3b <- glm(Survived ~ .-PassengerId -Embarked -Fare -Parch, data=training_data2)
summary(training_glm3b)

# 3B is my best


# time to try on the validation data

prep_set_valid <- 
  glm(Survived ~ .-PassengerId -Embarked -Fare -Parch, data=training_data2, family = "binomial")

# note: there is an issue where there are new names within the factor variable of name, maybe just don't set as factor?
# got rid of name, along with several other types
prob_survival_valid<-as.vector(predict(prep_set_valid, newdata = validation_data, type = "response"))
pred_survival_valid <- rep(0,418)  # Initialize prediction vector
pred_survival_valid[prob_survival_valid > 0.5] <- 1 # p>0.5 -> 1
validation_data$Survived2 <- pred_survival_valid
table(pred_survival_valid,validation_data$Survived)


# ======== Testing =========

prep_set <- 
  glm(Survived ~ .-PassengerId -Embarked -Fare -Parch, data=training_data2, family = "binomial")

# note: there is an issue where there are new names within the factor variable of name, maybe just don't set as factor?
# got rid of name, along with several other types
prob_survival<-as.vector(predict(prep_set, newdata = testing_data, type = "response"))
pred_survival <- rep(0,418)  # Initialize prediction vector
pred_survival[prob_survival > 0.5] <- 1 # p > 0.5 -> 1
testing_data$Survived <- pred_survival
testing_data

answer <- data_frame(testing_data$PassengerId,testing_data$Survived)

setwd("~/UVA/DSI/Fall 2018/SYS/Mining in R/Homework/Homework 0/sys6018-competition-titanic")
write.table(answer, file = "arg2eu.csv", row.names=F, col.names=c("PassengerId","Survived"), sep=",")


