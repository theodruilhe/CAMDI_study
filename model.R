# LIBRAIRIES IMPORTATION
library(dplyr)
library(glm2)
library(viridis)
library(dummies)
library(car)


# DATA IMPORTATION
initial_data <- read.csv("data_michigan.csv", header = TRUE, sep = ",")
data_final <- read.csv("data_final.csv", header = TRUE, sep = ",")

### DATA PREPARATION ###
colnames(data_final)

# QUALITATIVE VARIABLES MANIPULATION
data_final$derived_race <- as.factor(data_final$derived_race)
data_final$loan_purpose <- as.factor(data_final$loan_purpose)
data_final$applicant_credit_score_type <- as.factor(data_final$applicant_credit_score_type)
data_final$applicant_age <- as.factor(data_final$applicant_age)
data_final$deny <- as.factor(data_final$deny)
data_final$applicant_sex <- as.factor(data_final$applicant_sex)
data_final$same_sex <- as.factor(data_final$same_sex)
data_final$income <- data_final$income * 1000

data_final$derived_race <- relevel(data_final$derived_race, ref = "White")
data_final$loan_purpose <- relevel(data_final$loan_purpose, ref = "1")
data_final$applicant_credit_score_type <- relevel(data_final$applicant_credit_score_type, ref = "1")
data_final$applicant_sex <- relevel(data_final$applicant_sex, ref = "1")
data_final$applicant_age <- relevel(data_final$applicant_age, ref = "35-44")
data_final$deny <- relevel(data_final$deny, ref = "0")
data_final$same_sex <- relevel(data_final$same_sex, ref = "0")

# QUANTITATIVE VARIABLES MANIPULATION
data_final$loan_amount <- as.numeric(data_final$loan_amount)
data_final$loan_term <- as.numeric(data_final$loan_term)
data_final$property_value <- as.numeric(data_final$property_value)

### CORRELATION TEST ###
cor.test(data_final$loan_amount,data_final$income) # significant correlation and 0.42
cor.test(data_final$loan_amount,data_final$property_value) # significant correlation and 0.61
cor.test(data_final$income,data_final$property_value) # significant correlation and 0.58
# WE HAVE A LOT OF CORRELATION AMONG OUR NUMERIC VARIABLES
# We aim to check for multicollinearity among our explanatory variables then we will run a VIF test


### VIF TEST ###
# put all the explanatory variables together
X <- model.matrix(~ derived_race + loan_purpose + loan_amount + loan_term + 
                    property_value + income + applicant_credit_score_type +
                    applicant_sex + applicant_age + same_sex, data = data_final)


model_vif <- glm(deny ~ derived_race + loan_purpose + loan_amount + loan_term + 
                      property_value + income + applicant_credit_score_type +
                      applicant_sex + applicant_age + same_sex, data = data_final, family = "binomial")
summary(model_vif)
# Calculate VIFs for all explanatory variables
vif_result <- vif(model_vif)
print(vif_result)


### Model 1 ###
# Explain the deny by the discriminatory variable 
# deny = Beta_0 + Beta_1*race + Beta_2*sex + Beta_3*age + Beta_4*same_sex
X1 <- model.matrix(~ derived_race + applicant_sex + applicant_age + same_sex, data = data_final)
model_1 <- glm(deny ~ X1, data = data_final, family = "binomial")
summary(model_1)
# we can see that the variable "applicant_sex" and "same_sex" are not significant then we will remove it from the model


### Model 2 ###
# Add some control variables
X2 <- model.matrix(~ derived_race + applicant_age + income, data = data_final)
model_2 <- glm(deny ~ X2, data = data_final, family = "binomial")
summary(model_2)
# we can see that the variable "income" is significant and it obvious that it explain the deny, then we will keep it in the model

