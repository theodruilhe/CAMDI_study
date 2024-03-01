sink(log_file.txt)
# LIBRAIRIES IMPORTATION
library(dplyr)
library(glm2)
library(viridis)
library(fastDummies)
library(car)

#### DATA CLEANING ####

### DATA IMPORTATION
data <- read.csv("data_michigan.csv", header = TRUE, sep = ",")

## Create a data_temp in order to clean the dataset without modifying the original one
data_temp <- data

### CLEANING USELESS COLUMNS ON A NEW DATAFRAME 'data_temp' ###

# Here we just delete the column that we are not interested in, we end up with 35 columns
data_temp <- data_temp[, !(names(data_temp) %in% c('derived_msa.md','lei','county_code','activity_year', 'state_code','census_tract', 'derived_loan_product_type', 'derived_sex',
                                                   'derived_dwelling_category', 'derived_ethnicity', 'purchaser_type',
                                                   'preapproval', 'reverse_mortgage', 'rate_spread', 'hoepa_status',
                                                   'total_loan_costs', 'total_points_and_fees', 'origination_charges',
                                                   'discount_points', 'prepayment_penalty_term', 'lender_credits', 'other_nonamortizing_features',
                                                   'manufactured_home_secured_property_type', 'manufactured_home_land_property_interest',
                                                   'multifamily_affordable_units', 'total_units', 'applicant_ethnicity.1', 'applicant_ethnicity.2',
                                                   'applicant_ethnicity.3', 'applicant_ethnicity.4', 'applicant_ethnicity.5', 'applicant_race.1',
                                                   'applicant_race.2', 'applicant_race.3', 'applicant_race.4', 'applicant_race.5', 'co.applicant_ethnicity.1',
                                                   'co.applicant_ethnicity.2', 'co.applicant_ethnicity.3', 'co.applicant_ethnicity.4', 'co.applicant_ethnicity.5',
                                                   'co.applicant_race.1', 'co.applicant_race.2', 'co.applicant_race.3', 'co.applicant_race.4', 'co.applicant_race.5',
                                                   'applicant_ethnicity_observed', 'co.applicant_ethnicity_observed', 'co.applicant_race_observed',
                                                   'applicant_race_observed', 'applicant_sex_observed', 'co.applicant_sex_observed', 'applicant_age_above_62',
                                                   'co.applicant_age_above_62', 'submission_of_application', 'initially_payable_to_institution', 'aus.3',
                                                   'aus.4', 'aus.5', 'denial_reason.1', 'denial_reason.2', 'denial_reason.3', 'denial_reason.4',"conforming_loan_limit", 'lien_status', 'open.end_line_of_credit',
                                                   'intro_rate_period', 'loan_type'
))]


#### CLEANING LINES ####
# In this part, we aim to clean the lines of our dataset. It could be by deleting the missing values, 
# or by modifying the values of the variables (ex: Male --> 1, Female --> 0).


## TARGET VARIABLE CREATION
# In variable action_taken only values:
#1 - Loan originated
#2 - Application approved but not accepted (the bank approved the loan application but the applicant decided to not take it)
#3 - Application denied
# are interesting because they allow us to discern if the application is a deny or not.

data_temp <- data_temp %>%
  filter(action_taken %in% c(1, 2, 3))
# creating of our target variable 'deny'
data_temp <- data_temp %>%
  mutate(deny = ifelse(action_taken == 3, 1, 0))


## CONSTRUCTION METHOD
# recoding 2,1 to 1,0 for construction_method (more simple to have a dummy)
data_temp$construction_method <- ifelse(data_temp$construction_method == 2, 1,
                                        ifelse(data_temp$construction_method == 1, 0,
                                               data_temp$construction_method))


## SEX
# 1 = Male, 2 = Female
data_temp <- data_temp %>%
  filter(applicant_sex %in% c(1, 2))


## SAME_SEX
# we create this variable to see if the applicant and the co-applicant are of the same sex
# maybe we could see if there is some discrimination for same sex applications
data_temp <- data_temp %>%
  filter(co.applicant_sex %in% c(1, 2, 5))

data_temp <- data_temp %>%
  mutate(same_sex = ifelse(applicant_sex == co.applicant_sex, 1, 0))


## RACE
# Deleting race that we are not interested to study
data_temp <- subset(data_temp, !derived_race %in% c("Race Not Available", "Joint", "2 or more minority races", "Free Form Text Only"))
# We gather dervied race to have only 4 categories: White, Black, Asian and Native
data_temp$derived_race <- ifelse(data_temp$derived_race %in% c("American Indian or Alaska Native", "Native Hawaiian or Other Pacific Islander"), "Native",
                                 ifelse(data_temp$derived_race == "Black or African American","Black", data_temp$derived_race))


## LOAN_PURPOSE
# 5 = Not applicable, thus not interesting because it doesn't give any information
data_temp <- subset(data_temp, loan_purpose != 5)


## BUSINESS OR COMMERCIAL PURPOSE 
# We work on individual (and not on business or compagny), thus we keep 'Not primarily for a business or commercial purpose' 
data_temp <- subset(data_temp, business_or_commercial_purpose == 2)
data_temp <- data_temp[, !(names(data_temp) %in% c('business_or_commercial_purpose'))]


### MISSING VALUES PROCESSING ###
# We delete the missing values or 'Exempt' values only for our main explanotory variables i
# i.e. variables we can't do without
data_temp <- subset(data_temp, debt_to_income_ratio != "Exempt" & debt_to_income_ratio != "NA")
data_temp <- subset(data_temp, applicant_age != 8888)
data_temp <- subset(data_temp, !is.na(loan_term))
data_temp$loan_term <- as.numeric(data_temp$loan_term)
data_temp <- subset(data_temp, !is.na(property_value))
data_temp <- subset(data_temp, !is.na(income))
data_temp <- subset(data_temp, !is.na(loan_to_value_ratio))


### OUTLIERS PROCESSING ###

## LOAN AMOUNT 
# In our study we will only consider loans under $1 million
data_temp <- subset(data_temp, loan_amount < 1000000)


## PROPERTY VALUE
# As for the loan amount we will only consider property value lower than 1 million
data_temp$property_value <- as.numeric(data_temp$property_value)
data_temp <- subset(data_temp, property_value < 1000000)


## LOAN TO VALUE RATIO
# converting values to be numeric
data_temp$loan_to_value_ratio <- as.numeric(data_temp$loan_to_value_ratio)
# deleting large outliers
# We have some values that equals '9999', it is the same thing of NA value. 
#We will delete them and we choose 150 as threshold for the loan to value ratio after looking at the histogram
data_temp <- subset(data_temp, loan_to_value_ratio < 150)


## INCOME
# deleting outliers: based on the histogram, we have arbitrarily chosen to eliminate values below 500,000.
# We have also chosen to remove negative income values, as they make no sense in this context.
data_temp <- subset(data_temp, income < 500)
data_temp <- subset(data_temp, income > 0)


### CREATION OF FINAL DATA SET ###
data_final <- data_temp[, (names(data_temp) %in%  c("deny", "derived_race", "loan_purpose", "loan_amount", 
                                                    "loan_to_value_ratio", "interest_rate", "loan_term",
                                                    "property_value", "income", "applicant_credit_score_type",
                                                    "applicant_sex", "applicant_age", "same_sex"))]

# Export data to csv
write.csv(data_final, "data_final.csv", row.names = FALSE)

#### DESCRIPTIVE STATISTICS ####

data_final <- read.csv("data_final.csv", header = TRUE, sep = ",")
## TARGET VARIABLE: DENY
table(data_final$deny)
prop.table(table(data_final$deny))
barplot(table(data_final$deny))
barplot(prop.table(table(data_final$deny))*100, 
        main = "Distribution of the target variable Deny",
        ylab = "Percentage",
        col = viridis(5),
        legend = c("Loan accepted", "Application denied"),
        names.arg = sprintf("%s\n%.2f%%", names(prop.table(table(data_final$deny)) * 100), prop.table(table(data_final$deny)) * 100))


## DERIVED RACE
table(data_final$derived_race)
prop.table(table(data_final$derived_race))
barplot(table(data_final$derived_race))
barplot(prop.table(table(data_final$derived_race))*100, 
        main = "Distribution of race", 
        ylab = "Percentage",
        col = viridis(10),
        args.legend = list(x = "topleft", title = "Legend"),
        names.arg = sprintf("%s\n%.2f%%", names(prop.table(table(data_final$derived_race)) * 100), prop.table(table(data_final$derived_race)) * 100))


## LOAN PURPOSE
#Values:
#1 - Home purchase
#2 - Home improvement
#31 - Refinancing
#32 - Cash-out refinancing
#4 - Other purpose
table(data_final$loan_purpose)
barplot(table(data_final$loan_purpose))
barplot(prop.table(table(data_final$loan_purpose))*100, 
        main = "Distribution of Loan purpose",
        ylab = "Percentage",
        col = viridis(5),
        legend = c("1. Home purchase","2. Home improvement", "4. Refinancing", "31. Cash-out refinancing", "32. Other purpose"),
        names.arg = sprintf("%s\n%.2f%%", names(prop.table(table(data_final$loan_purpose)) * 100), prop.table(table(data_final$loan_purpose)) * 100))


## LOAN AMOUNT
summary(data_final$loan_amount)
hist(data_final$loan_amount, breaks = 50)


## LOAN TO VALUE RATIO
# ploting histograms
hist(data_final$loan_to_value_ratio, breaks = 50)
summary(data_final$loan_to_value_ratio)


## LOAN TERM
# The number of months after which the legal obligation will mature or terminate, or would have matured or terminated
summary(data_final$loan_term)
hist(data_final$loan_term, breaks = 50)


## PPROPERTY VALUE
summary(data_final$property_value)
hist(data_final$property_value, breaks = 50)
# histogram en densite
hist(data_final$property_value, breaks = 30, probability = TRUE,
     main = "Histogram in Density",
     xlab = "Property Value")
lines(density(data_final$property_value), col = "blue", lwd = 2)


## INCOME
# The gross annual income, in thousands of dollars, relied on in making the credit decision,
# or if a credit decision was not made, the gross annual income relied on in processing the application
summary(data_final$income)
hist(data_final$income, breaks = 50, probability = TRUE,
     main = "Histogram in Density",
     xlab = "Income")
lines(density(data_final$income), col = "blue", lwd = 2)


## APPLICANT CREDIT SCORE TYPE
#The name and version of the credit scoring model used to generate the credit score, or scores relied on in making the credit decision
table(data_final$applicant_credit_score_type)
barplot(table(data_final$applicant_credit_score_type))
# 11 different credit scoring model


## APPLICANT SEX
# 1: Male
# 2: Female
table(data_final$applicant_sex)
barplot(table(data_final$applicant_sex))


## APPLICANT AGE
table(data_final$applicant_age)
barplot(table(data_final$applicant_age))


## SAME SEX APPLICATION
table(data_final$same_sex)
barplot(table(data_final$same_sex))

#### MODEL ####

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
data_final$income <- data_final$income * 1000

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
coef_1 <- coefficients(model_1)
summary(model_1)
# we can see that the variable "applicant_sex" and "same_sex" are not significant then we will remove it from the model


### Model 2 ###
# Add some control variables
X2 <- model.matrix(~ derived_race + applicant_age + income, data = data_final)
model_2 <- glm(deny ~ X2, data = data_final, family = "binomial")
coef_2 <- coefficients(model_2)
summary(model_2)
# we can see that the variable "income" is significant and it obvious that it explain the deny, then we will keep it in the model


### Model 3 ###
# Add some control variables
X3 <- model.matrix(~ derived_race + applicant_age + income + loan_amount 
                   + loan_purpose + loan_term + property_value, data = data_final)

model_3 <- glm(deny ~ X3, data = data_final, family = "binomial")
summary(model_3)
coef_3 <- coefficients(model_3)
coef_3 <- coef_3[!is.na(coef_3)]
coef_3

#### Model 4 ####
# Add the square of quantivative variables
data_final$income_2 <- data_final$income^2
data_final$loan_amount_2 <- data_final$loan_amount^2
data_final$property_value_2 <- data_final$property_value^2

X4 <- model.matrix(~ derived_race + applicant_age + income + income_2 + loan_amount 
                   + loan_amount_2 + loan_purpose + loan_term  + property_value
                   + property_value_2, data = data_final)

model_4 <- glm(deny ~ X4, data = data_final, family = "binomial")
summary(model_4)
coef_4 <- coefficients(model_4)
coef_4 <- coef_3[!is.na(coef_4)]
# Calculate VIFs for all explanatory variables
vif_model_4 <- vif(model_4)
print(vif_model_4)


### INTERPRETATION OF RESULTS ###
xasian <- c(1, 1, 0, 0, 0, 0, 0, 0, 0, 0, mean(data_final$income), 
            mean(data_final$loan_amount), 0, 0, 0, 0, 
            mean(data_final$loan_term),mean(data_final$property_value))
xblack <- c(1, 0, 1, 0, 0, 0, 0, 0, 0, 0, mean(data_final$income),
            mean(data_final$loan_amount), 0, 0, 0, 0, 
            mean(data_final$loan_term),mean(data_final$property_value))
xnative <- c(1, 0, 0, 1, 0, 0, 0, 0, 0, 0, mean(data_final$income), 
             mean(data_final$loan_amount), 0, 0, 0, 0,
             mean(data_final$loan_term),mean(data_final$property_value))
xwhite <- c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, mean(data_final$income), 
            mean(data_final$loan_amount), 0, 0, 0, 0, 
            mean(data_final$loan_term),mean(data_final$property_value))

p_black <- exp(coef_3 %*% xblack) / (1 + exp(coef_3 %*% xblack)) # Probability of being deny for the black people
p_native <- exp(coef_3 %*% xnative) / (1 + exp(coef_3 %*% xnative)) # Probability of being deny for the native people
p_asian <- exp(coef_3 %*% xasian) / (1 + exp(coef_3 %*% xasian)) # Probability of being deny for the asian people
p_white <- exp(coef_3 %*% xwhite) / (1 + exp(coef_3 %*% xwhite))     # Probability of being deny for the white people

p_black
p_native
p_white
p_asian


matrix_p <- matrix(c(p_black, p_native, p_asian, p_white), nrow = 1)
colnames(matrix_p)<- c("P(deny|black)", "P(deny|native)",  "P(deny|asian)", "P(deny|white)")
rownames(matrix_p) <- "Value"
matrix_p

sink()