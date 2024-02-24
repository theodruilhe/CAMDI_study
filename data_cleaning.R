# LIBRAIRIES IMPORTATION
library(dplyr)
library(glm2)
library(viridis)
library(fastDummies)

### DATA IMPORTATION
data <- read.csv("data_michigan.csv", header = TRUE, sep = ",")

## Create a data_temp in order to clean the dataset without modifying the original one
data_temp <- data

#### CLEANING USELESS COLUMNS ON A NEW DATAFRAME 'data_temp' ####

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


#### MISSING VALUES PROCESSING ####
# We delete the missing values or 'Exempt' values only for our main explanotory variables i
# i.e. variables we can't do without
data_temp <- subset(data_temp, debt_to_income_ratio != "Exempt" & debt_to_income_ratio != "NA")
data_temp <- subset(data_temp, applicant_age != 8888)
data_temp <- subset(data_temp, !is.na(loan_term))
data_temp$loan_term <- as.numeric(data_temp$loan_term)
data_temp <- subset(data_temp, !is.na(property_value))
data_temp <- subset(data_temp, !is.na(income))
data_temp <- subset(data_temp, !is.na(loan_to_value_ratio))


#### OUTLIERS PROCESSING ####

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


#### CREATION OF FINAL DATA SET ####
data_final <- data_temp[, (names(data_temp) %in%  c("deny", "derived_race", "loan_purpose", "loan_amount", 
                                                    "loan_to_value_ratio", "interest_rate", "loan_term",
                                                    "property_value", "income", "applicant_credit_score_type",
                                                    "applicant_sex", "applicant_age", "same_sex"))]

# Export data to csv
write.csv(data_final, "data_final.csv", row.names = FALSE)

