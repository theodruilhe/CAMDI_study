# LIBRAIRIES IMPORTATION
library(dplyr)
library(glm2)
library(viridis)


## DATA IMPORTATION
data <- read.csv("data_michigan.csv", header = TRUE, sep = ",")
data_temp <- data
## CLEANING USELESS COLUMN ON A NEW DATAFRAME 'data_temp'
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
                                                   'aus.4', 'aus.5', 'denial_reason.4',"conforming_loan_limit", 'lien_status', 'open.end_line_of_credit', 
                                                   'intro_rate_period'
))]

## CLEANING LINES
table(data_temp$aus.1)

# Cleaning conforming_loan_limit
#data_temp$conforming_loan_limit <- ifelse(data_temp$conforming_loan_limit == "C", 1, 0)

# Cleaning action_taken and creating deny
table(data_temp$action_taken)
barplot(table(data_temp$action_taken))
data_temp <- data_temp %>%
  filter(action_taken %in% c(1, 2, 3))
table(data_temp$action_taken)
data_temp <- data_temp %>%
  mutate(deny = ifelse(action_taken == 3, 1, 0))
prop.table(table(data_temp$action_taken))
table(data_temp$deny)
prop.table(table(data_temp$deny))

# 2,1 to 1,0 for construction_method
data_temp$construction_method <- ifelse(data_temp$construction_method == 2, 1, 
                                        ifelse(data_temp$construction_method == 1, 0, 
                                               data_temp$construction_method))

summary(data_temp$income)
# dummy female (by applicant_sex)
# 0 = Male, 1 = Female
data_temp <- data_temp %>%
  filter(data_temp$applicant_sex %in% c(1, 2))
table(data_temp$applicant_sex)
data_temp$applicant_sex <- ifelse(data_temp$applicant_sex == 1, 0, 
                                  ifelse(data_temp$applicant_sex == 2, 1, 
                                         data_temp$applicant_sex))
table(data_temp$applicant_sex)
# dummy female (by co_applicant_sex)
data_temp <- data_temp %>%
  filter(co.applicant_sex %in% c(1, 2, 5))
data_temp$co.applicant_sex <- ifelse(data_temp$co.applicant_sex == 1, 0, 
                                     ifelse(data_temp$co.applicant_sex == 2, 1,data_temp$co.applicant_sex))
table(data_temp$co.applicant_sex)

# Il reste 5 si il n'y a pas de co-applicant

# Supprimer les lignes selon les conditions spécifiées
#data_temp <- subset(data_temp, derived_msa.md != 0 & derived_msa.md != 99999
data_temp <- subset(data_temp, !derived_race %in% c("Race Not Available", "Joint", "2 or more minority races", "Free Form Text Only"))
data_temp <- subset(data_temp, loan_purpose != 5)
# on ne garde que les prêts 'Not primarily for a business or commercial purpose' (individuel)
table(data_temp$business_or_commercial_purpose)
data_temp <- subset(data_temp, business_or_commercial_purpose == 2)
table(data_temp$business_or_commercial_purpose)
data_temp <- data_temp[, !(names(data_temp) %in% c('business_or_commercial_purpose'))]
data_temp <- subset(data_temp, debt_to_income_ratio != "Exempt" & debt_to_income_ratio != "NA")
data_temp <- subset(data_temp, applicant_age != 8888)
table(data_temp$applicant_age)
data_temp <- subset(data_temp, !(tract_minority_population_percent == 0 |
                                   ffiec_msa_md_median_family_income == 0 |
                                   tract_to_msa_income_percentage == 0 |
                                   tract_owner_occupied_units == 0 |
                                   tract_one_to_four_family_homes == 0 |
                                   tract_median_age_of_housing_units == 0))




## COMMENTAIRE SUR LA BASE

# est ce qu'on garde : activity_year, state_code, derived_race,lien status
# reponse NON

# pas sûr de comment recoder de manière interessante:
# loan_type , loan_purpose, open.end_line_of_credit (si on s'en sert on vire 111)
# business_or_commercial_purpose (on vire 111 si on s'en sert), interest_rate (on vire NA si on utilise)
# loan_term (in month??), occupancy_type (mettre en dummy?), debt_to_income_ratio, applicant_age
# co_applicant_age

# super interessant les salaires/age/minority par rapport aux tract à la fin de la base de données
# à voir comment interpréter

# regarder les modeles de credit dans la litterature pour savoir quelles variables inclures

# the bank try to maximize its profit (yi = indic (xi beta + epsi) expliquer)

# 0. rappeler sujet/question
# 1. model sous la forme indicatrice 
# 2. presenter sample size, representztif etc,which state
# 3. presenter main variables


#### Descriptive Statistic ####

## TARGET VARIABLE: deny
table(data_temp$deny)
prop.table(table(data_temp$deny))
barplot(table(data_temp$deny))
barplot(prop.table(table(data_temp$deny))*100, 
        main = "Distribution of the target variable Deny",
        ylab = "Percentage",
        col = viridis(5),
        legend = c("Loan accepted", "Application denied"),
        names.arg = sprintf("%s\n%.2f%%", names(prop.table(table(data_temp$deny)) * 100), prop.table(table(data_temp$deny)) * 100))


## Derived Race
table(data_temp$derived_race)
prop.table(table(data_temp$derived_race))
barplot(table(data_temp$derived_race))
barplot(prop.table(table(data_temp$derived_race))*100, 
        main = "Distribution of Type", 
        ylab = "Percentage",
        col = viridis(5),
        args.legend = list(x = "topleft", title = "Legend"),
        names.arg = sprintf("%s\n%.2f%%", names(prop.table(table(data_temp$derived_race)) * 100), prop.table(table(data_temp$derived_race)) * 100))
# que faire des American Indian ou Alaska Native et des Native Hawaiian ou Other Pacific Islander ?


## Loan purpose
#Values:
#1 - Home purchase
#2 - Home improvement
#31 - Refinancing
#32 - Cash-out refinancing
#4 - Other purpose
table(data_temp$loan_purpose)
barplot(table(data_temp$loan_purpose))
barplot(prop.table(table(data_temp$loan_purpose))*100, 
        main = "Distribution of Loan purpose",
        ylab = "Percentage",
        col = viridis(5),
        legend = c("1. Home purchase","2. Home improvement", "4. Refinancing", "31. Cash-out refinancing", "32. Other purpose"),
        names.arg = sprintf("%s\n%.2f%%", names(prop.table(table(data_temp$loan_purpose)) * 100), prop.table(table(data_temp$loan_purpose)) * 100))

## Loan amount
summary(data_temp$loan_amount)
hist(data_temp$loan_amount, breaks = 50)
# on a des valeurs aberrantes, on va les supprimer
data_temp <- subset(data_temp, loan_amount < 1000000)
summary(data_temp$loan_amount)
hist(data_temp$loan_amount, breaks = 50)


## Loan to value ratio
sum(is.na(data_temp$loan_to_value_ratio))
# beaucoup de NA mais variabble intéressante je conseille de supprimer les NA
data_temp <- subset(data_temp, !is.na(loan_to_value_ratio))
sum(is.na(data_temp$loan_to_value_ratio))
data_temp$loan_to_value_ratio <- as.numeric(data_temp$loan_to_value_ratio)
summary(data_temp$loan_to_value_ratio)
hist(data_temp$loan_to_value_ratio, breaks = 50)
data_temp <- subset(data_temp, loan_to_value_ratio < 150)
hist(data_temp$loan_to_value_ratio, breaks = 50)
summary(data_temp$loan_to_value_ratio)


## Interest Rate
#summary(data_temp$interest_rate)
#table(data_temp$deny)
#sum(is.na(data_temp$interest_rate))
# beaucoup de NA mais variable intéressante je conseille de supprimer les NA
#data_temp <- subset(data_temp, !is.na(interest_rate))
#sum(is.na(data_temp$interest_rate))
#data_temp$interest_rate <- as.numeric(data_temp$interest_rate)
#summary(data_temp$interest_rate)
#hist(data_temp$interest_rate, breaks = 50)

#hist(data_temp$interest_rate, breaks = 30, probability = TRUE, 
#       main = "Histogram in Density", 
#      xlab = "Interest Rate",
#     ylim = c(0, 0.30))
#lines(density(data_temp$interest_rate), col = "blue", lwd = 2)


## Loan term
# The number of months after which the legal obligation will mature or terminate, 
#or would have matured or terminated
summary(data_temp$loan_term)
data_temp <- subset(data_temp, !is.na(loan_term))
sum(is.na(data_temp$loan_term))
data_temp$loan_term <- as.numeric(data_temp$loan_term)
summary(data_temp$loan_term)
hist(data_temp$loan_term, breaks = 400)
# c'est presuqe une variable discrète
sorted_table <- sort(table(data_temp$loan_term), decreasing = TRUE)
# Extraire les valeurs les plus fréquentes
top_values <- names(sorted_table[1:7])
# Afficher les valeurs les plus fréquentes
print(top_values)
data_temp$loan_term <- ifelse(data_temp$loan_term %in% top_values, data_temp$loan_term, "other")
table(data_temp$loan_term)
barplot(table(data_temp$loan_term))


## Balloon payment
# 1 - Balloon payment
# 2 - No balloon payment
table(data_temp$balloon_payment)
barplot(table(data_temp$balloon_payment))

## Property value
summary(data_temp$property_value)
# as numeric
data_temp$property_value <- as.numeric(data_temp$property_value)
summary(data_temp$property_value)
# Na ?
sum(is.na(data_temp$property_value))
# supprimer les NA
data_temp <- subset(data_temp, !is.na(property_value))
sum(is.na(data_temp$property_value))
summary(data_temp$property_value)
hist(data_temp$property_value, breaks = 50)
# valeurs aberrantes
data_temp <- subset(data_temp, property_value < 1000000)
hist(data_temp$property_value, breaks = 50)
# histogram en densite
hist(data_temp$property_value, breaks = 30, probability = TRUE, 
     main = "Histogram in Density", 
     xlab = "Property Value")
lines(density(data_temp$property_value), col = "blue", lwd = 2)

## Construction method
# 1 - Site-built
# 2 - Manufactured home
table(data_temp$construction_method)
barplot(table(data_temp$construction_method))
# pas très intéressant


## Occupancy type
# 1 - Principal residence
# 2 - Second residence
# 3 - Investment property
table(data_temp$occupancy_type)
barplot(table(data_temp$occupancy_type))
# une très grande majorité de principal residence


## Income
summary(data_temp$income)
# NA?
sum(is.na(data_temp$income))
# supprimer les NA
data_temp <- subset(data_temp, !is.na(income))
sum(is.na(data_temp$income))
summary(data_temp$income)
hist(data_temp$income, breaks = 50)
# valeurs aberrantes
data_temp <- subset(data_temp, income < 500)
summary(data_temp$income)
hist(data_temp$income, breaks = 50)
# histogram en densite
hist(data_temp$income, breaks = 50, probability = TRUE, 
     main = "Histogram in Density", 
     xlab = "Income")
lines(density(data_temp$income), col = "blue", lwd = 2)


## Debt to income ratio
summary(data_temp$debt_to_income_ratio)
# na ?
sum(is.na(data_temp$debt_to_income_ratio))
# supprimer les NA
data_temp2 <- subset(data_temp, !is.na(debt_to_income_ratio))
# as numeric
data_temp$debt_to_income_ratio <- as.numeric(data_temp$debt_to_income_ratio)


## Applicant credit score type
# NA ?
sum(is.na(data_temp$applicant_credit_score_type))
table(data_temp$applicant_credit_score_type)
barplot(table(data_temp$applicant_credit_score_type))


## Applicant sex
table(data_temp$applicant_sex)
barplot(table(data_temp$applicant_sex))


## Denial reason
table(data_temp$denial_reason.1)
# supprimer la colonne denial reason
data_temp <- data_temp[, !(names(data_temp) %in% c('denial_reason.1'))]
data_temp <- data_temp[, !(names(data_temp) %in% c('denial_reason.2'))]
data_temp <- data_temp[, !(names(data_temp) %in% c('denial_reason.3'))]



### creer la base final1 avec les variables qui nous intéressent 
colnames(data_temp)
data_final <- data_temp[, (names(data_temp) %in% c('deny', 'derived_race', 'loan_purpose', 'loan_amount',
                                                   'loan_to_value_ratio', 'loan_term',
                                                   'loan_to_value_ratio', 'property_value','income',
                                                   'applicant_sex', 'applicant_age'))]

data_final$derived_race <- as.factor(data_final$derived_race)
summary(data_final)
# Export data to csv
write.csv(data_temp, "data_final.csv", row.names = FALSE)

## Descriptive Statistics for presentatios
table(data_temp$deny)
prop.table(table(data_temp$deny))
barplot(table(data_temp$deny))
barplot(prop.table(table(data_temp$deny))*100, 
        main = "Distribution of the target variable Deny",
        ylab = "Percentage",
        col = viridis(5),
        legend = c("Application accepted (193.5k)", "Application denied (40.5k)"),
        names.arg = sprintf(
          "%s\n%.2f%%", 
          names(prop.table(table(data_temp$deny)) * 100), 
          prop.table(table(data_temp$deny)) * 100))

table_counts <- table(data_temp$deny)

# Calculate percentages
percentages <- prop.table(table_counts) * 100


## Derived Race
table(data_temp$derived_race)

barplot(table(data_temp$derived_race))
barplot(prop.table(table(data_temp$derived_race))*100, 
        main = "Distribution of Type", 
        ylab = "Percentage",
        col = viridis(10),
        args.legend = list(x = "topleft", title = "Legend"),
        names.arg = sprintf("%s\n%.2f%%", names(prop.table(table(data_temp$derived_race)) * 100), prop.table(table(data_temp$derived_race)) * 100))


summary(data_final)

summary(data_final[, (names(data_final) %in% c('loan_amount', 'loan_to_value_ratio', 'loan_term',
                                               'loan_to_value_ratio', 'property_value','income'))])


### Model
colnames(data_final)
data_final$derived_race_ref <- as.factor(data_final$derived_race)

# Réorganiser les niveaux de la variable qualitative (exemple : "White" comme référence)
data_final$derived_race_ref <- relevel(data_final$derived_race_ref, ref = "White")

X <- model.matrix(~ derived_race_ref + loan_amount + income + applicant_sex,  data = data_final)

model <- glm(deny ~ X, data = data_final, family = "binomial")
#model <- glm(deny ~ loan_amount + income + property_value + applicant_sex, data = data_final, family = "binomial", maxit = 10000)
summary(model)

# Make logistic regression on deny, I work on data_final
data_final$applicant_sex <- as.factor(data_final$applicant_sex)
data_final$property_value <- scale(data_final$property_value)
data_final$loan_amount <- scale(data_final$loan_amount)
data_final$income <- scale(data_final$income)
data_final$loan_to_value_ratio <- scale(data_final$loan_to_value_ratio)
data_final$interest_rate <- scale(data_final$interest_rate)

# Logistic regression
colnames(data_final)
model <- glm(deny ~ loan_amount + interest_rate + income + property_value
             + applicant_sex, data = data_final, family = "binomial", maxit = 10000)

summary(model)
summary(data_final)


sampling_data <- data_final[sample(nrow(data_final), 5000, replace = FALSE), ]
model <- glm(deny ~ income, data = sampling_data, family = "binomial")

table(data_final$deny)


model <- glm(deny ~ loan_amount + interest_rate + income + property_value
             + applicant_sex, data = data_final, family = "binomial", maxit = 10000)
summary(model)