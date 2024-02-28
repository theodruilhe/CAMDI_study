# LIBRAIRIES IMPORTATION
library(dplyr)
library(glm2)
library(viridis)
library(fastDummies)

# DATA IMPORTATION
initial_data <- read.csv("data_michigan.csv", header = TRUE, sep = ",")
data_final <- read.csv("data_final.csv", header = TRUE, sep = ",")

#### Descriptive Statistic ####
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


