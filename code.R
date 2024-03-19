#############################################################
#                                                           #
#                     !!READ ME!!                           #
#                     !!READ ME!!                           #
#                     !!READ ME!!                           #
#                                                           #
#############################################################
#                                                           #
#         OUR CODE IS COMPOSED OF 3 PARTS THAT ARE:         #
#                   1) data_cleaning                        #
#                   2) descriptive_statistics               #
#                   3) model                                #
#         THEY WERE MADE TO BE EXECUTED IN THIS ORDER,      #
#         SO THAT THE CODE CAN EXECUTE WITHOUT ERRORS       #
#                                                           #
#                                                           #
##############################################################


##### SET WORKING DIRECTORY

setwd("path/to/your/directory")


sink("log_file.txt")

# LIBRAIRIES IMPORTATION
# Function to install and load a package
install_load_package <- function(package_name) {
  if (!requireNamespace(package_name, quietly = TRUE)) {
    install.packages(package_name)
  }
  library(package_name, character.only = TRUE)
}

# List of packages to install and load
packages <- c("dplyr", "glm2", "viridis", "car", "caret", "margins", "stargazer")

# Install and load each package from the list
for (package in packages) {
  install_load_package(package)
}

#######################################
#           DATA CLEANING             #
#######################################


### DATA IMPORTATION
data <- read.csv("data_michigan.csv", header = TRUE, sep = ",")

## Create a data_temp in order to clean the dataset without modifying the original one
data_temp <- data

#### CLEANING LINES ####
# In this part, we aim to clean the lines of our dataset. It could be by deleting the missing values, 
# or by modifying the values of the variables (ex: Male --> 1, Female --> 0).


## TARGET VARIABLE CREATION
# In variable action_taken only values:
#1 - Loan originated
#2 - Application approved but not accepted (the bank approved the loan application but the applicant decided to not take it)
#3 - Application denied
# are interesting because they allow us to discern if the application is a deny or not, the rest doesn't give informations 
# on the status of the application (or the decision is not taken yet)

data_temp <- data_temp %>%
  filter(action_taken %in% c(1, 2, 3))
# creating of our target variable 'deny'
# Deny = 1 if action_taken = 3 (see above), else it's 0.
data_temp <- data_temp %>%
  mutate(deny = ifelse(action_taken == 3, 1, 0))


## SEX, we only keep the two possibility 'Men' and 'Women' because there is some application
# where the information is not given
# 1 = Male, 2 = Female
data_temp <- data_temp %>%
  filter(applicant_sex %in% c(1, 2))


## RACE
# We now delete race that we are not interested to study, or not given (such as 'Race Not Available)
data_temp <- subset(data_temp, !derived_race %in% c("Race Not Available", "Joint", "2 or more minority races", "Free Form Text Only"))

# We gather derived race to have only 4 categories: White, Black, Asian and Native
# Means that we put all the possible values for native together to a simplest categorie called 'Native'
# Same for 'Black or African American that we call 'Black'
data_temp$derived_race <- ifelse(data_temp$derived_race %in% c("American Indian or Alaska Native", "Native Hawaiian or Other Pacific Islander"), "Native",
                                 ifelse(data_temp$derived_race == "Black or African American","Black", data_temp$derived_race))


## LOAN_PURPOSE
# We delete the lines with not-define loan_purposes (it doesn't represent a lot of lines) and so, we will be able
# use this variable with no-missing values.
# Then, we delete 5 = Not applicable, because it is not interesting as it doesn't give any information
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
# This allows to deal with outliers and to focus the study on the most-common applicant (really rich people with
# high income are not in the dataset anymore)
data_temp <- subset(data_temp, loan_amount < 1000000)


## PROPERTY VALUE
# As for the loan amount we will only consider property value lower than 1 million for the same reasons as above
data_temp$property_value <- as.numeric(data_temp$property_value)
data_temp <- subset(data_temp, property_value < 1000000)


## LOAN TO VALUE RATIO
# converting values to be numeric, so it can be interpreted as a numeric variable in the models
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

# We only keep the variable of interest
data_final <- data_temp[, (names(data_temp) %in%  c("deny", "derived_race", "loan_purpose",
                                                    "loan_amount", "loan_term", "property_value",
                                                    "income", "applicant_sex", "applicant_age"))]

# We export the CSV, so it will be importable on 'model.R' and 'descriptive_statistics.R' files.
# Export data to csv
write.csv(data_final, "data_final.csv", row.names = FALSE)


#######################################
#     DESCRIPTIVE STATISTICS          #
#######################################

##################################################
#################### REPRESENTATIVNESS ##########

# For our variables of interest, we compare the characteristics of the population vs the sample (proportions, medians)
# the initial data has sometimes to be cleaned to get rid of NA : we rename it "test" (a subset of "data")

###### Race

prop.table(table(data_final$derived_race))

# removing observations with another ethnicity than the one we will consider
test <- subset(data, !(derived_race %in% c("2 or more minority races", "Free Form Text Only", "Joint", "Race Not Available")))

prop.table(table(test$derived_race))
# for example here, the proportions have not changed so much (asian goes from 0.036452594 to 0.035253925 and
# white from 0.860843297 to 0.869935590), thus we repeat this analysis on the others variables.

###### Sex

test <- subset(data, 
               !(applicant_sex %in% c(3,4,6)))

prop.table(table(test$applicant_sex))

prop.table(table(data_final$applicant_sex))

###### Loan amount

test <- data
summary(test$loan_amount)
summary(data_final$loan_amount)

###### Income

test <- subset(data, is.na(income) != TRUE)

summary(test$income)

summary(data_final$income)

###### Age

test <- subset(data, applicant_age != 8888)

prop.table(table(test$applicant_age))

prop.table(table(data_final$applicant_age))

##################################################
#################### EXPLORATORY STATISTICS ##########

## We will here display some statistics of our variables in the final dataset.

#### UNIVARIATE STATISTICS AND PLOT ####

###### LaTeX code for the table of descriptive statistics of numerical variables 
stargazer(data_final,digits = 2)

## BARPLOT OF TARGET VARIABLE: DENY
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
hist(data_final$loan_amount, breaks = 50,xlab='Loan Amount')

## LOAN TERM
# The number of months after which the legal obligation will mature or terminate, or would have matured or terminated
summary(data_final$loan_term)
hist(data_final$loan_term, breaks = 50,xlab='Loan Term')


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


## APPLICANT SEX
# 1: Male
# 2: Female
table(data_final$applicant_sex)
barplot(table(data_final$applicant_sex),legend='1 for Male, 2 for Female')


## APPLICANT AGE

# to reorganize the categories (">74" years was before "25-34" on the bar plot) we capture manually the percentages and draw a new barplot
age_groups <- c("<25", "25-34", "35-44", "45-54", "55-64", "65-74", ">74")
proportions <- c(0.03934274, 0.21150511, 0.22421216, 0.20981369, 0.17215811, 0.10475308, 0.03821512)

color_palette <- colorRampPalette(c("skyblue", "blue"))(7) # colors for the barplot
barplot(proportions, names.arg = age_groups, xlab = "Age Groups", ylab = "Proportions", col =color_palette)



#### BIVARIATE STATISTICS #### 

# Visualize the relationship between income and derived_race with a boxplot
ggplot(data_final, aes(x=derived_race, y=income)) + 
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Income by Derived Race", x = "Derived Race", y = "Income")

# Ensure 'applicant_sex' is treated as a categorical variable
data_final$applicant_sex <- as.factor(data_final$applicant_sex)

# Visualize the relationship between income and applicant_sex with a boxplot
ggplot(data_final, aes(x=applicant_sex, y=income)) + 
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Income by Applicant Sex", x = "Applicant Sex", y = "Income")



#######################################
#              MODEL                  #
#######################################


# QUALITATIVE VARIABLES MANIPULATION
# We convert them as factor to be used in the models.
data_final$derived_race <- as.factor(data_final$derived_race)
data_final$loan_purpose <- as.factor(data_final$loan_purpose)
data_final$applicant_age <- as.factor(data_final$applicant_age)
data_final$deny <- as.factor(data_final$deny)
data_final$applicant_sex <- as.factor(data_final$applicant_sex)

# The levels of a factor are re-ordered so that the level specified by ref is first and the others are moved down.
data_final$derived_race <- relevel(data_final$derived_race, ref = "White")
data_final$loan_purpose <- relevel(data_final$loan_purpose, ref = "1")
data_final$applicant_sex <- relevel(data_final$applicant_sex, ref = "1")
data_final$applicant_age <- relevel(data_final$applicant_age, ref = "35-44")
data_final$deny <- relevel(data_final$deny, ref = "0")

# QUANTITATIVE VARIABLES MANIPULATION
data_final$loan_amount <- as.numeric(data_final$loan_amount)
data_final$loan_term <- as.numeric(data_final$loan_term)
data_final$property_value <- as.numeric(data_final$property_value)

# We set income to be in $ instead of thousands of dollars
data_final$income <- data_final$income * 1000

# ADD THE LOG OF THE QUANTITATIVE VARIABLES
data_final$log_income <- log(data_final$income)
data_final$log_loan_amount <- log(data_final$loan_amount)
data_final$log_property_value <- log(data_final$property_value)

### CORRELATION TEST ###
cor.test(data_final$loan_amount,data_final$income) # significant correlation and 0.42
cor.test(data_final$loan_amount,data_final$property_value) # significant correlation and 0.61
cor.test(data_final$income,data_final$property_value) # significant correlation and 0.58
# WE HAVE A LOT OF CORRELATION AMONG OUR NUMERIC VARIABLES
# We aim to check for multicollinearity among our explanatory variables then we will run a VIF test


#######################################
#                VIF                  #
#######################################

# put all the explanatory variables together
model_vif <- glm(deny ~ derived_race + loan_purpose + log_loan_amount + loan_term + 
                   log_property_value + log_income + applicant_sex + applicant_age, data = data_final, family = "binomial")
summary(model_vif)

# Calculate VIFs for all explanatory variables
vif_result <- vif(model_vif)

print(vif_result)

# We do not have multicolinearity as the maximum VIF is 2.702, thus it is not required to deal
# with multicolinearity to create our models.

#######################################
#           MODEL CREATION            #
#######################################

# We will now create our model, to do so, we will create a train_set and
# and a test_set, so at the end, we will be able to test the accuracy of the model

# We set seed for reproductibility
set.seed(123)

# Calculate the number of rows to include in the training set
training_rows <- floor(0.8 * nrow(data_final))

# Create a random sample of row indices for the training set
train_indices <- sample(seq_len(nrow(data_final)), size = training_rows)

# Split the data into training and testing sets using the indices
train_set <- data_final[train_indices, ]
test_set <- data_final[-train_indices, ]

### Model ###

# Add  control variables
model_2 <- glm(deny ~ derived_race + applicant_age + log_income + log_loan_amount 
               + loan_purpose + loan_term  + log_property_value + applicant_sex,
               data = train_set,
               family = "binomial")
summary(model_2)

coef_2 <- coefficients(model_2)
coef_2 <- coef_2[!is.na(coef_2)]
coef_2

#######################################
#       ACCURACY OF THE MODEL         #
#######################################
probabilities <- predict(model_2, newdata = test_set, type = "response")
# Converting probabilities to binary outcomes based on a 0.5 threshold
predicted_outcomes <- ifelse(probabilities > 0.5, 1, 0)

# Actual outcomes
actual_outcomes <- test_set$deny

# Creating a confusion matrix
conf_matrix <- table(Predicted = predicted_outcomes, Actual = actual_outcomes)

# Calculating accuracy from the confusion matrix
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)

# Print the confusion matrix and accuracy
print(conf_matrix)
# to have the latex code
print(paste("Accuracy:", accuracy))
# We obtain approximately 83% of accuracy


#######################################
#        INTERPRETATION OF            #
#           THE RESULTS               #
#######################################

### MARGINS EFFECTS ###
# Calculate marginal effects
marg_eff <- margins(model_2)

# Summary of marginal effects
summary_me <- summary(marg_eff)
summary(marg_eff)
## Doing a boxplot of the marginal effects ##
summary_me <- data.frame(
  factor = c('age<25', 'applicant_age>74', 'age between 25-34', 'age between 45-54', 
             'age between 55-64', 'age between 65-74', 'Woman', 'Asian', 
             'Black', 'Native', 'Home Improvement', 'Refinancing', 
             'Cash-Out Refinancing', 'Ohter Purpose', 'Loan term', 'log(income)', 'log(loan_amount)', 
             'log(property_value)'),
  AME = c(-0.0232, -0.0293, -0.0200, -0.0004, -0.0224, -0.0372, -0.0110, 0.0690, 
          0.1168, 0.0678, 0.2323, 0.1182, 0.1193, 0.2612, 0.0003, -0.0893, 0.0061, -0.0418),
  lower = c(-0.0329, -0.0371, -0.0252, -0.0054, -0.0274, -0.0427, -0.0143, 0.0584, 
            0.1103, 0.0478, 0.2248, 0.1127, 0.1149, 0.2533, 0.0003, -0.0924, 0.0030, -0.0458),
  upper = c(-0.0134, -0.0215, -0.0147, 0.0046, -0.0173, -0.0316, -0.0077, 0.0797, 
            0.1233, 0.0878, 0.2398, 0.1237, 0.1237, 0.2692, 0.0003, -0.0862, 0.0091, -0.0378)
)

# Order factors by the absolute size of AME for plotting
summary_me$factor <- factor(summary_me$factor, levels = summary_me$factor[order(abs(summary_me$AME))])

# Create a plot
ggplot(summary_me, aes(x = factor, y = AME)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  coord_flip() +  # Flip the coordinates to make the plot horizontal
  labs(y = "Average Marginal Effect (AME)", x = "Factor", title = "Marginal Effects with Confidence Intervals") +
  theme_bw() + # Use a black and white theme
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())  # Remove grid lines


### USA ###

# get the map of the USA
us_states_map <- map_data("state")

##### READ ME ########
#o avoid having to retrieve the data from the 50 states and rerun this code 50 times 
#we provide you with our "final_value.csv" file which presents the Average Marginal Effect associated with the variable Black, 
#for each state in order to be able to visualize it on the map.
final_values <- read.csv("final_value.csv", sep = ";")


us_states_map <- merge(us_states_map, final_values, by = "region")

ggplot(data = us_states_map, aes(x = long, y = lat, group = group, fill = value)) +
  geom_polygon(color = "white") +
  scale_fill_gradient(low = "white", high = "red", limits = c(0, max(final_values$value))) +
  theme_void() +
  coord_fixed(1.3) +
  guides(fill = guide_colorbar(title = "AME"))

sink()
