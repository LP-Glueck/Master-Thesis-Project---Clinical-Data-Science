##### This scripts is a mixture of some processing steps such as:
      # - missing value detection and removal through imputation (mice)
      # - getting rid of character type features when we need them factorised
      # - One-hot encoding of categorical variables after multiple imputation 

# Setting correct directory 
getwd()
setwd("Documents/Uni/Internship/Data")

# installing packages 
install.packages("mice")
library(mice)
library(readxl)
library(dplyr)

# reading out the data (latest & smallest version)
data <- read_excel("updated_compiled_data.xlsx")

# Removing the last 4 features related to the outcome variable
xdata <- data[1:169] 

# Replacing string NAs with actual NAs.
xdata[xdata=="NA"] <- NA

# Gathering the proportions of missing values from the total samples. 
p_missing <- unlist(lapply(xdata, function(x) sum(is.na(x))))/nrow(xdata)
pm_sorted <- sort(p_missing[p_missing > 0], decreasing = TRUE)
print(pm_sorted)

### Making sure the features are numeric or integers (factors) ### 
  # don't mind as long as they're not characters

numcol <- ncol(xdata)
# Converting all remaining variables that are character type to factors
for(i in 1:numcol) {
  xtype <- typeof(xdata[[i]])
    if(xtype=='character') {
      a <- colnames(xdata[i])
      print(paste(a, " is character type")) 
      xdata[[i]] <- as.factor(unlist(xdata[,i])) }
}


# Making sure that all features are either double or integers
for(i in 1:numcol) {
  xtype <- typeof(xdata[[i]]) 
  if(xtype != "double" && xtype != "integer") {
    print("character") }
}

features <- colnames(xdata)
print(tail(features)) # to make sure we can use tail to see that last few variables
# if you see a N_specialty_MUMC_external_hospital.std, great!  

# Define the features that need to be imputed
imputation_block <- c("eGFR", "person_responsible_for_medicines", "education", 
                      "allergy_medicine.yes_1", "inoutER_12m_external_hospital.yes_1", 
                      "in_all_12m_external_hospital.yes_1", "medicines_literacy")

length(imputation_block)
length(pm_sorted)
# these two values need to be the same

#### MICE imputation ####

mice_xdata <- mice(xdata, m = 5, method = c("polr",    # proportional odds ratio 
                                            "polyreg", # Polytomous logistic regression
                                            "polyreg", # Polytomous logistic regression
                                            "logreg",  # logistic regression (binary)
                                            "logreg",  # logistic regression (binary)
                                            "logreg",  # logistic regression (binary)
                                            "polyreg"),# Polytomous logistic regression
                   blocks = imputation_block, maxit = 5)
xdata_mice_filled <- complete(mice_xdata) # inserting the most optimal imputations

# calculating the propostion of missing values within the total samples per feature
proportion_missing <- unlist(lapply(xdata_mice_filled, function(x) sum(is.na(x))))
pm_sorted <- sort(proportion_missing[proportion_missing > 0], decreasing = TRUE) # ranking them
print(pm_sorted) # the output should let us know that no NAs remain

# As well doing a last check that we don't have character type features
numcol <- ncol(xdata_mice_filled)

for(i in 1:numcol) {
  xtype <- typeof(xdata_mice_filled[[i]])
  print(xtype)
}

# Let's save this imputed table now 
write.table(xdata_mice_filled, file="imputed_data.csv", sep = ",", row.names = F)

### Now we need to one-hot encode the categorical variables that were imputed 

## person_responsible_for_medicines
  # patient 
  # partner or caregiver 
  # health professional

xdata_mice_filled$person_responsible_for_medicines.patient_1 <- ifelse(xdata_mice_filled$person_responsible_for_medicines == "patient", 
                                                                       1, 
                                                                       0) 

xdata_mice_filled$person_responsible_for_medicines.partner_or_caregiver_1 <- ifelse(xdata_mice_filled$person_responsible_for_medicines == "partner or caregiver", 
                                                                                    1, 
                                                                                    0) 
xdata_mice_filled$person_responsible_for_medicines.health_professional_1 <- ifelse(xdata_mice_filled$person_responsible_for_medicines == "health professional", 
                                                                                  1,
                                                                                  0) 

table(xdata_mice_filled$person_responsible_for_medicines, exclude = NULL)
prop.table(table(xdata_mice_filled$person_responsible_for_medicines, exclude = NULL))

xdata_mice_filled$person_responsible_for_medicines.partner_or_caregiver_or_health_professional_1 <- ifelse((xdata_mice_filled$person_responsible_for_medicines == "partner or caregiver") 
                                                                                                           | 
                                                                                                             (xdata_mice_filled$person_responsible_for_medicines == "health professional"), 
                                                                                                           1, 
                                                                                                           0)

prop.table(table(xdata_mice_filled$person_responsible_for_medicines.partner_or_caregiver_or_health_professional_1, exclude = NULL))


## Education level 
  # divided into 4 types 


xdata_mice_filled$education.type_1_1 <- ifelse(xdata_mice_filled$education == "type 1", 
                                               1, 
                                               0) 
xdata_mice_filled$education.type_2_1 <- ifelse(xdata_mice_filled$education == "type 2", 
                                               1, 
                                               0) 
xdata_mice_filled$education.type_3_1 <- ifelse(xdata_mice_filled$education == "type 3", 
                                               1, 
                                               0) 
xdata_mice_filled$education.type_4_1 <- ifelse(xdata_mice_filled$education == "type 4", 
                                               1, 
                                               0) 

table(xdata_mice_filled$education, exclude = NULL)
prop.table(table(xdata_mice_filled$education, exclude = NULL))


## Medicine literacy 

xdata_mice_filled$medicines_literacy.adequate_1 <- ifelse(xdata_mice_filled$medicines_literacy == "adequate", 
                                             1, 
                                             0)  
xdata_mice_filled$medicines_literacy.suboptimal_1 <- ifelse(xdata_mice_filled$medicines_literacy == "suboptimal", 
                                               1, 
                                               0)  
xdata_mice_filled$medicines_literacy.insufficient_1 <- ifelse(xdata_mice_filled$medicines_literacy == "insufficient", 
                                                 1, 
                                                 0)  

table(xdata_mice_filled$medicines_literacy, exclude = NULL)
prop.table(table(xdata_mice_filled$medicines_literacy, exclude = NULL))

xdata_mice_filled$medicines_literacy.suboptimal_or_insufficient_1 <- ifelse((xdata_mice_filled$medicines_literacy == "suboptimal")
                                                               | # grouping any lower standards of med lit together
                                                                 (xdata_mice_filled$medicines_literacy == "insufficient"),
                                                               1,
                                                               0)

## eGFR 

xdata_mice_filled$eGFR.above_90_1 <- ifelse(xdata_mice_filled$eGFR == "> 90 ml/min", 
                                            1,
                                            0)
xdata_mice_filled$eGFR.above_90_1 <- ifelse(xdata_mice_filled$eGFR == "unknown or measured more than 12 months ago", 
                                            NA, 
                                            xdata_mice_filled$eGFR.above_90_1)
xdata_mice_filled$eGFR.60_89_1 <- ifelse(xdata_mice_filled$eGFR == "60-89 ml/min", 
                                         1, 
                                         0)
xdata_mice_filled$eGFR.60_89_1 <- ifelse(xdata_mice_filled$eGFR == "unknown or measured more than 12 months ago", 
                                         NA, 
                                         xdata_mice_filled$eGFR.60_89_1)
xdata_mice_filled$eGFR.30_59_1 <- ifelse(xdata_mice_filled$eGFR == "30-59 ml/min", 
                                         1, 
                                         0)
xdata_mice_filled$eGFR.30_59_1 <- ifelse(xdata_mice_filled$eGFR == "unknown or measured more than 12 months ago", 
                                         NA, 
                                         xdata_mice_filled$eGFR.30_59_1)
xdata_mice_filled$eGFR.15_29_1 <- ifelse(xdata_mice_filled$eGFR == "15-29 ml/min", 
                                         1, 
                                         0)
xdata_mice_filled$eGFR.15_29_1 <- ifelse(xdata_mice_filled$eGFR == "unknown or measured more than 12 months ago", 
                                         NA, 
                                         xdata_mice_filled$eGFR.15_29_1)
xdata_mice_filled$eGFR.below_15_1 <- ifelse(xdata_mice_filled$eGFR == "< 15 ml/min", 
                                            1,
                                            0)
xdata_mice_filled$eGFR.below_15_1 <- ifelse(xdata_mice_filled$eGFR == "unknown or measured more than 12 months ago", 
                                            NA, 
                                            xdata_mice_filled$eGFR.below_15_1)
xdata_mice_filled$eGFR.NA_1 <- ifelse(xdata_mice_filled$eGFR == "unknown or measured more than 12 months ago", 
                                      1, 
                                      0)

table(xdata_mice_filled$eGFR, exclude = NULL)
prop.table(table(xdata_mice_filled$eGFR, exclude = NULL))

xdata_mice_filled$eGFR.above_60_1 <- ifelse((xdata_mice_filled$eGFR.60_89_1 == 1) 
                                            |
                                              (xdata_mice_filled$eGFR.above_90_1 == 1),
                                            1,
                                            0)
xdata_mice_filled$eGFR.below_60_1 <- ifelse((xdata_mice_filled$eGFR.30_59_1 == 1)
                                            |
                                              (xdata_mice_filled$eGFR.15_29_1 == 1)
                                            |
                                              (xdata_mice_filled$eGFR.below_15_1 == 1),
                                            1, 
                                            0) 
xdata_mice_filled$eGFR.below_30_1 <- ifelse((xdata_mice_filled$eGFR.15_29_1 == 1)
                                            |
                                              (xdata_mice_filled$eGFR.below_15_1 == 1),
                                            1,
                                            0) 

prop.table(table(xdata_mice_filled$eGFR.below_15_1, exclude = NULL))
prop.table(table(xdata_mice_filled$eGFR.60_89_1 == 1 | xdata_mice_filled$eGFR.below_60_1 == 1 | xdata_mice_filled$eGFR.below_15_1 == 0, exclude = NULL))
prop.table(table(xdata_mice_filled$eGFR.above_90_1 == 1, exclude = NULL))

# Adding back the outcome variables to our imputed data now
xdata_mice_filled[191] <- data[,170]
xdata_mice_filled[192] <- data[,171]
xdata_mice_filled[193] <- data[,172]
xdata_mice_filled[194] <- data[,173]

# We'll save it again now 
write.table(xdata_mice_filled, file="imputed_data_V2.csv", sep = ",", row.names = F)
