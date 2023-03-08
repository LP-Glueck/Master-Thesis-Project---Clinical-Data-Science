##### This scripts is a mixture of some processing steps such as:
# - missing value detection and removal through imputation (mice)
# - getting rid of character type features when we need them factorised
# - One-hot encoding of categorical variables after multiple imputation 
# Setting correct directory 

# Setting working directory 
getwd()
setwd("Documents/Uni/Internship/Data")

# installing packages 
install.packages("mice")
library(mice)
library(readxl)
library(dplyr)
library(MASS)

# reading data in 
data <- read.csv("trimmed_compiled_data_V2.csv")


# this segment is to deal with troubles from manually removing features in excel
# excel converts the numerics 2-5 to 2nd of May

data$eGFR <- factor(data$eGFR, 
                    levels = c(1, 
                               2, 
                               3, 
                               4, 
                               5, 
                               6), 
                    labels = c("> 90 ml/min", 
                               "60-89 ml/min", 
                               "30-59 ml/min", 
                               "15-29 ml/min", 
                               "< 15 ml/min", 
                               "unknown or measured more than 12 months ago"))

# data$eGFR <- ifelse(data$eGFR == "unknown or measured more than 12 months ago", 
#                     NA, 
#                     data$eGFR)

data$n_visits_out_all_12m_MUMC.ord <- ifelse(data$n_visits_out_all_12m_MUMC.ord == "02/May", 
                    "2-5", 
                    data$n_visits_out_all_12m_MUMC.ord)



# Removing the last 4 features related to the outcome variable
idx1 <- ncol(data)-3
idx2 <- ncol(data)
outcomes <- data[,idx1:idx2]
xdata <- data[,c(1:idx1-1)]

# Replacing string NAs with actual NAs.
xdata[xdata=="NA"] <- NA


# Gathering the proportions of missing values from the total samples. 
p_missing <- unlist(lapply(xdata, function(x) sum(is.na(x))))
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

# Define the features that need to be imputed
imputation_block <- c("eGFR", "person_responsible_for_medicines", "education", 
                      "allergy_medicine.yes_1", "inoutER_12m_external_hospital.yes_1", 
                      "in_all_12m_external_hospital.yes_1", "medicines_literacy")
# these two values need to be the same
stopifnot(length(imputation_block) == length(pm_sorted))


#### MICE imputation ###

mice_xdata <- mice(xdata, m = 10, method = c("polr",    # proportional odds ratio 
                                             "polyreg", # Polytomous logistic regression
                                             "polyreg", # Polytomous logistic regression
                                             "logreg",  # logistic regression (binary)
                                             "logreg",  # logistic regression (binary)
                                             "logreg",  # logistic regression (binary)
                                             "polyreg"),# Polytomous logistic regression
                   blocks = imputation_block, maxit = 10)

#### Uncomment to save or load the imputations ####
# # Saving the imputation
saveRDS(mice_xdata, "mice_imputations.rds")
# 
# # If necessary, load it in here 
# mice_xdata <- readRDS(file = "mice_imputations.rds")
#####                                          #### 

# plotting the imputations and the effects on the means and SD of the variables
plot(mice_xdata)

# checking out the imputations (more just for visually inspecting)
mice_xdata$imp$eGFR
mice_xdata$imp$person_responsible_for_medicines
mice_xdata$imp$education
mice_xdata$imp$allergy_medicine.yes_1
mice_xdata$imp$inoutER_12m_external_hospital.yes_1
mice_xdata$imp$in_all_12m_external_hospital.yes_1
mice_xdata$imp$medicines_literacy

# function to get the mode
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Extracting the completed data sets for all iterations (n=10)
completed_data <- lapply(1:10, function(i) complete(mice_xdata, i))

# Extracting the imputed values for every variable across all the iterations (n=10)
imputed_vars <- lapply(imputation_block, function(var) {
  sapply(completed_data, function(x) x[[var]])
})

# Getting the mode of imputed values for each variable 
vars_mode <- lapply(imputed_vars, function(var) {
  apply(var, 1, getmode)
})

imputed_data <- xdata

# Replace missing values with mode imputed values for each variable
for (i in seq_along(imputation_block)) {
  var <- imputation_block[i]
  xdata[var] <- vars_mode[[i]]
  }

## checking that there are no more missing values
sum(is.na(xdata))

xdata_filled <- xdata

# Let's save this imputed table now 
write.table(xdata_filled, file="imputed_data.csv", sep = ",", row.names = F)

# Loading it back in if we're resuming here
# xdata <- read.csv("imputed_data.csv") 

# installing necessary packages for next stage
install.packages('corrr')
library(corrr)

# Checking for character variables
numcol <- ncol(xdata)
for(i in 1:numcol) {
  xtype <- typeof(xdata[[i]])
  print(xtype)
}

# factorising the features again 
for(i in 1:numcol) {
  xtype <- typeof(xdata[[i]])
  if(xtype=='character') {
    a <- colnames(xdata[i])
    print(paste(a, " is character type")) 
    xdata[[i]] <- as.factor(unlist(xdata[,i]))}
}

# We're going to remove the categorical variables that we don't want to include 
# for the correlation analysis given that they have many categories
no_cat_data <- xdata
no_cat_data <- no_cat_data[,!grepl('high_risk_medicines',colnames(no_cat_data))]
no_cat_data <- no_cat_data[,!grepl('medicines_wo_prescription',colnames(no_cat_data))]
no_cat_data <- no_cat_data[,!grepl('current_diseases',colnames(no_cat_data))]
no_cat_data <- no_cat_data[,!grepl('out_specialty_current_MUMC.',colnames(no_cat_data))]
no_cat_data <- no_cat_data[,!grepl('inout_specialty_12m_external_hospital.',colnames(no_cat_data))]
no_cat_data <- no_cat_data[,!grepl('inout_specialty_12m_MUMC.',colnames(no_cat_data))]
no_cat_data <- no_cat_data[,!grepl('specialty_MUMC.',colnames(no_cat_data))]
no_cat_data <- no_cat_data[,!grepl('specialty_MUMC_external_hospital.',colnames(no_cat_data))]

# We can correlate the remaining features
data_cor <- correlate(no_cat_data)

# correlate function introduced a 'term' variable, but we want that as the row names
# the variable, record is redundant, so we remove it from the columns and rows.
data_cor <- data_cor[,!grepl('term',colnames(data_cor))]
data_cor <- data_cor[!grepl('record',colnames(data_cor)),!grepl('record',colnames(data_cor))]

data_cor <- as.data.frame(data_cor) # right now it's a tibble table, dataframes are slightly easier to work with 
rownames(data_cor) <- colnames(data_cor) # directing row names to have solumn names 

# Converting NAs to zeros 
data_cor[is.na(data_cor)] <- 0
corr_feat <- c() # the list of features to remove 
l <- 1 
for (i in 1:ncol(data_cor)){
  for (j in 1:nrow(data_cor)){
    if (abs(data_cor[i,j]) > 0.8){
      print(paste(colnames(data_cor[i]),"is correlated with",colnames(data_cor[j])))
      corr_feat[l] <- colnames(data_cor[i])
      l = l + 1
    }
  }
}

# Due to the nature of the loop and our correlation matrix, we'll remove the duplicated feauture names
corr_feat <- unique(corr_feat)
# From the highly correlated features we're going to keep:
corr_feat <- corr_feat[!grepl("centered", corr_feat)] # centered measures 
corr_feat <- corr_feat[!grepl("std", corr_feat)] # standard deviation measures 
corr_feat <- corr_feat[!grepl("housing", corr_feat)] # all housing categorical binary variables
corr_feat <- corr_feat[!grepl("person_responsible_for_medicines", corr_feat)] # its only correlated with a binary version of itself 
corr_feat <- corr_feat[!grepl("patient_reported_medicine_use", corr_feat)] # Same as above 

# Now we'll remove the remaining highly correlated features from corr_feat
data_filled = xdata[, !colnames(xdata) %in% corr_feat]

### Now we've removed the correlated features , so we can one-hot encode the categorical features that haven't been removed

### Now we need to one-hot encode the categorical variables that were imputed 

## person_responsible_for_medicines
# patient 
# partner or caregiver 
# health professional

data_filled$person_responsible_for_medicines.patient_1 <- ifelse(data_filled$person_responsible_for_medicines == "patient", 
                                                                       1, 
                                                                       0) 

data_filled$person_responsible_for_medicines.partner_or_caregiver_1 <- ifelse(data_filled$person_responsible_for_medicines == "partner or caregiver", 
                                                                                    1, 
                                                                                    0) 
data_filled$person_responsible_for_medicines.health_professional_1 <- ifelse(data_filled$person_responsible_for_medicines == "health professional", 
                                                                                   1,
                                                                                   0) 

table(data_filled$person_responsible_for_medicines, exclude = NULL)
prop.table(table(data_filled$person_responsible_for_medicines, exclude = NULL))

data_filled$person_responsible_for_medicines.partner_or_caregiver_or_health_professional_1 <- ifelse((data_filled$person_responsible_for_medicines == "partner or caregiver") 
                                                                                                           | 
                                                                                                             (data_filled$person_responsible_for_medicines == "health professional"), 
                                                                                                           1, 
                                                                                                           0)

prop.table(table(data_filled$person_responsible_for_medicines.partner_or_caregiver_or_health_professional_1, exclude = NULL))


## Housing 

data_filled$housing.independent_1 <- ifelse(data_filled$housing == "independent",
                                            1,
                                            0)
data_filled$housing.home_care_1 <- ifelse(data_filled$housing == "home care",
                                          1,
                                          0)
data_filled$housing.institution_1 <- ifelse(data_filled$housing == "institution",
                                            1,
                                            0)

data_filled$housing.home_care_or_institution_1 <-ifelse((data_filled$housing == "home care")
                                                        |
                                                          (data_filled$housing == "institution"),
                                                        1,
                                                        0)

## independent_composite

data_filled$independent_composite.yes_1 <- ifelse(is.na(data_filled$housing.independent_1)
                                           | # for patients with NA in housing independence OR NA in medication independence
                                             is.na(data_filled$person_responsible_for_medicines.patient_1),
                                           NA,
                                           0)
data_filled$independent_composite.yes_1 <- ifelse((data_filled$housing.independent_1 == 1)
                                           & # For all patients that are living independently AND taking medication themselves
                                             (data_filled$person_responsible_for_medicines.patient_1 == 1),
                                           1,
                                           data_filled$independent_composite.yes_1)

table(data_filled$independent_composite.yes_1, exclude = NULL)
prop.table(table(data_filled$independent_composite.yes_1, exclude = NULL))

## Education level 
# divided into 4 types 


data_filled$education.type_1_1 <- ifelse(data_filled$education == "type 1", 
                                               1, 
                                               0) 
data_filled$education.type_2_1 <- ifelse(data_filled$education == "type 2", 
                                               1, 
                                               0) 
data_filled$education.type_3_1 <- ifelse(data_filled$education == "type 3", 
                                               1, 
                                               0) 
data_filled$education.type_4_1 <- ifelse(data_filled$education == "type 4", 
                                               1, 
                                               0) 

table(data_filled$education, exclude = NULL)
prop.table(table(data_filled$education, exclude = NULL))

## Medicine literacy 
# divided into 3 scales

data_filled$medicines_literacy.adequate_1 <- ifelse(data_filled$medicines_literacy == "adequate", 
                                                          1, 
                                                          0)  
data_filled$medicines_literacy.suboptimal_1 <- ifelse(data_filled$medicines_literacy == "suboptimal", 
                                                            1, 
                                                            0)  
data_filled$medicines_literacy.insufficient_1 <- ifelse(data_filled$medicines_literacy == "insufficient", 
                                                              1, 
                                                              0)  

table(data_filled$medicines_literacy, exclude = NULL)
prop.table(table(data_filled$medicines_literacy, exclude = NULL))

data_filled$medicines_literacy.suboptimal_or_insufficient_1 <- ifelse((data_filled$medicines_literacy == "suboptimal")
                                                                            | # grouping any lower standards of med lit together
                                                                              (data_filled$medicines_literacy == "insufficient"),
                                                                            1,
                                                                            0)

## independent_inadequate_composite

data_filled$independent_inadequate_composite.yes_1 <- ifelse(is.na(data_filled$independent_composite.yes_1)
                                                      | # checking for NAs in the indep.comp variable OR med lit 
                                                        is.na(data_filled$medicines_literacy.adequate_1),
                                                      NA,
                                                      0)
data_filled$independent_inadequate_composite.yes_1 <- ifelse((data_filled$independent_composite.yes_1 == 1)
                                                      & # for indep. comp patients BUT INadequate med lit
                                                        (data_filled$medicines_literacy.adequate_1 == 0),
                                                      1,
                                                      data_filled$independent_inadequate_composite.yes_1)


## eGFR 
# We have 5 levels now 


data_filled$eGFR.above_90_1 <- ifelse(data_filled$eGFR == "> 90 ml/min", 
                                            1,
                                            0)
data_filled$eGFR.above_90_1 <- ifelse(data_filled$eGFR == "unknown or measured more than 12 months ago", 
                                            NA, 
                                            data_filled$eGFR.above_90_1)
data_filled$eGFR.60_89_1 <- ifelse(data_filled$eGFR == "60-89 ml/min", 
                                         1, 
                                         0)
data_filled$eGFR.60_89_1 <- ifelse(data_filled$eGFR == "unknown or measured more than 12 months ago", 
                                         NA, 
                                         data_filled$eGFR.60_89_1)
data_filled$eGFR.30_59_1 <- ifelse(data_filled$eGFR == "30-59 ml/min", 
                                         1, 
                                         0)
data_filled$eGFR.30_59_1 <- ifelse(data_filled$eGFR == "unknown or measured more than 12 months ago", 
                                         NA, 
                                         data_filled$eGFR.30_59_1)
data_filled$eGFR.15_29_1 <- ifelse(data_filled$eGFR == "15-29 ml/min", 
                                         1, 
                                         0)
data_filled$eGFR.15_29_1 <- ifelse(data_filled$eGFR == "unknown or measured more than 12 months ago", 
                                         NA, 
                                         data_filled$eGFR.15_29_1)
data_filled$eGFR.below_15_1 <- ifelse(data_filled$eGFR == "< 15 ml/min", 
                                            1,
                                            0)
data_filled$eGFR.below_15_1 <- ifelse(data_filled$eGFR == "unknown or measured more than 12 months ago", 
                                            NA, 
                                            data_filled$eGFR.below_15_1)
data_filled$eGFR.NA_1 <- ifelse(data_filled$eGFR == "unknown or measured more than 12 months ago", 
                                      1, 
                                      0)

table(data_filled$eGFR, exclude = NULL)
prop.table(table(data_filled$eGFR, exclude = NULL))

data_filled$eGFR.above_60_1 <- ifelse((data_filled$eGFR.60_89_1 == 1) 
                                            |
                                              (data_filled$eGFR.above_90_1 == 1),
                                            1,
                                            0)
data_filled$eGFR.below_60_1 <- ifelse((data_filled$eGFR.30_59_1 == 1)
                                            |
                                              (data_filled$eGFR.15_29_1 == 1)
                                            |
                                              (data_filled$eGFR.below_15_1 == 1),
                                            1, 
                                            0) 
data_filled$eGFR.below_30_1 <- ifelse((data_filled$eGFR.15_29_1 == 1)
                                            |
                                              (data_filled$eGFR.below_15_1 == 1),
                                            1,
                                            0) 

prop.table(table(data_filled$eGFR.below_15_1, exclude = NULL))
prop.table(table(data_filled$eGFR.60_89_1 == 1 | data_filled$eGFR.below_60_1 == 1 | data_filled$eGFR.below_15_1 == 0, exclude = NULL))
prop.table(table(data_filled$eGFR.above_90_1 == 1, exclude = NULL))

data_filled$out_specialty_current_MUMC.cardiology_1 <- ifelse(data_filled$out_specialty_current_MUMC == "cardiology",
                                                       1,
                                                       0)
data_filled$out_specialty_current_MUMC.surgery_1 <- ifelse(data_filled$out_specialty_current_MUMC == "surgery",
                                                    1,
                                                    0)
data_filled$out_specialty_current_MUMC.surgery_hpb_1 <- ifelse(data_filled$out_specialty_current_MUMC == "surgery: hepato-pancreato-biliary",
                                                        1,
                                                        0)
data_filled$out_specialty_current_MUMC.surgery_trauma_1 <- ifelse(data_filled$out_specialty_current_MUMC == "surgery: trauma",
                                                           1,
                                                           0)
data_filled$out_specialty_current_MUMC.surgery_vascular_1 <- ifelse(data_filled$out_specialty_current_MUMC == "surgery: vascular",
                                                             1,
                                                             0)
data_filled$out_specialty_current_MUMC.dermatology_1 <- ifelse(data_filled$out_specialty_current_MUMC == "dermatology",
                                                        1,
                                                        0)
data_filled$out_specialty_current_MUMC.internal_medicine_vascular_1 <- ifelse(data_filled$out_specialty_current_MUMC == "internal medicine: vascular",
                                                                       1,
                                                                       0)
data_filled$out_specialty_current_MUMC.internal_medicine_D_1 <- ifelse(data_filled$out_specialty_current_MUMC == "internal medicine: D",
                                                                1,
                                                                0)
data_filled$out_specialty_current_MUMC.internal_medicine_general_1 <- ifelse(data_filled$out_specialty_current_MUMC == "internal medicine: general",
                                                                      1,
                                                                      0)
data_filled$out_specialty_current_MUMC.internal_medicine_geriatrics_1 <- ifelse(data_filled$out_specialty_current_MUMC == "internal medicine: geriatrics",
                                                                         1,
                                                                         0)
data_filled$out_specialty_current_MUMC.otorhinolaryngology_1 <- ifelse(data_filled$out_specialty_current_MUMC == "otorhinolaryngology",
                                                                1,
                                                                0)
data_filled$out_specialty_current_MUMC.gastroenterology_ibd_1 <- ifelse(data_filled$out_specialty_current_MUMC == "gastroenterology: inflammatory bowel diseases",
                                                                 1,
                                                                 0)
data_filled$out_specialty_current_MUMC.gastroenterology_liver_1 <- ifelse(data_filled$out_specialty_current_MUMC == "gastroenterology: liver",
                                                                   1,
                                                                   0)
data_filled$out_specialty_current_MUMC.gastroenterology_neuro_1 <- ifelse(data_filled$out_specialty_current_MUMC == "gastroenterology: neuro",
                                                                   1,
                                                                   0)
data_filled$out_specialty_current_MUMC.neurology_vascular_1 <- ifelse(data_filled$out_specialty_current_MUMC == "neurology: vascular",
                                                               1,
                                                               0)
data_filled$out_specialty_current_MUMC.neurology_1 <- ifelse(data_filled$out_specialty_current_MUMC == "neurology",
                                                      1,
                                                      0)
data_filled$out_specialty_current_MUMC.orthopedics_1 <- ifelse(data_filled$out_specialty_current_MUMC == "orthopedics",
                                                        1,
                                                        0)
data_filled$out_specialty_current_MUMC.rheumatology_gout_1 <- ifelse(data_filled$out_specialty_current_MUMC == "rheumatology: gout",
                                                              1,
                                                              0)
data_filled$out_specialty_current_MUMC.rheumatology_spondyloarthritis_1 <- ifelse(data_filled$out_specialty_current_MUMC == "rheumatology: spondyloarthritis",
                                                                           1,
                                                                           0)
data_filled$out_specialty_current_MUMC.rheumatology_1 <- ifelse(data_filled$out_specialty_current_MUMC == "rheumatology",
                                                         1,
                                                         0)
data_filled$out_specialty_current_MUMC.urology_functional_1 <- ifelse(data_filled$out_specialty_current_MUMC == "urology: functional",
                                                               1,
                                                               0)
data_filled$out_specialty_current_MUMC.urology_oncology_1 <- ifelse(data_filled$out_specialty_current_MUMC == "urology: oncology",
                                                             1,
                                                             0)

table(data_filled$out_specialty_current_MUMC, exclude = NULL)
max(prop.table(table(data_filled$out_specialty_current_MUMC, exclude = NULL)))

data_filled$n_visits_out_current_specialty_12m_36m_MUMC.1_1 <- ifelse(data_filled$n_visits_out_current_specialty_12m_36m_MUMC.ord == 1, 
                                                                      1, 
                                                                      0)
data_filled$n_visits_out_current_specialty_12m_36m_MUMC.2_1 <- ifelse(data_filled$n_visits_out_current_specialty_12m_36m_MUMC.ord == 2, 
                                                                      1, 
                                                                      0)
data_filled$n_visits_out_current_specialty_12m_36m_MUMC.3_or_above_1 <- ifelse(data_filled$n_visits_out_current_specialty_12m_36m_MUMC.ord == 3, 
                                                                               1, 
                                                                               0)



# grouping surgeries together 
data_filled$out_specialty_current_MUMC.surgery_composite_1 <- ifelse((data_filled$out_specialty_current_MUMC.surgery_1 == 1)
                                                              |
                                                                (data_filled$out_specialty_current_MUMC.surgery_hpb_1 == 1)
                                                              |
                                                                (data_filled$out_specialty_current_MUMC.surgery_trauma_1 == 1)
                                                              |
                                                                (data_filled$out_specialty_current_MUMC.surgery_vascular_1 == 1),
                                                              1,
                                                              0)
# grouping internal medicine
data_filled$out_specialty_current_MUMC.internal_medicine_composite_1 <- ifelse((data_filled$out_specialty_current_MUMC.internal_medicine_vascular_1 == 1)
                                                                        |
                                                                          (data_filled$out_specialty_current_MUMC.internal_medicine_D_1 == 1)
                                                                        |
                                                                          (data_filled$out_specialty_current_MUMC.internal_medicine_general_1 == 1)
                                                                        |
                                                                          (data_filled$out_specialty_current_MUMC.internal_medicine_geriatrics_1 == 1),
                                                                        1,
                                                                        0)
# grouping Gastroentero patients
data_filled$out_specialty_current_MUMC.gastroenterology_composite_1 <- ifelse((data_filled$out_specialty_current_MUMC.gastroenterology_ibd_1 == 1)
                                                                       |
                                                                         (data_filled$out_specialty_current_MUMC.gastroenterology_liver_1 == 1)
                                                                       |
                                                                         (data_filled$out_specialty_current_MUMC.gastroenterology_neuro_1 == 1),
                                                                       1,
                                                                       0)
# grouping neurology patients
data_filled$out_specialty_current_MUMC.neurology_composite_1 <- ifelse((data_filled$out_specialty_current_MUMC.neurology_vascular_1 == 1)
                                                                |
                                                                  (data_filled$out_specialty_current_MUMC.neurology_1 == 1),
                                                                1,
                                                                0)

# grouping Rheumatology
data_filled$out_specialty_current_MUMC.rheumatology_composite_1 <- ifelse((data_filled$out_specialty_current_MUMC.rheumatology_gout_1 == 1)
                                                                   |
                                                                     (data_filled$out_specialty_current_MUMC.rheumatology_spondyloarthritis_1 == 1)
                                                                   |
                                                                     (data_filled$out_specialty_current_MUMC.rheumatology_1 == 1),
                                                                   1,
                                                                   0)
# grouping urology
data_filled$out_specialty_current_MUMC.urology_composite_1 <- ifelse((data_filled$out_specialty_current_MUMC.urology_functional_1 == 1)
                                                              |
                                                                (data_filled$out_specialty_current_MUMC.urology_oncology_1 == 1),
                                                              1,
                                                              0)
# making further subgroups of group A = 1320 patients
data_filled$out_specialty_current_MUMC.group_A_1 <- ifelse((data_filled$out_specialty_current_MUMC.cardiology_1 == 1)
                                                    |
                                                      (data_filled$out_specialty_current_MUMC.internal_medicine_composite_1 == 1)
                                                    |
                                                      (data_filled$out_specialty_current_MUMC.gastroenterology_composite_1 == 1)
                                                    |
                                                      (data_filled$out_specialty_current_MUMC.neurology_composite_1 == 1)
                                                    |
                                                      (data_filled$out_specialty_current_MUMC.rheumatology_composite_1 == 1),
                                                    1,
                                                    0)

# group B subgrouping = 677
data_filled$out_specialty_current_MUMC.group_B_1 <- ifelse((data_filled$out_specialty_current_MUMC.surgery_composite_1 == 1)
                                                    |
                                                      (data_filled$out_specialty_current_MUMC.orthopedics_1 == 1)
                                                    |
                                                      (data_filled$out_specialty_current_MUMC.urology_oncology_1 == 1),
                                                    1,
                                                    0)


# group C = 298
data_filled$out_specialty_current_MUMC.group_C_1 <- ifelse((data_filled$out_specialty_current_MUMC.dermatology_1 == 1)
                                                    |
                                                      (data_filled$out_specialty_current_MUMC.otorhinolaryngology_1 == 1)
                                                    |
                                                      (data_filled$out_specialty_current_MUMC.urology_functional_1 == 1),
                                                    1,
                                                    0)

## n_visits_out_all_12m_MUMC.ord

data_filled$n_visits_out_all_12m_MUMC.0_1 <- ifelse(data_filled$n_visits_out_all_12m_MUMC.ord == "0",
                                             1,
                                             0)
data_filled$n_visits_out_all_12m_MUMC.1_1 <- ifelse(data_filled$n_visits_out_all_12m_MUMC.ord == "1",
                                             1,
                                             0)
data_filled$n_visits_out_all_12m_MUMC.2_to_5_1 <- ifelse(data_filled$n_visits_out_all_12m_MUMC.ord == "2-5",
                                                  1,
                                                  0)
data_filled$n_visits_out_all_12m_MUMC.above_5_1 <- ifelse(data_filled$n_visits_out_all_12m_MUMC.ord == "> 5",
                                                   1,
                                                   0)

data_filled$inout_specialty_12m_MUMC.surgery_composite_1 <- ifelse((data_filled$inout_specialty_12m_MUMC.surgery_1 == 1)
                                                            |
                                                              (data_filled$inout_specialty_12m_MUMC.plastic_surgery_1 == 1),
                                                            1,
                                                            0)

## n_specialty_MUMC

data_filled$specialty_MUMC.cardiology_1 <- ifelse((data_filled$out_specialty_current_MUMC.cardiology_1 == 1)
                                           |
                                             (data_filled$inout_specialty_12m_MUMC.cardiology_1 == 1),
                                           1,
                                           0)
data_filled$specialty_MUMC.surgery_composite_1 <- ifelse((data_filled$out_specialty_current_MUMC.surgery_composite_1 == 1)
                                                  |
                                                    (data_filled$inout_specialty_12m_MUMC.surgery_composite_1 == 1),
                                                  1,
                                                  0)
data_filled$specialty_MUMC.dermatology_1 <- ifelse((data_filled$out_specialty_current_MUMC.dermatology_1 == 1)
                                            |
                                              (data_filled$inout_specialty_12m_MUMC.dermatology_1 == 1),
                                            1,
                                            0)
data_filled$specialty_MUMC.internal_medicine_composite_1 <- ifelse((data_filled$out_specialty_current_MUMC.internal_medicine_composite_1 == 1)
                                                            |
                                                              (data_filled$inout_specialty_12m_MUMC.internal_medicine_1 == 1),
                                                            1,
                                                            0)
data_filled$specialty_MUMC.otorhinolaryngology_1 <- ifelse((data_filled$out_specialty_current_MUMC.otorhinolaryngology_1 == 1)
                                                    |
                                                      (data_filled$inout_specialty_12m_MUMC.otorhinolaryngology_1 == 1),
                                                    1,
                                                    0)
data_filled$specialty_MUMC.gastroenterology_composite_1 <- ifelse((data_filled$out_specialty_current_MUMC.gastroenterology_composite_1 == 1)
                                                           |
                                                             (data_filled$inout_specialty_12m_MUMC.gastroenterology_1 == 1),
                                                           1,
                                                           0)
data_filled$specialty_MUMC.neurology_composite_1 <- ifelse((data_filled$out_specialty_current_MUMC.neurology_composite_1 == 1)
                                                    |
                                                      (data_filled$inout_specialty_12m_MUMC.neurology_1 == 1),
                                                    1,
                                                    0)
data_filled$specialty_MUMC.orthopedics_1 <- ifelse((data_filled$out_specialty_current_MUMC.orthopedics_1 == 1)
                                            |
                                              (data_filled$inout_specialty_12m_MUMC.orthopedics_1 == 1),
                                            1,
                                            0)
data_filled$specialty_MUMC.rheumatology_composite_1 <- ifelse((data_filled$out_specialty_current_MUMC.rheumatology_composite_1 == 1)
                                                       |
                                                         (data_filled$inout_specialty_12m_MUMC.rheumatology_1 == 1),
                                                       1,
                                                       0)
data_filled$specialty_MUMC.urology_composite_1 <- ifelse((data_filled$out_specialty_current_MUMC.urology_composite_1 == 1)
                                                  |
                                                    (data_filled$inout_specialty_12m_MUMC.urology_1 == 1),
                                                  1,
                                                  0)


# to see the overall number of specialists a patient has visited in past 12 months
data_filled$n_specialty_MUMC <- data_filled$specialty_MUMC.cardiology_1 +
  data_filled$specialty_MUMC.surgery_composite_1 +
  data_filled$specialty_MUMC.dermatology_1 +
  data_filled$specialty_MUMC.internal_medicine_composite_1 +
  data_filled$specialty_MUMC.otorhinolaryngology_1 +
  data_filled$specialty_MUMC.gastroenterology_composite_1 +
  data_filled$specialty_MUMC.neurology_composite_1 +
  data_filled$specialty_MUMC.orthopedics_1 +
  data_filled$specialty_MUMC.rheumatology_composite_1 +
  data_filled$specialty_MUMC.urology_composite_1 +
  data_filled$inout_specialty_12m_MUMC.psychiatry_1 +
  data_filled$inout_specialty_12m_MUMC.ophthalmology_1 +
  data_filled$inout_specialty_12m_MUMC.pneumology_1 +
  data_filled$inout_specialty_12m_MUMC.obstetrics_and_gynaecology_1 +
  data_filled$inout_specialty_12m_MUMC.pediatrics_1 +
  data_filled$inout_specialty_12m_MUMC.anesthesiology_1 +
  data_filled$inout_specialty_12m_MUMC.other_1

table(data_filled$n_specialty_MUMC, exclude = NULL)
prop.table(table(data_filled$n_specialty_MUMC, exclude = NULL))
summary(data_filled$n_specialty_MUMC)

data_filled$n_specialty_MUMC.centered <- data_filled$n_specialty_MUMC - mean(data_filled$n_specialty_MUMC)
data_filled$n_specialty_MUMC.std <- data_filled$n_specialty_MUMC.centered / sd(data_filled$n_specialty_MUMC)

## n_specialty_MUMC_external
# Here we're grouping together any visits to a certain speciliast, either at MUMC or external

data_filled$specialty_MUMC_external_hospital.cardiology_1 <- ifelse((data_filled$specialty_MUMC.cardiology_1 == 1)
                                                             |
                                                               (data_filled$inout_specialty_12m_external_hospital.cardiology_1 == 1),
                                                             1,
                                                             0)
data_filled$specialty_MUMC_external_hospital.surgery_composite_1 <- ifelse((data_filled$specialty_MUMC.surgery_composite_1 == 1)
                                                                    | 
                                                                      (data_filled$inout_specialty_12m_external_hospital.surgery_composite_1 == 1),
                                                                    1,
                                                                    0)
data_filled$specialty_MUMC_external_hospital.dermatology_1 <- ifelse((data_filled$specialty_MUMC.dermatology_1 == 1)
                                                              |
                                                                (data_filled$inout_specialty_12m_external_hospital.dermatology_1 == 1),
                                                              1,
                                                              0)
data_filled$specialty_MUMC_external_hospital.internal_medicine_composite_1 <- ifelse((data_filled$specialty_MUMC.internal_medicine_composite_1 == 1)
                                                                              |
                                                                                (data_filled$inout_specialty_12m_external_hospital.internal_medicine_1 == 1),
                                                                              1,
                                                                              0)
data_filled$specialty_MUMC_external_hospital.otorhinolaryngology_1 <- ifelse((data_filled$specialty_MUMC.otorhinolaryngology_1 == 1)
                                                                      |
                                                                        (data_filled$inout_specialty_12m_external_hospital.otorhinolaryngology_1 == 1),
                                                                      1,
                                                                      0)
data_filled$specialty_MUMC_external_hospital.gastroenterology_composite_1 <- ifelse((data_filled$specialty_MUMC.gastroenterology_composite_1 == 1)
                                                                             |
                                                                               (data_filled$inout_specialty_12m_external_hospital.gastroenterology_1 == 1),
                                                                             1,
                                                                             0)
data_filled$specialty_MUMC_external_hospital.neurology_composite_1 <- ifelse((data_filled$specialty_MUMC.neurology_composite_1 == 1)
                                                                      |
                                                                        (data_filled$inout_specialty_12m_external_hospital.neurology_1 == 1),
                                                                      1,
                                                                      0)
data_filled$specialty_MUMC_external_hospital.orthopedics_1 <- ifelse((data_filled$specialty_MUMC.orthopedics_1 == 1)
                                                              |
                                                                (data_filled$inout_specialty_12m_external_hospital.orthopedics_1 == 1),
                                                              1,
                                                              0)
data_filled$specialty_MUMC_external_hospital.rheumatology_composite_1 <- ifelse((data_filled$specialty_MUMC.rheumatology_composite_1 == 1)
                                                                         |
                                                                           (data_filled$inout_specialty_12m_external_hospital.rheumatology_1 == 1),
                                                                         1,
                                                                         0)
data_filled$specialty_MUMC_external_hospital.urology_composite_1 <- ifelse((data_filled$specialty_MUMC.urology_composite_1 == 1)
                                                                    |
                                                                      (data_filled$inout_specialty_12m_external_hospital.urology_1 == 1),
                                                                    1,
                                                                    0)
data_filled$specialty_MUMC_external_hospital.psychiatry_1 <- ifelse((data_filled$inout_specialty_12m_MUMC.psychiatry_1 == 1)
                                                             |
                                                               (data_filled$inout_specialty_12m_external_hospital.psychiatry_1 == 1),
                                                             1,
                                                             0)
data_filled$specialty_MUMC_external_hospital.ophthalmology_1 <- ifelse((data_filled$inout_specialty_12m_MUMC.ophthalmology_1 == 1)
                                                                |
                                                                  (data_filled$inout_specialty_12m_external_hospital.ophthalmology_1 == 1),
                                                                1,
                                                                0)
data_filled$specialty_MUMC_external_hospital.pneumology_1 <- ifelse((data_filled$inout_specialty_12m_MUMC.pneumology_1 == 1)
                                                             |
                                                               (data_filled$inout_specialty_12m_external_hospital.pneumology_1 == 1),
                                                             1,
                                                             0)
data_filled$specialty_MUMC_external_hospital.obstetrics_and_gynaecology_1 <- ifelse((data_filled$inout_specialty_12m_MUMC.obstetrics_and_gynaecology_1 == 1)
                                                                             |
                                                                               (data_filled$inout_specialty_12m_external_hospital.obstetrics_and_gynaecology_1 == 1),
                                                                             1,
                                                                             0)
data_filled$specialty_MUMC_external_hospital.pediatrics_1 <- ifelse((data_filled$inout_specialty_12m_MUMC.pediatrics_1 == 1)
                                                             |
                                                               (data_filled$inout_specialty_12m_external_hospital.pediatrics_1 == 1),
                                                             1,
                                                             0)
data_filled$specialty_MUMC_external_hospital.anesthesiology_1 <- ifelse((data_filled$inout_specialty_12m_MUMC.anesthesiology_1 == 1)
                                                                 |
                                                                   (data_filled$inout_specialty_12m_external_hospital.anesthesiology_1 == 1),
                                                                 1,
                                                                 0)
data_filled$specialty_MUMC_external_hospital.other_1 <- ifelse((data_filled$inout_specialty_12m_MUMC.other_1 == 1)
                                                        |
                                                          (data_filled$inout_specialty_12m_external_hospital.other_1 == 1),
                                                        1,
                                                        0)


data_filled$n_specialty_MUMC_external_hospital <- data_filled$specialty_MUMC_external_hospital.cardiology_1 +
  data_filled$specialty_MUMC_external_hospital.surgery_composite_1 +
  data_filled$specialty_MUMC_external_hospital.dermatology_1 +
  data_filled$specialty_MUMC_external_hospital.internal_medicine_composite_1 +
  data_filled$specialty_MUMC_external_hospital.otorhinolaryngology_1 +
  data_filled$specialty_MUMC_external_hospital.gastroenterology_composite_1 +
  data_filled$specialty_MUMC_external_hospital.neurology_composite_1 +
  data_filled$specialty_MUMC_external_hospital.orthopedics_1 +
  data_filled$specialty_MUMC_external_hospital.rheumatology_composite_1 +
  data_filled$specialty_MUMC_external_hospital.urology_composite_1 +
  data_filled$specialty_MUMC_external_hospital.psychiatry_1 +
  data_filled$specialty_MUMC_external_hospital.ophthalmology_1 +
  data_filled$specialty_MUMC_external_hospital.pneumology_1 +
  data_filled$specialty_MUMC_external_hospital.obstetrics_and_gynaecology_1 +
  data_filled$specialty_MUMC_external_hospital.pediatrics_1 +
  data_filled$specialty_MUMC_external_hospital.anesthesiology_1 +
  data_filled$specialty_MUMC_external_hospital.other_1

table(data_filled$n_specialty_MUMC_external_hospital, exclude = NULL)
prop.table(table(data_filled$n_specialty_MUMC_external_hospital, exclude = NULL))

data_filled$n_specialty_MUMC_external_hospital.centered <- data_filled$n_specialty_MUMC_external_hospital - mean(data_filled$n_specialty_MUMC_external_hospital)
data_filled$n_specialty_MUMC_external_hospital.std <- data_filled$n_specialty_MUMC_external_hospital.centered / sd(data_filled$n_specialty_MUMC_external_hospital)

### These should be all completed variables now


# Adding back the outcome variables to our imputed data now
maxcol <- ncol(data_filled)
data_filled[maxcol+1] <- outcomes[1]
data_filled[maxcol+2] <- outcomes[2]
data_filled[maxcol+3] <- outcomes[3]
data_filled[maxcol+4] <- outcomes[4]


# We'll save it again now 
write.table(data_filled, file="imputed_data_V4.csv", sep = ",", row.names = F)

