# PACKAGES ################################################################

library(readxl)
library(ggplot2)

# DATA ################################################################
getwd()
setwd("/Users/liamg/Documents/Uni/Internship/Data")
data <- read_excel("./QZImportFinal2-Anoniem.xlsx", skip = 4) # Error refers to question not included in "determinantenlijst". "...66" is a column without any entries.
colnames(data)[1] <- "id"
table(is.na(data$id))
data <- data[data$id != "7EGpgbdhnP", ] # Has been checked, data entry error.
# when looking at the data, most columns are empty for this patient. 
data <- data[!c((data$id == "kO/DEMRWwJ") & (data$SPEC == "REUM SPA")), ] # Has been checked, data entry error.
data <- data[!c((data$id == "6FuHuP52e9") & (data$SPEC == "MDL-IBD")), ] # Has been checked, data entry error.
# no info for these two patient in combination with those specific specialties. 

dup_id <- data[(duplicated(data$id, fromLast = FALSE) == TRUE) | duplicated(data$id, fromLast = TRUE) == TRUE,]$id
#View(subset(data, (id %in% dup_id)))
data <- data[!c((data$id == "RWhAI8VyRg") & (data$SPEC == "NEU VASC")), ] # 2nd visit.
data <- data[!c((data$id == "+VwiZPm8lX") & (data$SPEC == "UROL ONCO")), ] # 2nd visit.
data <- data[!c((data$id == "a+/hkR+SLB") & (data$SPEC == "UROL FU")), ] # 2nd visit.
data <- data[!c((data$id == "2QFpSTWBcc") & (data$SPEC == "CARDIO")), ] # 2nd visit.
data <- data[!c((data$id == "SrixwIf+Xb") & (data$SPEC == "MDL NGM")), ] # 2nd visit.
data <- data[!c((data$id == "tqsHn6eVH+") & (data$SPEC == "INTERN GER")), ] # 2nd visit.
data <- data[!c((data$id == "apeIkxgLow") & (data$SPEC == "UROL FU")), ] # 2nd visit.
data <- data[!c((data$id == "Ybz9mB54S/") & (data$SPEC == "REUMA")), ] # 2nd visit.
data <- data[!c((data$id == "icPvB/wJqm") & (data$SPEC == "INTERN ALG")), ] # 2nd visit.
data <- data[!c((data$id == "icPvB/wJqm") & (data$SPEC == "UROL FU")), ] # 3rd visit.
data <- data[!c((data$id == "DIj75C1KaE") & (data$SPEC == "ORTHO")), ] # 2nd visit.
data <- data[!c((data$id == "9WNYBZm5+T") & (data$SPEC == "INTERN GER")), ] # 2nd visit.
data <- data[!c((data$id == "yOrpVMj8LB") & (data$SPEC == "MDL-IBD")), ] # 2nd visit.
data <- data[!c((data$id == "A0pW4TS3pV") & (data$SPEC == "UROL FU")), ] # 2nd visit.
data <- data[!c((data$id == "rc75mgq0uR") & (data$SPEC == "REUM SPA")), ] # 2nd visit.
data <- data[!c((data$id == "1bLYsdNwyJ") & (data$SPEC == "MDL NGM")), ] # 2nd visit.
data <- data[!c((data$id == "HLkzz6SpkZ") & (data$SPEC == "UROL FU")), ] # 2nd visit.
data <- data[!c((data$id == "j/WhD66npk") & (data$SPEC == "MDL NGM")), ] # 2nd visit.
data <- data[!c((data$id == "Iy68G3IisW") & (data$SPEC == "MDL NGM")), ] # 2nd visit.
data <- data[!c((data$id == "2oNzMJDTeO") & (data$SPEC == "ORTHO")), ] # 2nd visit.
data <- data[!c((data$id == "DUzC4IENg4") & (data$SPEC == "UROL FU")), ] # 2nd visit.
data <- data[!c((data$id == "NJIvDpH2nF") & (data$SPEC == "CHI")), ] # 2nd visit.
data <- data[!c((data$id == "azHk1mtt7Q") & (data$SPEC == "INTE VASC")), ] # 2nd visit.
data$record <- 1:nrow(data)


# VARIABLES ################################################################

## age

names(data)[names(data) == "A2a"] <- "age"
data$age.centered <- data$age - mean(data$age)
data$age.std <- data$age.centered / sd(data$age)


summary(data$age)
sd(data$age)
ggplot(data, aes(x = age)) + 
  geom_histogram(binwidth = 1) + 
  scale_x_continuous(name = "age in years",
                     breaks = seq(0, 100, 10),
                     labels = seq(0, 100, 10)) +
  theme_minimal() + 
  geom_vline(xintercept=mean(data$age), color = 'red')


names(data)[names(data) == "A1a"] <- "gender.male_1"
data$gender.male_1 <- ifelse(data$gender.male_1 == 2,
                             0, 
                             data$gender.male_1)

# checking proportions of gender
table(data$gender.male_1, exclude = NULL)
prop.table(table(data$gender.male_1, exclude = NULL))


## housing

names(data)[names(data) == "A3b"] <- "housing"
data$housing <- ordered(data$housing, 
                        levels = c(1, 
                                   2, 
                                   3), 
                        labels = c("independent", 
                                   "home care", 
                                   "institution"))
# data$housing.independent_1 <- ifelse(data$housing == "independent", 
#                                      1, 
#                                      0) 
# data$housing.home_care_1 <- ifelse(data$housing == "home care", 
#                                    1, 
#                                    0) 
# data$housing.institution_1 <- ifelse(data$housing == "institution", 
#                                      1, 
#                                      0) 

# table(data$housing, exclude = NULL)
# prop.table(table(data$housing, exclude = NULL))
# 
# data$housing.home_care_or_institution_1 <-ifelse((data$housing == "home care")
#                                                  |
#                                                    (data$housing == "institution"),
#                                                  1,
#                                                  0)


## person_responsible_for_medicines
names(data)[names(data) == "A5b"] <- "person_responsible_for_medicines"
data$person_responsible_for_medicines <- factor(data$person_responsible_for_medicines, 
                                                levels = c(1, 
                                                           2, 
                                                           3), 
                                                labels = c("patient", 
                                                           "partner or caregiver", 
                                                           "health professional"))

table(data$person_responsible_for_medicines, exclude = NULL)
prop.table(table(data$person_responsible_for_medicines, exclude = NULL))

## medicines_literacy

names(data)[names(data) == "A7c"] <- "medicines_literacy"
data$medicines_literacy <- factor(data$medicines_literacy, 
                                  levels = c(1, 
                                             2, 
                                             3), 
                                  labels = c("adequate", 
                                             "suboptimal", 
                                             "insufficient"))

table(data$medicines_literacy, exclude = NULL)
prop.table(table(data$medicines_literacy, exclude = NULL))

## education

names(data)[names(data) == "A6b"] <- "education"
data$education <- ordered(data$education, 
                          levels = c(1, 
                                     2, 
                                     3, 
                                     4), 
                          labels = c("type 1",
                                     "type 2", 
                                     "type 3",
                                     "type 4"))

table(data$education, exclude = NULL)
prop.table(table(data$education, exclude = NULL))

## current_diseases 

current_diseases.list <- strsplit(data$A10a, ",") # Assumption NA equals to negative indication.
data$current_diseases.diabetes_1 <- ifelse(lapply(current_diseases.list, function(x) { "1" %in% x}),
                                           1,
                                           0)
data$current_diseases.rheumatoid_arthritis_1 <- ifelse(lapply(current_diseases.list, function(x) { "2" %in% x}),
                                                       1,
                                                       0)
data$current_diseases.asthma_COPD_1 <- ifelse(lapply(current_diseases.list, function(x) { "3" %in% x}),
                                              1,
                                              0)
data$current_diseases.cardiovascular_disease_1 <- ifelse(lapply(current_diseases.list, function(x) { "4" %in% x}),
                                                         1,
                                                         0)
data$current_diseases.cardiac_arrhythmia_1 <- ifelse(lapply(current_diseases.list, function(x) { "5" %in% x}),
                                                     1,
                                                     0)
data$current_diseases.heart_failure_1 <- ifelse(lapply(current_diseases.list, function(x) { "6" %in% x}),
                                                1,
                                                0)
data$current_diseases.cancer_1 <- ifelse(lapply(current_diseases.list, function(x) { "7" %in% x}),
                                         1,
                                         0)
data$current_diseases.none_of_the_above_1 <- ifelse(lapply(current_diseases.list, function(x) { "8" %in% x}),
                                                    1,
                                                    0)
## number of current_diseases 

names(data)[names(data) == "A9a"] <- "n_current_diseases"
data$n_current_diseases <- ifelse(is.na(data$n_current_diseases),
                                  0,
                                  data$n_current_diseases)  # Assumption NA equals to negative indication.

table(data$n_current_diseases, exclude = NULL)
prop.table(table(data$n_current_diseases, exclude = NULL))
summary(data$n_current_diseases)
sd(data$n_current_diseases, na.rm = TRUE)
ggplot(data, aes(x = n_current_diseases)) +
  geom_bar() +
  scale_x_continuous(breaks = seq(0, 6, 1),
                     labels = seq(0, 6, 1)) +
  theme_minimal() 

data$n_current_diseases.centered <- data$n_current_diseases - mean(data$n_current_diseases) 
data$n_current_diseases.std <- data$n_current_diseases.centered / sd(data$n_current_diseases)

## eGFR

names(data)[names(data) == "A12a"] <- "eGFR"
data$eGFR <- ifelse(is.na(data$eGFR), 
                    6, 
                    data$eGFR)
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

data$eGFR <- ifelse(data$eGFR == "unknown or measured more than 12 months ago", 
                    NA, 
                    data$eGFR)

## n_prescribed_medicines

names(data)[names(data) == "B1a"] <- "n_prescribed_medicines"

table(data$n_prescribed_medicines, exclude = NULL)
prop.table(table(data$n_prescribed_medicines, exclude = NULL))
summary(data$n_prescribed_medicines)
sd(data$n_prescribed_medicines)
ggplot(data, aes(x = n_prescribed_medicines)) +
  geom_bar() +
  scale_x_continuous(breaks = seq(0, 20, 1),
                     labels = seq(0, 20, 1)) +
  theme_minimal() 

data$n_prescribed_medicines.centered <- data$n_prescribed_medicines - mean(data$n_prescribed_medicines)
data$n_prescribed_medicines.std <- data$n_prescribed_medicines.centered / sd(data$n_prescribed_medicines)

## high_risk_medicines

high_risk_medicines.list <- strsplit(data$B7a, ",") # Assumption NA equals to negative indication.
data$high_risk_medicines.platelet_aggregation_inhibitors_1 <- ifelse(lapply(high_risk_medicines.list, function(x) { "1" %in% x }),
                                                                     1,
                                                                     0)
data$high_risk_medicines.anticoagulants_1 <- ifelse(lapply(high_risk_medicines.list, function(x) { "2" %in% x }),
                                                    1,
                                                    0)
data$high_risk_medicines.NSAIDs_1 <-ifelse(lapply(high_risk_medicines.list, function(x) { "3" %in% x }),
                                           1,
                                           0)
data$high_risk_medicines.diuretics_1 <- ifelse(lapply(high_risk_medicines.list, function(x) { "4" %in% x }),
                                               1,
                                               0)
data$high_risk_medicines.RAS_inhibitors_1 <- ifelse(lapply(high_risk_medicines.list, function(x) { "5" %in% x }),
                                                    1,
                                                    0)
data$high_risk_medicines.systemic_corticosteroids_1 <- ifelse(lapply(high_risk_medicines.list, function(x) { "6" %in% x }),
                                                              1,
                                                              0)
data$high_risk_medicines.opioids_1 <- ifelse(lapply(high_risk_medicines.list, function(x) { "7" %in% x }),
                                             1,
                                             0)
data$high_risk_medicines.glucose_lowering_medicines_1 <- ifelse(lapply(high_risk_medicines.list, function(x) { "8" %in% x }),
                                                                1,
                                                                0)
data$high_risk_medicines.psychotropics_1 <- ifelse(lapply(high_risk_medicines.list, function(x) { "9" %in% x }),
                                                   1,
                                                   0)
data$high_risk_medicines.cardiac_medicines_1 <- ifelse(lapply(high_risk_medicines.list, function(x) { "10" %in% x }),
                                                       1,
                                                       0)
data$high_risk_medicines.immunosuppressants_1 <- ifelse(lapply(high_risk_medicines.list, function(x) { "11" %in% x }),
                                                        1,
                                                        0)
data$high_risk_medicines.oncolytics_1 <- ifelse(lapply(high_risk_medicines.list, function(x) { "12" %in% x }),
                                                1,
                                                0)

## medicines_wo_prescription

names(data)[names(data) == "B2b"] <- "medicines_wo_prescription.list" # Assumption NA equals to negative indication.
medicines_wo_prescription.list <- strsplit(data$medicines_wo_prescription.list, ",")
data$medicines_wo_prescription.NSAIDs_1 <- ifelse(lapply(medicines_wo_prescription.list, function(x) { "1" %in% x }),
                                                  1,
                                                  0)
data$medicines_wo_prescription.proton_pump_inhibitors_1 <- ifelse(lapply(medicines_wo_prescription.list, function(x) { "2" %in% x }),
                                                                  1,
                                                                  0)
data$medicines_wo_prescription.hypericum_1 <- ifelse(lapply(medicines_wo_prescription.list, function(x) { "3" %in% x }),
                                                     1,
                                                     0)
data$medicines_wo_prescription.red_yeast_rice_1 <- ifelse(lapply(medicines_wo_prescription.list, function(x) { "4" %in% x }),
                                                          1,
                                                          0)
data$medicines_wo_prescription.multi_vitamins_dietary_supplement_1 <- ifelse(lapply(medicines_wo_prescription.list, function(x) { "5" %in% x }),
                                                                             1,
                                                                             0)
data$medicines_wo_prescription.other_1 <- ifelse(lapply(medicines_wo_prescription.list, function(x) { "6" %in% x }),
                                                 1,
                                                 0)
data$medicines_wo_prescription.none_1 <- ifelse(lapply(medicines_wo_prescription.list, function(x) { "7" %in% x }),
                                                1,
                                                0)

## patient_reported_medicine_use.yes_1

names(data)[names(data) == "A21b"] <- "patient_reported_medicine_use.yes_1"
data$patient_reported_medicine_use.yes_1 <- ifelse(data$patient_reported_medicine_use.yes_1 == 2, 
                                                   0, 
                                                   data$patient_reported_medicine_use.yes_1)

table(data$patient_reported_medicine_use.yes_1, exclude = NULL)
prop.table(table(data$patient_reported_medicine_use.yes_1, exclude = NULL))

## pill_box.yes_1

names(data)[names(data) == "A4b"] <- "pill_box.yes_1"
data$pill_box.yes_1 <- ifelse(data$pill_box.yes_1 == 2,
                              0, 
                              data$pill_box.yes_1)

table(data$pill_box.yes_1, exclude = NULL)
prop.table(table(data$pill_box.yes_1, exclude = NULL))

## allergy_medicine.yes_1

names(data)[names(data) == "A13b"] <- "allergy_medicine.yes_1"
data$allergy_medicine.yes_1 <- ifelse(data$allergy_medicine.yes_1 == 2, 
                                      0, 
                                      data$allergy_medicine.yes_1)

table(data$allergy_medicine.yes_1, exclude = NULL)
prop.table(table(data$allergy_medicine.yes_1, exclude = NULL))

## out_specialty_current_MUMC

data$out_specialty_current_MUMC <- data$SPEC
data$out_specialty_current_MUMC <- factor(data$out_specialty_current_MUMC, 
                                          levels = c("CARDIO", 
                                                     "CHI", 
                                                     "CHI HPB",
                                                     "CHI TRAU",
                                                     "CHI VAAT",
                                                     "DERMA",
                                                     "INTE VASC",
                                                     "INTERN-D", 
                                                     "INTERN ALG", 
                                                     "INTERN GER",
                                                     "KNO-ALG",
                                                     "MDL-IBD",
                                                     "MDL-Lever",
                                                     "MDL NGM",
                                                     "NEU VASC",
                                                     "NEURO",
                                                     "ORTHO",
                                                     "REUM JICHT",
                                                     "REUM SPA",
                                                     "REUMA",
                                                     "UROL FU",
                                                     "UROL ONCO"), 
                                          labels = c("cardiology",
                                                     "surgery",
                                                     "surgery: hepato-pancreato-biliary", 
                                                     "surgery: trauma",
                                                     "surgery: vascular", 
                                                     "dermatology",
                                                     "internal medicine: vascular",
                                                     "internal medicine: D", 
                                                     "internal medicine: general",
                                                     "internal medicine: geriatrics",
                                                     "otorhinolaryngology",
                                                     "gastroenterology: inflammatory bowel diseases",
                                                     "gastroenterology: liver",
                                                     "gastroenterology: neuro",
                                                     "neurology: vascular",
                                                     "neurology",
                                                     "orthopedics",
                                                     "rheumatology: gout",
                                                     "rheumatology: spondyloarthritis",
                                                     "rheumatology",
                                                     "urology: functional",
                                                     "urology: oncology"))
table(data$out_specialty_current_MUMC, exclude = NULL)
max(prop.table(table(data$out_specialty_current_MUMC, exclude = NULL)))

# data$out_specialty_current_MUMC.cardiology_1 <- ifelse(data$out_specialty_current_MUMC == "cardiology", 
#                                                        1, 
#                                                        0)
# data$out_specialty_current_MUMC.surgery_1 <- ifelse(data$out_specialty_current_MUMC == "surgery", 
#                                                     1, 
#                                                     0)
# data$out_specialty_current_MUMC.surgery_hpb_1 <- ifelse(data$out_specialty_current_MUMC == "surgery: hepato-pancreato-biliary", 
#                                                         1, 
#                                                         0)
# data$out_specialty_current_MUMC.surgery_trauma_1 <- ifelse(data$out_specialty_current_MUMC == "surgery: trauma", 
#                                                            1, 
#                                                            0)
# data$out_specialty_current_MUMC.surgery_vascular_1 <- ifelse(data$out_specialty_current_MUMC == "surgery: vascular", 
#                                                              1, 
#                                                              0)
# data$out_specialty_current_MUMC.dermatology_1 <- ifelse(data$out_specialty_current_MUMC == "dermatology", 
#                                                         1, 
#                                                         0)
# data$out_specialty_current_MUMC.internal_medicine_vascular_1 <- ifelse(data$out_specialty_current_MUMC == "internal medicine: vascular", 
#                                                                        1, 
#                                                                        0)
# data$out_specialty_current_MUMC.internal_medicine_D_1 <- ifelse(data$out_specialty_current_MUMC == "internal medicine: D", 
#                                                                 1, 
#                                                                 0)
# data$out_specialty_current_MUMC.internal_medicine_general_1 <- ifelse(data$out_specialty_current_MUMC == "internal medicine: general", 
#                                                                       1, 
#                                                                       0)
# data$out_specialty_current_MUMC.internal_medicine_geriatrics_1 <- ifelse(data$out_specialty_current_MUMC == "internal medicine: geriatrics", 
#                                                                          1, 
#                                                                          0)
# data$out_specialty_current_MUMC.otorhinolaryngology_1 <- ifelse(data$out_specialty_current_MUMC == "otorhinolaryngology",
#                                                                 1,
#                                                                 0)
# data$out_specialty_current_MUMC.gastroenterology_ibd_1 <- ifelse(data$out_specialty_current_MUMC == "gastroenterology: inflammatory bowel diseases",
#                                                                  1,
#                                                                  0)
# data$out_specialty_current_MUMC.gastroenterology_liver_1 <- ifelse(data$out_specialty_current_MUMC == "gastroenterology: liver",
#                                                                    1,
#                                                                    0)
# data$out_specialty_current_MUMC.gastroenterology_neuro_1 <- ifelse(data$out_specialty_current_MUMC == "gastroenterology: neuro",
#                                                                    1,
#                                                                    0)
# data$out_specialty_current_MUMC.neurology_vascular_1 <- ifelse(data$out_specialty_current_MUMC == "neurology: vascular",
#                                                                1,
#                                                                0)
# data$out_specialty_current_MUMC.neurology_1 <- ifelse(data$out_specialty_current_MUMC == "neurology", 
#                                                       1, 
#                                                       0)
# data$out_specialty_current_MUMC.orthopedics_1 <- ifelse(data$out_specialty_current_MUMC == "orthopedics",
#                                                         1,
#                                                         0)
# data$out_specialty_current_MUMC.rheumatology_gout_1 <- ifelse(data$out_specialty_current_MUMC == "rheumatology: gout",
#                                                               1,
#                                                               0)
# data$out_specialty_current_MUMC.rheumatology_spondyloarthritis_1 <- ifelse(data$out_specialty_current_MUMC == "rheumatology: spondyloarthritis",
#                                                                            1,
#                                                                            0)
# data$out_specialty_current_MUMC.rheumatology_1 <- ifelse(data$out_specialty_current_MUMC == "rheumatology",
#                                                          1,
#                                                          0)
# data$out_specialty_current_MUMC.urology_functional_1 <- ifelse(data$out_specialty_current_MUMC == "urology: functional",
#                                                                1,
#                                                                0)
# data$out_specialty_current_MUMC.urology_oncology_1 <- ifelse(data$out_specialty_current_MUMC == "urology: oncology",
#                                                              1,
#                                                              0)

table(data$out_specialty_current_MUMC, exclude = NULL)
max(prop.table(table(data$out_specialty_current_MUMC, exclude = NULL)))

# grouping surgeries together 
# data$out_specialty_current_MUMC.surgery_composite_1 <- ifelse((data$out_specialty_current_MUMC.surgery_1 == 1) 
#                                                               | 
#                                                                 (data$out_specialty_current_MUMC.surgery_hpb_1 == 1) 
#                                                               | 
#                                                                 (data$out_specialty_current_MUMC.surgery_trauma_1 == 1) 
#                                                               | 
#                                                                 (data$out_specialty_current_MUMC.surgery_vascular_1 == 1), 
#                                                               1, 
#                                                               0)
# # grouping internal medicine 
# data$out_specialty_current_MUMC.internal_medicine_composite_1 <- ifelse((data$out_specialty_current_MUMC.internal_medicine_vascular_1 == 1)
#                                                                         |
#                                                                           (data$out_specialty_current_MUMC.internal_medicine_D_1 == 1) 
#                                                                         | 
#                                                                           (data$out_specialty_current_MUMC.internal_medicine_general_1 == 1) 
#                                                                         | 
#                                                                           (data$out_specialty_current_MUMC.internal_medicine_geriatrics_1 == 1),
#                                                                         1, 
#                                                                         0)
# # grouping Gastroentero patients
# data$out_specialty_current_MUMC.gastroenterology_composite_1 <- ifelse((data$out_specialty_current_MUMC.gastroenterology_ibd_1 == 1)
#                                                                        |
#                                                                          (data$out_specialty_current_MUMC.gastroenterology_liver_1 == 1)
#                                                                        |
#                                                                          (data$out_specialty_current_MUMC.gastroenterology_neuro_1 == 1),
#                                                                        1,
#                                                                        0)
# # grouping neurology patients 
# data$out_specialty_current_MUMC.neurology_composite_1 <- ifelse((data$out_specialty_current_MUMC.neurology_vascular_1 == 1) 
#                                                                 | 
#                                                                   (data$out_specialty_current_MUMC.neurology_1 == 1),
#                                                                 1, 
#                                                                 0)
# 
# # grouping Rheumatology 
# data$out_specialty_current_MUMC.rheumatology_composite_1 <- ifelse((data$out_specialty_current_MUMC.rheumatology_gout_1 == 1)
#                                                                    |
#                                                                      (data$out_specialty_current_MUMC.rheumatology_spondyloarthritis_1 == 1)
#                                                                    |   
#                                                                      (data$out_specialty_current_MUMC.rheumatology_1 == 1),
#                                                                    1,
#                                                                    0)
# # grouping urology 
# data$out_specialty_current_MUMC.urology_composite_1 <- ifelse((data$out_specialty_current_MUMC.urology_functional_1 == 1) 
#                                                               | 
#                                                                 (data$out_specialty_current_MUMC.urology_oncology_1 == 1),
#                                                               1,
#                                                               0)
# # making further subgroups of group A = 1320 patients 
# data$out_specialty_current_MUMC.group_A_1 <- ifelse((data$out_specialty_current_MUMC.cardiology_1 == 1)
#                                                     |
#                                                       (data$out_specialty_current_MUMC.internal_medicine_composite_1 == 1)
#                                                     |
#                                                       (data$out_specialty_current_MUMC.gastroenterology_composite_1 == 1) 
#                                                     |
#                                                       (data$out_specialty_current_MUMC.neurology_composite_1 == 1)
#                                                     |
#                                                       (data$out_specialty_current_MUMC.rheumatology_composite_1 == 1),
#                                                     1, 
#                                                     0)

# group B subgrouping = 677
# data$out_specialty_current_MUMC.group_B_1 <- ifelse((data$out_specialty_current_MUMC.surgery_composite_1 == 1)
#                                                     |
#                                                       (data$out_specialty_current_MUMC.orthopedics_1 == 1)
#                                                     |
#                                                       (data$out_specialty_current_MUMC.urology_oncology_1 == 1),
#                                                     1, 
#                                                     0)
# 
# 
# # group C = 298 
# data$out_specialty_current_MUMC.group_C_1 <- ifelse((data$out_specialty_current_MUMC.dermatology_1 == 1)
#                                                     |
#                                                       (data$out_specialty_current_MUMC.otorhinolaryngology_1 == 1)
#                                                     |
#                                                       (data$out_specialty_current_MUMC.urology_functional_1 == 1),
#                                                     1, 
#                                                     0)


## n_visits_out_current_specialty_12m_36m_MUMC.ord

names(data)[names(data) == "C3a"] <- "n_visits_out_current_specialty_12m_36m_MUMC.ord"
data$n_visits_out_current_specialty_12m_36m_MUMC.ord <- factor(data$n_visits_out_current_specialty_12m_36m_MUMC.ord,
                                                               levels = c("1",
                                                                          "2",
                                                                          "3"),
                                                               labels = c("1",
                                                                          "2",
                                                                          "3+"))
# data$n_visits_out_current_specialty_12m_36m_MUMC.1_1 <- ifelse(data$n_visits_out_current_specialty_12m_36m_MUMC.ord == 1,
#                                                                1,
#                                                                0)
# data$n_visits_out_current_specialty_12m_36m_MUMC.2_1 <- ifelse(data$n_visits_out_current_specialty_12m_36m_MUMC.ord == 2,
#                                                                1,
#                                                                0)
# data$n_visits_out_current_specialty_12m_36m_MUMC.3_or_above_1 <- ifelse(data$n_visits_out_current_specialty_12m_36m_MUMC.ord == 3,
#                                                                         1,
#                                                                         0)

table(data$n_visits_out_current_specialty_12m_36m_MUMC.ord, exclude = NULL)
prop.table(table(data$n_visits_out_current_specialty_12m_36m_MUMC.ord, exclude = NULL))


## n_visits_out_all_12m_MUMC.ord

names(data)[names(data) == "A14a"] <- "n_visits_out_all_12m_MUMC.ord"
data$n_visits_out_all_12m_MUMC.ord <- factor(data$n_visits_out_all_12m_MUMC.ord,
                                             levels = c(1, 
                                                        2, 
                                                        3, 
                                                        4), 
                                             labels = c("0", 
                                                        "1", 
                                                        "2-5", 
                                                        "> 5"))
# data$n_visits_out_all_12m_MUMC.0_1 <- ifelse(data$n_visits_out_all_12m_MUMC.ord == "0",
#                                              1, 
#                                              0)
# data$n_visits_out_all_12m_MUMC.1_1 <- ifelse(data$n_visits_out_all_12m_MUMC.ord == "1", 
#                                              1, 
#                                              0)
# data$n_visits_out_all_12m_MUMC.2_to_5_1 <- ifelse(data$n_visits_out_all_12m_MUMC.ord == "2-5",
#                                                   1, 
#                                                   0)
# data$n_visits_out_all_12m_MUMC.above_5_1 <- ifelse(data$n_visits_out_all_12m_MUMC.ord == "> 5",
#                                                    1, 
#                                                    0)

table(data$n_visits_out_all_12m_MUMC.ord, exclude = NULL)
prop.table(table(data$n_visits_out_all_12m_MUMC.ord, exclude = NULL))


## inout_specialty_12m_MUMC

inout_specialty_12m_MUMC.list <- strsplit(data$A150a, ",") # Assumption NA equals to negative indication.
data$inout_specialty_12m_MUMC.cardiology_1 <-ifelse(lapply(inout_specialty_12m_MUMC.list, function(x) { "1" %in% x }),
                                                    1,
                                                    0)
data$inout_specialty_12m_MUMC.urology_1 <- ifelse(lapply(inout_specialty_12m_MUMC.list, function(x) { "2" %in% x }),
                                                  1,
                                                  0)
data$inout_specialty_12m_MUMC.psychiatry_1 <- ifelse(lapply(inout_specialty_12m_MUMC.list, function(x) { "3" %in% x }),
                                                     1,
                                                     0)
data$inout_specialty_12m_MUMC.otorhinolaryngology_1 <- ifelse(lapply(inout_specialty_12m_MUMC.list, function(x) { "4" %in% x }),
                                                              1,
                                                              0)
data$inout_specialty_12m_MUMC.ophthalmology_1 <-ifelse(lapply(inout_specialty_12m_MUMC.list, function(x) { "5" %in% x }),
                                                       1,
                                                       0)
data$inout_specialty_12m_MUMC.internal_medicine_1 <- ifelse(lapply(inout_specialty_12m_MUMC.list, function(x) { "6" %in% x }),
                                                            1,
                                                            0)
data$inout_specialty_12m_MUMC.surgery_1 <- ifelse(lapply(inout_specialty_12m_MUMC.list, function(x) { "7" %in% x }),
                                                  1,
                                                  0)
data$inout_specialty_12m_MUMC.orthopedics_1 <- ifelse(lapply(inout_specialty_12m_MUMC.list, function(x) { "8" %in% x }),
                                                      1,
                                                      0)
data$inout_specialty_12m_MUMC.plastic_surgery_1 <- ifelse(lapply(inout_specialty_12m_MUMC.list, function(x) { "9" %in% x }),
                                                          1,
                                                          0)
data$inout_specialty_12m_MUMC.obstetrics_and_gynaecology_1 <- ifelse(lapply(inout_specialty_12m_MUMC.list, function(x) { "10" %in% x }),
                                                                     1,
                                                                     0)
data$inout_specialty_12m_MUMC.neurology_1 <- ifelse(lapply(inout_specialty_12m_MUMC.list, function(x) { "11" %in% x }),
                                                    1,
                                                    0)
data$inout_specialty_12m_MUMC.dermatology_1 <- ifelse(lapply(inout_specialty_12m_MUMC.list, function(x) { "12" %in% x }),
                                                      1,
                                                      0)
data$inout_specialty_12m_MUMC.gastroenterology_1 <- ifelse(lapply(inout_specialty_12m_MUMC.list, function(x) { "13" %in% x }),
                                                           1,
                                                           0)
data$inout_specialty_12m_MUMC.pneumology_1 <- ifelse(lapply(inout_specialty_12m_MUMC.list, function(x) { "14" %in% x }),
                                                     1,
                                                     0)
data$inout_specialty_12m_MUMC.rheumatology_1 <- ifelse(lapply(inout_specialty_12m_MUMC.list, function(x) { "15" %in% x }),
                                                       1,
                                                       0)
data$inout_specialty_12m_MUMC.do_not_select_1 <- ifelse(lapply(inout_specialty_12m_MUMC.list, function(x) { "16" %in% x }),
                                                        1,
                                                        0)
data$inout_specialty_12m_MUMC.pediatrics_1 <- ifelse(lapply(inout_specialty_12m_MUMC.list, function(x) { "17" %in% x }),
                                                     1,
                                                     0)
data$inout_specialty_12m_MUMC.anesthesiology_1 <- ifelse(lapply(inout_specialty_12m_MUMC.list, function(x) { "18" %in% x }),
                                                         1,
                                                         0)
data$inout_specialty_12m_MUMC.other_1 <- ifelse(lapply(inout_specialty_12m_MUMC.list, function(x) { "19" %in% x }),
                                                1,
                                                0)

# data$inout_specialty_12m_MUMC.surgery_composite_1 <- ifelse((data$inout_specialty_12m_MUMC.surgery_1 == 1)
#                                                             |
#                                                               (data$inout_specialty_12m_MUMC.plastic_surgery_1 == 1),
#                                                             1,
#                                                             0)

## n_specialty_MUMC

# data$specialty_MUMC.cardiology_1 <- ifelse((data$out_specialty_current_MUMC.cardiology_1 == 1)
#                                            |
#                                              (data$inout_specialty_12m_MUMC.cardiology_1 == 1),
#                                            1,
#                                            0)
# data$specialty_MUMC.surgery_composite_1 <- ifelse((data$out_specialty_current_MUMC.surgery_composite_1 == 1)
#                                                   |
#                                                     (data$inout_specialty_12m_MUMC.surgery_composite_1 == 1),
#                                                   1,
#                                                   0)
# data$specialty_MUMC.dermatology_1 <- ifelse((data$out_specialty_current_MUMC.dermatology_1 == 1)
#                                             |
#                                               (data$inout_specialty_12m_MUMC.dermatology_1 == 1),
#                                             1,
#                                             0)
# data$specialty_MUMC.internal_medicine_composite_1 <- ifelse((data$out_specialty_current_MUMC.internal_medicine_composite_1 == 1)
#                                                             |
#                                                               (data$inout_specialty_12m_MUMC.internal_medicine_1 == 1),
#                                                             1,
#                                                             0)
# data$specialty_MUMC.otorhinolaryngology_1 <- ifelse((data$out_specialty_current_MUMC.otorhinolaryngology_1 == 1)
#                                                     |
#                                                       (data$inout_specialty_12m_MUMC.otorhinolaryngology_1 == 1),
#                                                     1,
#                                                     0)
# data$specialty_MUMC.gastroenterology_composite_1 <- ifelse((data$out_specialty_current_MUMC.gastroenterology_composite_1 == 1)
#                                                            |
#                                                              (data$inout_specialty_12m_MUMC.gastroenterology_1 == 1),
#                                                            1,
#                                                            0)
# data$specialty_MUMC.neurology_composite_1 <- ifelse((data$out_specialty_current_MUMC.neurology_composite_1 == 1)
#                                                     |
#                                                       (data$inout_specialty_12m_MUMC.neurology_1 == 1),
#                                                     1,
#                                                     0)
# data$specialty_MUMC.orthopedics_1 <- ifelse((data$out_specialty_current_MUMC.orthopedics_1 == 1)
#                                             |
#                                               (data$inout_specialty_12m_MUMC.orthopedics_1 == 1),
#                                             1,
#                                             0)
# data$specialty_MUMC.rheumatology_composite_1 <- ifelse((data$out_specialty_current_MUMC.rheumatology_composite_1 == 1)
#                                                        |
#                                                          (data$inout_specialty_12m_MUMC.rheumatology_1 == 1),
#                                                        1,
#                                                        0)
# data$specialty_MUMC.urology_composite_1 <- ifelse((data$out_specialty_current_MUMC.urology_composite_1 == 1)
#                                                   |
#                                                     (data$inout_specialty_12m_MUMC.urology_1 == 1),
#                                                   1,
#                                                   0)
# 
# 
# # to see the overall number of specialists a patient has visited in past 12 months
# data$n_specialty_MUMC <- data$specialty_MUMC.cardiology_1 +
#   data$specialty_MUMC.surgery_composite_1 +
#   data$specialty_MUMC.dermatology_1 +
#   data$specialty_MUMC.internal_medicine_composite_1 +
#   data$specialty_MUMC.otorhinolaryngology_1 +
#   data$specialty_MUMC.gastroenterology_composite_1 +
#   data$specialty_MUMC.neurology_composite_1 +
#   data$specialty_MUMC.orthopedics_1 +
#   data$specialty_MUMC.rheumatology_composite_1 +
#   data$specialty_MUMC.urology_composite_1 +
#   data$inout_specialty_12m_MUMC.psychiatry_1 +
#   data$inout_specialty_12m_MUMC.ophthalmology_1 +
#   data$inout_specialty_12m_MUMC.pneumology_1 +
#   data$inout_specialty_12m_MUMC.obstetrics_and_gynaecology_1 +
#   data$inout_specialty_12m_MUMC.pediatrics_1 +
#   data$inout_specialty_12m_MUMC.anesthesiology_1 +
#   data$inout_specialty_12m_MUMC.other_1

# table(data$n_specialty_MUMC, exclude = NULL)
# prop.table(table(data$n_specialty_MUMC, exclude = NULL))
# summary(data$n_specialty_MUMC)
# sd(data$n_specialty_MUMC)
# ggplot(data, aes(x = n_specialty_MUMC)) +
#   geom_bar() +
#   scale_x_continuous(breaks = seq(0, 12, 1),
#                      labels = seq(0, 12, 1)) +
#   theme_minimal() 
# 
# data$n_specialty_MUMC.centered <- data$n_specialty_MUMC - mean(data$n_specialty_MUMC)
# data$n_specialty_MUMC.std <- data$n_specialty_MUMC.centered / sd(data$n_specialty_MUMC)


## in_all_12m_MUMC.yes_1 -- Hospitalizations in past 12 months

names(data)[names(data) == "A16a"] <- "in_all_12m_MUMC.yes_1"
data$in_all_12m_MUMC.yes_1 <- ifelse(data$in_all_12m_MUMC.yes_1 == 2, 
                                     0, 
                                     data$in_all_12m_MUMC.yes_1)

table(data$in_all_12m_MUMC.yes_1, exclude = NULL)
prop.table(table(data$in_all_12m_MUMC.yes_1, exclude = NULL))


## n_ER_12m_MUMC -- A&E visits past 12 months

names(data)[names(data) == "A18a"] <- "n_ER_12m_MUMC"

table(data$n_ER_12m_MUMC, exclude = NULL)
prop.table(table(data$n_ER_12m_MUMC, exclude = NULL))
summary(data$n_ER_12m_MUMC)
sd(data$n_ER_12m_MUMC)
ggplot(data, aes(x = n_ER_12m_MUMC)) +
  geom_histogram(binwidth = 1) +
  scale_x_continuous(breaks = seq(0, 20, 1),
                     labels = seq(0, 20, 1)) +
  theme_minimal() 

data$n_ER_12m_MUMC.centered <- data$n_ER_12m_MUMC - mean(data$n_ER_12m_MUMC)
data$n_ER_12m_MUMC.std <- data$n_ER_12m_MUMC.centered / sd(data$n_ER_12m_MUMC)

## inoutER_12m_external_hospital.yes_1 -- Visits to external clinics/hospitals p12m

names(data)[names(data) == "A140b"] <- "inoutER_12m_external_hospital.yes_1"
data$inoutER_12m_external_hospital.yes_1 <- ifelse(data$inoutER_12m_external_hospital.yes_1 == 2,
                                                   0, 
                                                   data$inoutER_12m_external_hospital.yes_1) 

table(data$inoutER_12m_external_hospital.yes_1, exclude = NULL)
prop.table(table(data$inoutER_12m_external_hospital.yes_1, exclude = NULL))

## inout_specialty_12m_external_hospital.list

inout_specialty_12m_external_hospital.list <- strsplit(data$A150b, ",") # Assumption NA equals negative indication.
data$inout_specialty_12m_external_hospital.cardiology_1 <- ifelse(lapply(inout_specialty_12m_external_hospital.list, function(x) { "1" %in% x }),
                                                                  1,
                                                                  0)
data$inout_specialty_12m_external_hospital.urology_1 <- ifelse(lapply(inout_specialty_12m_external_hospital.list, function(x) { "2" %in% x }),
                                                               1,
                                                               0)
data$inout_specialty_12m_external_hospital.psychiatry_1 <- ifelse(lapply(inout_specialty_12m_external_hospital.list, function(x) { "3" %in% x }),
                                                                  1,
                                                                  0)
data$inout_specialty_12m_external_hospital.otorhinolaryngology_1 <- ifelse(lapply(inout_specialty_12m_external_hospital.list, function(x) { "4" %in% x }),
                                                                           1,
                                                                           0)
data$inout_specialty_12m_external_hospital.ophthalmology_1 <- ifelse(lapply(inout_specialty_12m_external_hospital.list, function(x) { "5" %in% x }),
                                                                     1,
                                                                     0)
data$inout_specialty_12m_external_hospital.internal_medicine_1 <- ifelse(lapply(inout_specialty_12m_external_hospital.list, function(x) { "6" %in% x }),
                                                                         1,
                                                                         0)
data$inout_specialty_12m_external_hospital.surgery_1 <- ifelse(lapply(inout_specialty_12m_external_hospital.list, function(x) { "7" %in% x }),
                                                               1,
                                                               0)
data$inout_specialty_12m_external_hospital.orthopedics_1 <- ifelse(lapply(inout_specialty_12m_external_hospital.list, function(x) { "8" %in% x }),
                                                                   1,
                                                                   0)
data$inout_specialty_12m_external_hospital.plastic_surgery_1 <- ifelse(lapply(inout_specialty_12m_external_hospital.list, function(x) { "9" %in% x }),
                                                                       1,
                                                                       0)
data$inout_specialty_12m_external_hospital.obstetrics_and_gynaecology_1 <- ifelse(lapply(inout_specialty_12m_external_hospital.list, function(x) { "10" %in% x }),
                                                                                  1,
                                                                                  0)
data$inout_specialty_12m_external_hospital.neurology_1 <- ifelse(lapply(inout_specialty_12m_external_hospital.list, function(x) { "11" %in% x }),
                                                                 1,
                                                                 0)
data$inout_specialty_12m_external_hospital.dermatology_1 <- ifelse(lapply(inout_specialty_12m_external_hospital.list, function(x) { "12" %in% x }),
                                                                   1,
                                                                   0)
data$inout_specialty_12m_external_hospital.gastroenterology_1 <- ifelse(lapply(inout_specialty_12m_external_hospital.list, function(x) { "13" %in% x }),
                                                                        1,
                                                                        0)
data$inout_specialty_12m_external_hospital.pneumology_1 <- ifelse(lapply(inout_specialty_12m_external_hospital.list, function(x) { "14" %in% x }),
                                                                  1,
                                                                  0)
data$inout_specialty_12m_external_hospital.rheumatology_1 <- ifelse(lapply(inout_specialty_12m_external_hospital.list, function(x) { "15" %in% x }),
                                                                    1,
                                                                    0)
data$inout_specialty_12m_external_hospital.do_not_select_1 <- ifelse(lapply(inout_specialty_12m_external_hospital.list, function(x) { "16" %in% x }),
                                                                     1,
                                                                     0)
data$inout_specialty_12m_external_hospital.pediatrics_1 <- ifelse(lapply(inout_specialty_12m_external_hospital.list, function(x) { "17" %in% x }),
                                                                  1,
                                                                  0)
data$inout_specialty_12m_external_hospital.anesthesiology_1 <- ifelse(lapply(inout_specialty_12m_external_hospital.list, function(x) { "18" %in% x }),
                                                                      1,
                                                                      0)
data$inout_specialty_12m_external_hospital.other_1 <- ifelse(lapply(inout_specialty_12m_external_hospital.list, function(x) { "19" %in% x }),
                                                             1,
                                                             0)

data$inout_specialty_12m_external_hospital.surgery_composite_1 <- ifelse((data$inout_specialty_12m_external_hospital.surgery_1 == 1)
                                                                         |
                                                                           (data$inout_specialty_12m_external_hospital.plastic_surgery_1 == 1),
                                                                         1,
                                                                         0)

## inoutER_12m_external_hospital.yes_1 -- Visits to external clinics/hospitals p12m

names(data)[names(data) == "A140b"] <- "inoutER_12m_external_hospital.yes_1"
data$inoutER_12m_external_hospital.yes_1 <- ifelse(data$inoutER_12m_external_hospital.yes_1 == 2,
                                                   0, 
                                                   data$inoutER_12m_external_hospital.yes_1) 

table(data$inoutER_12m_external_hospital.yes_1, exclude = NULL)
prop.table(table(data$inoutER_12m_external_hospital.yes_1, exclude = NULL))


## in_all_12m_external_hospital.yes_1 --  any external hospitalizations

names(data)[names(data) == "A16b"] <- "in_all_12m_external_hospital.yes_1"
data$in_all_12m_external_hospital.yes_1 <- ifelse(data$in_all_12m_external_hospital.yes_1 == 2, 
                                                  0, 
                                                  data$in_all_12m_MUMC.yes_1)  

table(data$in_all_12m_external_hospital.yes_1, exclude = NULL)
prop.table(table(data$in_all_12m_external_hospital.yes_1, exclude = NULL))


## n_ER_12m_external_hospital -- A&E external visits in p12m

names(data)[names(data) == "A18b"] <- "n_ER_12m_external_hospital" 

table(data$n_ER_12m_external_hospital, exclude = NULL)
prop.table(table(data$n_ER_12m_external_hospital, exclude = NULL))
summary(data$n_ER_12m_external_hospital)
sd(data$n_ER_12m_external_hospital)
ggplot(data, aes(x = n_ER_12m_external_hospital)) +
  geom_histogram(binwidth = 1) +
  scale_x_continuous(breaks = seq(0, 15, 1),
                     labels = seq(0, 15, 1)) +
  theme_minimal()

data$n_ER_12m_external_hospital.centered <- data$n_ER_12m_external_hospital - mean(data$n_ER_12m_external_hospital)
data$n_ER_12m_external_hospital.std <- data$n_ER_12m_external_hospital.centered / sd(data$n_ER_12m_external_hospital)


## n_specialty_MUMC_external
# Here we're grouping together any visits to a certain speciliast, either at MUMC or external

# data$specialty_MUMC_external_hospital.cardiology_1 <- ifelse((data$specialty_MUMC.cardiology_1 == 1)
#                                                              |
#                                                                (data$inout_specialty_12m_external_hospital.cardiology_1 == 1),
#                                                              1,
#                                                              0)
# data$specialty_MUMC_external_hospital.surgery_composite_1 <- ifelse((data$specialty_MUMC.surgery_composite_1 == 1)
#                                                                     |
#                                                                       (data$inout_specialty_12m_external_hospital.surgery_composite_1 == 1),
#                                                                     1,
#                                                                     0)
# data$specialty_MUMC_external_hospital.dermatology_1 <- ifelse((data$specialty_MUMC.dermatology_1 == 1)
#                                                               |
#                                                                 (data$inout_specialty_12m_external_hospital.dermatology_1 == 1),
#                                                               1,
#                                                               0)
# data$specialty_MUMC_external_hospital.internal_medicine_composite_1 <- ifelse((data$specialty_MUMC.internal_medicine_composite_1 == 1)
#                                                                               |
#                                                                                 (data$inout_specialty_12m_external_hospital.internal_medicine_1 == 1),
#                                                                               1,
#                                                                               0)
# data$specialty_MUMC_external_hospital.otorhinolaryngology_1 <- ifelse((data$specialty_MUMC.otorhinolaryngology_1 == 1)
#                                                                       |
#                                                                         (data$inout_specialty_12m_external_hospital.otorhinolaryngology_1 == 1),
#                                                                       1,
#                                                                       0)
# data$specialty_MUMC_external_hospital.gastroenterology_composite_1 <- ifelse((data$specialty_MUMC.gastroenterology_composite_1 == 1)
#                                                                              |
#                                                                                (data$inout_specialty_12m_external_hospital.gastroenterology_1 == 1),
#                                                                              1,
#                                                                              0)
# data$specialty_MUMC_external_hospital.neurology_composite_1 <- ifelse((data$specialty_MUMC.neurology_composite_1 == 1)
#                                                                       |
#                                                                         (data$inout_specialty_12m_external_hospital.neurology_1 == 1),
#                                                                       1,
#                                                                       0)
# data$specialty_MUMC_external_hospital.orthopedics_1 <- ifelse((data$specialty_MUMC.orthopedics_1 == 1)
#                                                               |
#                                                                 (data$inout_specialty_12m_external_hospital.orthopedics_1 == 1),
#                                                               1,
#                                                               0)
# data$specialty_MUMC_external_hospital.rheumatology_composite_1 <- ifelse((data$specialty_MUMC.rheumatology_composite_1 == 1)
#                                                                          |
#                                                                            (data$inout_specialty_12m_external_hospital.rheumatology_1 == 1),
#                                                                          1,
#                                                                          0)
# data$specialty_MUMC_external_hospital.urology_composite_1 <- ifelse((data$specialty_MUMC.urology_composite_1 == 1)
#                                                                     |
#                                                                       (data$inout_specialty_12m_external_hospital.urology_1 == 1),
#                                                                     1,
#                                                                     0)
# data$specialty_MUMC_external_hospital.psychiatry_1 <- ifelse((data$inout_specialty_12m_MUMC.psychiatry_1 == 1)
#                                                              |
#                                                                (data$inout_specialty_12m_external_hospital.psychiatry_1 == 1),
#                                                              1,
#                                                              0)
# data$specialty_MUMC_external_hospital.ophthalmology_1 <- ifelse((data$inout_specialty_12m_MUMC.ophthalmology_1 == 1)
#                                                                 |
#                                                                   (data$inout_specialty_12m_external_hospital.ophthalmology_1 == 1),
#                                                                 1,
#                                                                 0)
# data$specialty_MUMC_external_hospital.pneumology_1 <- ifelse((data$inout_specialty_12m_MUMC.pneumology_1 == 1)
#                                                              |
#                                                                (data$inout_specialty_12m_external_hospital.pneumology_1 == 1),
#                                                              1,
#                                                              0)
# data$specialty_MUMC_external_hospital.obstetrics_and_gynaecology_1 <- ifelse((data$inout_specialty_12m_MUMC.obstetrics_and_gynaecology_1 == 1)
#                                                                              |
#                                                                                (data$inout_specialty_12m_external_hospital.obstetrics_and_gynaecology_1 == 1),
#                                                                              1,
#                                                                              0)
# data$specialty_MUMC_external_hospital.pediatrics_1 <- ifelse((data$inout_specialty_12m_MUMC.pediatrics_1 == 1)
#                                                              |
#                                                                (data$inout_specialty_12m_external_hospital.pediatrics_1 == 1),
#                                                              1,
#                                                              0)
# data$specialty_MUMC_external_hospital.anesthesiology_1 <- ifelse((data$inout_specialty_12m_MUMC.anesthesiology_1 == 1)
#                                                                  |
#                                                                    (data$inout_specialty_12m_external_hospital.anesthesiology_1 == 1),
#                                                                  1,
#                                                                  0)
# data$specialty_MUMC_external_hospital.other_1 <- ifelse((data$inout_specialty_12m_MUMC.other_1 == 1)
#                                                         |
#                                                           (data$inout_specialty_12m_external_hospital.other_1 == 1),
#                                                         1,
#                                                         0)
# 
# data$n_specialty_MUMC_external_hospital <- data$specialty_MUMC_external_hospital.cardiology_1 +
#   data$specialty_MUMC_external_hospital.surgery_composite_1 +
#   data$specialty_MUMC_external_hospital.dermatology_1 +
#   data$specialty_MUMC_external_hospital.internal_medicine_composite_1 +
#   data$specialty_MUMC_external_hospital.otorhinolaryngology_1 +
#   data$specialty_MUMC_external_hospital.gastroenterology_composite_1 +
#   data$specialty_MUMC_external_hospital.neurology_composite_1 +
#   data$specialty_MUMC_external_hospital.orthopedics_1 +
#   data$specialty_MUMC_external_hospital.rheumatology_composite_1 +
#   data$specialty_MUMC_external_hospital.urology_composite_1 +
#   data$specialty_MUMC_external_hospital.psychiatry_1 +
#   data$specialty_MUMC_external_hospital.ophthalmology_1 +
#   data$specialty_MUMC_external_hospital.pneumology_1 +
#   data$specialty_MUMC_external_hospital.obstetrics_and_gynaecology_1 +
#   data$specialty_MUMC_external_hospital.pediatrics_1 +
#   data$specialty_MUMC_external_hospital.anesthesiology_1 +
#   data$specialty_MUMC_external_hospital.other_1
# 
# table(data$n_specialty_MUMC_external_hospital, exclude = NULL)
# prop.table(table(data$n_specialty_MUMC_external_hospital, exclude = NULL))
# summary(data$n_specialty_MUMC_external_hospital)
# sd(data$n_specialty_MUMC_external_hospital)
# ggplot(data, aes(x = n_specialty_MUMC_external_hospital)) +
#   geom_histogram(binwidth = 1) +
#   scale_x_continuous(breaks = seq(0, 12, 1),
#                      labels = seq(0, 12, 1)) +
#   theme_minimal() 

# data$n_specialty_MUMC_external_hospital.centered <- data$n_specialty_MUMC_external_hospital - mean(data$n_specialty_MUMC_external_hospital)
# data$n_specialty_MUMC_external_hospital.std <- data$n_specialty_MUMC_external_hospital.centered / sd(data$n_specialty_MUMC_external_hospital)



## revision_physician
# data on whether original policy was changed or not 
# and if so, medicine or non-medicine related changes?

names(data)[names(data) == "S6c"] <- "revision_physician"
data$revision_physician <- ifelse(is.na(data$revision_physician),
                                  3,
                                  data$revision_physician) # Assumption NA equals negative indication.
data$revision_physician <- factor(data$revision_physician,
                                  levels = c(1, 
                                             2, 
                                             3), 
                                  labels = c("yes: medicine", 
                                             "yes: non-medicine", 
                                             "no"))

table(data$revision_physician, exclude = NULL)
prop.table(table(data$revision_physician, exclude = NULL))

data$revision_physician.medicine_1 <- ifelse(data$revision_physician == "yes: medicine",
                                             1,
                                             0)
data$revision_physician.non_medicine_1 <- ifelse(data$revision_physician == "yes: non-medicine",
                                                 1,
                                                 0)
data$revision_physician.none_1 <- ifelse(data$revision_physician == "no",
                                         1,
                                         0)

## y

data$y <- ifelse((data$revision_physician.medicine_1 == 1) 
                 | 
                   (data$revision_physician.non_medicine_1 == 1),
                 1,
                 0)

table(data$y, exclude = NULL)
prop.table(table(data$y, exclude = NULL))

## Let's save it as a few forms ##############################################

# Saving in Rdata form 
save(data, file="compiled_data_V1.Rdata")
saveRDS(data, "compiled_data_V1.Rds")

# as a csv
write.table(data, file="compiled_data_V1.csv", sep = ",", row.names = F)
# as a csv
write.table(data, file="compiled_data_V1.csv", sep = ",", row.names = F)

### Further manual filtering of this table will later be done resulting in the trimmed file


