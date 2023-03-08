# Medication Reconciliation and ML


This project is aimed at utilizing machine learning and deep learning to analyze patient data from MUMC (Maastricht University Medical Center) in order to develop models that are capable of stratifying patients into those that require and would benefit from medication reconciliation and those that would not. 

#### Introduction

Medication reconciliation is the process of comparing a patient's medication orders to all of the medications that the patient has been taking, in order to avoid medication errors. However, it has been shown that always performing medication reconciliation does not actually improves patient outcomes, as many patients are not at risk to these kinds or errors. 

#### Aim

This project aims to utulise machine learning and deep learning techniques to analyze patient data from MUMC in order to determine whether we can classify which patients require the tiresome clinical process.

#### Data

The data used in this project consists of patient data obtained from MUMC. This is along list of questionnaire data and some more regular demographic and clinical data. There are three main categories of the obtained data, patient-derived data, physician-derived data, and clinical pharmacist- or physician-derived data. These sets of patient data include a variety of continuous, categorical or ordinal data. 
The main outcomes of the data are whether or not the patient required a medication reconciliation. 

#### Scripts

##### The following scripts are included in this project:

- Updated_processing_script.R: This R script cleans and processes the raw patient data to a certain extent. The processed data is then used as input for the Correlation_Imputation_Dummy_Encoding.R script.
- Correlation_Imputation_Dummy_Encoding.R: This R script performs multivariate imputation by chained equations (MICE) to impute missing values in the processed patient data. The script also performs correlation threshold filtering, removes redundant features, and performs dummy encoding of categorical features. The output is a more prepared dataset for further analysis.
- RFE_Training.ipynb: This Jupyter notebook performs additional correlation threshold filtering on the prepared dataset, oversamples the minority class using SMOTE variations, performs recursive feature elimination, and trains models. The notebook includes several runs with different training data and feature selection methods.

#### Usage

To use this repository in the correct manner, first run updated_processing_script.R to clean and process the raw patient data to a certain extent.
Before inputting the data into the next script, it will have ot mnually processed a little (done in excel). Then run Correlation_Imputation_Dummy_Encoding.R to perform MICE and other necessary preprocessing steps on the processed data. Finally, RFE_Training.ipynb includes further preprocessing, oversampling, feature selection, and model training.

