# Master-Thesis-Project---Clinical-Data-Science

Project Name
This project involves processing a dataset using R to impute missing values, perform one-hot encoding of categorical variables, and remove irrelevant features. The resulting dataset can then be used for developing predictive models.

Getting Started
To get started, make sure you have the following R packages installed:

mice
readxl
dplyr

You can install these packages by running the following commands:

>>
install.packages("mice")
install.packages("readxl")
install.packages("dplyr")
>> 

Data
The dataset used in this project is called "trimmed_compiled_data.csv". It is composed of the results from the pre-processing script, but has been manually filtered in Excel prior to importing it into R.

Running the Analysis
The 2nd R script is called "Correlation_Imputation_Dummy_Encoding.R". It performs the following steps:

Detect and remove missing values through imputation using the MICE package.
Remove character type features that need to be factorized.
Perform one-hot encoding of categorical variables after multiple imputation.
Remove correlated features.
Convert categorical variables to one-hot encoded binary variables.
To run the analysis, you just have to change the directories (within script) to where you have the downloaded data stored. 
The resulting dataset will be saved as a CSV file called "imputed_data_V3csv".

Author
Liam Glueck  
