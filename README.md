### Analyzing the impact of Montreal’s Réseau Express Vélo (REV) on Surrounding Bike Lanes’ Ridership and the COVID-19 Cycling Recovery

# Description
This project contains the code used in the following article: 
[Analyzing the impact of Montreal’s Réseau Express Vélo (REV) on Surrounding Bike Lanes’ Ridership and the COVID-19 Cycling Recovery] (https://www.mdpi.com/2071-1050/16/14/5992)

## Instructions

Open the Rproj file, then access the individual scripts from the file menu <br/>

## Script descriptions

**1-Data_Preparation** :  Consolidates Montreal bike count data from multiple years <br/>
**2-Imputation (Day)** :  Imputes missing values <br/>
**3-Covid_Change_Normality_Test (Month)** : Verifies if the monthly average change in bike counts due to COVID-19 is normally distributed <br/>
**4-Covid_Change (Month)** : Creates a measure of weighted monthly average change due to COVID-19 that will be used to seperate the impact of the pandemic, and the impact of new bike infrastructure <br/>
**5-Stats_Tables (Year)** : Descriptive statistics for the data by year <br/>
**6-Cluster_Analysis (Month)** : Calculates the amoutn of REV ridership that is generated by pent-up demand for cycling infrastructure, and how much is absorbed from surrounding bike paths <br/>

## Folder structure 

**DataRaw** : Contains the raw bike count data obtained fro the [City of Montreal](https://donnees.montreal.ca/en/dataset/velos-comptage) <br/>
**DataTreated** : Contains all the processed data that was created with the scripts <br/>
**Outputs** : Contians all the tables and figures created with the scripts <br/>
**Scripts** : Contains the individual R scripts <br/>

## Packages : 
"tidyverse" <br/>
"imputeTS" <br/>
"forecast" <br/>
"openxlsx" <br/>
"ggpubr" <br/>
"ggfortify" <br/>
"patchwork" <br/>

Note : All package installation is handled within the scripts.

# Author

Gavin MacGregor
