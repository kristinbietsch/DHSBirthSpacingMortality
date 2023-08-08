# DHSBirthSpacingMortality
Readme AS 88: Birth Intervals and Child Mortality
This code is used to produce analysis for the Demographic Health Surveys' update examining mortality risks associated with short birth to conception intervals, maternal age at birth, and birth order
Files:
•	Master DHS Survey List.xlsx
o	This file contains information on all publicly available DHS datasets- each row is a different survey.  It contains the country name, year(s) of the survey, and names of each recode file. 
•	DHS Surveys for Birth Spacing Reports 070623.csv
o	This file contains the same surveys as Master DHS Survey List.xlsx with an added column noting if the files were included in older DHS analysis of child mortality or if they are included in the current study.
•	Surveys with Calendar Contraception Data.csv
o	This file is created by Looking for Calendars and Looping Calendar Analysis 071923.R
o	For each survey in WP41, AS37, and AS88, it reports if there is a calendar in the survey
•	Looking for Calendars and Looping Calendar Analysis 071923.R
o	This code first opens each IR file and determines if there is a calendar (data is present in both vcal_1 and v017
o	The second section of analysis opens each IR file, reshapes the calendar data, and then determines for each birth the length of gestation, the outcome of the previous pregnancy, and if the birth was the results of a contraceptive failure
o	The results are saved as new files for each survey in a folder for analysis in DHS Infant Mortality Analysis Tables 072623.R
•	DHS Infant Mortality Analysis Tables 072623.R
o	This is the main file of analysis for the report.  It loads all three spreadsheets discussed above.  It then recodes variables for all surveys and creates a pooled dataset  and recodes its variables.  It creates summary statistics for all surveys and the pooled dataset, as well as bivariate and multivariable Cox hazard models for the pooled dataset.  It creates and exports results tables.  
