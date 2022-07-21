# American Statistical Institute (ASA) DataFest Dashboard

This repository contains the code for the Interactive Dashboard created to visualize the growth of DataFest over the years. 

The folders in this repository are as follow:  

1. `R` :
The `R` folder has 2 files  

- `cleaning_script.R` : This file contains the code for cleaning up the data recieved by Universities in the Qualtrics Survey. 
- `helper.R` : This file contains the code for helper functions and libraries used to run the app

2. `data` :  
The data folder contains multiple csv files that are used in the dashboard. These are called in the 'helper.R' file.  
- `data_qualtrics_info_raw.csv` : This is raw version of responses downloaded from Qualtrics
- `data_qualtrics_info_sorted.csv` : This is the sorted version of the raw file containing Qualtrics responses
- `datafest-raw.csv`: This is the raw version of datafest data collected for the earlier version of the dashboard
- `datafest.csv ` : This is the processed datafest file used to build the earlier version of the dashboard
- `past_prompts.csv` - This is a dataset manually created to capture the year wise prompts for each datafest. This will have to be updated annually.
- `recent.csv` : **TO BE UPDATED**
- `titles.csv` : **TO BE UPDATED**
- `updated_datafest.csv` : This is the new datafest data or the master file used to build the current dashboard.
- `update_titles.csv` : **TO BE UPDATED**
- `past_winners` : **TO BE UPDATED**

3. app.R : This is code for the Shiny App / the Dashboard
