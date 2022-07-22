# American Statistical Institute (ASA) DataFest Dashboard

This repository contains the code for the interactive RShiny dashboard created to visualize the growth of DataFest over the years. 

The folders/files in this repository are:  

1. `R` :
The `R` folder has 2 files  

- `cleaning_script.R` : This file contains the code for cleaning up the data recieved by Universities in the Qualtrics survey. 
- `helper.R` : This file contains the code for helper functions and libraries used in `app.R` to run the app
 
<br/> 
    
2. `data` :  
The data folder contains multiple csv files that are used in the dashboard. These are called in the `helper.R` file.  
- `data_qualtrics_info_raw.csv` : This is the raw version of survey responses downloaded from Qualtrics
- `data_qualtrics_info_sorted.csv` : This is the sorted version of the raw file containing Qualtrics survey responses
- `datafest-raw.csv`: This is the raw version of datafest data collected for the earlier version of the dashboard (2011-2017)
- `datafest.csv ` : This is the pre-processed datafest file used to build the earlier version of the dashboard (2011-2017)
- `past_prompts.csv` - This is a dataset manually created to capture the year wise prompts for each datafest. This will have to be updated annually.
- `updated_datafest.csv` : This is the new datafest data or the master file used to build the current dashboard. (2011-2022)
- `update_titles.csv` : This is a dataset manually created to capture the year-wise university-wise awards given to students during an event. It also includes the team names, and a link to their slides/write ups/ video submissions
    
<br/>    
    
3. `app.R` : This is the code for the Shiny App /  Dashboard

<br/>

The deployed version of the app can be found [here](https://allis.shinyapps.io/datafest/) 

It is also hosted on the datafest website [here](https://asa-datafest.netlify.app/)
