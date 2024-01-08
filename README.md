# BigDataBowl-MADS

Repo for code and figures used in 2024 NFL Big Data Bowl submission: No Edge No Chance.

Original data and competition information: https://www.kaggle.com/competitions/nfl-big-data-bowl-2024/overview

Link to No Edge No Chance submission: https://www.kaggle.com/code/devinbasley26/no-edge-no-chance

## Files

### data-prep.Rmd
Cleans data given by the 2024 NFL Big Data Bowl competition page on Kaggle. Filters the tracking data for zone run plays inside the tackle box and calculates total directional change and relative directional change (in degrees) of the ball carrier. Creates all_processed_data.csv and filtered_processed_data.csv. The resulting files are not included in the repo because of size.

### Edge-Intensity-Rating.Rmd
Reads in all_processed_data.csv and uses data to create Edge Intensity Rating (EIR), a metric for evaluating edge setters. Makes the data file BC_and_ES_information.csv, which is available in the repo.

### graphs.Rmd
Makes static visualizations and animations for displaying total directional change, relative directional change, EIR, and their relationships with EPA.

### Model.Rmd
Investigates and confirms relationships between EIR and EPA. Uses a linear model and regression analysis.

## Folders

### edge-setting-epa
Files for the Shiny app. Deployed on: https://dkz51f-marion-haney.shinyapps.io/edge-setting-epa/ 

### figures
Files to reference in the competition Kaggle notebook.

