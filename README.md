# Project2
## Project Overview
Purpose: In this project we will download and analyze the mobile device usage 
and user behavior dataset from Kaggle and creates an interactive shiny app for exploring the data.  
It contains scripts for data preparation, summarization and visualization.
## Folders
/data/user_behavior_dataset.csv  #this is the original dataset  
/data/user_behavior_der.csv      #this is the cleaned dataset in .csv format  
/data/user_behavior_der.rds      #this is the original dataset in .rds format  
/app.R                             #shiny app script  
/data_preparation.qmd              # script for data preparation/processing  
/helpers.R                             #helper script for the app.R  
## Data preparation flow
1. **Load raw data**
2. **Clean and process data**
        - automatically fixes spaces in variable names
        - create derived variables 
3. **Data summarization**
        - create categorical summaries
        - create numerical summaries
        - create graphical summaries
4. **Save data**
        - saved processed/cleaned data as .rds and .csv
        
## Shiny app flow      
1. **Load processed data**
2. **Read helper script**
3. **UI Elements**  
4  **Server Logic**



