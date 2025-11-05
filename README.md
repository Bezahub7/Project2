# Project2
## Project Overview
Purpose: In this project we will download and analyze the mobile device usage and user behavior dataset 
from Kaggle and create an interactive shiny app for exploring the data.  
The Shiny app created explore mobile users behavior based on usage patterns.  
Users can subset, visualize and download (full or subsetted) data from the app.  

### Key Variables 
This are the most important variables used in the analysis and when creating visualizations.  
|Variable Name---------------|--------------Description----------------------------|--Type--|
|:---------------------------|:----------------------------------------------------|:------:|
|app_usage_time_min_day------|Average minutes per day spent using apps-------------|Numeric-|  
|screen_on_time_hours_day----|Total screen-on time per day (hours)-----------------|Numeric  
|battery_drain_m_ah_day------|Battery percentage drained per day-------------------|Numeric  
|number_of_apps_installed----|Total number of apps installed-----------------------|Numeric  
|data_usage_mb_day-----------|Data used per day in megabytes-----------------------|Numeric  
|user_behavior_class---------|Categorical rating from Minimal (1) to Power (5)-----|Factor   
|user_behavior_class_label---|Categorical rating from Minimal to Power user (label)|Factor   
|age_group-------------------|Age group (e.g., 18–24, 25–34, etc.)-----------------|Categorical   
|gender----------------------|Male / Female----------------------------------------|Categorical  
|operating_system------------|Android / iOS----------------------------------------|Factor  

        
## Shiny app flow      
1. **Load processed data**
2. **Read helper script**
3. **UI Elements**  
4. **Server Logic**  
5. **Run the app**  
6. **Deploy the app**  



