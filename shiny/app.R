# ---- libraries ----
library(shiny)
library(bslib)
library(dplyr)
library(tidyverse)
library(tidyr)
library(ggplot2)
library(DT)
library(reshape2)
library(scales)
library(shinycssloaders)
library(forcats)
library(stringr)
library(janitor)


#load my dataset

shinydata <- readRDS("../data/userbehavior.rds")

# ---- Helpers ----
numeric_vars <- c(
  "app_usage_time_min_day",
  "screen_on_time_hours_day",
  "battery_drain_m_ah_day",
  "number_of_apps_installed",
  "data_usage_mb_day",
  "age"
)

categorical_vars <- c(
  "gender",
  "operating_system",
  "user_behavior_class"
)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Interactive EDA: Mobile User Behavior"),

    sidebarPanel(
      width = 3,
      h4("Subset Data"),
      
      # --- Categorical filters w/ "All" ---
      # 1) Gender
      checkboxGroupInput(
        "f_gender", "Gender",
        choices = c("Male", "Female"),
        selected = c("Male", "Female") # "All" by default
      ),
      
      # 2) Operating System
      checkboxGroupInput(
        "f_os", "Operating System",
        choices = c("Android", "iOS"),
        selected = c("Android", "iOS") # "All" by default
      ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("explore_outputs_ui")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) 

  # ---------- Apply subset on demand ----------
  # Store subset in reactiveValues; only recompute when "Apply Subset" is pressed.
  rv <- reactiveValues(data = shinydata)
  
  observeEvent(input$apply_filters, {
    dat <- shinydata
    
    # Categorical filters
    if (length(input$f_gender)) {
      dat <- dat %>% filter(gender %in% input$f_gender)
    }
    if (length(input$f_os)) {
      dat <- dat %>% filter(operating_system %in% input$f_os)
    }
    
    
    rv$shinydata <- dat
  }, ignoreInit = TRUE)

# Run the application 
shinyApp(ui = ui, server = server)
