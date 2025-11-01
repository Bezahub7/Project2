# ---- call libraries ----
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

# call my derived dataset
userBehavior <- readRDS("./data/userbehavior_der.rds")
userBehavior

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
# to nicely label the sliders
nice_var_label <- function(x) {
  x |>
    str_replace_all("_", " ") |>
    str_to_title()
}

# ---- UI ----
ui <- page_fluid(
  
  titlePanel("Interactive EDA: Mobile User Behavior"),
  sidebarLayout(
    sidebarPanel(
      width = 4,
      h4("Subset Data"),
      
      # --- Categorical filters w/ "All" ---
      # 1) Gender
      checkboxGroupInput(
        inputId="f_gender",
        label   = "Gender",
        choices = c("Male", "Female"),
        selected = c("Male", "Female") # "All" by default
      ),
      
      # 2) Operating System
      checkboxGroupInput(
        inputId="f_os", 
        label   ="Operating System",
        choices = c("Android", "iOS"),
        selected = c("Android", "iOS") # "All" by default
      ),
      
      
      # --- Numeric Subsetting: choose variable, then dynamic 2-value slider ---
      h5("Numeric filter 1"),
      selectInput(inputId="num_var1",
                  label   ="Select numeric variable:",
                  choices = numeric_vars, selected = "app_usage_time_min_day"),
      uiOutput("num_range1_ui"),   # dynamic range slider
      
      h5("Numeric filter 2"),
      selectInput(inputId="num_var2", 
                  label   ="Select numeric variable:",
                  choices = numeric_vars, selected = "data_usage_mb_day"),
      uiOutput("num_range2_ui")
      ),
      # dynamic range slider

 mainPanel(
   
   )
    )
  )



# ---- SERVER ----
server <- function(input, output, session) {
  
  # ---------- Dynamic numeric range sliders ----------
  # We show ranges based on full data (not subsetted), so users can pick any range they like.
  output$num_range1_ui <- renderUI({
    v <- input$num_var1
    rng <- range(userBehavior[[v]], na.rm = TRUE)
    sliderInput("num_range1", label = paste("Range for", nice_var_label(v)),
                min = floor(rng[1]), max = ceiling(rng[2]),
                value = c(floor(rng[1]), ceiling(rng[2])),
                step = diff(rng)/100)
  })
  
  output$num_range2_ui <- renderUI({
    v <- input$num_var2
    rng <- range(userBehavior[[v]], na.rm = TRUE)
    sliderInput("num_range2", label = paste("Range for", nice_var_label(v)),
                min = floor(rng[1]), max = ceiling(rng[2]),
                value = c(floor(rng[1]), ceiling(rng[2])),
                step = diff(rng)/100)
  })
  
  # ---------- Apply subset on demand ----------
  
  observeEvent(input$apply_filters, {
    dat <- userBehavior
    
    # Categorical filters
    if (length(input$f_gender)) {
      dat <- dat %>% filter(gender %in% input$f_gender)
    }
    if (length(input$f_os)) {
      dat <- dat %>% filter(operating_system %in% input$f_os)
    }
    
    # Numeric filters, access column names dynamically
    if (!is.null(input$num_range1) && !is.null(input$num_var1)) {
      dat <- dat %>% filter(.data[[input$num_var1]] >= input$num_range1[1],
                            .data[[input$num_var1]] <= input$num_range1[2])
    }
    if (!is.null(input$num_range2) && !is.null(input$num_var2)) {
      dat <- dat %>% filter(.data[[input$num_var2]] >= input$num_range2[1],
                            .data[[input$num_var2]] <= input$num_range2[2])
    }
    
    rv$data <- dat
  }, ignoreInit = TRUE)
  

  
 
  
}


# Run the application 
shinyApp(ui = ui, server = server)