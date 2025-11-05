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

source("helpers.R")
# ---- UI ----
ui <- page_fluid(
  
  titlePanel("Interactive EDA: Mobile User Behavior"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
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
      uiOutput("num_range2_ui"),
      
      # dynamic range slider
    # --- Apply button: subsetting is applied ONLY when this is pressed ---
    actionButton("apply_filters", "Apply Subset", class = "btn btn-primary"),
    br(),
    helpText("Note: Adjust selections, then click 'Apply Subset' to update data across tabs.")
  ),
  
  mainPanel(
    width = 9,
    tabsetPanel(
      id = "tabs",
      # ---- About (default) ----
      tabPanel(
        title = "About",
        value = "about",
        h3("About This App"),
        p(strong("Purpose:"), "Explore mobile users behavior with interactive subsetting and Exploratory Data Analysis (EDA)."),
        p(strong("About Dataset:"), "This dataset provides a comprehensive analysis of mobile device usage patterns 
        and user behavior classification. It contains 700 samples of user data, including metrics such as app usage time, 
        screen-on time, battery drain, and data consumption."),
        p(
          "This app uses the ",
          a(
            href = "https://www.kaggle.com/datasets/valakhorasani/mobile-device-usage-and-user-behavior-dataset",
            target = "_blank",
            "Mobile Device Usage and User Behavior Dataset "
          ),
          "from ",
          img(
            src = "https://upload.wikimedia.org/wikipedia/commons/7/7c/Kaggle_logo.png",
            alt = "Kaggle Logo",
            width = "60px",
            style = "vertical-align: middle; margin-left: 6px;"
          ),
          "."
        )
        ,
        p(strong("Sidebar:"), "Choose categorical levels to include and define ranges for two numeric variables. ",
          "Click 'Apply Subset' to update the dataset used by other tabs."),
        
        p(
          strong("Tabs:"), " ",
          br(),
          em(actionLink("go_download", "Data Download", style = "text-decoration: underline; cursor: pointer;")),#make it clickable
          " provides a view of the (subsetted) data and allows downloading.", 
          br(),
          em(actionLink("go_explore","Data Exploration", style ="text-decoration: underline; cursor: pointer;")),
          " provides numeric and graphical summaries including tables (one-/two-way) and plots (boxplots, histograms, scatter, heatmap), with options for coloring and faceting."
        )
        ,
        
        # Relevant image path or URL
        tags$img(src = "https://storage.googleapis.com/kaggle-datasets-images/5784553/9504237/8dad205de91ba816c0db61edef10aa15/dataset-cover.jpeg?t=2024-09-28-20-44-40",
                 alt = "Smartphone Illustration", width = "350",
                 style = "border: 1px solid #ddd; border-radius: 8px; padding: 6px;")
      ),
# ---- Data Download ----
      tabPanel(
        title = "Data Download",
        value = "download",
        h3("View & Download Data"),
        DTOutput("data_tbl") %>% withSpinner(type = 6),
        br(),
        downloadButton("dl_data", "Download CSV", class = "btn btn-success")
      ),      
# ---- Data Exploration ----
tabPanel(
  title = "Data Exploration",
  value = "explore",
  h3("Explore Summaries & Plots"),
  fluidRow(
    column(
      width = 4,
      # What to show: categorical tables, numeric summaries, or plots
      radioButtons(inputId="explore_mode", label="What would you like to explore?",
                   choices = c("Categorical summaries (one-/two-way tables)" = "cats",
                               "Numeric summaries by category" = "nums",
                               "Plots" = "plots"),
                   selected = "plots"),
      # Controls for tables / plots
      uiOutput("explore_controls_ui")
    ),
    column(
      width = 8,
      # Outputs
      uiOutput("explore_outputs_ui") # switches based on mode (tables/plots)
    )
  )
)
    )
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
  # Store subset in reactiveValues; only recompute when "Apply Subset" is pressed.
  rv <- reactiveValues(data = userBehavior) 
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
  
  # A convenience reactive that always returns the latest applied subset
  filtered_data <- reactive({
    rv$data
  })
  # ---------- Data Download Tab ----------
  output$data_tbl <- renderDT({
    dat <- filtered_data()
    validate(need(nrow(dat) > 0, "No rows after filtering. Adjust filters and click 'Apply Subset'."))
    datatable(dat, options = list(pageLength = 10, scrollX = TRUE),
              rownames = FALSE, filter = "top")
  })
  
  # Switch to "Data Download" tab
  observeEvent(input$go_download, {
    updateTabsetPanel(session, "tabs", selected = "download")
  })
  
  # Switch to "Data Exploration" tab
  observeEvent(input$go_explore, {
    updateTabsetPanel(session, "tabs", selected = "explore")
  })
  
  output$dl_data <- downloadHandler(
    filename = function() {
      paste0("userBehavior_subset_", Sys.Date(), ".csv")
    },
    content = function(file) {
      readr::write_csv(filtered_data(), file)
    }
  ) 
  # ---------- Data Exploration Controls (left pane) ----------
  output$explore_controls_ui <- renderUI({
    mode <- input$explore_mode
    
    if (mode == "cats") {
      fluidRow(
        h5("Categorical summaries"),
        selectInput("cat_oneway", "One-way table:",
                    choices = categorical_vars, selected = "user_behavior_class"),
        selectInput("cat_twoway_1", "Two-way: Variable 1",
                    choices = categorical_vars, selected = "gender"),
        selectInput("cat_twoway_2", "Two-way: Variable 2",
                    choices = categorical_vars, selected = "operating_system")
      )
    } else if (mode == "nums") {
      fluidRow(
        h5("Numeric summaries by category"),
        selectInput("num_summary_var", "Numeric variable:",
                    choices = numeric_vars, selected = "app_usage_time_min_day"),
        selectInput("by_cat", "Summarize by:",
                    choices = categorical_vars, selected = "user_behavior_class"),
        checkboxInput("show_sd", "Include standard deviation", TRUE)
      )
    } else {
      # Plot controls
      fluidRow(
        h5("Plots"),
        selectInput("plot_type", "Plot type:",
                    choices = c("Bar (categorical distribution)" = "bar_cat",
                                "Boxplot (numeric by category)" = "box_by_cat",
                                "Boxplot with Points (numeric by category)" = "box_by_cat_with_pts",
                                "Histogram (numeric, colored by category)" = "hist_num",
                                "Scatter (numeric vs numeric, colored by category)" = "scatter",
                                "Faceted Boxplots (3 metrics by class)" = "facet_boxes",
                                "Correlation Heatmap (0–1)" = "heatmap")),
        # Conditional controls based on plot type
        conditionalPanel(
          condition = "input.plot_type == 'bar_cat'",
          selectInput("bar_cat_var", "Categorical variable:",
                      choices = categorical_vars, selected = "user_behavior_class"),
          selectInput("bar_fill", "Fill (color) by:",
                      choices = c("None" = ".", categorical_vars), selected = ".")
        ),
        conditionalPanel(
          condition = "input.plot_type == 'box_by_cat'",
          selectInput("box_num", "Numeric variable:",
                      choices = numeric_vars, selected = "data_usage_mb_day"),
          selectInput("box_cat", "Category:",
                      choices = categorical_vars, selected = "user_behavior_class"),
          selectInput("box_fill", "Fill (color) by:",
                      choices = c("None" = ".", categorical_vars), selected = ".")
        ),
        
        conditionalPanel(
          condition = "input.plot_type == 'box_by_cat_with_pts'",
          selectInput("boxpt_num", "Numeric variable:",
                      choices = numeric_vars, selected = "screen_on_time_hours_day"),
          selectInput("boxpt_cat", "Category:",
                      choices = categorical_vars, selected = "gender"),
          selectInput("boxpt_fill", "Fill (color) by:",
                      choices = c("None" = ".", categorical_vars), selected = ".")
        ),
        conditionalPanel(
          condition = "input.plot_type == 'hist_num'",
          selectInput("hist_num_var", "Numeric variable:",
                      choices = numeric_vars, selected = "app_usage_time_min_day"),
          selectInput("hist_fill", "Color by category:",
                      choices = categorical_vars, selected = "operating_system"),
          sliderInput("hist_bins", "Bins:", min = 10, max = 80, value = 35, step = 5),
          checkboxInput("hist_density", "Show density overlay", FALSE)
        ),
        conditionalPanel(
          condition = "input.plot_type == 'scatter'",
          selectInput("sc_x", "X (numeric):", choices = numeric_vars, selected = "app_usage_time_min_day"),
          selectInput("sc_y", "Y (numeric):", choices = numeric_vars, selected = "data_usage_mb_day"),
          selectInput("sc_color", "Color by (category):", choices = categorical_vars, selected = "operating_system"),
          selectInput("sc_facet", "Facet by:", choices = c("None" = ".", categorical_vars), selected = "."),
          checkboxInput("sc_smooth", "Add linear smooth", TRUE),
          sliderInput("pt_alpha", "Point alpha:", min = 0.2, max = 1, value = 0.7, step = 0.05)
        ),
        conditionalPanel(
          condition = "input.plot_type == 'facet_boxes'",
          helpText("Shows 3 key metrics (app usage, screen-on time, battery drain) by class."),
          selectInput("facet_fill", "Fill (color) by category:", choices = categorical_vars, selected = "user_behavior_class")
        )
      )
    }
  })  
  # ---------- Data Exploration Outputs (right pane) ----------
  output$explore_outputs_ui <- renderUI({
    mode <- input$explore_mode
    if (mode == "cats") {
      fluidRow(
        h4("Categorical Summaries"),
        fluidRow(
          column(6, h6(paste("One-way Table for:", nice_var_label(input$cat_oneway))), tableOutput("oneway_tbl")), # add dynamic header
          column(6, h6(paste("Two-way Table for: ",nice_var_label(input$cat_twoway_1), "Vs",nice_var_label(input$cat_twoway_2))), tableOutput("twoway_tbl"))
        )
      )
    } else if (mode == "nums") {
      fluidRow(
        h4("Numeric Summaries by Category "),
        fluidRow(
         h6(paste(nice_var_label(input$num_summary_var), "Summarized by ",nice_var_label((input$by_cat)))), # add dynamic header
        tableOutput("num_summ_tbl")
      )
  )
    } else {
      fluidRow(
        h4("Plots"),
        plotOutput("main_plot", height = "560px") %>% withSpinner(type = 6)
      )
    }
  })
  # ---------- Tables ----------
  output$oneway_tbl <- renderTable({
    dat <- filtered_data()
    validate(need(nrow(dat) > 0, "No data after filtering."))
    v <- input$cat_oneway
    as.data.frame(table(dat[[v]]), stringsAsFactors = FALSE) |>
      `colnames<-`(c(nice_var_label(v), "Count"))
  })
  
  output$twoway_tbl <- renderTable({
    dat <- filtered_data()
    validate(need(nrow(dat) > 0, "No data after filtering."))
    v1 <- input$cat_twoway_1
    v2 <- input$cat_twoway_2
    validate(need(v1 != v2, "Choose two different variables for the two-way table."))
    as.data.frame.matrix(table(dat[[v1]], dat[[v2]]))
  }, rownames = TRUE)
  
  output$num_summ_tbl <- renderTable({
    dat <- filtered_data()
    validate(need(nrow(dat) > 0, "No data after filtering."))
    numv <- input$num_summary_var
    byc  <- input$by_cat
    tmp <- dat |>
      group_by(.data[[byc]]) |>
      summarise(
        Mean   = mean(.data[[numv]], na.rm = TRUE),
        Median = median(.data[[numv]], na.rm = TRUE),
        N      = dplyr::n(),
        .groups = "drop"
      )
    if (isTRUE(input$show_sd)) {
      tmp <- tmp |>
        left_join(dat |>
                    group_by(.data[[byc]]) |>
                    summarise(SD = sd(.data[[numv]], na.rm = TRUE),
                              .groups = "drop"),
                  by = byc)
    }
    tmp |>
      rename(!!nice_var_label(byc) := all_of(byc))
  })
  # ---------- Plots ----------
  output$main_plot <- renderPlot({
    dat <- filtered_data()
    validate(need(nrow(dat) > 0, "No data after filtering. Adjust filters and click 'Apply Subset'."))
    
    pt <- input$plot_type
    
    if (pt == "bar_cat") {
      fill_var <- input$bar_fill
      p <- ggplot(dat, aes(x = .data[[input$bar_cat_var]],
                           fill = if (fill_var == ".") NULL else .data[[fill_var]])) +
        geom_bar(position = "dodge") +
        labs(title = paste("Distribution of", nice_var_label(input$bar_cat_var)),
             x = nice_var_label(input$bar_cat_var), y = "Count",
             fill = if (fill_var == ".") NULL else nice_var_label(fill_var)) +
        theme_minimal()
      return(p)
    }
    # to exactly replicate the static plot I did for one of the box plots with manual color selection
    if (pt == "box_by_cat" && input$box_fill=="user_behavior_class_label" ) {
      fillb_var <- input$box_fill
      p <- ggplot(dat, aes(x = .data[[input$box_cat]],
                           y = .data[[input$box_num]],
                           fill = if (fillb_var == ".") NULL else .data[[fillb_var]]))  +
        geom_boxplot() +
        scale_fill_manual(
        values = c(
          "Minimal User" = "#00B050",   # green
          "Light User"   = "#92D050",   # light green
          "Moderate User"= "#FFC000",   # yellow
          "Heavy User"   = "#FF7043",   # orange
          "Power User"   = "#C00000"    # red
        ),
      name = "User Behavior Classes"
      )+
        labs(title = paste(nice_var_label(input$box_num), "by", nice_var_label(input$box_cat)),
             x = nice_var_label(input$box_cat), y = nice_var_label(input$box_num),
             fill = nice_var_label(fillb_var)) +
        theme_minimal()
      return(p)
    }
    
    if (pt == "box_by_cat" && input$box_fill!="user_behavior_class_label" ) {
      fillb_var <- input$box_fill
      p <- ggplot(dat, aes(x = .data[[input$box_cat]],
                           y = .data[[input$box_num]],
                           fill = if (fillb_var == ".") NULL else .data[[fillb_var]]))  +
        geom_boxplot() +
        labs(title = paste(nice_var_label(input$box_num), "by", nice_var_label(input$box_cat)),
             x = nice_var_label(input$box_cat), y = nice_var_label(input$box_num),
             fill = nice_var_label(fillb_var)) +
        theme_minimal()
      return(p)
    }
    
    
    if (pt == "box_by_cat_with_pts") {
      fillbp_var <- input$boxpt_fill
      p <- ggplot(dat, aes(x = .data[[input$boxpt_cat]],
                           y = .data[[input$boxpt_num]],
                           fill = if (fillbp_var == ".") NULL else .data[[fillbp_var]]))  +
        geom_boxplot() +
        geom_jitter(width = 0.2, alpha = 0.3)+
        labs(title = paste(nice_var_label(input$boxpt_num), "by", nice_var_label(input$boxpt_cat)),
             x = nice_var_label(input$boxpt_cat), y = nice_var_label(input$boxpt_num),
             fill = nice_var_label(fillbp_var)) +
        theme_minimal()
      return(p)
    }
    
    if (pt == "hist_num") {
      p <- ggplot(dat, aes(x = .data[[input$hist_num_var]],
                           fill = .data[[input$hist_fill]])) +
        geom_histogram(bins = input$hist_bins, position = "identity", alpha = 0.5) +
        labs(title = paste("Distribution of", nice_var_label(input$hist_num_var),
                           "by", nice_var_label(input$hist_fill)),
             x = nice_var_label(input$hist_num_var), y = "Count",
             fill = nice_var_label(input$hist_fill)) +
        theme_minimal()
      if (isTRUE(input$hist_density)) {
        p <- p + geom_density(position = "identity", alpha = 0.2, color = NA)
      }
      return(p)
    }
    
    if (pt == "scatter") {
      p <- ggplot(dat, aes(x = .data[[input$sc_x]], y = .data[[input$sc_y]],
                           color = .data[[input$sc_color]])) +
        geom_point(alpha = input$pt_alpha) +
        labs(title = paste(nice_var_label(input$sc_x), "vs", nice_var_label(input$sc_y)),
             x = nice_var_label(input$sc_x), y = nice_var_label(input$sc_y),
             color = nice_var_label(input$sc_color)) +
        theme_minimal()
      if (isTRUE(input$sc_smooth)) {
        p <- p + geom_smooth(method = "lm", se = FALSE, linewidth = 0.8, color = "black")
      }
      if (!is.null(input$sc_facet) && input$sc_facet != ".") {
        p <- p + facet_wrap(reformulate(input$sc_facet), scales = "free")
      }
      return(p)
    }
    
    if (pt == "facet_boxes") {
      long <- dat |>
        select(user_behavior_class,
               app_usage_time_min_day,
               screen_on_time_hours_day,
               battery_drain_m_ah_day,
               all_of(input$facet_fill)) |>
        pivot_longer(
          cols = c(app_usage_time_min_day, screen_on_time_hours_day, battery_drain_m_ah_day),
          names_to = "metric", values_to = "value"
        )
      p <- ggplot(long, aes(x = user_behavior_class, y = value,
                            fill = .data[[input$facet_fill]])) +
        geom_boxplot(outlier.alpha = 0.5) +
        facet_wrap(~ metric, scales = "free_y",
                   labeller = as_labeller(nice_var_label)) +
        labs(title = "Key Metrics by User Behavior Class",
             x = "User Behavior Class", y = "Value",
             fill = nice_var_label(input$facet_fill)) +
        theme_minimal()
      return(p)
    }
    
    if (pt == "heatmap") {
      # Correlation heatmap (0 -> 1 color scale)
      num <- dat |>
        select(all_of(numeric_vars))
      validate(need(ncol(num) > 1 && nrow(num) > 2,
                    "Not enough numeric data to compute correlations."))
      cm <- suppressWarnings(cor(num, use = "pairwise.complete.obs"))
      cm[cm < 0] <- 0  # clamp negatives to 0 if any
      cl <- melt(round(cm, 2))
      ggplot(cl, aes(Var1, Var2, fill = value)) +
        geom_tile(color = "white") +
        geom_text(aes(label = sprintf("%.2f", value)), size = 3) +
        scale_fill_gradientn(
          colours = c("white", "yellow", "orange", "red", "darkred"),
          limits  = c(0, 1),
          name    = "Correlation"
        ) +
        labs(title = "Correlation Heatmap (0–1 Scale)",
             x = NULL, y = NULL) +
        theme_minimal(base_size = 12) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  })
  
}



# Run the application 
shinyApp(ui = ui, server = server)