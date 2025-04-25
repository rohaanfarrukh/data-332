
library(rsconnect)

library(ggplot2)
library(dplyr)
library(tidyverse)
library(tidyr)
library(shiny)
library(DT)
library(plotly)
library(bslib)
library(shinycssloaders)
library(httr)
library(readxl)
library(stringr)
rm(list = ls())

library(janitor)

# Fix GitHub blob/preview links to raw links
fix_raw_url <- function(url) {
  if (grepl("github.com", url) && grepl("blob", url)) {
    url <- gsub("github.com", "raw.githubusercontent.com", url)
    url <- gsub("/blob", "", url)
  }
  return(url)
}

# Read CSV or Excel file from GitHub
read_student_data <- function(url) {
  url <- fix_raw_url(url)
  ext <- tools::file_ext(url)
  temp_file <- tempfile(fileext = paste0(".", ext))
  GET(url, write_disk(temp_file, overwrite = TRUE))
  
  if (ext == "csv") {
    read.csv(temp_file, stringsAsFactors = FALSE)
  } else if (ext %in% c("xlsx", "xls")) {
    read_excel(temp_file)
  } else {
    stop("Unsupported file type: ", url)
  }
}

# Clean and standardize each dataset
clean_dataset <- function(df, dataset_id) {
  df <- janitor::clean_names(df)
  print(names(df))
  
  switch(dataset_id,
         
         # Dataset: nissou
         "nissou" = {
           df <- df %>%
             rename(init_speed = speed_mph, vehicle_type = type_of_car) %>%
             mutate(final_speed = as.numeric(init_speed), speed_change = 0)
         },
         
         # Dataset: retflipper
         "retflipper" = {
           df <- df %>%
             rename(init_speed = initial_read,
                    final_speed = final_read,
                    vehicle_type_code = type_of_car) %>%
             mutate(
               init_speed = as.numeric(init_speed),
               final_speed = as.numeric(final_speed),
               speed_change = ifelse(init_speed != final_speed, 1, 0),
               vehicle_type = recode(vehicle_type_code,
                                     "1" = "Emergency",
                                     "2" = "Hatchback",
                                     "3" = "Sedan",
                                     "4" = "SUV",
                                     "5" = "Van",
                                     "6" = "Minivan",
                                     "7" = "Motorcycle",
                                     "8" = "Coupe",
                                     "9" = "Truck",
                                     "10" = "Pickup Truck",
                                     .default = "Unknown")
             )
         },
         
         # Dataset: kritan
         "kritan" = {
           df <- df %>%
             rename(init_speed = initial_speed,
                    final_speed = final_speed,
                    speed_change = difference,
                    vehicle_type = body_style)
         },
         
         # Dataset: nick
         "nick" = {
           df <- df %>%
             rename(init_speed = mph,
                    vehicle_type = vehicle_style,
                    slow_flag = if_they_slow_down_yes_no) %>%
             mutate(
               final_speed = as.numeric(init_speed),
               speed_change = ifelse(grepl("yes", slow_flag, ignore.case = TRUE), 1, 0)
             )
         },
         
         # Dataset: tommy
         "tommy" = {
           df <- df %>%
             rename(init_speed = speed, vehicle_type = type_of_car) %>%
             mutate(
               final_speed = as.numeric(init_speed),
               speed_change = 0
             )
         },
         
         # Dataset: rohaan
         "rohaan" = {
           df <- df %>%
             rename(init_speed = init_speed,
                    final_speed = final_speed,
                    speed_change = speed_change,
                    vehicle_type = vehicle_type)
         },
         
         {
           stop("⚠️ No cleaning rule for dataset:", dataset_id)
         }
  )
  
  # Final cleanup: make sure types are correct and no NA in key columns
  df <- df %>%
    mutate(
      init_speed = as.numeric(init_speed),
      final_speed = as.numeric(final_speed),
      speed_change = as.numeric(speed_change),
      vehicle_type = str_to_title(as.character(vehicle_type))
    ) %>%
    filter(!is.na(init_speed), !is.na(final_speed), !is.na(speed_change))
  
  df[, c("init_speed", "final_speed", "speed_change", "vehicle_type")]
  df <- df %>%
    mutate(vehicle_type = str_to_lower(vehicle_type)) %>%
    mutate(vehicle_type = recode(vehicle_type,
                                 "suv" = "SUV",
                                 "suv " = "SUV",
                                 "suvs" = "SUV",
                                 "van" = "Van",
                                 "minivan" = "Van",
                                 "sevan" = "Sedan",
                                 "pickup truck" = "Truck",
                                 "pickup_truck" = "Truck",
                                 "truck" = "Truck",
                                 "sedan" = "Sedan",
                                 "sedans" = "Sedan",
                                 "sadan" = "Sedan",
                                 .default = str_to_title(vehicle_type)
    ))
}

# URLs mapped to dataset IDs
urls <- list(
  nissou = "https://raw.githubusercontent.com/nissou62/The-very-basics-of-R/refs/heads/main/shinymtcar_project/Data_Counting_Cars.csv",
  retflipper = "https://raw.githubusercontent.com/retflipper/DATA332_CountingCars/refs/heads/main/data/Counting_Cars.csv",
  kritan = "https://github.com/kritansth/data332/blob/main/counting_cars/cars_count.xlsx",
  nick = "https://raw.githubusercontent.com/nickhc41703/Data_332_assignments/refs/heads/main/Homework/counting_cars/counting_cars_final.csv",
  tommy = "https://raw.githubusercontent.com/TommyAnderson/Car-Data-Analysis/refs/heads/main/Car%20Data%20Collection.csv",
  rohaan = "https://raw.githubusercontent.com/rohaanfarrukh/data332_counting_cars/refs/heads/main/counting_cars_project/rscript/speed_counting_cars1.csv"
)

# Load and clean all datasets
all_data <- lapply(names(urls), function(id) {
  tryCatch({
    raw_df <- read_student_data(urls[[id]])
    clean_dataset(raw_df, id)
  }, error = function(e) {
    warning(paste("❌ Error with", id, ":", e$message))
    return(NULL)
  })
})

# Combine all non-null datasets into one dataframe
df <- bind_rows(Filter(Negate(is.null), all_data))

# UI
ui <- fluidPage(
  theme = bs_theme(bootswatch = "darkly", base_font = font_google("Roboto Mono")),
  
  titlePanel("🚘 Vehicle Speed Analysis Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("vehicle_filter", "Select Vehicle Types:",
                         choices = NULL)
    ),
    
    mainPanel(
      fluidRow(
        column(4, wellPanel(h4("Total Observations"), textOutput("total_obs"))),
        column(4, wellPanel(h4("Avg Initial Speed"), textOutput("avg_init"))),
        column(4, wellPanel(h4("Avg Final Speed"), textOutput("avg_final")))
      ),
      
      tabsetPanel(
        tabPanel("Scatter Plot",
                 withSpinner(plotlyOutput("speed_plot"))
        ),
        
        tabPanel("Bar Chart & Averages",
                 withSpinner(plotOutput("bar_chart")),
                 h4("Average Initial Speeds by Vehicle Type"),
                 DTOutput("avg_init_speed_table"),
                 h4("Average Final Speeds by Vehicle Type"),
                 DTOutput("avg_final_speed_table")
        ),
        
        tabPanel("Flashing Sign Effect",
                 h4("Effect of Flashing Sign on Speed (Flashing = 1)"),
                 withSpinner(plotOutput("flashing_effect_plot"))
        ),
        tabPanel("Min / Max / Mean Speeds",
                 withSpinner(plotOutput("min_max_mean_plot"))
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  observe({
    updateCheckboxGroupInput(session, "vehicle_filter",
                             choices = unique(df$vehicle_type),
                             selected = unique(df$vehicle_type))
  })
  
  filtered_data <- reactive({
    req(input$vehicle_filter)
    df %>% filter(vehicle_type %in% input$vehicle_filter)
  })
  
  output$total_obs <- renderText({ nrow(filtered_data()) })
  output$avg_init <- renderText({ round(mean(filtered_data()$init_speed, na.rm = TRUE), 2) })
  output$avg_final <- renderText({ round(mean(filtered_data()$final_speed, na.rm = TRUE), 2) })
  
  output$speed_plot <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = init_speed, y = final_speed, color = vehicle_type)) +
      geom_point(alpha = 0.7, size = 3) +
      labs(title = "Initial vs Final Speed",
           x = "Initial Speed",
           y = "Final Speed",
           color = "Vehicle Type") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$bar_chart <- renderPlot({
    avg_df <- filtered_data() %>%
      filter(speed_change == 1) %>%
      group_by(vehicle_type, speed_change) %>%
      summarise(
        init = mean(init_speed, na.rm = TRUE),
        final = mean(final_speed, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      pivot_longer(cols = c("init", "final"), names_to = "Speed_Type", values_to = "Average_Speed") %>%
      mutate(Speed_Type = factor(Speed_Type, levels = c("init", "final")))
    
    ggplot(avg_df, aes(x = vehicle_type, y = Average_Speed, fill = Speed_Type)) +
      geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
      facet_wrap(~ speed_change, labeller = label_both) +
      labs(title = "Average Speeds by Vehicle Type (Speed Change = 1)",
           x = "Vehicle Type",
           y = "Average Speed",
           fill = "Speed Type") +
      theme_minimal()
  })
  
  output$avg_init_speed_table <- renderDT({
    avg_init <- filtered_data() %>%
      group_by(vehicle_type) %>%
      summarise(Average_Initial_Speed = round(mean(init_speed, na.rm = TRUE), 2))
    datatable(avg_init, options = list(dom = 't'))
  })
  
  output$avg_final_speed_table <- renderDT({
    avg_final <- filtered_data() %>%
      group_by(vehicle_type) %>%
      summarise(Average_Final_Speed = round(mean(final_speed, na.rm = TRUE), 2))
    datatable(avg_final, options = list(dom = 't'))
  })
  
  output$flashing_effect_plot <- renderPlot({
    flashing_df <- df %>%
      filter(flashing == 1) %>%
      pivot_longer(cols = c(init_speed, final_speed),
                   names_to = "Speed_Type",
                   values_to = "Speed") %>%
      mutate(Speed_Type = recode(Speed_Type, init_speed = "Initial", final_speed = "Final"),
             Speed_Type = factor(Speed_Type, levels = c("Initial", "Final")))
    
    ggplot(flashing_df, aes(x = Speed_Type, y = Speed, fill = Speed_Type)) +
      geom_violin(trim = FALSE, alpha = 0.5) +
      geom_boxplot(width = 0.1, color = "black", outlier.shape = NA) +
      labs(title = "Speed Distribution Before and After Flashing Sign",
           x = "Speed Type",
           y = "Speed (mph)") +
      theme_minimal()
  })
  output$min_max_mean_plot <- renderPlot({
    stats_df <- filtered_data() %>%
      pivot_longer(cols = c(init_speed, final_speed), 
                   names_to = "Speed_Type", 
                   values_to = "Speed") %>%
      group_by(vehicle_type, Speed_Type) %>%
      summarise(
        Min = min(Speed, na.rm = TRUE),
        Mean = mean(Speed, na.rm = TRUE),
        Max = max(Speed, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      pivot_longer(cols = c(Min, Mean, Max), names_to = "Stat", values_to = "Value") %>%
      mutate(
        Speed_Type = recode(Speed_Type, init_speed = "Initial", final_speed = "Final")
      )
    
    ggplot(stats_df, aes(x = vehicle_type, y = Value, fill = Stat)) +
      geom_bar(stat = "identity", position = "dodge") +
      facet_wrap(~ Speed_Type) +
      labs(title = "Min, Mean, and Max Speeds by Vehicle Type",
           x = "Vehicle Type",
           y = "Speed (mph)",
           fill = "Statistic") +
      theme_minimal()
  })
  
}

shinyApp(ui = ui, server = server)

