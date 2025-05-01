
library(shiny)
library(ggplot2)
library(dplyr)
library(lubridate)
library(readr)
library(leaflet)
library(rsconnect)
library(DT)


files <- list.files(pattern = "uber-raw-data-.*14.csv", full.names = TRUE)
uber_data <- files %>%
  lapply(read_csv) %>%
  bind_rows() %>%
  slice_sample(n = 100000) %>%  
  rename(datetime = `Date/Time`) %>%
  mutate(
    datetime = mdy_hms(datetime),
    hour = hour(datetime),
    day = day(datetime),
    month = month(datetime),
    month_label = month(datetime, label = TRUE, abbr = TRUE),
    weekday = wday(datetime, label = TRUE, abbr = TRUE),
    week = isoweek(datetime)
  )

uber_data$month <- as.integer(uber_data$month)

# UI
ui <- navbarPage("Uber NYC Trips Analysis",
                 
                 tabPanel("Bar Charts",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("selected_month", "Choose a month:", choices = unique(as.character(uber_data$month_label)), selected = as.character(uber_data$month_label[1]))
                            ),
                            mainPanel(
                              h4("Trips by Hour"),
                              plotOutput("trips_by_hour", height = "300px"),
                              p("This chart shows how ride demand varies throughout the day (hourly), helping identify peak times."),
                              
                              h4("Trips Every Hour (All Months)"),
                              plotOutput("trips_every_hour", height = "300px"),
                              p("This chart displays overall Uber trip volume across all months, by hour."),
                              
                              h4("Trips by Day of Month"),
                              plotOutput("trips_by_day", height = "300px"),
                              p("This chart shows daily trip distribution within the selected month."),
                              
                              h4("Trips by Day of Week and Month"),
                              plotOutput("trips_by_day_month", height = "300px"),
                              p("Displays ride frequency by weekday for each month, highlighting weekly trends."),
                              
                              h4("Trips by Base and Month"),
                              plotOutput("trips_by_base_month", height = "300px"),
                              p("Shows how each Uber dispatch base contributes to trip volume across months."),
                              
                              h4("Trips by Month"),
                              plotOutput("trips_by_month", height = "300px"),
                              p("Total number of Uber rides in each month (April–September 2014).")
                            )
                          )
                 ),
                 
                 tabPanel("Heatmaps",
                          fluidPage(
                            h4("Heatmap: Hour vs Weekday"),
                            plotOutput("heatmap_hour_day", height = "300px"),
                            p("Shows how Uber demand fluctuates during each hour of the day and by weekday."),
                            
                            h4("Heatmap: Month vs Day"),
                            plotOutput("heatmap_month_day", height = "300px"),
                            p("Displays daily trip distribution within each month."),
                            
                            h4("Heatmap: Month vs Week"),
                            plotOutput("heatmap_month_week", height = "300px"),
                            p("Highlights weekly trip patterns across months."),
                            
                            h4("Heatmap: Base vs Day of Week"),
                            plotOutput("heatmap_base_weekday", height = "300px"),
                            p("Visualizes which bases are most active across different weekdays.")
                          )
                 ),
                 
                 tabPanel("Trips Table",
                          fluidPage(
                            h4("Trips by Day of Month Table"),
                            dataTableOutput("trips_table"),
                            p("This table displays the total number of trips for each day of the month (up to 31 rows).")
                          )
                 ),
                 
                 tabPanel("Map",
                          leafletOutput("trip_map", height = "500px")
                 ),
                 
                 tabPanel("Prediction Model",
                          fluidPage(
                            h4("Linear Regression: Trips by Hour"),
                            plotOutput("model_output_plot", height = "300px"),
                            p("This plot visualizes a linear regression model that predicts the number of trips based on the hour of day for the selected month.")
                          )
                 )
)

# Server
server <- function(input, output, session) {
  
  filtered_data <- reactive({
    uber_data %>% filter(as.character(month_label) == input$selected_month)
  })
  
  output$trips_by_hour <- renderPlot({
    filtered_data() %>%
      count(hour) %>%
      ggplot(aes(x = hour, y = n)) +
      geom_col(fill = "steelblue") +
      labs(title = "Trips by Hour", x = "Hour", y = "Trips") +
      theme_minimal()
  })
  
  output$trips_every_hour <- renderPlot({
    uber_data %>%
      count(hour) %>%
      ggplot(aes(x = hour, y = n)) +
      geom_col(fill = "purple") +
      labs(title = "Trips Every Hour (All Months)", x = "Hour", y = "Total Trips") +
      theme_minimal()
  })
  
  output$trips_by_day <- renderPlot({
    filtered_data() %>%
      count(day) %>%
      ggplot(aes(x = day, y = n)) +
      geom_col(fill = "darkgreen") +
      labs(title = "Trips by Day", x = "Day of Month", y = "Trips") +
      theme_minimal()
  })
  
  output$trips_by_day_month <- renderPlot({
    uber_data %>%
      count(weekday, month_label) %>%
      ggplot(aes(x = weekday, y = n, fill = month_label)) +
      geom_col(position = "dodge") +
      labs(title = "Trips by Day of Week and Month", x = "Day of Week", y = "Trips") +
      theme_minimal()
  })
  
  output$trips_by_base_month <- renderPlot({
    uber_data %>%
      count(Base, month_label) %>%
      ggplot(aes(x = Base, y = n, fill = month_label)) +
      geom_col(position = "dodge") +
      labs(title = "Trips by Base and Month", x = "Base", y = "Trips") +
      theme_minimal()
  })
  
  output$trips_by_month <- renderPlot({
    uber_data %>%
      count(month_label) %>%
      ggplot(aes(x = month_label, y = n)) +
      geom_col(fill = "coral") +
      labs(title = "Trips by Month", x = "Month", y = "Total Trips") +
      theme_minimal()
  })
  
  output$heatmap_hour_day <- renderPlot({
    uber_data %>%
      count(hour, weekday) %>%
      ggplot(aes(x = hour, y = weekday, fill = n)) +
      geom_tile() +
      labs(title = "Heatmap: Hour vs Weekday", x = "Hour", y = "Weekday") +
      theme_minimal()
  })
  
  output$heatmap_month_day <- renderPlot({
    uber_data %>%
      count(month_label, day) %>%
      ggplot(aes(x = day, y = month_label, fill = n)) +
      geom_tile() +
      labs(title = "Heatmap: Day vs Month", x = "Day", y = "Month") +
      theme_minimal()
  })
  
  output$heatmap_month_week <- renderPlot({
    uber_data %>%
      count(month_label, week) %>%
      ggplot(aes(x = week, y = month_label, fill = n)) +
      geom_tile() +
      labs(title = "Heatmap: Week vs Month", x = "Week", y = "Month") +
      theme_minimal()
  })
  
  output$heatmap_base_weekday <- renderPlot({
    uber_data %>%
      count(Base, weekday) %>%
      ggplot(aes(x = weekday, y = Base, fill = n)) +
      geom_tile() +
      labs(title = "Heatmap: Base vs Day of Week", x = "Weekday", y = "Base") +
      theme_minimal()
  })
  
  output$trip_map <- renderLeaflet({
    leaflet(data = uber_data[1:500, ]) %>%
      addTiles() %>%
      addCircleMarkers(~Lon, ~Lat, radius = 2, color = "blue", fillOpacity = 0.5)
  })
  
  output$trips_table <- renderDataTable({
    uber_data %>%
      count(day) %>%
      arrange(day) %>%
      rename(`Day of Month` = day, `Total Trips` = n)
  })
  
  output$model_output_plot <- renderPlot({
    model_data <- filtered_data() %>% count(hour)
    model <- lm(n ~ hour, data = model_data)
    ggplot(model_data, aes(x = hour, y = n)) +
      geom_point(color = 'blue') +
      geom_smooth(method = 'lm', color = 'red') +
      labs(title = 'Linear Regression: Trips vs Hour', x = 'Hour of Day', y = 'Trip Count') +
      theme_minimal()
  })
}

# Run app
shinyApp(ui, server)
