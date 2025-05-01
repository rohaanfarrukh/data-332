# uber data

[Shiny app](https://rohaanfarrukhdata332.shinyapps.io/uber/)


loading the data
```r
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
```
## bar charts
chart for trips by hour
```r
 
  output$trips_by_hour <- renderPlot({
    filtered_data() %>%
      count(hour) %>%
      ggplot(aes(x = hour, y = n)) +
      geom_col(fill = "steelblue") +
      labs(title = "Trips by Hour", x = "Hour", y = "Trips") +
      theme_minimal()
  })
```

chart for trips for every hour months
```r
output$trips_every_hour <- renderPlot({
    uber_data %>%
      count(hour) %>%
      ggplot(aes(x = hour, y = n)) +
      geom_col(fill = "purple") +
      labs(title = "Trips Every Hour (All Months)", x = "Hour", y = "Total Trips") +
      theme_minimal()
  })
```

chart for trips by day
```r
  output$trips_by_day <- renderPlot({
    filtered_data() %>%
      count(day) %>%
      ggplot(aes(x = day, y = n)) +
      geom_col(fill = "darkgreen") +
      labs(title = "Trips by Day", x = "Day of Month", y = "Trips") +
      theme_minimal()
  })
```

chart for trips by day of the month

```r
  output$trips_by_day_month <- renderPlot({
    uber_data %>%
      count(weekday, month_label) %>%
      ggplot(aes(x = weekday, y = n, fill = month_label)) +
      geom_col(position = "dodge") +
      labs(title = "Trips by Day of Week and Month", x = "Day of Week", y = "Trips") +
      theme_minimal()
  })
```

chart for trips by base
```r
  output$trips_by_base_month <- renderPlot({
    uber_data %>%
      count(Base, month_label) %>%
      ggplot(aes(x = Base, y = n, fill = month_label)) +
      geom_col(position = "dodge") +
      labs(title = "Trips by Base and Month", x = "Base", y = "Trips") +
      theme_minimal()
  })
```

chart for trips by month
```r
  output$trips_by_month <- renderPlot({
    uber_data %>%
      count(month_label) %>%
      ggplot(aes(x = month_label, y = n)) +
      geom_col(fill = "coral") +
      labs(title = "Trips by Month", x = "Month", y = "Total Trips") +
      theme_minimal()
  })
```

## Heatmap
heatmap hour day 
```r
  output$heatmap_hour_day <- renderPlot({
    uber_data %>%
      count(hour, weekday) %>%
      ggplot(aes(x = hour, y = weekday, fill = n)) +
      geom_tile() +
      labs(title = "Heatmap: Hour vs day", x = "Hour", y = "day") +
      theme_minimal()
  })
```

heatmap month day
```r
  output$heatmap_month_day <- renderPlot({
    uber_data %>%
      count(month_label, day) %>%
      ggplot(aes(x = day, y = month_label, fill = n)) +
      geom_tile() +
      labs(title = "Heatmap: Day vs Month", x = "Day", y = "Month") +
      theme_minimal()
  })
```

heatmap month weak
```r
  output$heatmap_month_week <- renderPlot({
    uber_data %>%
      count(month_label, week) %>%
      ggplot(aes(x = week, y = month_label, fill = n)) +
      geom_tile() +
      labs(title = "Heatmap: Week vs Month", x = "Week", y = "Month") +
      theme_minimal()
  })
```

heatmap base vs day of the week
```r
  output$heatmap_base_weekday <- renderPlot({
    uber_data %>%
      count(Base, weekday) %>%
      ggplot(aes(x = weekday, y = Base, fill = n)) +
      geom_tile() +
      labs(title = "Heatmap: Base vs Day of Week", x = "Weekday", y = "Base") +
      theme_minimal()
  })
```

## leaflet
```r
  output$trip_map <- renderLeaflet({
    leaflet(data = uber_data[1:500, ]) %>%
      addTiles() %>%
      addCircleMarkers(~Lon, ~Lat, radius = 2, color = "blue", fillOpacity = 0.5)
  })
```

## prediction model
```r
 output$model_output_plot <- renderPlot({
    model_data <- filtered_data() %>% count(hour)
    model <- lm(n ~ hour, data = model_data)
    ggplot(model_data, aes(x = hour, y = n)) +
      geom_point(color = 'blue') +
      geom_smooth(method = 'lm', color = 'red') +
      labs(title = 'Linear Regression: Trips vs Hour', x = 'Hour of Day', y = 'Trip Count') +
      theme_minimal()
  })
```

