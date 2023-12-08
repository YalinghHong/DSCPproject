library(shiny)
library(leaflet)
library(dplyr)
library(readr)
library(plotly)

data <- read_csv("summary.csv")

data <- data %>%
  mutate(AVG_PRCP = ifelse(AVG_PRCP > 200, NA, AVG_PRCP)) %>%
  mutate(AVG_TAVG = ifelse(AVG_TAVG > 1000, NA, AVG_TAVG)) %>%
  filter(YEAR >= 2000)

data$YEAR <- as.character(data$YEAR)

ui <- fluidPage(
  titlePanel("Comparison of global weather station data🌧☀️️"),
  tabsetPanel(
    tabPanel("Map Comparison", sidebarLayout(
      sidebarPanel(
        selectInput("variable1", "Select the data to view the first map:",
                    choices = c("Average precipitation🌧️" = "AVG_PRCP", "Average temperature☀️" = "AVG_TAVG")),
        selectInput("year1", "Select the year of the first map:", choices = unique(data$YEAR)),
        
        selectInput("variable2", "select the data to view the second map:",
                    choices = c("Average precipitation🌧️" = "AVG_PRCP", "Average temperature☀️" = "AVG_TAVG")),
        selectInput("year2", "Select the year of the second map:", choices = unique(data$YEAR))
      ),
      mainPanel(
        leafletOutput("map1", height = "300px"),
        leafletOutput("map2", height = "300px"),
        leafletOutput("map3", height = "300px")
      )
    )),
    tabPanel("Station Data", sidebarLayout(
      sidebarPanel(
        selectInput("stationName", "Select a station:", choices = unique(data$NAME))
      ),
      mainPanel(
        plotlyOutput("lineChart"),
        leafletOutput("stationMap", height = "300px")
      )
    ))
  )
)

server <- function(input, output) {
  # Existing server code for the first tab...
  renderMap <- function(variable, year) {
    selected_data <- data %>%
      filter(YEAR == year, !is.na(AVG_PRCP), !is.na(AVG_TAVG)) %>%
      select(STATION, NAME, LATITUDE, LONGITUDE, AVG_PRCP, AVG_TAVG)
    
    color_intensity <- if(variable == "AVG_PRCP") {
      selected_data$AVG_PRCP / max(selected_data$AVG_PRCP, na.rm = TRUE)
    } else {
      selected_data$AVG_TAVG / max(selected_data$AVG_TAVG, na.rm = TRUE)
    }
    
    color <- if(variable == "AVG_PRCP") {
      scales::col_numeric(palette = "Blues", domain = NULL)(color_intensity)
    } else {
      scales::col_numeric(palette = "Reds", domain = NULL)(color_intensity)
    }
    
    popup_content <- if(variable == "AVG_PRCP") {
      paste("Station: ", selected_data$STATION, "<br>",
            "Name: ", selected_data$NAME, "<br>",
            "Avg Precipitation: ", selected_data$AVG_PRCP)
    } else {
      paste("Station: ", selected_data$STATION, "<br>",
            "Name: ", selected_data$NAME, "<br>",
            "Avg Temperature: ", selected_data$AVG_TAVG)
    }
    
    
    leaflet(selected_data) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~LONGITUDE, lat = ~LATITUDE,
        popup = ~popup_content,
        radius = 5, color = ~color, stroke = FALSE, fillOpacity = 0.8,
        layerId = ~STATION
        
      )
  }
  
  
  
  
  
  output$map1 <- renderLeaflet({
    renderMap(input$variable1, input$year1)
  })
  
  output$map2 <- renderLeaflet({
    renderMap(input$variable2, input$year2)
  })
  
  output$map3 <- renderLeaflet({
    data_year1 <- data %>%
      filter(YEAR == input$year1) %>%
      select(STATION, LATITUDE, LONGITUDE, AVG_PRCP, AVG_TAVG)
    
    data_year2 <- data %>%
      filter(YEAR == input$year2) %>%
      select(STATION, LATITUDE, LONGITUDE, AVG_PRCP, AVG_TAVG)
    
    comparison_data <- merge(data_year1, data_year2, by = "STATION")
    
    
    
    
    comparison_data$Change <- if(input$variable2 == "AVG_PRCP") {
      comparison_data$AVG_PRCP.y - comparison_data$AVG_PRCP.x
    } else {
      comparison_data$AVG_TAVG.y - comparison_data$AVG_TAVG.x
    }
    
    comparison_data$Color <- ifelse(is.na(comparison_data$Change), "lightgrey",
                                    ifelse(comparison_data$Change > 0, "orange", "green"))
    
    ##comparison_data$Color[comparison_data$Change == 0] <- "blue"
    
    leaflet(comparison_data) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~LONGITUDE.x, lat = ~LATITUDE.x,
        color = ~Color, stroke = FALSE, fillOpacity = 0.8,
        radius = 5, popup = ~paste(Change)
        
      ) %>%
      addLegend("bottomright", 
                title = "Legend", 
                colors = c("orange", "green", "lightgrey"), 
                labels = c("Increase", "Decrease or Unchange", "Station Closed"),
                opacity = 0.8)
    
  })
  ##
  
  
  
  output$lineChart <- renderPlotly({
    station_data <- data %>%
      filter(NAME == input$stationName) %>%
      group_by(YEAR) %>%
      summarize(Avg_Prcp = mean(AVG_PRCP, na.rm = TRUE), 
                Avg_Tavg = mean(AVG_TAVG, na.rm = TRUE))
    
    plot_ly(station_data, x = ~YEAR) %>%
      add_trace(y = ~Avg_Prcp, type = 'scatter', mode = 'lines+markers', name = 'Avg Precipitation') %>%
      add_trace(y = ~Avg_Tavg, type = 'scatter', mode = 'lines+markers', name = 'Avg Temperature') %>%
      layout(title = 'Average Precipitation and Temperature Over Time', xaxis = list(title = 'Year'), 
             yaxis = list(title = 'Value'), hovermode = 'closest')
  })
  
  output$stationMap <- renderLeaflet({
    station_location <- data %>%
      filter(NAME == input$stationName) %>%
      summarise(LATITUDE = mean(LATITUDE, na.rm = TRUE), LONGITUDE = mean(LONGITUDE, na.rm = TRUE)) %>%
      slice(1)
    
    if (nrow(station_location) > 0) {
      leaflet(station_location) %>%
        addTiles() %>%
        addMarkers(~LONGITUDE, ~LATITUDE, popup = input$stationName)
    }
  })
}

shinyApp(ui = ui, server = server)
