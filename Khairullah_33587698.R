# Required Libraries
# Please install any libraries below as necessary
# install.packages("leaflet")
# install.packages("htmlwidgets")
# install.packages("plotly")
library(shiny)
library(leaflet)
library(dplyr)
library(htmlwidgets)
library(ggplot2)
library(plotly)
library(tidyr)
library(RColorBrewer)

# Set working directory and load data
# Please set the working directory accordingly
setwd('/Users/lutfimuzzakikhairullah/Documents/Monash/C6004/Semester03/FIT5147/Assignments/DVP')

# The used data below are final data that have previously wrangled with Python
flights <- read.csv('flights.csv') # wrangled flight data of on-time performance data
weather_log <- read.csv('weather_log.csv') # wrangled data based on weather-related cancellations

# Process the flight data
airport_summary <- flights %>%
  group_by(airport, state, state_name, icao, iata, latitude, longitude) %>%
  summarise(
    flight_count = n(),
    avg_dep_delay = mean(dep_delay, na.rm = TRUE),
    avg_arr_delay = mean(arr_delay, na.rm = TRUE),
    cancellation_count = sum(cancelled)
  )

# Define UI
ui <- fluidPage(
  # Set the title panel
  titlePanel("Flight Delays and Cancellations across the United States Airports in 2022"),
  sidebarLayout(
    # Set the components on the sidebar layout
    sidebarPanel(
      h4("User Guide"),
      p("This dashboard provides insights into flight delays and cancellations at various airports across the United States.
        Use the map to select an airport and explore the detailed reports for that location. The visualisations are interactive,
        and clicking on different elements will update the data displayed."),
      p("Visualisations Included:"),
      tags$ul(
        tags$li("Map: Shows airports with average departure delays. Click on a dot to select an airport."),
        tags$li("Delay Report: Stacked bar chart showing delays by reason for each airline at the selected airport."),
        tags$li("Cancellation Report: Cancellation Report: Pie chart showing the proportion of cancellations by reason at the selected airport."),
        tags$li("Weather Log: Heatmap showing weather conditions on days when weather-related cancellations occurred at the selected airport.")
      ),
      br(),
      p("Data Sources:"),
      tags$ul(
        tags$li("U.S. On-Time Performance Carrier Report from",
                tags$a(href = "https://www.transtats.bts.gov/DL_SelectFields.aspx?gnoyr_VQ=FGJ&QO_fu146_anzr=b0-gvzr",
                       "Bureau of Transportation Statistics (BTS)")),
        tags$li("U.S. Weather Events from",
                tags$a(href = "https://www.kaggle.com/datasets/sobhanmoosavi/us-weather-events/data",
                       "Moosavi et al. at provided in Kaggle"))
      ),
      br(),
      p("Hover over elements in the visualisations for detailed information.")
    ),
    
    # Set the components on the main panel layout
    mainPanel(
      leafletOutput("map"),
      h3(textOutput("chart_title")),
      p("This stacked bar chart provides a breakdown of flight delays by airline at the selected airport. Each bar is divided into
        sections representing different reasons for delays (Carrier, Weather, NAS, Security, Late Aircraft). Hover over a section to
        see detailed information about the delay count and average delay time."),
      plotlyOutput("bar_chart"),
      h3(textOutput("pie_chart_title")),
      p("The pie chart displays the reasons for flight cancellations at the selected airport. The chart segments represent different
        cancellation reasons (Carrier, Weather, NAS, Security). Hover over a segment to see the cancellation count and percentage."),
      textOutput("total_cancellations"),
      plotlyOutput("pie_chart"),
      h3(textOutput("heatmap_title")),
      p("The heatmap shows the weather conditions on days when weather-related cancellations occurred at the selected airport.
        The y-axis represents different weather types (Storm, Snow, Rain, Fog, Cold) and the x-axis represents severity levels
        (Heavy, Light, Moderate, Severe). The numbers in the boxes indicate the count of weather log occurrences. Hover over a
        box to see the count."),
      textOutput("total_logs"),
      plotlyOutput("heatmap")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Reactive value to store the selected airport
  selected_airport <- reactiveVal("Denver International Airport")
  
  # Render the map visualisation
  output$map <- renderLeaflet({
    # Define the color palette
    pal <- colorNumeric(palette = "YlOrRd", domain = airport_summary$avg_dep_delay)
    
    leaflet(data = airport_summary) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addCircleMarkers(
        lat = ~latitude, lng = ~longitude,
        radius = ~sqrt(sqrt(flight_count)),
        fillColor = ~pal(avg_dep_delay),
        stroke = TRUE, color = "#000000", weight = 1, fillOpacity = 0.7,
        popup = ~paste(
          "<strong>Airport Name:</strong>", airport, "<br>",
          "<strong>IATA/ICAO Code:</strong>", iata, "/", icao, "<br>",
          "<strong>Airport State:</strong>", state_name, " (", state, ")<br>",
          "<strong>Avg Departure Delay:</strong>", round(avg_dep_delay, 2), " mins<br>",
          "<strong>Avg Arrival Delay:</strong>", round(avg_arr_delay, 2), " mins<br>",
          "<strong>Flight Count:</strong>", flight_count, "<br>",
          "<strong>Cancellation Count:</strong>", cancellation_count
        ),
        label = ~paste(airport, " (", iata, "/", icao, ")"),
        labelOptions = labelOptions(
          direction = 'auto',
          offset = c(10, -10),
          opacity = 0.8,
          permanent = FALSE,
          textsize = "12px"
        ),
        layerId = ~airport
      ) %>%
      addLegend(
        position = "bottomright", pal = pal, values = ~avg_dep_delay,
        title = "Avg Departure Delay (mins)",
        opacity = 1
      ) %>%
      htmlwidgets::onRender("
        function(el, x) {
          var myMap = this;
          myMap.eachLayer(function(layer) {
            if (layer instanceof L.CircleMarker) {
              layer.on('mouseover', function(e) {
                myMap.eachLayer(function(l) {
                  if (l instanceof L.CircleMarker && l !== layer) {
                    l.setStyle({fillOpacity: 0.3});
                  }
                });
                this.setStyle({radius: this.options.radius * 1.5, fillOpacity: 1});
                this.openTooltip();
              });
              layer.on('mouseout', function(e) {
                myMap.eachLayer(function(l) {
                  if (l instanceof L.CircleMarker) {
                    l.setStyle({fillOpacity: 0.7});
                  }
                });
                this.setStyle({radius: this.options.radius / 1.5, fillOpacity: 0.7});
                this.closeTooltip();
              });
              layer.on('click', function(e) {
                var airportName = layer.options.layerId;
                console.log('Clicked airport:', airportName);
                Shiny.onInputChange('clicked_airport', airportName);
              });
            }
          });
        }
      ")
  })
  
  # Update the selected airport based on user interaction
  observeEvent(input$clicked_airport, {
    selected_airport(input$clicked_airport)
  })
  
  # Render the chart title
  output$chart_title <- renderText({
    paste("Delay Flights in", selected_airport())
  })
  
  # Render the stacked bar chart
  output$bar_chart <- renderPlotly({
    req(selected_airport())
    
    airport_data <- flights %>%
      filter(airport == selected_airport()) %>%
      group_by(op_carrier) %>%
      summarise(
        carrier_delay_count = sum(carrier_delay > 0, na.rm = TRUE),
        weather_delay_count = sum(weather_delay > 0, na.rm = TRUE),
        nas_delay_count = sum(nas_delay > 0, na.rm = TRUE),
        security_delay_count = sum(security_delay > 0, na.rm = TRUE),
        late_aircraft_delay_count = sum(late_aircraft_delay > 0, na.rm = TRUE),
        carrier_delay_avg = mean(carrier_delay[carrier_delay > 0], na.rm = TRUE),
        weather_delay_avg = mean(weather_delay[weather_delay > 0], na.rm = TRUE),
        nas_delay_avg = mean(nas_delay[nas_delay > 0], na.rm = TRUE),
        security_delay_avg = mean(security_delay[security_delay > 0], na.rm = TRUE),
        late_aircraft_delay_avg = mean(late_aircraft_delay[late_aircraft_delay > 0], na.rm = TRUE),
        avg_dep_delay = mean(dep_delay, na.rm = TRUE),
        avg_arr_delay = mean(arr_delay, na.rm = TRUE)
      ) %>%
      pivot_longer(
        cols = ends_with("_count"),
        names_to = "delay_reason",
        values_to = "delay_count"
      ) %>%
      mutate(
        delay_reason = sub("_count$", "", delay_reason),
        delay_reason = recode(delay_reason,
                              "carrier_delay" = "Carrier",
                              "weather_delay" = "Weather",
                              "nas_delay" = "NAS",
                              "security_delay" = "Security",
                              "late_aircraft_delay" = "Late Aircraft"),
        avg_delay_time = case_when(
          delay_reason == "Carrier" ~ carrier_delay_avg,
          delay_reason == "Weather" ~ weather_delay_avg,
          delay_reason == "NAS" ~ nas_delay_avg,
          delay_reason == "Security" ~ security_delay_avg,
          delay_reason == "Late Aircraft" ~ late_aircraft_delay_avg
        )
      ) %>%
      select(-ends_with("_avg"))
    
    p <- ggplot(airport_data, aes(x = op_carrier, y = delay_count, fill = delay_reason, text = paste(
      "<b>Airline:</b>", op_carrier, "<br><b>Count:</b>", delay_count, "<br><b>Reason:</b>", delay_reason,
      "<br><b>Avg Dep Delay:</b>", round(avg_dep_delay, 2), "mins",
      "<br><b>Avg Arr Delay:</b>", round(avg_arr_delay, 2), "mins"
      ))) +
      geom_bar(stat = "identity", color = "black", size = 0.1) +
      scale_fill_brewer(palette = "Paired") +
      labs(x = "Airline", y = "Delay Count", fill = "Delay Reasons") +
      theme_minimal() +
      # scale_x_discrete(guide = guide_axis(n.dodge = 2))
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))
    
    ggplotly(p, tooltip = "text") %>%
      layout(hoverlabel = list(bgcolor = "white", font = list(size = 12))) %>%
      animation_opts(frame = 100, transition = 100, redraw = FALSE)
  })
  
  # Render the pie chart title
  output$pie_chart_title <- renderText({
    paste("Cancellation Flights in", selected_airport())
  })
  
  # Render the total cancellations text
  output$total_cancellations <- renderText({
    airport_data <- flights %>%
      filter(airport == selected_airport(), nchar(cancellation_code) > 0)
    
    paste("Total Cancellations:", nrow(airport_data))
  })
  
  # Render the pie chart
  output$pie_chart <- renderPlotly({
    req(selected_airport())

    airport_data <- flights %>%
      filter(airport == selected_airport(), nchar(cancellation_code) > 0) %>%
      count(cancellation_code) %>%
      mutate(
        reason = case_when(
          cancellation_code == "A" ~ "Carrier (A)",
          cancellation_code == "B" ~ "Weather (B)",
          cancellation_code == "C" ~ "NAS (C)",
          cancellation_code == "D" ~ "Security (D)"
        ),
        percentage = n / sum(n) * 100
      )
    
    p <- plot_ly(airport_data, labels = ~reason, values = ~n, type = 'pie', textinfo = 'label+percent',
                 hoverinfo = 'text', text = ~paste("<b>Reason:</b>", reason, "<br><b>Count:</b>", n, "<br><b>Percentage:</b>", round(percentage, 2), "%"),
                 marker = list(colors = brewer.pal(n = 4, name = "Dark2"),
                               line = list(color = 'black', width = 0.5))) %>%
      layout(showlegend = TRUE,
             legend = list(title = list(text = "Cancellation Reasons")),
             hoverlabel = list(bgcolor = "white", font = list(size = 12)))
      
    p
  })
  
  # Render the heatmap title
  output$heatmap_title <- renderText({
    paste("Weather Log Occurrences for Weather-related Cancellations at", selected_airport())
  })
  
  # Render the total logs text
  output$total_logs <- renderText({
    # Get the weather-related cancellation
    # weather_cancellations <- flights %>%
    #   filter(airport == selected_airport(), cancellation_code == "B") %>%
    #   distinct(fl_date)
    
    # Filter weather log based on these dates
    weather_data <- weather_log %>%
      filter(airport == selected_airport())
    
    paste("Total Logs:", nrow(weather_data))
  })
  
  # Render the heatmap
  output$heatmap <- renderPlotly({
    req(selected_airport())
    
    # Get the weather-related cancellation
    # weather_cancellations <- flights %>%
    #   filter(airport == selected_airport(), cancellation_code == "B") %>%
    #   distinct(fl_date)
    
    # Filter weather log based on these dates
    weather_data <- weather_log %>%
      filter(airport == selected_airport())
    
    # Create a count of weather occurrences by type and severity
    weather_counts <- weather_data %>%
      group_by(type, severity) %>%
      summarise(count = n()) %>%
      ungroup() %>%
      complete(type, severity, fill = list(count = 0))
    
    # Create the heatmap plot
    heatmap_plot <- ggplot(weather_counts, aes(x = severity, y = type, fill = count, text = paste("<b>Count:</b>", count))) +
      geom_tile(color = "black", size = 0.5) +
      geom_text(aes(label = count), color = "black") +
      scale_fill_gradientn(colours = brewer.pal(9, "YlGnBu")) +
      labs(x = "Severity", y = "Weather Type", fill = "Occurrences") +
      theme_minimal()
    
    ggplotly(heatmap_plot, tooltip = "text") %>%
      layout(hoverlabel = list(bgcolor = "white", font = list(size = 12))) %>%
      animation_opts(frame = 100, transition = 100, redraw = FALSE)
  })
  
  # Set default selected airport
  selected_airport("Denver International Airport")
}

# Run the application
shinyApp(ui = ui, server = server)
