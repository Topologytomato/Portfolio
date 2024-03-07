# shiny app for MA615 final project
library(shiny)
library(leaflet)
library(sf)
library(tidyverse)
# read the travel time data and clean data
data_combined_traveltime <- read.csv("data_combined_traveltime.txt")
data_ferry <- read.csv("data_ferry.txt")
traveltime <- unique(data_combined_traveltime[,1:5])
ferry <- unique(data_ferry[,1:5])
traveltime <- rbind(traveltime,ferry)
traveltime <- unique(traveltime)

traveltime <- traveltime %>% 
  group_by(route_id, route_desc, start_point, stop_point) %>% 
  summarise(travel_time_average = mean(travel_time))


# read geojson data for bus routes
bus <- st_read("bus.geojson")
ferry <- st_read("ferry.geojson")
other <- st_read("other.geojson")
rapid <- st_read("rapid.geojson")
routes <- rbind(bus, ferry, other, rapid)

# read bus stop data
bus_stops <- read.csv("bus_stops.csv")
rapid_stops <- read.csv("rapid_stops.csv")
ferry_stops <- read.csv("ferry_stops.csv")
other_stops <- read.csv("other_stops.csv")

# clean and combine the stops data
ferry_stops <- ferry_stops[, c("stop_id", "X", "Y")]
names(ferry_stops)[1] <- "stop_name"
col_stops <- c("stop_name", "X", "Y")
stops <- rbind(bus_stops[,col_stops], rapid_stops[,col_stops],
               other_stops[,col_stops],ferry_stops[,col_stops])
stops <- unique(stops)
stops <- stops %>% group_by(stop_name) %>% summarise(x = mean(X), y = mean(Y))
stops <- stops[,c("stop_name", "x", "y")]

# join with the lat and lon data for stops
data_plot <- traveltime %>% left_join(stops, by = c("start_point" = "stop_name"))
names(data_plot)[6] <- "start_lon"
names(data_plot)[7] <- "start_lat"

data_plot <- data_plot %>% left_join(stops, by = c("stop_point" = "stop_name"))
names(data_plot)[8] <- "stop_lon"
names(data_plot)[9] <- "stop_lat"

# clean na and other empty values
data_plot <- data_plot %>% filter(route_id!="0")
data_plot <- data_plot[complete.cases(data_plot),]


# Define UI for app
ui <- navbarPage(titlePanel("MBTA data display"),
                 tabPanel("Routes Map",
                          selectInput(inputId = "route_desc",
                                      label = "transportation_type",
                                      choices = unique(data_plot$route_desc)),
                          selectInput(inputId = "route_id",
                                      label = "route_id",
                                      choices = unique(data_plot$route_id)),
                          selectInput(inputId = "start_point",
                                      label = "Start",
                                      choices = NULL),
                          selectInput(inputId = "stop_point",
                                      label = "End",
                                      choices = NULL),
                          fluidPage(h2("MBTA route map"),
                                    leafletOutput("map"))),
                 tabPanel("Scheduled Travel Times of certain pair stops",
                          selectInput(inputId = "route_desc2",
                                      label = "transportation_type",
                                      choices = unique(data_plot$route_desc)),
                          selectInput(inputId = "route_id2",
                                      label = "route_id",
                                      choices = unique(data_plot$route_id)),
                          selectInput(inputId = "start_point2",
                                      label = "Start",
                                      choices = NULL),
                          fluidPage(
                            h2("Travel times compare"),
                            leafletOutput("map2"),
                            textOutput("traveltime"))),
                 tabPanel("EDA plot",
                          fluidPage(
                            h2("Compare1"),
                            plotOutput("plot1"))))


server <- function(input, output,session) {
  observeEvent(input$route_desc,{
    updateSelectInput(session, "route_id",
        choices = unique(data_plot[which(data_plot$route_desc == input$route_desc), "route_id"])
        )
  })
  
  observeEvent(input$route_id,{
      updateSelectInput(session, "start_point",
        choices = unique(data_plot[which(data_plot$route_desc == input$route_desc
                                         & data_plot$route_id == input$route_id), "start_point"]),
        selected = NULL)
    
    updateSelectInput(session, "stop_point",
        choices = unique(data_plot[which(data_plot$route_desc == input$route_desc
                                         & data_plot$route_id == input$route_id), "stop_point"]),
        selected = NULL)})
          
  output$map <- renderLeaflet({
    routes_row <- which(routes$route_id == input$route_id & 
                          routes$route_desc == input$route_desc)
    
    leaflet(routes[routes_row, ]) %>% 
      addProviderTiles(providers$Stamen.Toner) %>%
      addPolylines(color = "red", weight = 5) %>%
      addMarkers(lat = as.numeric(stops[which(stops$stop_name == input$start_point), "y"]),
                 lng = as.numeric(stops[which(stops$stop_name == input$start_point), "x"]), 
                 label = as.character(input$start_point)) %>%
      addMarkers(lat = as.numeric(stops[which(stops$stop_name == input$stop_point), "y"]),
                 lng = as.numeric(stops[which(stops$stop_name == input$stop_point), "x"]),
                 label = as.character(input$stop_point))
  })
  
  observeEvent(input$route_desc2,{
    updateSelectInput(session, "route_id2",
                      choices = unique(data_plot[which(data_plot$route_desc == input$route_desc2), "route_id"])
    )
  })
  
  observeEvent(input$route_id2,{
    updateSelectInput(session, "start_point2",
                      choices = unique(data_plot[which(data_plot$route_desc == input$route_desc2
                                                       & data_plot$route_id == input$route_id2), "start_point"]),
                      selected = NULL)
    })
  
  output$map2 <- renderLeaflet({
    
    data_aaa <- data_plot %>% 
      filter(route_desc == input$route_desc2,
             route_id == input$route_id2,
             start_point == input$start_point2)
    
    routes_row <- which(routes$route_id == input$route_id & 
                          routes$route_desc == input$route_desc)
    
    leaflet(routes[routes_row, ]) %>% 
      addProviderTiles(providers$Stamen.Toner) %>%
      addPolylines(color = "red", weight = 5) %>%
      addMarkers(lat = as.numeric(data_aaa[1, "start_lat"]),
                 lng = as.numeric(data_aaa[1, "start_lon"]), 
                 label = as.character(input$start_point2)) %>%
      addMarkers(lat = as.numeric(data_aaa[1, "stop_lat"]),
                 lng = as.numeric(data_aaa[1, "stop_lon"]), 
                 label = as.character(data_aaa[1, "stop_point"]))
  })
  
  text_change <- reactive({
    paste0("Travel time from ",
           input$start_point2, " to ", 
           as.character(data_plot[which(data_plot$route_desc == input$route_desc2 &
                    data_plot$route_id == input$route_id2 &
                    data_plot$start_point == input$start_point2), "stop_point"]), 
           " is ", as.character(data_plot[which(data_plot$route_desc == input$route_desc2 &
                                                  data_plot$route_id == input$route_id2 &
                                                  data_plot$start_point == input$start_point2), "travel_time_average"])) 
  })
  
  output$traveltime <- renderText({text_change()})
  
  output$plot1 <- renderPlot({
    data_scheduletime <- data_combined_traveltime %>% group_by(route_id, route_desc, travel_time, month) %>% summarise(count = n())
    
    ggplot(data_scheduletime) +
      aes(x = travel_time, y = route_desc) +
      geom_boxplot(fill = "#BFD971") +
      labs(x = "scheduled travel time", y = "route type") +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 18L,
                                  hjust = 0.5),
        axis.title.y = element_text(size = 10L),
        axis.title.x = element_text(size = 10L)
      )
  })
}

shinyApp(ui, server)

