library(tidyverse)
library(ggplot2)
library(viridis)
library(lubridate)
library(sp)
library(geojson)
library(geojsonio)
library(leaflet)
library(htmltools)
library(htmlwidgets)
library(fmsb)
library(rsconnect)
library(stringr)
library(shinythemes)
library(maps)
library(ggthemes)
library(shiny)

# Read in the data and clean it
covid <- read.csv("covid_19_data_thru_June29.csv")

# CLEAN THE DATA
new_york_less <- state.name[c(1:31,33:50)]
texas_less <- state.name[c(1:42,44:50)]

data_US <- covid %>%
    filter(Country.Region == "US" & Province.State %in% state.name) %>%
    mutate(ObservationDate = mdy(ObservationDate))
colnames(data_US)[3] <- "States"
colnames(data_US)[7] <- "Death"

# Prepare world data
covid <- covid %>% 
    mutate(ObservationDate = mdy(ObservationDate),
           Country.Region = as.character(Country.Region))

#Clean the country names to work with the geo.json
covid$Country.Region[covid$Country.Region == " Azerbaijan"] <- "Azerbaijan"
covid$Country.Region[covid$Country.Region == "('St. Martin',)"] <- "St. Martin"
covid$Country.Region[covid$Country.Region == "Bahamas"] <- "The Bahamas"
covid$Country.Region[covid$Country.Region == "Bahamas, The"] <- "The Bahamas"
covid$Country.Region[covid$Country.Region == "Congo (Brazzaville)"] <- "Republic of the Congo"
covid$Country.Region[covid$Country.Region == "Congo (Kinshasa)"] <- "Democratic Republic of the Congo"
covid$Country.Region[covid$Country.Region == "Gambia, The"] <- "Gambia"
covid$Country.Region[covid$Country.Region == "Hong Kong"] <- "China"
covid$Country.Region[covid$Country.Region == "Mainland China"] <- "China"
covid$Country.Region[covid$Country.Region == "UK"] <- "United Kingdom"
covid$Country.Region[covid$Country.Region == "North Ireland"] <- "United Kingdom"
covid$Country.Region[covid$Country.Region == "occupied Palestinian territory"] <- "Palestine"
covid$Country.Region[covid$Country.Region == "Republic of Ireland"] <- "Ireland"
covid$Country.Region[covid$Country.Region == "Timor-Leste"] <- "East Timor"
covid$Country.Region[covid$Country.Region == "US"] <- "United States of America"
covid$Country.Region[covid$Country.Region == "West Bank and Gaza"] <- "West Bank"


# Find the totals for each date per country for Confirmed cases & Deaths
covid <- covid %>% 
    group_by(Country.Region, ObservationDate) %>% 
    summarise(Confirmed = sum(Confirmed), Deaths = sum(Deaths))

# Read in the geo.json
world_countries <- geojson_read(x = "countries.geo.json", 
                                what = "sp")




# START OF SHINY
ui <- fluidPage(theme = shinytheme("journal"),
                titlePanel("COVID-19 Visualizations"),
                tabsetPanel(
                type = "tabs",
                tabPanel("Overview",
                    sidebarLayout(
                     sidebarPanel(
                         h3("Welcome"),
                         p("This application lets you explore the rate of COVID-19 during the year 2020.
                              Click on the tabs to filter and visualize the data."),
                         br(),
                         em("Author: Declan Molony")),
                     mainPanel(
                         h2("Overview"),
                         p("The data set for this application comes from",
                           a(href = "https://www.kaggle.com/sudalairajkumar/novel-corona-virus-2019-dataset/data?select=covid_19_data.csv", "Kaggle."),
                           "At the end of 2019, the World Health Organization (WHO) became aware of a novel coronavirus in Wuhan City, Hubei Province of China.
                           Several different strains of coronavirus have developed in the past, including the famous one under the name of 'SARS' in 2002. The threat of a
                           novel coronavirus demands attention from public health administrators because its deadliness and contagiousness is unknown.
                           Daily information is crucial for the scientific community to track the spread of the viral disease, and for the public to stay 
                           informed on this crisis. The data in this application comes from the Johns Hopkins Github repository and includes data on 180 
                           countries and all of the states in the US."),
                           div(
                             p("Each tab performs a different visualization. Descriptions are as follows:",
                               tags$ul(
                                   tags$li(tags$b("COVID Outbreak US State Comparison:"), "Displays a scatter plot of the number of recorded cases and 
                                           the number of those who have died of COVID-19 by state."),
                                   tags$li(tags$b("Global Outbreak Map:"), "Use a slider to choose a date to see the spread of COVID worldwide on that day."))
                             ))))),
                
                tabPanel("COVID Outbreak US State Comparison",
                    sidebarLayout(
                        sidebarPanel("Choose two states to compare",
                         selectInput("column", "Y-axis:",
                                     c("Confirmed",
                                       "Death")),
                         selectInput("row", "Observation Date",
                                     c("ObservationDate")),
                         selectInput("state1", "First State:",
                                     c("New York", new_york_less)),
                         selectInput("state2", "Second State",
                                     c("Texas", texas_less))
                                     ),
                    mainPanel(plotOutput("point"),
                              textOutput("directions")))),
                
                tabPanel("Global Outbreak Map",
                    sidebarLayout(
                        sidebarPanel(
                            #Choose a date
                            sliderInput("dates",
                                        "Covid Cases on this day:",
                                        min = mdy("01-22-2020"),
                                        max = mdy("06-29-2020"),
                                        value = mdy("06-29-2020"))
                                    ),
                    mainPanel(leafletOutput("worldmap"),
                              textOutput("map_directions"))))
                     
))

# Define server logic required to draw a histogram
server <- function(input, output) {

# Graph for number of confirmed cases, deaths, and recoveries as compared by two US state    
    output$point <- renderPlot({
        scatter <- data_US %>% 
            filter(States == c(input$state1, input$state2)) %>% 
            ggplot(mapping = aes(color = States)) +
                geom_point(mapping = aes_string(x = input$row, y = input$column)) +
                labs(title = str_c(c("Comparison of COVID-19 ", input$column, " cases between ", input$state1, " and ", input$state2), collapse = ""))
        
        scatter + theme_stata() + scale_color_stata() +
            theme(axis.title.y = element_text(angle = 0, vjust = 0.5, face = "bold"),
                  axis.title.x = element_text(face = "bold"),
                  axis.text.y = element_text(angle = 0),
                  title = element_text(face = "bold"))
    })
    
# Instructions as for why there's set defaults for the states
    output$directions <- renderText({
        "The defaults for the states are set to New York and Texas because they
        have a comparable scale of COVID cases. Be aware that some states, like New York, 
        have cases in the hundreds of thousands, while other states, like Texas, have recorded
        cases in just the tens of thousands. Additionally, you should recognize that the time scale
        is not consistent since not every state started recording its cases at the same time. Finally,
        some states did not accurately record cases due to either lack of resources, or political reasons.
        You can certainly also just choose the same state for each input to just look at
        that state's trend line."
    })
    
# This is a leaflet map of confirmed cases of coronavirus
    output$worldmap <- renderLeaflet({
        desired_date <- covid %>% 
            filter(ObservationDate == input$dates)
        # Merge the desired date and world country Large SpatialPolygonsDataFrame
        world_countries <- merge(world_countries, desired_date,
                                 by.x = "name", by.y = "Country.Region")
        #This is for the colouring, I used the viridis palette
        #It's based on a logarithmic scale
        bins <- c(0, 10, 100, 1000, 10000, 100000, 1000000, 10000000)
        pal <- colorBin("Reds", domain = world_countries$Confirmed,
                        bins = bins)
        
        #This is to set-up a generic leaflet from the data
        corona_cases <- leaflet(world_countries) %>% 
            setView(0, 0, 1) %>%
            addProviderTiles("MapBox", options = providerTileOptions(
                id = "mapbox.light",
                accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')))
        
        #This is for the labels when you hover over a country
        labels <- sprintf(
            "<strong>%s</strong><br/>%g cases of<br>coronavirus",
            world_countries$name, world_countries$Confirmed) %>%
            lapply(htmltools::HTML)
        
        #This is the entire leaflet
        corona_cases %>% 
            #addTiles() to turn the ocean blue
            addTiles() %>% 
            addPolygons(
                #Add colouring to countries
                fillColor = ~pal(Confirmed),
                weight = 2,
                opacity = 1,
                color = "white",
                dashArray = "3",
                fillOpacity = 0.7,
                #Add highlighting when you hover over a country
                highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                #Labeling for when you hover over a country
                label = labels,
                labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")) %>% 
            #This is for creating a legend
            addLegend(pal = pal, values = ~Confirmed, opacity = 0.7, 
                      title = str_c("Number of coronavirus<br>cases per 
                      country<br> on ", as_date(input$dates)),
                      na.label = "No data yet",
                      position = "bottomright")
    })
    
# Instructions as for how to use the map
    output$map_directions <- renderText({
        "Hover over a country to see its number of coronavirus cases. Note that the scale is logarithmic. Additionally, the earlier you slide the date,
        the less country information there is due to low numbers at the beginning of the outbreak."
    })
    
}

shinyApp(ui = ui, server = server)
