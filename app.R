library(tidyverse)
library(ggplot2)
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
library(broom)
library(shiny)

covid <- read.csv("covid_19_data.csv")

new_york_less <- state.name[c(1:31,33:50)]
california_less <- state.name[c(1:4,6:50)]

data_US <- covid %>%
    filter(Country.Region == "US" & Province.State %in% state.name) %>%
    mutate(ObservationDate = mdy(ObservationDate))

ui <- fluidPage(theme = shinytheme("journal"),
                titlePanel("COVID-19 Visualizations"),
                tabsetPanel(
                type = "tabs",
                tabPanel("Overview",
                    sidebarLayout(
                     sidebarPanel(
                         h3("Welcome"),
                         p("This application lets you explore the rate of COVID-19 during the year 2020.
                              Click on any of the tabs to filter and visualize the data."),
                         br(),
                         em("Author: Declan Molony")
                     ),
                     mainPanel(
                         h2("Overview"),
                         p("The data set for this application comes from",
                           a(href = "https://www.kaggle.com/sudalairajkumar/novel-corona-virus-2019-dataset/data?select=covid_19_data.csv", "Kaggle."),
                           "At the end of 2019, the World Health Organization (WHO) became aware of a novel coronavirus in Wuhan City, Hubei Province of China.
                           Several strains of a coronavirus have arisen in the past, including the famous one under the name of 'SARS' in 2002. The threat of a
                           novel coronavirus demands attention from public health administrators because its deadliness and contagiousness is unknown.
                           Daily information is crucial for the scientific community to track the spread of the viral disease, and for the public to stay 
                           informed on this crisis. The data in this application comes from the Johns Hopkins Github repository and includes data on 180 
                           countries and all of the states in the US."),
                           div(
                             p("Each tab performs a different visualization. Descriptions are as follows:",
                               tags$ul(
                                   tags$li(tags$b("COVID Response by State"), "Displays a comparison scatter plot of the number of recorded cases, recovered, and deaths of COVID in each state."),
                                   tags$li(tags$b("Interactive Data Table:"), "Allows an individual to select variables and search through the dataset"),
                                   tags$li(tags$b("Interactive Map:"), "Choosing a country zooms in on it from a global map and displays the happiness scores of that country."),
                                   tags$li(tags$b("Regression Analysis:"), "Creates a scatter plot between two variables and checks the significance of the slope."))
                             ))))),
                
                tabPanel("COVID Response by State",
                    sidebarPanel("Choose two states to compare",
                     selectInput("column", "Y-axis:",
                                 c("Confirmed",
                                   "Deaths",
                                   "Recovered")),
                     selectInput("row", "ObservationDate",
                                 c("ObservationDate")),
                     selectInput("state1", "First State:",
                                 c("New York", new_york_less)),
                     selectInput("state2", "Second State",
                                 c("California", california_less))
                                 ),
                    mainPanel(plotOutput("point")),
                              textOutput("directions"))
                     
))

# Define server logic required to draw a histogram
server <- function(input, output) {

# Graph for number of confirmed cases, deaths, and recoveries as compared by two US state    
    output$point <- renderPlot({
        data_US %>% 
            filter(Province.State == c(input$state1, input$state2)) %>% 
            ggplot(mapping = aes(color = Province.State)) +
                geom_point(mapping = aes_string(x = input$row, y = input$column)) +
                labs(title = str_c(c("Scatterplot of ", input$state1, " versus ", input$state2, " with respect to ", input$column, " cases."), collapse = ""))
    })
# Instructions as for why there's set defaults for the states
    output$directions <- renderText({
        "The defaults for the states are set to New York and California because they
        have a comparable scale of COVID cases. Be aware that California, for example, 
        has number of cases in the hundreds of thousands, and Alabama has recorded
        cases in just the tens of thousands.
        You can certainly also just choose the same state for each input to just look at
        that state's trend line."
    })
}

shinyApp(ui = ui, server = server)
