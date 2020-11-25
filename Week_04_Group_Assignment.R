pti <- c("shiny","shinydashboard","jsonlite", "zoo", "leaflet","kableExtra","DT")
pti <- pti[!(pti %in% installed.packages())]
if(length(pti)>0){
    install.packages(pti)
}

##########
### Shiny starter code
##########
library(shiny)
library(shinydashboard)
library(jsonlite)
library(zoo)
library(leaflet)
library(dplyr)
library(DT)

# Prepare data
raw_data = readRDS("C:\\data\\ISBIKE\\isbike_20201118.rds")
raw_df = fromJSON(raw_data)

data_list_df = raw_df$dataList

char_to_date = function(char_string){
    date_time_value = as.POSIXct(char_string,format='%Y-%m-%dT%H:%M:%S')
    return(date_time_value)
}

clean_names = c("ID","STATION_NO","STATION_NAME","ACTIVE","EMPTY","FULL","LAT","LON","LAST_CONNECTION")
clean_df = setNames(data_list_df, clean_names) %>%
    transform(LAST_CONNECTION = char_to_date(LAST_CONNECTION), 
              EMPTY = as.numeric(EMPTY), 
              FULL = as.numeric(FULL),
              LAT = as.numeric(LAT),
              LON = as.numeric(LON),
              STATION_NO = as.numeric(STATION_NO))

MAX_CONNECTION = max(clean_df$LAST_CONNECTION)
MAX_CONNECTION_YESTERDAY = MAX_CONNECTION - as.difftime(1, unit="days")

analytical_df = clean_df %>% 
    transform(TOTAL = EMPTY + FULL, RATE = round(FULL/(EMPTY + FULL),2), ACTIVE_SUSPICION = ifelse(LAST_CONNECTION<MAX_CONNECTION_YESTERDAY, 0, 1), TEST_DATA = ifelse(LAT==0 | LON==0, 1, 0))

output_names = c("ID","Station No","Station Name","Is Station Active?","Available Bike Count","In-Usage Bike Count","Latitude","Longitude","Station Last Connection Time","Total Bike Count","Active Bike Rate","Active Analysis Result","Test Values?")

mapDataFrame = data.frame("MAP_NAME"=c(1,2,3,4,5),"MAP_CODE"=c(providers$Stamen.TonerLite, 'Esri.WorldImagery', providers$Stamen.Toner, providers$Esri.NatGeoWorldMap, providers$CartoDB.Positron))

# header board
header <- dashboardHeader(
    title = 'Istanbul Metropolitan Municipality Bike Analysis'
    # task list for status of data processing
    , dropdownMenuOutput('task_menu'))

# Side bar boardy
sidebar <- dashboardSidebar(
    sidebarMenu(
        id = 'menu_tabs'
        , menuItem('Map', tabName = 'mapISBIKE')
        , menuItem('Active Bike Station Suspicion', tabName = 'isActive')
        , menuItem('Most Frequently Used Stations', tabName = 'mostFreqStations')
    )
)

# Body board
body <- dashboardBody(
    tabItems(
        tabItem(
            tabName = 'mapISBIKE',
            leafletOutput('map'),
            checkboxInput("activeCheckbox", "Active/Inactive Stations", TRUE),
            checkboxInput("testCheckbox", "Non-Test Values", TRUE),
            selectInput('mapType', 'Map Type', c(providers$Stamen.TonerLite, 'Esri.WorldImagery', providers$Stamen.Toner, providers$Esri.NatGeoWorldMap, providers$CartoDB.Positron)),
            sliderInput("emptyCount",
                        "Empty Bicycle Number",
                        min = min(analytical_df$EMPTY),
                        max = max(analytical_df$EMPTY),
                        value = c(min(analytical_df$EMPTY),max(analytical_df$EMPTY)),
                        step = 1,
                        ticks = FALSE,
                        sep = ""),
            verbatimTextOutput('summary')
        ),
        tabItem(
            tabName = 'isActive',
            DT::DTOutput('isActiveTable')
        ),
        tabItem(
            tabName = 'mostFreqStations',
            DT::DTOutput('mostFreqStationsTable')
        )
    )
)

# Shiny UI
ui <- dashboardPage(
    title = 'test',
    dashboardHeader(),
    sidebar,
    body
)

server <- function(input, output, session) {
    observe({
        req(input$mydata)
        updateTabItems(session, 'menu_tabs', 'mapISBIKE')
    })
    output$map <- renderLeaflet({
        filtered_df = analytical_df %>% 
            filter(ACTIVE_SUSPICION == ifelse(input$activeCheckbox,1,0), TEST_DATA == ifelse(input$testCheckbox,0,1), EMPTY >= input$emptyCount[1], EMPTY <= input$emptyCount[2])
        leaflet() %>%
            addProviderTiles(input$mapType)%>%
            addMarkers(lng = filtered_df$LON, lat=filtered_df$LAT, label=filtered_df$STATION_NAME, popup=paste("Station No:",filtered_df$STATION_NO,"Active:",filtered_df$ACTIVE,"Empty:",filtered_df$EMPTY,"Full:",filtered_df$FULL,"Last Connection:",filtered_df$LAST_CONNECTION,sep=" "))
    })
    output$isActiveTable = DT::renderDT(
        analytical_df %>% 
            filter(ACTIVE_SUSPICION==0) %>% 
            transform(LAST_CONNECTION = as.character(LAST_CONNECTION)) %>% 
            setNames(.,output_names)
    )
    output$summary <- renderText({
        print("Here we can see that our data contains test values because they have latitude and longitude zero or null values")
    })
    output$mostFreqStationsTable = DT::renderDT(
        analytical_df %>% 
            filter(ACTIVE_SUSPICION == 1, TEST_DATA == 0) %>% 
            arrange(desc(RATE))
    )
    observe({
        req(input$mydata)
        proxy <- leafletProxy('map')
        print(proxy$id)
        proxy %>% 
            setView(runif(1) * 30 +2, runif(1) * 30 + 2, 7)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)