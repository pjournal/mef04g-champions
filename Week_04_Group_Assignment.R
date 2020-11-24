pti <- c("shiny","shinydashboard","jsonlite", "zoo", "leaflet")
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

# Prepare data
raw_data = readRDS("C:\\data\\ISBIKE\\isbike_20201118.rds")
raw_df = fromJSON(raw_data)

data_list_df = raw_df$dataList

char_to_date = function(char_string){
    date_time_value = as.POSIXct(char_string,format='%Y-%m-%dT%H:%M:%S')
    return(date_time_value)
}

print(str(data_list_df))

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
    transform(TOTAL = EMPTY + FULL, RATE = FULL/(EMPTY + FULL), ACTIVE_SUSPICION = ifelse(LAST_CONNECTION<MAX_CONNECTION_YESTERDAY, 0, 1))

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Istanbul Metropolitan Municipality Bike Analysis"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            checkboxInput("activeCheckbox", "Show only active stations", value = FALSE),
            checkboxInput("testCheckbox", "Eliminate test values", value = FALSE)
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            leafletOutput("mymap")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$mymap <- renderLeaflet({
        leaflet() %>%
            addProviderTiles(providers$Stamen.TonerLite,
                             options = providerTileOptions(noWrap = TRUE)
            )%>%
            addMarkers(lng = clean_df$LON, lat=clean_df$LAT, label=clean_df$STATION_NAME, popup=paste("Station No:",clean_df$STATION_NO,"Active:",clean_df$ACTIVE,"Empty:",clean_df$EMPTY,"Full:",clean_df$FULL,"Last Connection:",clean_df$LAST_CONNECTION,sep=" "))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)