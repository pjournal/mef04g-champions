pti <- c("shiny","shinydashboard","tidyverse", "lubridate", "tidyr","readr","readxl")
pti <- pti[!(pti %in% installed.packages())]
if(length(pti)>0){
  install.packages(pti)
}
Sys.setenv(LANG = "en")

## Libraries
library(shiny)
library(shinydashboard)
library(tidyverse)
library(lubridate)
library(tidyr)
library(dplyr)
library(readr)
library(readxl)
library(stringr)
library(stringi)
library(clipr)

##Preparing the data

setwd(dirname(rstudioapi::getSourceEditorContext()$path)) #Sets the current working directory.
df_clean = readRDS('tefas_df_clean.rds')

df_price_change = df_clean%>% 
  filter(date==ymd("2019-11-15")|date==ymd("2020-11-16"))%>%
  group_by(code,fund_type,category,company_name)%>%
  arrange(code,date)%>%
  mutate(previous_price=lag(price),annual_change=price/previous_price-1)%>%
  relocate(annual_change)%>%
  filter(!is.na(annual_change))%>%
  select(-previous_price)

df_plot_price = df_clean %>% 
  group_by(code,fund_type,category,company_name) %>% 
  arrange(code,date)%>%mutate(previousday=lag(price,n=1), daily_price_change=price/previousday-1) %>%
  filter(date>=ymd("20191115")) %>%
  summarise(avg_daily_change=mean(daily_price_change, na.rm = TRUE), stdev=sd(daily_price_change, na.rm = TRUE),earliest=min(date), count=n()) %>%
  filter(earliest<=ymd("20191118")) %>% 
  arrange(count) %>%
  left_join(df_price_change,df_plot_price %>% select(code, avg_daily_change, stdev), by="code") %>%
  select(-fund_type.y, -category.y) %>%
  rename(fund_type=fund_type.x,category=category.x) %>%
  relocate(avg_daily_change,stdev)

df_plot_categories = df_plot_price %>%
  group_by(category, fund_type) %>%
  summarize(avg_daily_change=mean(avg_daily_change,na.rm=TRUE),stdev=mean(stdev,na.rm = TRUE), annual_change=mean(annual_change,na.rm=TRUE))

setwd(dirname(rstudioapi::getSourceEditorContext()$path)) #Sets the current working directory.
df_clean = readRDS('tefas_df_clean.rds')

categories = 
  df_clean %>% 
  distinct(category) %>% 
  unlist(.,use.names = FALSE)

print(categories)

# header board
header = dashboardHeader(
  title = 'TEFAS Fund Data Exploratory Data Analysis'
  # task list for status of data processing
  , dropdownMenuOutput('task_menu'))

# Side bar boardy
sidebar <- dashboardSidebar(
  sidebarMenu(
    id = 'menu_tabs'
    , menuItem('Dataset', tabName = 'miDataset')
  ),
  sidebarMenu(
    id = 'menu_tabs'
    , menuItem('Annual Price Change vs Standard Deviation of Daily Change', tabName = 'APCvsSTDDC')
  ),
  sidebarMenu(
    id = 'menu_tabs'
    , menuItem('Annual Price Change vs Standard Deviation of Daily Change Categories', tabName = 'APCvsSTDDCC')
  ),
  sidebarMenu(
    id = 'menu_tabs'
    , menuItem('Sources', tabName = 'miSources')
  )
)

# Body board
body <- dashboardBody(
  tabItems(
    tabItem(
      tabName = 'miDataset',
      DT::DTOutput('isActiveTable')
    ),
    tabItem(
      tabName = 'APCvsSTDDC',
      plotOutput('APCvsSTDDCPlot'),
      selectInput("categoryAPCvsSTDDCInput",
                  h3("Category"), 
                  choices = c("All",categories),
                  selected = categories,
                  multiple = TRUE
      ),
      sliderInput("cpRate",
                  "Rate",
                  min = 0,
                  max = 100,
                  value = c(0,100),
                  step = 5,
                  ticks = FALSE,
                  sep = "")
    ),
    tabItem(
      tabName = 'APCvsSTDDCC',
      plotOutput('APCvsSTDDCCPlot'),
      selectInput("categoryAPCvsSTDDCCInput",
                  h3("Category"), 
                  choices = c("All",categories),
                  selected = categories,
                  multiple = TRUE
      )
    ),
    tabItem(
      tabName = 'miSources',
      paste('test')
    )
  )
)

# Shiny UI
ui <- dashboardPage(
  title = 'TEFAS Fund Data Exploratory Data Analysis',
  dashboardHeader(),
  sidebar,
  body
)

server <- function(input, output, session) {
  observe({
    req(input$mydata)
    updateTabItems(session, 'menu_tabs', 'mapISBIKE')
  })
  output$isActiveTable = DT::renderDT(
    df_clean
  )
  output$APCvsSTDDCPlot = renderPlot({
    ggplot(df_plot_price %>%
             filter(category %in% input$categoryAPCvsSTDDCInput)
           )+
      geom_point(aes(x=avg_daily_change, y=stdev))+
      scale_x_log10()+
      scale_y_log10()+
      theme_minimal()+
      theme(legend.position="bottom")+
      facet_wrap(vars(category)) +
      labs(title = 'Annual Price Change vs Daily Change Std', x='Annual Price Change', y='Daily Change Std') 
  })
  output$APCvsSTDDCCPlot = renderPlot({
    ggplot(df_plot_categories %>%
             filter(category %in% input$categoryAPCvsSTDDCCInput)
    )+
      geom_point(aes(x=annual_change, y=stdev, color=category))+
      scale_x_log10()+
      scale_y_log10()+
      theme_minimal()+
      theme(legend.position="bottom")+
      facet_wrap(vars(fund_type))+
      labs(title = 'Annual Price Change vs Daily Change Std for Categories', x='Annual Price Change', y='Daily Change Std') 
  })
}

# Run the application 
shinyApp(ui = ui, server = server)