

library(shiny)
library(shinyWidgets)
library(tidyverse)
library(lubridate)
library(stringi)
library(readxl)
#library(qwraps2)
#library(writexl)
library(matsindf)


df_indexed=readRDS("C:/Users/lenovo/OneDrive/MEF/503-Data Analytics/Project/monthly_indexed.rds")



ui <- fluidPage(
    
    # Application title
    titlePanel("Compare Fund Performance"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(    dateRangeInput("date_input", "report dates", start="2019-11-01", end = "2020-11-01", format="yyyy-mm"),
                         selectInput(inputId = "group1",choices = c("fund_type"="fund_type", "category"="category", "company_name"="company_name", "OKS"="OKS", "contribution"="contribution", "participation"="participation", "fund name"="name"), label = "please select group by variables", multiple = TRUE, selected = "fund_type"),
                         selectInput(inputId = "fund_type",choices = c("pension"="pension", "mutual"="mutual", "benchmark"="benchmark"), label = "please select fund types", multiple = TRUE, selected = c("pension"="pension", "mutual"="mutual")),
                         pickerInput(inputId = "category", label = "select fund categories", choices = c(df_indexed%>%distinct(category))[[1]],multiple = TRUE, options = list(`actions-box` = TRUE), selected =c(df_indexed%>%distinct(category))[[1]]),
                         pickerInput(inputId = "company", label = "select companies", choices = c("ALL",c(df_indexed%>%distinct(company_name))[[1]]),multiple = TRUE, options = list(`actions-box` = TRUE), selected="ALL"),
                         pickerInput(inputId = "fund", label = "select funds", choices = c("ALL",c(df_indexed%>%distinct(name))[[1]]),multiple = TRUE, options = list(`actions-box` = TRUE), selected="ALL"),
                         pickerInput(inputId = "OKS", label = "OKS", choices = ,c(TRUE, FALSE),multiple = TRUE, options = list(`actions-box` = TRUE, maxOptions="66"), selected =c(TRUE,FALSE)),
                         pickerInput(inputId = "contribution", label = "Contribution Funds", choices = c(TRUE, FALSE),multiple = TRUE, options = list(`actions-box` = TRUE), selected =c(TRUE,FALSE)),
                         pickerInput(inputId = "benchmark", label = "Choose Benchmarks", choices = c(df_indexed%>%filter(fund_type=="benchmark")%>%distinct(name))[[1]],multiple = TRUE, options = list(`actions-box` = TRUE), selected =c(df_indexed%>%filter(fund_type=="benchmark")%>%distinct(name))[[1]]),
                         pickerInput(inputId = "participation", label = "Participation funds", choices = c(TRUE, FALSE),multiple = TRUE, options = list(`actions-box` = TRUE), selected =c(TRUE,FALSE))),
            
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot")
        )
    )
    
    
    
    
    #tableOutput("static"),
    #dataTableOutput("dynamic"),

)

server <- function(input, output, session) {
    
    output$distPlot <- renderPlot( ggplot()+
                                       geom_line(size = 1L, data=  df_indexed%>%
                                  filter(date>=input$date_input[1] & date<=input$date_input[2] )%>%
                                  filter(is.na(monthly_price_change)==FALSE)%>%
                                  filter(OKS %in% input$OKS)%>%
                                  filter(contribution %in% input$contribution)%>%
                                  filter(participation %in% input$participation)%>%
                                  filter(fund_type %in% input$fund_type)%>%
                                  filter(category%in%input$category)%>%
                                  filter(name %in% input$benchmark | fund_type!="benchmark")%>%
                                  #{if(input$company %in% c("ALL")) filter(.,TRUE) else filter(. , company_name %in% input$company)  }%>%
                                  filter(if(input$company %in% c("ALL")) TRUE else company_name %in% input$company)%>%
                                  filter(if(input$fund %in% c("ALL")) TRUE else name %in% input$fund)%>%                                      
                                  group_by(across(all_of(input$group1)), date )%>%
                                  summarise(avg_monthly_gain=mean(monthly_price_change))%>%
                                  mutate(cumulative_change=cumprod(avg_monthly_gain+1)))+
                                  aes_string(x = "date", y = "cumulative_change", group = input$group1, color=input$group1) +
                                  theme_minimal() )
}



# Run the application 
shinyApp(ui = ui, server = server)
