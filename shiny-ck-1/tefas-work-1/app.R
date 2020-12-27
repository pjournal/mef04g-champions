#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(tidyverse)
library(lubridate)
library(stringi)
library(readxl)
#library(qwraps2)
#library(writexl)
library(matsindf)


df_r=readRDS("C:/Users/lenovo/OneDrive/MEF/503-Data Analytics/Project/monthly_fund_benchmark_merged.rds")

#we had to export to xlsx and import again to get rid of year variable. R don't let us remove it because it was a grouping variable before.
write_xlsx(df_r,"C:/Users/lenovo/OneDrive/MEF/503-Data Analytics/Project/monthly_indexed.xlsx")
df_x=read_excel("C:/Users/lenovo/OneDrive/MEF/503-Data Analytics/Project/monthly_indexed.xlsx")%>%select(-year)

#Price values are indexed
df_indexed=df_x%>%group_by(code)%>% 
    index_column(var_to_index = "price", time_var = "date")%>%
    relocate(price_indexed)%>%relocate(price)%>%
    relocate(date)%>%select(-ends_with(".y"))%>%
    rename_with(~gsub(".x","",.),ends_with(".x"))

#monthly price changes calculated
df_indexed=df_indexed%>%group_by(code)%>% 
    arrange(code,date)%>%
    mutate(previousmonth=lag(price,n=1), monthly_price_change=price/previousmonth-1)%>%
    select(-previousmonth)


ui <- fluidPage(
    tableOutput("static"),
    dataTableOutput("dynamic"),
    dateRangeInput("date_input", "report dates", start="2019-11-01", end = "2020-11-01", format="yyyy-mm"),
    selectInput(inputId = "group1",choices = c("fund_type"="fund_type", "category"="category", "company_name"="company_name", "OKS"="OKS", "contribution"="contribution", "participation"="participation"), label = "please select group by variables", multiple = TRUE, selected = "fund_type"),
    selectInput(inputId = "fund_type",choices = c("pension"="pension", "mutual"="mutual", "benchmark"="benchmark"), label = "please select fund types", multiple = TRUE, selected = c("pension"="pension", "mutual"="mutual")),
    pickerInput(inputId = "category", label = "select fund categories", choices = c(df_indexed%>%distinct(category))[[1]],multiple = TRUE, options = list(`actions-box` = TRUE), selected =c(df_indexed%>%distinct(category))[[1]]),
    #pickerInput(inputId = "company_pension", label = "select pension fund companies", choices = c(df_indexed%>%filter(fund_type=="pension")%>%distinct(company_name))[[1]],multiple = TRUE, options = list(`actions-box` = TRUE), selected =c(df_indexed%>%filter(fund_type=="pension")%>%distinct(company_name))[[1]]),
    #pickerInput(inputId = "company_mutual", label = "select mutual fund companies", choices = c(df_indexed%>%filter(fund_type=="mutual")%>%distinct(company_name))[[1]],multiple = TRUE, options = list(`actions-box` = TRUE), selected =c(df_indexed%>%filter(fund_type=="mutual")%>%distinct(company_name))[[1]]),
    #checkboxGroupInput(inputId = "company",label = "select companies",choices = c(df_indexed%>%distinct(company_code))[[1]], selected = c(df_indexed%>%distinct(company_code))[[1]]),
    pickerInput(inputId = "OKS", label = "OKS", choices = c(TRUE, FALSE),multiple = TRUE, options = list(`actions-box` = TRUE, maxOptions="66"), selected =c(TRUE,FALSE)),
    pickerInput(inputId = "contribution", label = "Contribution Funds", choices = c(TRUE, FALSE),multiple = TRUE, options = list(`actions-box` = TRUE), selected =c(TRUE,FALSE)),
    pickerInput(inputId = "participation", label = "Participation funds", choices = c(TRUE, FALSE),multiple = TRUE, options = list(`actions-box` = TRUE), selected =c(TRUE,FALSE))
)
server <- function(input, output, session) {

    output$dynamic <- renderDataTable(df_indexed%>%
        filter(date>=input$date_input[1] & date<=input$date_input[2] )%>%
        filter(is.na(monthly_price_change)==FALSE)%>%
        filter(OKS==input$OKS)%>%
        filter(contribution==input$contribution)%>%
        filter(participation==input$participation)%>%
        filter(fund_type ==input$fund_type)%>%
        filter(category==input$category)%>%
        #filter(company_name ==input$company_pension)%>%
        #filter(company_name ==input$company_mutual)%>%
        group_by(across(all_of(input$group1)), date )%>%
        summarise(avg_monthly_gain=mean(monthly_price_change))%>%
        mutate(cumulative_change=cumprod(avg_monthly_gain+1))%>%
        relocate(cumulative_change), escape=TRUE, options = list(pageLength = 15))
}



# Run the application 
shinyApp(ui = ui, server = server)
