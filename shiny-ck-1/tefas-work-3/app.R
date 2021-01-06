

library(shiny)
library(shinyWidgets)
library(tidyverse)
library(lubridate)
library(TTR)
library(rlang)
# library(stringi)
# library(readxl)
#library(qwraps2)
#library(writexl)
# library(matsindf)


df_indexed=readRDS("monthly_indexed.rds")



ui <- fluidPage(
    
    # Application title
    titlePanel("Compare Fund Performance"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(dateRangeInput("date_input", "report dates", start="2019-11-01", end = "2020-11-01", format="yyyy-mm"),
                     selectInput(inputId = "fund_type",choices = c("pension"="pension", "mutual"="mutual", "benchmark"="benchmark"), label = "please select fund types", multiple = TRUE, selected = c("pension"="pension", "mutual"="mutual", "benchmark"="benchmark")),
                     pickerInput(inputId = "OKS", label = "OKS", choices = ,c(TRUE, FALSE),multiple = TRUE, options = list(`actions-box` = TRUE, maxOptions="66"), selected =c(TRUE,FALSE)),
                     pickerInput(inputId = "contribution", label = "Contribution Funds", choices = c(TRUE, FALSE),multiple = TRUE, options = list(`actions-box` = TRUE), selected =c(TRUE,FALSE)),
                     pickerInput(inputId = "participation", label = "Participation funds", choices = c(TRUE, FALSE),multiple = TRUE, options = list(`actions-box` = TRUE), selected =c(TRUE,FALSE)),
                     selectInput(inputId = "category", label = "select fund categories",  multiple=TRUE, choices =NULL), 
                     selectInput(inputId = "company", label = "select companies", multiple=TRUE, choices = NULL),
                     selectInput(inputId = "fund", label = "select funds", multiple=TRUE, choices = NULL), 
                     pickerInput(inputId = "benchmark", label = "Select benchmarks", choices = c(df_indexed %>% filter(fund_type=="benchmark")%>%distinct(name)%>%arrange(name)),multiple = TRUE, options = list(`actions-box` = TRUE), selected = c(df_indexed %>% filter(fund_type=="benchmark")%>%distinct(name)%>%arrange(name))),
                     pickerInput(inputId = "group1",choices = c("fund_type"="fund_type", "category"="category", "company_name"="company_name", "OKS"="OKS", "contribution"="contribution", "participation"="participation", "fund name"="name"), label = "please select group by variables", multiple = TRUE),
                     selectInput(inputId = "metric", label = "select metrics", multiple=FALSE, choices = c("growth_rate"="growth_rate","stdev"="stdev"), selected = "growth_rate")
                     ),
            
        mainPanel(
            plotOutput("distPlot")
        )
    )


    #tableOutput("static"),
    #dataTableOutput("dynamic"),

)


server <- function(input, output, session) {

    
observe({
    
    df_cat <- df_indexed%>%filter(fund_type%in%input$fund_type& OKS%in%input$OKS & contribution%in%input$contribution & participation%in%input$participation)%>%distinct(category)%>%arrange(desc(category))
    updateSelectInput(session = session, inputId = "category", label = "select category", choices = rev(append(df_cat[[1]],"ALL")), selected = "ALL")
})


observe({
    
    df_comp <- df_indexed%>%filter(fund_type%in%input$fund_type & OKS%in%input$OKS & contribution%in%input$contribution & participation%in%input$participation)%>%distinct(company_name) %>% arrange(desc(company_name))
    updateSelectInput(session = session, inputId = "company", label = "select company",choices = rev(append(df_comp[[1]],"ALL")), selected = "ALL")

})

observe({

    df_fund <- df_indexed%>%filter(fund_type%in%input$fund_type & OKS%in%input$OKS & contribution%in%input$contribution & participation%in%input$participation)%>%filter(if(c("ALL")%in%input$category) TRUE else category%in%input$category)%>%filter(if(c("ALL")%in%input$company) TRUE else company_name%in%input$company)%>%distinct(name)%>%arrange(desc(name))
    updateSelectInput(session = session, inputId = "fund", label = "select fund",choices = rev(append(df_fund[[1]],"ALL")), selected = "ALL")

})


    output$distPlot <- renderPlot({ 
     if(input$metric=="growth_rate") t="cumprod(avg_monthly_gain+1)" else t="runSD(x = avg_monthly_gain, n=1, sample = FALSE, cumulative = TRUE)"

        df_plot=df_indexed%>%
            filter(date>=input$date_input[1] & date<=input$date_input[2] )%>%
             mutate(monthly_price_change=if_else(is.na(monthly_price_change)==TRUE, 0,monthly_price_change))%>%
            filter(fund_type %in% input$fund_type)%>%
            filter(OKS %in% input$OKS)%>%
            filter(contribution %in% input$contribution)%>%
            filter(participation %in% input$participation)%>%
            filter(if(c("ALL")%in%input$category) all() else category%in%input$category )%>%
            filter(if(c("ALL")%in%input$company) all() else company_name%in%input$company )%>%
            filter(if(c("ALL")%in%input$fund) all() else name%in%input$fund )%>%
            # filter(if_else(is.null(input$company), company_name%in%df_comp , company_name%in%input$company))%>%
            #filter(if_else(is.null(input$fund), name%in%df_fund , name%in%input$fund))%>%
            filter(name %in% input$benchmark | fund_type!="benchmark")%>%
            group_by(across(all_of(input$group1)), date )%>%
            summarise(avg_monthly_gain=mean(monthly_price_change))%>%
            
            mutate(!!as.name(input$metric) := eval(parse(text=t)))
            
            
        
        
        ggplot()+geom_line(size = 1L, data=df_plot )+
            aes_string(x = "date", y =input$metric, group = input$group1, color=input$group1) +
            theme_minimal()+theme(legend.position = "bottom")
        })

}


# Run the application 
shinyApp(ui = ui, server = server)
