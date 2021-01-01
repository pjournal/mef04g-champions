
pti <- c("shiny","shinydashboard","shinyWidgets","tidyverse","tidyr","lubridate","DT")
pti <- pti[!(pti %in% installed.packages())]
if(length(pti)>0){
    install.packages(pti)
}

Sys.setenv(LANG = "en")

## Libraries
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(tidyverse)
library(tidyr)
library(lubridate)
library(DT)

##Preparing the data

#setwd(dirname(rstudioapi::getSourceEditorContext()$path)) #Sets the current working directory.
setwd("C:/Users/ahmet/Desktop/MEF_BDA/BDA-503/Project")
df_clean = readRDS('df_clean.rds')

df_price_change = df_clean%>% 
    dplyr::filter(date==ymd("2019-11-15")|date==ymd("2020-11-16"))%>%
    group_by(code,fund_type,category,company_name)%>%
    arrange(code,date)%>%
    mutate(previous_price=dplyr::lag(price),annual_change=price/previous_price-1)%>%
    relocate(annual_change)%>%
    dplyr::filter(!is.na(annual_change))%>%
    select(-previous_price)

df_plot_price = df_clean %>% 
    group_by(code,fund_type,category,company_name) %>% 
    arrange(code,date)%>%mutate(previousday=lag(price,n=1), daily_price_change=price/previousday-1) %>%
    dplyr::filter(date>=ymd("20191115")) %>%
    summarise(avg_daily_change=mean(daily_price_change, na.rm = TRUE), stdev=sd(daily_price_change, na.rm = TRUE),earliest=min(date), count=n()) %>%
    dplyr::filter(earliest<=ymd("20191118")) %>% 
    arrange(count) %>%
    left_join(df_price_change,df_plot_price %>% select(code, avg_daily_change, stdev), by="code") %>%
    select(-fund_type.y, -category.y) %>%
    rename(fund_type=fund_type.x,category=category.x) %>%
    relocate(avg_daily_change,stdev)

df_plot_categories = df_plot_price %>%
    group_by(category, fund_type) %>%
    summarize(avg_daily_change=mean(avg_daily_change,na.rm=TRUE),stdev=mean(stdev,na.rm = TRUE), annual_change=mean(annual_change,na.rm=TRUE))

df_today=df_clean %>% 
    filter(date==ymd("2020-11-16")) %>%
    mutate(total_value_millions=round(total_value/1000000,2), total_value_bins=floor(total_value/10000000))

df_today_investors= df_clean %>% 
    filter(date==ymd("2020-11-16"))

df_valued_funds = df_clean %>%
    dplyr::filter(date == ymd("2019-11-18") | date == ymd("2020-11-16")) %>%
    select(date,fund_type, category, code, company_name ,name, total_value, price) %>%
    arrange(code, name, date) %>%
    group_by(code, name) %>%
    mutate(previous_price=as.numeric(dplyr::lag(price,n=1)), 
           change_price_percentage=100*(price-as.numeric(dplyr::lag(price,n=1)))/ as.numeric(dplyr::lag(price,n=1)),
           previous_total_value = as.numeric(dplyr::lag(total_value,n=1)),
           change_total_value_percentage=100*(total_value-as.numeric(dplyr::lag(total_value,n=1)))/ as.numeric(dplyr::lag(total_value,n=1))) %>%
    dplyr::filter(date==ymd('2020-11-16')) %>%
    select(date,fund_type, category, code, name, price, previous_price, change_price_percentage, total_value, previous_total_value,change_total_value_percentage)

categories = 
    df_clean %>% 
    distinct(category) %>% 
    unlist(.,use.names = FALSE)

## Ahmet Emin Datasets

# Libraries
library(tidyverse)
library(ggplot2)
library(lubridate)
library(readxl)
library(stringr)
library(stringi)
library(writexl)
library(kableExtra)
library(dplyr)
library(matsindf)

# Dataset
setwd("C:/Users/ahmet/Desktop/MEF_BDA/BDA-503/Project")
df_benchmarking = readRDS("benchmark.rds") 
df_main = readRDS("df_clean_2.1.rds")
df_monthly = readRDS("monthly_fund_data.rds")

df_benchmarking = df_benchmarking %>%
    mutate(year=year(date), month=month(date)) %>%
    transform(b_tufe=as.numeric(b_tufe), b_property_index=as.numeric(b_property_index))

df_bm = df_benchmarking %>%
    mutate(tufe=round(b_tufe/268.98, digits = 2),usd=round(b_usd/2.9248, digits = 2), gold=round(b_gold/99205.72,digits = 2), xu100=round(b_xu100/752.328, digits = 2), property_index=round(b_property_index/83.7, digits = 2), deposit_rate=round(b_deposit_rate/10.4525, digits = 2)) %>%
    select(date, year, month, tufe, usd, gold, xu100, property_index)
df_bm_all=pivot_longer(df_bm, tufe:property_index, names_to = "category", values_to = "points")

df_benchmar_month=df_benchmarking %>%
    mutate(date_2=as.Date(date)-15) %>%
    mutate(year=year(date_2), month=month(date_2)) %>%
    transform(b_tufe=as.numeric(b_tufe), b_property_index=as.numeric(b_property_index))

df_all = left_join(df_monthly, df_benchmar_month, by = c('year','month')) %>%
    filter(data_date<'2020-10-10') %>%
    select(date=data_date,year, month, fund_type, category, company_name, company_code, name, code, price, total_value, shares, b_tufe, b_usd, b_gold, b_xu100, b_property_index, b_deposit_rate) %>%
    arrange(code,year,month) 

df_select = df_all %>%
    filter(date<'2018-02-05') %>%
    select(code)
distinct_df = df_select %>% distinct(code)
distinct_vector = distinct_df$code
distinct_list = as.list(distinct_vector)

df_2018 = df_all %>% filter(code %in% distinct_list)
deneme_df_2018 = df_2018 %>% group_by(code) %>%
    summarise(min(date))

df_code = df_2018 %>%
    filter(date>'2018-03-15')

df_code_2 = df_code %>% group_by(code) %>% mutate(counter = row_number(code))

df_code_index=df_code_2 %>% index_column(var_to_index = "price", time_var = "counter") %>%
    select(counter,date = date.x, year=year.x, month = month.x, fund_type = fund_type.x, category = category.x, company_name = company_name.x, company_code = company_code.x, name = name.x, code, price, price_indexed, total_value = total_value.x, shares = shares.x, b_tufe = b_tufe.x, b_usd = b_usd.x, b_gold = b_gold.x, b_xu100 = b_xu100.x, b_property_index = b_property_index.x, b_deposit_rate = b_deposit_rate.x ) %>%
    arrange(code, date)

df_code_cp = df_code_index %>%
    filter(date  == ymd('2018-03-30') | date  == ymd('2020-09-30'))

df_5 = df_code_cp %>%
    group_by(fund_type, category, company_name, company_code, name, code) %>%
    mutate(prv_price=lag(price_indexed), prv_usd=lag(b_usd), chng_usd=b_usd/lag(b_usd), prv_gold=lag(b_gold), chng_gold= b_gold/lag(b_gold), prv_xu100=lag(b_xu100), chng_xu100=b_xu100/lag(b_xu100)) %>%
    select(counter,date,year, month, fund_type, category, company_name, company_code,name, code, prv_price, price, price_indexed,total_value, shares, b_usd, prv_usd, chng_usd, b_gold, prv_gold, chng_gold, b_xu100, prv_xu100, chng_xu100) %>%
    filter(date==ymd('2020-09-30'))

df_6 = df_5 %>%
    select(fund_type, category, company_code, code, price_indexed, chng_usd, chng_gold, chng_xu100)

best_fund_2_years = df_6 %>% arrange(desc(price_indexed))
df_7 = best_fund_2_years %>%
    mutate(profit=case_when(
        price_indexed > chng_usd ~ "excellent profit",
        price_indexed < 1 ~ "loss",
        price_indexed <= chng_usd ~ "profit but less than usd"
    )
    )

df_8 = df_7 %>%
    group_by(category, profit) %>%
    count(is.na(profit),) %>%
    filter(is.na(profit) ==FALSE)

best_top_50 = best_fund_2_years[1:50, ] %>%
    select(-chng_usd, -chng_gold, -chng_xu100)

best_top_10 = best_top_50[2:11, ]

best_10_df=best_top_10 %>% distinct(code)
best_10_vector=best_10_df$code
best_10_list=as.list(best_10_vector)

best_10_all=df_code_index %>% filter(code %in% best_10_list)


categories2 = 
    df_bm_all %>% 
    distinct(category) %>% 
    unlist(.,use.names = FALSE)

categories3 = 
    df_8 %>% 
    distinct(category) %>% 
    unlist(.,use.names = FALSE)

categories4 = 
    best_10_all %>% 
    distinct(name) %>% 
    unlist(.,use.names = FALSE)

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
        , menuItem('Distibution of Funds Valuation For Contribution', tabName = 'DoFVfC')
    ),
    sidebarMenu(
        id = 'menu_tabs'
        , menuItem('Distibution of Number of People in Funds For Fund Types', tabName = 'DoNoPiFfFT') #Distibution of Number of People in Funds For Fund Types
    ),
    sidebarMenu(
        id = 'menu_tabs'
        , menuItem('Most Valued Funds', tabName = 'mvf') #Most Valued Funds
    ),
    sidebarMenu(
        id = 'menu_tabs'
        , menuItem('Appreciation of Fund Types', tabName = 'Appreciation_of_Fund_Types')
    ),
    sidebarMenu(
        id = 'menu_tabs'
        , menuItem('Profit Situation Compared On Dollar', tabName = 'Profit_Situation_Compared_on_Dollar')
    ),
    sidebarMenu(
        id = 'menu_tabs'
        , menuItem('Most Appreciated Funds in the Last 2.5 Years', tabName = 'Best_10_Funds')
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
            pickerInput(inputId = "categoryAPCvsSTDDCInput",
                        label = "Categories",
                        choices = categories,
                        multiple = TRUE,
                        options = list(`actions-box` = TRUE, maxOptions="66"), 
                        selected = categories
            )
        ),
        tabItem(
            tabName = 'APCvsSTDDCC',
            plotOutput('APCvsSTDDCCPlot'),
            pickerInput(inputId = "categoryAPCvsSTDDCCInput",
                        label = "Categories",
                        choices = categories,
                        multiple = TRUE,
                        options = list(`actions-box` = TRUE, maxOptions="66"), 
                        selected = categories
            )
        ),
        tabItem(
            tabName = 'DoFVfC', #Distibution of Funds Valuation For Contribution
            plotOutput('DoFVfCPlot')
        ),
        tabItem(
            tabName = 'DoNoPiFfFT', #Distibution of Number of People in Funds For Fund Types
            plotOutput('DoNoPiFfFTPlot')
        ),
        tabItem(
            tabName = 'mvf', #Most Valued Funds
            DT::DTOutput('mvfTable')
        ),
        tabItem(
            tabName = 'Appreciation_of_Fund_Types',
            plotOutput('Appreciation_of_Fund_Types_Plot'),
            pickerInput(inputId = "category_Appreciation_of_Fund_Types_Input",
                        label = "Categories2",
                        choices = categories2,
                        multiple = TRUE,
                        options = list(`actions-box` = TRUE, maxOptions="66"), 
                        selected = categories2
            )
        ),
        tabItem(
            tabName = 'Profit_Situation_Compared_on_Dollar',
            plotOutput('Profit_Situation_Compared_on_Dollar_Plot'),
            pickerInput(inputId = "category_Profit_Situation_Compared_on_Dollar_Input",
                        label = "Categories3",
                        choices = categories3,
                        multiple = TRUE,
                        options = list(`actions-box` = TRUE, maxOptions="66"), 
                        selected = categories3
            )
        ),
        tabItem(
            tabName = 'Best_10_Funds',
            plotOutput('Best_10_Funds_Plot'),
            pickerInput(inputId = "Best_10_Funds_Input",
                        label = "Categories4",
                        choices = categories4,
                        multiple = TRUE,
                        options = list(`actions-box` = TRUE, maxOptions="66"), 
                        selected = categories4
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
                   dplyr::filter(category %in% input$categoryAPCvsSTDDCInput)
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
                   dplyr::filter(category %in% input$categoryAPCvsSTDDCCInput)
        )+
            geom_point(aes(x=annual_change, y=stdev, color=category))+
            scale_x_log10()+
            scale_y_log10()+
            theme_minimal()+
            theme(legend.position="bottom")+
            facet_wrap(vars(fund_type))+
            labs(title = 'Annual Price Change vs Daily Change Std for Categories', x='Annual Price Change', y='Daily Change Std') 
    })
    output$DoFVfCPlot = renderPlot({
        ggplot(df_today)+
            geom_histogram(bins=50,aes(x=total_value_millions))+
            facet_grid(rows=vars(contribution)) +
            labs( x='Total Value in Million', y='Count') 
    })
    output$DoNoPiFfFTPlot = renderPlot({
        ggplot(df_today_investors) +
            geom_histogram(bins=50,aes(x=people))+facet_wrap(vars(fund_type)) +
            labs( x='People', y='Count')
    })
    output$Appreciation_of_Fund_Types_Plot = renderPlot({
        ggplot(df_bm_all %>%
                   dplyr::filter(category %in% input$category_Appreciation_of_Fund_Types_Input)   
                   )+
            geom_line(aes(x=date, y=points, color=category),size=1)+
            scale_colour_manual(values = c("tufe" = "red", "gold" = "blue","usd" = "orange","xu100" = "yellow","property_index" = "green")) +
            labs(x = "Date", y = "Change")
    })
    output$Profit_Situation_Compared_on_Dollar_Plot = renderPlot({
        ggplot(df_8 %>%
                   dplyr::filter(category %in% input$category_Profit_Situation_Compared_on_Dollar_Input)   
        )+
            aes(x = profit, fill = n, weight = n) +
            geom_bar() +
            scale_fill_gradient() +
            theme_minimal()+
            theme(legend.position="bottom")+
            facet_wrap(vars(category))+
            labs(title = 'Profit of Funds Compared to Dollar for Categories', x='Situation Compared To Dollar', y='Count of Funds')
    })
    output$Best_10_Funds_Plot = renderPlot({
        ggplot(best_10_all %>%
                   dplyr::filter(name %in% input$Best_10_Funds_Input)   
        )+
            geom_line(data=best_10_all,aes(x=date, y=price_indexed, color=name),size=1)+
            labs(title = 'Most Appreciated Funds in the Last 2.5 Years', x='Date', y='Change')
    })   
    output$mvfTable = DT::renderDT(
        df_valued_funds %>%
            arrange(desc(change_price_percentage)) %>%
            select(fund_type, category, code, name, previous_price, price, change_price_percentage)
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
