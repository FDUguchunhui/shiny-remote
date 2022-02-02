#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(mongolite)
library(lubridate)
library(shinydashboard)
library(tidyverse)
library(maps)
library(usmap)
library(survival)
library(haven)
library(ggfortify)

# covid_patients <- mongo(collection='cov_pt_records',
#                         db='optum_covid19_elii_20210916',
#                         url = "mongodb://dbUser:gqgroup2021@129.106.153.109:39000"
# )


# covid_patients <- mongo(collection='patients',
#                         db='COVID',
#                         url = "mongodb://localhost/")
# 
# covid_diagnoses <- mongo(collection='diagnoses',
#                          db='COVID',
#                          url = "mongodb://localhost/")
# 
# 
# 
# 
# 
# patients <- covid_patients$find('{}')
# diagnoses <- covid_diagnoses$find('{}')


covid_patients <- read_csv('patients.csv')
covid_diagnoses <- read_csv('diagnoses.csv')

# Define UI for application that draws a histogram

ui <- dashboardPage(
  dashboardHeader(title='COVID Optum dashboard'),
  
  dashboardSidebar(
    
    sidebarMenu(
      sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
                        label = "Search..."),
      menuItem("Summary", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Cross-over analysis", icon = icon("chart-pie"), tabName = "widgets",
               badgeLabel = "new", badgeColor = "green"),
      menuItem("Survival analysis", icon = icon("chart-line"), tabName = "widgets",
               badgeLabel = "new", badgeColor = "green")),
    
    
    
    selectInput('disease', label='Disease ICD-10 code', 
                choices=as.factor(diagnoses$diagnosis_cd)),
    
    dateRangeInput("dateRange", "Diagnosis date between:",
                                  start='2020-01-01',
                                  end='2022-01-01')),
  
  dashboardBody(
    fluidRow( 
      box(plotOutput('barplot', height = 250)),
      box(plotOutput('hist', height = 250)),
      style='padding:0px;'
    ),
    
    fluidRow(
      box(plotOutput('covidMap')),
      
      box(plotOutput('survival')),
      
      box(plotOutput('barplot2'))
    )
  )
)

# ui <- fluidPage(
#     
#     plotOutput('barplot'),
#     
#     dateRangeInput("dateRange", "Diagnosis date between:", 
#                    start='2020-01-01',
#                    end='2022-01-01'),
#     plotOutput('hist')
# )

# Define server logic required to draw a histogram
server <- function(input, output) {
  
    output$barplot2 <- renderPlot({
      barplot(table(patients$birth_yr))
    })
    
    # filter diagnoses data according to range widget
 
    
    output$barplot <- renderPlot({
        ggplot(patients, 
               aes(gender)) +
            geom_bar(aes(fill=gender))
        })
    
    
    # alldata$date_of_death <- date(alldata$date_of_death)
    filtered <- reactive({
      diagnoses %>% 
      filter(!is.na(diag_date),
             diag_date > input$dateRange[1],
             diag_date < input$dateRange[2]) %>% 
      mutate(`diagnosis date`=as.factor(format(diag_date, '%Y-%m')))
      })
    
    output$hist <- renderPlot({
        # alldata2 <- alldata %>% filter(!is.na(date_of_death))
      
      filtered() %>% 
        ggplot(aes(`diagnosis date`)) + 
        geom_bar() +
        theme(axis.text.x = element_text(angle = 45, hjust=1))
    })
    
    
    output$covidMap <- renderPlot({
      
      # create location summary data
      patients_loc <- patients %>% group_by(region) %>% 
        summarise(count=n()) %>% 
        mutate(
          lat = recode(
            region,
            '1' = 40.722,
            '2' = 34.243,
            '3' = 32.925,
            '4' = 42.561,),
          lon = recode(
            region,
            '1' = -438.006,
            '2' = -444.940,
            '3' = -461.006,
            '4' = -479.448),
        ) %>% 
        select(lon, lat, region, count) %>% 
        filter(!is.na(lat), !is.na(lon))
      
      patients_loc<- usmap_transform(patients_loc)
      
      plot_usmap() +
        geom_point(data = patients_loc, aes(x = lon.1, y = lat.1, 
                                            color=region, size=count)) +
        theme(legend.position = "right")
    })
    
    # survival analysis
    url <- "http://web1.sph.emory.edu/dkleinb/allDatasets/surv2datasets/addicts.dta"
    
    # Import data and turn it into a data frame
    addicts <- data.frame(read_dta(url))
    
    output$survival <- renderPlot({
      model_fit <- survfit(Surv(survt, status) ~ clinic, data = addicts)
      
      autoplot(model_fit) + 
        labs(x = "\n Survival Time (Days) ", y = "Survival Probabilities \n", 
             title = "Survival Times Of \n Methadone Patients \n") + 
        theme(plot.title = element_text(hjust = 0.5), 
              axis.title.x = element_text(face="bold", colour="#FF7A33", size = 12),
              axis.title.y = element_text(face="bold", colour="#FF7A33", size = 12),
              legend.title = element_text(face="bold", size = 10))
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
