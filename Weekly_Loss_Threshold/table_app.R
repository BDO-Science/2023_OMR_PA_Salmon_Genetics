#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(DT)
library(readxl)
library(lubridate)

genetic_data <-read_excel(file.path("../data_input/WY1996-2022 Genetically Confirmed WR_CVP and SWP salvage and loss for OMR analysis_20230111_Final.xlsx"), sheet = "WY1996-2022 JUVWR Genetic_ID", range = cell_cols("A:K")) %>% 
    rename(SampleDate='Sample Date',SampleTime='Sample Time',FL=Forklength,Facility=Loc,Salvage_KevinReece=Salvage,Loss_KevinReece=Loss,LAD_KevinReece='Model Race',GeneticAssignment=Assignment) %>%
    mutate(SampleTime=format(SampleTime, "%H:%M:%S")) %>% mutate(SampleDate=as.Date(SampleDate)) %>%
    mutate(SampleDateTime=as.POSIXct(paste(SampleDate, SampleTime), format="%Y-%m-%d %H:%M:%S", tz="America/Los_Angeles")) %>% select(-Catch)

genetic_data_sum <- genetic_data %>% mutate(Week=week(SampleDate),Year=year(SampleDate)) %>% filter(Week<=20) %>%
    group_by(Year,Week) %>% summarise(Loss=sum(Loss_KevinReece))
    
dist_data <- read.csv(file = file.path("../data_input/Dummy_dist.csv"))

jpe_data <- read.csv(file = file.path("../data_input/Winter-run_JPE.csv")) %>% rename(Year=WY)

year_options <- unique(year(genetic_data$SampleDate))

test <- dist_data %>% left_join(genetic_data_sum %>% filter(Year==2019)) %>% mutate(Loss=replace_na(Loss,0),Year=2019) %>%
    left_join(jpe_data) %>% mutate(accumulated_loss=cumsum(Loss),
                                   yellow_vulnerability=JuvenileProductionEstimate*1.17*0.5*Present_inDelta,
                                   yellow_cumulative=JuvenileProductionEstimate*1.17*0.5*EnterDelta,
                                   yellow_light_vul=as.factor(ifelse(Loss>yellow_vulnerability,"Yes","No")))

str(test)
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    title="Exploration of winter-run Chinook Salmon weekly loss threshold for the salvage facilities",

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("year_input",
                        "Year",
                        choices=year_options,
                        selected=2019,multiple=FALSE),
            numericInput("percent_jpe", "% JPE for annual threshold:", 0.54, min = 0.01, max = 100)),
    # Show a plot of the generated distribution
    plotOutput("Plot")),
    fluidRow(
        column(4, tableOutput("tableSum"))))

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    #Pull together data for table
    sum_data <- reactive({ dist_data %>% left_join(genetic_data_sum %>% filter(Year==input$year_input)) %>% 
        mutate(Loss=replace_na(Loss,0),Year=input$year_input) %>%
        left_join(jpe_data) %>% mutate(accumulated_loss=cumsum(Loss),
                                       yellow_vulnerability=JuvenileProductionEstimate*1.17*0.5*Present_inDelta,
                                       yellow_cumulative=JuvenileProductionEstimate*1.17*0.5*EnterDelta,
                                       yellow_light_vul=as.factor(ifelse(Loss>yellow_vulnerability,"Yes","No"))) })
    
    output$Plot <- renderPlot({
        print(ggplot(data=sum_data()) +
            geom_bar(aes(x=Week,y=Loss),stat="identity"))
    })
    
    output$tableSum <- renderTable(sum_data())
    
}

# Run the application 
shinyApp(ui = ui, server = server)
