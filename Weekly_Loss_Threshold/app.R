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
library(rsconnect)

genetic_data <-read.csv("Paired_Genetic_Data_Loss_Comparison_2023-01-17.csv")

genetic_data_sum <- genetic_data %>% mutate(Week=week(SampleDate),Year=year(SampleDate)) %>% filter(Week<=20) %>%
    group_by(Year,Week) %>% summarise(Loss=sum(Loss_SalvageData))
    
dist_data <- read.csv(file = file.path("Dummy_dist.csv"))

jpe_data <- read.csv(file = file.path("Winter-run_JPE.csv")) %>% rename(Year=WY)

year_options <- unique(year(genetic_data$SampleDate))

test <- dist_data %>% left_join(genetic_data_sum %>% filter(Year==2019)) %>% mutate(Loss=replace_na(Loss,0),Year=2019) %>%
    left_join(jpe_data) %>% mutate(accumulated_loss=cumsum(Loss),
                                   yellow_vulnerability=JuvenileProductionEstimate*1.17*0.5*Present_inDelta,
                                   yellow_cumulative=JuvenileProductionEstimate*1.17*0.5*EnterDelta,
                                   yellow_light_vul=as.factor(ifelse(Loss>yellow_vulnerability,"Yes","No")))

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Winter-run Chinook Salmon weekly loss threshold"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("year_input",
                        "Year",
                        choices=year_options,
                        selected=2019,multiple=FALSE),
            numericInput("percent_jpe", "% JPE for annual threshold:", 0.54, min = 0.01, max = 0.99), 
            helpText("Plot legends: bars = observed weekly loss, red bars = yellow light vulnerability threshold triggered, white solid line = observed accumulated loss, yellow dotted line = yellow light vulnerability threshold, yellow solid line = yellow light cumulative threshold.
                     NOTE: Yellow light vulnerability and cumulative thresholds are set based on 50% of the JPE annual thresholds.")),
    # Show a plot of the generated distribution
    mainPanel(tabsetPanel(tabPanel("Plot",plotOutput("Plot")),tabPanel("Table",tableOutput("tableSum"))))))

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    #Pull together data for table
    sum_data <- reactive({ dist_data %>% left_join(genetic_data_sum %>% filter(Year==as.integer(input$year_input))) %>% 
        mutate(Loss=replace_na(Loss,0),Year=as.integer(input$year_input)) %>%
        left_join(jpe_data) %>% mutate(accumulated_loss=cumsum(Loss),
                                       yellow_vulnerability=JuvenileProductionEstimate*(as.numeric(input$percent_jpe)/100)*0.5*Present_inDelta,
                                       yellow_cumulative=JuvenileProductionEstimate*(as.numeric(input$percent_jpe)/100)*0.5*EnterDelta,
                                       yellow_light_vul=as.factor(ifelse(Loss>yellow_vulnerability,"Yes","No")),
                                       yellow_light_cum=as.factor(ifelse(Loss>yellow_cumulative,"Yes","No")),
                                       red_vulnerability=JuvenileProductionEstimate*(as.numeric(input$percent_jpe)/100)*Present_inDelta,
                                       red_cumulative=JuvenileProductionEstimate*(as.numeric(input$percent_jpe)/100)*0.5*EnterDelta,
                                       red_light_vul=as.factor(ifelse(Loss>red_vulnerability,"Yes","No")),
                                       red_light_cum=as.factor(ifelse(Loss>red_cumulative,"Yes","No")),) })
    
    output$Plot <- renderPlot({
        print(ggplot(data=sum_data()) + theme_dark() +
            geom_bar(aes(x=Week,y=Loss,fill=yellow_light_vul),stat="identity")) +
            scale_x_continuous(breaks=seq(1,18,1),labels = c(1:18)) +
            geom_line(aes(x=Week,y=accumulated_loss),color="white") +
            geom_line(aes(x=Week,y=yellow_cumulative),color="yellow",linetype = "solid") +
            geom_line(aes(x=Week,y=yellow_vulnerability),color="yellow",linetype = "dashed") +
            scale_fill_manual(name="yellow_light_vul",values=c("black","red"))+
            theme(axis.text.x = element_text(size=12,color="black"),axis.text.y = element_text(size=12,color="black"),
                  axis.title.x = element_text(size=14),axis.title.y = element_text(size=14))
    })
    
    output$tableSum <- renderTable(sum_data())
    
}

# Run the application 
shinyApp(ui = ui, server = server)
