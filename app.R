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
library(plotly)
library(readxl)
library(googlesheets4)
library(here)

url <- "https://docs.google.com/spreadsheets/d/1poSbKDSkWFOewSkB-7mswwxJm20EVfTUtbxAdjfnOXM/edit?usp=sharing"

gs4_deauth()  #don't need authorization for a public shared link

covid <- read_sheet(url)%>% 
    arrange(Date) %>% 
    mutate(new_cases = total_cases - lag(total_cases))

# covid <- read_excel(here("data", "mbts_covid.xlsx")) %>% 
#     arrange(Date) %>% 
#     mutate(new_cases = total_cases - lag(total_cases))

covid_cols <- names(covid)[2:length(covid)]

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Manchester-By-The-Sea COVID-19 tracker"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("covid_col",
                        "Data:",
                        choices = covid_cols,
                        selected = covid_cols[1])
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotlyOutput("covidPlot"),
           helpText("Data is collected from weekly", a(href="http://manchester.ma.us/337/Board-of-Health", "Manchester-By-The-Sea Board of Health updates")),
           helpText("Tabulated data is available for download from", a("google sheets", href=url)),
           helpText("Source code is available for download from", a(href="https://github.com/gordon-turner/mbts-covid", "github"))
           
           
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$covidPlot <- renderPlotly({

        gg <- ggplot(covid, aes_string(x="Date", y=input$covid_col)) + 
            geom_col()+
            geom_smooth()
            
        ggplotly(gg)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
