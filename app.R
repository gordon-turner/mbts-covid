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
library(zoo)
library(here)

url <- "https://docs.google.com/spreadsheets/d/1poSbKDSkWFOewSkB-7mswwxJm20EVfTUtbxAdjfnOXM/edit?usp=sharing"

gs4_deauth()  #don't need authorization for a public shared link

covid <- read_sheet(url, sheet = "town")%>% 
    arrange(Date) %>% 
    mutate(new_cases = total_cases - lag(total_cases))

school <- read_sheet(url, sheet = "school")%>% 
    arrange(Date) %>% 
    group_by(Date, school) %>% 
    summarize(new_cases = sum(new_cases)) %>% 
    ungroup() %>% 
    complete(Date, school) %>%
    mutate(new_cases = replace_na(new_cases, 0)) %>% 
    group_by(school) %>% 
    mutate(total_cases = cumsum(new_cases)) %>% 
    ungroup()

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
            selectInput("source", 
                        label = "Source", 
                        choices=c("Town", "Schools"), 
                        selected = "Town"),
            conditionalPanel("input.source=='Town'",
                             varSelectInput("data_col",
                                            "Data:",
                                            covid %>% select(-Date), 
                                            selected = "new_cases")
                             
                             ),
            conditionalPanel("input.source=='Schools'",
                             varSelectInput("school_data_col",
                                            "Data:",
                                            school %>% select(new_cases, total_cases), 
                                            selected = "new_cases")
                             
            )
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotlyOutput("covidPlot"),
           helpText("Town data is collected from weekly", a(href="http://manchester.ma.us/337/Board-of-Health", "Manchester-By-The-Sea Board of Health updates")),
           helpText("School data is collected from the ", a(href="https://docs.google.com/spreadsheets/d/e/2PACX-1vRzBa5Hc2rniwiVAimtn_pUSaSArcrueR6L47_NItYd6jJhUXoBuuN8pRgWwlt3M3nF9YBd_C2wOfxk/pubhtml?gid=0&single=true", "district Covid dashboard"), " and superintendent emails cross-checked with", a(href="https://ma01807435.schoolwires.net/Page/1557", "2020-2021 district monthly updates.")),
           helpText("Tabulated data is available for download from", a("google sheets", href=url)),
           helpText("Source code is available for download from", a(href="https://github.com/gordon-turner/mbts-covid", "github"))
           
           
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$covidPlot <- renderPlotly({

        if (input$source=="Town"){
            gg <- ggplot(covid, aes_string(x="Date", y=input$data_col)) + 
                geom_col()+
                geom_line(aes(x=Date, 
                              y=rollmean(!!!input$data_col, 4, na.pad=TRUE, align="right"),
                              color = "4 week rolling avg."
                ) 
                )+
                scale_color_manual(name = NULL, values = c("4 week rolling avg." = "black"))            
        }else if (input$source=="Schools"){
            if (input$school_data_col == "total_cases"){
                gg <- ggplot(school, aes_string(x="Date", y="total_cases", fill="school")) + 
                    geom_area()
                
            }else{
                gg <- ggplot(school, aes_string(x="Date", y=input$school_data_col)) + 
                    geom_col(aes(fill=school))
            }
        }

            
        ggplotly(gg)%>%
            layout(legend = list(
                orientation = "h"
            )
            )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
