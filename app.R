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
    mutate(total_cases = cumsum(new_cases))
    #mutate(new_cases = total_cases - lag(total_cases))

school <- school %>% 
    group_by(Date) %>% 
    summarise(new_cases = sum(new_cases)) %>% 
    arrange(Date) %>% 
    mutate(total_cases = cumsum(new_cases))

#school_totals <- school %>% 
#    pivot_wider( names_from = school, values_from = new_cases, values_fn = cumsum)

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
           helpText("School data is collected from superintendent emails and cross-checked with", a(href="https://ma01807435.schoolwires.net/Page/1557", "district monthly updates"), "or the district", a(href="https://docs.google.com/spreadsheets/d/e/2PACX-1vRzBa5Hc2rniwiVAimtn_pUSaSArcrueR6L47_NItYd6jJhUXoBuuN8pRgWwlt3M3nF9YBd_C2wOfxk/pubhtml?gid=0&single=true", "Covid dashboard") ),
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
            gg <- ggplot(school, aes_string(x="Date", y=input$school_data_col)) + 
                geom_col(position=position_dodge())
                # geom_line(aes(x=Date, 
                #               y=rollmean(!!!input$school_data_col, 4, na.pad=TRUE, align="right"),
                #               color = "rolling avg."
                # ) 
                # )+
                # scale_color_manual(name = NULL, values = c("rolling avg." = "black"))
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
