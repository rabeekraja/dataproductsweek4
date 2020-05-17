#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(plotly)
library(shinyjs)
# Dynamic dashboard page with interactive UI components
shinyUI(dashboardPage(
    dashboardHeader(title = "Covid-19 Dashboard"),
    dashboardSidebar(
      shinyjs::useShinyjs(),
        # Radio button to define static continets options
        radioButtons("continentInput", 
                                        label = "Select Continent: ",
                                        choices = list("Asia"='Asia',"Europe"='Europe',"Africa"='Africa',"America"='America',"Oceania"='Oceania'),
                                        selected = 'Asia'),
        actionButton("submit1", "Submit Continent", icon("refresh"),
                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
        #Select Drop down that dynamically list available covid-19 country
        # under selected continent
        sidebarMenuOutput("covidselect"),
        shinyjs::hidden(actionButton("submit2", "Submit Country", icon("refresh"),
                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
        shinyjs::hidden(p(id = "text1", "Processing..."))
      #  sidebarMenuOutput("submitBtn")
    ),
    dashboardBody(
        div(tabBox(
            # Tab that display country summary for covid-19   
            tabPanel("CovidSummary","",htmlOutput("covidSumry")),
            #Tab that display selected data set for given country
            tabPanel("Data", "",tableOutput("dsplyData")),
            # tab that plot plotly interactive graph/plot
            tabPanel("Plot","", plotlyOutput("covidPlot"))
        ),
       
    ),class="span7")
    
))
