
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#
library(shinyIncubator)
library(shiny)

shinyUI(fluidPage(
 
  # Application title
  headerPanel("Commit Extraction"),
  ###############################################################################################################
  #
  #                                 TABS - Display graphs
  #
  ###############################################################################################################
  progressInit(),
  fileInput("fileInput", "Choose file to upload", multiple = FALSE,  accept = c(
    'text/csv',
    'text/comma-separated-values',
    'text/tab-separated-values',
    'text/plain',
    '.csv',
    '.tsv')),
  hr(),
  tabsetPanel(
    id ="tabs1",
    tabPanel("BoxPlot", 
             
       fluidRow(
         column(12,   
             tags$style(type="text/css", "
                     #loadmessage {
                       position: fixed;
                       top: 0px;
                       left: 0px;
                       width: 100%;
                       padding: 5px 0px 5px 0px;
                       text-align: center;
                       font-weight: bold;
                       font-size: 100%;
                       color: #000000;
                       background-color: #CCFF66;
                       z-index: 105;
                     }
            "),
             plotOutput("graph1"),
             conditionalPanel(condition="$('html').hasClass('shiny-busy')", tags$div("Loading...",id="loadmessage"))
         ),  
         column(12,checkboxInput("checkboxByRepo", "Show by repository", FALSE))
       )
       
    ),
    tabPanel("BarChart1", plotOutput("graph2")),
    tabPanel("GoogleBarChart", htmlOutput("motionchart")),
    tabPanel("BarChart2", 
              fluidRow(column(6, plotOutput("graph3.1"),  plotOutput("graph3.3")), column(6, plotOutput("graph3.2"),  plotOutput("graph3.4")))
    ),
    tabPanel("BarChart3", 
             fluidRow(column(12, selectInput("selectQuantile", label = h3("Quantile"),choices = c("Q1","Q2","Q3","Q4"), selected = "Q1"))),
             fluidRow(column(10,plotOutput("graph4.1")), column(2, uiOutput("controls4.1.1"), uiOutput("controls4.1.2"))),
             fluidRow(column(10, plotOutput("graph4.2")), column(2, uiOutput("controls4.2.1"), uiOutput("controls4.2.2"))),
             fluidRow(column(10, plotOutput("graph4.3")), column(2, uiOutput("controls4.3.1"), uiOutput("controls4.3.2"))),
             fluidRow(column(10, plotOutput("graph4.4")), column(2, uiOutput("controls4.4.1"), uiOutput("controls4.4.2")))
    ),
    tabPanel("Area",
             fluidRow(
               column(12, plotOutput("graph5")),  
               column(3, checkboxInput("checkboxByRepo2", label = h4("Show by repository"), FALSE)),
               column(3, radioButtons("radioButtonsPeriod", label = h4("Unit Time"), choices = c("Yearly","Monthly","Weekly"), selected = "Weekly")),
               column(3, radioButtons("radioButtonsTypeGraph", label = h4("Type of Graph"), choices = c("Identity","Stack"), selected = "Identity")),
               column(3, checkboxInput("checkboxCumulative", label = h4("Cumulative"), FALSE))
             ) 
        ),
    tabPanel("TableCommits", tableOutput("table")),
    tabPanel("TableCommitsDetails", tableOutput("table2"))
  ),
  
  ###############################################################################################################
  #
  #                                   MAIN CONTROLS FOR REPOSITORIES, BRANCHES AND CATEGORIES
  #
  ###############################################################################################################
  conditionalPanel(
    condition = "input.tabs1=='BoxPlot' || input.tabs1=='BarChart1' || input.tabs1=='BarChart2' || input.tabs1=='GoogleBarChart' || input.tabs1=='TableCommits' || input.tabs1=='TableCommitsDetails' || input.tabs1=='Area' ",
    h2("Configuration"),
    fluidRow(
        column(3, checkboxInput("checkboxShowAllRepo", "Show All Repositories", FALSE)),
        column(3, uiOutput("repositoriesControls")),
        column(3, uiOutput("branchesControls"),  uiOutput("categoriesControls")),
        column(3, checkboxInput("checkboxIgnoreHighValues", "Ignore High Values", TRUE))
    )
  )
 
 
))

