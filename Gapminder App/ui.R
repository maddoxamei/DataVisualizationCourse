library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(dplyr)

id <- "1oH3T0E2K_QfzS4ISC8G75Y7C6sCWhZtD"
df <- readr::read_csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id)) 

header <- dashboardHeader(disable=T)

sidebar <- dashboardSidebar(
  sidebarMenu(
    selectizeInput("regions", "Filter Regions:",  df$Region %>% unique() %>% sort(),
              #options = list(`live-search` = TRUE),
              options = list(placeholder = "All Regions"),
              multiple = TRUE),
    pickerInput("countries", "Filter Countries in Selected Regions:", NULL,
                options = list(`live-search` = TRUE,
                               `actions-box` = TRUE,
                               title = "All countries"
                               ),
                multiple = TRUE),
    switchInput(
      "yr_sep",
      label = icon("calendar-check"),
      onLabel = "Isolate <b>specific</b> year",
      offLabel = "Aggregate <b>all</b> years",
      onStatus = "info"
    ),
    conditionalPanel("input.yr_sep",
                     sliderTextInput("year", "Year adjustment:", NA,
                                     animate = animationOptions(interval = 300, loop = TRUE))
    )
    #tags$script(HTML("$('.shiny-input-container:has(input[id=\"year\"]) > label').css({color: 'blue'})"))
  )
)

body.plot <- dashboardBody(
)

body.summary <- tabPanel("Summary",icon = icon("list-alt"),
                         verbatimTextOutput("datasummary"))

body <- dashboardBody(
  navbarPage( title = "Gapminder",
    tabPanel("Plot",icon = icon("chart-bar"),
             body.plot),
    body.summary,
    tabPanel("Table",icon = icon("table"),
             DT::dataTableOutput("datatable"))
  )
)

dashboardPage(header, sidebar, body)