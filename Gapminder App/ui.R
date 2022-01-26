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

body.plot <- tabPanel("Plot",icon = icon("chart-bar"),
  fluidRow(
    box(width=11,
        title="Scatter Plot",
        status = "info",
        solidHeader = T,
        plotlyOutput("scatterplot")
    ),
    column(width=1,
        dropdown(
          icon = icon("paint-brush"),
          style = "jelly",
          label = "Color",
          right = TRUE,
          tooltip = tooltipOptions(title = "Click to select color variable",
                                   placement = "left"),
          animate = animateOptions(
            enter = animations$fading_entrances$fadeInRightBig,
            exit = animations$fading_exits$fadeOutLeftBig
          ),
          varSelectizeInput("color", "Color Variable", 
                            df %>% select(-c(Year, Country)))
        ),
        
        dropdown(
          icon = icon("search-location"),
          style = "jelly",
          label = "Track",
          right = TRUE,
          tooltip = tooltipOptions(title = "Click to view country tracking options",
                                   placement = "left"),
          animate = animateOptions(
            enter = animations$fading_entrances$fadeInRightBig,
            exit = animations$fading_exits$fadeOutLeftBig
          ),
          prettyToggle(
            "trail",
            label_on = "Trailing Activated",
            label_off = "Trailing Deactivated",
            shape = "round",
            icon_on = icon("route"),
            icon_off = icon("route"),
            animation = "pulse"
          ),
          selectizeInput("tracked_countries", "Track the Following Countries:", NULL, multiple = TRUE),
        )
    )
  )
)

body.summary <- tabPanel("Summary",icon = icon("list-alt"),
                         verbatimTextOutput("datasummary"),
                         fluidRow(
                           valueBoxOutput("nrows"),
                           valueBoxOutput("ncountries"),
                           valueBoxOutput("nregions")
                         ),
                         fluidRow(
                           box(title="Fertility",
                               status = "info",
                               solidHeader = T,
                               plotlyOutput("fertility")
                           ),
                           box(title="Life Expectancy",
                               status = "info",
                               solidHeader = T,
                               plotlyOutput("lifeexp")
                           ),
                           box(title="Population",
                               status = "info",
                               solidHeader = T,
                               plotlyOutput("population")
                           )
                         )
                         )

body <- dashboardBody(
  navbarPage( title = "Gapminder",
              body.plot,
              body.summary,
              tabPanel("Table",icon = icon("table"),
                       DT::dataTableOutput("datatable"))
  )
)

dashboardPage(header, sidebar, body)