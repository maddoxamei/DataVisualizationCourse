library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(dplyr)
library(plotly)
library(highcharter)
library(rvest)

# links <- c("https://data.cdc.gov/Case-Surveillance/United-States-COVID-19-Cases-and-Deaths-by-State-o/9mfq-cb36",
#            "https://data.cdc.gov/NCHS/Provisional-COVID-19-Death-Counts-by-Week-Ending-D/r8kw-7aab",
#            "https://data.cdc.gov/NCHS/Provisional-COVID-19-Death-Counts-in-the-United-St/kn79-hsxy",
#            "https://healthdata.gov/Hospital/COVID-19-Reported-Patient-Impact-and-Hospital-Capa/anag-cw7u",
#            "https://healthdata.gov/Hospital/COVID-19-Hospital-Data-Coverage-Report/v4wn-auj8",
#            "https://healthdata.gov/Hospital/COVID-19-Reported-Patient-Impact-and-Hospital-Capa/g62h-syeh",
#            "https://data.cdc.gov/Case-Surveillance/COVID-19-Case-Surveillance-Public-Use-Data-with-Ge/n8mc-b4w4",
#            "https://data.cdc.gov/Vaccinations/COVID-19-Vaccinations-in-the-United-States-County/8xkx-amqh",
#            "https://data.cdc.gov/Public-Health-Surveillance/United-States-COVID-19-County-Level-of-Community-T/nra9-vzzn")
# 
# files <- NULL
# for( x in links ){
#     x.list <- strsplit(x, '/')[[1]]
#     file <- paste0("https://", x.list[3], "/api/views/", x.list[6], "/rows.csv?accessType=DOWNLOAD")
#     names(file) <- gsub('-', ' ', x.list[5])
#     files <- c(files, file)
# }

questions <- "https://www.cdc.gov/coronavirus/2019-ncov/covid-data/faq-surveillance.html" %>%
    read_html() %>% html_element('#Surveillance') %>% html_elements('div.card-accordion') %>% html_text()

header <- dashboardHeader(disable=T)

# useSweetAlert()

tags$script(src = "https://code.highcharts.com/mapdata/countries/us/us-all.js")

sidebar <- dashboardSidebar(
    sidebarMenu(
        uiOutput("region.selector"),
        switchInput(
            "date_sep",
            label = icon("calendar-check"),
            onLabel = "Isolate <b>specific</b> year",
            offLabel = "Aggregate <b>all</b> years",
            onStatus = "info"
        ),
        conditionalPanel("input.date_sep",
                         sliderTextInput("date", "Date adjustment:", NA,
                                         animate = animationOptions(interval = 100, loop = TRUE))
        )
        # menuItem( "Row Filtration Criteria", menuSubItem(
        #     pickerGroupUI(
        #         id = "column-filters",
        #         inline = FALSE,
        #         params = list(
        #             res_state = list(inputId = "res_state", label = "State"),
        #             res_county = list(inputId = "res_county", label = "County"),
        #             age_group = list(inputId = "age_group", label = "Age Group:"),
        #             sex = list(inputId = "sex", label = "Sex:"),
        #             race = list(inputId = "race", label = "Race:")
        #         )
        #     )
        # ))
    )
)

body.statistics <- tabPanel("Visualizations",icon = icon("chart-line"),
                            # box(title = "Relational Trends",
                            #     solidHeader = T,
                            #     status = "primary",
                            #     width = 6,
                            #     fluidRow(
                            #         column(
                            #             width = 4,
                            #             dropdown(
                            #                 icon = icon("paint-brush"),
                            #                 style = "jelly",
                            #                 label = "Color",
                            #                 width = '200%',
                            #                 right = FALSE,
                            #                 tooltip = tooltipOptions(title = "Click to select color metric",
                            #                                          placement = "left"),
                            #                 animate = animateOptions(
                            #                     enter = animations$fading_entrances$fadeInRightBig,
                            #                     exit = animations$fading_exits$fadeOutRightBig
                            #                 ),
                            #                 # uiOutput("map.color.selector")
                            #             )
                            #         )
                            #     ),
                            #     highchartOutput("scattertrend")),
                         box(title = "Regional Trends",
                             solidHeader = T,
                             status = "primary",
                             width = 6,
                             fluidRow(
                                 column(
                                     width = 4,
                                     dropdown(
                                         icon = icon("paint-brush"),
                                         style = "jelly",
                                         label = "Color",
                                         width = '200%',
                                         right = FALSE,
                                         tooltip = tooltipOptions(title = "Click to select color metric",
                                                                  placement = "left"),
                                         animate = animateOptions(
                                             enter = animations$fading_entrances$fadeInRightBig,
                                             exit = animations$fading_exits$fadeOutRightBig
                                         ),
                                         uiOutput("map.color.selector")
                                     )
                                 )
                             ),
                             highchartOutput("regionaltrend")),
                         box(title = "Monthly Trends",
                             solidHeader = T,
                             status = "primary",
                             width = 12,
                             # actionBttn(
                             #     inputId = "plotly_help",
                             #     icon = icon("info-circle"),
                             #     label = "Help",
                             #     style = "jelly"
                             # ),
                             fluidRow(
                                 column(
                                     width = 4,
                                     dropdown(
                                         icon = icon("paint-brush"),
                                         style = "jelly",
                                         label = "Color",
                                         width = '100%',
                                         right = FALSE,
                                         tooltip = tooltipOptions(title = "Click to select color metric",
                                                                  placement = "bottom"),
                                         animate = animateOptions(
                                             enter = animations$fading_entrances$fadeInLeftBig,
                                             exit = animations$fading_exits$fadeOutLeftBig
                                         ),
                                         uiOutput("linegraph.color.selector")
                                     )
                                 ),
                                 column(
                                     width = 4,
                                     dropdown(
                                         icon = icon("hourglass"),
                                         style = "jelly",
                                         label = "Window",
                                         width = '100%',
                                         right = FALSE,
                                         tooltip = tooltipOptions(title = "Click to select the length of window average",
                                                                  placement = "bottom"),
                                         animate = animateOptions(
                                             enter = animations$fading_entrances$fadeInLeftBig,
                                             exit = animations$fading_exits$fadeOutLeftBig
                                         ),
                                         sliderTextInput("linegraph.window", "Number of day(s) to average over",
                                                         choices = 1:14)
                                     )
                                 )
                             ),
                             plotlyOutput("monthlytrend"))
)

body.info <- tabPanel("Information",icon = icon("list-alt"),
                         fluidRow(
                             infoBox(title = "Each row represents",
                                     value = "Aggregated Day for a Jurisdiction", #"Deidentified Patient Case",
                                     subtitle = a("Click here for Patient Case form", href = "https://www.cdc.gov/coronavirus/2019-ncov/downloads/pui-form.pdf"),
                                     icon = icon("digital-tachograph"),
                                     color = "purple",
                                     width = 4
                                     ),
                             valueBoxOutput("nrows"),
                             valueBoxOutput("nstates")),
                         box(title = "National COVID-19 Case Surveillance FAQ",
                             collapsible = TRUE,
                             collapsed = TRUE,
                             solidHeader = TRUE,
                             status = "primary",
                             width = 12,
                             lapply(questions, function(x) {
                                 ques <- strsplit(x,'\\?')[[1]]
                                 box(title = ques[1],
                                     collapsible = TRUE,
                                     collapsed = TRUE,
                                     solidHeader = TRUE,
                                     status = "primary",
                                     p(ques[2]))
                             })
                             ),
                         box(title = "Deidentified Patient Data Dictionary",
                             solidHeader = TRUE,
                             status = "primary",
                             width = 12,
                             collapsible = TRUE,
                             DT::dataTableOutput("calculation.method"),
                             br(),
                             DT::dataTableOutput("data.dictionary")
                             )
)

body.table <- tabPanel("Table",icon = icon("table"),
                       box(title = "Hidden Columns",
                           solidHeader = TRUE,
                           status = "primary",
                           width = 12,
                           uiOutput("hidden.column.selector")),
                       box(title = "Unfiltered Raw Dataset",
                           solidHeader = TRUE,
                           status = "primary",
                           width = 12,
                           DT::dataTableOutput("data.table"))
                       )

body <- dashboardBody(
    tags$script(HTML("$('.box').eq(0).css('border', '5px solid #3DA0D1');")),
    navbarPage( title = "Covid Case Surveillance",
                body.statistics,
                body.info,
                body.table
    )
)

dashboardPage(header, sidebar, body)
