library(shiny)
library(shinyWidgets)
library(dplyr)
library(rvest)
library(plotly)
if( !require(gssr) ){
  remotes::install_github("kjhealy/gssr")
  library(gssr)
}

# Grab variable list 
# var.df <- sapply(1:7, function(x){
#   sprintf("https://sda.berkeley.edu/D3/GSS18/Doc/hcbkf0%s.htm", x) %>%
#     read_html() %>%
#     html_nodes("table") %>%
#     html_table()
# }) %>%
#   bind_rows()

# id <- "1oK0Opc_tOHVSRvCxnYDEUu5zKEO7ZQ52"
# df <- readr::read_csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id)) 


factors <- gss_doc %>% 
  filter(marginals %>% sapply(ncol) == 5) %>% 
  select(id) %>% as.list() %>% unlist()

ui <- fluidPage(
  navbarPage("General Social Survey",
             tabPanel("View Data", fluid = TRUE,
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("xvar", "Explanatory Variable:",  gss_all %>% colnames() %>% sort()),
                          selectizeInput("yvar", "Response Variable", NULL),
                          radioButtons("yr_sep", "How to handle yearly data", c("Aggregate","Separate")),
                          conditionalPanel("input.yr_sep == 'Separate'", uiOutput("yr_adj"))
                        ),
                        mainPanel(
                          tabsetPanel(type = "tabs",
                                      tabPanel("Plot", plotlyOutput("dataplot")),
                                      tabPanel("Summary", verbatimTextOutput("datasummary")),
                                      tabPanel("Table", DT::dataTableOutput("datatable"))
                          )
                        )
                      )
             ),
             tabPanel("Variable Descriptions", fluid = TRUE,
                      fluidRow(
                        column(width=6, h2("Select rows for more information"), DT::dataTableOutput("vartable")),
                        column(width=6, h2("Additional information"), DT::dataTableOutput("varrows"))
                      )
             )
  )
)

server <- function(input, output, session) {
  
  observe({
    updateSelectizeInput(session, "yvar", 
                         choices = gss_all %>% select(-input$xvar) %>% colnames() %>% sort(),
                         options = list(placeholder = "Selection of this variable is optional"),
                         server = T,
                         selected = "")
  })
  
  output$yr_adj <- renderUI({
    sliderInput("year", "Year adjustment:",
                value = 1, step = 1,
                min = data()$year %>% type.convert() %>% min(), 
                max = data()$year %>% type.convert() %>% max(),
                sep = "",
                animate = animationOptions(interval = 300, loop = TRUE))
  })
  
  details <- reactive({
    df <- gss_doc %>% 
      filter(row_number() %in% input$vartable_rows_selected) %>%
      select(marginals) %>%
      as.list() %>%
      bind_rows()
  })
  
  data <- reactive({
    gss_all %>%
      select(year, try(!!input$xvar, !!input$yvar)) %>%
      filter(!if_all(-"year", is.na)) %>%
      mutate(across(intersect(colnames(.),factors), as.factor))
  })
  
  dataset <- reactive({
    if(input$yr_sep == "Aggregate") return(data())
    return(data() %>% filter(year == input$year))
  })
  
  output$vartable = DT::renderDataTable({
    gss_doc %>% 
      select(id, description, text)
  })
  
  output$varrows = DT::renderDataTable(
    details(), selection = 'none'
  )
  
  output$dataplot <- renderPlotly({
    if(input$yvar == "") plot_ly(dataset(), 
                                 x=~get(input$xvar))
  })
  output$datasummary = renderPrint(
    dataset() %>% summary()
  )
  output$datatable = DT::renderDataTable(
    dataset(), selection = 'none'
  )
  
}

shiny::shinyApp(ui, server)



