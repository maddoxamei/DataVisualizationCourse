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
                          pickerInput("xvar", "Explanatory Variable:",  gss_all %>% colnames() %>% sort(),
                                      options = list(`live-search` = TRUE)),
                          pickerInput("yvar", "Response Variable", NULL, 
                                      options = list(`live-search` = TRUE, title = "Selection of this variable is optional")),
                          radioGroupButtons("yr_sep", "How to handle yearly data", 
                                            c("Aggregate","Separate"), 
                                            checkIcon = list(yes = icon("ok",lib = "glyphicon"))),
                          conditionalPanel("input.yr_sep == 'Separate'", uiOutput("yr_adj")),
                          #radioGroupButtons("plot_type", "Choose a graph", choices = NULL, justified = T)
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
    updatePickerInput(session, "yvar", 
                         choices = gss_all %>% select(-input$xvar) %>% colnames() %>% sort())
    updateRadioGroupButtons(session, "plot_type", choices = c(`<i class='fa fa-bar-chart'></i>` = "bar", `<i class='fa fa-line-chart'></i>` = "line", 
                                                              `<i class='fa fa-pie-chart'></i>` = "pie"))
  })
  
  output$yr_adj <- renderUI({
    sliderTextInput("year", "Year adjustment:",
                data$full %>% filter(!if_any(-"year", is.na)) %>% select(year) %>% as.list() %>% unlist() %>% unique() %>% sort(),
                animate = animationOptions(interval = 300, loop = TRUE))
  })
  
  details <- reactive({
    gss_doc %>% 
      filter(row_number() %in% input$vartable_rows_selected) %>%
      select(marginals) %>%
      as.list() %>%
      bind_rows()
  })
  
  data <- reactiveValues(full = NULL, use = NULL)
  observeEvent(list(input$xvar, input$yvar), data$full <- gss_all %>%
                                          select(year, !!input$xvar, intersect(colnames(.), !!input$yvar)) %>%
                                          filter(!if_all(-"year", is.na)) %>%
                                          mutate(across(intersect(colnames(.),factors), as.factor)))
  observeEvent(list(input$yr_sep, input$year, data$full), {
                   if(input$yr_sep == "Aggregate") data$use <- data$full
                   else data$use <- data$full %>% filter(year == input$year)
                   })
  
  
  output$vartable = DT::renderDataTable({
    gss_doc %>% 
      select(id, description, text)
  })
  
  output$varrows = DT::renderDataTable(
    details(), selection = 'none'
  )
  
  output$dataplot <- renderPlotly({
    if(input$yvar == "") return(plot_ly(data$use, 
                                 x=~get(input$xvar)))
    else{
      plot_ly(data$use, 
              x=~get(input$xvar), y=~get(input$yvar))
    }
  })
  output$datasummary = renderPrint(
    data$use %>% summary()
  )
  output$datatable = DT::renderDataTable(
    data$use, selection = 'none'
  )
  
}

shiny::shinyApp(ui, server)



