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


gss_doc %>% filter(marginals %>% sapply(ncol) == 5) %>% select(id) %>% as.list()

ui <- fluidPage(
  navbarPage("General Social Survey",
             tabPanel("View Data", fluid = TRUE,
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("xvar", "Explanatory Variable:",  gss_all %>% colnames() %>% sort()),
                          selectizeInput("yvar", "Response Variable", NULL),
                          sliderInput("yr_adj", "Year adjustment:",
                                      min = 2000, max = 2022,
                                      value = 1, step = 1,
                                      animate =
                                        animationOptions(interval = 300, loop = TRUE))
                        ),
                        mainPanel(
                          plotlyOutput("plot")
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
  
  details <- reactive({
    gss_doc %>% 
      filter(row_number() %in% input$vartable_rows_selected) %>%
      select(marginals) %>%
      as.list() %>%
      bind_rows()
  })
  
  output$vartable = DT::renderDataTable({
    gss_doc %>% 
      select(id, description, text)
  })
  
  output$varrows = DT::renderDataTable(
    details(), selection = 'none'
  )
  
  output$plot <- renderPlotly({
    
  })
}

shiny::shinyApp(ui, server)



