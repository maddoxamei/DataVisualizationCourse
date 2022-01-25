library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(dplyr)

id <- "1oH3T0E2K_QfzS4ISC8G75Y7C6sCWhZtD"
df <- readr::read_csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id)) 

test <- function(x, yr_sep, year){
  if( !yr_sep ) return( x )
  x %>%
    filter(Year == year)
}

server <- function(session, input, output){
  
  observeEvent(input$regions, 
               updatePickerInput(session, "countries",
                                 choices = df %>%
                                   select(Region, Country) %>%
                                   filter( if(length(input$regions)==0) T else Region %in% input$regions) %>%
                                   arrange(Region)%>% unique() %>%
                                   tidyr::pivot_wider(names_from = Region, values_from = Country) %>%
                                   as.list() %>% lapply(unlist))
               )
  
  observeEvent(input$yr_sep,
               updateSliderTextInput(session, "year",
                                     choices = data$agg %>% filter(!if_any(-"Year", is.na)) %>% pull(Year) %>% unique() %>% sort())
               )
  
  data <- reactiveValues(agg = df, use = df)
  observeEvent(list(input$regions, input$countries), {
    data$agg <- df %>%
      filter( if(length(input$regions)==0) T else Region %in% input$regions) %>%
      filter( if(length(input$countries)==0) T else Country %in% input$countries)
  })
  observeEvent(list(input$yr_sep, input$year, data$agg), {
                 if(input$yr_sep) data$use <- data$agg %>%
                     filter(Year == input$year)
                 else data$use <- data$agg
  })


  output$datasummary = renderPrint(
    data$use %>% summary()
  )
  output$datatable = DT::renderDataTable(
    data$use, selection = 'none'
  )
}
shinyServer(server)