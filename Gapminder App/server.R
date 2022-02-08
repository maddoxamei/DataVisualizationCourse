library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(dplyr)
library(plotly)

#id <- "1oH3T0E2K_QfzS4ISC8G75Y7C6sCWhZtD"
#df <- readr::read_csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id)) 
df <- readr::read_csv("./data/WorldBankData.csv") %>%
  filter(!if_any(-"Year",is.na))

getHoverText <- function(df){
  paste("<b>",df$Country,"(",df$Year,")</b><br><br>",
        "Fertility:", df$Fertility,"<br>",
        "Life Expectancy:",df$LifeExpectancy,"<br>",
        "Population:",df$Population,"<br>")
}

server <- function(session, input, output){
  observeEvent(input$regions, {
    updatePickerInput(session, "countries",
                      choices = df %>%
                        select(Region, Country) %>%
                        filter( if(length(input$regions)==0) T else Region %in% input$regions) %>%
                        arrange(Region)%>% unique() %>%
                        tidyr::pivot_wider(names_from = Region, values_from = Country) %>%
                        as.list() %>% lapply(unlist))
    # updateSelectizeInput(session, "color", 
    #                      choices = df %>% select(-c(Year, Country)) %>% colnames(), 
    #                      server = TRUE)
  })


  observeEvent(input$yr_sep,
               updateSliderTextInput(session, "year",
                                     choices = data$agg %>% filter(!if_any(-"Year", is.na)) %>% pull(Year) %>% unique() %>% sort())
               )

  data <- reactiveValues(agg = df, use = df, tracked = df, color = "Region")
  observeEvent(list(input$regions, input$countries), {
    data$agg <- df %>%
      filter( if(length(input$regions)==0) T else Region %in% input$regions) %>%
      filter( if(length(input$countries)==0) T else Country %in% input$countries)
    updateSelectizeInput(session, "tracked_countries",
                         choices = data$agg %>%
                           pull(Country) %>% unique() %>% sort(),
                         server = TRUE)
  })
  observeEvent(list(input$yr_sep, input$year, data$agg), {
                 if(input$yr_sep) data$use <- data$agg %>%
                     filter(Year == input$year)
                 else data$use <- data$agg
  })
  observeEvent(list(input$tracked_countries, data$use, input$trail), {
    if(input$yr_sep) data$tracked <- data$agg %>%
        filter( if(length(input$tracked_countries)==0) F else Country %in% input$tracked_countries) %>%
        filter( if(input$trail) Year <= as.numeric(input$year) else Year == as.numeric(input$year) )
    else data$tracked <- data$agg %>% filter(F)
  })
  observeEvent(input$color, data$color <- input$color)

  output$regions_output <- renderUI({
    selectizeInput("regions", "Filter Regions:",  df$Region %>% unique() %>% sort(),
                   options = list(placeholder = "All Regions"),
                   multiple = TRUE)
  })

  output$color_output <- renderUI({
    selectizeInput("color", "Color Variable",
                      df %>% select(-c(Year, Country)) %>% colnames())
  })

  output$scatterplot <- renderPlotly({
    plot_ly(type = 'scatter', mode = 'markers') %>%
      add_trace(data = data$use,
                x=~Fertility, y=~LifeExpectancy, size=~Population,
                #color = ~get(input$color),
                color = ~get(data$color),
                text = getHoverText(data$use),
                hoverinfo = "text") %>%
      layout(xaxis = list(range = c(0, 9)),
             yaxis = list(range = c(15, 90),
                          zeroline = FALSE)) %>%
      add_annotations(data = data$tracked %>%
                        filter( Year == input$year ),
                      x=~Fertility, y=~LifeExpectancy,
                      text = ~Country) %>%
      add_trace(data = data$tracked,
                x=~Fertility, y=~LifeExpectancy,
                text = getHoverText(data$tracked),
                name = "History",
                hoverinfo = "text",
                marker = list(
                  color = I("black"),
                  symbol = 'x'
                )
      )
  })

  output$nrows <- renderValueBox({
    valueBox(data$use %>% nrow(), "Number of Rows", icon("bars"))
  })
  output$nregions <- renderValueBox({
    valueBox(data$use %>% pull(Region) %>% unique() %>% length(),
             "Number of Regions",
             icon("globe"))
  })
  output$ncountries <- renderValueBox({
    valueBox(data$use %>% pull(Country) %>% unique() %>% length(),
             "Number of Countries",
             icon("flag"))
  })

  output$fertility <- renderPlotly({
    hist <- data$use %>%
      plot_ly(x = ~Fertility, histnorm = "probability")
    box <- data$use %>%
      plot_ly(x = ~Fertility, type = "box")
    subplot(hist, box, nrows = 2, heights = c(0.7, 0.3), shareX = T) %>%
      layout(showlegend = FALSE)
  })
  output$lifeexp <- renderPlotly({
    hist <- data$use %>%
      plot_ly(x = ~LifeExpectancy, histnorm = "probability")
    box <- data$use %>%
      plot_ly(x = ~LifeExpectancy, type = "box")
    subplot(hist, box, nrows = 2, heights = c(0.7, 0.3), shareX = T) %>%
      layout(showlegend = FALSE)
  })
  output$population <- renderPlotly({
    hist <- data$use %>%
      plot_ly(x = ~Population, histnorm = "probability")
    box <- data$use %>%
      plot_ly(x = ~Population, type = "box")
    subplot(hist, box, nrows = 2, heights = c(0.7, 0.3), shareX = T) %>%
      layout(showlegend = FALSE)
  })

  output$datatable <- DT::renderDataTable(
    data$use, selection = 'none'
  )
}
shinyServer(server)