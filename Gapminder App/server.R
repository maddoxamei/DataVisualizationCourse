library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(dplyr)
library(plotly)

id <- "1oH3T0E2K_QfzS4ISC8G75Y7C6sCWhZtD"
df <- readr::read_csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id)) 


getHoverText <- function(df){
  paste("<b>",df$Country,"(",df$Year,")</b><br><br>",
        "Fertility:", df$Fertility,"<br>",
        "Life Expectancy:",df$LifeExpectancy,"<br>",
        "Population:",df$Population,"<br>")
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
  
  data <- reactiveValues(agg = df, use = df, tracked = df)
  observeEvent(list(input$regions, input$countries), {
    data$agg <- df %>%
      filter( if(length(input$regions)==0) T else Region %in% input$regions) %>%
      filter( if(length(input$countries)==0) T else Country %in% input$countries)
    updateSelectizeInput(session, "tracked_countries", 
                         choices = data$agg %>%
                           pull(Country) %>% unique() %>% sort())
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
  
  output$scatterplot <- renderPlotly({
    plot_ly(type = 'scatter', mode = 'markers') %>%
      add_trace(data = data$use,
                x=~LifeExpectancy, y=~Fertility, size=~Population,
                text = ~Country, name = ~get(input$color),
                hovertemplate = getHoverText(data$use)) %>% #, color=~get(input$color)
      layout(xaxis = list(range = c(15, 90)),
             yaxis = list(range = c(0, 9),
                          zeroline = FALSE)) %>%
      add_annotations(data = data$tracked %>%
                        filter( Year == input$year ),
                      x=~LifeExpectancy, y=~Fertility,
                      text = ~Country) %>%
      add_trace(data = data$tracked,
                x=~LifeExpectancy, y=~Fertility,
                text = ~Year,
                name = "History",
                hovertemplate = getHoverText(data$tracked),
                marker = list(
                  color = I("black"),
                  symbol = 'x'
                )
      )
    
    
    
    
    # plot_ly(data$use,
    #         x=~LifeExpectancy, y=~Fertility,
    #         text = ~Country
    #         color = ~Region) %>%
    #   add_annotations(data = data$use %>%
    #                     filter( if(length(input$tracked_countries)==0) F else Country %in% input$tracked_countries)) %>%
      # add_trace(data = data$use %>% head(),
      #           x=~LifeExpectancy, y=~Fertility,
      #           marker = list(
      #             color = I("black"),
      #             symbol = 'x'
      #           )
      # )
                
      
      # plot_ly(data = data$use, x=~LifeExpectancy, y=~Fertility, size=~Population,
      #         text = ~Country,
      #         color = ~get(input$color),
      #         hovertemplate = paste("<b>%{text}</b><br><br>",
      # 
      #                               "%{yaxis.title.text}: %{y:,.2f}<br>",
      # 
      #                               "%{xaxis.title.text}: %{x:,.2f}<br>",
      # 
      #                               "Population (size): %{marker.size:,.2f}",
      #                               "<extra></extra>")) %>% #, color=~get(input$color)
      # layout(xaxis = list(range = c(15, 90)),
      #        yaxis = list(range = c(0, 9),
      #                     zeroline = FALSE)) %>%
      # add_annotations( data = data$use %>%
      #                    filter( if(length(input$tracked_countries)==0) F else Country %in% input$tracked_countries)) #%>%
      # add_trace(data = data$use %>% head(), 
      #           marker = list(
      #             color = I("black"), 
      #             symbol = 'x'))
  })


  output$datasummary <- renderPrint(
    data$use %>% summary()
  )
  output$datatable <- DT::renderDataTable(
    data$use, selection = 'none'
  )
}
shinyServer(server)