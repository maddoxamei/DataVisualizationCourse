library(shiny)
library(highcharter)
library(dplyr)

map <- highcharter::get_data_from_map( highcharter::download_map_data("countries/us/us-all") )

ui <- fluidPage(
  tags$script(src = "https://code.highcharts.com/mapdata/countries/us/us-all.js"),
  highchartOutput("plot")
)

server <- function(input, output, session) {
  # data_4_map <- download_map_data("custom/world-robinson-highres") %>%
  #   get_data_from_map() %>% 
  #   select(`hc-key`) %>%
  #   mutate(value = round(100 * runif(nrow(.)), 2))
  
  
  
  output$plot <- renderHighchart(
    # datasets::USArrests %>%
    #   mutate("woe-name" = rownames(.)) %>%
    #   inner_join(map) %>%
    #   highcharter::hcmap(data = .,
    #     map = 'countries/us/us-all',
    #     download_map_data = F,
    #     value = "UrbanPop", name = "Urban Population"
    #   )
    
    df.state_totals %>%
      # filter(state == 'FL') %>%
      inner_join(map, by = c("state" = "hc-a2")) %>%
      highcharter::hcmap(data = .,
                         map = 'countries/us/us-all',
                         download_map_data = F,
                         value = "total_cases", name = "Urban Population"
      ) %>%
      
    
    # hcmap(map = "custom/world-robinson-highres",
    #       data =  data_4_map,
    #       value = "value",
    #       joinBy = "hc-key",
    #       name = "Pop",
    #       download_map_data = F)
  )
}

shinyApp(ui, server)

