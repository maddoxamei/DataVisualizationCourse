library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(dplyr)
library(plotly)
library(highcharter)

# Deidentified Patient Case
# link <- "https://data.cdc.gov/Case-Surveillance/COVID-19-Case-Surveillance-Public-Use-Data-with-Ge/n8mc-b4w4" %>%
#     strsplit(., '/')
# 
# df <- paste0("https://", link[[1]][3], "/api/views/", link[[1]][6], "/rows.csv?accessType=DOWNLOAD") %>%
#     readr::read_csv(n_max = 10000) #data.table::fread(nrows = 0)

# Case Survellance aggregation by State
link <- "https://data.cdc.gov/Case-Surveillance/United-States-COVID-19-Cases-and-Deaths-by-State-o/9mfq-cb36" %>%
    strsplit(., '/')


df.agg <- paste0("https://", link[[1]][3], "/api/views/", link[[1]][6], "/rows.csv?accessType=DOWNLOAD") %>%
    readr::read_csv(name_repair = function(x) stringi::stri_replace_all_regex(x,
                                                                              c("tot", "conf", "prob"),
                                                                              c("total", "confirmed", "probable"),
                                                                              vectorize_all = F)) %>% #data.table::fread(nrows = 0)
    
    tibble::rowid_to_column() %>%
    mutate(submission_date = lubridate::mdy(submission_date)) %>%
    select(-rowid)

state_totals <- df.agg %>%
    select(state, where(is.double), -submission_date) %>%
    group_by(state) %>%
    summarize_all(sum, na.rm = T)

dictionary <- data.table::fread("data/data_dictionary.csv", skip = 2) %>% 
    tibble::tibble() %>%
    select(Variable, everything())

# getHoverText <- function(data){
#     paste("<b>",data$res_county,"(",df$Year,")</b><br><br>",
#           "Fertility:", df$Fertility,"<br>",
#           "Prop:",df$LifeExpectancy,"<br>",
#           "Population:",df$Population,"<br>")
# }

map <- get_data_from_map( download_map_data("countries/us/us-all") )

server <- function(input, output, session) {
    # output$color.selector <- renderUI(
    #     selectizeInput("color.variable", "Color Variable", 
    #                    df %>% select(where(~ length(unique(.x)) <= 10 & is.character(.x))) %>% colnames())
    # )                
    
    output$region.selector <- renderUI(
        selectizeInput("state", "Filter State", 
                       df.agg %>% pull(state) %>% unique() %>% 
                           sort() %>% append("All States", after = 0))
    )
    
    output$map.color.selector <- renderUI(
        selectizeInput("map.color", "Visualized Metric", 
                       df.agg %>% select(where(is.numeric)) %>% colnames())
    )
    
    output$linegraph.color.selector <- renderUI(
        pickerInput("linegraph_color", "Visualized Metric(s)", 
                       df.agg %>% select(where(is.numeric)) %>% colnames(),
                       multiple = TRUE,
                    selected = "new_case",
                       options = list(`live-search` = TRUE,
                                      `actions-box` = TRUE,
                                      title = "All metrics selected"
                       ))
    )
    
    # observeEvent(input$plotly_help, {
    #     sendSweetAlert(
    #         session = session,
    #         title = "Interactivity",
    #         text = "Something helpful",
    #         type = "info"
    #     )
    # })
    
    # filtered.dataset <- callModule(
    #     module = pickerGroupServer,
    #     id = "column-filters",
    #     data = df,
    #     vars = c("res_state", "res_county", "age_group", "sex", "race")
    # )
    
    # color <- reactiveVal( NULL )
    # observeEvent(input$color.variable,
    #              color( rlang::sym(input$color.variable) ))
    
    datasets <- reactiveValues(state_filtered_monthly = NULL)

    observeEvent(input$state, {
        datasets$state_filtered_monthly <- df.agg %>%
            {if (input$state != "All States") filter(., state == input$state) else . } %>%
            select(where(is.double)) %>%
            group_by(submission_date) %>%
            summarize_all(sum, na.rm = T)
        
        updateSliderTextInput(session, "date",
                              choices = df.agg %>%
                                  # select(where(is.double)) %>%
                                  # na.omit() %>% # tidyr::drop_na(any_of("death_yn"))
                                  pull(submission_date) %>% 
                                  unique() %>% sort())
    })
    
    
    # observeEvent(list(filtered.dataset(), color()), {
    #     datasets$base <- filtered.dataset() %>%
    #         tibble::rowid_to_column() %>%
    #         mutate(case_month = lubridate::ym(case_month))
    #     
    #     datasets$death <- datasets$base %>%
    #         tidyr::drop_na(any_of("death_yn")) %>%
    #         count(case_month, county_fips_code, !!color(), death_yn, name = "county_death_count") %>%
    #         mutate(county_death_count = ifelse(death_yn == "Yes", county_death_count, 0)) %>%
    #         count(case_month, county_fips_code, !!color(), wt = county_death_count, name = "death_count")
    # })
    
    output$monthlytrend <- renderPlotly(
        datasets$state_filtered_monthly %>%
            {if (length(input$linegraph_color) != 0) select(., submission_date, input$linegraph_color) else select(., where(is.double))} %>%
            full_join(x = mutate(., submission_date = as.character(submission_date)), 
                      y =summarise(., across(everything(), ~zoo::rollmean(.x, input$linegraph.window))) %>% 
                          mutate(submission_date = as.character(submission_date)), 
                      by = "submission_date", 
                      suffix = c('.raw', '')) %>%
            tidyr::gather(key = "metric", value = "count", -c(ends_with('.raw'), submission_date)) %>%
            plot_ly() %>%
            add_trace(
                type = "scatter",
                mode = "marker+lines",
                x = ~submission_date, y = ~count, color = ~metric,
                name = {if (length(input$linegraph_color)==1) "Averaged count" else NULL}) %>%
            { if (length(input$linegraph_color) == 1) add_trace(.,
                type = "bar",
                marker = list(color = "firebrick",
                              line = list(color = "white", width = .1)),
                opacity = .2,
                x = ~submission_date, y = ~get(paste0(input$linegraph_color[1],'.raw')),
                name = "Raw count") else .} %>%
            layout(barmode = 'stack', hovermode="x unified",
                   xaxis = list(title = '')) %>%
            config(
                displaylogo = FALSE,
                modeBarButtons = list(list("toImage", "resetViews"))
            )
    )
    
    output$regionaltrend <- renderHighchart(
        { if (input$date_sep) df.agg %>%
                filter(., submission_date == input$date)
            else state_totals } %>%
            inner_join(map, by = c("state" = "hc-a2")) %>%
            hcmap(map = 'countries/us/us-all', # additional all provides counties
                  download_map_data = FALSE,
                  data = .,
                  nullColor = "#d3d3d3",
                  # joinBy = "country_fips_code",
                  value = input$map.color,
                  name = stringi::stri_trans_totitle(stringi::stri_replace_all_fixed(input$map.color, '_', ' ')) 
            ) %>%
            hc_colorAxis(
                stops = color_stops(colors = viridisLite::inferno(10, begin = 0.01))
                # min = datasets$base %>% pull(input$map.color) %>% min(., na.rm = T),
                # max = datasets$base %>% pull(input$map.color) %>% max(., na.rm = T)
            ) %>%
            hc_title(text = stringi::stri_trans_totitle(stringi::stri_replace_all_fixed(input$map.color, '_', ' '))) %>%
            highcharter::hc_mapNavigation(enabled = TRUE, enableMouseWheelZoom = TRUE)
    )
    
    output$nrows <- renderValueBox({
        valueBox(df.agg %>% nrow(), "Number of Rows", icon = icon("bars"), color = "purple")
    })
    
    output$nstates <- renderValueBox({
        valueBox(df.agg %>% pull(state) %>% unique() %>% length(), "Number of Jurisdictions", icon = icon("flag-usa"), color = "purple")
    })
    
    output$calculation.method <- DT::renderDataTable(
        dictionary %>% 
            select("Calculation (if applicable)") %>% 
            filter(row_number() %in% input$data.dictionary_rows_selected),
        rownames = FALSE,
        options = list(paging = FALSE, info = FALSE, searching = FALSE)
    )
    output$data.dictionary <- DT::renderDataTable(
        dictionary %>% select(-"Calculation (if applicable)"),
        caption = 'Click on a row to display the calculation method', 
        options = list(pageLength = 3, lengthChange = FALSE, scrollX = TRUE),
        selection = "single",
        rownames = FALSE
    )
    
    output$hidden.column.selector <- renderUI(
        pickerInput("hidden.columns", "Columns to hide:", df.agg %>% colnames(),
                    multiple = TRUE,
                    selected = c("created_at",
                                 "consent_cases",
                                 "consent_deaths"
                    ),
                    options = list(`live-search` = TRUE,
                                   `actions-box` = TRUE,
                                   `deselect-all-text` = "Hide all columns",
                                   `select-all-text` = "Reveal all columns",
                                   `none-selected-text` = "No columns are hidden",
                                   width = 'auto'
                    ))
    )

    output$data.table <- DT::renderDataTable(
        df.agg %>%
            mutate(across(where(~ length(unique(.x)) <= 70 & is.character(.x)), as.factor)) %>%
            select(-input$hidden.columns),
        selection = "none",
        filter = "top",
        options = list(pageLength = 5, scrollX = TRUE, autoWidth = TRUE)
    )
}
