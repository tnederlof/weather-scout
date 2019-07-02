source("startup.R")
source("global_db.R")

ui <- navbarPage("Weather Scout", id = "nav",
                 # tablPanel 1 which contains the main map, search and data display panels of the application
                 tabPanel("Interactive map",
                          div(class = "outer",
                              tags$head(
                                # allows for custom css and javascript
                                includeCSS("static_assets/style.css"),
                                includeScript("static_assets/gomap.js")
                              ),
                              # create the leaflet.js based map
                              leafletOutput("map", width = "100%", height = "100%"),
                              # create the destination search box and enter button (which can also be
                              # triggered by hitting enter due to a line added in gomap.js)
                              absolutePanel(top = 5, left = 70, uiOutput("map_search")),
                              absolutePanel(top = 25, left = 375, uiOutput("map_enter")),
                              # create side panel
                              absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                            draggable = FALSE, top = 74, left = "auto", right = 20, bottom = "auto",
                                            width = 600, height = "auto",
                                            # display information about the selected weather station
                                            box(uiOutput("station_info"), width = 12, height = "100px"),
                                            # display control and graph output panels
                                            tabBox(width = 12,
                                                   id = "main_tab_box", height = "690px",
                                                   tabPanel("Quick Look",
                                                            wellPanel(withSpinner(uiOutput("quick_data"), proxy.height = "200px")),
                                                            withSpinner(plotlyOutput("quick_temp_graph", height = "400px"))
                                                   ),
                                                   tabPanel("Advanced",
                                                            withSpinner(uiOutput("timeseries_date_control"), proxy.height = "200px"),
                                                            withSpinner(plotlyOutput("temp_timeseries_graph", height = "400px"))
                                                   )
                                            )
                              )
                          )
                 ),
                 # tablPanel 2 which contains the about page giving background on the project and about the author
                 tabPanel("About",
                   fluidRow(
                     column(3,
                            tags$br()
                            ),
                     column(6,
                            includeMarkdown("static_assets/about_information.md")
                     ),
                     column(3,
                            tags$br()
                     )
                   )
                 )
)

server <- function(input, output, session) {
  # create the initial leaflet map
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$OpenTopoMap, options = providerTileOptions(
        updateWhenZooming = FALSE, # map won't update tiles until zoom is done (done to increase speed)
        updateWhenIdle = TRUE      # map won't load new tiles when panning (done to increase speed)
      )) %>%
      leaflet::addLegend("bottomleft", labels = c("Hourly", "Daily"),
                         colors = c("green", "blue"), title = "Weather Station Data Frequency",
                         layerId = "legend") %>%
      # set the view so the whole United States is in view as a starting point
      # (this could be changed to start in a particular area)
      leaflet::setView(lng = -89.3, lat = 38.3458, zoom = 5)
  })
  
  # initialize the store of reactive values used throughout the application
  map_status_list <- reactiveValues("station_selected" = NULL,
                                    "destination" = NULL,
                                    "destination_elevation" = NULL,
                                    "quick_date" = NULL,
                                    "start_date" = NULL,
                                    "end_date" = NULL)
  
  # used to draw  the initial weather station circle markers on the leaflet map
  observe({
    # create a palette that maps factor levels to colors
    pal <- colorFactor(c("green", "blue"), domain = c("h", "d"), ordered = T)
    # draw circle markers of the weather stations on the map
    # all_stations_tbl is loaded in global_db.R file at application startup
    leafletProxy("map", data = all_stations_tbl) %>%
      clearShapes() %>%
      addCircleMarkers(~longitude, ~latitude, label = ~as.character(name),
                       layerId = ~id, color = ~pal(freq), radius = 6, fillOpacity = 0.75, opacity = 0.3,
                       group = "stations")
  })
  
  # returns a list with lat and lon elements based on the searched field using the Google Geocoding API
  # (requires an api key set in config.yml file)
  target_pos <- reactive({
    req(input$target_zone)
    geocode(input$target_zone)
  })
  
  # everytime the search button is activated, retrieve a new set of coordinates and save the destination found
  observeEvent(input$search_target_zone, {
    req(input$target_zone)
    if (input$target_zone != "") {
      target_pos <- target_pos()
      LAT = target_pos$lat
      LONG = target_pos$lon
      map_status_list[["destination"]] <- list("lat" = LAT, "lng" = LONG)
    }
  }, ignoreNULL = F)
  
  # only count a click as an intended destination click when the user clicks on a non weather station
  observeEvent(input$map_click, {
    req(input$map_click)
    if (!is.null(input$map_marker_click)) {
      if (paste(input$map_marker_click$lat, input$map_marker_click$lng) != paste(input$map_click$lat, input$map_click$lng)) {
        map_status_list[["destination"]] <- input$map_click
      }
    } else {
      map_status_list[["destination"]] <- input$map_click
    }
  })
  
  # anytime a destination is set with a map click or a search is triggered then find the new destination elevation and save it
  observeEvent({
    input$map_click
    input$search_target_zone
  }, {
    req(map_status_list[["destination"]])
    destination_df <- data.frame(x = map_status_list[["destination"]]$lng,
                                 y = map_status_list[["destination"]]$lat)
    
    destination_elevation <- suppressMessages(get_elev_point(destination_df, prj = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs",
                                                             src = "epqs"))$elevation * 3.28084
    map_status_list[["destination_elevation"]] <- destination_elevation
  })
  
  # returns the elevation difference between the selected station and the destination
  # this is then used to adjust the temperature later on
  destination_elevation_diff <- reactive({
    req(map_status_list[["destination_elevation"]])
    req(map_status_list[["station_selected"]])
    
    station_info <- all_stations_tbl %>% dplyr::filter(id == map_status_list[["station_selected"]])
    station_elevation <- station_info$elevation * 3.28084
    return(map_status_list[["destination_elevation"]] - station_elevation)
  })
  
  # once a destination is selected, draw a green marker at that location and display the elevation on hover
  # notice that this code below sets the view to be centered on the new location with a zoom of 10
  observeEvent(input$search_target_zone, {
    req(map_status_list[["destination"]])
    req(map_status_list[["destination_elevation"]])
    
    click_icon <- makeAwesomeIcon(
      icon = 'compass',
      iconColor = 'black',
      library = 'ion',
      markerColor = "green"
    )
    
    leafletProxy("map") %>%
      leaflet::removeMarker("click_location") %>% 
      setView(lng = map_status_list[["destination"]]$lng, lat = map_status_list[["destination"]]$lat, zoom = 10) %>%
      addAwesomeMarkers(map_status_list[["destination"]]$lng, map_status_list[["destination"]]$lat,
                        label = paste("Selected Destination:", round(map_status_list[["destination_elevation"]], 0), "(ft)"),
                        icon = click_icon, layerId = "click_location")
  })
  
  # once a destination is selected, draw a green marker at that location and display the elevation on hover
  # notice that this code below does NOT set the view to be centered since that would be confusing to the user
  observeEvent(input$map_click, {
    req(map_status_list[["destination"]])
    req(map_status_list[["destination_elevation"]])
    
    click_icon <- makeAwesomeIcon(
      icon = 'compass',
      iconColor = 'black',
      library = 'ion',
      markerColor = "green"
    )
    
    leafletProxy("map") %>%
      leaflet::removeMarker("click_location") %>%
      addAwesomeMarkers(map_status_list[["destination"]]$lng, map_status_list[["destination"]]$lat,
                        label = paste("Selected Destination:", round(map_status_list[["destination_elevation"]], 0), "(ft)"),
                        icon = click_icon, layerId = "click_location")
  })
  
  # create a series of HTML to display information about a weather station
  format_station_info <- function(station_data) {
    content <- tagList(
      tags$h3(HTML(paste0("Weather Station: ", station_data$name))),
      tags$strong(paste(sprintf("%s, %s %s",station_data$post_office, station_data$state, station_data$zipcode), sprintf("(Elevation %s ft)", round((as.numeric(station_data$elevation) * 3.28084), 0)))), tags$br(),
      tags$strong(sprintf("Latitude: %s    Longitude: %s", station_data$latitude, station_data$longitude)), tags$br(),
      tags$br()
    )
    return(content)
  }
  
  # draw a blue marker to show which weather station is currently selected
  observeEvent(map_status_list[["station_selected"]], {
    
    station_details_tbl <- all_stations_tbl %>% dplyr::filter(id == map_status_list[["station_selected"]])
    
    selected_icon <- makeAwesomeIcon(
      icon = 'wifi',
      iconColor = 'black',
      library = 'ion',
      markerColor = "blue"
    )
    leafletProxy("map") %>%
      leaflet::removeMarker("selected_station") %>% 
      addAwesomeMarkers(station_details_tbl$longitude, station_details_tbl$latitude, label = station_details_tbl$name,
                        icon = selected_icon, layerId = "selected_station")
  })
  
  # save the id of the circle market when clicked, this id becomes the station selected
  observeEvent(input$map_marker_click$id, {
    map_status_list[["station_selected"]] <- input$map_marker_click$id
  })
  
  # return the id of the closest station to a certain long and lat
  closest_station <- function(all_stations_tbl, long, lat) {
    all_stations_tbl_dist <- all_stations_tbl %>% dplyr::filter(daily)
    all_stations_tbl_dist[["distance"]] <- as.numeric(apply(all_stations_tbl_dist[,c("longitude","latitude")], 1, function(x) distHaversine(c(long, lat), x))) * 0.000621371
    all_stations_tbl_dist <- all_stations_tbl_dist %>% dplyr::arrange(distance)
    return(all_stations_tbl_dist[1,]$id)
  }
  
  # select and save the closest weather station when destination is searched
  observeEvent(input$search_target_zone, {
    req(target_pos())
    target_pos <- target_pos()
    map_status_list[["station_selected"]] <- closest_station(all_stations_tbl, target_pos$lon, target_pos$lat)
  })
  
  ## uncomment to make the nearest weather station automatically select when a destination is set
  # observeEvent(map_status_list[["destination"]], {
  #   req(map_status_list[["destination"]])
  #   map_status_list[["station_selected"]] <- closest_station(all_stations_tbl, map_status_list[["destination"]]$lng, map_status_list[["destination"]]$lat)
  # })
  
  # retrieve single weather station data for a particular selected data frequency
  single_station_data <- eventReactive({
    map_status_list[["station_selected"]]
    input$data_freq
    }, {
    req(input$data_freq)
    get_weather_station_data(map_status_list[["station_selected"]], dw, pool,
                             frequency = input$data_freq)
  })
  
  
  station_info_tbl <- eventReactive(map_status_list[["station_selected"]], {
    if (is.null(map_status_list[["station_selected"]]))
      return(NULL)
    
    selected_station <- all_stations_tbl %>% dplyr::filter(id == map_status_list[["station_selected"]])
    format_station_info(selected_station)
  })
  
  # display the selected weather station information in the sidebar
  output$station_info <- renderUI({
    if (is.null(map_status_list[["station_selected"]])) {
      tagList(
        tags$h3(HTML("No Station Currently Selected")),
        tags$strong("The search bar will find a location, the closest weather station will be selected."), tags$br(),
        tags$strong("Click anywhere on the map to specify a more exact location."), tags$br(),
        tags$br()
      )
    } else {
      station_info_tbl() 
    }
  })
  
  # destination search box
  output$map_search <- renderUI({
    textInput("target_zone", "" , "", placeholder = "Enter a location: ex Denver")
  })
  
  # destination search button
  output$map_enter <- renderUI({
    actionButton("search_target_zone", "Search Location")
  })
  
  # ui quick data control panel options
  output$quick_data <- renderUI({
    req(map_status_list[["station_selected"]])
    
    selected_station <- all_stations_tbl %>% dplyr::filter(id == map_status_list[["station_selected"]])
    
    freq_options_vec <- NULL
    default_freq <- "d"
    if (!is.null(input$data_freq)) {
      # if the user left freq at hourly then use the hourly default instead of daily to be consistent
      if (input$data_freq == "h") {
        if (selected_station$hourly) {
          freq_options_vec <- c(freq_options_vec, c("Hourly" = "h"))
          default_freq <- "h"
        }
        if (selected_station$daily) {
          freq_options_vec <- c(freq_options_vec, c("Daily" = "d"))
        }
      } else {
        default_freq <- "d"
        if (selected_station$hourly) {
          freq_options_vec <- c(freq_options_vec, c("Hourly" = "h"))
          default_freq <- "d"
        }
        if (selected_station$daily) {
          freq_options_vec <- c(freq_options_vec, c("Daily" = "d"))
        }
      }
    }
    
    tagList(
      htmlOutput("help_prompt_text"),
      tags$br(),
      splitLayout(cellWidths = c("40%", "30%"),
                  dateInput('quick_date_selected', label = 'Start Date: yyyy-mm-dd', value = Sys.Date()),
                  selectInput("data_freq", "Data Frequency:", freq_options_vec, selected = default_freq)
      ),
      splitLayout(cellWidths = c("50%", "30%"),
                  checkboxInput("adjust_temp", label = "Adjust Temp for Elevation Diff", value = TRUE),
                  checkboxInput("show_range_bars", label = "Show High/Low Ranges", value = F)
      )
    )
  })
  
  # ui quick data control panel helper text (shows at the start when no weather station selected)
  output$help_prompt_text <- renderUI({
    str1 <- "Pick a date at the start of your trip!"
    str2 <- "Explore likely temperature ranges based on historical data."
    HTML(paste("<b>", paste(str1, str2, sep = '<br/>'), "</b>"))
  })
  
  # shows 6 week of data starting with the selected data in the quick_data panel
  observe({
    req(input$quick_date_selected)
    map_status_list[["start_date"]] <- input$quick_date_selected
    map_status_list[["end_date"]] <- input$quick_date_selected + weeks(6)
  })
  
  # if dates are selected in the time series panels then use those start/end dates instead of default
  observe({
    req(input$ts_date_start)
    req(input$ts_date_end)
    map_status_list[["start_date"]] <- input$ts_date_start
    map_status_list[["end_date"]] <- input$ts_date_end
  })
  
  # ui time series data control panel options
  output$timeseries_date_control <- renderUI({
    req(map_status_list[["station_selected"]])
    req(map_status_list[["start_date"]])
    req(map_status_list[["end_date"]])
    
    tagList(
      wellPanel(
        splitLayout(
          dateInput('ts_date_start', label = 'Start Date: yyyy-mm-dd', value = map_status_list[["start_date"]]),
          dateInput('ts_date_end', label = 'End Date: yyyy-mm-dd', value = map_status_list[["end_date"]])
        )
      )
    )
  })
  
  # cleans the weather station data to prepare it for downstream graphs
  temperature_data_clean <- reactive({
    req(single_station_data())
    req(input$data_freq)
    
    # bring in the raw station data
    temp_tbl <- single_station_data()
    
    # if requested in the control panel, adjust temperature based on the elevation difference
    # between the selection station and the choosen destination
    if (!is.null(map_status_list[["destination"]])) {
      if (!is.null(input$adjust_temp)) {
        if (input$adjust_temp) {
          # the general rule of thumb is for every 1k ft temperature drops 3.57 degrees
          adjustment_factor <- ((destination_elevation_diff() / 1000) * 3.57) * -1
        } else {
          adjustment_factor <- 0
        }
      } else {
        adjustment_factor <- 0
      }
    } else {
      adjustment_factor <- 0
    }
    
    # parse hourly data
    if (input$data_freq == "h") {
      temp_avg_tbl <- temp_tbl %>%
        dplyr::filter(freq == "h") %>%
        dplyr::mutate(metric = dplyr::case_when(metric == "temperature mean" ~ "Avg Temp",
                                                metric == "temperature 90th percentile" ~ "90th Pctl Temp",
                                                metric == "temperature 10th percentile" ~ "10th Pctl Temp",
                                                T ~ metric)) %>%
        dplyr::filter(metric %in% c("Avg Temp", "90th Pctl Temp", "10th Pctl Temp")) %>%
        dplyr::select(metric, month, day, hour, value) %>%
        dplyr::mutate(value = value / 10) %>%
        dplyr::arrange(month, day, hour) %>%
        dplyr::mutate(date = ymd_hms(paste(paste("2018", month, day, sep = "-"), paste((hour-1L), "59", "59", sep = ":"), sep = " "))) %>%
        dplyr::select(-c(month, day, hour))
      
      sd_values <- temp_avg_tbl %>%
        dplyr::group_by(date) %>%
        tidyr::nest() %>%
        dplyr::mutate(lwr = unlist(purrr::map(data, function(x) {
          per_10 <- dplyr::filter(x, metric == "10th Pctl Temp")$value
          temp_avg <- dplyr::filter(x, metric == "Avg Temp")$value
          sd <- temp_avg - ((temp_avg - per_10) / 1.645)
          sd
        }))) %>%
        dplyr::mutate(upr = unlist(purrr::map(data, function(x) {
          per_90 <- dplyr::filter(x, metric == "90th Pctl Temp")$value
          temp_avg <- dplyr::filter(x, metric == "Avg Temp")$value
          sd <- ((per_90 - temp_avg) / 1.645) + temp_avg
          sd
        }))) %>%
        dplyr::select(date, lwr, upr) %>%
        dplyr::mutate(lwr = lwr + adjustment_factor,
                      upr = upr + adjustment_factor)
      
      hourly_total_data_tbl <- temp_avg_tbl %>%
        dplyr::filter(metric == "Avg Temp") %>%
        dplyr::left_join(sd_values, by = "date") %>%
        dplyr::rename(Metric = metric) %>%
        dplyr::mutate(value = value + adjustment_factor)
        
    } else {
      hourly_total_data_tbl <- NULL
    }
    
    # parse daily data
    if (input$data_freq == "d") {
      temp_avg_tbl <- temp_tbl %>%
        dplyr::filter(freq == "d") %>%
        dplyr::mutate(metric = dplyr::case_when(metric == "Long-term averages of daily average temperature" ~ "Avg Temp",
                                                metric == "Long-term averages of daily maximum temperature" ~ "High Temp",
                                                metric == "Long-term averages of daily minimum temperature" ~ "Low Temp",
                                                T ~ metric)) %>%
        dplyr::filter(metric %in% c("Avg Temp", "High Temp", "Low Temp")) %>%
        dplyr::select(metric, month, day, value) %>%
        dplyr::mutate(value = value / 10) %>%
        dplyr::arrange(month, day) %>%
        dplyr::mutate(date = as.Date(paste(month, day, "2018", sep = "/"), "%m/%d/%Y")) %>%
        dplyr::select(-c(month, day)) %>%
        dplyr::mutate(value = value + adjustment_factor)
      temp_std_tbl <- temp_tbl %>%
        dplyr::filter(freq == "d") %>%
        dplyr::mutate(metric = dplyr::case_when(metric == "Long-term standard deviations of daily average temperature" ~ "Avg Temp",
                                                metric == "Long-term standard deviations of daily maximum temperature" ~ "High Temp",
                                                metric == "Long-term standard deviations of daily minimum temperature" ~ "Low Temp",
                                                T ~ metric)) %>%
        dplyr::filter(metric %in% c("Avg Temp", "High Temp", "Low Temp")) %>%
        dplyr::select(metric, month, day, value) %>%
        dplyr::mutate(sd = value / 10) %>%
        dplyr::arrange(month, day) %>%
        dplyr::mutate(date = as.Date(paste(month, day, "2018", sep = "/"), "%m/%d/%Y")) %>%
        dplyr::select(-c(month, day, value))
      daily_total_data_tbl <- dplyr::left_join(temp_avg_tbl, temp_std_tbl, by = c("metric", "date")) %>%
        dplyr::mutate(lwr = value - sd,
                      upr = value + sd) %>%
        dplyr::rename(Metric = metric)
      daily_total_data_tbl$Metric <- factor(daily_total_data_tbl$Metric, levels = c("High Temp", "Avg Temp", "Low Temp"))
    } else {
      daily_total_data_tbl <- NULL
    }
    
    return(list("h" = hourly_total_data_tbl, "d" = daily_total_data_tbl))
  })
  
  # filter the single station data according to the date selection in the control panel
  # this data is used in the quick data graphs downstream
  single_station_data_quick <- reactive({
    if (is.null(temperature_data_clean()[["h"]]) & is.null(temperature_data_clean()[["d"]]))
      return(NULL)
    req(map_status_list[["start_date"]])
    
    start_date_use <- paste(month(map_status_list[["start_date"]]), day(map_status_list[["start_date"]]), sep = "/")
    
    validate(need(stringr::str_detect(start_date_use, "/"), "Enter start date in 'MM/DD' format"))
    
    start_date <- stringr::str_split(start_date_use, "/")[[1]]
    
    validate(need((length(start_date) == 2), "Enter start date in 'MM/DD' format"))
    
    start_date_combo <- as.Date(paste(start_date[1], start_date[2], "2018", sep = "/"), "%m/%d/%Y")
    
    if (!is.null(temperature_data_clean()[["h"]])) {
      h_data <- temperature_data_clean()[["h"]] %>%
        dplyr::filter(date >= start_date_combo, date <= (start_date_combo + days(2)))
    } else {
      h_data <- NULL
    }
    
    if (!is.null(temperature_data_clean()[["d"]])) {
      start_date_combo_use <- start_date_combo - (wday(start_date_combo) - 1)
      
      data_prev_year <- temperature_data_clean()[["d"]]
      data_prev_year$date <- data_prev_year$date - years(1)
      data_post_year <- temperature_data_clean()[["d"]]
      data_post_year$date <- data_post_year$date + years(1)
      
      d_data <- dplyr::bind_rows(data_prev_year, temperature_data_clean()[["d"]], data_post_year) %>%
        dplyr::mutate(wday = wday(date),
                      week_num = paste(year(date), week(date))) %>%
        dplyr::mutate(monday_t = dplyr::case_when(wday == 1 ~ week_num,
                                                  T ~ as.character(NA))) %>%
        dplyr::group_by(Metric) %>%
        dplyr::arrange(date) %>%
        dplyr::mutate(monday_t = zoo::na.locf(monday_t, na.rm = F)) %>%
        dplyr::ungroup() %>%
        dplyr::filter(!is.na(monday_t)) %>%
        dplyr::group_by(Metric, monday_t) %>%
        dplyr::arrange(date) %>%
        dplyr::summarise(value = mean(value, na.rm = T),
                         sd = mean(sd, na.rm = T),
                         date = first(date)) %>%
        dplyr::mutate(lwr = value - sd,
                      upr = value + sd) %>%
        dplyr::ungroup() %>%
        dplyr::select(-monday_t) %>%
        dplyr::filter(date >= start_date_combo_use) %>%
        dplyr::select(Metric, value, date, sd, lwr, upr)
      d_data <- d_data %>% dplyr::filter(date <= tail(head(sort(unique(d_data$date)), 8), 1))
    } else {
      d_data <- NULL
    }
    return(list("h" = h_data, "d" = d_data))
  })
  
  # generate the quick data weekly bar graph
  output$quick_temp_graph <- renderPlotly({
    if (is.null(single_station_data_quick()[["h"]]) & is.null(single_station_data_quick()[["d"]]))
      return(NULL)
    req(single_station_data_quick())
    req(input$data_freq)
    if (is.null(input$show_range_bars))
      return(NULL)
    
    if (input$data_freq == "h") {
      total_data_tbl <- single_station_data_quick()[["h"]]
      graph_date_format_var <- "%b %d %H"
    } else if (input$data_freq == "d") {
      total_data_tbl <- single_station_data_quick()[["d"]]
      graph_date_format_var <- "%b %d"
    } else {
      stop("Frequency not yet supported")
    }
    req(total_data_tbl)
    
    total_data_tbl_clean <- total_data_tbl %>%
      dplyr::rename(Average = value,
                    Date = date,
                    Low = lwr,
                    High = upr) %>%
      dplyr::filter(!is.na(Average))
    
    margin_list = list(l = 60, r = 60, b = 60, t = 60, pad = 0)
    
    if (input$data_freq == "d") {
      high_temp <- dplyr::filter(total_data_tbl_clean, Metric == "High Temp") %>% dplyr::select(-c(Metric, Low, High)) %>% setNames(c("high_avg", "date", "high_sd"))
      avg_temp <- dplyr::filter(total_data_tbl_clean, Metric == "Avg Temp") %>% dplyr::select(-c(Metric, Low, High)) %>% setNames(c("avg", "date", "avg_sd"))
      low_temp <- dplyr::filter(total_data_tbl_clean, Metric == "Low Temp") %>% dplyr::select(-c(Metric, Low, High)) %>% setNames(c("low_avg", "date", "low_sd"))
      all_temps <- dplyr::left_join(high_temp, low_temp, by = "date") %>% dplyr::left_join(avg_temp, by = "date") %>%
        dplyr::mutate(range_from_low = high_avg - low_avg) %>%
        dplyr::mutate(high_avg_round = round(high_avg, 0),
                      low_avg_round = round(low_avg, 0),
                      avg_round = round(avg, 0),
                      high_sd_round = round(high_sd,1),
                      low_sd_round = round(low_sd, 1),
                      date_format = format(date, "%b %d"))
      all_temps$date_format <- purrr::map_chr(stringr::str_split(all_temps$date_format, " "), ~paste(.x, collapse = "<br>"))
      all_temps$date_format <- factor(all_temps$date_format, levels = unique(all_temps$date_format))
      
      if ((all(!is.na(all_temps$high_sd))) & (input$show_range_bars)) {
        graph_ranges <- c((min(all_temps$low_avg - all_temps$low_sd) - 5),
                          (max(all_temps$high_avg + all_temps$high_sd) + 5))
      } else {
        graph_ranges <- c(min(all_temps$low_avg - 5),
                          max(all_temps$high_avg) + 5)
      }
      
      if (input$show_range_bars & all(!is.na(all_temps$high_sd))) {
        p <- plot_ly(all_temps, x = ~date_format, y = ~low_avg, type = 'bar', marker = list(color = 'rgba(1,1,1, 0.0)'), name = "Low Temp",
                     hoverinfo = 'text', hovertext = paste0(format(all_temps$date, "%b %d"),": Low Avg ", all_temps$low_avg_round, " (°F)"),
                     text = all_temps$low_avg_round, textfont = list(color = '#FFFFFF', size = 16),
                     textposition = "top center") %>%
          add_trace(y = ~range_from_low, marker = list(color = '#000000'), name = "High Temp",
                    hoverinfo = 'text', hovertext = paste0(format(all_temps$date, "%b %d"),": High Avg ", all_temps$high_avg_round, " (°F)"),
                    text = all_temps$high_avg_round, textfont = list(color = '#FFFFFF', size = 16),
                    textposition = "bottom center") %>%
          add_lines(y = ~high_avg,
                    line = list(color = 'transparent'),
                    showlegend = F,
                    error_y = list(array = ~high_sd,
                                   color = '#A9A9A9'),
                    name = "High Temp",
                    hoverinfo = 'text', hovertext = paste0(format(all_temps$date, "%b %d"),": High Avg ", all_temps$high_avg_round, " (°F)"),
                    mode = "text", text = all_temps$high_avg_round, textfont = list(color = '#FFFFFF', size = 16),
                    textposition = "bottom center") %>%
          add_lines(y = ~low_avg,
                    line = list(color = 'transparent'),
                    showlegend = F,
                    error_y = list(array = ~low_sd,
                                   color = '#A9A9A9'),
                    name = "Low Temp",
                    hoverinfo = 'text', hovertext = paste0(format(all_temps$date, "%b %d"),": Low Avg ", all_temps$low_avg_round, " (°F)"),
                    mode = "text", text = all_temps$low_avg_round, textfont = list(color = '#FFFFFF', size = 16),
                    textposition = "top center") %>%
          layout(title = 'Average Temperature (based on Historical Data)',
                 xaxis = list(title = ""),
                 yaxis = list(title = "Temperature (°F)",
                              range = c((min(all_temps$low_avg - all_temps$low_sd) - 5),
                                        (max(all_temps$high_avg + all_temps$high_sd) + 5))),
                 margin = margin_list,
                 barmode = 'stack',
                 paper_bgcolor = '#f5f5f5',
                 plot_bgcolor = '#f5f5f5',
                 showlegend = FALSE)
      } else {
        p <- plot_ly(all_temps, x = ~date_format, y = ~low_avg, type = 'bar', marker = list(color = 'rgba(1,1,1, 0.0)'), name = "Low Temp",
                     hoverinfo = 'text', hovertext = paste0(format(all_temps$date, "%b %d"),": Low Avg ", all_temps$low_avg_round, " (°F)"),
                     text = all_temps$low_avg_round, textfont = list(color = '#FFFFFF', size = 16),
                     textposition = "top center") %>%
          add_trace(y = ~range_from_low, marker = list(color = '#000000'), name = "High Temp",
                    hoverinfo = 'text', hovertext = paste0(format(all_temps$date, "%b %d"),": High Avg ", all_temps$high_avg_round, " (°F)"),
                    text = all_temps$high_avg_round, textfont = list(color = '#FFFFFF', size = 16),
                    textposition = "bottom center") %>%
          add_lines(y = ~high_avg,
                    line = list(color = 'transparent'),
                    showlegend = F,
                    name = "High Temp",
                    hoverinfo = 'text', hovertext = paste0(format(all_temps$date, "%b %d"),": High Avg ", all_temps$high_avg_round, " (°F)"),
                    mode = "text", text = all_temps$high_avg_round, textfont = list(color = '#FFFFFF', size = 16),
                    textposition = "bottom center") %>%
          add_lines(y = ~low_avg,
                    line = list(color = 'transparent'),
                    showlegend = F,
                    name = "Low Temp",
                    hoverinfo = 'text', hovertext = paste0(format(all_temps$date, "%b %d"),": Low Avg ", all_temps$low_avg_round, " (°F)"),
                    mode = "text", text = all_temps$low_avg_round, textfont = list(color = '#FFFFFF', size = 16),
                    textposition = "top center") %>%
          layout(title = 'Average Temperature (based on Historical Data)',
                 xaxis = list(title = ""),
                 yaxis = list(title = "Temperature (°F)",
                              range = graph_ranges),
                 margin = margin_list,
                 barmode = 'stack',
                 paper_bgcolor = '#f5f5f5',
                 plot_bgcolor = '#f5f5f5',
                 showlegend = FALSE)
      }
      
    } else if (input$data_freq == "h") {
      avg_temp <- dplyr::filter(total_data_tbl_clean, Metric == "Avg Temp") %>%
        dplyr::select(-c(Metric)) %>%
        setNames(c("avg", "date", "low", "high")) %>%
        dplyr::mutate(low = avg - low,
                      high = high - avg) %>%
        dplyr::mutate(avg_round = round(avg, 0),
                      low_round = round(low, 0),
                      high_round = round(high, 0))
      
      if ((all(!is.na(total_data_tbl_clean$Low))) & (input$show_range_bars)) {
        graph_ranges <- c((min(avg_temp$avg - avg_temp$low) - 5),
                          (max(avg_temp$avg + avg_temp$high) + 5))
      } else {
        graph_ranges <- c(min(avg_temp$avg) - 5,
                          max(avg_temp$avg) + 5)
      }
      
      if (input$show_range_bars & all(!is.na(total_data_tbl_clean$Low))) {
        p <- plot_ly(avg_temp, x = ~date, y = ~avg, type = 'scatter', mode = 'lines+markers', name = "Average Temp",
                     error_y = list(array = ~high,
                                    arrayminus = ~low,
                                    symmetric = F,
                                    color = '#000000'),
                     hoverinfo = 'text', hovertext = paste0(hour(avg_temp$date), ":00 ", format(avg_temp$date, "%b %d"),": Avg ", avg_temp$avg_round, " (°F)")) %>%
          layout(title = 'Average Temperature (based on Historical Data)',
                 xaxis = list(title = "",
                              type = 'date',
                              tickformat = "%H<br>%b %d"),
                 yaxis = list(title = "Temperature (°F)",
                              range = graph_ranges),
                 margin = margin_list,
                 paper_bgcolor = '#f5f5f5',
                 plot_bgcolor = '#f5f5f5',
                 showlegend = FALSE)
      } else {
        p <- plot_ly(avg_temp, x = ~date, y = ~avg, type = 'scatter', mode = 'lines+markers', name = "Average Temp",
                     hoverinfo = 'text', hovertext = paste0(hour(avg_temp$date), ":00 ", format(avg_temp$date, "%b %d"),": Avg ", avg_temp$avg_round, " (°F)")) %>%
          layout(title = 'Average Temperature (based on Historical Data)',
                 xaxis = list(title = "",
                              type = 'date',
                              tickformat = "%H<br>%b %d"),
                 yaxis = list(title = "Temperature (°F)",
                              range = graph_ranges),
                 margin = margin_list,
                 paper_bgcolor = '#f5f5f5',
                 plot_bgcolor = '#f5f5f5',
                 showlegend = FALSE)
      }
    }
    p
  })
  
  # filter the single station data according to the date(s) selection in the control panel
  # this data is used in the time series graphs downstream
  single_station_data_filtered <- reactive({
    req(map_status_list[["start_date"]])
    req(map_status_list[["end_date"]])
    if (is.null(temperature_data_clean()[["h"]]) & is.null(temperature_data_clean()[["d"]]))
      return(NULL)
    
    start_date_use <- paste(month(map_status_list[["start_date"]]), day(map_status_list[["start_date"]]), sep = "/")
    end_date_use <- paste(month(map_status_list[["end_date"]]), day(map_status_list[["end_date"]]), sep = "/")
    
    validate(need(stringr::str_detect(start_date_use, "/"), "Enter start date in 'MM/DD' format"))
    validate(need(stringr::str_detect(end_date_use, "/"), "Enter end date in 'MM/DD' format"))
    
    start_date <- stringr::str_split(start_date_use, "/")[[1]]
    end_date <- stringr::str_split(end_date_use, "/")[[1]]
    
    validate(need((length(start_date) == 2), "Enter start date in 'MM/DD' format"))
    validate(need((length(end_date) == 2), "Enter end date in 'MM/DD' format"))
    
    start_date_combo <- as.Date(paste(start_date[1], start_date[2], "2018", sep = "/"), "%m/%d/%Y")
    end_date_combo <- as.Date(paste(end_date[1], end_date[2], "2018", sep = "/"), "%m/%d/%Y")
    
    if (!is.null(temperature_data_clean()[["h"]])) {
      h_data <- temperature_data_clean()[["h"]] %>%
        dplyr::filter(date >= start_date_combo, date <= end_date_combo)
    } else {
      h_data <- NULL
    }
    if (!is.null(temperature_data_clean()[["d"]])) {
      d_data <- temperature_data_clean()[["d"]] %>%
        dplyr::filter(date >= start_date_combo, date <= end_date_combo)
    } else {
      d_data <- NULL
    }
    return(list("h" = h_data, "d" = d_data))
  })
  
  # generate the time series line graph
  output$temp_timeseries_graph <- renderPlotly({
    if (is.null(single_station_data_filtered()[["h"]]) & is.null(single_station_data_filtered()[["d"]]))
      return(NULL)
    req(single_station_data_filtered())
    req(input$data_freq)
    
    if (input$data_freq == "h") {
      total_data_tbl <- single_station_data_filtered()[["h"]]
      graph_date_format_var <- "%b %d: %H"
    } else if (input$data_freq == "d") {
      total_data_tbl <- single_station_data_filtered()[["d"]]
      graph_date_format_var <- "%b %d"
    } else {
      stop("Frequency not yet supported")
    }
    
    total_data_tbl_clean <- total_data_tbl %>%
      dplyr::rename(Average = value,
                    Date = date,
                    Low = lwr,
                    High = upr) %>%
      dplyr::filter(!is.na(Average))
    
    gg <- ggplot(data = total_data_tbl_clean, aes(x = Date, y = Average, group = Metric, colour = Metric)) +
      geom_line() +
      geom_point() +
      theme_minimal() +
      xlab("") + ylab("Temperature (°F)") +
      ggtitle("Average Temperature (based on Historical Data)") +
      geom_ribbon(aes(ymin = Low, ymax = High, fill = Metric), alpha = 0.3)
    
    if (input$data_freq == "h") {
      gg <- gg + scale_x_datetime(labels = date_format(graph_date_format_var))
    } else if (input$data_freq == "d") {
      gg <- gg + scale_x_date(labels = date_format(graph_date_format_var))
    }
    gg_interactive <- plotly::ggplotly(gg) %>% layout(showlegend = FALSE,
                                                      paper_bgcolor = '#f5f5f5',
                                                      plot_bgcolor = '#f5f5f5')
    gg_interactive$x$layout$margin$t <- 60
    gg_interactive$x$layout$margin$r <- 60
    gg_interactive$x$layout$margin$b <- 60
    gg_interactive$x$layout$margin$l <- 60
    
    # edit tooltips
    if ((input$data_freq == "d")) {
      
      gg_interactive$x$data[[1]]$text <- purrr::map_chr(gg_interactive$x$data[[1]]$text,
                                                        ~ stringr::str_replace_all(.x, "Metric: High Temp<br />Metric: High Temp", ""))
      gg_interactive$x$data[[1]]$text <- purrr::map_chr(gg_interactive$x$data[[1]]$text,
                                                        ~ stringr::str_replace_all(.x, "Average", "High Avg"))
      gg_interactive$x$data[[2]]$text <- purrr::map_chr(gg_interactive$x$data[[2]]$text,
                                                        ~ stringr::str_replace_all(.x, "Metric: Avg Temp<br />Metric: Avg Temp", ""))
      gg_interactive$x$data[[3]]$text <- purrr::map_chr(gg_interactive$x$data[[3]]$text,
                                                        ~ stringr::str_replace_all(.x, "Metric: Low Temp<br />Metric: Low Temp", ""))
      gg_interactive$x$data[[3]]$text <- purrr::map_chr(gg_interactive$x$data[[3]]$text,
                                                        ~ stringr::str_replace_all(.x, "Average", "Metric: Low Avg"))
      
      gg_interactive$x$data[[4]]$text <- purrr::map_chr(gg_interactive$x$data[[4]]$text,
                                                        ~ stringr::str_replace_all(.x, "High:", "High Max:"))
      gg_interactive$x$data[[4]]$text <- purrr::map_chr(gg_interactive$x$data[[4]]$text,
                                                        ~ stringr::str_replace_all(.x, "Low:", "High Min:"))
      gg_interactive$x$data[[4]]$text <- purrr::map_chr(gg_interactive$x$data[[4]]$text,
                                                        ~ stringr::str_replace_all(.x, "Average:", "High Avg:"))
      gg_interactive$x$data[[4]]$text <- purrr::map_chr(gg_interactive$x$data[[4]]$text,
                                                        ~ stringr::str_replace_all(.x, "<br />Metric: High Temp", ""))
      gg_interactive$x$data[[4]]$text <- purrr::map_chr(gg_interactive$x$data[[4]]$text,
                                                        ~ paste(stringr::str_split(.x, "<br />")[[1]][c(3, 2, 4, 1)], collapse = "<br />"))
      
      
      gg_interactive$x$data[[5]]$text <- purrr::map_chr(gg_interactive$x$data[[5]]$text,
                                                        ~ stringr::str_replace_all(.x, "High:", "Avg Max:"))
      gg_interactive$x$data[[5]]$text <- purrr::map_chr(gg_interactive$x$data[[5]]$text,
                                                        ~ stringr::str_replace_all(.x, "Low:", "Avg Min:"))
      gg_interactive$x$data[[5]]$text <- purrr::map_chr(gg_interactive$x$data[[5]]$text,
                                                        ~ stringr::str_replace_all(.x, "Average:", "Avg:"))
      gg_interactive$x$data[[5]]$text <- purrr::map_chr(gg_interactive$x$data[[5]]$text,
                                                        ~ stringr::str_replace_all(.x, "<br />Metric: Avg Temp", ""))
      gg_interactive$x$data[[5]]$text <- purrr::map_chr(gg_interactive$x$data[[5]]$text,
                                                        ~ paste(stringr::str_split(.x, "<br />")[[1]][c(3, 2, 4, 1)], collapse = "<br />"))
      
      gg_interactive$x$data[[6]]$text <- purrr::map_chr(gg_interactive$x$data[[6]]$text,
                                                        ~ stringr::str_replace_all(.x, "High:", "Low Max:"))
      gg_interactive$x$data[[6]]$text <- purrr::map_chr(gg_interactive$x$data[[6]]$text,
                                                        ~ stringr::str_replace_all(.x, "Low:", "Low Min:"))
      gg_interactive$x$data[[6]]$text <- purrr::map_chr(gg_interactive$x$data[[6]]$text,
                                                        ~ stringr::str_replace_all(.x, "Average:", "Low Avg:"))
      gg_interactive$x$data[[6]]$text <- purrr::map_chr(gg_interactive$x$data[[6]]$text,
                                                        ~ stringr::str_replace_all(.x, "<br />Metric: Low Temp", ""))
      gg_interactive$x$data[[6]]$text <- purrr::map_chr(gg_interactive$x$data[[6]]$text,
                                                        ~ paste(stringr::str_split(.x, "<br />")[[1]][c(3, 2, 4, 1)], collapse = "<br />"))
    } else if (input$data_freq == "h") {
      gg_interactive$x$data[[1]]$text <- purrr::map_chr(gg_interactive$x$data[[1]]$text,
                                                        ~ stringr::str_replace_all(.x, "<br />Metric: Avg Temp<br />Metric: Avg Temp", ""))
      gg_interactive$x$data[[2]]$text <- purrr::map_chr(gg_interactive$x$data[[2]]$text,
                                                        ~ stringr::str_replace_all(.x, "Metric: Avg Temp<br />", ""))
      gg_interactive$x$data[[2]]$text <- purrr::map_chr(gg_interactive$x$data[[2]]$text,
                                                        ~ stringr::str_replace_all(.x, "<br />Metric: Avg Temp", ""))
      gg_interactive$x$data[[2]]$text <- purrr::map_chr(gg_interactive$x$data[[2]]$text,
                                                        ~ stringr::str_replace_all(.x, "High:", "Avg Max:"))
      gg_interactive$x$data[[2]]$text <- purrr::map_chr(gg_interactive$x$data[[2]]$text,
                                                        ~ stringr::str_replace_all(.x, "Low:", "Avg Min:"))
      gg_interactive$x$data[[2]]$text <- purrr::map_chr(gg_interactive$x$data[[2]]$text,
                                                        ~ stringr::str_replace_all(.x, "Average:", "Avg:"))
      gg_interactive$x$data[[2]]$text <- purrr::map_chr(gg_interactive$x$data[[2]]$text,
                                                        ~ paste(stringr::str_split(.x, "<br />")[[1]][c(3, 2, 4, 1)], collapse = "<br />"))
    }
    
    gg_interactive
  })
}



shinyApp(ui, server)
# library(profvis)
# #
# profvis({
# runApp(shinyApp(ui, server))
# })