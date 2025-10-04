library(shiny)
library(AdvancedRHT2025Lab05)
library(DT)
library(sf)
library(leaflet)
library(bslib)

# UI with LiU official theme colors
ui <- page_sidebar(
  title = "Thenmap API Explorer",
  theme = bs_theme(
    bootswatch = "flatly",
    # LiU-blue (#00b9e7) as primary color
    primary = "#00b9e7",
    # LiU-turquoise (#17c7d2) as secondary
    secondary = "#17c7d2",
    # LiU-green (#00cfb5) as success color
    success = "#00cfb5",
    base_font = font_google("Open Sans")
  ),

  sidebar = sidebar(
    width = 350,

    card(
      card_header("Dataset Selection"),
      selectInput("dataset",
                  "Dataset:",
                  choices = c("Sweden (Municipalities)" = "se-7",
                              "Norway (Municipalities)" = "no-7",
                              "Denmark (Municipalities)" = "dk-7",
                              "Finland (Municipalities)" = "fi-8",
                              "World (Countries)" = "world-2"),
                  selected = "se-7"),

      textInput("date",
                "Date (YYYY or YYYY-MM-DD):",
                value = "2000",
                placeholder = "e.g., 2015 or 2015-06-01")
    ),

    card(
      card_header("Temporal Comparison (Optional)"),
      checkboxInput("enable_comparison",
                    "Enable comparison mode",
                    value = FALSE),

      conditionalPanel(
        condition = "input.enable_comparison == true",
        textInput("compare_date",
                  "Compare with:",
                  value = "2015",
                  placeholder = "Must be later than base date"),
        div(
          class = "alert alert-info",
          style = "font-size: 0.85em; padding: 8px;",
          "New regions will be highlighted in green on the map"
        )
      )
    ),

    card(
      actionButton("fetch",
                   "Fetch Data",
                   class = "btn-primary btn-lg w-100",
                   icon = icon("download")),
      br(), br(),
      checkboxInput("show_map", "Display interactive map", value = TRUE)
    )
  ),

  navset_card_tab(
    nav_panel(
      "Data Table",
      icon = icon("table"),
      DTOutput("data_table")
    ),
    nav_panel(
      "Metadata",
      icon = icon("info-circle"),
      verbatimTextOutput("info_output")
    ),
    nav_panel(
      "Map View",
      icon = icon("map"),
      leafletOutput("map_output", height = 700)
    )
  )
)

# Server logic
server <- function(input, output, session) {

  # Create API instance
  api <- thenmapsAPI(version = "v2")

  # Reactive data fetching
  data_reactive <- eventReactive(input$fetch, {
    req(input$dataset, input$date)

    tryCatch({
      get_data(api = api,
               dataset_name = input$dataset,
               date = input$date)
    }, error = function(e) {
      return(data.frame(Error = paste("Failed to fetch data:", e$message)))
    })
  })

  info_reactive <- eventReactive(input$fetch, {
    req(input$dataset, input$date)

    tryCatch({
      get_info(api = api,
               dataset_name = input$dataset,
               date = input$date)
    }, error = function(e) {
      return(list(error = paste("Failed to fetch info:", e$message)))
    })
  })

  geo_reactive <- eventReactive(input$fetch, {
    req(input$dataset, input$date, input$show_map)

    tryCatch({
      get_geo(api = api,
              dataset_name = input$dataset,
              date = input$date,
              geo_props = "name")
    }, error = function(e) {
      return(NULL)
    })
  })

  geo_compare_reactive <- eventReactive(input$fetch, {
    req(input$dataset, input$show_map, input$enable_comparison, input$compare_date)

    tryCatch({
      get_geo(api = api,
              dataset_name = input$dataset,
              date = input$compare_date,
              geo_props = "name")
    }, error = function(e) {
      return(NULL)
    })
  })

  output$data_table <- renderDT({
    datatable(data_reactive(),
              options = list(pageLength = 15, scrollX = TRUE),
              filter = 'top',
              class = 'cell-border stripe')
  })

  output$info_output <- renderPrint({
    str(info_reactive())
  })

  output$map_output <- renderLeaflet({
    if (!input$show_map) {
      return(NULL)
    }

    geo_data <- geo_reactive()

    if (is.null(geo_data)) {
      return(leaflet() %>%
               addTiles() %>%
               addMarkers(lng = 0, lat = 0, popup = "No geographic data available"))
    }

    geo_data_wgs84 <- st_transform(geo_data, 4326)

    new_region_ids <- c()
    if (input$enable_comparison && !is.null(input$compare_date)) {
      geo_compare <- geo_compare_reactive()

      if (!is.null(geo_compare)) {
        base_ids <- geo_data_wgs84$id
        compare_ids <- geo_compare$id
        new_region_ids <- setdiff(compare_ids, base_ids)
        geo_data_wgs84 <- st_transform(geo_compare, 4326)
      }
    }

    label_text <- sapply(seq_len(nrow(geo_data_wgs84)), function(i) {
      name_val <- geo_data_wgs84$name[i]
      id_val <- geo_data_wgs84$id[i]
      is_new <- id_val %in% new_region_ids

      base_text <- if (is.na(name_val) || name_val == "" || name_val == "NA") {
        paste0("<strong>Region ID: ", id_val, "</strong>")
      } else {
        paste0("<strong>", name_val, "</strong><br>ID: ", id_val)
      }

      if (is_new) {
        paste0(base_text, "<br><span style='color: green;'>NEW REGION</span><br>",
               "<span style='font-size: 11px;'>Not in ", input$date, ", exists in ", input$compare_date, "</span>")
      } else {
        paste0(base_text, "<br><span style='font-size: 11px;'>Existed in: ", input$date, "</span>")
      }
    })

    # Use LiU colors in the map
    map <- leaflet(geo_data_wgs84) %>%
      addTiles()

    existing_data <- geo_data_wgs84[!geo_data_wgs84$id %in% new_region_ids, ]
    if (nrow(existing_data) > 0) {
      existing_labels <- label_text[!geo_data_wgs84$id %in% new_region_ids]
      map <- map %>%
        addPolygons(
          data = existing_data,
          fillColor = "#00b9e7",  # LiU-blue
          fillOpacity = 0.2,
          color = "#2C3E50",
          weight = 1,
          label = lapply(existing_labels, htmltools::HTML),
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "13px",
            direction = "auto"
          ),
          highlight = highlightOptions(
            weight = 3,
            color = "#17c7d2",  # LiU-turquoise for highlight
            fillOpacity = 0.8,
            bringToFront = TRUE
          )
        )
    }

    new_data <- geo_data_wgs84[geo_data_wgs84$id %in% new_region_ids, ]
    if (nrow(new_data) > 0) {
      new_labels <- label_text[geo_data_wgs84$id %in% new_region_ids]
      map <- map %>%
        addPolygons(
          data = new_data,
          fillColor = "#00cfb5",  # LiU-green for new regions
          fillOpacity = 0.8,
          color = "#17c7d2",  # LiU-turquoise border
          weight = 2,
          label = lapply(new_labels, htmltools::HTML),
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "13px",
            direction = "auto"
          ),
          highlight = highlightOptions(
            weight = 4,
            color = "#00b9e7",  # LiU-blue for highlight
            fillOpacity = 0.9,
            bringToFront = TRUE
          )
        )
    }

    map
  })
}

shinyApp(ui = ui, server = server)
