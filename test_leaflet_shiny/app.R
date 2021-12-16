r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

mp_list <- core_de %>%
  filter(session == 19) %>% 
  slice(1:100) %>%
  distinct(name) %>%
  pull

ui <- shiny::htmlTemplate(
  # Index Page
  "www/index.html",
  
  # Name input
  name_selector = selectInput("name_input", "MP Name", mp_list, selected = NULL),
  
  # Leaflet map
  leaflet_map = leafletOutput(outputId = "map", height = "800")
  )

server <- function(input, output, session) {

  
  # Select right SpatialPolygon data for leaflet based on leg. session
  wahlkreis_ll <- reactive({
    # case_when(
    #   input$session_input == "BTW2021 | LP20" ~ btw21_wahlkreise_spdf,
    #   input$session_input == "BTW2017 | LP19" ~ btw17_wahlkreise_spdf,
    #   input$session_input == "BTW2013 | LP18" ~ btw13_wahlkreise_spdf,
    #   input$session_input == "BTW2009 | LP17" ~ btw09_wahlkreise_spdf,
    #   input$session_input == "BTW2005 | LP16" ~ btw05_wahlkreise_spdf,
    #   input$session_input == "BTW2002 | LP15" ~ btw02_wahlkreise_spdf
    # )
    
    btw17_wahlkreise_spdf
  })

  
  output$map <- renderLeaflet({
    # Plot on leaflet map
    leaflet() %>%
      addProviderTiles("CartoDB.Positron", options = providerTileOptions(opacity = 0.99)) %>%
      addPolygons(
        data = wahlkreis_ll(),
        stroke = TRUE,
        weight = 1,
        color = ~ party_color,
        fillColor = ~ party_color,
        fillOpacity = 0.5,
        smoothFactor = 0.5,
        popup = ~ popup_image,
        highlightOptions = highlightOptions(
          color = '#636363',
          fillColor = '#636363',
          opacity = 1,
          weight = 2,
          fillOpacity = 0.5,
          bringToFront = TRUE,
          sendToBack = TRUE
        )
      )
  })
  
  
  # output$map <- renderLeaflet({
  #   leaflet(coord_mp(),
  #           options = leafletOptions(
  #             zoomControl = FALSE,
  #             minZoom = 6,
  #             maxZoom = 6
  #           )) %>%
  #     # setView(-96, 37.8, 4) %>%
  #     addTiles() %>%
  #     addMarkers(lat = ~ lat,
  #                lng = ~ long,
  #                popup = ~ popup_image)
  # })
}

shinyApp(ui, server)