# server
server <- function(input, output, session) {
  
  # assign constants for overview section
  output$nr_of_mps <- renderText({
    core_de %>% 
      distinct(pageid) %>% 
      nrow() 
  })
  
  output$total_nr_of_mps <- renderText({
    deu_core %>% 
      distinct(pageid) %>% 
      nrow() %>% 
      as.character()
  })
  
  output$nr_of_sessions <- renderText({
    core_de %>% 
      distinct(session) %>% 
      nrow()
  })
  
  output$total_nr_of_sessions <- renderText({
    deu_political %>% 
      distinct(session) %>% 
      nrow() 
  })
  
  output$nr_of_countries <- renderText({
    "1"
  })
  
  
  # Reactive name list based on varying inputs
  name_list_reactive <- reactive({
    
    if (!is.null(input$session_input) & is.null(input$party_input)) {
      
      core_de %>%
        filter(
          session %in% input$session_input
        ) %>%
        distinct(name) %>%
        add_row(name = "", .before = 1) %>% 
        pull
      
    } else if (is.null(input$session_input) & !is.null(input$party_input)) {
      
      core_de %>%
        filter(
          party %in% input$party_input
        ) %>%
        distinct(name) %>%
        add_row(name = "", .before = 1) %>% 
        pull
      
    } else {
      
      core_de %>%
        filter(
          session %in% input$session_input,
          party %in% input$party_input
        ) %>%
        distinct(name) %>%
        add_row(name = "", .before = 1) %>% 
        pull
      
    }
    
  })
  
  # update MP name input according to session and party selection
  observe({
    
    ## trigger: 'session_input'
    if (!is.null(input$session_input) & is.null(input$party_input)) {
      
      updateSelectInput(session,
                        "name_input",
                        choices = name_list_reactive(),
                        selected = name_list_reactive()[1])
      
    ## trigger: 'party_input'  
    } else if (is.null(input$session_input) & !is.null(input$party_input)) {
      
      updateSelectInput(session,
                        "name_input",
                        choices = name_list_reactive(),
                        selected = name_list_reactive()[1])
      
    ## trigger: both  
    } else if (!is.null(input$session_input) & !is.null(input$party_input)) {
      
      updateSelectInput(session,
                        "name_input",
                        choices = name_list_reactive(),
                        selected = name_list_reactive()[1])
      
    ## trigger: none
    } else {
      
      mp_list
      
    }
    
  })
  
  # Reactive data frame based on MP name input
  mp_df <- reactive(
    core_de %>%
      filter(name == input$name_input)
  )
  
  # Reactive data frame based on session and party input
  reactive_df <- reactive({
    ## 'session_input'
    if (!is.null(input$session_input) & is.null(input$party_input)) {
      core_de %>% 
        filter(session %in% input$session_input)
    ## 'party_input'
    } else if (is.null(input$session_input) & !is.null(input$party_input)) {
      core_de %>% 
        filter(party %in% input$party_input)
    ## both
    } else {
      core_de %>% 
        filter(session %in% input$session_input,
               party %in% input$party_input)
    }
  })
  
  # Name MP
  name_mp <- reactive(
    mp_df() %>% 
      distinct(name) %>% 
      pull

  )
  
  # Age plot ----------------------------------------------------------------
  # Age MP
  age_mp <- reactive(
    mp_df() %>% 
      distinct(age) %>% 
      pull
  )

  # Age plot
  age_plot <- reactive({
    
    # Plot without highlight
    if (input$name_input == "") {
      ggplot(reactive_df(), aes(x = age)) +
        stat_slab(alpha = 0.5, justification = 0) +
        geom_boxplot(width = 0.1) +
        theme_lgl() +
        labs(title = "Age",
             subtitle = "Density & Boxplot",
             x = "", 
             y = "")
      
    # Highlight MP with geom_vline()
    } else {
      ggplot(reactive_df(), aes(x = age)) +
        stat_slab(alpha = 0.5, justification = 0) +
        geom_boxplot(width = 0.1) +
        theme_lgl() +
        labs(title = "Age",
             subtitle = paste0("Density & Boxplot | MP: ", name_mp()),
             x = "", 
             y = "") +
        geom_vline(xintercept = age_mp(), 
                   alpha = 0.5,
                   color = "black") + 
        geom_text(aes(x = age_mp(), 
                      y = 0.5, 
                      label = paste0("Age ", 
                                     name_mp(),
                                     ": ", 
                                     as.character(round(age_mp())),
                                     " years")
                      ), 
                  angle = 90,
                  vjust = -0.5,
                  family = "Corbel",
                  color = "grey40")
    }
    
  })
  
  # Final age plot (histogram)
  output$age_plot_final <- renderPlot({
    
    validate(
      need(
        (!is.null(input$session_input) || !is.null(input$party_input)),
        "\n\n\n\nPlease select at least one session or one party"
      )
    )
    
   age_plot()
    
  })
  
  # Session plot ------------------------------------------------------------
  # Term Length MP
  term_length_mp <- reactive(
    mp_df() %>% 
      distinct(term_length) %>% 
      pull
  )
  
  # Value for dynamic x axis of plots
  max_x_axis <- reactive(
    reactive_df() %>% 
      filter(term_length == max(term_length)) %>% 
      distinct(term_length) %>% 
      pull
  )
  
  session_plot <- reactive({
    
    # Plot without highlight
    if (input$name_input == "") {
      ggplot(reactive_df(), aes(x = term_length)) +
        stat_slab(alpha = 0.5, justification = 0) +
        geom_boxplot(width = 0.1) +
        xlim(0, max_x_axis()) +
        theme_lgl() +
        labs(title = "Total Term of Office",
             subtitle = "Density & Boxplot",
             x = "", 
             y = "")
      
      # Highlight MP with geom_vline()
    } else {
      ggplot(reactive_df(), aes(x = term_length)) +
        stat_slab(alpha = 0.5, justification = 0) +
        geom_boxplot(width = 0.1) +
        xlim(0, max_x_axis()) +
        theme_lgl() +
        labs(title = "Total Term of Office",
             subtitle = paste0("Density & Boxplot | MP: ", name_mp()),
             x = "", 
             y = "") +
        geom_vline(xintercept = term_length_mp(), 
                   alpha = 0.5,
                   color = "black") + 
        geom_text(aes(x = term_length_mp(), 
                      y = 0.5, 
                      label = paste0("Total Term of Office ", 
                                     name_mp(),
                                     ": ", 
                                     as.character(round(term_length_mp())),
                                     " years")
        ), 
        angle = 90,
        vjust = -0.5,
        family = "Corbel",
        color = "grey40")
    }
    
  })
  
  # Session plot (histogram)
  output$session_plot_final <- renderPlot({
    
    validate(
      need(
        (!is.null(input$session_input) || !is.null(input$party_input)),
        "\n\n\n\nPlease select at least one session or one party"
      )
    )
    
    session_plot()
    
  })

  # Radar plot --------------------------------------------------------------
  # Data frame for radar plot -- MP
  # radar_plot_df <- reactive({
  #   
  #   deu_metrics %>% 
  #     filter(name %in% input$name_input)
  #   
  # })
  
  # Data frame for radar plot -- comparison group
  
  # output$debug_radar_plot_df <- renderPrint(
  #   radar_plot_df()
  # )
  
  # radar_plot <- reactive({
  #   
  #   radar_plot_df() %>% 
  #     ggplot(
  #       aes(
  #         x = var,
  #         y = value,
  #         group = name,
  #         color = name,
  #         fill = name
  #       )
  #     ) + 
  #     geom_point(show.legend = FALSE) +
  #     geom_polygon(alpha = 0.5) +
  #     coord_radar() +
  #     labs(x = "", y = "") +
  #     theme_lgl() +
  #     theme(
  #       axis.text.y = element_blank(),
  #       axis.ticks.y = element_blank(),
  #       legend.position = "top",
  #       plot.title = element_text(hjust = 0.5)
  #     )
  #   
  # })
    
    # output$radar_plot_final <- renderPlot({
    # 
    #   validate(
    #     need(
    #       (input$name_input != ""),
    #       "\n\n\n\nPlease select an MP"
    #     )
    #   )
    # 
    #   radar_plot()
    #   
    # })
  

  

  

  # Map ---------------------------------------------------------------------
  
  # Select right SpatialPolygon data for leaflet based on leg. session
  wahlkreis_ll <- reactive({
    btw_spdf <- list(
      "20" = btw21_wahlkreise_spdf,
      "19" = btw17_wahlkreise_spdf,
      "18" = btw13_wahlkreise_spdf,
      "17" = btw09_wahlkreise_spdf,
      "16" = btw05_wahlkreise_spdf,
      "15" = btw02_wahlkreise_spdf
    )
    

    # Select the right SpatialPolygon data for the selected session
    if (length(input$session_input) > 0){
      selected_spdf <- btw_spdf[[max(input$session_input)]]

    } else {
      selected_spdf <- btw_spdf[[as.character(max(session_list))]]
    }
    

      
      # Assign color scheme depending on selectors
      if (input$name_input != "" & length(input$party_input) == 0){
        print("1")
        selected_spdf@data <- selected_spdf@data %>%
          mutate(
            party_color = case_when(name == input$name_input ~
                                      party_color,
                                    TRUE ~ "#d9d9d9"),
            fill_opacity = case_when(name == input$name_input ~
                                       0.7,
                                     TRUE ~ 0.4))
      } else if (length(input$party_input) > 0 & input$name_input == "") {
        # If party, but no MP is selected, highlight party constituencies
        print("2")
        
        selected_spdf@data <- selected_spdf@data %>%
          mutate(
            party_color = case_when(party %in% input$party_input ~
                                      party_color,
                                    TRUE ~ "#d9d9d9"),
            fill_opacity = case_when(party %in% input$party_input ~
                                       0.7,
                                     TRUE ~ 0.4))
      } else if (length(input$party_input) > 0 & input$name_input != "") {
        print("3")
        
        selected_spdf@data <- selected_spdf@data %>%
          mutate(
            party_color = case_when(party %in% input$party_input ~
                                      party_color,
                                    TRUE ~ "#d9d9d9"),
            fill_opacity = case_when(party %in% input$party_input & name != input$name_input ~
                                       0.3,
                                     party %in% input$party_input & name == input$name_input ~
                                       0.8,
                                     TRUE ~ 0.3)
          )
      } else {
        selected_spdf@data <- selected_spdf@data %>%
          mutate(
            fill_opacity = 0.5)
      }
      
      # Add state border if selected MP is elected via Landeslist
      if (str_detect(input$name_input, "Landesliste")) {
        # Find name of state that this MP was running in for the latest selected session
        land_name <- core_de %>%
          filter(name == input$name_input) %>%
          distinct(constituency2) %>%
          pull
        
        # Find party color of MP
        MP_party_color <- core_de %>%
          filter(name == input$name_input) %>%
          select(party_color) %>%
          distinct() %>% 
          pull

        # create new spdf with aggregated polygon for land border
        land_border_spdf <- raster::aggregate(selected_spdf, by = "LAND_NAME") %>% subset(LAND_NAME == land_name)
        # assign NA columns to be able to bind with full dataframe again
        land_border_spdf@data[colnames(btw02_wahlkreise_spdf@data)] <- NA
        # assign values to formatting relevant columns
        land_border_spdf@data <-land_border_spdf@data %>%
          mutate(
            fill_opacity = 0,
            party_color = MP_party_color,
            border_weight = 5
          )

        # Bind additional polygon with state border to spdf for selected session
        selected_spdf <- rbind(land_border_spdf, selected_spdf)
      }
      
    return(selected_spdf)
   
    
  })
  
  highlight_options <- highlightOptions(
    color = '#636363',
    fillColor = '#636363',
    opacity = 1,
    weight = 2,
    fillOpacity = ~ fill_opacity,
    bringToFront = FALSE
  )
  
  color_pal <- reactive({
    # Party color palette for leaflet map legend -------------------------------
    if (length(input$session_input) > 0) {
      # if at least one session is selected
      filtered_color_df <- core_de %>% 
        filter(session %in% input$session_input) %>% 
        filter(!str_detect(constituency, "Landesliste")) %>% 
        distinct(party, party_color) %>% 
        filter(!is.na(party)) 
    } else {
      # If no specific session is selected
      filtered_color_df <- core_de %>% 
        filter(party != "none") %>% 
        filter(session == max(session)) %>% 
        filter(!str_detect(constituency, "Landesliste")) %>% 
        distinct(party, party_color) %>% 
        filter(!is.na(party))
    }
    
    # Create factor color palette
    pal <- colorFactor(palette = filtered_color_df %>% 
                                  pull(party_color),
                       levels = filtered_color_df %>% 
                                  pull(party))
    return(pal)
  })
  
  
  
  
  output$map <- renderLeaflet({
      

  # Plot on leaflet map
  if (length(input$session_input) > 1) {
    
    leaflet() %>%
      addProviderTiles("CartoDB.Positron", options = providerTileOptions(opacity = 0.99)) %>%
      addPolygons(
        data = wahlkreis_ll(),
        group = "2002 Election",
        stroke = TRUE,
        weight = ~ border_weight,
        color = ~ party_color,
        fillColor = ~ party_color,
        fillOpacity = ~ fill_opacity,
        smoothFactor = 0.5,
        layerId = ~ name,
        popup = ~ popup_image,
        highlightOptions = highlight_options
      ) %>%
      addLegend(
        data = wahlkreis_ll(),
        title = "",
        position = "bottomleft",
        pal = color_pal(),
        values = ~ party,
        opacity = 1
      ) 
    } else {
      leaflet() %>%
        addProviderTiles("CartoDB.Positron", options = providerTileOptions(opacity = 0.99)) %>%
        addPolygons(
          data = wahlkreis_ll(),
          stroke = TRUE,
          weight = ~ border_weight,
          color = ~ party_color,
          fillColor = ~ party_color,
          fillOpacity = ~ fill_opacity,
          smoothFactor = 0.5,
          layerId = ~ name,
          popup = ~ popup_image,
          highlightOptions = highlight_options
        ) %>%
        addLegend(
          data = wahlkreis_ll()[wahlkreis_ll()@data$party != "none" & !is.na(wahlkreis_ll()@data$party),],
          title = "",
          position = "bottomleft",
          pal = color_pal(),
          values = ~ party,
          opacity = 1
        )
    }
      
  })
  
  # Observe event for filtering with clicking in map
  # observe({
  #   event <- input$map_shape_click
  #   if (is.null(event))
  #     return()
  #   updateSelectInput(session, "name_input", selected = event$id)
  # })
  

  # Download section --------------------------------------------------------
  # Age plot
  output$age_plot_download <- downloadHandler(
    
    filename = "age_plot.png",
    
    content = function(file) {
      
      ggsave(age_plot(), filename = file, device = "png", width = 8, height = 5)
      
    }
    
  )
  
  # Term length plot
  output$term_length_plot_download <- downloadHandler(
    
    filename = "term_length_plot.png",
    
    content = function(file) {
      
      ggsave(session_plot(), filename = file, device = "png", width = 8, height = 5)
      
    }
    
  )
  
  # Usability features ------------------------------------------------------
  observe({
    
    if (is.null(input$session_input) & is.null(input$party_input)) {
      
      # disable name input if there is no session and party input
      disable("name_input")
      
      # disable downloads 
      disable("age_plot_download")
      disable("term_length_plot_download")
      
    } else {
      
      # enable name input if there is session and/or party input
      enable("name_input")
      
      # enable downloads 
      enable("age_plot_download")
      enable("term_length_plot_download")
      
    }
    
  })

}