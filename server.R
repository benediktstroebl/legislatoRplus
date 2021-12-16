# server
server <- function(input, output, session) {
  
  # Reactive name list based on varying inputs
  name_list_reactive <- reactive({
    
    if (!is.null(input$session_input) & is.null(input$party_input)) {
      
      pol_core_de %>%
        filter(
          session %in% input$session_input
        ) %>%
        distinct(name) %>%
        add_row(name = "", .before = 1) %>% 
        pull
      
    } else if (is.null(input$session_input) & !is.null(input$party_input)) {
      
      pol_core_de %>%
        filter(
          party %in% input$party_input
        ) %>%
        distinct(name) %>%
        add_row(name = "", .before = 1) %>% 
        pull
      
    } else {
      
      pol_core_de %>%
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
    pol_core_de %>%
      filter(name == input$name_input)
  )
  
  # Reactive data frame based on session and party input
  reactive_df <- reactive({
    ## 'session_input'
    if (!is.null(input$session_input) & is.null(input$party_input)) {
      pol_core_de %>% 
        filter(session %in% input$session_input)
    ## 'party_input'
    } else if (is.null(input$session_input) & !is.null(input$party_input)) {
      pol_core_de %>% 
        filter(party %in% input$party_input)
    ## both
    } else {
      pol_core_de %>% 
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
      distinct(age_at_death) %>% 
      pull
  )

  # Age plot
  age_plot <- reactive({
    
    # Plot without highlight
    if (input$name_input == "") {
      ggplot(reactive_df(), aes(x = age_at_death)) +
        stat_slab(alpha = 0.5, justification = 0) +
        geom_boxplot(width = 0.1) +
        theme_lgl() +
        labs(title = "Age",
             subtitle = "Density & Boxplot",
             x = "", 
             y = "")
      
    # Highlight MP with geom_vline()
    } else {
      ggplot(reactive_df(), aes(x = age_at_death)) +
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
  output$mymap <- renderLeaflet({
    # Plot on leaflet map
    leaflet() %>%
      addProviderTiles("CartoDB.Positron", options = providerTileOptions(opacity = 0.99)) %>%
      addPolygons(
        data = btw21_wahlkreise_spdf,
        stroke = TRUE,
        weight = 1,
        color = "#968C83",
        fillColor = '#968C83',
        fillOpacity = 0.5,
        smoothFactor = 0.5,
        popup = ~ WKR_NAME,
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