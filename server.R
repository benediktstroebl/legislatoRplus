library(shiny) # remove if global.R works properly

server <- function(input, output, session) {
  
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
  
  coord_mp <- reactive(
    core_de %>%
      filter(name == input$name_input) %>%
      separate(birthplace, into = c("lat", "long"), sep = ",") %>%
      mutate(
        across(c("lat", "long"), ~ as.numeric(.x)),
        popup_image = case_when(
          !is.na(image_url) & !is.na(party_logo_url) ~ paste0(
            "<img src = ",
            image_url,
            " width='50'>",
            " </br>",
            " </br>",
            " <img src = ",
            party_logo_url,
            " width='50'>"
          ),!is.na(image_url) ~ paste0("<img src = ",
                                       party_logo_url,
                                       " width='50'>"),
          TRUE ~ "No image available."
        ),

        # age_at_death = if_else(
        #   # Just compute age at death when death is higher than birth (logical check)
        #   death > birth,
        #   # Use lubridate for the difference and round the result
        #   time_length(difftime(death, birth), "years") %>% round(3),
        #   # If the birth date is larger (more recent) than the death date, an error in the data can be assumed
        #   NA_real_
        # )
      )
  )
  
  # Reactive data frame based on input
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
    coord_mp() %>% 
      pull(name)
  )
  
  # Age plot ----------------------------------------------------------------
  # Age MP
  age_mp <- reactive(
        age_at_death = if_else(
          # Just compute age at death when death is higher than birth (logical check)
          death > birth,
          # Use lubridate for the difference and round the result
          time_length(difftime(death, birth), "years") %>% round(3),
          # If the birth date is larger (more recent) than the death date, an error in the data can be assumed
          NA_real_
        )
      )
  )
  
  output$age_debug <- renderPrint(
    coord_mp() %>% 
      pull(age_at_death)
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
             subtitle = "Histogram",
             x = "", 
             y = "")
      
    # Highlight MP with geom_vline()
    } else {
      ggplot(reactive_df(), aes(x = age_at_death)) +
        stat_slab(alpha = 0.5, justification = 0) +
        geom_boxplot(width = 0.1) +
        theme_lgl() +
        labs(title = "Age",
             subtitle = paste0("Histogram | MP: ", name_mp()),
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
                                     as.character(round(age_mp())))
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
  # Session MP
  session_mp <- reactive(
    coord_mp() %>% 
      pull(session)
  )
  
  session_plot <- reactive({
    ggplot()
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
  radar_plot_df <- reactive({
    
    deu_metrics %>% 
      filter(name %in% input$name_input)
    
  })
  
  # Data frame for radar plot -- comparison group
  
  output$debug_radar_plot_df <- renderPrint(
    radar_plot_df()
    # deu_metrics %>% 
    #   slice(1:100)
    # "test"
  )
  
  radar_plot <- reactive({
    
    radar_plot_df() %>% 
      ggplot(
        aes(
          x = var,
          y = value,
          group = name,
          color = name,
          fill = name,
        )
      ) + 
      geom_point(show.legend = FALSE) +
      geom_polygon(alpha = 0.5) +
      coord_radar() +
      labs(x = "", y = "") +
      theme_lgl() +
      theme(
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "top",
        plot.title = element_text(hjust = 0.5)
      )
    
  })
    
    output$radar_plot_final <- renderPlot({

      validate(
        need(
          (input$name_input != ""),
          "\n\n\n\nPlease select an MP"
        )
      )

      radar_plot()
      
    })

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
  
  output$age_plot_final <- renderPlot(
    ggplot()
  )

}