library(shiny) # remove if global.R works properly

server <- function(input, output, session) {
  
  name_list_reactive <- reactive({
    
    if (!is.null(input$session_input) & is.null(input$party_input)) {
      
      core_de %>%
        filter(
          session %in% input$session_input
        ) %>%
        distinct(name) %>%
        pull
      
    } else if (is.null(input$session_input) & !is.null(input$party_input)) {
      
      core_de %>%
        filter(
          party %in% input$party_input
        ) %>%
        distinct(name) %>%
        pull
      
    } else {
      
      core_de %>%
        filter(
          session %in% input$session_input,
          party %in% input$party_input
        ) %>%
        distinct(name) %>%
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
  
  output$age_plot_final <- renderPlot(
    ggplot()
  )

}