ui <- fluidPage(
  
  # use JS features
  useShinyjs(),
  
  # Session input
  selectInput("session_input", "Select Legislative Sessions", session_list, multiple = TRUE),
  
  # Party input
  selectInput("party_input", "Select Parties", party_list, multiple = TRUE),
  
  # Name input
  selectInput("name_input", "MP Name", mp_list, selected = FALSE, multiple = FALSE),
  
  # Leaflet map
  leafletOutput(outputId = "mymap"),
  
  br(),
  
  fluidRow(
    column(
      6,
      plotOutput("age_plot_final") %>% 
        withSpinner(color = "black")
    ),
    column(
      6,
      plotOutput("session_plot_final") %>% 
        withSpinner(color = "black")
      # verbatimTextOutput("debug_df")
      # verbatimTextOutput("debug_radar_plot_df"),
      # plotOutput("radar_plot_final") %>% 
        # withSpinner(color = "black")
    )
  )
  
)