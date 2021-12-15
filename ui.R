library(shiny)

ui <- fluidPage(
  
  # Session input
  selectInput("session_input", "Select Legislative Sessions", session_list, multiple = TRUE),
  
  # Party input
  selectInput("party_input", "Select Parties", party_list, multiple = TRUE),
  
  # Name input
  selectInput("name_input", "MP Name", mp_list, selected = FALSE, multiple = FALSE),
  
  # Name input
  # name_selector = selectInput("name_input", "MP Name", mp_list, selected = NULL),
  selectInput("name_input", "MP Name", mp_list, selected = NULL),
  
  selectInput("session_input", "Select Legislative Sessions", session_list, multiple = TRUE),
  
  selectInput("party_input", "Select Parties", party_list, multiple = TRUE),
  
  # verbatimTextOutput("name_list_reactive_output"),
  
  # Leaflet map
  leafletOutput(outputId = "mymap"),
  
  br(),
  
  fluidRow(
    column(
      8,
      plotOutput("age_plot_final") %>% 
        withSpinner(color = "black"),
      plotOutput("session_plot_final") %>% 
        withSpinner(color = "black")
    ),
    column(
      4,
      verbatimTextOutput("debug_radar_plot_df"),
      plotOutput("radar_plot_final") %>% 
        withSpinner(color = "black")
      6,
      plotOutput("age_plot_final")
    ),
    column(
      6,
      verbatimTextOutput("age_debug")
    )
  )
  
)