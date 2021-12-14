library(shiny)

ui <- fluidPage(
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
      6,
      plotOutput("age_plot_final")
    ),
    column(
      6,
      verbatimTextOutput("age_debug")
    )
  )
  
)