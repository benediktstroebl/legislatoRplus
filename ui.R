# custom download functions
plotDownloadButton <- function(outputId, label = "Download") {
  tags$a(id = outputId, class = "btn btn-default shiny-download-link", href = "", 
         target = "_blank", download = NA, icon("chart-bar"), label)
}

excelDownloadButton <- function(outputId, label = "Download") {
  tags$a(id = outputId, class = "btn btn-default shiny-download-link", href = "", 
         target = "_blank", download = NA, icon("file-excel"), label)
}

# ui
ui <- fluidPage(
  
  # use JS features
  useShinyjs(),
  
  
  fluidRow(
    # Inputs
    column(
      8,
      # Session input
      selectInput("session_input", "Select Legislative Sessions", session_list, multiple = TRUE),
      
      # Party input
      selectInput("party_input", "Select Parties", party_list, multiple = TRUE),
      
      # Name input
      selectInput("name_input", "MP Name", mp_list, selected = FALSE, multiple = FALSE)
    ),
    # Downloads
    column(
      4,
      div(style = "text-align: center;", 
          plotDownloadButton("age_plot_download", label = "Age Plot")
      ),
      br(),
      div(style = "text-align: center;", 
          plotDownloadButton("term_length_plot_download", label = "Term Length Plot")
      )
    )
  ),
    
  # Leaflet map
  leafletOutput(outputId = "map"),
  
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