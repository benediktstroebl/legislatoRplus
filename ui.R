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
ui <- shiny::htmlTemplate(
  # Index Page
  "www/index.html",
  
  # Total MP count
  nr_of_mps = textOutput("nr_of_mps", inline = T),
  
  total_nr_of_mps = textOutput("total_nr_of_mps", inline = T),
  
  # Number of sessions supported
  nr_of_sessions = textOutput("nr_of_sessions", inline = T),
  
  total_nr_of_sessions = textOutput("total_nr_of_sessions", inline = T),
  
  # Number of countries supported
  nr_of_countries = textOutput("nr_of_countries", inline = T),
  
  # Session input
  session_selector = selectInput("session_input", "Select Legislative Sessions", session_list, multiple = TRUE),
  
  # Party input
  party_selector = selectInput("party_input", "Select Parties", party_list, multiple = TRUE),
  
  # Name input
  name_selector = selectInput("name_input", "MP Name", mp_list, selected = FALSE, multiple = FALSE),

  # Downloads
  age_plot_download_button = plotDownloadButton("age_plot_download", label = "Age Plot"),

  term_plot_download_button = plotDownloadButton("term_length_plot_download", label = "Term Length Plot"),

  # Leaflet map
  leaflet_map = leafletOutput(outputId = "map", height = "650"),
  
  age_plot = plotOutput("age_plot_final") %>% 
    withSpinner(color = "black"),

  session_plot = plotOutput("session_plot_final") %>% 
    withSpinner(color = "black")
)




