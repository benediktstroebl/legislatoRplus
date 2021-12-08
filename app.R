library(shiny)
library(tidyverse)
library(leaflet)
library(legislatoR)
library(lubridate)

# party_logo_path <- here::here("test_leaflet_shiny/party_logos")

portrait_de <- get_portrait("deu")
political_de <- get_political("deu")

session_list <- political_de %>% 
  distinct(session) %>% 
  pull

party_list <- political_de %>% 
  distinct(party) %>% 
  pull

# core_de <- get_core("deu") %>%
#   left_join(portrait_de) %>%
#   left_join(political_de) 

dominant_party_de <- get_political("deu") %>%
  group_by(pageid, party) %>%
  count %>%
  group_by(pageid) %>%
  filter(n == max(n)) %>%
  left_join(
    get_political("deu") %>%
      group_by(pageid) %>%
      filter(session_start == min(session_start)) %>%
      arrange(session_start) %>%
      distinct(pageid, .keep_all = T)
  ) %>%
  group_by(pageid) %>%
  filter(session_start == min(session_start))

core_de <- get_core("deu") %>%
  left_join(portrait_de) %>%
  left_join(political_de) %>% 
  mutate(
  party_logo_url = case_when(
    party == "SPD" ~ "https://upload.wikimedia.org/wikipedia/commons/thumb/2/2d/Sozialdemokratische_Partei_Deutschlands%2C_Logo_um_2000.svg/1024px-Sozialdemokratische_Partei_Deutschlands%2C_Logo_um_2000.svg.png",
    party == "CDU" ~ "https://upload.wikimedia.org/wikipedia/commons/thumb/b/b3/CDU_logo.svg/1000px-CDU_logo.svg.png",
    party == "FDP" ~ "https://upload.wikimedia.org/wikipedia/commons/thumb/e/e2/Logo_der_Freien_Demokraten.svg/1280px-Logo_der_Freien_Demokraten.svg.png",
    party == "CSU" ~ "https://upload.wikimedia.org/wikipedia/commons/thumb/e/ee/CSU_Logo_since_2016.svg/1280px-CSU_Logo_since_2016.svg.png",
    party == "BÜNDNIS 90/DIE GRÜNEN" ~ "https://upload.wikimedia.org/wikipedia/commons/thumb/4/4b/Bündnis_90_-_Die_Grünen_Logo.svg/1280px-Bündnis_90_-_Die_Grünen_Logo.svg.png",
    party == "DIE LINKE" ~ "https://upload.wikimedia.org/wikipedia/commons/thumb/4/45/Die_Linke_logo.svg/1280px-Die_Linke_logo.svg.png",
    party == "PDS" ~ "https://upload.wikimedia.org/wikipedia/commons/thumb/a/a9/PDS-Logo.svg/1280px-PDS-Logo.svg.png",
    party == "AfD" ~ "https://upload.wikimedia.org/wikipedia/commons/thumb/4/48/AfD-Logo-2017.svg/1280px-AfD-Logo-2017.svg.png",
    party == "DP" ~ "https://upload.wikimedia.org/wikipedia/commons/thumb/8/82/Logo_Deutsche_Partei.svg/1280px-Logo_Deutsche_Partei.svg.png",
    TRUE ~ NA_character_
  )
) %>%
  distinct(pageid,
           wikidataid,
           name,
           party,
           party_logo_url,
           birthplace,
           image_url) %>%
  left_join(dominant_party_de) %>%
  arrange(session_start) %>%
  distinct(pageid, .keep_all = TRUE) %>%
  drop_na(constituency) %>%
  mutate(constituency_merge = str_replace_all(constituency, " ", "")) %>% 
  left_join(btw17_wahlkreisnamen, keep = TRUE, by = c("constituency_merge" = "wkr_merge"))

mp_list <- core_de %>%
  distinct(name) %>%
  pull

 

# r_colors <- rgb(t(col2rgb(colors()) / 255))
# names(r_colors) <- colors()


#
# ui <- fluidPage(
#   leafletOutput("mymap"),
#   p(),
#   selectInput("name_input", "MP Name", mp_list, selected = NULL)
# )

# ui <- shiny::htmlTemplate(
  # Index Page
  # "www/index.html",
  
ui <- fluidPage(
  # Name input
  # name_selector = selectInput("name_input", "MP Name", mp_list, selected = NULL),
  selectInput("name_input", "MP Name", mp_list, selected = NULL),
  
  selectInput("session_input", "Select Legislative Sessions", session_list, multiple = TRUE),
  
  selectInput("party_input", "Select Parties", party_list, multiple = TRUE),
  
  verbatimTextOutput("name_list_reactive_output"),
  
  # Leaflet map
  leafletOutput(outputId = "mymap"),
  
  br()
  
  # plotOutput("age_plot_final")
)

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
      
    # } else if (is.null(input$session_input) & is.null(input$party_input)) {
      
      # mp_list
      
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
  
  # debugging
  output$name_list_reactive_output <- renderPrint(name_list_reactive())
  
  # update MP name input according to session and party selection
  ## trigger: 'session_input'
  observeEvent(input$session_input, {

    # MP name input
    updateSelectInput(session,
                      "name_input",
                      choices = name_list_reactive(),
                      selected = name_list_reactive()[1])

  })
  
  # update MP name input according to session and party selection
  ## trigger: 'party_input'
  observeEvent(input$party_input, {

    # MP name input
    updateSelectInput(session,
                      "name_input",
                      choices = name_list_reactive(),
                      selected = name_list_reactive()[1])

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
        )
      )
  )
  
  output$mymap <- renderLeaflet({
    # Plot on leaflet map
    leaflet() %>%
      addProviderTiles("CartoDB.Positron", options = providerTileOptions(opacity = 0.99)) %>%
      addPolygons(
        data = wahlkreise_ll,
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
  
  # output$mymap <- renderLeaflet({
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