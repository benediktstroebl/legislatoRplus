library(shiny)
library(tidyverse)
library(leaflet)
library(legislatoR)

party_logo_path <- here::here("test_leaflet_shiny/party_logos")

dominant_party_de <- get_political("deu") %>% 
  group_by(pageid, party) %>% 
  count %>% 
  group_by(pageid) %>% 
  filter(n == max(n)) %>% 
  left_join(
    pol_de %>% 
      group_by(pageid) %>% 
      filter(session_start == min(session_start)) %>% 
      arrange(session_start) %>% 
      distinct(pageid, .keep_all = T)
  ) %>% 
  group_by(pageid) %>%
  filter(session_start == min(session_start)) 

core_de <- get_core("deu") %>% 
  left_join(get_portrait("deu")) %>% 
  left_join(get_political("deu")) %>% 
  # mutate(
  #   party_logo_path = case_when(
  #     party == "SPD" ~ paste0(party_logo_path, "/spd_logo.png"),
  #     party == "CDU" ~ paste0(party_logo_path, "/cdu_logo.png"),
  #     party == "FDP" ~ paste0(party_logo_path, "/fdp_logo.png"),
  #     party == "CSU" ~ paste0(party_logo_path, "/csu_logo.png"),
  #     party == "BÜNDNIS 90/DIE GRÜNEN" ~ paste0(party_logo_path, "/b90gr_logo.png"),
  #     party == "DIE LINKE" ~ paste0(party_logo_path, "/linke_logo.png"),
  #     party == "PDS" ~ paste0(party_logo_path, "/pds_logo.png"),
  #     party == "AfD" ~ paste0(party_logo_path, "/afd_logo.png"),
  #     party == "DP" ~ paste0(party_logo_path, "/dp_logo.png"),
  #     TRUE ~ NA_character_
  #   )
  # ) %>%
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
  distinct(pageid, wikidataid, name, party, party_logo_url, birthplace, image_url) %>% 
  left_join(dominant_party_de) %>% 
  arrange(session_start) %>% 
  distinct(pageid, .keep_all = TRUE)

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

mp_list <- core_de %>% 
  slice(1:100) %>%
  distinct(name) %>% 
  pull

ui <- fluidPage(
  leafletOutput("mymap"),
  p(),
  selectInput("name_input", "MP Name", mp_list, selected = NULL)
)

server <- function(input, output, session) {
  
 coord_mp <- reactive(
   core_de %>% 
     filter(name == input$name_input) %>% 
     separate(birthplace, into = c("lat", "long"), sep = ",") %>% 
     mutate(across(c("lat", "long"), ~ as.numeric(.x)),
            popup_image = ifelse( # add case_when options with na if both urls not available
              !is.na(image_url),
              paste0("<img src = ", image_url, " width='100'>"),
              paste0("<img src = ", party_logo_url, " width='100'>")
              # "No image available."
            )
     )
 )
  
  output$mymap <- renderLeaflet({
    leaflet(coord_mp(),
            options = leafletOptions(zoomControl = FALSE,
                                     minZoom = 6, maxZoom = 6)) %>% 
      # setView(-96, 37.8, 4) %>%
      addTiles() %>% 
      addMarkers(lat = ~lat, lng = ~long, popup = ~ popup_image)
  })
}

shinyApp(ui, server)