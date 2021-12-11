# library(shiny)
# library(tidyverse)
# library(leaflet)
# library(legislatoR)
# library(lubridate)
# 
# party_logo_path <- here::here("test_leaflet_shiny/party_logos")
# 
# # input list party
# party_list <- get_political("deu") %>% 
#   distinct(party) %>% 
#   pull
# 
# # input list session
# session_list <- get_political("deu") %>% 
#   distinct(session) %>% 
#   pull
# 
# dominant_party_de <- get_political("deu") %>% 
#   group_by(pageid, party) %>% 
#   count %>% 
#   group_by(pageid) %>% 
#   filter(n == max(n)) 
# party_logo_path <- here::here("test_leaflet_shiny/party_logos")
# 
# dominant_party_de <- get_political("deu") %>%
#   group_by(pageid, party) %>%
#   count %>%
#   group_by(pageid) %>%
#   filter(n == max(n)) %>%
#   left_join(
#     get_political("deu") %>%
#       group_by(pageid) %>%
#       filter(session_start == min(session_start)) %>%
#       arrange(session_start) %>%
#       distinct(pageid, .keep_all = T)
#   ) %>%
#   group_by(pageid) %>%
#   filter(session_start == min(session_start))
# 
# core_de <- get_core("deu") %>%
#   left_join(get_portrait("deu")) %>%
#   left_join(get_political("deu")) %>%
#   # mutate(
#   #   party_logo_path = case_when(
#   #     party == "SPD" ~ paste0(party_logo_path, "/spd_logo.png"),
#   #     party == "CDU" ~ paste0(party_logo_path, "/cdu_logo.png"),
#   #     party == "FDP" ~ paste0(party_logo_path, "/fdp_logo.png"),
#   #     party == "CSU" ~ paste0(party_logo_path, "/csu_logo.png"),
#   #     party == "BÜNDNIS 90/DIE GRÜNEN" ~ paste0(party_logo_path, "/b90gr_logo.png"),
#   #     party == "DIE LINKE" ~ paste0(party_logo_path, "/linke_logo.png"),
#   #     party == "PDS" ~ paste0(party_logo_path, "/pds_logo.png"),
#   #     party == "AfD" ~ paste0(party_logo_path, "/afd_logo.png"),
#   #     party == "DP" ~ paste0(party_logo_path, "/dp_logo.png"),
# #     TRUE ~ NA_character_
# #   )
# # ) %>%
# mutate(
#   party_logo_url = case_when(
#     party == "SPD" ~ "https://upload.wikimedia.org/wikipedia/commons/thumb/2/2d/Sozialdemokratische_Partei_Deutschlands%2C_Logo_um_2000.svg/1024px-Sozialdemokratische_Partei_Deutschlands%2C_Logo_um_2000.svg.png",
#     party == "CDU" ~ "https://upload.wikimedia.org/wikipedia/commons/thumb/b/b3/CDU_logo.svg/1000px-CDU_logo.svg.png",
#     party == "FDP" ~ "https://upload.wikimedia.org/wikipedia/commons/thumb/e/e2/Logo_der_Freien_Demokraten.svg/1280px-Logo_der_Freien_Demokraten.svg.png",
#     party == "CSU" ~ "https://upload.wikimedia.org/wikipedia/commons/thumb/e/ee/CSU_Logo_since_2016.svg/1280px-CSU_Logo_since_2016.svg.png",
#     party == "BÜNDNIS 90/DIE GRÜNEN" ~ "https://upload.wikimedia.org/wikipedia/commons/thumb/4/4b/Bündnis_90_-_Die_Grünen_Logo.svg/1280px-Bündnis_90_-_Die_Grünen_Logo.svg.png",
#     party == "DIE LINKE" ~ "https://upload.wikimedia.org/wikipedia/commons/thumb/4/45/Die_Linke_logo.svg/1280px-Die_Linke_logo.svg.png",
#     party == "PDS" ~ "https://upload.wikimedia.org/wikipedia/commons/thumb/a/a9/PDS-Logo.svg/1280px-PDS-Logo.svg.png",
#     party == "AfD" ~ "https://upload.wikimedia.org/wikipedia/commons/thumb/4/48/AfD-Logo-2017.svg/1280px-AfD-Logo-2017.svg.png",
#     party == "DP" ~ "https://upload.wikimedia.org/wikipedia/commons/thumb/8/82/Logo_Deutsche_Partei.svg/1280px-Logo_Deutsche_Partei.svg.png",
#     TRUE ~ NA_character_
#   )
# ) %>%
#   distinct(pageid,
#            wikidataid,
#            name,
#            party,
#            party_logo_url,
#            birthplace,
#            image_url) %>%
#   left_join(dominant_party_de) %>%
#   arrange(session_start) %>%
#   distinct(pageid, .keep_all = TRUE) %>%
#   drop_na(constituency) %>% 
#   left_join(btw17_wahlkreisnamen, keep = TRUE, by = c("constituency" = "WKR_NAME"))
# 
# r_colors <- rgb(t(col2rgb(colors()) / 255))
# names(r_colors) <- colors()
# 
# mp_list <- core_de %>%
#   slice(1:100) %>%
#   distinct(name) %>%
#   pull
# #
# # ui <- fluidPage(
# #   leafletOutput("mymap"),
# #   p(),
# #   selectInput("name_input", "MP Name", mp_list, selected = NULL)
# # )
# 
# # ui <- fluidPage(
# #   selectInput("session_input", "Select Legislative Sessions", session_list, multiple = TRUE),
# #   selectInput("party_input", "Select Parties", party_list, multiple = TRUE),
# #   br(),
# #   selectInput("name_input", "MP Name", mp_list, selected = NULL),
# #   leafletOutput("mymap"),
# #   p(),
# #   plotOutput("age_plot_final"),
# #   verbatimTextOutput("test_df")
# #   # plotOutput("plot_session_n")
# # )
# # 
# # server <- function(input, output, session) {
# #   
# #   
# #  coord_mp <- reactive(
# #    core_de %>% 
# #      filter(name == input$name_input) %>% 
# #      separate(birthplace, into = c("lat", "long"), sep = ",") %>% 
# #      mutate(across(c("lat", "long"), ~ as.numeric(.x)),
# #             popup_image = case_when( 
# #               !is.na(image_url) & !is.na(party_logo_url) ~ paste0(
# #                 "<img src = ", 
# #                 image_url, 
# #                 " width='50'>",
# #                 " </br>",
# #                 " </br>",
# #                 " <img src = ", 
# #                 party_logo_url, 
# #                 " width='50'>"
# #                 ),
# #               !is.na(image_url) ~ paste0(
# #                 "<img src = ", 
# #                 party_logo_url, 
# #                 " width='50'>"
# #               ),
# #               TRUE ~ "No image available."
# #             )
# #      )
# #  )
#  
#  # age_df <- reactive(
#  #   get_political("deu") %>% 
#  #     left_join(get_core("deu")) %>% 
#  #     filter(
#  #       party %in% input$party_input,
#  #       session %in% input$session_input
#  #     ) %>% 
#  #     mutate(
#  #       age_at_death = if_else(
#  #         # Just compute age at death when death is higher than birth (logical check)
#  #         death > birth,
#  #         # Use lubridate for the difference and round the result
#  #         time_length(difftime(death, birth), "years") %>% round(3),
#  #         # If the birth date is larger (more recent) than the death date, an error in the data can be assumed
#  #         NA_real_
#  #       )
#  #     )
#  # )
#  
#  # age_mp <- reactive(
#  #   age_df() %>% 
#  #     filter(name %in% input$name_input) %>% 
#  #     pull(age_at_death)
#  # )
#  # 
#  # age_plot <- reactive({
#  #   
#  #   if (is.null(input$name_input)) {
#  #     
#  #     age_df() %>%
#  #       ggplot(aes(x = age_at_death)) +
#  #       geom_density() 
#  #     
#  #   } else {
#  #     
#  # 
#  #      age_df() %>%
#  #         ggplot(aes(x = age_at_death)) +
#  #         geom_density() +
#  #         geom_vline(xintercept = age_mp())
#  # 
#  #     
#  #   }
#  #   
#  # })
#  #   
#  # 
#  # 
#  # 
#  # 
#  # 
#  # 
#  # output$age_plot_final <- renderPlot(
#  #   age_plot()
#  # )
#  # 
#  # # debug
#  # output$test_df <- renderPrint(
#  #   age_df()
#  # )
#  # 
#  # icons <- awesomeIcons(
#  #   icon = "user",
#  #   markerColor = "lightgray",
#  #   iconColor = "white",
#  #   library = "fa"
#  # )
#   
#   output$mymap <- renderLeaflet({
#     leaflet(coord_mp(),
#             options = leafletOptions(zoomControl = FALSE,
#                                      minZoom = 6, maxZoom = 6)) %>% 
#       # addTiles() %>% 
#       addAwesomeMarkers(lat = ~lat, lng = ~long, popup = ~popup_image, icon = icons) %>% 
#       addProviderTiles(providers$Stamen.Toner)
#   })
#   
# }
#     
#     
# ui <- shiny::htmlTemplate(
#   # Index Page
#   "www/index.html",
#   
#   # Name input
#   name_selector = selectInput("name_input", "MP Name", mp_list, selected = NULL),
#   
#   # Leaflet map
#   leaflet_map = leafletOutput(outputId = "mymap")
# )
# 
# server <- function(input, output, session) {
#   coord_mp <- reactive(
#     core_de %>%
#       filter(name == input$name_input) %>%
#       separate(birthplace, into = c("lat", "long"), sep = ",") %>%
#       mutate(
#         across(c("lat", "long"), ~ as.numeric(.x)),
#         popup_image = case_when(
#           !is.na(image_url) & !is.na(party_logo_url) ~ paste0(
#             "<img src = ",
#             image_url,
#             " width='50'>",
#             " </br>",
#             " </br>",
#             " <img src = ",
#             party_logo_url,
#             " width='50'>"
#           ),!is.na(image_url) ~ paste0("<img src = ",
#                                        party_logo_url,
#                                        " width='50'>"),
#           TRUE ~ "No image available."
#         )
#       )
#   )
#   
#   output$mymap <- renderLeaflet({
#     # Plot on leaflet map
#     leaflet() %>%
#       addProviderTiles("CartoDB.Positron", options = providerTileOptions(opacity = 0.99)) %>%
#       addPolygons(
#         data = wahlkreise_ll,
#         stroke = TRUE,
#         weight = 1,
#         color = "#968C83",
#         fillColor = '#968C83',
#         fillOpacity = 0.5,
#         smoothFactor = 0.5,
#         popup = ~ WKR_NAME,
#         highlightOptions = highlightOptions(
#           color = '#636363',
#           fillColor = '#636363',
#           opacity = 1,
#           weight = 2,
#           fillOpacity = 0.5,
#           bringToFront = TRUE,
#           sendToBack = TRUE
#         )
#       )
#   })
#   
#   
#   # output$mymap <- renderLeaflet({
#   #   leaflet(coord_mp(),
#   #           options = leafletOptions(
#   #             zoomControl = FALSE,
#   #             minZoom = 6,
#   #             maxZoom = 6
#   #           )) %>%
#   #     # setView(-96, 37.8, 4) %>%
#   #     addTiles() %>%
#   #     addMarkers(lat = ~ lat,
#   #                lng = ~ long,
#   #                popup = ~ popup_image)
#   # })
# }
# 
# shinyApp(ui, server)


party_logo_path <- here::here("test_leaflet_shiny/party_logos")

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
  filter(session == 19) %>%
  filter(constituency != "Landesliste") %>%
  left_join(btw17_kerg, keep = TRUE, by = c("constituency" = "WKR_NAME"))

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

mp_list <- core_de %>%
  slice(1:100) %>%
  distinct(name) %>%
  pull
#
# ui <- fluidPage(
#   leafletOutput("mymap"),
#   p(),
#   selectInput("name_input", "MP Name", mp_list, selected = NULL)
# )

ui <- shiny::htmlTemplate(
  # Index Page
  "www/index.html",
  
  # Name input
  name_selector = selectInput("name_input", "MP Name", mp_list, selected = NULL),
  
  # Leaflet map
  leaflet_map = leafletOutput(outputId = "mymap")
)

server <- function(input, output, session) {
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
  
  # Select right SpatialPolygon data for leaflet based on leg. session
  wahlkreis_ll <- reactive(
    case_when(
      input$session_input == "BTW2021 | LP20" ~ btw21_wahlkreise_spdf,
      input$session_input == "BTW2017 | LP19" ~ btw17_wahlkreise_spdf,
      input$session_input == "BTW2013 | LP18" ~ btw13_wahlkreise_spdf,
      input$session_input == "BTW2009 | LP17" ~ btw09_wahlkreise_spdf,
      input$session_input == "BTW2005 | LP16" ~ btw05_wahlkreise_spdf,
      input$session_input == "BTW2002 | LP15" ~ btw02_wahlkreise_spdf
    )
  )
  
  output$mymap <- renderLeaflet({
    # Plot on leaflet map
    leaflet() %>%
      addProviderTiles("CartoDB.Positron", options = providerTileOptions(opacity = 0.99)) %>%
      addPolygons(
        data = wahlkreis_ll,
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