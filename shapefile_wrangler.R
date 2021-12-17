joinShapefileWithDeuCore <-
  function(spdf_dataframe, session_of_spdf) {
    new_AT_data <-
      spdf_dataframe@data %>%
      mutate(session = session_of_spdf,
             WKR_NR = as.integer(WKR_NR)) %>%
      mutate(constituency_join = str_replace_all(WKR_NAME, char_to_replace_for_join, "") %>% str_to_lower()) %>%
      left_join(core_de,
                by = c("constituency_join" = "constituency_join",
                       "WKR_NR" = "WKR_NR",
                       "session" = "session")) %>%
      distinct() %>% 
      mutate(
        popup_image = case_when(
          !is.na(image_url) & !is.na(party_logo_url) ~ paste0(
            "<style>
                @font-face {
                font-family: 'Corbel';
                src: url('/Corbel.eot');
                src: url('/Corbel.eot?#iefix') format('embedded-opentype'),
                    url('/Corbel.woff2') format('woff2'),
                    url('/Corbel.woff') format('woff'),
                    url('/Corbel.ttf') format('truetype'),
                    url('/Corbel.svg#Corbel') format('svg');
                font-weight: normal;
                font-style: normal;
                font-display: swap;
                }
                
                @font-face {
                font-family: 'Old London';
                src: url('/OldLondon.woff2') format('woff2'),
                    url('/OldLondon.woff') format('woff');
                font-weight: normal;
                font-style: normal;
                font-display: swap;
                }
            
                table {
                    font-family: Corbel;
                    font-size: 12px;
                    width: 200px;
                    margin-left:auto; 
                    margin-right:auto;
                    margin-top: 30px;
                }
                
                tr td {
                  padding: 0;
                  margin-top: 10px;
                  margin-bottom: 10px;
                  height: auto;
                }
                
                .popup-content{
                    width:auto !important;
                    height:auto !important;
                }
            
                a {
                color: black;
                text-decoration: none;
                }
            
                a:hover {
                    text-decoration: underline;
                }
            
                p {
                    color: black;
                }
                
                span {
                    color: black;
                    font-weight: bold
                }
            
            </style>",
            "<div class='popup-content'>",
            "<table>",
                "<tr>
                    <td style='text-align:center' colspan='2'>",
                      "<img src = ",
                      image_url,
                      " width='140px'>",
                    "</td>",
                "</tr>",
                "<tr>
                    <td style='text-align:center' colspan='2'>",
                      "<img src = ",
                      party_logo_url,
                      " width='70px'>",
                "</td>",
                "</tr>",
                "<tr>
                    <td style='text-align:center'>",
                    "<span>",
                      "Name",
                    "</span>",
                    "</td>",
                    "<td style='text-align:center'>",
                      name,
                    "</td>",
                "</tr>",
            "<tr>
                    <td style='text-align:center'>",
            "<span>",
            "Age",
            "</span>",
            "</td>",
            "<td style='text-align:center'>",
            age,
            "</td>",
            "</tr>",
            "<tr>
              <td style='text-align:center'>",
            "<span>",
            "Constituency",
            "</span>",
            "</td>",
            "<td style='text-align:center'>",
            WKR_NAME.x,
            "</td>",
            "</tr>",
            "<tr>
              <td style='text-align:center'>",
            "<span>",
            "Sessions Served",
            "</span>",
            "</td>",
            "<td style='text-align:center'>",
            sessions_served,
            "</td>",
            "</tr>",
            "</table>",
            "</div>"
          ),
          !is.na(image_url) ~ paste0("<img src = ",
                                     party_logo_url,
                                     " width='50'>"),
          TRUE ~ "No image available."
        )
      )
    return(new_AT_data)
  }


# btw02
btw02_wahlkreise_spdf@data <- joinShapefileWithDeuCore(btw02_wahlkreise_spdf, 15)
# btw05
btw05_wahlkreise_spdf@data <- joinShapefileWithDeuCore(btw05_wahlkreise_spdf, 16)
# btw09
btw09_wahlkreise_spdf@data <- joinShapefileWithDeuCore(btw09_wahlkreise_spdf, 17)
# btw13
btw13_wahlkreise_spdf@data <- joinShapefileWithDeuCore(btw13_wahlkreise_spdf, 18)
# btw17
btw17_wahlkreise_spdf@data <- joinShapefileWithDeuCore(btw17_wahlkreise_spdf, 19)
# btw21
btw21_wahlkreise_spdf@data <- joinShapefileWithDeuCore(btw21_wahlkreise_spdf, 20)








# # Find constituencies that do not match in legislatoR and Bundeswahlleiter kerg data
# anti_join(
#   deu_political %>%  mutate(
#     WKR_NAME_join = str_replace_all(constituency, char_to_replace_for_join, "") %>% str_to_lower()
#   ) %>% filter(session >= 15),
#   kerg_full,
#   by = c("session", "WKR_NAME_join")
# ) %>% filter(constituency != "Landesliste")
# 
# # Find WKR_NAME mismatches between kerg files and shapefiles
# anti_join(btw02_wahlkreise_spdf@data  %>% mutate(WKR_NR = as.integer(WKR_NR)), btw02_kerg, by = c("WKR_NR", "session"))
# anti_join(btw05_wahlkreise_spdf@data %>% mutate(WKR_NR = as.integer(WKR_NR)), btw05_kerg)
# anti_join(btw09_wahlkreise_spdf@data %>% mutate(WKR_NR = as.integer(WKR_NR)), btw09_kerg)
# anti_join(btw13_wahlkreise_spdf@data %>% mutate(WKR_NR = as.integer(WKR_NR)), btw13_kerg)
# anti_join(btw17_wahlkreise_spdf@data %>% mutate(WKR_NR = as.integer(WKR_NR)),btw17_kerg)
# anti_join(btw21_wahlkreise_spdf@data %>% mutate(WKR_NR = as.integer(WKR_NR)),btw21_kerg)

# Find mismatches between spdf and legislatoR
# btw02_wahlkreise_spdf@data %>%
#   mutate(
#     session = 15,
#     WKR_NR = as.integer(WKR_NR)
#   ) %>%
#   mutate(constituency_join = str_replace_all(WKR_NAME, char_to_replace_for_join, "") %>% str_to_lower()) %>%
#   anti_join(core_de,
#             by = c("constituency_join" = "constituency_join",
#                    "session" = "session"))
# 
# 
# 
# # btw17
# btw17_wahlkreise_spdf@data <-
#   btw17_wahlkreise_spdf@data %>%
#   mutate(
#     session = 19,
#     WKR_NR = as.integer(WKR_NR)
#   ) %>%
#   mutate(constituency_join = str_replace_all(WKR_NAME, char_to_replace_for_join, "") %>% str_to_lower()) %>%
#   left_join(
#     core_de,
#     by = c("constituency_join" = "constituency_join",
#            "session" = "session")) %>%
#   mutate(popup_image = case_when(
#     !is.na(image_url) & !is.na(party_logo_url) ~ paste0(
#       "<img src = ",
#       image_url,
#       " width='50'>",
#       " </br>",
#       " </br>",
#       " <img src = ",
#       party_logo_url,
#       " width='50'>"
#     ),
#     !is.na(image_url) ~ paste0("<img src = ",
#                                party_logo_url,
#                                " width='50'>"),
#     TRUE ~ "No image available."
#   ))

