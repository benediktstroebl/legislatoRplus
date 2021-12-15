party_logo_path <- here::here("test_leaflet_shiny/party_logos")

# Political
deu_political <- get_political(legislature = "deu")

# Core
deu_core <- get_core(legislature = "deu")

# Portraits
deu_portraits <- get_portrait(legislature = "deu")

# If there is a MP with multiple party affiliations; 
dominant_party_de <- deu_political %>%
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

# Join legislatoR deu_political with wahlkreis_data
deu_political_final <- deu_political %>% 
  mutate(constituency_join = str_replace_all(constituency, char_to_replace_for_join, "") %>% str_to_lower()) %>% 
  left_join(kerg_full, by = c("constituency_join" = "WKR_NAME_join",
                              "session" = "session"))

# CORE DE
core_de <- deu_core %>%
  left_join(deu_portraits) %>%
  left_join(deu_political_final, by = "pageid") %>%
  filter(session >= 15) %>% 
  mutate(
    WKR_NR = as.integer(WKR_NR),
    session = as.integer(session)) %>% 
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
  )
  
  
  
  
  
  # btw17_wahlkreisnamen <- read_csv2(
  #   "https://www.bundeswahlleiter.de/dam/jcr/90ae9719-97bb-43f9-8e26-3e9ef0f18609/btw17_wahlkreisnamen.csv",
  #   locale = locale("de", encoding = "latin1")
  # ) %>%
  # slice(-c(1:4)) %>%
  # row_to_names(1) %>%
  # mutate(wkr_merge = str_replace_all(WKR_NAME, "-|–|[:blank:]", "") %>% str_to_lower())

