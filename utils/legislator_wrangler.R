# legislatoR tables ---------------------------------------------------------
# Political
deu_political <- get_political(legislature = "deu") %>% distinct()

# Core
deu_core <- get_core(legislature = "deu") %>% distinct()

# Portraits
deu_portrait <- get_portrait(legislature = "deu") %>% distinct()


# Intermediate Tables for joining -------------------------------------------

# Term length by MP and highest session MP has served in
term_length_de <- deu_political %>% 
  left_join(deu_core) %>% 
  group_by(pageid, name) %>% 
  summarise(
    min_session_start = min(session_start),
    max_session_end = max(session_end),
    min_session = min(session),
    max_session = max(session)
  ) %>% 
  ungroup %>% 
  mutate(
    term_length = time_length(difftime(max_session_end, min_session_start), "years") %>% round(3)
  ) %>% 
  select(-name)

# Dominant party (CURRENTLY NOT IN USE)
# dominant_party_de <- deu_political %>%
#   group_by(pageid, party) %>%
#   dplyr::summarise(n = n()) %>% 
#   group_by(pageid) %>%
#   dplyr::filter(n == max(n)) %>%
#   left_join(
#     deu_political %>%
#       group_by(pageid) %>%
#       filter(session_start == min(session_start)) %>%
#       arrange(session_start) %>%
#       distinct(pageid, .keep_all = T)
#   ) %>%
#   group_by(pageid) %>%
#   dplyr::filter(session_start == min(session_start))

# Wikipedia traffic
deu_traffic <- get_traffic("deu")


# Join legislatoR deu_political with wahlkreis_data
deu_political_with_wkr_data <- deu_political %>% 
  # Some manual correction of legislatoR data
  # LALE AKGÜN was not in Köln III during session 15 but Köln II
  mutate(
    constituency = ifelse(pageid == "237781" & session == 15, "Köln II", constituency),
  ) %>% 
  mutate(constituency_join = str_replace_all(constituency, char_to_replace_for_join, "") %>% str_to_lower()) %>% 
  left_join(kerg_full, by = c("constituency_join" = "WKR_NAME_join",
                              "session" = "session"))


# COMBINE TABLES TO MAIN TABLE: core_de ------------------------------------
core_de <- deu_core %>%
  left_join(deu_portrait) %>%
  # left_join(dominant_party_de) %>%
  left_join(term_length_de) %>%
  left_join(deu_political_with_wkr_data, by = "pageid") %>%
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
    ),
    party_color = case_when(
      party == "SPD" ~ party_color_map$SPD,
      party == "CDU" ~ party_color_map$CDU,
      party == "FDP" ~ party_color_map$FDP,
      party == "CSU" ~ party_color_map$CSU,
      party == "BÜNDNIS 90/DIE GRÜNEN" ~ party_color_map$`BÜNDNIS 90/DIE GRÜNEN`,
      party == "DIE LINKE" ~ party_color_map$`DIE LINKE`,
      party == "PDS" ~ party_color_map$PDS,
      party == "AfD" ~ party_color_map$AfD,
      party == "DP" ~ party_color_map$DP,
      party == "none" ~ party_color_map$none
    ),
    border_weight = 1
  ) %>% 
  # Create new column "sessions_served" per pageid
  left_join(aggregate(session~pageid, deu_political, paste0, collapse=", ") %>% dplyr::rename(sessions_served = session)) %>%
  mutate(
    age = case_when(
      # Compute age at death when death is higher than birth (logical check)
      death > birth ~ time_length(difftime(death, birth), "years") %>% ceiling(),
      # If MP is alive compute difference between birth date and current date
      is.na(death) ~ time_length(difftime(Sys.Date(), birth), "years") %>% ceiling(),
      TRUE ~ NA_real_
    )
  ) %>% 
  # Mutate name column to indicate whether MP was elected via Landesliste or direct
  mutate(name = if_else(constituency == "Landesliste", paste0(name, " (Landesliste)"), name), name)
  
  
  