# Dependencies ------------------------------------------------------------
library(shiny)
library(shinycssloaders)
library(shinyjs)
library(tidyverse)
library(readr)
library(lubridate)
library(leaflet)
library(legislatoR)
library(rgdal)
library(sp)
library(plyr)
library(stringr)
library(janitor)
library(ggthemes)
library(ggdist)
library(partycoloR)


# Constants used throughout code  ----------------------------------------

  # regex statement to translate constituency name/WKR_NAME to join key
char_to_replace_for_join <- "-|–|[:blank:]|–|[.]"
  # Party colors
party_color_map <- list(
  CDU = "#000000",
  SPD = "#E3001B",
  FDP = "#FFEE00",
  AFD = "009EE0",
  `BÜNDNIS 90/DIE GRÜNEN` = "#64A12D",
  CSU = "#008AC5",
  PDS = "#BE3075",
  `DIE LINKE` = "#BE3075",
  DP = "#F80000"
)

# Load Scripts ------------------------------------------------------------

# Load SpatialPolygon data for map
source("shapefile_loader.R")

# Load Wahlkreis mapping for each session
source("wahlkreis_data_loader.R")

# Load legislatoR data setup
source("legislator_wrangler.R")

# Load radar plot configuration 
source("radar_plot_config.R")

# Load legislatoR theme
source("theme_lgl.R")


# Data Setup --------------------------------------------------------------

btw17_wahlkreisnamen <- read_csv2(
  "https://www.bundeswahlleiter.de/dam/jcr/90ae9719-97bb-43f9-8e26-3e9ef0f18609/btw17_wahlkreisnamen.csv",
  locale = locale("de", encoding = "latin1")
) %>% 
  slice(-c(1:4)) %>% 
  row_to_names(1) %>% 
  mutate(wkr_merge = str_replace_all(WKR_NAME, " ", ""))

# party_logo_path <- here::here("test_leaflet_shiny/party_logos")

session_list <- deu_political %>% 
  distinct(session) %>% 
  pull

party_list <- deu_political %>% 
  distinct(party) %>% 
  pull

mp_list <- core_de %>%
  distinct(name) %>%
  add_row(name = "", .before = 1) %>% 
  pull

# Highest session MP is part of
max_session_de <- deu_political %>% 
  select(pageid, session) %>% 
  group_by(pageid) %>% 
  summarise(max_session = max(session)) %>% 
  ungroup 

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

# Term length by MP
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
  ) 

pol_core_de <- deu_political %>% 
  left_join(deu_core) %>% 
  left_join(portrait_de) %>% 
  left_join(dominant_party_de) %>%
  left_join(term_length_de) %>%
  left_join(max_session_de) %>% 
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
    age_at_death = case_when(
      # Compute age at death when death is higher than birth (logical check)
      death > birth ~ time_length(difftime(death, birth), "years") %>% round(3),
      # If MP is alive compute difference between birth date and current date
      is.na(death) ~ time_length(difftime(Sys.Date(), birth), "years") %>% round(3),
      TRUE ~ NA_real_
    )
  ) %>% 
    drop_na(constituency) %>%
    mutate(constituency_merge = str_replace_all(constituency, " ", "")) %>% 
    left_join(btw17_wahlkreisnamen, keep = TRUE, by = c("constituency_merge" = "wkr_merge"))

