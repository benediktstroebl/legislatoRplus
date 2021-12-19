# Dependencies ------------------------------------------------------------
library(shiny)
library(raster)
library(shinycssloaders)
library(shinyjs)
library(tidyverse)
library(readr)
library(lubridate)
library(leaflet)
library(legislatoR)
library(rgdal)
library(sp)
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
  AfD = "#009EE0",
  `BÜNDNIS 90/DIE GRÜNEN` = "#64A12D",
  `BÃœNDNIS 90/DIE GRÃœNEN` = "#64A12D",
  CSU = "#008AC5",
  PDS = "#BE3075",
  `DIE LINKE` = "#BE3075",
  DP = "#F80000",
  none = "#636363"
)

# Register fonts with R, for Windows users
if (.Platform$OS.type == "windows") {
  windowsFonts(Corbel=windowsFont("Corbel"))
}

# Party logo path
#party_logo_path <- here::here("test_leaflet_shiny/party_logos")

# Load Scripts ------------------------------------------------------------

# Load SpatialPolygon data for map
source("shapefile_loader.R")

# Load Wahlkreis mapping for each session
source("wahlkreis_data_loader.R")

# Load legislatoR data setup
source("legislator_wrangler.R")

# join shapefiles with legislatoR & constituency data and create popup html
source("shapefile_wrangler.R")

# Load radar plot configuration 
source("radar_plot_config.R")

# Load legislatoR theme
source("theme_lgl.R")


# Data Lists to populate dropdown selectors -------------------------------

session_list <- deu_political %>%
  distinct(session) %>%
  filter(session >= 15) %>%
  arrange(session) %>% 
  pull

names(session_list) <- c(
              "BTW 2002 | Session 15",
              "BTW 2005 | Session 16",
              "BTW 2009 | Session 17",
              "BTW 2013 | Session 18",
              "BTW 2017 | Session 19"
              #"BTW2021 | LP20"  # Session 20 not yet in legislatoR data
)

party_list <- deu_political %>% 
  filter(session >= 15) %>%
  distinct(party) %>% 
  pull

mp_list <- core_de %>%
  filter(session >= 15) %>%
  distinct(name) %>%
  add_row(name = "", .before = 1) %>% 
  pull





