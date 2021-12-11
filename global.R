# Dependencies ------------------------------------------------------------
library(shiny)
library(tidyverse)
library(leaflet)
library(legislatoR)
library(rgdal)
library(sp)
library(janitor)

#  Load Scripts ----------------------------------------------------------

# Load SpatialPolygon data for map
source("shapefile_loader.R")

# btw17_wahlkreisnamen <- read_csv2("data/wahlkreisdaten/btw17_wahlkreisnamen.csv",
#                                   locale = locale(encoding = "latin1"))

btw17_wahlkreisnamen <- read_csv2(
  "https://www.bundeswahlleiter.de/dam/jcr/90ae9719-97bb-43f9-8e26-3e9ef0f18609/btw17_wahlkreisnamen.csv",
  locale = locale("de", encoding = "latin1")
) %>% 
  slice(-c(1:4)) %>% 
  row_to_names(1) %>% 
  mutate(wkr_merge = str_replace_all(WKR_NAME, " ", ""))


