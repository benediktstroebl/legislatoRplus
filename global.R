# Dependencies ------------------------------------------------------------
library(shiny)
library(tidyverse)
library(leaflet)
library(legislatoR)
library(rgdal)
library(sp)

#  Load Scripts ----------------------------------------------------------

# Load SpatialPolygon data for map
source("shapefile_loader.R")

# Load Wahlkreis mapping for each session
source("wahlkreis_data_loader.R")
