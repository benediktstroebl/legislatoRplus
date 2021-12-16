# Import shapefiles for Wahlkreise per session

# btw21
btw21_wahlkreise_spdf <-
  readOGR(dsn = "data/btw21/shapefiles/Geometrie_Wahlkreise_20DBT_geo.shp",
          encoding = "UTF-8",
          use_iconv = TRUE) %>%
  # Transform spatial polygons data frame to longlat data
  spTransform(CRS("+init=epsg:4326"))
btw21_wahlkreise_spdf@data <- btw21_wahlkreise_spdf@data %>% 
  mutate(session = 20,
         WKR_NR = as.integer(WKR_NR))


# btw17
btw17_wahlkreise_spdf <-
  readOGR(dsn = "data/btw17/shapefiles/Geometrie_Wahlkreise_19DBT_geo.shp",
          encoding = "UTF-8",
          use_iconv = TRUE) %>%
  # Transform spatial polygons data frame to longlat data
  spTransform(CRS("+init=epsg:4326"))
btw17_wahlkreise_spdf@data <- btw17_wahlkreise_spdf@data %>% 
  mutate(session = 19, WKR_NR = as.integer(WKR_NR))


# btw13
btw13_wahlkreise_spdf <-
  readOGR(dsn = "data/btw13/shapefiles/Geometrie_Wahlkreise_18DBT.shp",
          encoding = "UTF-8",
          use_iconv = TRUE) %>%
  # Transform spatial polygons data frame to longlat data
  spTransform(CRS("+init=epsg:4326"))
btw13_wahlkreise_spdf@data <- btw13_wahlkreise_spdf@data %>% 
  mutate(session = 18, WKR_NR = as.integer(WKR_NR))



# btw09
btw09_wahlkreise_spdf <-
  readOGR(dsn = "data/btw09/shapefiles/Geometrie_Wahlkreise_17DBT.shp",
          encoding = "UTF-8",
          use_iconv = TRUE) %>%
  # Transform spatial polygons data frame to longlat data
  spTransform(CRS("+init=epsg:4326"))
btw09_wahlkreise_spdf@data <- btw09_wahlkreise_spdf@data %>% 
  mutate(session = 17, WKR_NR = as.integer(WKR_NR))



# btw05
btw05_wahlkreise_spdf <-
  readOGR(dsn = "data/btw05/shapefiles/Geometrie_Wahlkreise_16DBT_geo.shp",
          encoding = "UTF-8",
          use_iconv = TRUE) %>%
  # Transform spatial polygons data frame to longlat data
  spTransform(CRS("+init=epsg:4326"))
btw05_wahlkreise_spdf@data <- btw05_wahlkreise_spdf@data %>% 
  mutate(session = 16,WKR_NR = as.integer(WKR_NR))



# btw02
btw02_wahlkreise_spdf <-
  readOGR(dsn = "data/btw02/shapefiles/Geometrie_Wahlkreise_15DBT_geo.shp",
          encoding = "UTF-8",
          use_iconv = TRUE) %>%
  # Transform spatial polygons data frame to longlat data
  spTransform(CRS("+init=epsg:4326"))
btw02_wahlkreise_spdf@data <- btw02_wahlkreise_spdf@data %>% 
  mutate(session = 15, WKR_NR = as.integer(WKR_NR))




# 
# 
# 
# Plot on leaflet map
# leaflet() %>%
#   addProviderTiles("CartoDB.Positron", options = providerTileOptions(opacity = 0.99)) %>%
#   addPolygons(
#     data = btw17_wahlkreise_spdf,
#     stroke = TRUE,
#     weight = 1,
#     color = "#968C83",
#     fillColor = '#968C83',
#     fillOpacity = 0.5,
#     smoothFactor = 0.5,
#     popup = btw17_wahlkreise_spdf@data$WKR_NAME.x,
#     highlightOptions = highlightOptions(
#       color = '#636363',
#       fillColor = '#636363',
#       opacity = 1,
#       weight = 2,
#       fillOpacity = 0.5,
#       bringToFront = TRUE,
#       sendToBack = TRUE
#     )
#   )
