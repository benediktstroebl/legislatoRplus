# Import shape file for Wahlkreise
wahlkreise_spdf <- readOGR(dsn = "data/shape_files/Geometrie_Wahlkreise_19DBT_geo.shp",
                           encoding = "UTF-8",
                           use_iconv = TRUE)

# Transform spatial polygons data frame to longlat data
wahlkreise_ll <- spTransform(wahlkreise_spdf, CRS("+init=epsg:4326"))


# # Plot on leaflet map
# leaflet() %>%
#   addProviderTiles("CartoDB.Positron", options = providerTileOptions(opacity = 0.99)) %>%
#   addPolygons(
#     data = wahlkreise_ll,
#     stroke = TRUE,
#     weight = 1,
#     color = "#968C83",
#     fillColor = '#968C83',
#     fillOpacity = 0.5,
#     smoothFactor = 0.5,
#     popup = wahlkreise_ll$WKR_NAME,
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
