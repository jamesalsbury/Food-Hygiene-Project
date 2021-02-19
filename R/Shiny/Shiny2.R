
sp_postcodes <- list()

for (i in seq_along(postcodeAreas)){
  sp_postcodes[[postcodeAreas[i]]] = aggregate(sp_poly[[postcodeAreas[i]]], dissolve=T, FUN = mean)
  sp_postcodes[[postcodeAreas[i]]]@data$ID = i
  sp_postcodes[[postcodeAreas[i]]]@data$name = postcodeAreas[i]
  sp_postcodes[[postcodeAreas[i]]]@data$description = paste0(postcodeAreas[i], " postcode area")
}


for (i in seq_along(postcodeAreas)) {
  if (i == 1) {
    All_postcode_areas_merged <- sp_postcodes[[postcodeAreas[1]]]
  } else {
    All_postcode_areas_merged <- rbind(All_postcode_areas_merged, sp_postcodes[[i]])
  }
}

mytext <- paste0(
 All_postcode_areas_merged@data$description,"<br/>") %>%
  lapply(htmltools::HTML)
leaflet() %>%
  setView(lng = -0.75, lat = 53, zoom = 8) %>%
  addTiles() %>%
  addScaleBar() %>%
  addPolygons(data = All_postcode_areas_merged,
              fillColor = "red",
              weight = 2,
              opacity = 0.8,
              #label = mytext,
              color = "white",
              dashArray = "3",
              fillOpacity = 0.25)

saveRDS(All_postcode_areas_merged, "data/ShinyData/All_postcode_areas_merged.rds")


leaflet() %>%
  setView(lng = -0.75, lat = 53, zoom = 8) %>%
  addTiles() %>%
  addScaleBar() %>%
  addPolygons(data = merged_sp_summary$AL@polygons[[1]],
              fillColor = "red",
              weight = 2,
              opacity = 0.8,
              #label = mytext,
              color = "white",
              dashArray = "3",
              fillOpacity = 0.25)

merged_sp_summary$AL@polygons


