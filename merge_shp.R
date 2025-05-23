library(sf)
library(dplyr)


shp_files <- list.files("Shapefiles", pattern = "\\.shp$", full.names = TRUE)

for (i in seq_along(shp_files)) {
  filename <- tools::file_path_sans_ext(basename(shp_files[i]))
  
  # Read the shapefile
  layer <- st_read(shp_files[i])
  layer <- st_transform(layer, crs = 4326)
  
  assign(filename, layer)
}




KRP18_25$LCAP_CLASS<-as.numeric(KRP18_25$LCAP_CLASS)


LAR18_25 <- LAR18_25 %>%
  mutate(across(c(SLOPE, WIND_EROS, SALINITY, SODICITY, MICRELIEF), as.character))

LARRI_25 <- LARRI_25 %>%
  mutate(across(c(SLOPE, WIND_EROS, SALINITY,SODICITY,MICRELIEF), as.character))



NANGU_25 <- NANGU_25 %>%
  mutate(across(c(SLOPE, WIND_EROS, SALINITY,SODICITY,MICRELIEF), as.character))

ORACK_25 <-ORACK_25 %>%
  mutate(across(c(SLOPE, WIND_EROS), as.character))

ROPLU_100<-ORACK_25 %>%
  mutate(across(c(SLOPE), as.character))

TTREE_25<-TTREE_25 %>%
  mutate(across(c(SLOPE), as.character))

WEA18_25 <- WEA18_25 %>%
  mutate(across(LCAP_CLASS, as.numeric))

# Combine them into a single spatial object
combined_layers <- bind_rows(ALICU_25_GLC, `BESWI_25 GLC`, `DUNMA_50 GLC`,FRE15_50,KARLS_50,KRP18_25,LAR18_25,LARRI_25, NANGU_25,ORACK_25,ROPLU_100,TTREE_25,WEA18_25,WILDM_25)


# Save to a new shapefile
st_write(combined_layers, "Summary14.shp")
