source("./data_prep.R")

library("tidyverse")
library("sf")
library("osmdata")
library("spdep")
library("dplyr")

houses_bog <- dplyr::filter(bog_med, city == "Bogotá D.C")
houses_med <- dplyr::filter(bog_med, city == "Medellín")

bog_geo <- st_as_sf(x = houses_bog, coords = c("lon", "lat"), crs = 4326)
med_geo <- st_as_sf(x = houses_med, coords = c("lon", "lat"), crs = 4326)
cal_geo <- st_as_sf(x = cal, coords = c("lon", "lat"), crs = 4326)

rm(list = c("bog_med", "cal", "houses_bog", "houses_med", "houses_preproc"))

# Get a spcecific set of polygons corresponding to the geographic feature of interest
get_feature <- function(place, key, value) {
    feature <- opq(bbox = getbb(place)) %>%
        add_osm_feature(key = key, value = value) %>%
        osmdata_sf() %>%
        .$osm_polygons %>%
        select(osm_id)

    feature
}

# Load matrix of pairs of selected key-value pairs
keyvals <- read.csv("../stores/st_maps_key_val.csv")

# Get and store polygons for the previously defined set of geographic features - Bogota
for (i in 1:nrow(keyvals)) {
    feature <- get_feature("Bogota Colombia", keyvals$key[i], keyvals$value[i])
    dist_feature <- st_distance(x = bog_geo, y = feature)
    path <- paste0("../stores/", "dist_bog_", keyvals$value[i], ".Rds")
    saveRDS(dist_feature, path)
}

# Get and store polygons for the previously defined set of geographic features - Medellin
for (i in 1:nrow(keyvals)) {
    feature <- get_feature("Medellin Colombia", keyvals$key[i], keyvals$value[i])
    dist_feature <- st_distance(x = med_geo, y = feature)
    path <- paste0("../stores/", "dist_med_", keyvals$value[i], ".Rds")
    saveRDS(dist_feature, path)
}

# Get and store polygons for the previously defined set of geographic features - Cali
for (i in 1:nrow(keyvals)) {
    feature <- get_feature("Cali Colombia", keyvals$key[i], keyvals$value[i])
    dist_feature <- st_distance(x = cal_geo, y = feature)
    path <- paste0("../stores/", "dist_cal_", keyvals$value[i], ".Rds")
    saveRDS(dist_feature, path)
}
