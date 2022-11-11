## Getting Polution and light behaviour for houses in database
## Nicolas Velasquez
rm(list = ls())
setwd(r'(C:\Users\juan.velasquez\OneDrive - Universidad de los Andes\Maestria\Semestres\2022-2\BIG DATA & MACHINE LEARNING FOR APPLIED ECONOMICS\Talleres\Problem-Set3\Spatial-model-of-house-prices)')
#### **Instalar/llamar las librerías de la clase**
require(pacman)
p_load(
  tidyverse, rio, skimr, viridis, osmdata,
  ggsn, ## scale bar
  raster, stars, ## datos raster
  ggmap, ## get_stamenmap
  sf, ## Leer/escribir/manipular datos espaciales
  leaflet
) ## Visualizaciones dinámicas

houses_bog <- readRDS("./stores/dist_vars_imputed_bog.Rds")
houses_cal <- readRDS("./stores/dist_vars_imputed_cal.Rds")
houses_med <- readRDS("./stores/dist_vars_imputed_med.Rds")
# skim(houses)
# glimpse(houses)
bog_geo <- st_as_sf(x = houses_bog, coords = c("lon", "lat"), crs = 4326)
med_geo <- st_as_sf(x = houses_med, coords = c("lon", "lat"), crs = 4326)
cal_geo <- st_as_sf(x = houses_cal, coords = c("lon", "lat"), crs = 4326)
## solucionar conflictos de funciones
select <- dplyr::select
scalebar <- ggsn::scalebar
# leaflet() %>% addTiles() %>% addCircleMarkers(data=cal_geo[4000:4010,])
## importar raster de luces: raster
luces_r <- raster("./stores/VNL_2021.tif")
## importar raster de luces: stars
luces_s <- read_stars("./stores/VNL_2021.tif")
## atributos
names(luces_s) <- "lum_val"
luces_s[[1]] %>%
  as.vector() %>%
  summary()
luces_s[[1]][is.na(luces_s[[1]]) == T] %>% head() # Reemplazar NA's
## puedo reproyectar un raster?
st_crs(luces_s)
luces_new_crs <- st_transform(luces_s, crs = 4326)
## plot data
plot(luces_s)
## download boundary
## get Bogota-UPZ
bog <- opq(bbox = getbb("Bogota Colombia")) %>%
  add_osm_feature(key = "boundary", value = "administrative") %>%
  osmdata_sf()
bog <- bog$osm_multipolygons %>% subset(admin_level == 9)
## get Medallo- Comunas
med <- opq(bbox = getbb("Medellín, Valle de Aburrá, Antioquia, Colombia")) %>%
  add_osm_feature(key = "boundary", value = "administrative") %>%
  osmdata_sf()
med <- med$osm_multipolygons %>% subset(admin_level == 8)
## get Cali- Barrios
cal <- opq(bbox = getbb("Cali, Sur, Valle del Cauca, Colombia")) %>%
  add_osm_feature(key = "boundary", value = "administrative") %>%
  osmdata_sf()
cal <- cal$osm_multipolygons %>% subset(admin_level == 9)
## load data
# l_bog_0 = read_stars("./stores/night_light_202002.tif") %>% st_crop(bog)
# names(l_bog_0) = "date_202002"
## cliping
l_bog <- st_crop(x = luces_s, y = bog) # crop luces de Colombia con polygono de bogota
l_med <- st_crop(x = luces_s, y = med) # crop luces de Colombia con polygono de bogota
l_cal <- st_crop(x = luces_s, y = cal) # crop luces de Colombia con polygono de bogota
# l_bog= c(l_bog_0,l_bog_1)
## BOGOTA
ggplot() +
  geom_stars(data = l_bog, aes(y = y, x = x, fill = lum_val)) + # plot raster
  scale_fill_viridis(option = "A", na.value = "white") +
  geom_sf(data = bog, fill = NA, col = "green") +
  theme_bw()

puntos_bog <- st_as_sf(x = l_bog, as_points = T, na.rm = T) # raster to sf (points)
poly_bog <- st_as_sf(x = l_bog, as_points = F, na.rm = T) # raster to sf (polygons)
## MEDALLO
ggplot() +
  geom_stars(data = l_med, aes(y = y, x = x, fill = lum_val)) + # plot raster
  scale_fill_viridis(option = "A", na.value = "white") +
  geom_sf(data = med, fill = NA, col = "green") +
  theme_bw()

puntos_med <- st_as_sf(x = l_med, as_points = T, na.rm = T) # raster to sf (points)
poly_med <- st_as_sf(x = l_med, as_points = F, na.rm = T) # raster to sf (polygons)
## CALI
ggplot() +
  geom_stars(data = l_cal, aes(y = y, x = x, fill = lum_val)) + # plot raster
  scale_fill_viridis(option = "A", na.value = "white") +
  geom_sf(data = cal, fill = NA, col = "green") +
  theme_bw()

puntos_cal <- st_as_sf(x = l_cal, as_points = T, na.rm = T) # raster to sf (points)
poly_cal <- st_as_sf(x = l_cal, as_points = F, na.rm = T) # raster to sf (polygons)
### **Imputación de valores de pixel a casas**
## Asignar luminosidad
inputed_bog <- st_join(x = bog_geo, y = poly_bog)
inputed_med <- st_join(x = med_geo, y = poly_med)
inputed_cal <- st_join(x = cal_geo, y = poly_cal)

leaflet() %>%
  addTiles() %>%
  addPolygons(data = poly_cal[inputed_cal[1:10, ], ]) %>%
  addCircles(data = inputed_cal[1:10, ], color = "red")

inputed_bog <- st_drop_geometry(inputed_bog)
inputed_med <- st_drop_geometry(inputed_med)
inputed_cal <- st_drop_geometry(inputed_cal)
saveRDS(inputed_bog, file = "./stores/lum_dist_vars_imputed_bog.Rds")
saveRDS(inputed_med, file = "./stores/lum_dist_vars_imputed_med.Rds")
saveRDS(inputed_cal, file = "./stores/lum_dist_vars_imputed_cal.Rds")


############################ **Gráficos**  #####################################

### plot Bogota-UPZ}
bog$ramdon <- runif(nrow(bog), 10, 20)
map <- ggplot() +
  geom_sf(data = bog, aes(fill = ramdon)) +
  scale_fill_viridis(option = "E", name = "Luminity")
## add Scale_bar
map <- map + north(data = bog, location = "topleft", symbol = 1) +
  scalebar(data = bog, dist = 5, transform = T, dist_unit = "km")
## add theme
map <- map + theme_linedraw()
## add osm layer
osm_layer_bog <- get_stamenmap(
  bbox = as.vector(st_bbox(bog)),
  maptype = "toner", source = "osm", zoom = 13
)
map2_bog <- ggmap(osm_layer_bog) +
  geom_sf(data = bog, aes(fill = ramdon), alpha = 0.3, inherit.aes = F) +
  scale_fill_viridis(option = "D", name = "Variable") +
  scalebar(data = bog, dist = 5, transform = T, dist_unit = "km") +
  north(data = bog, location = "topleft") + theme_linedraw() + labs(x = "", y = "")
map2_bog
### plot Medallo-Comunas
med$ramdon <- runif(nrow(med), 10, 20)
map_med <- ggplot() +
  geom_sf(data = med, aes(fill = ramdon)) +
  scale_fill_viridis(option = "E", name = "light activity")
## add Scale_bar
map_med <- map_med + north(data = med, location = "topleft", symbol = 1) +
  scalebar(data = med, dist = 5, transform = T, dist_unit = "km")
## add theme
map_med <- map_med + theme_linedraw()
## add osm layer
osm_layer_med <- get_stamenmap(
  bbox = as.vector(st_bbox(med)),
  maptype = "toner", source = "osm", zoom = 13
)
map2_med <- ggmap(osm_layer_med) +
  geom_sf(data = med, aes(fill = ramdon), alpha = 0.3, inherit.aes = F) +
  scale_fill_viridis(option = "D", name = "light activity") +
  scalebar(data = med, dist = 5, transform = T, dist_unit = "km") +
  north(data = med, location = "topleft") + theme_linedraw() + labs(x = "", y = "")
map2_med
### plot Cali-barrios
cal$ramdon <- runif(nrow(cal), 10, 20)
map_cal <- ggplot() +
  geom_sf(data = cal, aes(fill = ramdon)) +
  scale_fill_viridis(option = "E", name = "light activity")
## add Scale_bar
map_cal <- map_cal + north(data = cal, location = "topleft", symbol = 1) +
  scalebar(data = cal, dist = 5, transform = T, dist_unit = "km")
## add theme
map_cal <- map_cal + theme_linedraw()
## add osm layer
osm_layer_cal <- get_stamenmap(
  bbox = as.vector(st_bbox(cal)),
  maptype = "toner", source = "osm", zoom = 13
)
map2_cal <- ggmap(osm_layer_cal) +
  geom_sf(data = cal, aes(fill = ramdon), alpha = 0.3, inherit.aes = F) +
  scale_fill_viridis(option = "D", name = "light activity") +
  scalebar(data = cal, dist = 5, transform = T, dist_unit = "km") +
  north(data = cal, location = "topleft") + theme_linedraw() + labs(x = "", y = "")
map2_cal
