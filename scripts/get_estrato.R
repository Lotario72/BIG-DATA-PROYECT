source("./data_prep.R")

require(pacman) 
p_load(tidyverse,rio,skimr,viridis,
       sf, ## leer/escribir/manipular datos espaciales
       nngeo, ## st_nn function
       spdep, ## Construct neighbours list from polygon list 
       osmdata) ## Get OSM's data

# cargar datos desde data_prep
houses_bog <- dplyr::filter(bog_med, city == "Bogotá D.C")
houses_med <- dplyr::filter(bog_med, city == "Medellín")

  
## dataframe to sf
houses_bog <- st_as_sf(x = houses_bog, coords=c("lon","lat"), crs=4326)
houses_med <- st_as_sf(x = houses_med, coords=c("lon","lat"), crs=4326)
houses_cal <- st_as_sf(x = houses_cal, coords=c("lon","lat"), crs=4326)

# oftener poligono 
sf_use_s2(FALSE)

get_estrato <- function(city){
  if(city == "bog"){
    houses <- houses_bog
    df <- getbb(place_name = "Bogota", featuretype = "", format_out = "sf_polygon") %>% .$multipolygon
    mnz <- st_read("C:/Users/ADMIN/Documents/3. Uniandes/10. BDML/PS3 DANE/MGN2017_11_BOGOTA/11_BOGOTA/URBANO/MGN_URB_MANZANA.shp") %>% select(MANZ_CCNCT) %>% .[df,]
    mgn <- import("C:/Users/ADMIN/Documents/3. Uniandes/10. BDML/PS3 DANE/11Bogota/CNPV2018_MGN_A2_11.CSV")
    viv <- import("C:/Users/ADMIN/Documents/3. Uniandes/10. BDML/PS3 DANE/11Bogota/CNPV2018_1VIV_A2_11.CSV")
  }

  if(city == "med"){
    houses <- houses_med
    df <- getbb(place_name = "Medellín", featuretype = "", format_out = "sf_polygon")
    mnz <- st_read("C:/Users/ADMIN/Documents/3. Uniandes/10. BDML/PS3 DANE/MGN2017_05_ANTIOQUIA/05_ANTIOQUIA/URBANO/MGN_URB_MANZANA.shp") %>% select(MANZ_CCNCT) %>% .[df,]
    mgn <- import("C:/Users/ADMIN/Documents/3. Uniandes/10. BDML/PS3 DANE/05Antioquia/CNPV2018_MGN_A2_05.CSV") %>% filter(U_MPIO == 1)
    viv <- import("C:/Users/ADMIN/Documents/3. Uniandes/10. BDML/PS3 DANE/05Antioquia/CNPV2018_1VIV_A2_05.CSV") %>% filter(U_MPIO == 1)
  }
  
  if(city == "cal"){
    houses <- houses_cal
    df <- getbb(place_name = "Cali", featuretype = "", format_out = "sf_polygon")
    mnz <- st_read("C:/Users/ADMIN/Documents/3. Uniandes/10. BDML/PS3 DANE/MGN2017_76_VALLE_DEL_CAUCA/76_VALLE_DEL_CAUCA/URBANO/MGN_URB_MANZANA.shp") %>% select(MANZ_CCNCT) %>% .[df,]
    mgn <- import("C:/Users/ADMIN/Documents/3. Uniandes/10. BDML/PS3 DANE/76_ValleDelCauca/76ValleDelCauca/76_ValleDelCauca_CSV/CNPV2018_MGN_A2_76.CSV") %>% filter(U_MPIO == 1)
    viv <- import("C:/Users/ADMIN/Documents/3. Uniandes/10. BDML/PS3 DANE/76_ValleDelCauca/76ValleDelCauca/76_ValleDelCauca_CSV/CNPV2018_1VIV_A2_76.CSV") %>% filter(U_MPIO == 1)
  }
  
  ### Unir datos imputados con manzanas DANE
  houses <- st_join(x=houses , y=mnz)
  
  
  ### Crear data estratos
  ## data manzanas
  mgn <- mgn %>% select(COD_DANE_ANM,UA_CLASE,COD_ENCUESTAS,U_VIVIENDA)
  
    ## data vivienda
  viv <- viv %>% select(COD_ENCUESTAS,UA_CLASE,U_VIVIENDA,V_TOT_HOG,VA1_ESTRATO)
  
  ## joing mnz-hogar-vivienda
  viv_mgn <- left_join(viv, mgn, by=c("UA_CLASE","COD_ENCUESTAS","U_VIVIENDA"))
  
  ##=== collapse data ===##
  db <- viv_mgn %>%
    group_by(COD_DANE_ANM) %>% 
    summarise(med_VA1_ESTRATO=median(VA1_ESTRATO,na.rm=T))
  
  
  ### UNir estratos hogar por manzana
  
  houses <- left_join(houses,db,by=c("MANZ_CCNCT"="COD_DANE_ANM"))
  
  estrato <- houses %>% select(property_id, med_VA1_ESTRATO)
  estrato <- st_drop_geometry(estrato)
  path_exp <- paste0("C:/Users/ADMIN/Documents/GitHub/Spatial-house-pricing-model/stores/estrato_", city, ".rds")
  export(estrato, path_exp)
}


get_estrato("bog")
get_estrato("med")
get_estrato("cal")