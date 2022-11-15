rm(list = ls())
# Librerías
library("tidyverse")
library("skimr")
library("stargazer")
library("vtable")
# importar base de datos definitva por ciudad
data_bog<- readRDS("../stores/lum_dist_vars_imputed_bog.Rds")
data_med<- readRDS("../stores/lum_dist_vars_imputed_med.Rds")
data_cal<- readRDS("../stores/lum_dist_vars_imputed_cal.Rds")

# Histograma log(precio)
histo_precio_bog <- ggplot(data_bog, aes(x = log(price))) +
  geom_histogram(bins = 50,color = "grey30", fill = "blue") +
  ggtitle("Precio vivienda Bogotá 2022-2022") +
  labs(x = "Log(precio vivienda)", y = "Cantidad") +
  theme_bw()
ggsave("../views/Log_precio_bog.png", histo_precio_bog)

histo_precio_med <- ggplot(data_med, aes(x = log(price))) +
  geom_histogram(bins = 50,color = "grey30", fill = "blue") +
  ggtitle("Precio vivienda Medellín 2022-2022") +
  labs(x = "Log(precio vivienda)", y = "Cantidad") +
  theme_bw()
ggsave("../views/Log_precio_med.png", histo_precio_med)

# Histogrma área vivienda
#data_bog <- data_bog%>% mutate_at (c ('surface_total'), ~ ( scale (.)%>% as.vector ))
#data_med <- data_med%>% mutate_at (c ('surface_total'), ~ ( scale (.)%>% as.vector ))
#data_cal <- data_cal%>% mutate_at (c ('surface_total'), ~ ( scale (.)%>% as.vector ))

histo_area_bog <- ggplot(data_bog, aes(x = surface_total)) +
  xlim(0, 500) +  geom_histogram(bins = 100,color = "gray30", fill = "red") +
  ggtitle("Área vivienda Bogotá 2022-2022") + 
labs(x = "Metros cuadrados (M2)", y = "Cantidad") +
  theme_bw()
ggsave("../views/area_bog.png", histo_area_bog)

histo_area_med <- ggplot(data_med, aes(x = surface_total)) +
  xlim(0, 500) + geom_histogram(bins = 100,color = "gray30", fill = "red") +
  ggtitle("Área vivienda Medellín 2022-2022") +
  labs(x = "Metros cuadrados (M2)", y = "Cantidad") +
  theme_bw()
ggsave("../views/area_med.png", histo_area_med)

histo_area_cal <- ggplot(data_cal, aes(x = surface_total)) +
  xlim(0, 500) + geom_histogram(bins = 100,color = "grey30", fill = "red") +
  ggtitle("Área vivienda Cali 2022-2022") +
  labs(x = "Metros cuadrados (M2)", y = "Cantidad") +
  theme_bw()
ggsave("../views/area_cal.png", histo_area_cal)

# Tablas descriptivas
data_bog <- data_bog[,!names(data_bog) %in% c("property_id", "city")]
data_med <- data_med[,!names(data_med) %in% c("property_id", "city")]
data_cal <- data_cal[,!names(data_cal) %in% c("property_id", "city")]

get_descriptives <- function(city){
  if(city == "bog"){
    data <- data_bog
  }
  if(city == "med"){
    data <- data_med
  }
  if(city == "cal"){
    data <- data_cal
  }

  data <- data %>% rename(
      'area M2' = surface_total,
      "Numero de habitaciones" = bedrooms,
      "Numero de baños" = bathrooms,
      "Distancia gasolinería mas cercana" =  closest_fuel,
      "Distancia hospital mas cercano" = closest_hospital,
      "Distancia estacion policia mas cercana" = closest_police,
      "Distancia parque mas cercano" = closest_park,
      "Distancia rio mas cercano" = closest_river,
      "Distancia universidad mas cercana" = closest_university,
      "Distancia estacion transporte mas cercana" = closest_station,
      "Distancia supermercado mas cercano" = closest_supermarket,
      "Distancia C.Comercial mas cercano" = closest_mall,
      "Indice de iluminacion" = lum_val,
      "Tipo de  vivienda (1 si es casa)" = house,
      "Cuenta con sala o  comedor" = sala_com,
      "Servicios interiores adicional" = upgrade_in,
      "Servicios exteriores adicional" = upgrade_out,
      "Luz natural" = light,
      "Numero de gasolineras a 500 mts" = less_500m_fuel,
      "Numero de hospitales a 500 mts" = less_500m_hospital,
      "Numero de estaciones policia a 500 mts" = less_500m_police,
      "Numero de parques a 500 mts" = less_500m_park,
      "Numero de rios a 500 mts" = less_500m_river,
      "Numero de universidades a 500 mts" = less_500m_university,
      "Numero de estaciones transporte a 500 mts" = less_500m_station,
      "Numero de Supermercado a 500 mts" = less_500m_supermarket,
      "Numero de C.comercial a 500 mts" = less_500m_mall,
    )
  sumtable(data, out='latex',file= "../views/descriptive_", city, ".tex")
}

get_descriptives("bog")
get_descriptives("med")
get_descriptives("cal")