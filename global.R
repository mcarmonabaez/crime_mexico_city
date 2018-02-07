

# cargar paquetes ---------------------------------------------------------

cargar_paquetes <- function(paquetes_extra = NULL){
  paquetes <- c("tidyverse", "stringr", "lubridate", paquetes_extra)
  if (length(setdiff(paquetes, rownames(installed.packages()))) > 0) {
    install.packages(setdiff(paquetes, rownames(installed.packages())))
  }
  lapply(paquetes, require, character.only = TRUE)
  return(search())
}

cargar_paquetes(c("shiny", "shinydashboard", "plotly", "DCluster",
                  "jsonlite", "RCurl", "rgdal", "rgeos", "ggmap",
                  "scales", "geojsonio", "downloader", "spdep",
                  "viridis", "maptools", "rvest", "stringi", "leaflet"))

# 
# library("shiny")
# library("shinydashboard"); library("plotly"); library("DCluster")
# library("jsonlite"); library("RCurl"); library("rgdal")
# library("rgeos"); library("ggmap"); library("scales")
# library("geojsonio"); library("downloader"); library("spdep")
# library("viridis"); library("maptools"); library("rvest");
# library("stringi"); library("leaflet")
# library("tidyverse"); library("stringr"); library("lubridate")





# datos -------------------------------------------------------------------


cat_dias <- data.frame(day = c("Tuesday", "Wednesday", "Thursday",  "Friday",
                               "Saturday", "Sunday", "Monday" ),
                       dia_semana = c("Martes", "Miércoles", "Jueves",
                                      "Viernes", "Sábado", "Domingo", "Lunes"))

temp <- tempfile()
download.file("http://data.diegovalle.net/hoyodecrimen/cuadrantes.csv.zip",temp)
crimenes <- read_csv(unz(temp,"clean-data/crime-lat-long.csv")) %>%
  mutate(hora = as.numeric(str_sub(string = as.character(hour), 1, 2)),
         day = weekdays(date)) %>%
  left_join(cat_dias, by = "day")

cuadrantes <- read_csv(unz(temp,"clean-data/cuadrantes-hoyodecrimen.csv")) 
unlink(temp)


# datos para el mapa ------------------------------------------------------


tmp_cuadrantes <-  tempfile("cuads", fileext = ".json")
download.file("https://hoyodecrimen.com/api/v1/cuadrantes/geojson", tmp_cuadrantes)

cuadrantes_json <- rgdal::readOGR(tmp_cuadrantes, "OGRGeoJSON", verbose = FALSE)

tmp_sectores <-  tempfile("secs", fileext = ".json")
download.file("https://hoyodecrimen.com/api/v1/sectores/geojson", tmp_sectores)
sectores_json <-  rgdal::readOGR(tmp_sectores, "OGRGeoJSON", verbose = FALSE)

crime_sectors <- fromJSON("https://hoyodecrimen.com/api/v1/sectores/all/crimes/all/period")$rows
#fortify the data for ggplot2
fsectors <- fortify(sectores_json, region = "sector")
sector_mapa <- left_join(fsectors, crime_sectors, by = c("id" = "sector"))


crime_cuadrantes <- fromJSON("https://hoyodecrimen.com/api/v1/cuadrantes/all/crimes/all/period")$rows
fcuadrantes <- fortify(cuadrantes_json, region = "cuadrante")
cuadrante_mapa <- left_join(fcuadrantes, crime_cuadrantes, by = c("id" = "cuadrante"))


long_media <-  mean(cuadrante_mapa$long)
lat_media <- mean(cuadrante_mapa$lat)


# auxiliares --------------------------------------------------------------

coordenadas_labels <- sector_mapa %>%
  dplyr::select(id, long, lat, population) %>%
  unique %>% 
  group_by(long, lat) %>%
  mutate(n = n()) %>%
  ungroup() %>% 
  arrange(id, desc(n)) %>%
  group_by(id, population) %>%
  summarise(lat = first(lat),
            long = first(long))

cuenta_sector <- cuadrantes %>%
  group_by(sector, crime) %>%
  summarise(total = sum(count)) %>%
  ungroup()

cuenta_sector_year <- cuadrantes %>%
  group_by(year, sector, crime) %>%
  summarise(total = sum(count)) %>%
  ungroup()

cuenta_mes <- cuadrantes %>%
  group_by(year, date,  crime) %>%
  summarise(total = sum(count)) %>%
  ungroup()

year <- unique(cuenta_sector_year$year)


res_cuadrantes <- cuadrantes %>%
  group_by(year, date, municipio, crime) %>%
  summarise(count = sum(count, na.rm = T) ) %>% 
  ungroup() 

str(res_cuadrantes)

res_cuadrantes_tot <- cuadrantes %>%
  group_by(year, date) %>%
  summarise(count = sum(count, na.rm = T) ) %>% 
  ungroup()   

  
tipo_crimen <- sort(unique(crimenes$crime))
muns <- sort(unique(res_cuadrantes$municipio))

