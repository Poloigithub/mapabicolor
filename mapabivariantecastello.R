# instalamos los paquetes si hace falta
if(!require("tidyverse")) install.packages("tidyverse")
if(!require("patchwork")) install.packages("patchwork")
if(!require("sf")) install.packages("sf")
if(!require("raster")) install.packages("raster")
if(!require("biscale")) install.packages("biscale")
if(!require("sysfonts")) install.packages("sysfonts")
if(!require("showtext")) install.packages("showtext")

# paquetes
library(tidyverse)
library(sf)
library(readxl)
library(biscale)
library(patchwork)
library(raster)
library(sysfonts)
library(showtext)
library(raster)

# raster de CORINE LAND COVER 2018
urb <- raster("mapa/U2018_CLC2018_V2020_20u1.tif")

# datos de renta y Gini
renta <- read_excel("mapa/30824.xlsx")
gini <- read_excel("mapa/37677.xlsx")

# límites censales del INE
limits <- read_sf("mapa/SECC_CE_20200101.shp") 

# filtramos la Comunidad Autónoma de Madrid
limits <- filter(limits, CUMUN == "12040")

#eliminar columbretes
limits <- limits[-72,]



# obtenemos los límites municipales
mun_limit <- group_by(limits, CUMUN) %>% summarise()

# proyectamos los límites 
limits_prj <- st_transform(limits, projection(urb))

# acortamos y enmascaramos 
urb_mad <- crop(urb, limits_prj) %>% 
  mask(limits_prj)

# eliminamos píxeles no urbanos 
urb_mad[!urb_mad %in% 1:2] <- NA 

# plot del raster
plot(urb_mad)

# proyectamos 
urb_mad <- projectRaster(urb_mad, crs = CRS("+proj=longlat +datum=WGS84 +no_defs"))

# transformamos el raster a xyz y objeto sf 
urb_mad <- as.data.frame(urb_mad, xy = TRUE, na.rm = TRUE) %>%
  st_as_sf(coords = c("x", "y"), crs = 4326)

# añadimos las columnas de las coordinadas
urb_mad <- urb_mad %>% rename(urb = 1) %>% cbind(st_coordinates(urb_mad))

## datos renta y gini INE

renta_sec <- mutate(renta, NATCODE = str_extract(CUSEC, "[0-9]{5,10}"), 
                    nc_len = str_length(NATCODE),
                    mun_name = str_remove(CUSEC, NATCODE) %>% str_trim()) %>%
  filter(nc_len > 5)

gini_sec <- mutate(gini, NATCODE = str_extract(CUSEC, "[0-9]{5,10}"), 
                   nc_len = str_length(NATCODE),
                   mun_name = str_remove(CUSEC, NATCODE) %>% str_trim()) %>%
  filter(nc_len > 5)

# unimos ambas tablas de renta y Gini con los límites censales
mad <- left_join(limits, renta_sec, by = c("CUSEC"="NATCODE")) %>% 
  left_join(gini_sec, by = c("CUSEC"="NATCODE"))

# convertimos columnas en numérico 
mad <- mutate_at(mad, c(23:27, 30:31), as.numeric)

## creamos clasificación bivariante
mapbivar <- bi_class(mad, GINI_2017, RNMP_2017, style = "quantile", dim = 3) %>% 
  mutate(bi_class = ifelse(str_detect(bi_class, "NA"), NA, bi_class))

# resultado
head(dplyr::select(mapbivar, GINI_2017, RNMP_2017, bi_class))

## redistribuimos los píxeles urbanos a la desigualdad
mapdasi <- st_join(urb_mad, st_transform(mapbivar, 4326))

# leyenda bivariante
legend2 <- bi_legend(pal = "DkBlue",
                     dim = 3,
                     xlab = "Más desigual",
                     ylab = "Más renta",
                     size = 8)


#descarga de fuente
font_add_google("Montserrat", "Montserrat")
showtext_auto()


ggplot(mapbivar) + 
  geom_sf(aes(fill = bi_class), 
          colour = NA, 
          size = .1, 
          show.legend = FALSE) +
  geom_sf(data = mun_limit,  
          color = "white", 
          fill = NA, 
          size = 0.2) +
  annotation_custom(ggplotGrob(legend2), 
                    xmin = 0.02, xmax = 0.05,
                    ymin = 50, ymax = 30.1)  +
  bi_scale_fill(pal = "DkBlue", 
                dim = 3, 
                na.value = "grey90") +
  labs(title = "Castelló de la Plana",  x = "", y ="") +
  bi_theme() +
  theme(plot.title = element_text(family = "Montserrat", size = 20, face = "bold")) +
  coord_sf(crs = 4326)
