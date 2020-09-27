# FINAL_EXAMEN
#' @author Nombre completo aqu?: Miguel Angel Tenorio Enciso
#' @codigo Codigo aqu?: 18160049
#' @fecha 26/09/2020
#............................................................................
library(tidyverse)
library(sf)
library(sp)
library(raster)
library(rgee)
library(mapview)
library(mapedit)
library(ggplot2)
library(lubridate)
# Pregunta 1
getwd()
str(data_h)
head(data_h)
heigh <- sf::st_as_sf(data_h, coords=c("X", "Y"), crs=4326)
heigh
dem1 <- raster("C:/Users/user/Documents/Datos examen/ASTGTM_S13W075_dem.tif")
dem2 <- raster("C:/Users/user/Documents/Datos examen/ASTGTM_S13W076_dem.tif")
dem <- merge(dem1, dem2)


heigh_huancavelica <- heigh %>%
  filter(DEPARTAMEN=="HUANCAVELICA")

mapview(list(dem, heigh_huancavelica))
heigh_huancavelica$elev <- extract(dem, heigh_huancavelica)

presupuesto <- data_h %>%
  filter(elev >= 4000) %>%
  mutate(presup = POBLACION*750)

sum(presupuesto_huancavelica$presup)



# Pregunta 2
tabla <- read.csv("C:/Users/Miguel Tenorio E/Desktop/2020/5toCiclo/Programación/FINAL/table (46).csv")
View(tabla)
head(tabla)
ggplot(data = tabla, aes(x = HORA, y = TEMPERATURA)) + 
  geom_point(color = 'yellow', size = 2, alpha = 0.6) +
  geom_smooth(color = 'blue') + 
  xlab('Hora') + 
  ylab('T°C') +
  ggtitle('Relación de la temperatura VS tiempo en la provincia de HUAROCHIRI') + 
  theme_dark()

# Pregunta 3
getwd()
file.choose()
setwd("C:/Users/Miguel Tenorio E/Desktop/2020/5toCiclo/Programación/FINAL")
data_h <- read.csv("C:\\Users\\Miguel Tenorio E\\Desktop\\2020\\5toCiclo\\Programación\\FINAL\\centros_poblados.csv")
View(data_h)
##Filatramos Lima
lima <- data_h %>% 
  filter(UBIGEO == 150303)
plot(lima)
ggplot(data = lima, aes() + 
  geom_bar()+
  theme_classic()
  
head(lima)

Tmin<-min(lima)
Tmax<-max(lima)
Tmed <- mean(lima)
etp_Hargreaves <- function(R_a){
  0.0023*R_a*(Tmed + 17.8)*(Tmax - Tmin)^1/2
}
etp_Hargreaves(50)
# Pregunta 4
#1------ Libreria mapview : Con esta librería podemos trabajar de forma 
#interactiva con objetos espaciles especializados en la parte superior de una
#región requerida.
#------ libreria mapedit : Edite de forma interactiva tus datos espaciles en R.

#2----- CSR con datos espaciales: Esto hace que sea mas facil de publicar mapa
#archivos html y como tambien mas dinámico.

#3-----Los requerimientos de sistema radica en las necesidades del usuario,
#y el origen de la gestión de requerimientos de un proyecto de software
#se origina en los requerimientos y/o especificaciones del sistema.

# Extra
Rmarkdown><
