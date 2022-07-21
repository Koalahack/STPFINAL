require("sf")
require("spdep")
require("dplyr")
library(readr)
library(ggspatial)
library(Metrics)
require("tidyverse")
require("here")
require("spatstat")
library(ggplot2)
require(mapdata)
require(maps)
require(ggrepel)
library(tseries)
library(MASS)
library(forecast)
library(readxl)


db <- read_excel("Datos.xlsx", sheet = "DatosPromedioAnual")
SerieMensual <- read_excel("Datos.xlsx", sheet = "HistoricoMensual")
SerieTrimestral <- read_excel("Datos.xlsx", sheet = "HistoricoTrimestral")



#Cargue Poligonos
map<-st_read("gadm36_COL_shp/gadm36_COL_1.shp")

#Base Unida
db_aj<-left_join(map, db, by = c("NAME_1"))

#Mapa PBI
windows()
ggplot()+
  geom_sf(data=db_aj, aes(geometry = (geometry), fill = Espa?a)) +
  scale_fill_gradient2(low = "green", high = "red", mid = "yellow", midpoint = 40 , space = "Lab")+
  theme_bw() +
  theme(axis.title =element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size=6))+    
  labs(title = "Ingreso promedio remesas anual por regiones, provenientes de Espa?a",
       fill = "Valor remesa promedio (Millones USD)")+
  annotation_scale()+
  annotation_north_arrow(location='tr')


windows()
ggplot()+
  geom_sf(data=db_aj, aes(geometry = (geometry), fill = Chile)) +
  scale_fill_gradient2(low = "green", high = "red", mid = "yellow", midpoint = 15 , space = "Lab")+
  theme_bw() +
  theme(axis.title =element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size=6))+    
  labs(title = "Ingreso promedio remesas anual por regiones, provenientes de Chile",
       fill = "Valor remesa promedio (Millones USD)")+
  annotation_scale()+
  annotation_north_arrow(location='tr')



windows()
ggplot()+
  geom_sf(data=db_aj, aes(geometry = (geometry), fill = Estados_Unidos)) +
  scale_fill_gradient2(low = "green", high = "red", mid = "yellow", midpoint = 80 , space = "Lab")+
  theme_bw() +
  theme(axis.title =element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size=6))+    
  labs(title = "Ingreso promedio remesas anual por regiones, provenientes de Estados Unidos",
       fill = "Valor remesa promedio (Millones USD)")+
  annotation_scale()+
  annotation_north_arrow(location='tr')



windows()
ggplot()+
  geom_sf(data=db_aj, aes(geometry = (geometry), fill = Estados_Unidos)) +
  scale_fill_gradient2(low = "green", high = "red", mid = "yellow", midpoint = 80 , space = "Lab")+
  theme_bw() +
  theme(axis.title =element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size=6))+    
  labs(title = "Ingreso promedio remesas anual por regiones, provenientes de Estados Unidos",
       fill = "Valor remesa promedio (Millones USD)")+
  annotation_scale()+
  annotation_north_arrow(location='tr')


windows()
ggplot()+
  geom_sf(data=db_aj, aes(geometry = (geometry), fill = Reino_Unido)) +
  scale_fill_gradient2(low = "green", high = "red", mid = "yellow", midpoint = 5 , space = "Lab")+
  theme_bw() +
  theme(axis.title =element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size=6))+    
  labs(title = "Ingreso promedio remesas anual por regiones, provenientes de Reino Unido",
       fill = "Valor remesa promedio (Millones USD)")+
  annotation_scale()+
  annotation_north_arrow(location='tr')



windows()
ggplot()+
  geom_sf(data=db_aj, aes(geometry = (geometry), fill = Venezuela)) +
  scale_fill_gradient2(low = "green", high = "red", mid = "yellow", midpoint = 0.3 , space = "Lab")+
  theme_bw() +
  theme(axis.title =element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size=6))+    
  labs(title = "Ingreso promedio remesas anual por regiones, provenientes de Venezuela",
       fill = "Valor remesa promedio (Millones USD)")+
  annotation_scale()+
  annotation_north_arrow(location='tr')



windows()
ggplot()+
  geom_sf(data=db_aj, aes(geometry = (geometry), fill = Otros_Pa?ses)) +
  scale_fill_gradient2(low = "green", high = "red", mid = "yellow", midpoint = 60 , space = "Lab")+
  theme_bw() +
  theme(axis.title =element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size=6))+    
  labs(title = "Ingreso promedio remesas anual por regiones, provenientes de Otros Pa?ses",
       fill = "Valor remesa promedio (Millones USD)")+
  annotation_scale()+
  annotation_north_arrow(location='tr')



serie=ts(SerieMensual$`Valor en Millones de dolares`,frequency=12, start=c(2001,1))
windows()
plot(serie, xlab="A?o- mes", ylab="Millones de dolares (USD)", col="red")


serie=ts(SerieTrimestral$`Valor en Millones de dolares`,frequency= 4, start=c(2001,1))
windows()
plot(serie, xlab="A?o- mes", ylab="Millones de dolares (USD)", col="red")

#tranformacion de la varianza

windows()
bc1<- boxcox(serie~1)

lam1<-bc1$x[which.max(bc1$y)]  #para saber que tramsformacion hacerle segun box-cox
lam1
serie_t=log(serie)##Serie Transformada
windows()
plot(serie_t, xlab="Anho- mes", ylab="Ln Zt", col="red")


##Analizar ACF y PACF
windows()
par(mfrow=c(1,3))
plot(serie_t, xlab="Anho- mes", ylab="Ln Zt", col="red")
acf(serie_t, lag=25,ylim=c(-1,1), main="ACF")
acf(serie_t, lag=25,ylim=c(-1,1), type="partial", main="PACF")



###Prueba Estacionariedad
######dikey fuller
library(urca)

dfserie<-ur.df(serie_t,lags = 3) # el n?mero de rezagos no afecta los resultados

summary(dfserie)




###Diferenciacion

dts<-diff(serie_t,differences =1)
#length(dts)/4
##Prueba Estacionariedad
dfserie<-ur.df(dts,lags = 3) # el n?mero de rezagos no afecta los resultados

summary(dfserie)##Dados los reultados, nos quedamos con d=1





##Analizar ACF y PACF
windows()
par(mfrow=c(1,3))
plot(dts, xlab="Anho- dia", ylab="Ln (Zt)- Ln (Zt-1)", col="red")
acf(dts, lag=25,ylim=c(-1,1), main="ACF")
acf(dts, lag=25,ylim=c(-1,1), type="partial", main="PACF")




modelo1 <- auto.arima(serie_t)
summary(modelo1)




# algunos otros diagn?sticos
# chequeo si los residuales son ruido blanco
x11()
tsdiag(modelo1)



# prueba de normalidad
shapiro.test(modelo1$residuals)                #prueba de Shapiro-Wilks
# library(fBasics)
fBasics::jarqueberaTest(modelo1$residuals)             # librer?a fBasics 
fBasics::normalTest(modelo1$residuals, method=("jb"))  # librer?a fBasics
###Homogeneidad de varianza

white.test(modelo1$residuals, lag=2)


windows()
plot(forecast(modelo1,20,conf= c(95)), main = ""
     , ylab = "N?mero de clientes retirados", xlab= "A?o/Trimestre")
pre=forecast(modelo1,20,conf= c(95))
pre_final=exp(pre$mean)
