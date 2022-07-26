
# 0. Paquetes ####

# Para análisis
library(forecast)
library(MASS)
library(urca)
library(tseries)

# Para gráficas
library(tidyverse)
require(cowplot)
source("ggseries.R")

# 1. Datos ####
datos = readxl::read_xlsx("datos.xlsx")
datos = datos[1:(nrow(datos)-2),]
control = datos[(nrow(datos)-2):nrow(datos),]

n = nrow(datos)

# 2. Serie ####
serie=ts(datos$Inflacion)
años = datos$año
windows()

source("ggseries.R")

x11()
graf_ts(serie,2,años,"")

modelo1 <- auto.arima(serie)
summary(modelo1)

arima(serie, order = c(1,0,0), method = "CSS-ML")

# 4. TRANSFORMACION ####
windows()
bc1<- boxcox(serie~1)

lam1<-bc1$x[which.max(bc1$y)]  #para saber que tramsformacion hacerle segun box-cox
lam1
serie_t=log(serie)##Serie Transformada
windows()
grafico2 = ggplot(mapping = aes(x=años,
                                y=serie_t)) +
  geom_line() +
  geom_point() +
  xlab("años")+
  ylab("logarito de inflaciín interanual (%)")+
  scale_y_continuous(breaks = seq(-0.5,2.6,0.5),
                     limits = c(-0.5,2.4))+
  scale_x_continuous(breaks = seq(min(años),max(años)+2,3), 
                     limits = c(min(años),max(años)))+
  theme_bw()+
  labs(title = "")
grafico2

windows()
par(mfrow=c(1,3))
plot(serie_t, xlab="Anho- mes", ylab="Ln Zt", col="red")
acf(serie_t, lag=,ylim=c(-1,1), main="ACF")
acf(serie_t, lag=25,ylim=c(-1,1), type="partial", main="PACF")

modelo1 <- auto.arima(serie_t)
summary(modelo1)

##Prueba Estacionariedad
dfserie<-ur.df(serie_t,lags = 3) # el n?mero de rezagos no afecta los resultados

# 5. Diferenciación ####
dts<-diff(serie_t,differences =1)
#length(dts)/4
##Prueba Estacionariedad
dfserie<-ur.df(dts,lags = 3) # el n?mero de rezagos no afecta los resultados

summary(dfserie)##Dados los reultados, nos quedamos con d=1


adf.test(dts)

##Analizar ACF y PACF
windows()
par(mfrow=c(1,3))
plot(dts, xlab="Anho- dia", ylab="Ln (Zt)- Ln (Zt-1)", col="red")
acf(dts, lag=25,ylim=c(-1,1), main="ACF")
acf(dts, lag=25,ylim=c(-1,1), type="partial", main="PACF")


modeloSt <- auto.arima(serie_t)
summary(modeloSt)

modeloDTS <- auto.arima(dts)
summary(modeloDTS)

summary(arima(dts, order = c(1,0,1), method = "CSS-ML"))

x11()
tsdiag(modelo1)
