
# 0. Paquetes ####

library(forecast)
library(MASS)
library(urca)
library(tseries)

# 1. Datos ####
datos = readxl::read_xlsx("datos.xlsx")


# 2. Serie ####
serie=ts(datos$Inflacion)
años = datos$año
windows()
plot(y=serie,x=años, xlab="Añoo- mes", ylab="Millones de dolares (USD)", col="red")


windows()
par(mfrow=c(1,3))
plot(serie, xlab="Anho- mes", ylab="Ln Zt", col="red")
acf(serie, lag=25,ylim=c(-1,1), main="ACF")
acf(serie, lag=25,ylim=c(-1,1), type="partial", main="PACF")

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
plot(serie_t, xlab="Anho- mes", ylab="Ln Zt", col="red")

windows()
par(mfrow=c(1,3))
plot(serie_t, xlab="Anho- mes", ylab="Ln Zt", col="red")
acf(serie_t, lag=25,ylim=c(-1,1), main="ACF")
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


modelo1 <- auto.arima(serie_t)
summary(modelo1)

modelo1 <- auto.arima(dts)
summary(modelo1)

summary(arima(dts, order = c(1,0,1), method = "CSS-ML"))

x11()
tsdiag(modelo1)



