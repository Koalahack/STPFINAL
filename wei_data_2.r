
library(tseries)
library(forecast)

# ejemplos de identificacion usando datos de Wei(2006)
# ejemplo 6.1
# Daily average number of truck manufacturing defects
#45 observaciones diarias de dias laborales consecutivos entre
#noviembre 4 y enero 10.
# ----------------------------------------------------------------------------
# entrada de datos directa
(seriew1=ts(c(1.2,1.5,1.54, 2.7, 1.95, 2.4, 3.44, 2.83, 1.76, 2.0, 2.09,
1.89,  1.8,   1.25,  1.58,  2.25,  2.5,   2.05,  1.46,  1.54,    1.42,    1.57,
1.4,   1.51,  1.08,  1.27,  1.18,  1.39,  1.42,  2.08,  1.85,    1.82,    2.07,
2.32,  1.23,  2.91,  1.77,  1.61,  1.25,  1.15,  1.37,  1.79,    1.68 ,   1.78,
1.84)))

# El numero de observaciones es pequeno. No es prudente usar la transformacion de Box-Cox.
# grafica de la serie y correlogramas muestrales
x11()
plot.ts(seriew1, type="o")
dev.off()

x11()
par(mfrow=c(2,1))
acf(seriew1, lag.max=10)
pacf(seriew1, lag.max=10)
dev.off()

# Conclusion: la serie parece ser generada por un proceso AR(1).
#(1- phi B)(Zt - mu)=at




#AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA

# estimación del modelo usando la función arima de stats
# ----------------------------------------------------------------------------
# estimación ML condicional 
(mod_w1_CSS=arima(seriew1, c(1, 0, 0), method = c("CSS"))) 
(res_w1_CSS=residuals(mod_w1_CSS))
# estimación ML exacta
(mod_w1_ML=arima(seriew1, c(1, 0, 0), init=c(.5, .3), method = c("ML"))) 
(res_w1_ML=residuals(mod_w1_ML))
# estimación ML exacta con valores iniciales dados por la estimación condicional
(mod_w1_CSS_ML=arima(seriew1, c(1, 0, 0), method = c("CSS-ML"))) 
(res_w1_CSS_ML=residuals(mod_w1_CSS_ML))

# diagnósticos sobre el modelo estimado con versimilitud exacta: mod_w1_CSS_ML 
# ----------------------------------------------------------------------------
# raíces de los polinomios
# library(UnitCircle)              # contiene una función para obtener las raíces de los polinomios
x11()
UnitCircle::uc.check(c(1, -coef(mod_w1_CSS_ML)[1]))
dev.off()

# algunos otros diagnósticos
# chequeo si los residuales son ruido blanco y posibles observaciones extremas
x11()
tsdiag(mod_w1_CSS_ML)
dev.off()
###Homogeneidad de varianza

white.test(res_w1_ML, lag=2)


# Otra forma de verificar si los residuales son ruido blanco
x11()
par(mfrow=c(2,1)) 
acf(res_w1_CSS_ML, lag.max=12)
pacf(res_w1_CSS_ML, lag.max=12)
dev.off()

# prueba de Ljung-Box
(L_B_prueba=Box.test(res_w1_CSS_ML, lag = 12, type = "Ljung-Box"))  # hay que corregir los grados
                                              # de libertad como 12-# de parámetros
                                              # Arma del modelo estimado
# cálculo del valor crítico para la prueba de Ljung-Box
qchisq(.95, df=11)
# cálculo del valor p para la prueba de Ljung-Box
pchisq(L_B_prueba$statistic, df=11, lower.tail = F)

# chequeo de observaciones atípicas extremas (no es un análisis completo de outliers)
res_est_w1=res_w1_CSS_ML/(mod_w1_CSS_ML$sigma2^.5)  # estandarización de los residuales
plot.ts(res_est_w1)
x11()
plot.ts(rnorm(45))

# chequeo de normalidad
# gráfico cuantil-cuantil
x11()
qqnorm(res_est_w1,  xlab = "Cuantiles Teóreticos", ylab = "Cuantiles Muestrales")
qqline(res_est_w1)
 
# histograma, densidad kernel y gráfico normal
x11()
mu<-mean(res_est_w1)
sigm<-sd(res_est_w1)
x<-seq(-4,4,length=100)
y<-dnorm(x,mu,sigm)
hist(res_est_w1,prob=T,ylim=c(0,.7),xlim=c(-5,5),col="yellow")
lines(density(res_est_w1))
lines(x,y,lwd=2,col="blue")
# conclusión: Se detecta alejamiento de la normalidad

# prueba de normalidad
shapiro.test(res_est_w1)                #prueba de Shapiro-Wilks
# library(fBasics)
fBasics::jarqueberaTest(res_est_w1)             # librería fBasics 
fBasics::normalTest(res_est_w1, method=("jb"))  # librería fBasics
# conclusión: Se rechaza la normalidad.
##





# valores ajustados
(ajust=seriew1-residuals(mod_w1_CSS_ML))

x11();plot.ts(seriew1);lines(ajust,col=2)

# gráfico para los valores ajustados
x11()
ts.plot(seriew1,ajust)   # gráfico de las series contra el tiempo
lines(seriew1, col="black")
lines(ajust, col="red")
plot(as.vector(seriew1),as.vector(ajust), type="p")   # gráfico de las series contra el tiempo

# detección de observaciones atípicas
x11();plot.ts(res_est_w1)
ind=(abs(res_est_w1)>2.5)
(grupo=cbind(res_est_w1, ind))
# las observaciones 7 y 36 parecen ser atípicas

# generación de las variables indicadoras
ind7=rep(0, times=45)
ind7[7]=1

ind36=rep(0, times=45)
ind36[36]=1

atip=as.matrix(cbind(ind7, ind36))

# estimacion del modelo con observaciones atípicas
# especificación y estimación del modelo
(mod_w1_atip=arima(seriew1, xreg=atip, order = c(1, 0, 0),method = c("CSS-ML")))

(ajust_atip=seriew1-residuals(mod_w1_atip))

x11();plot.ts(seriew1);lines(ajust_atip,col=2)
lines(ajust,col=4)


# library(astsa)
m1 <- astsa::sarima(xdata = seriew1, xreg=atip, p = 1, d = 0, q = 0)
m2 <- astsa::sarima(xdata = seriew1, p = 1, d = 0, q = 0)

ajust.1 <- seriew1 - m1$fit$residuals
ajust.2 <- seriew1 - m2$fit$residuals

x11();plot.ts(seriew1);lines(ajust.1,col=2);lines(ajust.2,col=4)

p.1 <- astsa::sarima.for(xdata = seriew1, n.ahead = 1, newxreg = cbind(0,0), xreg=atip, p = 1, d = 0, q = 0)
p.2 <- astsa::sarima.for(xdata = seriew1, n.ahead = 5, p = 1, d = 0, q = 0)

x11();plot.ts(seriew1, xlim=c(1,50));lines(ajust.1,col=2);lines(ajust.2,col=4)
points(x=46,y=p.1$pred,col=2); points(x=46,y=p.2$pred,col=4)

# diagnósticos
# chequeo si los residuales son ruido blano y posibles observaciones extremas
x11();tsdiag(mod_w1_atip)

# chequeo de normalidad
# gráfico cuantil-cuantil
# estandarización de los residuales
res_est_w1=residuals(mod_w1_atip)/(mod_w1_atip$sigma2^.5)
x11()
qqnorm(res_est_w1,  xlab = "Cuantiles Teóricos", ylab = "Cuantiles Muestrales")
qqline(res_est_w1)
 
# histograma, densidad kernel y gráfico normal
x11()
mu<-mean(res_est_w1)
sigm<-sd(res_est_w1)
x<-seq(-4,4,length=100)
y<-dnorm(x,mu,sigm)
hist(res_est_w1,prob=T,ylim=c(0,.7),xlim=c(-5,5),col="yellow")
lines(density(res_est_w1))
lines(x,y,lwd=2,col="blue")
# conclusión: No se detecta alejamiento severo de la normalidad

# prueba de normalidad
shapiro.test(res_est_w1)                #prueba de Shapiro-Wilk
jarqueberaTest(res_est_w1)              # prueba de Jarque-Bera, librería fBasics
normalTest(res_est_w1, method=("jb"))
# conclusión: Hay normalidad

#AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA









# ejemplo 6.2
# Wolf yearly sunspot numbers from 1700 to 2001
# ----------------------------------------------------------------------------
# entrada de datos usando un archivo de texto.
(seriew2=ts(scan("DatosWei/W2_WolfYearly SunspotNumbers.txt")))

# grafica de la serie
plot.ts(seriew2, type="o")

# transformacion de Box-Cox.
# como la serie tiene valores cero, se debe volver estrictamente positiva sumando una constante
seriew2_pos=seriew2+0.01
library(TSA)
(TSA::BoxCox.ar(seriew2_pos))

# la transformacion obtenida es lambda=.5
# se transforma la serie usando la raiz cuadrada 
tseriew2=seriew2^.5
par(mfrow=c(2,1))
plot.ts(seriew2)
plot.ts(tseriew2)

# grafica de la serie y correlogramas muestrales
detach("package:TSA")
par(mfrow=c(3,1))
plot.ts(seriew2, type="o")
acf(tseriew2, lag.max=20, ci=0)
pacf(tseriew2, lag.max=20)

# conclusion: Los resultados sugieren un AR(2) o un AR(9).
# el proceso parece ser ciclico (ACF sinusoidal) con un periodo ciclico de 11 anos.



#AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA

# estimación del modelo usando la función arima de stats
# estimación ML condicional 
(mod_w2_CSS=arima(tseriew2, c(9, 0, 0), method = c("CSS"))) 
(res_w2_CSS=residuals(mod_w2_CSS))
# estimación ML exacta
(mod_w2_ML=arima(tseriew2, c(9, 0, 0), method = c("ML"))) 
(res_w2_ML=residuals(mod_w2_ML))
# estimación ML exacta con valores iniciales dados por la estimación condicional
(mod_w2_CSS_ML=arima(tseriew2, c(9, 0, 0), method = c("CSS-ML"))) 
(res_w2_CSS_ML=residuals(mod_w2_CSS_ML))

#AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA











# ejemplo 6.3 
# Series W3   Blowfly data
# ----------------------------------------------------------------------------
(seriew3=ts(c(1676, 3075, 3815, 4639, 4424, 2784, 5860, 5781, 4897, 3920, 3835, 3618,
3050, 3772, 3517, 3350, 3018, 2625, 2412, 2221, 2619, 3203, 2706, 2717,
2175, 1628, 2388, 3677, 3156, 4272, 3771, 4955, 5584, 3891, 3501, 4436,
4369, 3394, 3869, 2922, 1843, 2837, 4690, 5119, 5838, 5389, 4993, 4446,
4651, 4243, 4620, 4849, 3664, 3016, 2881, 3821, 4300, 4168, 5448, 5477,
8579, 7533, 6884, 4127, 5546, 6316, 6650, 6304, 4842, 4352, 3215, 2652,
2330, 3123, 3955, 4494, 4780, 5753, 5555, 5712, 4786, 4066)))

# grafica de la serie
plot.ts(seriew3, type="o")

# transformacion de Box-Cox estabilizadora de varianza
plot.ts(diff(seriew3), type="o")
library(TSA)
BoxCox.ar(seriew3)
# parece que la transformacion esta afectada por la existencia de datos atipicos.
# el resto de datos parecen provenir de un proceso de varianza estable.
# se decide no transformar.

# grafica de la serie y correlogramas muestrales
detach("package:TSA")
par(mfrow=c(2,1))
acf(seriew3, lag.max=15, ci=0)
pacf(seriew3, lag.max=15)
# conclusion: un modelo AR(1) parece ser adecuado



#AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA

# estimación del modelo usando la función arima de stats
# estimación ML condicional 
(mod_w3_CSS=arima(seriew3, c(1, 0, 0), method = c("CSS"))) 
(res_w3_CSS=residuals(mod_w3_CSS))
# estimación ML exacta
(mod_w3_ML=arima(seriew3, c(1, 0, 0), method = c("ML"))) 
(res_w3_ML=residuals(mod_w3_ML))
# estimación ML exacta con valores iniciales dados por la estimación condicional
(mod_w3_CSS_ML=arima(seriew3, c(1, 0, 0), method = c("CSS-ML"))) 
(res_w3_CSS_ML=residuals(mod_w3_CSS_ML))

#AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA















# ejemplo 6.4
# Series W4   Monthly unemployed young women between
# ages 16 and 19 in the U.S. from January 1961 to August 2002
# ----------------------------------------------------------------------------
# entrada de datos usando un archvo de texto.
(seriew4=ts(scan("DatosWei/W4_USMonthlySeriesUnemployedYoungWomen.txt")))

# grafica de la serie
plot.ts(seriew4, type="l")

# transformacion de Box-Cox.
library(TSA)
(BoxCox.ar(seriew4))

# lambda=.8
tseriew4=seriew4^.8
par(mfrow=c(2,1))
plot.ts(seriew4)
plot.ts(tseriew4)
# parece que no es necesario transformar

# grafica de la serie y correlogramas muestrales
detach("package:TSA")
par(mfrow=c(2,1))
acf(seriew4, lag.max=15, ci=0)
pacf(seriew4, lag.max=15, ci=0)
# conclusion parece que hay raiz unitaria

# analisis para la serie diferenciada una vez
plot.ts(diff(seriew4), type="l")
# conclusion parece que la serie diferenciada es estacionaria
# correlogramas muestrales
par(mfrow=c(2,1))
acf(diff(seriew4), lag.max=15)
pacf(diff(seriew4), lag.max=15, ci=0)
#alternativamente,
par(mfrow=c(2,1))
acf(diff(seriew4), lag.max=15, ci.type = "ma")
pacf(diff(seriew4), lag.max=15, ci=0)
# Conclusion: parece que la serie diferenciada sigue un MA(1). Sin embargo,
# la serie parece contener depedencia entre anos, es decir, una componente estacional,
# lo que es frecuente en serie mensuales, trimestrales, etc.    

#Para probar si el parametro de tendencia deterministica Theta0 es necesario,
#se calcula el t-ratio

wbar <- mean(diff(seriew4))
Swbar <- sqrt(var(diff(seriew4)) / length(diff(seriew4)))

t_ratio <- wbar/Swbar
t_ratio

#cuantil 0.025 superior con 498 g.l.
qt(p=0.025, df=, length(diff(seriew4))-1, lower.tail = F)

#lo cual indica que Theta0 no es significante.




#AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA

# estimación del modelo usando la función arima de stats
# estimación ML condicional con deriva
(mod_w4_CSS=arima(diff(seriew4), c(0, 0, 1), method = c("CSS"))) 
(res_w4_CSS=residuals(mod_w4_CSS))
# estimación ML condicional sin deriva
(mod_w4_CSS=arima(seriew4, c(0, 1, 1), method = c("CSS"))) 
(res_w4_CSS=residuals(mod_w4_CSS))
# estimación ML exacta sin deriva
(mod_w4_ML=arima(seriew4, c(0, 1, 1), method = c("ML"))) 
(res_w4_ML=residuals(mod_w4_ML))
# estimación ML exacta con valores iniciales dados por la estimación condicional sin deiva
(mod_w4_CSS_ML=arima(seriew4, c(0, 1, 1), method = c("CSS-ML"))) 
(res_w4_CSS_ML=residuals(mod_w4_CSS_ML))

#AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA












# ejemplo 6.5
# Series W5   Yearly cancer death rate (per 100,000) for Pennsylvania between
# 1930 and 2000
# ----------------------------------------------------------------------------
(seriew5=ts(c(100.4, 105.1, 109.4, 110.5, 116.9, 118.2, 122.9, 126.5, 125.8, 130.7, 131.7,
131.7, 134.8, 136.1, 137.8, 142.8, 144.5, 144.4, 150.5, 152.2, 155.2, 157.2,
157.4, 161.9, 165,   164,   168.2, 170.8, 169.9, 169.7, 173.3, 173.8, 175.8,
177.8, 179.9, 180,   182,   184.2, 187.7, 187.4, 186.5, 188.3, 190.6, 190.8,
196.4, 199.1, 203.8, 205.7, 215.9, 216.3, 219.5, 219.6, 222.8, 226.3, 230.8,
235.2, 235.8, 241,   235.9, 241.4, 249.6, 248.6, 251.4, 250.2, 249.5, 248.7,
251.1, 250.1, 247.6, 251.3, 244.2)))

# grafica de la serie
plot.ts(seriew5, type="l")

# transformacion de Box-Cox.
library(TSA)
(BoxCox.ar(seriew5))
# lambda=-0.1
# Parece que no es necesario transformar la serie (verificar)
# grafica de los correlogramas muestrales
detach("package:TSA")
par(mfrow=c(2,1))
acf(seriew5, lag.max=15, ci=0)
pacf(seriew5, lag.max=15, ci=0)
# conclusion parece que hay raiz unitaria

# analisis para la serie diferenciada una vez
plot.ts(diff(seriew5), type="l")
# conclusion parece que la serie diferenciada es estacionaria

#Para probar si el parametro de tendencia deterministica Theta0 es necesario,
#se calcula el t-ratio

wbar <- mean(diff(seriew5))
Swbar <- sqrt(var(diff(seriew5)) / length(diff(seriew5)))

t_ratio <- wbar/Swbar
t_ratio

#cuantil 0.025 superior con 498 g.l.
qt(p=0.025, df=, length(diff(seriew5))-1, lower.tail = F)

#lo cual indica que Theta0 si es significante.

# correlogramas muestrales
par(mfrow=c(2,1))
acf(diff(seriew5), lag.max=15)
pacf(diff(seriew5), lag.max=15)
#Conclusion: parece que la serie diferenciada sigue es ruido blanco
# y por tanto la serie original es un paseo aleatorio con deriva.



#AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA

# estimación del modelo usando la función arima de stats
# estimación ML condicional con deriva
(mod_w5_CSS=arima(diff(seriew5), c(0, 0, 0), method = c("CSS"))) 
(res_w5_CSS=residuals(mod_w5_CSS))
# estimación ML exacta con deriva
(mod_w5_ML=arima(diff(seriew5), c(0, 0, 0), method = c("ML"))) 
(res_w5_ML=residuals(mod_w5_ML))
# estimación ML exacta con valores iniciales dados por la estimación condicional sin deiva
(mod_w5_CSS_ML=arima(diff(seriew5), c(0, 0, 0), method = c("CSS-ML"))) 
(res_w5_CSS_ML=residuals(mod_w5_CSS_ML))

#AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA






# ejemplo 6.6
# Series W6   Yearly U.S. tobacco production from 1871 to 1984
(seriew6=ts(scan("DatosWei/W6_USYearlyTobaccoProduction.txt")))
# ----------------------------------------------------------------------------
# grafica de la serie
plot.ts(seriew6, type="l")

# transformacion de Box-Cox.
library(TSA)
(BoxCox.ar(seriew6))

# lambda=.4
# serie transfornada 
tseriew6=seriew6^.4
# grafica de la sere sin transformar y transformada
par(mfrow=c(2,1))
plot.ts(seriew6)
plot.ts(tseriew6)
# la transformacion no trabaja bien. Sin embargo, la varianza tiene
# tendencia a aumentar con el nivel de la serie. Se experimenta usando logaritmos
lseriew6=log(seriew6)
# grafica de la sere sin transformar y transformada con logaritmo natural
par(mfrow=c(2,1))
plot.ts(seriew6)
plot.ts(lseriew6)
# la transformacion logaritmica parece trabajar

# grafica de la serie transformada y correlogramas muestrales
detach("package:TSA")
par(mfrow=c(3,1))
plot.ts(lseriew6)
acf(lseriew6, lag.max=15, ci=0)
pacf(lseriew6, lag.max=15, ci=0)
# conclusion parece que la serie necesita ser diferenciada


# analisis para la serie transformada diferenciada una vez
plot.ts(diff(log(seriew6)), type="l")
# conclusion parece que la serie diferenciada es estacionaria


#Para probar si el parametro de tendencia deterministica Theta0 es necesario,
#se calcula el t-ratio

wbar <- mean(diff(log(seriew6)))
Swbar <- sqrt(var(diff(log(seriew6))) / length(diff(log(seriew6))))

t_ratio <- wbar/Swbar
t_ratio

#cuantil 0.025 superior con 498 g.l.
qt(p=0.025, df=, length(diff(log(seriew6)))-1, lower.tail = F)

#lo cual indica que Theta0 si no es significante.

# correlogramas muestrales
par(mfrow=c(2,1))
acf(diff(log(seriew6)), lag.max=15)
pacf(diff(log(seriew6)), lag.max=15, ci=0)
#Conclusion: parece que la serie transformada diferenciada sigue es proceso MA(1)


#AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA

# estimación del modelo usando la función arima de stats
# estimación ML condicional con deriva
(mod_w6_CSS=arima(diff(log(seriew6)), c(0, 0, 1), method = c("CSS"))) 
(res_w6_CSS=residuals(mod_w6_CSS))
# estimación ML exacta con deriva
(mod_w6_ML=arima(diff(log(seriew6)), c(0, 0, 1), method = c("ML"))) 
(res_w6_ML=residuals(mod_w6_ML))
# estimación ML exacta con valores iniciales dados por la estimación condicional sin deiva
(mod_w6_CSS_ML=arima(diff(log(seriew6)), c(0, 0, 1), method = c("CSS-ML"))) 
(res_w6_CSS_ML=residuals(mod_w6_CSS_ML))

#AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA

