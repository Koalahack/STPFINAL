


m1 <- arima.sim(list(order = c(1,0,0), ar = 0.6), n = 100, sd = sqrt(0.1), mean=3.6)

est_m1 <- arima(m1, order = c(1,0,0), method = "CSS-ML")
est_m1$aic
predict(est_m1, n.ahead = 15)

library(TSA)

x11()
plot(est_m1, n.ahead = 15, type = "b", xlab = "Tiempo", ylab = "Serie")

abline(h = coef(est_m1)[names(coef(est_m1)) == "intercept"])


# Ejemplo 9.4 del libro Time Series Analysis with Applications in R, Cryer and Chan (2008), pag. 206

# Serie sobre el n?mero anual de liebres en la cuenca principal de la bahia de Hudson (Canada).

#Transformaci?n adecuada: Raiz Cuadrada
#Modelo propuesto: AR(3)

data(hare)

m1.hare <- arima(sqrt(hare), order = c(3,0,0))

plot(m1.hare, n.ahead = 25, type = "b", xlab = "Year", ylab = "Sqrt(hare)")

abline(h = coef(m1.hare)[names(coef(m1.hare)) == "intercept"])
