# Clase 2022-05-03
# Véase la clase de la semana pasada, a partir de "Otras funciones"

#
# --- 4.1.6 DISPOSITIVOS GRAFICOS ------------------------------------------------------
#

# dev.(...)
# Funciones: pdf() jpeg(), postscript()

# --------------------------------------------------------------------------------
# Bloque 1: Generación de datos para los gráficos
x <- seq(0, 2 * pi, length.out = 20)
set.seed(2)
y1 <- sin(x) + 0.3 * rnorm(20)
y2 <- sin(x) + 0.15 * rnorm(20)
# Bloque 2: Iniciar un dispositivo gráfico (número 2)
# y representar (x,y1)
windows()
plot(x, y1, type = "o", pch = 15, col = 2)
# Bloque 3: Iniciar otro dispositivo gráfico (número 3)
# y representar (x,y2)
windows()
plot(x, y2, type = "o", pch = 16, col = 4)
## Comprueba la lista de dispositivos y el dispositivo actual
dev.list()
dev.cur()

# Bloque 4: Añadir la curva $f(x)=sen(x)$ a ambos gráficos
# primero en rojo en el dispositivo 2
dev.set(2)
curve(sin(x), add = TRUE, col = 2, lty = 3)
# después en azul el dispositivo 3
dev.set(3) # esto no sería necesario (3 es el dispositivo actual)
curve(sin(x), add = TRUE, col = 4, lty = 3)
# Bloque 5: Comparar las dos gráficas en un nuevo dispositivo (4)
# en este caso será un fichero con nombre 'graf.pdf'
pdf("graf.pdf")
plot(x, y1, pch = 15, col = 2, ylab = "")
points(x, y2, pch = 16, col = 4)
# añadimos segmentos entre los puntos
segments(x, y1, x, y2, col = "purple")
# y la curva f(x)=sen(x) de referencia
curve(sin(x), add = TRUE, lty = 3)
# cerramos el dispositivo actual (el pdf)
dev.off()

#
# --- 4.1.7 GRAFICOS DE VARIABLES ALEATORIAS -------------------------------------
#

# Lo vimos previamente

#
# --- 4.1.8 PAQUETES ESPECIFICOS -------------------------------------------------
#

# ggplot2



#
# --- SEGUNDA PARTE DEL TEMA. ANALISIS ESTADISTICO --------------------------------
#

# Definición de modelos estadístico

help("~")
# Ajuste del modelo lineal

# --------------------------------------------------------------------------------
# Eg
library(MASS)
plot(forbes$bp, forbes$pres, xlab = "temperatura", ylab = "presión")

lm(pres ~ bp, data = forbes)

fit <- lm(pres ~ bp, data = forbes)
typeof(fit)
class(fit)

names(fit)

plot(forbes$bp, forbes$pres, xlab = "temperatura", ylab = "presión")
abline(fit)

summary(fit)

anova(fit)

confint(fit)
predict(fit, newdata = data.frame(bp = 200), interval = "confidence", se.fit = TRUE)
predict(fit, newdata = data.frame(bp = 200), interval = "prediction")

x0 <- data.frame(bp = seq(min(forbes$bp), max(forbes$bp), length.out = 20))
pred.m <- predict(fit, newdata = x0, interval = "confidence", se.fit = T)
pred.p <- predict(fit, newdata = x0, interval = "prediction", se.fit = T)
matplot(x0$bp, cbind(pred.m$fit, pred.p$fit[, -1]),
    lty = c(1, 2, 2, 3, 3),
    col = c(1, 2, 2, 4, 4), type = "l", xlab = "temperatura", ylab = "presión", main = ""
)
legend("topleft", c("Media", "Predicción"), lty = c(2, 3), col = c(2, 4), bt = "n")
points(forbes$bp, forbes$pres)

# --------------------------------------------------------------------------------
# Eg 2
library(faraway)
data(coagulation)
boxplot(coag ~ diet, data = coagulation)

lm(coag ~ diet, data = coagulation)
fit <- lm(coag ~ diet, data = coagulation)
anova(fit)
# Alternativa usando aov en vez de lm
fit2 <- aov(coag ~ diet, data = coagulation)
summary(fit2)

pairwise.t.test(coagulation$coag, coagulation$diet)
TukeyHSD(aov(coag ~ diet, coagulation))
