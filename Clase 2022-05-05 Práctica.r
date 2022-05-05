rm(list = ls())
hatco <- read.csv("./Inputfiles/hatco2.csv", stringsAsFactors = TRUE)

str(hatco)
head(hatco)
plot(hatco[, c(6:13)])
# relación lineal entre variables explicativas y explicadas?
# En algunos, bastante
# colinealidad?
# cierta entre fidelidad y velocudad, entre velocudad y servconj, y entre servconj y fidelidad
# (relación lineal entre los 3)
mod1 <- lm(fidelidad ~ velocidad + precio + flexprec + imgfabri + servconj + imgfvent + calidadp, hatco)
mod1
# ────────────────────────────────────────────────────────────────────────────────
# 2.3. Inferencia sobre el modelo
summary(mod1)
# R^2 == 0.775, R^2 ajustado 0.758
anova(mod1)
#  Varias de las variables son estadísticamente no significativas

plot(hatco[, c(2:7)])
otroMod <- lm(fidelidad ~ tamano + adquisic + tindustr + tsitcomp + velocidad + precio, hatco)
otroMod
summary(otroMod)
anova(otroMod)
# El R^2 ronda el 80%
# al nivel de significación 5%, todas las variables del mod2 son significativas salvo tindustr (con p-valor 0.13 > 0.05)
# ────────────────────────────────────────────────────────────────────────────────
# Prescindir del término constante?
summary(mod1)
# p valor del término constante: 0.044 < 0.05. No se rechaza al nivel de significación 5%
# para el nivel de significación 1% sí se rechaza
summary(otroMod)
# p valor < 10^(-16) < 0.01, no se puede prescindir del término constante ni al NS 5% ni 1%

# ────────────────────────────────────────────────────────────────────────────────
# 2.4. Diagnóstico del modelo

# Homocedasticidad
residuos1.estandarizados  <- rstandard(mod1) # residuos estandarizados
plot(mod1$fitted.values, residuos1.estandarizados)
plot(xij, residuos1.estandarizados) # TODO:
# Incorrelación

plot(hatco$empresa, e)
library(lmtest)
dwtest(mod1)
# Hipótesis nula: autocorrelación de los errores es 0 ==> no se rechaza
# Eso es, no rechazamos TODO: º

# Normalidad
ks.test(residuos1.estandarizados, pnorm)
qqnorm(residuos1.estandarizados)
qqline(residuos1.estandarizados)

# Linealidad
library("car")
crPlots(mod1)
# La mayoría de las variables se ajustan bastante bien a la recta.
# Velocidad no tanto

# Datos anómalos
sum(abs(residuos1.estandarizados) > 2.5)
# 2 Empresas anómalas:
hatco[abs(residuos1.estandarizados) > 2.5, ]
# datos influyentes:
cooks.distance(mod1) # leverage, aislamiento
plot(cooks.distance(mod1))
which(cooks.distance(mod1) > 0.1)
# 7
hatvalues(mod1)
plot(hatvalues(mod1))
indices <- which(hatvalues(mod1) > 0.25)
hatco[indices, ]
# 22, 55
influenceIndexPlot(mod1)
# 7, 100


# Eliminación observaciones anómalas

hatco <- hatco[-c(7,100),]
mod2 <- lm(fidelidad ~ velocidad + precio + flexprec + imgfabri + servconj + imgfvent + calidadp, hatco)
mod2

summary(mod2)
anova(mod2)
# Observamos, si comparamos con el sumario y anova del mod1, que ka regresión se ajusta mejor, puesto
# que los p-valores son aún más reducidos en varias de las variables: eliminar los datos anómalos hace que nos ajustemos
# mejor al modelo
# Tampoco es una sorpresa, claro, precisamente hemos eliminado los datos que en cualquier caso son los que argumentan que el modelo
# es algo dudoso o no bien ajustado