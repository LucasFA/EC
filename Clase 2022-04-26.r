
rm(list = ls())

data(cars, package = "datasets")
plot(cars$speed, cars$dist)
abline(line(cars$speed, cars$dis))

#
# --- 4.1.1 - PLOT ---------------------------------------------------------------
#


plot(cars$speed, cars$dist,
    main = "Diagrama de dispersión",
    xlab = "Distancia de frenado (pies)", ylab = "Velocidad (millas por hora)",
    pch = 21, col = "blue", bg = "cyan", cex = 1.5
)

radio <- 0:10
area <- pi * radio^2
plot(radio, area,
    type = "b", main = "Áreas de círculos en función del radio",
    xlab = "Radio (r)", ylab = expression(Área == pi * r^2), col = "purple", pch = 20
)

class(AirPassengers)
plot(AirPassengers, main = "Una serie temporal")
title(main = "Una serie temporal")

class(ChickWeight$Diet)
plot(ChickWeight$Diet, main = "Un factor")

class(Titanic)
plot(Titanic, main = "Una tabla cruzada con tres factores de clasificación")

#
# --- 4.1.2 - CURVE --------------------------------------------------------------
#

curve(x^3 - 3 * x, -2, 2, ylab = expression(f(x) == x^3 - 3 * x))

curve(dnorm(x, mean = 10, sd = 2), 4, 16,
    ylab = "densidad",
    main = expression(paste("Normal (", mu == 10, ",", sigma == 2, ")"))
)

curve(x^2, -1, 1,
    ylab = "",
    main = expression(paste(
        f(x) == x^2,
        "y", f(x) == x^4,
        sep = " "
    ))
)
curve(x^4, -1, 1, col = 2, add = TRUE)

# --------------------------------------------------------------------------------
# Ejercicio 1

curve(dnorm(x, 0, 1), col = 1, xlim = c(-3, 7), ylim = c(0, 0.8))
curve(dnorm(x, 0, 0.5), add = T, col = 2)
curve(dnorm(x, 3, 1), add = T, col = 3)
title(main = "Funciones de densidad")


#
# --- 4.1.3 SYMBOLS --------------------------------------------------------------
#

n <- 10
set.seed(1)
x <- runif(n)
colores <- rainbow(n)
symbols(1:n, x, circles = x, bg = colores, ylim = c(0, 1.5), xlim = c(0, 11))

#
# --- 4.1.4 AÑADIR ELEMENTOS A GRÁFICO. LOW LEVEL PLOTTING ------------------------
#

# points
set.seed(123)
plot(rnorm(10),
    main = "Cuatro muestras de una Normal estándar",
    pch = 17, ylim = c(-3, 3)
)
points(rnorm(10), pch = 17, col = 2)
points(rnorm(10), pch = 17, col = 3)
points(rnorm(10), pch = 17, col = 4)

# --------------------------------------------------------------------------------
# lines, abline
set.seed(123)
x <- rnorm(10)
plot(x,
    main = "Muestra de una Normal estándar",
    ylim = c(-3, 3), pch = 19
)
lines(x, col = 4, lwd = 2)
abline(h = 0, col = 1)
abline(h = -3, col = 2, lty = 3, lwd = 2)
abline(h = 3, col = 2, lty = 3, lwd = 2)

# --------------------------------------------------------------------------------
# text
plot(x,
    main = "Muestra de una Normal estándar",
    ylim = c(-4, 4), xlim = c(0, 10), pch = 19
)
abline(h = -3, col = 2, lty = 3, lwd = 2)
abline(h = 3, col = 2, lty = 3, lwd = 2)
pr3 <- round(pnorm(-3), 5) # P[X < -3] para X<-N(0,1)
text(5, 3.5, paste0("probabilidad=", pr3), col = 2, pos = 3)
text(0.2, 0, paste0("probabilidad=", 1 - 2 * pr3), pos = 3, srt = 90)
text(5, -3.5, paste0("probabilidad=", pr3), col = 2)

# --------------------------------------------------------------------------------
# Legend
# Eg1
x <- seq(0, 2 * pi, length = 100)
y1 <- cos(x)
y2 <- sin(x)
plot(x, y1,
    type = "l", col = 2, lwd = 3,
    xlab = expression(group("[", list(0, 2 * pi), "]")),
    ylab = "", main = "Seno y Coseno"
)
lines(x, y2, col = 3, lwd = 3, lty = 2)
points(pi, 0, pch = 17, col = 4)
legend(0, -0.5, c("Coseno", "Seno"), col = 2:3, lty = 1:2, lwd = 3)
abline(v = pi, lty = 3)
abline(h = 0, lty = 3)
text(pi, 0, expression(group("(", list(pi, 0), ")")),
    adj = c(0, 0)
)
# Eg2
radio <- seq(0, 5, by = 0.1)
area <- pi * radio^2
perimetro <- 2 * pi * radio
plot(radio, area, type = "o", ylab = "", pch = 19, col = 2)
lines(radio, perimetro, type = "o", pch = 17, col = 4)
legend("topleft",
    legend = c("Área", "Perímetro"),
    lty = 1, pch = c(19, 17), col = c(2, 4), bt = "n"
)

# --------------------------------------------------------------------------------
# Añadir leyenda a gráfica del ejercicio 1

curve(dnorm(x, 0, 1), col = 1, xlim = c(-3, 7), ylim = c(0, 0.8), lwd = 2)
curve(dnorm(x, 0, 0.5), add = T, col = 2, lwd = 2)
curve(dnorm(x, 3, 1), add = T, col = 3, lwd = 2)
title(main = "Funciones de densidad")
legend("topright",
    legend = c("N(0, 1)", "N(0, 0.5)", "N(3, 1)"),
    col = 1:3,
    lty = 1,
    lwd = 2
)
graphics.off()

# --------------------------------------------------------------------------------
# Otras funciones

# Ejemplo con polygon() y segments()
plot(1:9,
    type = "n", xlab = "", ylab = "", axes = FALSE,
    main = "Ejemplos polygon(), segments()"
)
polygon(c(4, 6, 6, 4, NA, 6, 8, 7), c(2, 2, 4, 4, NA, 6, 6, 8),
    density = c(10, 20), angle = c(-45, 45), col = c("red", "darkblue")
)
segments(
    x0 = c(3, 2), y0 = c(6, 7), x1 = c(3, 4), y1 = c(8, 7),
    lwd = 2, col = "green"
)
# Ejemplo con axis() y title()
plot(Orange$circumference, Orange$age,
    xaxt = "n",
    xlab = "", ylab = ""
)
x <- quantile(Orange$circumference, probs = c(0.01, 0.25, 0.5, 0.75, 0.99))
xcm <- x / 10 ## x está en milímetros, xcm en centímetros
axis(1, at = x, labels = round(xcm, 1))
abline(v = x, lty = 3, lwd = 3, col = "lightgray")
title(
    main = "Ejemplo de axis() y grid()",
    sub = "Datos Orange",
    xlab = "x = circunferencia (cm)",
    ylab = "y = edad (días desde 1968/12/31)"
)

#
# --- 4.1.5 OPCIONES. FUNCIÓN `par` -------------------------------------------------
#

par(mfrow = c(2, 2)) # matriz de 2x2 gráficos (se rellenan por filas)
curve(dnorm(x), -3, 3, main = "Gráfico 1: Normal estándar")
curve(dt(x, df = 10), -3, 3, main = "Gráfico 2: t de Student")
curve(dchisq(x, df = 5), 0, 20, main = "Gráfico 3: chi-cuadrado")
curve(df(x, df1 = 20, df2 = 10), 0, 5, main = "Gráfico 4: F de Snedecor")

par(
    mar = c(3.5, 3.5, 2.5, 1.5), oma = c(1, 1, 1, 1), mgp = c(1.5, 0.5, 0),
    cex.axis = 0.8, cex.lab = 0.8, cex.main = 1, col.lab = "blue", bty = "n"
)

par(mfrow = c(2, 2)) # matriz de 2x2 gráficos (se rellenan por filas)
curve(dnorm(x), -3, 3, main = "Gráfico 1: Normal estándar")
curve(dt(x, df = 10), -3, 3, main = "Gráfico 2: t de Student")
curve(dchisq(x, df = 5), 0, 20, main = "Gráfico 3: chi-cuadrado")
curve(df(x, df1 = 20, df2 = 10), 0, 5, main = "Gráfico 4: F de Snedecor")
