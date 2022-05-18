
# 5.2 Generación de números pseudo-aleatorios y distribuciones de probabilidad en R
rnorm(n = 10, mean = 10, sd = 2) # Normal con media 10 y desviación típica 2
runif(10) # Uniforme en (0,1)
rpois(n = 10, lambda = 1) # Poisson con media 1

sample(1:5, 5) # permutación
sample(1:5, 10, replace = TRUE) # muestra con reemplazo
sample(c(0, 1), 5, replace = TRUE, prob = c(0.4, 0.5)) # Bernoulli
sample(cars$speed, 3) # tres valores aleatorios de cars$speed

mx <- numeric()
for (i in 1:10) {
    set.seed(i)
    x <- rnorm(100, mean = 2)
    mx[i] <- mean(x)
}
mean(mx)

dpois(x = 0:10, lambda = 3)
plot(
    dpois(x = 0:10, lambda = 3),
    type = "h",
    xlab = "x",
    ylab = "P(X=x)",
    main = expression(Poisson(lambda == 3))
)

plot(ppois(q = 0:10, lambda = 3),
    type = "s", xlab = "x", ylab = expression(P(X <= x)),
    main = expression(Poisson(lambda == 3))
)
qnorm(0.025, lower.tail = FALSE) # valor crítico (Normal) alpha=0.05

# 5.3 Ley de los grandes números
# Probamos primero algunos lanzamientos (uno, diez):
sample(c(cara = 1, cruz = 0), 1, replace = TRUE, prob = c(0.5, 0.5))
x <- sample(c(cara = 1, cruz = 0), 10, replace = TRUE, prob = c(0.5, 0.5))
x
mean(x) # frecuencia relativa de cara en 10 lanzamientos
x <- sample(c(cara = 1, cruz = 0), 50, replace = TRUE, prob = c(0.5, 0.5))
mean(x) # frecuencia relativa de cara en 50 lanzamientos

# Representamos gráficamente la solución
set.seed(1) # para poder reproducir los resultados de abajo
nsim <- 1000
x <- sample(c(cara = 1, cruz = 0), nsim, replace = TRUE, prob = c(0.5, 0.5))
n <- 1:nsim
plot(n, cumsum(x) / n,
    type = "l", ylab = "Proporción de caras",
    xlab = "Número de lanzaamientos", ylim = c(0, 1)
)
abline(h = 0.5, lty = 2, col = 2)

# alternativamente podríamos haber hecho:
x <- rbinom(nsim, size = 1, prob = 0.5)
head(x)
# ó
x <- runif(nsim) < 0.5
head(x)


# 5.3.2. Detección de problemas de convergencia
# Esperanza no finita
set.seed(1)
nsim <- 10000
x <- rcauchy(nsim)
n <- 1:nsim
plot(n, cumsum(x) / n,
    type = "l", ylab = "Media muestral",
    xlab = "Número de simulaciones",
    main = "Secuencia de medias de una Cauchy"
)
abline(h = 0, lty = 2, col = 2)

# probando con varias semillas
for (i in 2:4) {
    set.seed(i)
    x <- rcauchy(nsim)
    plot(n, cumsum(x) / n,
        type = "l", ylab = "Media muestral",
        xlab = "Número de simulaciones"
    )
    abline(h = 0, lty = 2, col = 2)
}
# cajas para observar cómo se comporta: muchos datos anómalos
for (i in 2:4) {
    set.seed(i)
    x <- rcauchy(nsim)
    boxplot(x, ylab = "Media muestral")
}

# 5.3.3  Precisión de la aproximación

# ~~> intervalos de confianza
# Teorema central del límite: Teorema Central del Límite.
# Dada una secuencia X1, ..., Xn variables aleatorias iid
# con esperanza E[Xi] = µ y varianza V(Xi) = s^2 < 8
# ==> Z_n = (mean(c(X1,...Xn)) - \mu )/(\sigma/sqrt(n))
# para n --> \infty
# ~~> intervalo de confianza
# ...

set.seed(1)
nsim <- 10000
x <- rnorm(nsim)
# Aproximación de la media:
mean(x)
# error estándar
sd(x) / sqrt(nsim)
# error máximo admisible al nivel de confianza 0.95
qnorm(0.975) * sd(x) / sqrt(nsim)

# representamos con intervalos de confianza
n <- 1:nsim
# medias muestrales para n=1,2,...,nsim
estim <- cumsum(x) / n
# correspondientes errores de estimación
# por simplicidad con varianza muestral en lugar de cuasivarianza
estim.err <- sqrt(cumsum((x - estim)^2)) / n
# gráfico de convergencia
plot(n, estim,
    type = "l", xlab = "Número de simulaciones",
    ylab = "Aproximación de la media y error", ylim = c(-1, 1)
)
# media teórica:
abline(h = 0, col = 2)
# intevalos de confianza (1-alpha=0.95)
z <- qnorm(0.975)
lines(estim - z * estim.err, lty = 3, lwd = 2, col = "blue")
lines(estim + z * estim.err, lty = 3, lwd = 2, col = "blue")

# Ejercicio. Comprueba y visualiza la convergencia en el caso variables con distribución
# uniforme, U(-3, 3).

# --------------------------------------------------------------------------------
# descando
# --------------------------------------------------------------------------------

# 5.3.4. Determinación del número de simulaciones

# En el caso en que la varianza de las X_i, \sigma^2 sea conocida, resolver el problema es
# determinar n tal que z_{\alpha/2} * \sigma / \sqrt(n) < \epsilon
# n = (z_{\alpha/2} \sigma / \epsilon )^2

# Ejercicio. Calcular el número de simulaciones necesario para aproximar la media µ
# de la siguientes distribuciones con un error máximo admisible de 0.1|µ|:
# 1. Normal con media µ = 10 y desviación típica s = 5.
# 2. Chi-cuadrado con 10 grados de libertad (µ = 10, s^2 = 2 * µ).
# 3. Poisson con parámetro <U+03BB> = 10 (µ = 10, s^2 = µ)

determina.n <- function(epsilon, sigma, alpha) {
    z <- qnorm(alpha / 2, lower.tail = F)
    n <- z * sigma / epsilon
    return(n^2)
}
# 1.
determina.n()
# 2.
# 3.

# --------------------------------------------------------------------------------

# En el caso \sigma^2 desconocidam, ste algoritmo
# 1. Fijar tamaño inicial n0.
# 2. Generarlos.
# 3. Si querías más precisión, genera


# --------------------------------------------------------------------------------
# Integración de MC
# Clásica
# Observación: la integral de h en [0,1] es la esperanza de h(X) con X ~> U(0,1)

# EG int. en [0,1] de 4x^4
h <- function(x) (4 * x^4) * (x > 0 & x < 1)
# visualizamos la función en el dominio de integración
curve(h, 0, 1)
# fijamos el número de simulaciones (n=nsim)
nsim <- 100
# calculamos la aproximación
set.seed(1)
x <- runif(nsim) # x1,...,xn
hx <- sapply(x, h) # h(x1),...h(xn)
mean(hx) # la aproximación final
# valor exacto
4 / 5
# --------------------------------------------------------------------------------
# gráfica de ello:
nsim <- 1000
set.seed(1)
x <- runif(nsim)
hx <- sapply(x, h)
# aproximaciones para $n=1,...,nsim$
estim <- cumsum(hx) / (1:nsim)
# errores de estimación correspondientes
estim.err <- sqrt(cumsum((hx - estim)^2)) / (1:nsim)
plot(1:nsim, estim,
    type = "l", ylab = "Aproximación y límites de error",
    xlab = "Número de simulaciones"
)
z <- qnorm(0.025, lower.tail = FALSE)
lines(estim - z * estim.err, col = "blue", lwd = 2, lty = 3)
lines(estim + z * estim.err, col = "blue", lwd = 2, lty = 3)
abline(h = 4 / 5, col = 2)
# Aproximación final y su error
estim[nsim]
estim.err[nsim]
# --------------------------------------------------------------------------------
# Caso general: integrales con límites infinitos
# idea: factorizar la función en producto de f densidad por otra
# I = \int h(x) = \int c(x)f(x) = E[c(X)]
# ~~> I \aproxeq 1/n \sum c(X_i)
# EG \int_2^\infty \frac{e^{-x^2/2}}{\sqrt{2\pi}}dx
# ~~> c(x) = I(x>2)

nsim <- 1000
set.seed(1)
x <- rnorm(nsim) # x1,...,xn
cc <- function(x) (x > 2)
cx <- cc(x) ## c(x1),...,c(xn)
# aproximación:
mean(cx)
# error
qnorm(0.025, lower.tail = FALSE) * sd(cx) / sqrt(nsim)
# valor exacto
pnorm(2, lower.tail = FALSE)
# --------------------------------------------------------------------------------
# ej la misma integral pero con límite inferior en 4.5
nsim <- 1000
set.seed(1)
x <- rnorm(nsim) # x1,...,xn
cc <- function(x) (x > 4.5)
cx <- cc(x) ## c(x1),...,c(xn)
# aproximación:
mean(cx)
# error
qnorm(0.025, lower.tail = FALSE) * sd(cx) / sqrt(nsim)
# valor exacto
pnorm(4.5, lower.tail = FALSE)
# Falla para valores muy pequeños

#
# --- 5.4.2 MUESTREO POR IMPORTANCIA ---------------------------------------------------
#

# Idea: cambiar la función de densidad por otra con una cola más pesada
# generar Z_1..Z_n desde f densidad g(z) con soporte incluyendo el de f(x)
# I = \int c(x) f(x) dx = \int c(z) w(z) g(z) dz = E[c(Z)w(Z)]
# w(z) = f(z)/g(z)
# ~~> I \aproxeq 1/n \sum c(Z_i)w(z_i)

nsim <- 1000
set.seed(1)
z <- rexp(nsim) + 4.5
w <- dnorm(z) / dexp(z - 4.5)
boxplot(w)
# aproximación:
mean(w)

# error
qnorm(0.025,lower.tail=FALSE)*sd(w)/sqrt(nsim)
