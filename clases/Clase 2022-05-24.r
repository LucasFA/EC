# Simulación de Variables Aleatorias

# 5.5.1 Método de inversión
#

# --------------------------------------------------------------------------------
# Eg

# F(x) = 1-exp(-\lambda x), x>= 0
# u = F^-1 (x) ==> x = F^-1(u) = - frac{log(1-5)}{\lambda}

n <- 1000
set.seed(1)
u <- runif(n)
lambda <- 0.5
x <- -log(u) / lambda
x[1:10]

# Comprobamos que es una exponencial con lambda 0.5
hist(x,
    freq = FALSE, breaks = "FD", main = "Método de inversión (Exponencial)",
    ylim = c(0, 0.5)
)
lines(density(x), col = "blue")
curve(dexp(x, rate = lambda), add = TRUE, col = 2)

# Comprobamos con Kolmogorov - Smirnov:
ks.test(x, pexp, rate = lambda)

# --------------------------------------------------------------------------------
# Eg
# Generamos desde una doble exp/de Cauchy
# f(x) = \l/2exp(-\|x|) ==>
# F(x) = \{
# x<0 -> exp(\l/x)/2
# x>0 -> 1-exp(\l/x)/2
# }
ddexp <- function(x, lambda) lambda * exp(-lambda * abs(x)) / 2
rddexp <- function(n, lambda) {
    u <- runif(n)
    x <- ifelse(u < 0.5,
        log(2 * u) / lambda,
        -log(2 * (1 - u)) / lambda # este ifelse es la F^-1(u)
    )
    return(x)
}

n <- 1000
set.seed(1)
lambda <- 1
x <- rddexp(n, lambda)
x[1:10]

hist(x,
    freq = FALSE, breaks = "FD", ylim = c(0, 0.6),
    main = "Método de inversión (Doble exponencial)"
)
lines(density(x), col = "blue")
curve(ddexp(x, lambda), add = TRUE, col = 2)

# la función de distribución de la doble exponencial
pddexp <- function(x, lambda) {
    x <- ifelse(x < 0,
        exp(lambda * x) / 2,
        1 - exp(-lambda * x) / 2
    )
    return(x)
}
# pasamos la función como argumento a ks.test()
ks.test(x, pddexp, lambda = lambda)

# --------------------------------------------------------------------------------
# EG Weibull
# F(x) = 1-exp(l(\l/x)^\alpha)

f <- function(x, lambda, alpha) {
    alpha * lambda^alpha * x^(alpha - 1) * exp(-(lambda * x)^alpha)
}

rweib <- function(n, lambda, alpha) {
    u <- runif(n)
    x <- (-log(1 - u))^(1 / alpha) / lambda
    return(x)
}

n <- 1000
set.seed(1)
lambda <- 0.5
alpha <- 2
x <- rweib(n, lambda, alpha)
x[1:10]

hist(x,
    freq = FALSE, breaks = "FD", ylim = c(0, 0.5),
    main = "Método de inversión (Weibull)"
)
lines(density(x), col = "blue")
curve(ddexp(x, lambda), add = TRUE, col = 2)

ks.test(x, pweibull, shape = alpha, scale = 1 / lambda)

# Problema: y si la inversa es difícil de calcular? Numéricamente? Alternativa:

# Método de aceptación-rechazo

# 2 pasos:
#   Generamos un valor candidato para la variable aleatoria.
#   Aceptamos el valor generado sólo si verica una condición particular.
# Alg:
#   Generas U
#   Generas candidato Y desde g(y)
#   Comprobación:
#       U < 1/M * f(Y)/g(Y) ==> X = Y se acepta como valor simulado de f
#       Si no, se rechaza el valor

# EG Beta desde Uniforme
a <- 2.7
b <- 6.3
# auxiliar? la uniforme basta. g(y) = unif
# Necesitamos calcular la M

res <- optimize(
    f = function(x) dbeta(x, shape1 = a, shape2 = b),
    maximum = TRUE, interval = c(0, 1)
)
res
M <- res$objective

a <- 2.7
b <- 6.3
curve(dbeta(x, shape1 = a, shape2 = b), 0, 1)
curve(M * dunif(x), 0, 1, add = TRUE, col = 2, lty = 2)
legend("right",
    legend = c(
        "f(x): Beta (a=2.7, b=6.3)",
        "g(x)*M"
    ),
    col = c(1, 2), lty = c(1, 2), bty = "n"
)

n <- 1000
x <- double(n)
f <- function(x) dbeta(x, shape1 = a, shape2 = b)
g <- function(x) 1
# Generación de los n valores a través del algoritmo
set.seed(1)
contador <- 0
for (i in 1:n) {
    repeat {
        u <- runif(1)
        y <- runif(1)
        contador <- contador + 1
        if (u <= f(y) / (M * g(y))) {
            break
        }
    }
    x[i] <- y
}

# Número de simulaciones total que han sido necesarias
contador
# Veremos después que el número esperado
n * M

# Comprobamos que la secuencia generada procede de la distribución deseada
hist(x, freq = FALSE, breaks = "FD", main = "Método de aceptación-rechazo (Beta)")
lines(density(x), col = "blue")
curve(dbeta(x, shape1 = a, shape2 = b), add = TRUE, col = 2)
ks.test(x, pbeta, shape1 = a, shape2 = b)

# Nota: eficiencia del algoritmo, elección de g
# Cuanto más cerca esté M de 1, más eficiente será (M=1 ==> f = g, no tiene sentido)
# En alguna familia paramétrica,
# M_opt = min theta max x f(x)/g_theta(x)

# EG Tomamos una doble exponencial como aux:

# Densidad de la doble exponencial:
ddexp <- function(x, lambda) lambda * exp(-lambda * abs(x)) / 2
# Construimos una función que dado lambda nos da el M óptimo
M.lambda <- function(lambda) {
    optimize(
        f = function(x) dnorm(x) / ddexp(x, lambda), maximum = TRUE,
        interval = c(0, 2)
    )$objective
}
# Minimizamos la función anterior en lambda para obtener el óptimo
# y el correspondiente M
res <- optimize(M.lambda, interval = c(0.5, 2))
res

lambda.opt <- res$minimum
M <- res$objective

curve(dnorm(x), -3, 3, ylim = c(0, 0.7))
curve(M * ddexp(x, lambda.opt), add = TRUE, col = 2, lty = 2)
legend("topright",
    legend = c("f(x): N(0,1)", "g(x)*M"),
    col = c(1, 2), lty = c(1, 2), bty = "n"
)

# Función para simular de la doble exponencial (método de inversión)
rddexp <- function(n, lambda) {
    u <- runif(n)
    x <- ifelse(u < 0.5, log(2 * u) / lambda, -log(2 * (1 - u)) / lambda)
    return(x)
}
# Método de aceptación-rechazo para simular la normal
n <- 1000
x <- double(n)
f <- function(x) dnorm(x)
g <- function(x) ddexp(x, lambda = lambda.opt)
# Generación de los n valores a través del algoritmo
set.seed(1)
contador <- 0
for (i in 1:n) {
    u <- runif(1)
    y <- rddexp(1, lambda.opt)
    contador <- contador + 1
    while (u > f(y) / (M * g(y))) {
        u <- runif(1)
        y <- rddexp(1, lambda.opt)
        contador <- contador + 1
    }
    x[i] <- y
}
# Número de simulaciones total que han sido necesarias
contador
# El número esperado es
n * M
# Comprobamos que la secuencia generada procede de la distribución deseada
hist(x, freq = FALSE, breaks = "FD", main = "Método de aceptación-rechazo (Normal)")
lines(density(x), col = "blue")
curve(dnorm(x), add = TRUE, col = 2)
# Confirmamos con el test de Kolmogorov-Smirnov
ks.test(x, pnorm)

1 / M # tasa de aceptación

# --------------------------------------------------------------------------------
# modif a A-R

# --------------------------------------------------------------------------------
# Otros métodos

# Composición
#  Si la densidad objetivo es una mixtura discreta de densidades
#  eso es, es la suma (discreta ie no integral) de funciones de densidad
# -> escoges j.
# -> generas X desde f_j

# Box Muller
# Para generar normales independientes
# E <- Exp(1); U <- Unif
# ==> X1 = sqrt(2E) cos(2piU) es normal
# ==> X2 = sqrt(2E) sin(2piU) es normal
# y son indep

# Aplicaciones
# aproximar la distribución de estimadores

# EG 1
mu <- 10
sigma <- 1
n <- 10 # tamaño de la muestra
nsim <- 1000 # número de simulaciones
# simulamos nsim=1000 muestras de tamaño n=10
# y las almacenamos por filas en una matriz (nsim*n)
set.seed(1)
muestras <- matrix(rnorm(nsim * n, mean = mu, sd = sigma), ncol = n, nrow = nsim)
# A partir de cada muestra calculamos la media muestral
medias <- rowMeans(muestras)
# medias contiene los nsim=1000 valores simulados de la media muestral
# un histograma de estos valores nos da una aproximación de la distribución muestral
hist(medias, breaks = 20, freq = FALSE, main = "Distribución muestral de la media")
# superponemos la densidad suavizada
lines(density(medias), col = "blue")
# ahora la distribución exacta (N(mu, sigma/sqrt(n)))
curve(dnorm(x, mean = mu, sd = sigma / sqrt(n)), col = 2, add = TRUE)



# Ej Comprobar TCL
# 1.
# 2.
# 3.


# Eg 2 mediana

mu <- 10
sigma <- 1
n <- 10 # tamaño de la muestra
nsim <- 1000 # número de simulaciones
# simulamos nsim=1000 muestras de tamaño n=10
# y las almacenamos por filas en una matriz (nsim*n)
set.seed(1)
muestras <- matrix(rnorm(nsim * n, mean = mu, sd = sigma), ncol = n, nrow = nsim)
# A partir de cada muestra calculamos la medianal
medianas <- apply(muestras, 1, median)
# medianas contiene los nsim=1000 valores simulados de la media muestral
# un histograma de estos valores nos da una aproximación de la distribución muestral
hist(medianas, breaks = 20, xlim = c(9, 11), freq = FALSE, main = "Distribución muestral de la mediana")
# superponemos la densidad suavizada
lines(density(medianas), col = "blue")


# Comparación de estimadores

# Sea f una f densidad de una N(0,1) contaminada por una N(3,3) con frecuencias 0.95, 0.05 respectivamente.
# Con el método de composición:
n <- 100
set.seed(1)
j <- rbinom(n, 1, 0.05) # 1's y 0's indicando si es f2 o f1, respectivamente
x <- rnorm(n, 3 * j, 1 + 2 * j) # esto genera n valores de f1 o f2 dependiendo de j
# representamos un histograma de la muestra generada
hist(x, breaks = "FD", freq = FALSE, main = "Muestra contaminada")
# superponemos la densidad desde la que se generó
curve(0.95 * dnorm(x, 0, 1) + 0.05 * dnorm(x, 3, 3), add = TRUE, col = 2)
# Con datos anómalos
mean(x)
median(x)
# Dan resultados similares

n <- 100
set.seed(1)
j <- rbinom(n * nsim, 1, 0.05)
muestras <- matrix(rnorm(n * nsim, 3 * j, 1 + 2 * j), nrow = nsim, ncol = n)
# cada fila de la matriz 'muestras' es una muestra de tamaño n
# calculamos los estimadores
medias <- apply(muestras, 1, mean)
medianas <- apply(muestras, 1, median)
# comparamos la distribución muestral usando un boxplot
boxplot(medias, medianas, names = c("Media", "Mediana"))
# una línea horizontal indicando el valor a estimar (mu=0)
abline(h = 0, col = 2)
# La media se ve bastante afectada por valores anómalos

# Estimamos los errores cuadráticos medios E[(theta_gorro - theta)^2]
ecm.media <- mean((medias - 0)^2)
ecm.media
ecm.mediana <- mean((medianas - 0)^2)
ecm.mediana
# boxplot de las desviaciones al cuadrado
boxplot(medias^2, medianas^2,
    ylab = "Errores cuadráticos",
    names = c("Media", "Mediana")
)