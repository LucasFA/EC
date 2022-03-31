

# install.packages("Rsolnp")
# install.packages("maxLik")
rm(list = ls())
muestra <- scan(file = "./inputfiles/datos_31.txt")

# --------------------------------------------------------------------------------
# Directo
# log-likelihood
logl <- function(theta) {
    a <- theta[1]
    b <- theta[2]
    l <- sum(log(dgamma(x = muestra, shape = a, scale = b)))
    return(-l)
}

# Establecer valores iniciales para los parámetros
b0 <- a0 <- 1
res <- optim(par = c(a0, b0), fn = logl)
res

b0 <- a0 <- 2
res <- optim(par = c(a0, b0), fn = logl)
res$par


library(Rsolnp)
b0 <- a0 <- 1
res <- solnp(pars = c(a0, b0), fun = logl, LB = c(0, 0))
res$pars

# --------------------------------------------------------------------------------
# Método de los momentos
media_muestra <- mean(muestra)
b0_momentos <- var(muestra) / mean(muestra)
a0_momentos <- mean(muestra) / b0_momentos
a0_momentos
b0_momentos

res <- solnp(pars = c(a0, b0), fun = logl, LB = c(0, 0))
res$pars
c(a0_momentos, b0_momentos) - res$pars

# --------------------------------------------------------------------------------
# maxLik
library(maxLik)
logl2 <- function(theta) {
    -logl(theta)
}
res <- maxLik(logl2, start = c(1, 1))
res

res <- maxLik(logl2, start = c(b0_momentos, a0_momentos))
res
# Se realizan 2 iteraciones menos
# Observación: el orden es ese, al contrario, sse realizan 25 iteraciones
maxLik(logl2, start = c(a0_momentos, b0_momentos))

A <- matrix(c(1, 0, 0, 1), 2)
B <- c(0, 0)
maxLik(logl2, start = c(1, 1), constraints = list(ineqA = A, ineqB = B))

# --------------------------------------------------------------------------------
# Ecuaciones normales

f <- function(a) {
    log(a) - digamma(a) - log(media_muestra) + mean(log(muestra))
}
res <- uniroot(f, c(0.1, 100))
res
a <- res$root
b <- media_muestra / a
a
b

#
# ─── APARTADO 2 ─────────────────────────────────────────────────────────────────
#

    