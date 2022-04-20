

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

medias <- function(x, na.rm = TRUE) {
    if (na.rm) {
        x <- x[!is.na(x)]
    }
    if (!is.vector(x) || !(is.numeric(x) || is.logical(x))) {
        warning("argument is not numeric or logical. Returning NA")
        return(list(aritm = NA, geom = NA, arm = NA))
    }
    hay_malos_valores <- any(x <= 0)
    if (hay_malos_valores) {
          warning("El vector tiene valores negativos o cero.")
      }

    aritmetica <- function(x) {
        return(sum(x) / length(x))
    }

    geometrica <- function(x) {
        if (hay_malos_valores) {
            return(NA)
        }
        return(prod(x)^(1 / length(x)))
    }

    armonica <- function(x) {
        if (hay_malos_valores) {
            return(NA)
        }
        return(length(x) / sum(1 / x))
    }


    return(list(aritm = aritmetica(x), geom = geometrica(x), arm = armonica(x)))
}

medias(c(1, 0))
medias(c(1, "d"))

medias(1:10)
medias(c(1:10, NA))
medias(0:10)
medias(-1:10)

# --------------------------------------------------------------------------------

naive_mediana <- function(x) {
    # vector => vector ordenado => punto medio del vector ordenado
    ordenado <- sort(x)
    if (length(x) %% 2 == 1) {
        ordenado[length(x) / 2 + 1]
    } else {
        mean(c(ordenado[length(x) / 2], ordenado[length(x) / 2 + 1]))
    }
}

mediana <- function(x, rm.na = TRUE) {
    if (rm.na) {
        x <- x[!is.na(x)]
    }
    if (!is.vector(x) || !is.numeric(x)) {
        stop("El argumento no es un vector numérico")
    }
    return(naive_mediana(x))
}

mediana(1:5)
mediana(1:6)
mediana(c(1:6, NA))
mediana("hola")
set.seed(1)
test <- runif(20)
sort(test)
mediana(test)

# --------------------------------------------------------------------------------
# Cuantiles WIP
cuartiles <- function(x, na.rm = T) {
    if (!is.numeric(x)) {
        stop("Argumento no es numérico.")
    }
    if (na.rm) {
        x <- x[!is.na(x)]
    }

    xord <- sort(x)
    n <- length(x)
    ## Q1 ocupa la posición (n+1)/4 (interpolando si es decimal)
    pos.Q1 <- (n + 1) / 4
    i <- trunc(pos.Q1)
    Q1 <- x[i] + (pos.Q1 - i) * (x[i + 1] - x[i])
    ## Q2 es la mediana
    if (n %% 2 == 0) {
          Q2 <- (xord[n / 2] + xord[1 + n / 2]) / 2
      } else {
        Q2 <- xord[ceiling(n / 2)]
    }
    ## Q3 ocupa la posición 3*(n+1)/4 (interpolando si es decimal)
    pos.Q3 <- 3 * (n + 1) / 4
    i <- trunc(pos.Q3)
    Q3 <- x[i] + (pos.Q3 - i) * (x[i + 1] - x[i])

    return(list(Q1 = Q1, Q2 = Q2, Q3 = Q3))
}

cuartiles(1:9)
cuartiles(1:10)
quantile(1:9,c(.25,.5,.75))
quantile(1:10,c(.25,.5,.75))
