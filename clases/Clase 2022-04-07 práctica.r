# Práctica de funciones II:
## Newton Raphson
rm(list = ls())
f <- function(x) x^2 - 5

f.prima <- function(x) {
    return(2 * x)
}

curve(f, 0, 10)
abline(h = 0, col = 2)

NR.paso <- function(x0) {
    x0 - f(x0) / f.prima(x0)
}

x0 <- c(2)
for (i in 2:5) {
    x0[[i]] <- NR.paso(x0[[i - 1]])
}
x0
# --------------------------------------------------------------------------------
# Ahora para otra función:
f <- function(x) {
    x^3 - 2 * x - 5
}
f.prima <- function(x) {
    3 * x^2 - 2
}
curve(f, -3, 3)
abline(h = 0)

NR.paso(3) # se acerca a la raíz
NR.paso(0) # se aleja significativamente de la raíz


# --------------------------------------------------------------------------------
rm(list = ls())

# Newton Raphson con el doble criterio de parada especificado (cuadro página 4)
algoritmo.NR <- function(f, f.prima, x0, tol = 1e-8, nmax = 50, dibuja = T) {
    if (missing(f) || !is.function(f)) {
        stop("No se ha proveído una función")
    }
    if (missing(f.prima) || !is.function(f.prima)) {
        stop("No se ha proveído la derivada o no es función")
    }
    if (!is.numeric(x0)) {
        stop("La semilla inicial no es de tipo numérico")
    }

    if (dibuja) {
        curve(f, 0, 10) # TODO: determinar en qué entorno debería mostrarse la gráfica (eg x^2 - 100 o x - 100)
        abline(h = 0, col = 2)
    }
    x1 <- x0
    i <- 0
    while (i < nmax) {
        x0 <- x1
        x1 <- x0 - f(x0) / f.prima(x0)
        i <- i + 1
        if (abs(x1 - x0) <= tol) {
            break
        }
    }
    if (i == nmax && abs(x1 - x0) > tol) {
        warning("Número máximo de iteraciones alcanzado. El método no converge")
    }
    # devuelve el error alcanzado
    return(list(aprox = x1, tol = abs(x1 - x0), niteraciones = i))
}
# --------------------------------------------------------------------------------
# Tests
f <- function(x) x^2 - 5
f.prima <- function(x) {
    return(2 * x)
}
algoritmo.NR(f, f.prima, 5)

f <- function(x) x^3 - 2 * x - 5
f.prima <- function(x) 3 * x^2 - 2
algoritmo.NR(f, f.prima, 5)

# --------------------------------------------------------------------------------
# 1.3. Implementación en R usando una aproximación numérica de la derivada

library(numDeriv)
f <- function(x) x^2 - 5
# derivada en x=2
res <- genD(func = f, x = 0)$D[[1]]
res
# --------------------------------------------------------------------------------

# Nota: podríamos haber sustituido el código anterior por este
algoritmo.NR <- function(f, f.prima, x0, tol = 1e-8, nmax = 50, dibuja = T) {
    if (missing(f) || !is.function(f)) {
        stop("No se ha proveído una función")
    }
    if (!is.numeric(x0)) {
        stop("La semilla inicial no es de tipo numérico")
    }

    if (missing(f.prima)) {
        f.prima <- function(p) {
            return(numDeriv::genD(func = f, x = p)$D[[1]])
        }
    }

    if (dibuja) {
        curve(f, 0, 10) # TODO: determinar en qué entorno debería mostrarse la gráfica (eg x^2 - 100)
        abline(h = 0, col = 2)
    }

    x1 <- x0
    i <- 0
    while (i < nmax) {
        x0 <- x1
        x1 <- x0 - f(x0) / f.prima(x0)
        i <- i + 1
        if (abs(x1 - x0) <= tol) {
            break
        }
    }
    if (i == nmax && abs(x1 - x0) > tol) {
        warning("Número máximo de iteraciones alcanzado. El método no converge")
    }
    return(list(aprox = x1, tol = abs(x1 - x0), niteraciones = i))
}

library(stats)
f <- function(x) x^2 - 5
res.NR <- algoritmo.NR(f, x0 = 5, dibuja = T)
res.uni <- uniroot(f, interval = c(0, 10))
res.NR$aprox - res.uni$root

f <- function(x) x^3 - 2 * x - 5
res.NR <- algoritmo.NR(f, x0 = 20, dibuja = T)
res.uni <- uniroot(f, interval = c(0, 10))
res.NR$aprox - res.uni$root

f <- function(x) exp(2 * x) - x - 6
res.NR <- algoritmo.NR(f, x0 = 10, dibuja = T)
# curve(f, 0, 2)
# abline(h = 0) # la imagen producida por algoritmo.NR es apenas legible
res.uni <- uniroot(f, interval = c(0, 10))
res.NR$aprox - res.uni$root

#
# --- EJERCICIO PROPUESTO 2 ------------------------------------------------------
#


dif.eq <- function(x1, r, n) {
    x <- vector()
    x[[1]] <- x1
    for (i in 2:n) {
        x[[i]] <- r * x[[i - 1]] * (1 - x[[i - 1]])
    }
    return(x)
}
# a
res <- dif.eq(r = 2, x1 = 0.1, n = 20)
res
# --------------------------------------------------------------------------------
# b
res <- dif.eq(r = 2.99, x1 = 0.95, n = 500)
res
plot(1:500, res)
# ────────────────────────────────────────────────────────────────────────────────
# c
dif.eq2 <- function(x1, r) {
    formula <- function(x) {
        r * x * (1 - x)
    }
    x <- vector()
    x[[1]] <- x1
    x[[2]] <- formula(x[[1]])

    i <- 2
    while (abs(x[[i]] - x[[i - 1]]) >= 0.02) {
        i <- i + 1
        x[[i]] <- formula(x[[i - 1]])
    }
    return(list(sucesion = x, iteraciones = length(x) - 1))
}

dif.eq2(x1 = 0.95, r = 2.99)
