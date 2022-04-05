# Estructuras de repetición

# --------------------------------------------------------------------------------
# for
for (i in 1:3) print(i^2)

for (letra in letters[1:3]) print(letra)

lista <- list(matrix(1:6, 2, 3), 1:2)
for (i in lista) print(i)

# El ciclo se puede interrumpir en cualquier momento usando break, next o return.
# Esta última opción está asociada a funciones.

for (i in 1:5) if (i %% 2 == 0) print(i) else next

set.seed(1)
x <- runif(100)
suma <- 0

for (i in 1:100) {
    suma <- suma + x[i]
    if (suma > 0.5) {
        print(paste("Me paro en i=", i, "porque la suma supera 0.5"))
        break
    }
}

f <- function(x) {
    suma <- 0
    for (i in 1:length(x)) {
        suma <- suma + x[i]
        if (suma > 0.5) {
            return(paste("Me paro en i=", i, "porque la suma supera 0.5"))
        }
    }
    return(paste("He completado el ciclo y la suma es", suma))
}

f(rep(0.1, 4))
f(rep(0.1, 10))

# --------------------------------------------------------------------------------
# while

set.seed(1)
i <- suma <- 0
x <- numeric()
while (suma < 0.5) {
    i <- i + 1
    x[i] <- runif(1, 0, 0.1) # uniforme en (0,0.1)
    suma <- suma + x[i]
}
print(round(x, 4))
# --------------------------------------------------------------------------------

# repeat: bucle infinito
set.seed(1)
i <- suma <- 0
x <- numeric()
repeat{
    i <- i + 1
    x[i] <- runif(1, 0, 0.1) # uniforme en (0,0.1)
    suma <- suma + x[i]
    if (suma >= 0.5) break
}
print(round(x, 4))

# --------------------------------------------------------------------------------
# Equivalencia entre bucles

# Es posible reescribir cualquier ciclo for usando while. También, cualquier ciclo while
# se puede escribir usando repeat. Esto signica que while es más exible que for, y que
# repeat es más exible que while. Una buena práctica en la programación en R es usar
# siempre la solución menos exible a un problema, lo que nos lleva a elegir for siempre
# que sea posible.

# La misma regla anterior nos llevará a evitar ciclos for cuando podamos resolver el
# problema usando funciones vectoriales, o adecuadas para la estructura de datos concreta
# que estemos trabajando

# 1
x <- 1:5
suma <- 0
for (i in seq_along(x)) suma <- suma + x[i]
suma / length(x)

# 2. En R es más conveniente y seguro
sum(x) / length(x)

# Otro ejemplo:
A <- matrix(1:6, 2, 3)
sumas.filas <- numeric(nrow(A))
for (i in 1:nrow(A)) {
    for (j in 1:ncol(A)) sumas.filas[i] <- sumas.filas[i] + A[i, j]
}
sumas.filas

# Alternativa mejorada:
rowSums(A)

# --------------------------------------------------------------------------------
# La misma idea:
#
# --- FAMILIA APPLY --------------------------------------------------------------
#
# Funciones lapply y sapply

lista <- list(v1 = 1:10, v2 = factor(), v3 = letters[1:4])
lapply(lista, length)

df <- data.frame(matrix(1:6, 2, 3))
lapply(df, mean)

sapply(lista, length)

# Función apply
# MARGIN especicaría la
# dimensión donde vamos a operar. Si X es una matriz entonces MARGIN=1 operaría por filas,
# MARGIN=2 por columnas, y MARGIN=c(1,2) lo haría por filas y columnas.

A <- cbind(1:4, seq(0, 1, length.out = 4), (1:4)^2)
apply(A, 1, median) # mediana por filas

apply(A, 2, var) # cuasivarianza por columnas
apply(A, c(1, 2), sqrt)


# --------------------------------------------------------------------------------
# Función split

# Esta función permite clasicar datos consistentes en vectores, matrices o data frames
# de acuerdo a un criterio de clasicación.
help("split")

Orange
arboles <- split(Orange, Orange$Tree)
lapply(arboles, head, n = 2)


# Como segundo ejemplo consideramos la tarea consistente primero en obtener y mostrar
# las diagonales de una matriz cuadrada, y después calcular sus sumas.
A <- matrix(1:9, 3, 3)
diagonales <- split(A, col(A) + row(A)) # TODO: add to cheatsheet
diagonales
sapply(diagonales, sum)

# --------------------------------------------------------------------------------
# Funciones de clasificación y agrupación: by, aggregate y tapply

aggregate(Orange$age, by = list(Orange$Tree), summary)

by(Orange$age, Orange$Tree, summary)

tapply(Orange$age, Orange$Tree, summary)

# --------------------------------------------------------------------------------
# Recursividad
# Una función pueda hacer referencia a la propia función.
# Esto permite crear estructuras definidas fácilmente por recursividad.
# Consume muchos recursos por lo que deben siempre valorarse otras alternativas.

# --------------------------------------------------------------------------------

log.natural <- function(x) {
    if (missing(x) || !is.numeric(x)) {
        stop("Debe proporcionar un argumento 'x' numérico")
    } else if (any(x <= 0)) {
        x <- x[x > 0]
        warning("Se han eliminado los valores negativos o cero")
    }
    return(log(x))
}

log.natural()
log.natural(-1:5)


#
# --- DEPURACION DE ERRORES: DEBUGGING -------------------------------------------

# traceback, browser y debug.
# --------------------------------------------------------------------------------
# traceback

f <- function(x) {
    n <- round(x)
    set.seed(n)
    u <- runif(n)
    return(u)
}

mean(f(-2))
traceback()

# --------------------------------------------------------------------------------
# browser

f <- function(x) {
    browser()
    n <- round(x)
    set.seed(n)
    u <- runif(n)
    return(u)
}

mean(f(-2))

# --------------------------------------------------------------------------------
# debug

f <- function(x) {
    n <- round(x)
    set.seed(n)
    u <- runif(n)
    return(u)
}

debug(f)
debug(mean)
mean(f(2))
undebug(f)
undebug(mean)

# debugonce es debug pero sin necesidad de undebug: sólo se hace interactivo una vez


factorial.d <- function(n) {
    if (missing(n) || !is.numeric(n)) {
        return(NA)
    }
    n <- as.integer(n)
    fact <- 1
    if (n > 1) for (i in 1:n) fact <- fact * i
    return(fact)
}
factorial.d(5)
factorial.d(0)
factorial.d(100)
factorial.d(5000)

factorial.r <- function(n) {
    if (missing(n) || !is.numeric(n)) {
        return(NA)
    }
    n <- as.integer(n)
    if (n > 1) fact <- n * factorial.r(n - 1) else fact <- 1
    return(fact)
}
factorial.r(5)
factorial.r(100)
factorial.r(5000)
factorial.v <- function(n) {
    if (missing(n) || !is.numeric(n)) {
        return(NA)
    }
    n <- as.integer(n)
    if (n > 1) fact <- prod(n:1) else fact <- 1
    return(fact)
}
factorial.v(5)
factorial.v(100)
factorial.v(5000)

# --------------------------------------------------------------------------------

# benchmarking

factorial.d <- function(n) {
    if (missing(n) || !is.numeric(n)) {
        return(NA)
    }
    n <- as.integer(n)
    t1 <- Sys.time() # tiempo inicial
    fact <- 1
    if (n > 1) for (i in 1:n) fact <- fact * i
    t2 <- Sys.time() # tiempo final
    return(list(factorial = fact, tiempo = t2 - t1))
}

factorial.v <- function(n) {
    if (missing(n) || !is.numeric(n)) {
        return(NA)
    }
    n <- as.integer(n)
    t1 <- Sys.time() # tiempo inicial
    if (n > 1) fact <- prod(n:1) else fact <- 1
    t2 <- Sys.time() # tiempo final
    return(list(factorial = fact, tiempo = t2 - t1))
}

factorial.d(10000000)
factorial.v(10000000)
#
# --- SUCESION DE FIBONNACI ------------------------------------------------------
#

Fibonacci.r <- function(n) {
    if (missing(n) || !is.numeric(n)) {
        return(NA)
    }
    n <- as.integer(n)
    if (n == 0) {
        return(0)
    } else if (n <= 2) {
        return(1)
    } else {
        return(Fibonacci.r(n - 1) + Fibonacci.r(n - 2))
    }
}

Fibonacci.r(5)
Fibonacci.r(20)

Fibonacci.v <- function(n) {
    if (missing(n) || !is.numeric(n)) {
        return(NA)
    }
    n <- as.integer(n)
    v <- integer(n)
    v[1:2] <- 1
    for (i in 3:n) v[i] <- v[i - 1] + v[i - 2]
    return(v[n])
}

Fibonacci.v(5)

# benchmarking
Fibonacci.r(200)
