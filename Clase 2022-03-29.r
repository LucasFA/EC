# Clase 2022-03-29

# Funciones
rm(list = ls())

f.suma <- function(x = 0, y = 0) x + y
formals(f.suma)
body(f.suma)

# Modificar los argumentos
argum.original <- formals(f.suma) # almacenamos los argumentos originales
formals(f.suma) <- alist(x = , y = , z = ) # nolint
f.suma

# Modificar el cuerpo
cuerpo.original <- body(f.suma) # almacenamos el cuerpo original
body(f.suma) <- expression({
    suma1 <- x + y
    suma2 <- x + z
    return(list(suma1 = suma1, suma2 = suma2))
})
f.suma(1, 2, 3)

# reestablecemos argumentos y cuerpo originales
formals(f.suma) <- argum.original
body(f.suma) <- cuerpo.original
f.suma

environment(f.suma)

x <- 1
f <- function(x) {
    y <- 1
    x <- x + y
    return(x)
}
f(x)

x # cambia su valor solo dentro de la función

y # este objeto solo existe dentro de la función

class(var)
typeof(var)

formals(var)
body(var)
environment(var)
class(sum)
typeof(sum)

formals(sum)
body(sum)
environment(sum)
sum

class(`[`)
typeof(`[`)

lista <- list(x = 1:10, y = 10:13)
lapply(lista, function(v) v^2)

formals(paste)
paste("uno", "dos", "tres", sep = "+")

# Hay que tener en cuenta que cuando se usa el argumento ... en una función, el resto
# de argumentos que le siguen se deben pasar siempre por nombre y no por posición.

grafico <- function(x, ...) {
    z <- (x - mean(x)) / sd(x)
    plot(x, z, ...)
}
grafico(1:10, main = "Gráfico 1")

grafico(1:10, main = "Gráfico 2", type = "l", col = 4)

x <- 1
f <- function(y) {
    z <- x + y
    z
}

environment(f)
ls()
f1 <- function(a, b) a + b
f2 <- function(a, b) {
    f1 <- function(a, b) a - b
    f1(a, b)
}
f1(1, 1)

f2(1, 1)

y <- 10
f <- function(x) {
    y <- 2
    y^2 + g(x)
}
g <- function(x) x * y

f(3)

#
# --- ESTRUCTURAS DE CONTROL -----------------------------------------------------
#

# if
x <- 5
if (x >= 5) cat("Aprobado") else cat("Suspenso")

calif <- if (x >= 5) print("Aprobado") else print("Suspenso")

calif


saludo <- function(franja = NA, nombre) {
    if (is.na(franja)) {
        paste("Hola", nombre)
    } else if (franja == 1) {
        paste("Buenos días", nombre)
    } else if (franja == 2) {
        paste("Buenas tardes", nombre)
    } else if (franja == 3) {
        paste("Buenas noches", nombre)
    } else {
        warning("Franja puede ser 1 (mañana), 2 (tarde) ó 3 (noche)")
        paste("Hola", nombre)
    }
}

saludo(nombre = "Lola")
saludo(franja = 1, nombre = "Lola")
saludo(franja = 4, nombre = "Lola")

x <- 5
if (x > 4 | n > 1) "se comprueban las dos"

if (x > 4 || n > 1) "se comprueba sólo la primera"

raiz <- function(x) {
    if (!is.vector(x) || !is.numeric(x) || any(x < 0)) {
        stop("Argumento no válido")
    }
    sqrt(x)
}


# ifelse
# Observa que la condición x==0 se evalúa para cada elemento del vector x[i],
milog <- function(x) ifelse(x == 0, NA, log(x))
milog(c(1, 0, 2))
milog(c(1, 0, NA))

milog <- function(x) if (x == 0) NA else log(x)
milog(c(1, 0, 2))
Sys.setenv("_R_CHECK_LENGTH_1_CONDITION_" = "true")

# switch
opciones <- function(x) {
    if (x == "a") {
        "opción 1"
    } else if (x == "b") {
        "opción 2"
    } else if (x == "c") {
        "opción 3"
    } else {
        stop("Valor de x no válido")
    }
}

opciones("a")
# se compacta a

opciones <- function(x) {
    switch(x,
        a = "opción 1",
        b = "opción 2",
        c = "opción 3",
        stop("Valor de x no válido")
    )
}
opciones <- function(x) {
    switch(x,
        "uno",
        "dos",
        "tres",
        "cuatro"
    )
}

opciones(2)
opciones(1.4)
opciones(5)
opciones("a")

opciones <- function(x) {
    resultado <- c("uno", "dos", "tres", "cuatro")
    resultado[x]
}
opciones(2)
opciones(1.4)
opciones(5)
opciones("a")

# Traducir a switch
saludo <- function(franja = NA, nombre) {
    if (is.na(franja)) {
        paste("Hola", nombre)
    } else if (franja == 1) {
        paste("Buenos días", nombre)
    } else if (franja == 2) {
        paste("Buenas tardes", nombre)
    } else if (franja == 3) {
        paste("Buenas noches", nombre)
    } else {
        warning("Franja puede ser 1 (mañana), 2 (tarde) ó 3 (noche)")
        paste("Hola", nombre)
    }
}


saludo <- function(franja = NA, nombre) {
    switch(as.character(franja),
        "NA" = paste("Hola", nombre),
        "1"  = paste("Buenos días", nombre),
        "2"  = paste("Buenas tardes", nombre),
        "3"  = paste("Buenas noches", nombre),
        {
            warning("Franja puede ser 1 (mañana), 2 (tarde) ó 3 (noche)")
            paste("Hola", nombre)
        }
    )
}

saludo(1, "Lucas")
saludo(2, "Lucas")
saludo(3, "Lucas")
saludo(4, "Lucas")
saludo(111, "Lucas")
saludo("42", "Lucas")
saludo(c(NA, 1), "Lucas")
saludo(, "Lucas")
