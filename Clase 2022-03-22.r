# Clase 2022-03-22

#
# --- LEER FICHEROS --------------------------------------------------------------
#

datos <- read.table(file = "datos.txt", header = T, as.is = c(1))
datos
lapply(datos, class)
str(datos)
datos <- read.table(file = "datos_comas.csv", header = T, sep = ",")
datos <- read.csv(file = "datos_comas.csv", header = T)
# read.csv2() para csv separado por ";"
datos
datos <- scan(
    file = "datos.txt",
    skip = 1,
    what = list(
        DNI = "", edad = 0, sexo = "",
        estudios = "", salario = 0
    )
)
str(datos)
lapply(datos, class)
as.data.frame(datos)
lapply(as.data.frame(datos), class)

# Leer de terminal
datos2 <- scan()


rm(list = ls())
data(cars)
data(package = "boot")
data(coal, package = "boot")
data(package = "datasets")
data()
ls()
ls("package:datasets")

# --------------------------------------------------------------------------------
# Attach, detach

mean(datos$Salario)
attach(datos)
search()
mean(Salario)
detach(datos)
search()


# --------------------------------------------------------------------------------
# Escribir a ficheros

write.table(cars[1:10, ], file = "outputfiles/datos1.txt")
datos1 <- read.table("datos1.txt", header = TRUE)

write(runif(10), file = "outputfiles/datos2.txt", ncolumns = 1)
write(t(as.matrix(datos1)), file = "outputfiles/datos3.txt", ncolumns = 2)

write(cars[1:10, ], file = "outputfiles/datos3.txt")
help("write")


#
# --- FUNCIONES ------------------------------------------------------------------
#

f.suma <- function(x, y) x + y
f.suma
f.suma(1:10, 2)
f.suma(diag(3), 1:3)

f.suma <- function(x = 0, y = 0) x + y

f2 <- function(x, y) {
    x^2
    return(list(suma = x + y, resta = x - y))
    y^2
}
f2(5, 2)

media1 <- function(x) {
    if (!is.vector(x)) {
        stop("x no es un vector")
    }
    if (!is.numeric(x)) {
        stop("x no es un vector numérico")
    }
    num_na <- sum(is.na(x))
    media <- sum(x, na.rm = T) / (length(x) - num_na)

    c(media, num_na)
}
# Test
media1(c())
media1(c(NA))
media1(c(NA, 1, 3))
media1(list("o", 1, 3))

media2 <- function(x, n = 1) {
    if(length(x) != length(n)) {
        message("Los argumentos no tienen la misma longitud. Se aplicará reciclaje")
    }
    n <- rep(n, length.out = length(x))

    if (!(is.vector(n) && is.numeric(n))) {
        stop("El argumento n no es un vector numérico")
    }
    if (!(is.vector(x) && is.numeric(x))) {
        stop("El argumento x no es un vector numérico")
    }
    if (any(n < 0)) {
        warning("El argumento n tiene valores negativos")
    }

    tam_muestra <- sum(n)
    media <- drop(x %*% n) / tam_muestra

    list(media = media, tam_muestra = tam_muestra)
}
#--------------------------------------------------------------------------------
# Test
media2(list(0, 3), c(1, 2))             # not a vector
media2(unlist(list(0, 3)), c(1, 2))     # a vector
media2(c(0, 3), list(1, 1.777))

media2(c(0, 3), c(1, 1.777))
media2(c(0, 1, 5), c(3, 1))
media2(c(1, 2), c(-11, 1))

# sum(n) = 0 lleva a valores +- inf o NaN
media2(c(1, 0), c(-1, 1))
media2(c(0, 1), c(-1, 1))

media2(c(1, 1), c(-1, 1))
media2(c(-1, -1), c(-1, 1))

media2(c(3), c(2))
media2(c(), c())

x <- runif(20)
n <- sample(1:20)
all.equal(media2(x, n)$media, weighted.mean(x, n))
