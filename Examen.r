
#
# --- EJERCICIO 1 ----------------------------------------------------------------
#


x <- seq(from = 1, by = 2, length.out = 50)

mx <- mean(x)
sx <- sd(x)

# --------------------------------------------------------------------------------
# b)

sum(abs(x - mx) > sx)

# --------------------------------------------------------------------------------
# c)
x[abs(x - mx) > sx] <- NA
x
# --------------------------------------------------------------------------------
# d)
# De los que no sean NA, cuenta los que tengan módulo 3 == 0 (eso es, divisibles por 3). Contarlos
sum(x[!is.na(x)] %% 3 == 0)


#
# --- EJERCICIO 2 ----------------------------------------------------------------
#

# 1
data(airquality, package = "datasets") # Aunque por defecto ya lo tengo cargado. Por compatibilidad
aire <- airquality
class(aire)
str(aire)

# --------------------------------------------------------------------------------
# 2
# Primero devuelve la lista de vectores lógicos que contiene si un valor es un NA
# y suma cada columna (ie elemento de la lista)
lapply(lapply(aire, is.na), sum)

# --------------------------------------------------------------------------------
# 3
# Vamos a eliminar
sum(!complete.cases(aire))
# filas:

aire <- aire[complete.cases(aire), ]

# --------------------------------------------------------------------------------
# 4

aire$Month <- factor(aire$Month, labels = month.name[5:9])

# --------------------------------------------------------------------------------
# 5
aggregate(aire[c("Wind", "Ozone")], list(aire$Month), median)

# --------------------------------------------------------------------------------
# 6

aire.mayo <- aire[aire$Month == "May", ]

#
# --- EJERCICIO 3 ----------------------------------------------------------------
#

progresion.geometrica <- function(n, a1, r) {
    if (is.na(n) || !is.numeric(n) || n <= 0 || n != trunc(n)) {
        stop("Argumento n no es un entero positivo")
    }
    if (is.na(a1) || !is.numeric(a1)) {
        stop("El primer término de la sucesión debe ser un valor numérico")
    }
    if (is.na(r) || !is.numeric(r)) {
        stop("El cociente (r) entre los términos debe ser un valor numérico")
    }

    v <- a1 * r^(0:(n - 1)) # Nota: el primer valor es con n= 0 ==> r = 1 ==> v[[1]] == a1

    calcularprod2 <- function() {
        if (a1 > 0 && r > 0) {
            return(sqrt(a1^2 * r^(n - 1))^n)
        } else {
            return(NA)
        }
    }
    ret <-
        list(
            v = v,
            suma1 = sum(v),
            suma2 = a1 * (1 - r^n) / (1 - r),
            producto1 = prod(v),
            producto2 = calcularprod2()
        )
    return(ret)
}

progresion.geometrica(20, 2, -0.5)
progresion.geometrica(20, 2, 0.5)
