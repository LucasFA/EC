#
# --- EJERCICIOS DE REPASO DEL TEMA 2 Y 3 ----------------------------------------
#
rm(list = ls())
# Ejercicio 1

set.seed(1)
x <- runif(100)

mx <- mean(x)
minIndex <- which.min(abs(x - mx))
x[minIndex]
sum(x < mx)

x[!(x < mx)]
A <- cbind(x, abs(x - mx))
A

# --------------------------------------------------------------------------------
# Ejercicio 2
rm(list = ls())
hatco <- read.table("./inputfiles/hatco.txt", header = T)[]
# # Eliminar la primera columna, que es meramente un índice asociado
# hatco <- hatco[2:ncol(hatco)]
hatco$x8 <- factor(hatco$x8, labels = c("Pequeña", "Grande"))
# 3
hatco$cliente <- as.character(hatco$cliente)

str(hatco)

sum(hatco$x8 == "Pequeña") # Pequeña
sum(hatco$x8 == "Grande") # Grande
hatco$x8

hatco.pequeña <- subset(hatco, x8 == "Pequeña")
hatco.grandes <- subset(hatco, x8 == "Grande")
hatco.pequeña[sample(seq_len(nrow(hatco.pequeña)), 1), ]
hatco.grandes[sample(seq_len(nrow(hatco.grandes)), 1), ]

# --------------------------------------------------------------------------------
# 5

fidelidad.media <- mean(hatco$y)
fidelidad.media.pequeña <- mean(hatco.pequeña$y)
fidelidad.media.grandes <- mean(hatco.grandes$y)

# --------------------------------------------------------------------------------
# 6

sum(hatco.pequeña$y > fidelidad.media.pequeña)
sum(hatco.grandes$y > fidelidad.media.grandes)
# --------------------------------------------------------------------------------
# 7
str(hatco)
hatco2 <- hatco
numeric.cols <- colnames(Filter(is.numeric, hatco))
numeric.cols
hatco2[numeric.cols] <- sapply(hatco2[numeric.cols], scale)
head(hatco2)


#
# --- EJERCICIO 3 ----------------------------------------------------------------
#

progresion_aritmetica <- function(n, a1, d) {
    a <- a1 + d * (seq_len(n) - 1)
    suma <- sum(a)
    producto <- prod(a)

    return(list(v = a, suma = suma, producto = producto))
}

progresion_aritmetica2 <- function(n, a1, d, explicit = FALSE) {
    a <- a1 + d * (seq_len(n) - 1)
    if (!explicit) {
        suma <- sum(a)
        producto <- prod(a)

        return(list(v = a, suma = suma, producto = producto))
    } else {
        return(
            list(
                v = a,
                suma = n * (a[[1]] + a[[n]]) / 2,
                producto = d^n * gamma(a[[1]] / d + n + 1) / gamma(a[[1]] / d + 1)
            )
        )
    }
}
