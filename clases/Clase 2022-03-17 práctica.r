# Clase 2022-03-17
rm(list = ls())

#
# ─── EJERCICIO 1 ────────────────────────────────────────────────────────────────
#

# a
l <- list(1:5, 2:6, 3:7)
names(l) <- c("x1", "x2", "x3")

x <- runif(10)
l[[4]] <- x

# --------------------------------------------------------------------------------

# b
y <- rnorm(10)
l[[5]] <- y

# c

suma_l <- lapply(l, FUN = sum)
suma_l
class(suma_l)

suma_s <- sapply(l, FUN = sum)
suma_s
class(suma_s)

# lapply devuelve una lista mientras suma_s devuelve un vector (numérico)

# --------------------------------------------------------------------------------

# d
reg <- lm(y ~ x)
typeof(reg)
# list. También vale
is.list(reg)

# e
lapply(reg, class)
lapply(reg, typeof)

# f
matriz <- cbind(reg$residuals, reg$fitted.values, x, y)
dim(matriz) <- c(10, 4)
matriz

colnames(matriz) <- c("residuos", "valores ajustados", "x", "y")


#
# --- EJERCICIO 2 ----------------------------------------------------------------
#
rm(list = ls())
x <- c(1.2, 1.8, 2.2, 2.5, 1.1)
y <- c(15, 18, 10, 12, 6)
n <- c(12, 23, 5, 9, 11)

datos <- data.frame(x, y, n)
rm(list = c("x", "y", "n"))
# a
sample_size <- sum(datos[["n"]])

# b
media_x <- sum(datos[["x"]] * datos[["n"]] / sample_size)
media_y <- sum(datos[["y"]] * datos[["n"]] / sample_size)

q_var_x <- sum((datos[["x"]] - media_x)^2 * datos[["n"]])   / (sample_size - 1)
q_var_y <- sum((datos[["y"]] - media_y)^2 * datos[["n"]])   / (sample_size - 1)


# c
datos.n <- datos[rep(seq_len(nrow(datos)), datos$n), 1:2]
# lo mismo, pero más conciso que,
datos.n <-
    data.frame(
        cbind(
            cbind(
                rep(
                    datos[["x"]],
                    times = datos[["n"]]
                )
            ),
            cbind(
                rep(
                    datos[["y"]],
                    times = datos[["n"]]
                )
            )
        )
    )

colnames(datos.n) <- c("x", "y")

# d
all.equal(mean(datos.n[["x"]]), media_x)
all.equal(mean(datos.n[["y"]]), media_y)

all.equal(var(datos.n[["x"]]), q_var_x)
all.equal(var(datos.n[["y"]]), q_var_y)

# Efectivamente, son iguales.
# --------------------------------------------------------------------------------
# e

datos.n <- transform(
    datos.n,
    tipif_x = (x - media_x) / sqrt(q_var_x),
    tipif_y = (y - media_y) / sqrt(q_var_y)
)

datos.n <- within(datos.n, {
    tipif_x <- (x - media_x) / sqrt(q_var_x)
    tipif_y <- (y - media_y) / sqrt(q_var_y)
    }
)

#
# --- EJERCICIO 3 ----------------------------------------------------------------
#
rm(list = ls())
df <- ChickWeight
# a
head(df, 5)
tail(df, 3)

# b
str(df)
# c
summary(df)

# d
peso.dieta <- tapply(df$weight, INDEX = list(df$Diet), summary)
class(peso.dieta)
# > Array
mode(peso.dieta)
typeof(peso.dieta)
peso.dieta

# e
peso.dieta.2 <- data.frame(
    matrix(
        unlist(peso.dieta),
        byrow = T, nrow = length(peso.dieta), ncol = length(peso.dieta[[1]])
    )
)

colnames(peso.dieta.2) <- names(peso.dieta[[1]])
peso.dieta.2

# f
peso.dieta.2.2 <- aggregate(df$weight, by = list(df$Diet), summary)
peso.dieta.2.2
class(peso.dieta.2.2)

# g
set.seed(77)
Chick100 <- ChickWeight[sample(seq_len(nrow(ChickWeight)), size = 100), ]
# h
Chick100[sample(seq_len(nrow(Chick100))), ]
# bueno, bien, pero era permutar las columnas
Chick100[, sample(seq_len(ncol(Chick100)))]
# i
Chick100[order(names(Chick100))]

# j
Chick100[order(Chick100$Diet), ]
Chick100[order(Chick100$Diet, Chick100$weight), ]

# k

Chick100.ordenado <- Chick100[order(Chick100$weight, decreasing = T), ]
Chick100.ordenado[!duplicated(Chick100.ordenado$Diet), ]
