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
y <- c(15, 18, 10, 12, 16)
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