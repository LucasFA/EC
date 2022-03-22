# Clase 08-03
# Factores
help("factor")
civil <- c(
    "soltero/a", "viudo/a", "casado/a", "soltero/a", "viudo/a", "divorciado/a",
    "soltero/a", "casado/a", "soltero/a", "divorciado/a"
)
civil.f <- factor(civil)
civil.f
class(civil)
class(civil.f)
attributes(civil)
attributes(civil.f)

# Ordenado
factor(civil, levels = c("soltero/a", "casado/a", "divorciado/a", "viudo/a"))
sexo.f <- factor(
    c(1, 1, 2, 1, 1, 2, 2, 1, 2, 1),
    labels = c("hombre", "mujer")
)
sexo.f
unclass(sexo.f)
unclass(civil.f)
# Acceder y modificar
civil.f[1]
civil.f[-(1:5)]
levels(sexo.f)
levels(sexo.f)[2]
levels(sexo.f) <- c("masculino", "femenino")
sexo.f
# Aplicaciones
edad <- c(23, 25, 20, 19, 20, 22, 24, 20, 23, 19)
mean(edad[sexo.f == "hombre"])
mean(edad[sexo.f == "mujer"])
# tapply
help("tapply")
tapply(edad, sexo.f, mean)
tapply(edad, sexo.f, sd)
tapply(edad, sexo.f, summary)
tapply(edad, civil.f, summary)
# plotting
boxplot(edad ~ sexo.f)
help("boxplot")

rm(list = ls())
# Segunda parte: matrices, arrays
# dim(x) <-
x <- 1:10
x
class(x)
attributes(x)
dim(x) <- c(2, 5)
attributes(x)
class(x)
# matrix
x <- 1:10
x <- matrix(1:10, nrow = 2, ncol = 5)
x <- matrix(1:10, nrow = 2, ncol = 5, byrow = TRUE)
# bind
cbind(1:3, 4:6, 7:9)
rbind(1:3, 4:6, 7:9)
x
cbind(x, c(2, 555))
rbind(x, 51:55)

A <- matrix(1:9, 3, 3)
rownames(A) <- c("fila.1", "fila.2", "fila.3")
A
class(A)
attributes(A)


A <- matrix(1:9, 3, 3)

A[, 1]
A[, -1]
A[-1, 1]
A[-1, -1]
A[1:2, 2:3]
A > 3

B <- A
B[A > 3] <- NA
B

# Ejercicio
# Matrix cuadrada dimension 3 runif
set.seed(1)
A <- matrix(runif(9), 3, 3)
# i)
matriz_bools <- A >= 0.5
matriz_bools
A[matriz_bools]
# ii)
sum(matriz_bools)
# iii)
B <- A
B
B[!matriz_bools] <- 0
B
# iv)
C <- A
C[col(A) < row(A)] <- NA

D <- A
D[col(A) > row(A)] <- NA
D


# Arrays
x <- 1:24
dim(x) <- c(3, 4, 2)
x
x <- array(1:24, dim = c(3, 4, 2))
attributes(x)
x[, , ]

# Operaciones con matrices
A <- matrix(c(0, 1, -1, 2), 2, 2)
A
# * es elemento a elemento
A * A

A %*% A
crossprod(A, A)
A <- matrix(c(3, 5, 2, 4, -2, -2, -1, 1, 1), 3, 3)
b <- c(8, 4, 1)
solve(A, b)

AA <- crossprod(A, A)
solve(AA)
chol2inv(chol(AA))

f <- function(x, y) 2 * y / (1 + x^2)
xx <- seq(0, 1, length.out = 5)
yy <- xx + 1
outer(xx, yy, f)