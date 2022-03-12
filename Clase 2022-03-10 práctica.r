rm(list = ls())
# Práctica 10-03
A <- matrix(1:9, 3, 3)
x <- 1:3

A %*% x
# Vector 3x1. Producto matriz*vector usual
A %*% t(x)
# No funciona. Lo que esperarías por las dimensiones.
# Esto lo indica el mensaje de error
x %*% A
# No esperarías que funcionara. Devuelve un vector 1x3 igual que
t(x) %*% A
# que es igual, como esperarías, a
t(t(A) %*% x) ## Instrucción no copiada del documento
# En resumen, x %*% A realmente ejecuta t(x) %*% A
t(x) %*% x
# Devuelve el producto escalar de x, lo que esperarías.


## Sistemas de ecuaciones ---------------------
# Sistema 1
solve(2, 2)
# Resuelve 2*x = 2
# Sistema 2
A <- matrix(c(3, 1, 4, 2), 2, 2)
b <- c(12, 8)
solve(A, b)
# Resuelve A*x=b
# Sistema 3
solve(A, diag(2))
# resuelve A * X = Id_2 # (diag(2) = Id_2)


rm(list = ls())
# Apartado 2.1
A <- matrix(c(10, 7, 8, 7, 7, 5, 6, 5, 8, 6, 10, 9, 7, 5, 9, 10), 4, 4)
b <- c(32, 23, 33, 31)

solve(A, b)
solve(A, b + 0.05)
solve(A, b + 0.1)
# Los resultados cambian significativamente de uno al otro para un cambio tan pequeño

# Número de condición
kappa_aprox <- kappa(A)
# > 3341.215
kappa_exacto <- kappa(A, exact = T)
# > 2984.093
rcond_funcion <- rcond(A)

valores_propios <- eigen(A)$values
k <- max(valores_propios) / min(valores_propios)
k - kappa_exacto
# < 10^(-9)
kinv <- 1 / k
kinv - rcond_funcion
# < 0. 0002
all.equal(kinv, rcond_funcion)
# No son exactos. Más o menos
# Sin embargo,
1 / kappa_aprox - rcond_funcion
# tiene menor resultado todavía.
# Recordamos que rcond también utiliza una aproximación


rm(list = ls())

#
# ─── REGRESION LINEAL ───────────────────────────────────────────────────────────
#
set.seed(2)
## Computación directa
reg_lineal <- function(x, y) {
    X <- cbind(1, x)

    temp <- t(X) %*% X
    inversade_Xtraspuesta_X <- solve(temp)

    beta_gorro <- inversade_Xtraspuesta_X %*% t(X) %*% y
    # > 1.045, 0.947
    # Efectivamente el modelo se asemeja al esperado, con beta = (1, 1)

    curve(1 + x, -3, 3) # Modelo real
    points(x, y)
    curve(beta_gorro[1] + beta_gorro[2] * x, -3, 3, add = T, col = 2)
    beta_gorro
}

generate_data <- function(n) {
    x <- rnorm(n)
    y <- 1 + x + rnorm(n, 0, 0.1)
    l <- list("x" = x, "y" = y)
    l
}
data_5 <- generate_data(5)
x_5 <- data_5$x
y_5 <- data_5$y

beta_gorro <- reg_lineal(x_5, y_5)

data_50 <- generate_data(50)
x_50 <- data_50$x
y_50 <- data_50$y

reg_lineal(x_50, y_50)

data_500 <- generate_data(500)
x_500 <- data_500$x
y_500 <- data_500$y
reg_lineal(x_500, y_500)
# Cuantas más muestras tomes, el modelo se acerca más.
# Con n = 50, ya se acerca significativamente, pero esto
# debería medirse con medidas como R^2

# Problemas prácticos:
#
#
#
# ────────────────────────────────────────────────────────────────────────────────
# Descomposición QR
x <- x_5
y <- y_5

X <- cbind(1, x)
qr_descomp <- qr(X)
Q <- qr.Q(qr_descomp)
b <- t(Q) %*% y

R <- qr.R(qr_descomp)

qr_beta_gorro <- backsolve(R, b)
qr_beta_gorro - beta_gorro
# Son esencialmente idénticos. Difieren en menos de 10^(-15) en cada término
lm(y ~ x)

# ────────────────────────────────────────────────────────────────────────────────
# Ejercicio propuesto: 4.
# ────────────────────────────────────────────────────────────────────────────────

rm(list = ls())

mi_matriz <- function(n) {
    m <- matrix(nrow = n, ncol = n)
    for (j in 1:n) {
        m[, j] <- (1:n)^j
    }
    m
}

f_error <- function(x, sol_real) {
    max(abs(x - sol_real))
}

ejercicio_para_n <- function(n) {
    sol_premeditada <- rep(1, times = n)
    A <- mi_matriz(n)
    b <- A %*% sol_premeditada
    x <- solve(A, b)


    error <- f_error(x, sol_premeditada)
    cat(
        "El error absoluto máximo para", n,
        "es de", error, "\n"
    )
    condicionamiento <- rcond(A)
    cat("El condicionamiento de la matriz es", condicionamiento, "\n\n")
}

for (i in 3:12) {
    ejercicio_para_n(i)
}

# Resumen de los resultados:
# El condicionamiento de la matrix ya empieza en 0.0046 para n=3, y decrece en n.
# Tanto para n = 3 como n = 4, el error reportado es nulo,
# pero a partir de n = 5, con el recíproco del condicionamiento 10^(-5), el error alcanza 10^(-12),
# creciendo hasta 6*10^(-4) para n = 11, con condicionamiento 10^(-15),
# y para n = 12 solve() devuelve un error indicando que el sistema es, prácticamente, singular