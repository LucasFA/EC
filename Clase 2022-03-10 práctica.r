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
# Regresión lineal
## Computación directa
reg_lineal <- function(sample_size=5, seed=2, show_points = T){
    n <- sample_size
    set.seed(seed)
    x <- rnorm(n)
    y <- 1 + x + rnorm(n, 0, 0.1)
    X_mat_regresion <- cbind(1, x)

    temp <- t(X_mat_regresion) %*% X_mat_regresion
    inversade_Xtraspuesta_X <- solve(temp)

    beta_gorro <- inversade_Xtraspuesta_X %*% t(X_mat_regresion) %*% y
    # > 1.045, 0.947
    # Efectivamente el modelo se asemeja al esperado, con beta = (1, 1)

    curve(1 + x, -3, 3)
    if(show_points){
        points(x, y)
    }
    curve(beta_gorro[1] + beta_gorro[2] * x, -3, 3, add = T, col = 2)
}
reg_lineal(5)
reg_lineal(50)
reg_lineal(500)
# Cuantas más muestras tomes, el modelo se acerca más.
# Con n = 50, ya se acerca significativamente, pero esto
# debería medirse con medidas como R^2
