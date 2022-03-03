# Práctica 03-03
rm(list = ls())
ls()

# Ejercicio 1
x <- seq(1, 10, by = 0.2)
n <- length(x) # a
names(x) <- paste("x_", 1:n, ) # b
mx <- mean(x) # c
sum(x > mx) # d número de elementos de x mayores que mx
which.min(x > mx) # e
y <- seq(1, by = 2, length.out = 15)
y # f. n primeros números impares
x[y[1:5]] # g

rm(list = ls())
# Ejercicio 2
x <- seq(-2, 2, by = 0.1)
x

(x < -1) * 1 +
    (-1 <= x & x < 0) * log(x^2) +
    (0 <= x & x < 1) * log(x^2 + 1) +
    (1 <= x) * 2

rm(list = ls())
# Ejercicio 3
set.seed(1)
x <- runif(50)
# a
sum(x < 0.25 & x < 0.75)
# b
outlier_flag <- (x < 0.1 | 0.9 < x)
outlier_flag
sum(outlier_flag)
outlier_indexes <- which(outlier_flag)
outlier_indexes
x[which(outlier_flag)] <- NA
mean(x)
# c
x[outlier_indexes] <- 0 # Alternativamente, x[which(is.na(x))] <- 0
mean(x)
# En el apartado anterior: NA. Ahora: 0.45503...

rm(list = ls())
# Ejercicio 4
d <- 1.2
n <- 1:20
# La sucesión dada es a_n = a_1 + (n - 1)*d
# esta se simplifica a a_(n+1) = a_n + d
a <- seq(1, by = d, length.out = 20)
sum(a)
20 * (a[1] + a[20]) / 2
## Ambos con resultado 248
# b


rm(list = ls())
# Ejercicio 5
x <- c(2, 2, 8, 7, 6, 1, 5)
y <- seq()
