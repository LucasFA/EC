# Práctica 03-03

rm(list = ls())
# Ejercicio 1 --------------------------------------------------------------
x <- seq(1, 10, by = 0.2)
# a
n <- length(x)

# b
names(x) <- paste("x_", 1:n, sep = "") 
x
# c
mx <- mean(x)

# d número de elementos de x mayores que mx
sum(x > mx)

# e
which.min(x > mx)
y <- seq(1, by = 2, length.out = 15)
y 
# f) n primeros números impares
x[y[1:5]] # g


rm(list = ls())
# Ejercicio 2 --------------------------------------------------------------
x <- seq(-2, 2, by = 0.1)
x

(x < -1)           * 1 +
(-1 <= x & x < 0)  * log(x^2) +
(0 <= x & x < 1)   * log(x^2 + 1) +
(1 <= x)           * 2


rm(list = ls())
# Ejercicio 3 ---------------------------------------------------------------
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
# Ejercicio 4 -----------------------------------------------------------------
d <- 1.2
longit <- 20
n <- 1:longit
# La sucesión dada es a_n = a_1 + (n - 1) * d
# esta se simplifica a a_(n+1) = a_n + d
# Fuente: Andrés. Como alternativa: a <- a_1 +((1:20) - 1) * d
a <- seq(1, by = d, length.out = longit)
s <- sum(a)
s == longit * (a[1] + a[longit]) / 2
# Nota: por defecto R usa doubles para numéricos

# A partir de aquí ya lo hice fuera, pero por completarlo:
# b
std_dev <- sd(a)
std_dev == abs(d) * sqrt(longit * (longit + 1) / 12)

# c
p <- prod(a)
all.equal(p, d^longit * gamma(a[1] / d + longit) / gamma(a[1] / d))


rm(list = ls())
# Ejercicio 5 -----------------------------------------------------------
x <- c(2, 2, 8, 7, 6, 1, 5)
y <- x[2:length(x)] - x[1:(length(x) - 1)]
y

rm(list = ls())
# Ejercicio 6 ---------------------------------------------------------
ABE <- LETTERS
ABE.5 <- sample(ABE, 5, replace = FALSE)
ABE.5

random_word <- function(word_vector = LETTERS) {
    paste(sample(word_vector), collapse = "")
}

PAL <- c(
    random_word(ABE.5),
    random_word(ABE.5)
)
PAL
