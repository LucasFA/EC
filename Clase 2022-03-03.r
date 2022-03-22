# Clases
x <- 1:7
y <- x > 3 & x < 5
y

paste(c("X", "Y"), 1:5, sep = "")
names(x) <- paste(c("x"), 1:length(x), sep = "")
x
nth <- paste0(1:10, c("st", "nd", "rd", rep("th", 7)))
nth

LETTERS[1:20]
month.name[1:14]

x <- seq(0, 1, length.out = 10)
x[c(2, 4)]
x[1:7]
x[seq(1, 10, by = 2)]
x
x[-c(2, 5)] # x quitando -c(...)


x <- -5:5
x > 0
x[x > 0]

peso <- c(60, 75, 56, 70)
sexo <- c("F", "M", "F", "M")
peso[sexo == "F"]
names(peso) <- c("Marta", "Jose", "Paula", "Paco")
peso[c("Marta", "Paula")]

x <- c(1:8, NA, 10) # x es un vector con un dato faltante
x
is.na(x)
mean(x)
mean(x, na.rm = TRUE)
is.na(x) <- c(2, 3)
x # la sentencia anterior define como NA los elementos segundo y tercero

x[is.na(x)] <- 77
x
0 / 0
Inf - Inf
x <- c(NaN, 1:4, NA)
x
is.na(x)
is.nan(x)

x <- 1:5
y <- seq(0, 1, length.out = 5)
x + y
x * y
exp(x) + log(1 + y)
x^2

z <- c(-2, -1)
x + z

x <- (1:10) / 10
(x < 0.3) * (2 * x) + (0.3 <= x & x < 0.7) * x^2 + (x >= 0.7) * 2

x <- c(2, 3, 5, 10, 6, 1)
cumsum(x)
prod(x)
cumprod(x)
max(x)
which.max(x)
x[which.min(x)] # equivale a min(x)
x
sort(x) # ordena
order(x) # devuelve los índices ordenados
x
x[order(x)] ### == sort(x)

set.seed(1)
x <- runif(100) # 100 valores aleatorios de una Uniforme(0,1)
mx <- mean(x)
which((x - mx) == min((x - mx)))

x <- 1:5
mode(x)

y <- x > 2
mode(y)

z <- letters[5]
mode(z)