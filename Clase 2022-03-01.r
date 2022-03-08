# Clase 01-03

help("source")

getwd() # == pwd
search()
library()
# Eg Estimador máximo verosimil
library()
.Machine$integer.max
.Machine$double.xmax

# Tema 2
## Tabla de tipos de datos:
## tipo \ dim     1D       2D         n-D
## homogéneo    vector   matrix      array
## heterogéneo  list     data frame

# Nota: vector es atómico, unidad básica.
x <- c(0, 1, 2, 3)
y <- x == 0:3
Reduce("&", y)
## TRUE
a <- c(-1, x, 3)
a
z <- c("aiodn", "pqow")
q <- c(a, z, x)
q
# Implicit type conversion
rep(1:5, times = seq(1, by = 2, length.out = 5))
rep(1:5, each = 2)
rep(1:3, each = 5, times=3)
