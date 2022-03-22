# 2.2
example(mean)

# 1. Generamos dos vectores de datos desde sendas distribuciones normales
# y los guardamos en sendos objetos 'x' e 'y'
x <- rnorm(50)
y <- rnorm(50, mean = 10, sd = 2)
# Imprimimos en la consola el contenido de los objetos 'x' e 'y'
x
y
# 2. Resumen descriptivo de los datos
summary(x)
summary(y)
# 3. Histograma de x
hist(x)
# 4. Representamos la distribución conjunta de los datos (scatterplot)
plot(x, y)
# 5. Ajustamos una recta de regresión a los datos
fit <- lm(y ~ x)
summary(fit)
# 6. Superponemos la recta de regresión al gráfico anterior
abline(fit)
# 7. Creamos dos nuevos vectores con secuencias de valores
x <- 1:10
y <- seq(-pi, pi, length.out = 10)
# 8. Escribimos una matriz con los vectores anteriores por columnas
cbind(x, y)
# 9. Escribimos una matriz con dos filas y cinco columnas con los elementos de x
matrix(x, 2, 5)
# 10. Creamos una matriz con filas y columnas indexadas por x e y,
# cuyos valores son cos(y)/(1 + x^2))
f <- outer(x, y, function(x, y) cos(y) / (1 + x^2))
f
# 11. Dos representaciones tridimensional de f como función de x e y
# primero un diagrama de contornos
contour(x, y, f)
# añadimos más niveles
contour(x, y, f, nlevels = 15, add = TRUE)
# y ahora un mapa de colores
image(x, y, f)
# 12. Demostración de otras funciones gráficas
demo(graphics)
demo(persp)
##############

# 2.3
x <- 5
y <- 2 * x
z <- log(y)
k <- x * y * z
ls()
rm(x, y)
ls()
rm(list = ls())
ls()

# matrix(data = NA, nrow = 1, ncol = 1, byrow = FALSE, dimnames = NULL)
matrix(pi, 1, 3)

#########
# 2.4
0.3 - 0.1 == 0.2
0.3 - 0.2
all.equal(0.3 - 0.2, 0.1)