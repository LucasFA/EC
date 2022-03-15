# Clase 2022-03-15
# Listas
rm(list = ls())
A <- matrix(1:6, 3, 2)
m1 <- rowMeans(A)
m2 <- colMeans(A)
minA <- min(A)
maxA <- max(A)
nomb <- c("col1", "col2", "col3")
cond <- A > mean(A) + sd(A)
cond
A
lista <- list(A, m1, m2, minA, maxA, nomb, cond)
lista

lista <- list(
    matriz = A, med_fil = m1, med_col = m2, minimo = minA, maximo = maxA,
    nombres = nomb, cond = cond
)
lista[["matriz"]]
lista$matriz

class(lista)
lapply(lista, class)
lapply(lista, mean)

sapply(lista, class)
sapply(lista, mean)

unlist(lista)

class(lista[1])
class(lista[[1]])

x <- rnorm(100)
res <- hist(x)
res
sum(diff(res$breaks) * res$density)


#
# ─── DATA FRAMES ────────────────────────────────────────────────────────────────
#
rm(list = ls())
dni <- c("22456715A", "22456716B", "22456717C", "22456718D", "22456719E")
edad <- c(45, 35, 52, 60, 25)
sexo <- factor(c("Hombre", "Mujer", "Hombre", "Mujer", "Hombre"))
estudios <- factor(c("superior", "superior", "profesional", "medio", "profesional"))
salario <- c(2500, 1500, 2000, 1200, 1800)

datos <- data.frame(dni, edad, sexo, estudios, salario)
datos
class(datos)
names(datos)
str(datos)

summary(datos)

datos[3] # Tratar como lista
datos[3, ] # Tratar como matriz
datos[, 3]

datos[1:2, c("edad", "salario")]
datos[, c(-1, -3, -4)]

model <- lm(datos$salario ~ datos$edad)
model
plot(model)

sample(datos, size = 2) # same as
datos[sample(seq_len(length(datos)), 2)]

mtcars
head(mtcars)
tail(mtcars)

subset(mtcars, vs == 0 & hp > 90)

subset(mtcars, vs == 0 & hp > 90, select = c(-vs))

# Transformar la masa de libras a kg:
transform(mtcars, wt = wt / 2.2046)
transform(mtcars, wtkg = wt / 2.2046)
# --------------------------------------------------------------------------------
# within:
mtcars2 <- within(mtcars, {
    vs <- factor(vs, labels = c("V", "S"))
    am <- factor(am, labels = c("automatic", "manual"))
    cyl <- ordered(cyl)
    gear <- ordered(gear)
    carb <- ordered(carb)
})
mtcars2

with(mtcars2, boxplot(mpg ~ vs))
with(subset(mtcars2, vs == "V"), hist(mpg, main = "vs=V"))
with(subset(mtcars2, vs == "S"), hist(mpg, main = "vs=S"))

# --------------------------------------------------------------------------------
# Comprobación de tipo y casteo

x <- c(1, 2, 2, 1, 2, 1, 1, 1)
is.numeric(x)
is.factor(x)
x <- as.factor(x)
x
class(x)
as.matrix(1:2)
as.complex(1:2)
A <- matrix(1:4, 2, 2)
is.numeric(A)
as.numeric(A)
D <- as.data.frame(A)
is.data.frame(D[1])
is.data.frame(D[1, ])
is.data.frame(D[, 1])
is.vector(D[, 1])
as.numeric(factor(c("H", "M")))
hh <- hist(runif(100), plot = FALSE)
is.list(hh)
