
rm(list = ls())

employee <- read.table("./inputfiles/employee.txt", header = T, stringsAsFactors = T)
head(employee)
str(employee)
levels(employee$gender) <- c("female", "male") # Cuidado: hay que comprobar que el orden sea correcto

# --------------------------------------------------------------------------------

attach(employee)
hist(salary)

res <- hist(salary, plot = FALSE)
res

hist(salary, breaks = 100)
# puntos de corte para intervalos con distinta amplitud
x1 <- seq(15000, 40000, by = 5000)
x2 <- seq(50000, 80000, by = 10000)
x3 <- seq(100000, 140000, by = 20000)
hist(salary, breaks = c(x1, x2, x3))

lines(density(salary), col = "blue")

curve(dnorm(x, mean = mean(salary), sd = sd(salary)), add = T)
# Claramente no se ajusta bien

# --------------------------------------------------------------------------------

qqnorm(salary)
qqline(salary)

ks.test(salary, pnorm, mean = mean(salary), sd = sd(salary))
shapiro.test(salary)

boxplot(salary)

summary(salary)

hist(salary, probability = TRUE, main = "", axes = FALSE)
axis(1)
lines(density(salary), col = "red", lwd = 2)
par(new = TRUE) ## Para que el próximo gráfico se superponga al anterior
boxplot(salary, horizontal = TRUE, axes = FALSE, lwd = 2, new = T)

boxplot(salary ~ gender)
boxplot(salary ~ minority)
boxplot(salary ~ jobcat)
# A continuación salario con una doble clasificación
boxplot(salary ~ gender * jobcat)

# Con startsal y age
boxplot(salary ~ age)
boxplot(salary ~ startsal)

# --------------------------------------------------------------------------------

plot(startsal, salary)

mod <- lm(salary ~ startsal)
mod
abline(mod, col = "blue")

# ej
# 1
plot(age, salary)
mod <- lm(salary ~ age)
mod
abline(mod, col = "blue")
# Los jóvenes cobran más, a razón de perder 211$ del salario anual
# por año de edad en el salario
# 2
plot(edu, salary)
mod <- lm(salary ~ edu)
mod
abline(mod, col = "blue")
# Los años de educación son un dato que significativamente asociado
# al salario del empleado: cada año de educación se asocia a casi 4 000$
# más de salario (anual)
# También se observa que ciertos valores son muchos más comunes que otros:
# 8, 12, 15 y 16 son los más comunes

#
# --- 1.2 VARIABLES CUALITATIVAS -------------------------------------------------
#

tab <- table(jobcat)
tab
tab.fi <- prop.table(tab)
tab.fi
data.frame(tab, Freq.rel = as.numeric(tab.fi))

barplot(tab)
pie(tab)

# --------------------------------------------------------------------------------

analizar <- function(caracteristica) {
    tab <- table(caracteristica)
    tab.fi <- prop.table(tab)
    data.frame(tab, Freq.rel = as.numeric(tab.fi))

    barplot(tab)
    pie(tab)
}
analizar(gender)
analizar(minority)

# --------------------------------------------------------------------------------

tab2 <- table(jobcat, gender)
tab2
# Y podemos añadir las sumas por filas y columnas
addmargins(tab2)

barplot(tab2)
varios_plots <- function(tab) {
    barplot(tab,
        legend.text = TRUE, args.legend = list(x = "topleft", bty = "n"),
        ylim = c(0, 300), density = 30, col = c("green", "blue", "red"),
        main = "Number of employees by gender and job category"
    )

    barplot(tab,
        legend.text = TRUE, args.legend = list(x = "top", bty = "n"),
        density = 30, col = c("green", "blue", "red"),
        main = "Number of employees by gender and job category",
        beside = TRUE
    )
}
varios_plots(tab2)
# Construye una tabla de contingencia que muestre la clasicación de los individuos según
# jobcat y minority. Representa un diagrama de barras que muestre dicha clasicación.

tab2 <- table(jobcat, minority)
tab2
varios_plots(tab2)

# --------------------------------------------------------------------------------
# Ejercicio propuesto
data(airquality, package = "datasets")
attach(airquality)
# 1
hist(Ozone, breaks = 10, freq = F)
curve(
    dnorm(x, mean = mean(Ozone, na.rm = T), sd = sd(Ozone, na.rm = T)),
    add = T
)
# La normal no modeliza bien estos valores. Además, no está acotada inferiormente por 0

# 3
qqnorm(Ozone)
qqline(Ozone)
# Vemos que modeliza de los valores para los cuales más se desvía es para los cuantiles bajos y altos

ks.test(Ozone, pnorm, mean = mean(Ozone, na.rm = T), sd = sd(Ozone, na.rm = T))
shapiro.test(Ozone)

# 4
par(mfrow = c(1, 2))
boxplot(Ozone)
puntos <- seq(from = 0, to = 1, length.out = 101)
boxplot(pnorm(puntos), mean = mean(Ozone, na.rm = T), sd = sd(Ozone, na.rm = T))

# 5
par(mfrow = c(2, 3), mar = c(1, 3, 1, 2))
aggregate(airquality$Ozone, by = list(airquality$Month), FUN = boxplot)
# Parece que en los meses de verano (principalmente julio y agosto) suben los
# niveles de ozono en el aire. Esto se reduce en septiembre salvo en algunos casos,
# posiblemente correspondientes a algún lugar en el que la causa común,
# posiblemente la temperatura, se prolongue.
dev.off()
# 6
fit <- lm(Ozone ~ Wind)
plot(Ozone ~ Wind)
abline(fit)

plot(fit)
summary(fit)
# El viento está fuertemente relacionado con los niveles de ozono.
# En los datos presenta un R^2 de 0.36 y una pendiente de -5.5 (unidades correspondientes)

fit <- lm(Ozone ~ Temp)
plot(Ozone ~ Temp)
abline(fit)

plot(fit)
summary(fit)
# La temperatura presenta un factor aún más importante.
# Su R^2 es de 0.48 y una pendiente de 2.43 (unidades correspondientes)

cor(Wind, Temp)

fit <- lm(Ozone ~ Temp * Wind) # probar tanto con * como con +
fit
summary(fit)
plot(fit)
