
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
boxplot(salary, horizontal = TRUE, axes = FALSE, lwd = 2)

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

# ────────────────────────────────────────────────────────────────────────────────
# Ejercicio propuesto
data(airquality, package = "datasets")
