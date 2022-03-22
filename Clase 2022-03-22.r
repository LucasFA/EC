# Clase 2022-03-22

#
# --- LEER FICHEROS --------------------------------------------------------------
#

datos <- read.table(file = "datos.txt", header = T, as.is = c(1))
datos
lapply(datos, class)
str(datos)
datos <- read.table(file = "datos_comas.csv", header = T, sep = ",")
datos <- read.csv(file = "datos_comas.csv", header = T)
# read.csv2() para csv separado por ";"
datos
datos <- scan(
    file = "datos.txt",
    skip = 1,
    what = list(
        DNI = "", edad = 0, sexo = "",
        estudios = "", salario = 0
    )
)
str(datos)
lapply(datos, class)
as.data.frame(datos)
lapply(as.data.frame(datos), class)

# Leer de terminal
datos2 <- scan()


rm(list = ls())
data(cars)
data(package = "boot")
data(coal, package = "boot")
data(package = "datasets")
data()
ls()
ls("package:datasets")

# --------------------------------------------------------------------------------
# Attach, detach

mean(datos$Salario)
attach(datos)
search()
mean(Salario)
detach(datos)
search()


# --------------------------------------------------------------------------------
# Escribit a ficheros

write.table(cars[1:10, ], file = "outputfiles/datos1.txt")
datos1 <- read.table("datos1.txt", header = TRUE)

write(runif(10), file = "outputfiles/datos2.txt", ncolumns = 1)
write(t(as.matrix(datos1)), file = "outputfiles/datos3.txt", ncolumns = 2)

write(cars[1:10, ], file = "outputfiles/datos3.txt")
help("write")
