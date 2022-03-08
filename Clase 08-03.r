# Clase 08-03
# Factores
help("factor")
civil <- c(
    "soltero/a", "viudo/a", "casado/a", "soltero/a", "viudo/a", "divorciado/a",
    "soltero/a", "casado/a", "soltero/a", "divorciado/a"
)
civil.f <- factor(civil)
civil.f
class(civil)
class(civil.f)
attributes(civil)
attributes(civil.f)

# Ordenado
factor(civil, levels = c("soltero/a", "casado/a", "divorciado/a", "viudo/a"))
sexo.f <- factor( c(1, 1, 2, 1, 1, 2, 2, 1, 2, 1), labels = c("hombre", "mujer"))
sexo.f
unclass(sexo.f)
unclass(civil.f)
# Acceder y modificar
civil.f[1]
civil.f[-(1:5)]
levels(sexo.f)
levels(sexo.f)[2]
levels(sexo.f)<-c('masculino','femenino')
sexo.f
# Aplicaciones
edad <- c(23, 25, 20, 19, 20, 22, 24, 20, 23, 19)
mean(edad[sexo.f == "hombre"])
mean(edad[sexo.f == "mujer"])
    # tapply
help("tapply")
tapply(edad, sexo.f, mean)
tapply(edad, sexo.f, sd)
tapply(edad, sexo.f, summary)
tapply(edad, civil.f, summary)
    # plotting
boxplot(edad~sexo.f)
help("boxplot")

