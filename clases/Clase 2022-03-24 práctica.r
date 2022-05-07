# Clase 2022-04-24 práctica.r
censo <- read.csv("./Inputfiles/census.csv", header = T, as.is = F)
censo

# b
lapply(censo, class)

# c
lapply(censo, function(x) {
    sum(is.na(x))
})
# lo mismo
lapply(lapply(censo, is.na), sum)

# d
sum(complete.cases(censo))
# e
censo2 <- censo[complete.cases(censo), ]
censo2
all(censo2 == na.omit(censo))

# f
write.table(censo2,
    file = "./outputfiles/censo2.txt",
    sep = "\t",
    col.names = T,
    row.names = F
)

# g
# coincide con censo2?
censo3 <- read.table(
    file = "./outputfiles/censo2.txt",
    header = T,
    sep = "\t",
    stringsAsFactors = T
)
# Misma estructura
str(censo2)
str(censo3)
str_censo_2 <- capture.output(str(censo2))
str_censo_3 <- capture.output(str(censo3))
all(str_censo_2 == str_censo_3)

any(censo2 != censo3)
all(censo2 == censo3)

#
# --- EJERCICIO 2 ----------------------------------------------------------------
#
rm(list = ls())
set.seed(90)

filas <- 10
columnas <- 5
matriz <- matrix(rnorm(filas * columnas), nrow = filas, ncol = columnas)
# a
colnames(matriz) <- paste0("col", seq_len(ncol(matriz)))
matriz

class(colnames(matriz))

write.table(matriz,
    file = "./outputfiles/matriz.txt",
    sep = ",",
    row.names = F,
    quote = F
)

my_df <- read.table(file = "./outputfiles/matriz.txt", header = T, sep = ",")
my_df
#
# --- EJERCICIO 3 ----------------------------------------------------------------
#
rm(list = ls())
olimpics <- read.csv(file = "./Inputfiles/Olympics100m.csv", stringsAsFactors = T)
str(olimpics)

# b
sum(is.na(olimpics))

# c
resumen <- summary(olimpics)
resumen
class(resumen)
typeof(resumen)

write.table(resumen, file = "./outputfiles/resumen.txt", row.names = F, sep = "\t", quote = F)
read.table(file = "./outputfiles/resumen.txt", sep = "\t", header = T, strip.white = T)

# d
resumen2 <- aggregate(olimpics$TIME, by = list(olimpics$Gender), FUN = summary)
resumen2
write.csv(resumen2, file = "./outputfiles/resumen2.csv", row.names = F)

# Comprobamos que pueda ser bien leído
resumen2_leido <- read.csv(file = "./outputfiles/resumen2.csv")
resumen2_leido
lapply(resumen2_leido, class)
