library(tidyr)
library(arules)
library(knitr)
library(arulesViz)
library(here)
library(tidyverse)
library(DescTools)

path = here("datos")

data_name = "juego.csv"

df_juego = read.csv(file = file.path(path, data_name),
                    sep = ";"
                    )

#priumeras filas
head(df_juego)

#tipos de datos
str(df_juego)

#resumen de las variables
summary(df_juego)
glimpse(df_juego)

#filasy coles
dim(df_juego)

kable(df_juego)

rules_all = apriori(df_juego[, -1])

str(rules_all)

#coverage es la elevacion y la cantidad de transacciones con esa regla del antecedente?
#el support es el soporte de la regla, es decir, la proporcion de vces que aparecen los dos articulos al mismo tiempo
#en las diapositivas definimos el soporte de x proporcion de veces que aparece x

#fila 3
#no se juega y hay humedad alta 4/14 = 0.28, no se juega 0.35 (coverage es el soporte del antecedente) - el 80% de las veces que no se juega, hay humedad alta
#lift cuanto me aumenta la presencia del lado derecho cuando sabemos el lado izquierdo en fila 1 es 1/(1-0.3571429)

inspect(rules_all)
reglas_completas = as(rules_all, "data.frame")
head(reglas_completas)


rules = apriori(df_juego[, -1],
                control = list(verbose = F),
                parameter = list(minlen = 2,
                                 supp = 0.1,
                                 conf = 0.8),
                appearance = list(rhs = c("juego=no", "juego=si"),
                                  default = "lhs")
                )
kable(as(rules, "data.frame"))

df_rules_juego_no =  as(rules, "data.frame")
kable(filter( df_rules_juego_no, df_rules_juego_no$rules %like% "%juego=no%"))

quality(rules) = round(quality(rules), digits = 3)
rules.ord = sort(rules, decreasing = TRUE, by = "lift")
kable(as(rules.ord, "data.frame"), digits = 3)


#Remover las redundantes
inspect(rules.ord[1 : 6])

subset.matrix <- is.subset(rules.ord, rules.ord)
subset.matrix <- as.matrix(subset.matrix)
subset.matrix[lower.tri(subset.matrix, diag = T)] <- NA
redundant <- colSums(subset.matrix, na.rm = T) >= 1
which(redundant)

rules.pruned <- rules.ord[ ! redundant]
kable(as(rules.pruned, "data.frame"), digits = 3)


plot(rules.pruned)
plot(rules.pruned, method = "grouped")
plot(rules.pruned, method = "graph")
