library(dplyr)
library(factoextra)
library(knitr)
library(psych)
library(corrplot)

library(readr)

 # Archivo con los datos

datos <- read.csv("consumos_2018.csv",
                  sep = ";",
                  dec = ",",
                  header = TRUE
)
#Describir el conjunto de datos

str(datos)

head(datos)
summary(datos)
# Variables a utilizar
vars_to_cluster <- c("kids_r", "prop_r", "resid", "age", "inc_r", "persh", 
                     "inc_b", "prop_b", "kids_b")

dfDemograficas <- datos %>% 
  dplyr::select(all_of(vars_to_cluster))

#Graficas de la matriz de correlaciones utilizando las alternativas de la librería “psych”

pairs.panels(dfDemograficas, pch='.') 
corrplot(cor(dfDemograficas), order = "hclust")

# Estandarizar las variables
dfDemograficasStd <- scale(dfDemograficas)


# Suma de cuadrado error (o within)---
wss <- (nrow(dfDemograficasStd) - 1) * sum(apply(dfDemograficasStd, 2, var))
for (i in 2:10) {
  wss[i] <- sum(kmeans(dfDemograficasStd, centers = i)$withinss)
}
plot(1:10, wss, type = "b", xlab = "Number of Clusters",
     ylab = "Suma de cuadrados dentro de los clusters")

# Indice Silhouette  ---
fviz_nbclust(dfDemograficasStd, kmeans, method = "silhouette") +
  labs(title    = "Número óptimo de clusters a considerar",
       subtitle = "Indice Silhouette")

# Agrupamiento no jerarquico
clustering <- kmeans(dfDemograficasStd, centers = 5)


dfDemograficas$cluster <- clustering$cluster

View(dfDemograficas)

formula_para_describir <- as.formula(
  paste0( paste(vars_to_cluster, collapse = " + "), " ~ cluster") 
)

formula_para_describir


tablaResumen <- describeBy(
  formula_para_describir, 
  mat = TRUE, 
  data = dfDemograficas
)

library(blockcluster)
  MtxParaVisualizar <- as.matrix( dfDemograficas%>% dplyr::select( - cluster))
out <- coclusterContinuous(MtxParaVisualizar, nbcocluster = c(4, 4))
plot(out)
a
a

