rm(list=ls())

library(readxl)
library(dplyr)
library(factoextra)
library(knitr)
library(psych)

dataPath  = here::here("datos") 

dataName  = "nacimientos.xls"


df_nacimientos = read_xls(
                      file.path(dataPath, dataName)
                    ) 


str(df_nacimientos)

dim(df_nacimientos)

summary(df_nacimientos)

df_cluster = df_nacimientos %>% 
             select(-id, -centro)

row.names(df_cluster) = df_nacimientos$id

d = dist(df_cluster, method = "binary")

fit = hclust(d, method = "average")


# Indice Silhouette  ---
fviz_nbclust(df_cluster, kmeans,  method = "silhouette") +
  labs(title    = "Número óptimo de clusters a considerar",
       subtitle = "Indice Silhouette")


# Suma de cuadrado error (o within)---
wss <- (nrow(dfToCluster) - 1) * sum(apply(dfToCluster, 2, var))
for (i in 2:10) {
  wss[i] <- sum(kmeans(dfToCluster, centers = i)$withinss)
}
plot(1:10, wss, type = "b", xlab = "Number of Clusters",
     ylab = "Suma de cuadrados dentro de los clusters (within)")


groups <- cutree(fit, k = 4) 
