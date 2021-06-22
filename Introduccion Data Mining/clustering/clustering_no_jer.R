rm(list=ls())

library(readxl)
library(dplyr)
library(factoextra)
library(knitr)
library(psych)
library(map)
library(tidyr)


dataPath  = here::here("datos") 
dataName  = "consumos_2018.csv"


df_consumos =  read.csv(file.path(dataPath, dataName),
                    sep = ";",
                    dec = ",",
                    header = TRUE
  )

summary(df_consumos)
str(df_consumos)

vars_to_cluster =  c("kids_r", "prop_r", "resid", "age", "inc_r", "persh", 
                     "inc_b", "prop_b", "kids_b", "zona")

dfDemograficas = df_consumos %>% 
  select(all_of(vars_to_cluster))

#pairs.panels(dfDemograficas, pch='.') 

# Estandarizar las variables
dfDemograficasStd <- scale(dfDemograficas)
par(mfrow=c(1,1))

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


dfDemograficasStd$cluster <- clustering$cluster

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
MtxParaVisualizar <- as.matrix( dfDemograficasStd)
out <- coclusterContinuous(MtxParaVisualizar, nbcocluster = c(6,1))
plot(out)
