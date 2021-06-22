rm(list=ls())

library(readxl)
library(dplyr)
library(factoextra)
library(knitr)
library(psych)
library(map)
library(tidyr)


dataPath  = here::here("datos") 

dataName  = "datoscereales.xls"


dfCereales = read_xls(
  file.path(dataPath, dataName)
) %>% 
  dplyr::mutate(
    marca   = factor(marca),
    publico = factor(publico),
    paquete = factor(paquete)
  )

str(dfCereales)

dim(dfCereales)

summary(dfCereales)

dfCereales = dfCereales %>% 
  mutate(marca = paste("marca", marca, sep = "_"),
         valor_marca = 1,
         publico = paste("publico", publico, sep = "_"),
         valor_publico = 1
  ) %>% 
  spread(key = marca, value = valor_marca) %>% 
  spread(key = publico, value = valor_publico)


dfCereales[is.na(dfCereales)] = 0

dfCereales = dfCereales %>% 
            rename('marca_3Arroyos' = 'marca_3 Arroyos')

# Asignar el 'id' de los cereales como nombre de las filas
rownames(dfCereales) <- dfCereales$id

# seleccionar solo las variables de las características nutricionales
vars_to_cluster = c("calorias" ,"proteinas","carbohidratos","fibras", "lipidos","sodio",          
                     "potasio","vitamina","hierro",  "marca_3Arroyos","marca_Granix",   
                    "marca_Kelloggs", "marca_Nestlé",  "marca_Nutridía","marca_Quaker", "publico_adultos"
                     ,"publico_chicos" )

dfToCluster = dfCereales %>% 
                      select(all_of(vars_to_cluster))
                    summary(dfToCluster)


# Calcular las distancias
d = dist(dfToCluster, method = "euclidean")

# Realizar el agrupamiento jerárquico
fit = hclust(d, method = "ward.D")


hclust_custom = function(x, k) {
  groups <- cutree(fit, k = k)
  dfToCluster$cluster <- groups
  dfToCluster
}

install.packages("blockcluster")
library(blockcluster)
MtxParaVisualizar <- as.matrix(dfToCluster %>% dplyr::select( - cluster))
out <- coclusterContinuous(MtxParaVisualizar, nbcocluster = c(4, 4))
plot(out)


# Indice Silhouette  ---
fviz_nbclust(dfToCluster, hclust_custom,  method = "silhouette") +
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

# Unir los clusters formado con los datos originales para poder describir los grupos
dfToCluster         <- data.frame(dfToCluster)
dfToCluster$cluster <- groups
dfCereales$cluster  <- groups


# Caracterizar los segmentos encontrados (utilizando las variables estandarizadas)
formula_para_describir <- as.formula(
  paste0( paste(vars_to_cluster, collapse = " + "), " ~ cluster") 
)

formula_para_describir


tablaResumen = describeBy(
  formula_para_describir, 
  mat = TRUE, 
  data = dfToCluster
)

cat("\n### Medidas resúmenes (variables estandarizadas) para los clusters elegidos.\n") 


dimension_cereales = dfCereales%>%
  select(id, marca, publico, paquete)

prueba<- left_join(dimension_cereales, dfToCluster, by=c("id", as.character(names(dfToCluster)[2])))
