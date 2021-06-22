library(readxl)
library(dplyr)
library(factoextra)
library(knitr)
library(psych)

#dataPath  <- here::here("datos") 

#dataName  <- "datoscereales.xls"


dfCereales <- datoscereales %>% 
  dplyr::mutate(
    marca   = factor(marca),
    publico = factor(publico),
    paquete = factor(paquete)
  )
dfCereales
str(dfCereales)
dim(dfCereales)
head(dfCereales)
summary(dfCereales)
rownames(dfCereales) <- dfCereales$id

# seleccionar solo las variables de las características nutricionales
vars_to_cluster <- c( "calorias", "proteinas", "carbohidratos", "fibras", 
                      "lipidos", "sodio", "potasio", "vitamina", "hierro" )
dfToCluster <- dfCereales %>% 
  dplyr::select(all_of(vars_to_cluster))
summary(dfToCluster)
View(dfToCluster)

dfToCluster <- scale(dfToCluster)
summary(dfToCluster)

#Calculo las distancias
d   <- dist(dfToCluster, method = "euclidean")
d

# Realizar el agrupamiento jerárquico
fit1 <- hclust(d, method = "ward.D")
plot(fit1)

# Indentificar los distintos clusters elegidos 

rect.hclust(fit1, k = 4, border = "red")

# Indice Silhouette  ---
fviz_nbclust(dfToCluster, kmeans, method = "silhouette") +
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

tablaResumen <- describeBy(
  formula_para_describir, 
  mat = TRUE, 
  data = dfToCluster
)

cat("\n### Medidas resúmenes (variables estandarizadas) para los clusters elegidos.\n") 
## 
#Medidas resúmenes (variables estandarizadas) para los clusters elegidos.

kable(
  tablaResumen %>% 
    dplyr::mutate(variable = rownames(.),
                  cv       = 100 * sd / abs(mean) ) %>% 
    dplyr::rename(cluster = group1) %>% 
    dplyr::select(variable, cluster, n, mean, median, cv, min, max) %>% 
    arrange(as.numeric(as.character(cluster))), 
  digits = 2
)

# Caracterizar los segmentos encontrados (utilizando las variables originales)
tablaResumen <- describeBy(
  formula_para_describir, 
  mat = TRUE, 
  data = dfCereales
)

cat("\n### Medidas resúmenes (variables originales) para los clusters elegidos.\n") 

## ### Medidas resúmenes (variables originales) para los clusters elegidos.
kable(
  tablaResumen %>% 
    dplyr::mutate(variable = rownames(.),
                  cv       = 100 * sd / abs(mean) ) %>% 
    dplyr::rename(cluster = group1) %>% 
    dplyr::select(variable, cluster, n, mean, median, cv, min, max) %>% 
    arrange(as.numeric(as.character(cluster))), 
  digits = 2
)

dim(tablaResumen)

#Atencion!!!!
#Ejemplo con cambio de grupos
#cambio cantidad de grupos
fit <- hclust(d, method = "ward.D")
plot(fit)

# Indentificar los distintos clusters elegidos 

rect.hclust(fit, k = 5, border = "red")

fviz_nbclust(dfToCluster, kmeans, method = "silhouette") +
  labs(title    = "Número óptimo de clusters a considerar",
       subtitle = "Indice Silhouette")
wss <- (nrow(dfToCluster) - 1) * sum(apply(dfToCluster, 2, var))
for (i in 2:10) {
  wss[i] <- sum(kmeans(dfToCluster, centers = i)$withinss)
}
plot(1:10, wss, type = "b", xlab = "Number of Clusters",
     ylab = "Suma de cuadrados dentro de los clusters (within)")

groups <- cutree(fit, k = 5) 
dfToCluster         <- data.frame(dfToCluster)
dfToCluster$cluster <- groups
dfCereales$cluster  <- groups

dimension_cereales<- datoscereales%>%
  select(id, marca, publico, paquete)

class(dfToCluster)
dimension_cereales<-as.data.frame(dimension_cereales)
                                  
prueba<- left_join(dimension_cereales, dfToCluster, by=c("id", as.character(names(dfToCluster)[2])))

prueba1<-Reduce(function(x,y) merge(x, y, all=TRUE), list(dimension_cereales, dfToCluster))
View(prueba)


# Caracterizar los segmentos encontrados (utilizando las variables estandarizadas)
formula_para_describir <- as.formula(
  paste0( paste(vars_to_cluster, collapse = " + "), " ~ cluster") 
)

formula_para_describir

tablaResumen1 <- describeBy(
  formula_para_describir, 
  mat = TRUE, 
  data = dfToCluster
)

cat("\n### Medidas resúmenes (variables estandarizadas) para los clusters elegidos.\n") 
## 
#Medidas resúmenes (variables estandarizadas) para los clusters elegidos.

kable(
  tablaResumen1 %>% 
    dplyr::mutate(variable = rownames(.),
                  cv       = 100 * sd / abs(mean) ) %>% 
    dplyr::rename(cluster = group1) %>% 
    dplyr::select(variable, cluster, n, mean, median, cv, min, max) %>% 
    arrange(as.numeric(as.character(cluster))), 
  digits = 2
)

# Caracterizar los segmentos encontrados (utilizando las variables originales)
tablaResumen1 <- describeBy(
  formula_para_describir, 
  mat = TRUE, 
  data = dfCereales
)

cat("\n### Medidas resúmenes (variables originales) para los clusters elegidos.\n") 

## ### Medidas resúmenes (variables originales) para los clusters elegidos.
kable(
  tablaResumen1 %>% 
    dplyr::mutate(variable = rownames(.),
                  cv       = 100 * sd / abs(mean) ) %>% 
    dplyr::rename(cluster = group1) %>% 
    dplyr::select(variable, cluster, n, mean, median, cv, min, max) %>% 
    arrange(as.numeric(as.character(cluster))), 
  digits = 2
)

dim(tablaResumen)
# El cluster 1 tiene alta fibra, alto en potasio y bajo en calorias.
#c2: alta proteina y bajo en sodio
#c3: baja proteina
#c4: alto sodio

#1.7: se divide el grupo 2, los gruposnuevos formados se diferencian por las calorias. 


