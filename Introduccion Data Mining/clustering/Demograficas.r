## Seteos iniciales ----
rm(list=ls())
dev.off()

lista_paquetes = c('dbscan','FactoMineR','gridExtra','janitor','factoextra','psych','corrplot','readr','tidyverse', 'DescTools', 'here','blockcluster', 'knitr', 'readxl', 'ggplot2')
nuevos_paquetes = lista_paquetes[!(lista_paquetes %in% installed.packages()[,"Package"])]
if(length(nuevos_paquetes)) install.packages(nuevos_paquetes, dependencies = TRUE)

suppressWarnings(suppressMessages(easypackages::libraries(lista_paquetes)))

PATH = here::here('datos') 

# Archivo con los datos
datos = read.csv(file.path(PATH,"consumos_2018.csv"),
                 sep = ";",
                 dec = ",",
                 header = TRUE
)

desc_campos = readxl::read_xlsx(file.path(PATH,"desc_campos.xlsx")) %>% 
              clean_names()



head(datos)

summary(datos)

# 2.1 Intro Kmeans con las demograficas ----
# Variables a utilizar
vars_to_cluster = c("kids_r", "prop_r", "resid", "age", "inc_r", "persh", 
                    "inc_b", "prop_b", "kids_b")

dfDemograficas = datos %>% 
  select(all_of(vars_to_cluster))



#Graficas de la matriz de correlaciones utilizando las alternativas de la librería “psych”

corrplot(cor(dfDemograficas), order = "hclust")


# Estandarizar las variables
dfDemograficasStd = scale(dfDemograficas)




# Suma de cuadrado error (o within)---
wss <- (nrow(dfDemograficasStd) - 1) * sum(apply(dfDemograficasStd, 2, var))
for (i in 2:10) {
  wss[i] <- sum(kmeans(dfDemograficasStd, centers = i)$withinss)
}

options(repr.plot.width=5, repr.plot.height=4)
plot(1:10, wss, type = "b", xlab = "Number of Clusters",
     ylab = "Suma de cuadrados dentro de los clusters")



# Indice Silhouette  ---
#::fviz_nbclust(as.data.frame(dfDemograficasStd), kmeans, method = "wss") +
#  labs(title    = "Número óptimo de clusters a considerar",
#       subtitle = "Indice Silhouette")

#Plot
#library(blockcluster)
#MtxParaVisualizar <- as.matrix( dfDemograficas%>% dplyr::select( - cluster))
#out <- coclusterContinuous(MtxParaVisualizar, nbcocluster = c(4, 4))
#plot(out)



# Agrupamiento no jerarquico
set.seed(42)
clustering <- kmeans(dfDemograficasStd, centers = 4)

dfDemograficasStd = dfDemograficasStd %>% 
              as.data.frame()

dfDemograficasStd$cluster <- clustering$cluster 

dfDemograficasStd = dfDemograficasStd %>% 
  as.data.frame()



#dfDemograficasStds$cluster <- clustering$cluster

formula_para_describir <- as.formula(
  paste0( paste(vars_to_cluster, collapse = " + "), " ~ cluster") 
)

tablaResumen <- describeBy(  formula_para_describir, 
                             mat = TRUE, 
                             data = dfDemograficasStd
)

tablaResumen =  tablaResumen %>% 
  dplyr::mutate(variable = rownames(.),
                cv       = 100 * sd / abs(mean) ) %>% 
  dplyr::rename(cluster = group1) %>% 
  dplyr::select(variable, cluster, n, mean, median, cv, min, max) %>% 
  arrange(as.numeric(as.character(cluster)))   %>% 
  mutate(variable = substr(variable, 1, nchar(variable)-1))

tablaResumen = tablaResumen  %>% 
  merge(desc_campos, by.x = 'variable', by.y = 'campo', all.x = TRUE)

##2.4 Kmeans con las demograficas + zona ----
# Variables a utilizar
vars_to_cluster = c("kids_r", "prop_r", "resid", "age", "inc_r", "persh", 
                    "inc_b", "prop_b", "kids_b", 'zona')

df_Dem_Zona = datos %>% 
  select(all_of(vars_to_cluster))

df_Dem_Zona_Std = scale(df_Dem_Zona)



# Suma de cuadrado error (o within)---
wss_2 <- (nrow(df_Dem_Zona_Std) - 1) * sum(apply(df_Dem_Zona_Std, 2, var))

for (i in 2:11) {
  wss_2[i] <- sum(kmeans(df_Dem_Zona_Std, centers = i)$withinss)
}

#options(repr.plot.width=5, repr.plot.height=4)

plot(1:11, wss_2, type = "b", xlab = "Number of Clusters",
     ylab = "Suma de cuadrados dentro de los clusters")


# Agrupamiento no jerarquico
set.seed(42)
clustering_2 <- kmeans(df_Dem_Zona_Std, centers = 4)


df_Dem_Zona$cluster <- clustering_2$cluster

#dfDemograficasStds$cluster <- clustering$cluster

formula_para_describir_2 <- as.formula(
  paste0( paste(vars_to_cluster, collapse = " + "), " ~ cluster") 
)

tablaResumen_2 <- describeBy(  formula_para_describir_2, 
                               mat = TRUE, 
                               data = df_Dem_Zona
)

tablaResumen_2 =  tablaResumen_2 %>% 
  dplyr::mutate(variable = rownames(.),
                cv       = 100 * sd / abs(mean) ) %>% 
  dplyr::rename(cluster = group1) %>% 
  dplyr::select(variable, cluster, n, mean, median, cv, min, max) %>% 
  arrange(as.numeric(as.character(cluster)))   %>% 
  mutate(variable = substr(variable, 1, nchar(variable)-1))

tablaResumen_2 = tablaResumen_2  %>% 
  merge(desc_campos, by.x = 'variable', by.y = 'campo', all.x = TRUE)

#tablaResumen_2

df_Dem_Zona = df_Dem_Zona  %>% 
  mutate(cluster = as.factor(cluster))

par(mfrow=c(2,2))

ggplot(df_Dem_Zona) +
  aes(x=inc_b, y=cluster) +
  geom_boxplot()  +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "inc_b", y = "Cluster")

ggplot(df_Dem_Zona) +
  aes(x=kids_b, y=cluster) +
  geom_boxplot()  +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "kids", y = "Cluster")

ggplot(df_Dem_Zona) +
  aes(x=prop_b, y=cluster) +
  geom_boxplot()  +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "prop", y = "Cluster")

ggplot(df_Dem_Zona) +
  aes(x=age, y=cluster) +
  geom_boxplot()  +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Edad", y = "Cluster")


ggplot(df_Dem_Zona) + 
  aes(x = zona, fill = cluster) +
  geom_bar(width = 0.75) + 
  labs(y = "Cluster", x = "Zonas") +
  coord_flip()



##2.5 MCA + Kmeans ----
#Aplicamos MCA a las variables de consumo para que sean continuas y luego introducimos todas en un kmeans.

#Seleccionamos las variables numericas
datos_numericos = datos %>% 
  select(-id, -zona)

#Seleccionamos las variables a normalizar
vars = c("kids_r", "prop_r", "resid", "age", "inc_r", "persh", 
         "inc_b", "prop_b", "kids_b")

df_demogr = datos_numericos %>% 
  select(all_of(vars))

#Normalizamos las continuas
df_demogr = scale(df_demogr)

df_consumo = datos_numericos %>% 
  select(!all_of(vars))

#Cambiamos los tipos de datos a booleanas
df_consumo = df_consumo %>% 
  mutate(var_0001 = as.factor(var_0001),
         var_0002 = as.factor(var_0002),
         var_0003 = as.factor(var_0003),
         var_0004 = as.factor(var_0004),
         var_0005 = as.factor(var_0005),
         var_0006 = as.factor(var_0006),
         var_0007 = as.factor(var_0007),
         var_0008 = as.factor(var_0008),
         var_0009 = as.factor(var_0009),
         var_0010 = as.factor(var_0010),
         var_0011 = as.factor(var_0011),
         var_0012 = as.factor(var_0012)
  )

#Aplicamos MCA a las variables de consumo

res_MCA = FactoMineR::MCA(df_consumo, ncp = 6)

print(res_MCA)

#Eigen values
eig_val = res_MCA$eig
eig_val

df_values_MCA = get_mca_ind(res_MCA)$coord

barplot(eig_val[, 2], 
        names.arg = 1:nrow(eig_val), 
        main = "Variances Explained by Dimensions (%)",
        xlab = "Principal Dimensions",
        ylab = "Percentage of variances",
        col ="steelblue"
)
lines(x = 1:nrow(eig_val), eig_val[, 2], 
      type = "b", pch = 19, col = "red")


#Armamos el df de nuevo
datos_numericos = cbind(df_demogr, df_values_MCA)


datos_numericos = datos_numericos %>% 
                  as.data.frame() %>% 
                  clean_names() %>% 
                  as.data.frame()
                  

#Volvemos a correr K-means con las variables demograficas y las 8 componentes que representan las variables de consumo

set.seed(42)
clustering_3 = kmeans(datos_numericos, centers = 4)

formula_para_describir_3 = as.formula(
  paste0( paste(names(datos_numericos), collapse = " + "), " ~ cluster") 
)



datos_numericos$cluster <- clustering_3$cluster

tablaResumen_3 <- describeBy(  formula_para_describir_3, 
                               mat = TRUE, 
                               data = datos_numericos
)

tablaResumen_3 =  tablaResumen_3 %>% 
  dplyr::mutate(variable = rownames(.),
                cv       = 100 * sd / abs(mean) ) %>% 
  dplyr::rename(cluster = group1) %>% 
  dplyr::select(variable, cluster, n, mean, median, cv, min, max) %>% 
  arrange(as.numeric(as.character(cluster)))   %>% 
  mutate(variable = substr(variable, 1, nchar(variable)-1))

tablaResumen_3 = tablaResumen_3  %>% 
  merge(desc_campos, by.x = 'variable', by.y = 'campo', all.x = TRUE)

#tablaResumen_2

datos_numericos = datos_numericos  %>% 
  mutate(cluster = as.factor(cluster))

par(mfrow=c(2,2))

ggplot(datos_numericos) +
  aes(x=inc_b, y=cluster) +
  geom_boxplot()  +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "inc_b", y = "Cluster")

ggplot(datos_numericos) +
  aes(x=kids_b, y=cluster) +
  geom_boxplot()  +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "kids", y = "Cluster")

ggplot(datos_numericos) +
  aes(x=prop_b, y=cluster) +
  geom_boxplot()  +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "prop", y = "Cluster")

ggplot(datos_numericos) +
  aes(x=age, y=cluster) +
  geom_boxplot()  +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Edad", y = "Cluster")

clusters = as.data.frame(clustering_3$cluster)

###Acá lo que intentamos es al DF original, pegarle los clusters

datos_final = datos %>% 
              select(-id, -zona)

datos_final$cluster <- clustering_3$cluster
datos_final = datos_final %>% 
              mutate(cluster = as.factor(cluster))


# Change histogram plot line colors by groups



datos_final %>% 
  tabyl(cluster,var_0001) %>% 
  adorn_totals(c("col")) %>% 
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns() %>% 
  adorn_title(placement = "top" ,"Clusters - var 1")
