library(readr)
library(knitr)
library(psych)
library(ggplot2)
library(dplyr)
library(here)
library(ggfortify)
library(grid)
library(gridExtra)

path = here("datos")

data_name = "medifis.txt"

medifis = read.table(file = file.path(path, data_name),sep="\t",dec=".",fileEncoding = "utf8",header = T)
#1 - análisis exploratorio de los datos
summary(medifis)

res = pastecs::stat.desc(medifis)
round(res,2)

pairs.panels(medifis, 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE, # show correlation ellipses
             
             
)


dfInd = stack(medifis)
names(dfInd) = c("valor","variable")
ggplot(dfInd, aes(x = variable, y = valor)) +
  geom_boxplot()+  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#Se encuentea outliers en aes y drt. Tendriamos que sacar el maximo y el minimo. Cual es el criterio de cantidad de outliers???


#no es naturaleza  continua no se agrega a el estudio de PCA
#diametro del creaneo , "molesta"
#1 - análisis exploratorio de los datos
#2 - PCA:
 # a) ¿q matriz de usar, R o S?


X = medifis[,2:ncol(medifis)] #quito la variable sexo

mediPca2 = prcomp(as.matrix(X),center = T, scale. = T)

summary(mediPca2)

##SIN UTILIZA LA VAR. CRANEO
X2 = select(medifis,-sexo,-dcr)  #quito la variable sexo

mediPcaacotado = prcomp(as.matrix(X2),center = T, scale. = T)

summary(mediPcaacotado)


#cual es el costo de tener una variable mas??

  #b) ¿con cuantas PC te quedas?

##Nos quedamos con PC1 y las variable sexo y craneo dcr

  #c) ¿es necesaria una transformación previamente en los datos?
## se podrian sacar los dos outliers

X3 = select(medifis,-sexo,-dcr) #quito la variable sexo
X3 = log(X3)
mediPca3 = prcomp(as.matrix(X3),center = T, scale. = T)

summary(mediPca3)
## no se notas diferencia significativas. Puede ser que no sean outliers .....
  #d) ¿existes valores extremos o outliers?
##si existen dos! Se encuentea outliers en aes y drt. Tendriamos que sacar el maximo y el minimo. 
##Cual es el criterio de cantidad de outliers???



  #e) ¿existe una variable o más que se pueda excluir del análisis o no se justifique su análisis en el PCA?
#sexo y diametro del creas 

  #f) Interpretar las PC seleccionadas.
data.frame(mediPcaacotado)
autoplot(mediPcaacotado, data = medifis, colour = 'sexo', loadings = TRUE, loadings.label = TRUE, loadings.label.size = 3)

autoplot(mediPcaacotado)

dtp <- data.frame('sexo' = medifis$sexo, craneo=medifis$dcr ,mediPcaacotado$x[,1:2]) 

ggplot(data = dtp) + 
  geom_point(aes(x = PC1, y = craneo, col = sexo )) + 
  theme_minimal() 




g1 <- autoplot(mediPcaacotado, frame = TRUE, frame.type = 'norm')
g2 <- autoplot(mediPcaacotado, frame = TRUE, frame.type = 'norm')
require(grid)
require(gridExtra)
grid.arrange(g1,g2, ncol = 2)

biplot(mediPcaacotado)
#recomienda usar las funciones qeu vienen con stat