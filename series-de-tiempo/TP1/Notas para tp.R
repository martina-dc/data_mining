Creamos una lista con los paquetes que utilizaremos en este trabajo
lista_paquetes <- c('here','tseries','forecast', 'astsa','PerformanceAnalytics',
                    'quantmod','Quandl','ggplot2', 'gridExtra','dygraphs',            'PASWR2',
                    'pastecs','psych','lessR')

#Nos quedamos en esta nueva lista solamente con aquellos que no tengamos instalados
nuevos_paquetes <- lista_paquetes[!(lista_paquetes %in% installed.packages()[,"Package"])]

#Si hay mas de 1 no instalado, lo instalamos
if(length(nuevos_paquetes)) 
  install.packages(nuevos_paquetes, dependencies = TRUE)

#Importamos todos los paquetes
suppressWarnings(suppressMessages(easypackages::libraries(lista_paquetes)))


################################
####### SETEO DE CARPETA #######
################################
rm(list = ls())
#Armamos el path a los datos
path  <- here('data')


# Cuando hablamos de la serie que elegimos, hablemos de granularidad de la seri la serie.
# Para convertin la serie en serie de tiempo, se itiliza la funcion TS.

#Ejemplo


names(mortalidad)

data.class(mortalidad$mortalidad_infantil_argentina)

morta = ts(mortalidad$mortalidad_infantil_argentina, 
           frequency = 1, 
           start = c(2011,07,01))

data.class(morta)

data.class(mortalidad$mortalidad_infantil_argentina) ## CHARACTER

autoplot(morta)

## como arreglar que la variable sea un factor ##

mortalidad <- read.csv2(file = "tasa-mortalidad-infantil.csv",
                        sep = ",", 
                        header = T)
View(mortalidad)

names(mortalidad)

data.class(mortalidad$mortalidad_infantil_argentina)

morta = ts(mortalidad$mortalidad_infantil_argentina, 
           frequency = 1, 
           start = c(1990,01))

data.class(morta)

data.class(mortalidad$mortalidad_infantil_argentina) ## CHARACTER

autoplot(morta)

## como arreglar que la variable sea un factor ##

time = as.Date(mortalidad$?..indice_tiempo)
data.class(time)

mortalidad_infantil_argentina <- as.numeric(as.character(mortalidad$mortalidad_infantil_argentina))

mortalidad_argentina = as.xts(mortalidad_infantil_argentina,
                              order.by = time)

data.class(mortalidad_argentina)

autoplot(mortalidad_argentina, 
         main = "Mortalidad Infantil Argentina") + 
  ylab("En %")

ggAcf(mortalidad_argentina)


#2. Graficar las series originales. Analizar si es necesario diferenciarlas para 
#convertirlas en estacionarias (2). Exponer los conceptos relacionados con la 
#estacionariedad de la serie.
#(2En esta parte de la materia la diferenciación la realizarán sin ninguna 
#prueba de hipótesis)

#con esta podriamos graficar la serie original
y1 = autoplot(Ad(GSPC)) + 
  ggtitle("GSPC", subtitle = "En Niveles") + 
  ylab("Valor")

#en esta hace la dif: 

y2 = autoplot(diff(Ad(GSPC))) + 
  ggtitle("GSPC", subtitle = "Primera Diferencia") + 
  ylab("En %")

grid.arrange(y1,y2)


#para saber cuanto diferenciarla
ndiff() #no tiene estacionalidad
nsdiff() #si tiene estacionalidad
 

############################################
# Otras funciones para graficar
ts.plot(white_noise, main = "White Noise", ylab = "")

chart.TimeSeries(white_noise)

dygraph(white_noise)
###########################################

3. Graficar la FAS, FAC y FACP de las mismas en un solo gráfico y analizarlas.
¿Qué puede inferir de los gráficos?. Justificar con teoría.


## FUNCI?N DE AUTOCORRELACI?N ## (FAC) o ACF
g1 = ggAcf(rw_drift)+ 
  ggtitle("Random Walk")