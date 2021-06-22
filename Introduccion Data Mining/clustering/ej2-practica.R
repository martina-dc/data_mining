library(tidyr)
library(arules)
library(knitr)
library(arulesViz)
library(here)
library(tidyverse)
library(DescTools)

path = here("datos")

data_name = "consumos_2018.csv"

df_consumo = read.csv(file = file.path(path, data_name),
                    sep = ";", dec = ",", header = T
                      )
head(df_consumo)
str(df_consumo)
dim(df_consumo)

#paso todo a factor
df_consumo[12:23] = data.frame(
                              apply(
                                    df_consumo[12:23],
                                    2,
                                    function(x) as.factor(x)
                                    )
                              )



str(df_consumo)

mediana_prop_b = median(df_consumo$prop_b)

#loi mismo de la zona 5
df_zona_5 = df_consumo[ df_consumo$zona == 5, ]
head(df_zona_5)
str(df_zona_5)
summary(df_zona_5)

df_consumo = df_consumo %>% 
             mutate(prop_b_dic = case_when(
                                           prop_b > mediana_prop_b ~ 1,
                                           TRUE ~ 0
               
                                            )
                    )


