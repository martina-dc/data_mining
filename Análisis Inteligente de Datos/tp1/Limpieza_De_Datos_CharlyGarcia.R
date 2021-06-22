rm(list=ls())

#Sentencia para asegurar que todos los integrantes del equipo tengan los paquetes instalados:
list_of_packages = c("tidyverse","readxl", "DescTools", "dplyr", "tidyr", "readr", "easypackages", "here","sas7bdat","readtext") #easepackages se utiliza en esta sentencia

new_packages = list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]

if(length(new_packages)) install.packages(new_packages)


#Importamos todos los paquetes que vamos a utilizar 
easypackages::libraries(list_of_packages)

#Ruta donde estan los achivos de trabajo 
path1 = here("Datos")
path_lmdhp_lmdhp = here("Datos","lmdhp","album_la_maquina_de_hacer_pajaros")
path_lmdhp_peliculas = here("Datos","lmdhp","album_peliculas")
path_solista = here("Datos","solista")


#Descomprimimos el archivo de trabajo y creamos la carpeta datos
if (!file.exists(path1)) dir.create(path1)
unzip('datos_charly.zip', exdir = path1)


#Carga de archivos de trabajo
#---- 

#Leemos el porsuigieco
porsuigieco = read_delim(
                        file = file.path(path1,"porsuigieco.txt"),
                        delim = "|"
                        )

#Leemos suigeneris
suigeneris = read_csv(
                      file = file.path(path1,"suigeneris.csv")
                      )

#Leemos serugiran y el df de albums
serugiran = read_excel(
                       path = file.path(path1,"serugiran.xlsx")
                      )

albums = read_excel(
            path = file.path(path1,"albums.xlsx")
                   )

#Leemos bbatj
bbatj = read.sas7bdat(file = file.path(path1,"bbatj.sas7bdat")
                      ) 


#Leemos todos los archivos dentro de la carpeta lmdhp, lmdhp

#Levantamos en un vector todos los archivos
list_of_files_lmdhp = list.files(path = c('Datos/lmdhp/album_la_maquina_de_hacer_pajaros'),
                                pattern = "\\.txt$")
lmdhp  = data.frame() 

#iteramos dentro de nuestro vector de lista de archivos, utilizando como índice el nombre el txt en cada caso
for(f in list_of_files_lmdhp){
  
    #Leemos el txt como se encuentra en el archivo de origen  
    file = read.table(file.path(path_lmdhp_lmdhp, f), header = TRUE, sep = "\t")
    
    #le cambiamos el nombre al df leido, y le ponemos columna, ya que cada txt genera un nombre diferente
    file = rename(file, columna = names(file))
    
    #separamos nuestra única columna en dos, una que tenga el nombre y otra el valor, ahora tenemos 2 cols
    file = separate(file, col = 'columna', sep = ':', into = c('id', 'dato') )
    
    #pivoteamos los datos creando una sola fila y utilizando la columna id como nombres de nuestras nuevas columnas
    file = pivot_wider(file, id_cols = 'id', names_from = 'id', values_from = 'dato')
    
    #adicionamos al final del df creado, nuestro df de una sola fila
    lmdhp  = rbind(lmdhp, file)
}





#Leemos todos los archivos dentro de la carpeta lmdhp, album_peliculas
list_of_files_peliculas = list.files(path = c('Datos/lmdhp/album_peliculas'),
                                 pattern = "\\.txt$")
peliculas  = data.frame() 
for(f in list_of_files_peliculas){
  file = read.table(file.path(path_lmdhp_peliculas, f), header = TRUE, sep = "\t")
  
  file = rename(file, columna = names(file))
  
  file = separate(file, col = 'columna', sep = ':', into = c('id', 'dato') )
  
  file = pivot_wider(file, id_cols = 'id', names_from = 'id', values_from = 'dato')
  
  peliculas  = rbind(peliculas, file)
}


#Leemos el album solista

list_of_albums_solistas = list.files(path = c('Datos/solista'),
                                     pattern = "\\.txt$")
albums_solistas  = data.frame() 

for(album in list_of_albums_solistas){
  file = read.table(file.path(path_solista, album), header = TRUE, sep = "\t", fileEncoding="utf-8",quote = "" )
  albums_solistas  = rbind(albums_solistas, file)
}

rm(file)

#---- 

#Revisión de estado de los archivos
#----
#Analizamos datos faltantes en nuestros archivos cargados

#Albums solistas
sapply(albums_solistas, function(x) sum(is.na(x)))

#Bbatj
sapply(bbatj, function(x) sum(is.na(x)))

#Lmdhp
sapply(lmdhp, function(x) sum(is.na(x)))

#peliculas
sapply(peliculas, function(x) sum(is.na(x)))

#porsuigieco
sapply(porsuigieco, function(x) sum(is.na(x)))

#serugiran
sapply(serugiran, function(x) sum(is.na(x)))

#suigeneris
sapply(suigeneris, function(x) sum(is.na(x)))

#Podemos ver que tenemos muchisimos faltantes en album id y album name

#----

#Creación del archivo entregable
#----

#Juntamos todos aquellos df que tienen los campos correctos para entregarse, antes revisamos los tipos de datos

#solistas todo ok
#bbatj todo ok
#porsuigieco todo ok

str(albums_solistas)
str(bbatj)
str(porsuigieco)


df_resultado = rbind(albums_solistas, bbatj, porsuigieco)

#Serugiran todo ok pero en español
#Transformamos al ingles los nombres de serugiran
names(serugiran) = names(df_resultado)

#Agregamos seru giran al resultado
df_resultado = rbind(df_resultado, serugiran)

#Completamos los datos faltantes en lmdhp y pelicuals con los datos que tenemso en el df de albums
#Los datos faltantes son:  track_number, disc_number, album_name, album_id, album_artist, uri, analysis_url

#Cambiamos los nombres del archivo albums
names(albums) = c("album_id", "album_name", "album_type", "album_artist", "categoria")

#Nos creamos un archivo albums para lmdhp
albums_lmdhp = albums %>% 
               select("album_id", "album_name", "album_artist") %>% 
               filter(album_id == '4uXadaCsBVwLK6s5V14Kjw') 

#Agregamos la info de los albums a el df lmdhp y las columnas faltantes con NAs
lmdhp = cbind(lmdhp, albums_lmdhp) %>% 
        mutate(uri = NA,
               analysis_url = NA,
               track_number = NA,
               disc_number = NA
               )

#Creamos un archivo albums para peliculas
albums_peliculas = albums %>% 
  select("album_id", "album_name", "album_artist") %>% 
  filter(album_id == '6hlwylwLskentQQsbTKpcj')

#Agregamos la info de los albums a el df peliculas y las columnas faltantes con NAs
peliculas = cbind(peliculas, albums_peliculas)%>% 
  mutate(uri = NA,
         analysis_url = NA,
         track_number = NA,
         disc_number = NA
  )



#A Suigeneris le faltan disc_number y album artist
#Ademas vemos que las columnas album id y album name solo tiene correctamente la info del album cargada para la primera cancion


#Creamos un archivo albums para suigeneris
albums_suigeneris = albums %>% 
                    select(album_id, album_artist)

#Completamos todos los NAs con el album_id y album_name correspondientes y creamos la columna faltante
#Además mergeamos con albums_suigeneris para incorporar el dato de album_artist
suigeneris = suigeneris %>% 
            fill(album_id) %>% 
            fill(album_name) %>% 
            mutate(disc_number = NA) %>% 
            merge(albums_suigeneris, by = 'album_id', all.x = TRUE) 

#Compilamos lo que ya teniamos en df_resultado, con los albums faltantes
df_resultado  = rbind(df_resultado, lmdhp, peliculas, suigeneris)

#Ya tenemos nuestro archivo generado, ahora vamos a ver si existe algun error de tipeo en los datos categóricos
sort(table(df_resultado$album_name), decreasing = T)
sort(table(df_resultado$album_artist), decreasing = T)

#Guardamos el archivo de trabajo como txt
write.table(df_resultado,
            file = "resultado.txt",
            quote = TRUE,
            col.names = TRUE,
            row.names = FALSE,
            sep = "\t",
            dec = "." )

#Se validó que se pueda abrir el archivo con el siguiente codigo y funciona correctamente
#archivo = read.table(file = "resultado.txt",
#                     header = TRUE
#                      )
