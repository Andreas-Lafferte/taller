#*******************************************************************************************************************
#
#                          APRENDIENDO METODOLOGÍA POR VALENTINA
#           TALLER N° 7: Introducción a la manipulación y análisis descriptivo
#                                     de datos en R
#
#                           8 de junio del 2023
#                     Daniela Olivares y Andreas Laffert
#
#       Código práctica 01: Procesamiento, limpieza y manipulación de datos en R.
#
#******************************************************************************************************************


# 1. Cargar librerias -----------------------------------------------------

#Usualmente para cargar paquetes lo hacemos de la siguiente manera:

install.packages("paquete")
library(paquete)

#Pero en esta ocasión utilizaremos un paquete llamado pacman,
# que facilita y agiliza la lectura (instalación y carga)

install.packages("pacman")
library(pacman)

# Cargamos los paquetes de la sesion

pacman::p_load(tidyverse, # colección de paquetes para manipulación de datos
               dplyr, # para manipular datos
               haven, # para importar datos
               car, # para recodificar datos
               magrittr)# para manipular datos

options(scipen = 999) # para desactivar notacion cientifica
rm(list = ls()) # para limpiar el entorno de trabajo


# 2. Importar datos -------------------------------------------------------

# Como hemos mencionado, nuestros datos los dejaremos en la carpeta input/data de nuestro proyecto.

elsoc_2022 <- read_sav("ELSOC W06 v1.0 SPSS.sav")  # No funciona

elsoc_2022 <- read_sav("input/data/ELSOC W06 v1.0 SPSS.sav") # No funciona

elsoc_2022 <- haven::read_sav("input/data/ELSOC_W06_v1.0_SPSS.sav") #Si funciona!


# 3. Explorar datos -------------------------------------------------------

View(elsoc_2022) # Ver datos

names(elsoc_2022) # Nombre de columnas

dim(elsoc_2022) # Dimensiones

str(elsoc_2022) # Estructura de los datos (las clases y categorias de repuesta)


# 4. Limpiar datos --------------------------------------------------------

# En este práctico solo nos centraremos en manipular datos con dplyr() incorporando el uso de %>%

# 4.1 Seleccionar -----------------------------

# La estructura general del comando siempre es select(datos, variable1, variable2, variable3)

#Por indexación o ubicación en la base de datos:

elsoc_2022 %>% dplyr::select(1, 2) # la primera y la segunda columna

elsoc_2022 %>% dplyr::select(1:4) # la primera hasta la cuarta columna

elsoc_2022 %>% dplyr::select(c(1, 4, 5)) # la primera, la cuarta y la quinta columna

# Por nombre de la variable/columna

elsoc_2022 %>% 
  dplyr::select(m0_sexo, m0_edad, m13)

# Renombrar las variables al momento de seleccionarlas,

elsoc_2022 %>% 
  dplyr::select(sexo = m0_sexo, edad = m0_edad, ingreso = m13)

# Para reordenar nuestras variables

elsoc_2022 %>% 
  dplyr::select(m0_edad, m0_sexo, c25, m13)

## ---- ¡APLIQUEMOS CONOCIMIENTOS!----

# En este ejemplo utilizaremos las siguientes variables:
#
#  m0_sexo: sexo del entrevistado
# m0_edad: edad del entrevistado
# m13: ingreso mensual entrevistado
# c25: preferencia entre autoritarismo y democracia
# f05_01: justificación violencia hacia delincuentes

# Creamos una nueva base procesada

proc_elsoc <- elsoc_2022 %>% 
  dplyr::select(edad = m0_edad,
                sexo = m0_sexo,
                ingreso = m13,
                altruismo = c03,
                confianza_carab = c05_03)

proc_elsoc

# 4.2 Filtrar -----------------------------

# Usamos filter() especifcando las condiciones que queremos aplicar

proc_elsoc %>% 
  dplyr::filter(autor_democ == 1)

#También podemos agregar muchas condiciones para filtrar nuestros datos.

proc_elsoc %>% 
  dplyr::filter(autor_democ == 1 & edad >= 25)

# ¿Y si son character o factor?

proc_elsoc %>% 
  dplyr::filter(sexo == "Mujer")

## ---- ¡APLIQUEMOS CONOCIMIENTOS!----

# Filtremos nuestros datos quedándonos solo con aquellos casos o
# personas que tengan o sean mayores a 25 años de edad.

proc_elsoc <- proc_elsoc %>% dplyr::filter(edad >= 25)

proc_elsoc

# 4.3 Recodificar  -----------------------------

# Hay múltiples formas de recodificar en R, pero en este ejemplo trabajaremos
# con el comando recode() del paquete car.

# Recodificaremos las siguientes variables: sexo, ingreso, autor_democ y jv_delincuentes.

# Esto lo haremos con la funcion mutate()

# Sexo e ingresos:

proc_elsoc %>%
  dplyr::mutate(sexo = car::recode(sexo, recodes = c("'Hombre' = 'Masculino';
                                                   'Mujer' = 'Femenino'")))


proc_elsoc %>%
  dplyr::mutate(ingreso = car::recode(ingreso, recodes = c("-888 = NA; -999 = NA")))


## ---- ¡APLIQUEMOS CONOCIMIENTOS!----

# Recodifiquemos las variables sexo e ingresos 
# pero ahora sobre escribiendolas en nuestra base de datos


proc_elsoc <- proc_elsoc %>% 
  dplyr::mutate(sexo = car::recode(sexo,
                                   recodes = c("'Hombre' = 'Masculino'; 'Mujer' = 'Femenino'")),
                ingreso = car::recode(ingreso, 
                                      recodes = c("-888 = NA; -999 = NA")))

proc_elsoc


# Altruismo y confianza en carabineros conviertiendolos a factor

proc_elsoc <- proc_elsoc %>%
  dplyr::mutate(altruismo = car::recode(altruismo,
                                        recodes = c("1 = 'La mayoria de las veces tratan de ayudar a los demas';
                                              2 = 'La mayoria de las veces se preocupan solo de si mismas';
                                              3 = 'Depende';
                                              -666 = NA;
                                              -777 = NA;
                                              -888 = NA;
                                              -999 = NA"),
                                        as.factor = TRUE), # transformar a factor
                confianza_carab = car::recode(confianza_carab,
                                              recodes = c("1 = 'Nada';
                                                    2 = 'Poca';
                                                    3 = 'Algo';
                                                    4 = 'Bastante';
                                                    5 = 'Mucha';
                                                    -666 = NA;
                                                    -777 = NA;
                                                    -888 = NA;
                                                    -999 = NA"),
                                              as.factor = TRUE, # transformar a factor
                                              levels = c("Nada",
                                                         "Poca",
                                                         "Algo",
                                                         "Bastante",
                                                         "Mucha"))) # asignar niveles

proc_elsoc

# Los valores -888 y -999 fueron codificados como valores pérdidos ya que
# estos valores significan no sabe y no responde, respectivamente.

# 4.4 Tratamiento casos perdidos -----------------------------

is.na(proc_elsoc)

is.na(proc_elsoc$ingreso)

sum(is.na(proc_elsoc)) # total base

colSums(is.na(proc_elsoc)) # total por columna/variable

proc_elsoc <- na.omit(proc_elsoc)

proc_elsoc


# 5. Transformar variables ------------------------------------------------

# Transformaremos las variables edad e ingresos, y crearemos una nueva variable
# llamada año de la encuesta y otra llamada ingreso_minimo.


# Nueva variable ano

proc_elsoc <- proc_elsoc %>% dplyr::mutate(ano = 2022)

proc_elsoc

# Edad e ingresos en tramos con case_when()

proc_elsoc <- proc_elsoc %>%
  dplyr::mutate(tramo_edad = case_when(edad <= 29 ~ "Jovenes",
                                       edad >= 30 & edad <= 59 ~ "Adultos",
                                       edad >= 60 ~ "Adutos mayores"))


proc_elsoc

proc_elsoc <- proc_elsoc %>% 
  dplyr::mutate(tramo_ingreso = case_when(ingreso <= 250000 ~ "Tramo 1",
                                          ingreso > 250000 & ingreso <= 500000 ~ "Tramo 2",
                                          ingreso > 500000 & ingreso <= 750000 ~ "Tramo 3",
                                          ingreso > 750000 & ingreso <= 1000000 ~ "Tramo 4",
                                          ingreso > 1000000 ~ "Tramo 5"))

proc_elsoc

names(proc_elsoc)

# Ingreso minimo con if_else()

proc_elsoc <- proc_elsoc %>%
  dplyr::mutate(ingreso_minimo = if_else(ingreso < 410000, "debajo minimo", "sobre minimo"))

select(proc_elsoc, ingreso, ingreso_minimo) #veamosla!


# 6. Guardar y exportar ---------------------------------------------------

saveRDS(proc_elsoc, file = "output/datos_proc.Rdata")
