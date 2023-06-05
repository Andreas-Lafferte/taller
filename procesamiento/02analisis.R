#*******************************************************************************************************************
#
#                          APRENDIENDO METODOLOGÍA POR VALENTINA
#           TALLER N° 7: Introducción a la manipulación y análisis descriptivo
#                                     de datos en R
#
#                           8 de junio del 2023
#                     Daniela Olivares y Andreas Laffert
#
#       Código práctica 02: Análisis descriptivo de datos en R.
#
#******************************************************************************************************************

# 1. Cargar librerías -----------------------------------------------------

install.packages("pacman") #para instalar (NO NECESARIO SI YA LA TENGO INSTALADA)
library(pacman) # para llamar/cargar

pacman::p_load(tidyverse, # colección de paquetes para manipulación de datos
               dplyr, # para manipular datos
               psych, # para analizar datos
               sjmisc, # para analizar datos
               crosstable) # para tablas de contingencia

options(scipen = 999) # para desactivar notacion cientifica
rm(list = ls()) # para limpiar el entorno de trabajo

# 2. Importar datos -------------------------------------------------------

datos_proc <- readRDS("output/datos_proc.Rdata")

# 3. Explorar datos -------------------------------------------------------

View(datos_proc) # Ver datos
names(datos_proc) # Nombre de columnas
dim(datos_proc) # Dimensiones
str(datos_proc) # Estructura de los datos (las clases y categorias de repuesta)

# 4. Analisis -------------------------------------------------------------

# 4.1 Variables categoricas ----

#### 4.1.1 Frecuencias 

##### a) Absolutas y relativas

table(datos_proc$sexo)

table(datos_proc$ingreso_minimo)

table(datos_proc$altruismo)

(freq_table1 <-table(datos_proc$altruismo))
prop.table(freq_table1)*100

##### b) Acumuladas

(freq_table2 <- table(datos_proc$tramo_ingreso))
(freq_table3 <- prop.table(freq_table2)*100)
cumsum(freq_table3)

tbl3 <- table(datos_proc$tramo_ingreso)
cbind(Freq=tbl3, relat = prop.table(tbl3)*100, Cum = cumsum(tbl3))

##### Con frq() de sjmisc ##

sjmisc::frq(datos_proc$tramo_ingreso)


## 4.1.2 Tablas de contingencia (o doble entrada)

table(datos_proc$sexo, datos_proc$tramo_edad)

table(datos_proc$sexo, datos_proc$tramo_edad) %>%
  addmargins()

crosstable(datos_proc, cols = tramo_edad, by = sexo, total = "both")

crosstable(datos_proc, cols = sexo, by = tramo_edad, total = "both")

crosstable(datos_proc, cols = sexo, by = tramo_edad, total = "row")

crosstable(datos_proc, cols = sexo, by = tramo_edad, total = "column")


# 4.2 Variables numericas ----

##### a) Con `summary` 

summary(datos_proc$edad)

summary(datos_proc$ingreso)

##### b) Con `psych`

psych::describe(datos_proc$edad)

psych::describe(datos_proc$edad,
                quant = c(.25,.75),
                IQR = TRUE)

psych::describe(datos_proc$ingreso,
                quant = c(.25,.75),
                IQR = T)

##### c) Con `summarise` de `dplyr`

datos_proc %>% 
  summarise(media = mean(ingreso),
            mediana = median(ingreso),
            q1 = quantile(ingreso, probs = .25),
            q2 = quantile(ingreso, probs = .75),
            rango = max(ingreso) - min(ingreso),
            desviacion_estandar = sd(ingreso),
            varianza = var(ingreso),
            coef_variacion = sd(ingreso)/mean(ingreso))
