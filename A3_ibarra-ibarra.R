library(tidyverse)
library(readxl)
library(haven)
library(data.table)
library(sjlabelled)
library(sjPlot)
library(mice)
library(survey)

# DATOS -------------------------------------------------------------------
getwd()
#setwd('DSci-ALIL/actividad3/dsci-actividad3')
personas <- read_sav("datasets/encovi_personas2017_ds.sav") 

hogares <- read_sav("datasets/encovi_hogares2017_ds.sav")

# SET UP ------------------------------------------------------------------


# Inspeccionar tabla
glimpse(personas)

# Inspeccionar columnas y etiquetas
personas$CMHP17 %>% class
personas$CMHP17 %>% attr('label')
personas$CMHP17 %>% attr('labels')

# Ver todas las etiquetas
view_df(personas)

# PERSONAS
new_names_pers <- c("id_hogar", "id_per", "parentesco", "edad", "sexo", 
                    "sit_conyu", "nivel_edu", "edu_ano_aprobado", "edu_sem_aprobado",
                    "tipo_edu", "sit_econo", "sector_eco", "cat_ocupa",
                    "trab_remun", "ing_laboral", "horas_trab", "ing_otro",
                    "ing_pension",
                    "pesop", "grp_edad", "anos_edu", "tipo_ciudad")

# Renombrar
personas <- personas %>%
  setnames(old = colnames(.), new = new_names_pers) %>%
  
  # Convierte los identificadores a caracteres
  mutate(across(.cols = c("id_hogar", "id_per"),
                .fns = as.character))


# HOGARES

# Ver todas las etiquetas
view_df(hogares)

new_names_hogs <- c("id_hogar", "tipo_ciudad", "fecha_ent",
                    "tipo_viv", "agua", "excretas", 
                    "per_x_hog", "num_cuartos", "num_duchas", "num_banos", "num_carros",
                    "nevera", "lavadora", "secadora",
                    "clap",
                    "quintil", "pob_ing", "pob_nbi", "pob_integral",
                    "pesoh", "hacinamiento", "incidencia")

hogares <- hogares %>%
  setnames(old = colnames(.), new = new_names_hogs) %>%
  
  # Convierte los identificadores a caracteres
  mutate(id_hogar = as.character(id_hogar))


# EXPLORAR ----------------------------------------------------------------

# Variable de parentesco
table(personas$parentesco)

# Cuantos hogares tienen pareja?
# Asegurate que no haya ningun error
parejas_tot <- personas %>%
  mutate(pareja = ifelse(parentesco == 2, yes = 1, no = 0)) %>%
  group_by(id_hogar) %>%
  summarise(pareja = sum(pareja, na.rm = T)) 

# Utiliza el dataframe creado para calcular la proporcion de hogares con pareja
sum(parejas_tot$pareja)
mean(parejas_tot$pareja)
# FALTAN LOS PESOS MUESTRALES DE HOGARES PARA QUE ESTA PROPORCION SEA CORRECTA




# RECORDATORIO: NAs 98 y 99
personas[personas == 98 | personas == 99] <- NA
hogares[hogares == 98 | hogares == 99] <- NA

# IMPUTACIONES ------------------------------------------------------------


# Cuantos NA hay por columna en la base de hogares
sapply(hogares, function(x) sum(is.na(x)))


# Revisar cuantas personas declaran trabajar de forma remunerada pero no
# reportan ingresos validos

# Probar asi
sum(personas$trab_remun == 1 & (personas$ing_laboral > 0 | is.na(personas$ing_laboral)))

# Corregir

sum(is.na(personas$ing_laboral) & (!is.na(personas$trab_remun) & personas$trab_remun == 1))


sum(!is.na(personas$trab_remun) & personas$trab_remun == 1 & (personas$ing_laboral <= 0 | is.na(personas$ing_laboral)))

# Alternativa con dplyr
nas_ing <- personas %>%
  filter(trab_remun == 1 & !is.na(trab_remun) &
           (ing_laboral <= 0 | is.na(ing_laboral)))


# Veamos la situacion laboral de este grupo
table(nas_ing$sit_econo)

# Solución ----------------------------------------------------------------

# Objetivo
#     Imputar a aquellas personas que deberían tener ingresos laborales reportados
#   pero no tienen valores asociados. Para ello usaremos como base los datos de nas_ing
#   calculados previamente en el script.

# Criterios para elección de donantes
#   Sexo
#   Grupo de Edad
#   Tipo de Ciudad
#   Sector económico

#   Consideramos que para tener datos representativos que permitan imputar los
#   valores faltantes, la clasificación de acuerdo a los criterios escogidos puede
#   arrojar un resultado que se acerque a la realidad. Tomando en cuenta no solo
#   indicadores como el grupo de edad y el sexo sino también el tipo de ciudad y
#   el sector económico para obtener la lista de donantes que permitan calcular
#   el promedio del ingreso laboral.

# Paso 1. Crearemos función de imputar

#' imputar
#' 
#' Método que permite calcular, dadas ciertas características de una persona, 
#' el ingreso laboral promedio del grupo de donantes y el número de donantes 
#' que poseen dichas características.
#'
#' @param sx - Sexo asociado a una persona
#' @param grup_ed - Grupo edad asociado a una persona
#' @param ciudad - Tipo ciudad asociado a una persona
#' @param sector - Sector económico asociado a una persona
#'
#' @return - Lista de valores con promedio de ingreso laboral y número de donantes
#' @export
#'
#' @examples
imputar <- function(sx, grup_ed, ciudad, sector) {
  donantes <- personas %>%
    select(sexo, grp_edad, sector_eco, ing_laboral, tipo_ciudad, pesop) %>%
    filter(
      sexo == sx,
      grp_edad == grup_ed,
      !is.na(ing_laboral),
      tipo_ciudad == ciudad,
      sector_eco == sector
    )
  
  valores <-
    list(
      "ing_laboral" = mean(donantes$ing_laboral, na.rm = T),
      "nro_donantes" = nrow(donantes),
      "ing_pm" = weighted.mean(donantes$ing_laboral,  donantes$pesop, na.rm = T)
    )
  return(valores)
}

# Paso 2. Extraemos de la tabla nas_ing las características a evaluar, para obtener
# todos los grupos de donantes. 

# Obtenemos las combinaciones únicas de todas las variables.

grupos_donantes <-
  unique(nas_ing[, c("sexo", "grp_edad", "tipo_ciudad", "sector_eco")])

# Paso 3. Aplicamos la función de imputar para cada grupo de donante y calcular
# el promedio de ingresos y el número de donantes del grupo

# Los cálculos realizados son complejos ya que por cada fila se hace un select y filter,
# la ejecución de este ciclo puede demorar hasta 30s dependiendo de las especificaciones
# del computador

for (i in 1:nrow(grupos_donantes)) {
  valores <-
    imputar(grupos_donantes$sexo[i],
            grupos_donantes$grp_edad[i],
            grupos_donantes$tipo_ciudad[i],
            grupos_donantes$sector_eco[i])
  grupos_donantes$ing_imp[i] <- valores$ing_laboral
  grupos_donantes$n_imp[i] <- valores$nro_donantes
  grupos_donantes$ing_imp_pm[i] <- valores$ing_pm
}

# Paso 4.Sustituimos valores faltantes 

# En esta tabla podremos ver, por cada grupo de las variables escogidas, el ingreso
# laboral promedio del grupo y el número de donantes usado

grupos_donantes$ing_imp[is.nan(grupos_donantes$ing_imp)]<-NA


# Paso 5.Unimos los valores calculados con la tabla de nas_ing 
res_ing_imp  <- nas_ing %>% 
  select(-ing_laboral) %>% 
  left_join(grupos_donantes, by=c("sexo","grp_edad", "tipo_ciudad", "sector_eco"))


# Paso 6.Calculamos el porcentaje de valores faltantes imputados exitosamente
sum(!is.na(res_ing_imp$ing_imp))/sum(nrow(res_ing_imp))*100

# Vemos que el porcentaje es bastante alto, si observamos aquellos que no fueron
# imputados es porque no se encontraron donantes con las características dadas, 
# el valor de la columna de n_imp  es 0.

res_ing_imp[is.na(res_ing_imp$ing_imp),]

# Si se desea imputar estos valores en otra iteración, podría ejecutarse el método 
# de imputar reduciendo el número de categorías. Es importante destacar que esto
# no arrojaría un resultado tan cercano a la realidad, ya que estamos ampliando el scope.


# Paso 7. Unimos los valores calculados con la tabla de personas

# En la columna de ing_laboral_imp encontraremos tanto los valores originales de
# ing_laboral y, si esa persona no reportó ingresos anteriormente, el valor será
# el imputado de acuerdo al grupo

personas_imp <- personas %>% 
  left_join(res_ing_imp) %>% 
  mutate(ing_laboral_imp = coalesce(ing_laboral, ing_imp))


# Para verificar lo realizado, calculamos nuevamente el número de valores faltantes
# y vemos que disminuyó considerablemente. Pasando de 1236 antes de la imputación
# a 57 después de la misma. 

nas_ing_imp <- personas_imp %>%
  filter(trab_remun == 1 & !is.na(trab_remun) &
           (ing_laboral_imp <= 0 | is.na(ing_laboral_imp)))

