#___________________________________
# 30/07/2024
# Script con el manejo de la master database, de ahí saldrá toda la información
# Este script crea:
# dfvarMaster: contiene los nombres de las variables y su descripción
# dfestufasMadre: es el excel convertido en dataframe depurada
# dfestufasTodasVisitas: la dataframe con las variables que se usan para el análisis
# Cuando cambien los datos, deberemos correr de nuevo este script para actualizar
# las tablas, figuras y tx estadístico
# Actualizaciones de Estufas master database.xlsx: 14/08/2024, 18/08/2024
#____________________________________

# ASIGNACIÓN NOMBRES DE LAS COLUMNAS Base Estufas master database --------------------------------------

library(tidyverse)
library(readxl)
dfestufasMaster <- read_excel("exceles/Estufas master database.xlsx")
#View(dfestufasMadre)
descripMaster <- unlist(as.character(dfestufasMaster[1,]))
descripMaster
nombresMaster <- c('studid', 'visit', 'edad', 'escol', 'ocup_casa', 'ocup_campesino', 'ocup_diaria',
                   'ocup_comerciante', 'ocup_gobierno', 'ocup_particular', 'ocup_otro',
                   'num_adultos', 'num_ninios', 'num_menores5', 'enf_cronicas', 'diabetes',
                   'has', 'obesidad', 'renal', 'epoc', 'cancer', 'musesq', 'otraenf', 'nomotraenf',
                   'medicacion', 'tabaquismo', 'ultimafumada', 'tiempotabaq', 'tiemponotabaq',
                   'otrosfuman', 'ultimafumadaotros','tiempofumapasivo',
                   'cocinadentro', 'cocinacuarto', 'cocinatecho', 'cocinacerrada', 'cocinacircaire',
                   'cocinamatutina', 'cocinavespertina', 'cocinanoche', 'numvecescocina','cocinahoras', 'comenencocina',
                   'horasfamiencocina', 'combu_carbon', 'combu_lenia', 'combu_gas', 'combu_ecoestufa',
                   'combu_otro', 'nom_combu_otro', 'humo', 'usoeco_horas', 'usoeco_dias',
                   'usogas_horas', 'usogas_dias', 'usolenia_horas', 'usolenia_dias', 'usocarbon_horas',
                   'usocarbon_dias', 'usototal_horasporsemana', 'usoeco_porc_hsem', 'usogas_porc_hsem',
                   'usolenia_porc_hsem', 'usocarbon_porc_hsem', 'tiempo_humo', 'combu_eco_lenia',
                   'combu_eco_carbon', 'combu_eco_otro', 'humodania','porque_humo_dania', 'bene_eco_rapida',
                   'bene_eco_menoslenia', 'bene_eco_menostiempo', 'bene_eco_otros', 'cant_combu_antes_eco',
                   'cant_combu_antes_eco_carbon', 'cant_combu_antes_lenia','cant_combu_con_eco_porsem',
                   'tos', 'flema', 'pechoapret', 'disnea', 'limactiv', 'salircasa', 'dormir', 'energia',
                   'epoctotal', 'mmrc', 'sintrespninios', 'num_sintrespninios', 'cefalea', 'irriojos', 'dolor',
                   'congenasal', 'ansiedad', 'preocup', 'desinteres', 'depresion', 'phq4total',
                   'pss1', 'pss2', 'pss3', 'pss4', 'pss5', 'pss6', 'pss7', 'pss8', 'pss9', 'pss10', 'psstotal',
                   'psqi1', 'psqi2','psqi3','psqi4','psqi5','psqi6','psqi7','psqi8','psqi9','psqi10','psqi11',
                   'psqi12', 'psqi13','psqi14','psqi15','psqi16','psqi17','psqi18','psqi19', 'psqicomponent1',
                   'psqicomponent2','psqicomponent3', 'psqicomponent4', 'psqicomponent5', 'psqicomponent6',
                   'psqicomponent7', 'psqitotal', 'studid1', 'fecha1', 'tas', 'tad', 'pulso', 
                   'spo2', 'glucosa',
                   'enayunas', 'peso', 'talla', 'imc', 'usoclinica', 'studid2', 'fecha2',
                   'ubicacion1', 'estufaenuso', 'pm25', 'pm10', 'particles', 'co2',
                   'hcho', 'temp', 'porcrh', 'fotococina1', 'fotococina2',
                   'ubicacion_2', 'pm10_2', 'pm25_2', 'particles_2', 'co2_2',
                   'hcho_2', 'temp_2', 'porcrh_2', 'fotoubicacion', '171', '172', '173', '174')

dfvarMaster <- data.frame(variable = nombresMaster,
                          descripcion = descripMaster)
#Para que salgan como numéricas, vuelvo a importar el escel pero desde la fila 2.
library(readxl)
dfestufasMadre <- read_excel("exceles/Estufas master database.xlsx", 
                             skip = 1)
names(dfestufasMadre) <- nombresMaster
# Salvamos los nombres de las variables con su descripcón y la base madre:
save(dfvarMaster, file = 'Rdata/dfvarMaster.Rda')
save(dfestufasMadre, file = 'Rdata/dfestufasMadre.Rda')

# CREAR DFESTUFASTODASVISITAS ---------------------------------------------


library(tidyverse)
load('Rdata/dfestufasMadre.Rda')

nombresV1 <- c('studid', 'visit','edad', 'escol', 'ocup_casa', 'ocup_campesino', 'ocup_diaria',
               'ocup_comerciante', 'ocup_gobierno', 'ocup_particular', 'ocup_otro', 
               'num_adultos', 'num_ninios', 'enf_cronicas', 'diabetes',
               'has', 'obesidad', 'renal', 'epoc', 'cancer', 'musesq', 'otraenf',
               'tabaquismo', 'otrosfuman', 'tiempofumapasivo',
               'cocinadentro', 'cocinacuarto', 'cocinatecho', 'cocinacerrada', 'cocinacircaire',
               'numvecescocina','cocinahoras', 'comenencocina',
               'horasfamiencocina', 'combu_carbon', 'combu_lenia', 'combu_gas', 'combu_ecoestufa',
               'combu_otro', 'humo', 'usoeco_porc_hsem',
               'usogas_porc_hsem',
               'usolenia_porc_hsem', 'usocarbon_porc_hsem', 'tiempo_humo', 'combu_eco_lenia',
               'combu_eco_carbon', 'combu_eco_otro', 'humodania','bene_eco_rapida',
               'bene_eco_menoslenia','bene_eco_menostiempo', 'bene_eco_otros', 
               'epoctotal', 'mmrc', 'sintrespninios', 'num_sintrespninios', 'cefalea', 'irriojos', 'dolor',
               'congenasal', 'phq4total', 'psstotal',
               'psqitotal', 'tas', 'tad', 'pulso', 
               'spo2', 'glucosa', 'enayunas',  'imc','usoclinica', 
               'ubicacion1', 'estufaenuso', 'pm25', 'pm10', 'ubicacion_2', 
               'pm10_2', 'pm25_2')

#Creo la variable grupo, quito la fila de separación y acomodo la variable grupo
dfestufasTodasVisitas <- dfestufasMadre |>
  select(all_of(nombresV1)) |>
  mutate(grupo = ifelse(str_starts(str_to_upper(studid), 'C'), 'Ecostove', 'Control')) |>
  filter(!is.na(grupo)) |>
  select(1, 80, 2:79) |>
  mutate(comenencocina = ifelse(str_detect(str_to_lower(comenencocina), 'cocina'), 1, 0))

# NOTA: la variable comenencocina en la master db estaba como lugar donde comen,
# Rebacca la convirtió en dicotómica (come en cocina si o no.), por eso el código anterior


#Transformo las variables categóricas:
nombresDicoto <- c('ocup_casa', 'ocup_campesino', 'ocup_diaria',
                   'ocup_comerciante', 'ocup_gobierno', 'ocup_particular', 'ocup_otro', 
                   'enf_cronicas', 'diabetes',
                   'has', 'obesidad', 'renal', 'epoc', 'cancer', 'musesq', 'otraenf',
                   'tabaquismo', 'otrosfuman', 
                   'cocinadentro', 'cocinacuarto', 'cocinatecho', 'cocinacerrada', 'cocinacircaire',
                   'comenencocina',
                   'combu_carbon', 'combu_lenia', 'combu_gas', 'combu_ecoestufa',
                   'combu_otro', 'humo',
                   'combu_eco_lenia',
                   'combu_eco_carbon', 'combu_eco_otro', 'humodania','bene_eco_rapida',
                   'bene_eco_menoslenia','bene_eco_menostiempo', 'bene_eco_otros', 
                   'sintrespninios', 'enayunas','usoclinica', 
                   'estufaenuso')
dfestufasTodasVisitas <- dfestufasTodasVisitas |>
  mutate(across(all_of(nombresDicoto), as.character)) |>
  mutate(visit = as.factor(visit))

#Data wrangling:
# Se detectó un problema: en el lugar de la cocina, se nota que los missing en realidad son ceros.
# primero corrijo esto:
dfestufasTodasVisitas <- dfestufasTodasVisitas |>
  mutate(across(all_of(c('cocinadentro', 'cocinacuarto', 'cocinatecho', 'cocinacerrada')),
                ~ ifelse(is.na(.x), '0', .x)))

# el 14/08/2024 se trabajan las variables de OTHER SYMPTOMS, Rebecca actualizó el Excel 
# Estufas master database.xlsx y convirtió en categóricas las variables. Por eso el siguiente
# código:
varOtrosSintomas <- c('cefalea', 'irriojos', 'dolor', 'congenasal')
dfestufasTodasVisitas <- dfestufasTodasVisitas |>
  mutate(across(all_of(varOtrosSintomas), ~ factor(.x, labels = c('Never',
                                                    'Occasionally',
                                                    'More than half of the time',
                                                    'Daily'))))
  
save(dfestufasTodasVisitas, file = 'Rdata/dfestufasTodasVisitas.Rda')



rm(list = ls())
