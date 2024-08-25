#_____________________________
# 24/07/2024
# Script que genera la estadística de la visita 1
# incluye las características de los pacientes, que no se preguntan
# en las visitas subsecuentes.
# Depende de script master, si los datos cambian, hay que volver a 
# correr el ScrMaster.R
# Crea: dfpatCharFinalV1
# En 25/08/2024 se crea el repositorio en github, esta línea se creó en lenovo
# esta línea se pone después de git push origin main --force
#___________________________________

library(tidyverse)
load('Rdata/dfestufasTodasVisitas.Rda')
dfestufasV1 <- dfestufasTodasVisitas |>
  filter(visit == 1)
# guardo la base como .csv y como objeto de R
save(dfestufasV1, file = 'Rdata/dfestufasV1.Rda')
write.csv(dfestufasV1, file = 'csvs/dfestufasV1.csv')
dfestufas <- dfestufasV1

# FUNCIONES -----------------------------------------------------

f_PromVarNum <- function(columnas, df = dfestufas){
  dfVarNum<- df |>
    select(all_of(c('grupo', columnas))) |>
    group_by(grupo) |>
    summarise(across(everything(), list(
      promedio = ~round(mean(.x, na.rm = TRUE), 1),
      desvest = ~round(sd(.x, na.rm = TRUE),1)
    ), .names = '{.col}__{.fn}')) |>
    pivot_longer(cols = -1,
                 names_sep = '__',
                 names_to = c('variable', '.value')) |>
    mutate(valor = paste0(promedio, ' ± ', desvest)) |>
    select(grupo, variable,valor) |>
    pivot_wider(names_from = grupo, values_from = valor)
  dfVarNum
}

f_frecCategos <- function(columnas, df = dfestufas){
  df_categoricas <- df |>
    select(all_of(c('grupo', columnas))) |>
    pivot_longer(all_of(c(columnas)), names_to = 'variable', values_to = 'categos') %>%
    group_by(grupo, variable, categos) %>%
    na.omit %>%
    summarise(n = n()) %>%
    mutate(frec = round(n/sum(n)*100, 1))%>%
    mutate (valor = paste0(n, ' (', frec, ')')) %>%
    select(grupo, variable, categos, valor) |>
    pivot_wider(names_from = grupo, values_from = valor) |>
    arrange(variable) |>
    filter(!categos == '0')
  df_categoricas
}

f_roundcond <- function(x, dec = 1){
  #browser()
  ifelse(trunc(x) == 0,
         ifelse(x%%1 < 0.002,
                format(round(x, 3),
                       trim = TRUE),
                format(round(x, dec),
                       nsmall = 2,
                       big.mark = ',',
                       trim = TRUE)),
         
         ifelse(x%%1 < 0.001,
                format(round(x),
                       nsmall = 0,
                       big.mark = ',',
                       trim = TRUE),
                format(round(x, dec),
                       nsmall = dec,
                       big.mark = ',',
                       trim = TRUE))         )
}


# PATIENT CHARACTERISTICS DESCRIPTIVE ------------------------------------

#características de los pacientes para mediana e iqr
varPatCharMed <- c('grupo', 'edad', 'num_adultos', 'num_ninios', 'tiempofumapasivo', 'tiempo_humo')

dfvarPatCharMed <- dfestufas |>
  select(all_of(varPatCharMed)) |>
  group_by(grupo) |>
  summarise(across(everything(), list(
    mediana = ~round(median(.x, na.rm = TRUE), 1),
    q25 = ~round(quantile(.x, 0.25, na.rm = TRUE),1),
    q75 = ~round(quantile(.x, 0.75, na.rm = TRUE),1)
  ), .names = '{.col}__{.fn}')) |>
  pivot_longer(cols = -1,
               names_sep = '__',
               names_to = c('variable', '.value')) |>
  mutate(valor = paste0(mediana, ' (', q25, ' - ', q75, ')')) |>
  select(grupo, variable,valor) |>
  pivot_wider(names_from = grupo, values_from = valor)

varPatCharProm <- c('grupo', 'imc')

# características de los pacientes para promedio y DE
# como se va a usar básicamente el promedio en el resto del plan (ver el Word)
# hago una función:
# f_PromVarNum <- function(columnas, df = dfestufas){
#   dfVarNum<- df |>
#     select(all_of(c('grupo', columnas))) |>
#     group_by(grupo) |>
#     summarise(across(everything(), list(
#       promedio = ~round(mean(.x, na.rm = TRUE), 1),
#       desvest = ~round(sd(.x, na.rm = TRUE),1)
#     ), .names = '{.col}__{.fn}')) |>
#     pivot_longer(cols = -1,
#                  names_sep = '__',
#                  names_to = c('variable', '.value')) |>
#     mutate(valor = paste0(promedio, ' ± ', desvest)) |>
#     select(grupo, variable,valor) |>
#     pivot_wider(names_from = grupo, values_from = valor)
#   dfVarNum
# }


dfvarPatCharProm <- f_PromVarNum(varPatCharProm)
# características de los pacientes pero con variables categóricas
#hacemos una función que saque la frecuencia de todas las categóricas
#por la manera en que estructuró la base Rebecca, este algoritmo sólo reporta
#la cantidad con la categoría en cuestión
f_frecCategos <- function(columnas, df = dfestufas){
  df_categoricas <- df |>
    select(all_of(c('grupo', columnas))) |>
    pivot_longer(all_of(c(columnas)), names_to = 'variable', values_to = 'categos') %>%
    group_by(grupo, variable, categos) %>%
    na.omit %>%
    summarise(n = n()) %>%
    mutate(frec = round(n/sum(n)*100, 1))%>%
    mutate (valor = paste0(n, ' (', frec, ')')) %>%
    select(grupo, variable, categos, valor) |>
    pivot_wider(names_from = grupo, values_from = valor) |>
    arrange(variable) |>
    filter(!categos == '0')
  df_categoricas
}

# seleccionamos las variables categóricas del apartado de características
varcategoricas <- c('escol',names(dfestufas)[5:11], 'enf_cronicas') 
# Creamos la dataframe con las categóricas, reporta frecuencia y porcentaje
dfvarPatCharCateg <- f_frecCategos(varcategoricas)

dfvarPatChar <- bind_rows(dfvarPatCharCateg, dfvarPatCharMed, dfvarPatCharProm)
#ordenamos de acuerdo como Rebecca sugiere que salgan, hay que añadir un índice
ordenVarPatChar <- c(15,2,3,4,5,6,7,8,9,10,11,12,1,13,14,17,18,16)
dfvarPatChar <- dfvarPatChar |>
  ungroup() |>
  mutate(indice = ordenVarPatChar) |>
  mutate(across(all_of(c('Control', 'Ecostove')),
                ~ ifelse(is.na(.x), '0', .x))) |>
  arrange(indice) |>
  select(-5)
#write_excel_csv(dfvarPatChar, file = 'patientCharact.csv')
#Rebecca lo regresa como .csv
library(readr)
dfpatCharRebecca <- read_csv("csvs/patientcharacEng2.csv", show_col_types = FALSE)
#quitamos los separadores:
dfpatCharRebecca <- dfpatCharRebecca |>
  filter(!is.na(variable)) |>
  select(Finalname)
dfvarPatCharEng <- bind_cols(dfvarPatChar, dfpatCharRebecca)

#Hacemos las dataframes para que funcionen como separadores en la tabla de flextable
dfeducacion <- data.frame(variable = NA,
                          categos = NA,
                          Control = NA,
                          Ecostove = NA,
                          Finalname = 'Education (N, %):')
dfocupacion <- data.frame(variable = NA,
                          categos = NA,
                          Control = NA,
                          Ecostove = NA,
                          Finalname = 'Occupation (N, %):')
dfprovEdad <- filter(dfvarPatCharEng, variable == 'edad')
dfprov1 <- filter(dfvarPatCharEng, variable == 'escol') |>
  mutate(Finalname = paste0(' - ', Finalname))
dfprov2 <- filter(dfvarPatCharEng, str_starts(variable, 'ocup'))|>
  mutate(Finalname = paste0(' - ', Finalname))
dfprov3 <- filter(dfvarPatCharEng, str_starts(variable, 'ocup', negate = TRUE) &
                    variable != 'escol' & variable != 'edad')
dfpatCharFinalV1 <- bind_rows(dfprovEdad, dfeducacion, dfprov1, dfocupacion, dfprov2, dfprov3) |>
  select(variable, Finalname, Control, Ecostove)
save(dfpatCharFinalV1, file = 'Rdata/dfpatCharFinalV1.Rda')

# TX ESTADÍSTICO DE PATIENT CHARACTERISTICS. ------------------------------

# Se habló con Rebecca, sólo se hará estadística de las siguientes variables:
# enf_cronicas (CHI), imc (t), tiempofumapasivo (wilcoxon), tiempo_humo(wilcoxon)
chiEnfCronica <- chisq.test(as.factor(dfestufas$enf_cronicas), as.factor(dfestufas$grupo))
wTiempoFumaPasivo <- wilcox.test(tiempofumapasivo ~ grupo, dfestufas)
wTiempoHumo <- wilcox.test(tiempo_humo ~ grupo, dfestufas)
tImc <- t.test(imc ~ grupo, data = dfestufas)
tImc



# COOKING PRACTICES DESCRIPTIVE -------------------------------------------

#usamos la función f_frecCategos que está en la sección de PATIENT CHARACTERISTICS
varCookingCateg <- c('cocinadentro', 'cocinacuarto', 'cocinatecho', 'cocinacerrada',
                     'cocinacircaire', 'humo', "comenencocina")
dfvarCookingCateg <- f_frecCategos(varCookingCateg)

# ahora usamos la función para calcular los promes y desvest
varCookingNum <- c("numvecescocina", "cocinahoras", "usoeco_porc_hsem", "usogas_porc_hsem", "usolenia_porc_hsem", "usocarbon_porc_hsem")
dfvarCookingNum <- f_PromVarNum(varCookingNum)
#Unimos las dos bases
dfvarCooking <- bind_rows(dfvarCookingCateg, dfvarCookingNum)
#guardamos para generar una columna de índice para respetar el orden de la tabla de Rebecca
write_excel_csv(dfvarCooking, file = 'csvs/varcooking.csv')

#leemos la tabla con el índice y ordenamos la dataframe
dfvarcookingInd <- read_csv('csvs/varcookingIndice.csv')
dfvarCooking <- dfvarCooking |>
  ungroup() |>
  mutate(indice = dfvarcookingInd$indice) |>
  arrange(indice) |>
  select(-indice)
#Guardamos la base para darla a Rebecca y edite los FinalNames.
#El .csv generado, lo abrí en Excel, sustituí la variable categos con la de FinalNames y la 
#guardé como varcookingRebecca.xlsx en la carpeta de exceles.
write_excel_csv(dfvarCooking, file = 'csvs/varcookingarr.csv')

# Recibo el excel con la versión en inglés de Rebecca, le puse varcookingEng.xlsx.
library(readxl)
dfvarcookingEng <- read_excel("exceles/varcookingEng.xlsx")
#View(dfvarcookingEng)

#Añadimos la variable FinalName
dfvarCooking2Eng <- dfvarCooking |>
  mutate(FinalName = dfvarcookingEng$FinalName)

#Creamos las dataframes provisionales para la df final
dflugarCocina <- data.frame(variable = NA,
                            categos = NA,
                            Control = NA,
                            Ecostove = NA,
                            FinalName = 'Location of kitchen (N, %):')

#df final
dfvarCookingFinal <- bind_rows(dflugarCocina, dfvarCooking2Eng) |>
  mutate(FinalName = ifelse(str_starts(variable, 'cocina') & !is.na(categos), paste0(' - ', FinalName), FinalName)) |>
  select(variable, FinalName, Control, Ecostove)
save(dfvarCookingFinal, file = 'Rdata/dfvarCookingFinal.Rda')

# En la reunión con Rebecca del 24/07/2024 se decidió separar las variables de porcentaje en una tabla
# separada, calculando la p y añadiendo una columna con estos valores.
dfvarCookingPracticesV1 <- dfvarCookingFinal |>
  filter(!str_starts(FinalName, '%'))
save(dfvarCookingPracticesV1, file = 'Rdata/dfvarCookingPracticesV1.Rda')
# con esta df se hace la tabla de cooking practices en el .Rmd

# COOKING METHODS WITH STATISTICS -----------------------------------------
dfvarCookingMethods <- setdiff(dfvarCookingFinal, dfvarCookingPracticesV1)
varCookingMethods <- dfvarCookingMethods |>
  select(variable) |>
  pull()
listapesCookingMeth <- apply(dfestufas[, varCookingMethods], 2, function(x) t.test(x ~ dfestufas$grupo)$p.value)
listapesCookingMeth <- round(listapesCookingMeth, 3)
listapesCookingMeth <- ifelse(listapesCookingMeth < 0.001, '< 0.001', listapesCookingMeth)
dflistapesCookingMeth <- data.frame(variable = varCookingMethods,
                                    p = listapesCookingMeth)
dfvarCookingMethodsV1 <- dfvarCookingMethods |>
  left_join(dflistapesCookingMeth, by = 'variable')
save(dfvarCookingMethodsV1, file = 'Rdata/dfvarCookingMethodsV1.Rda')
#Esta es la df que se usa para la flextable de cooking methods.




# OVERVIEW SYMPTOM SCALES WITH STATISTICS ---------------------------------
library(tidyverse)
varSymptomScales <- c('grupo', 'epoctotal', 'mmrc', 'phq4total', 'psstotal', 'psqitotal')

# ESTADÍSTICA PARAMÉTRICA
dfTsymptomScales <- dfestufas |>
  select(all_of(varSymptomScales)) |>
  #group_by(grupo) |>
  summarise(across(everything(), list(
    promedio = ~ round(mean(.x, na.rm = TRUE), 2),
    desvest = ~ round(sd(.x, na.rm = TRUE), 2)
  )), .by = grupo) |>
  pivot_longer(cols = -grupo,
               names_sep = '_',
               names_to = c('variable', '.value')) |>
  mutate(promdesvest = paste0(promedio, ' ± ', desvest)) |>
  select(grupo, variable, promdesvest) |>
  pivot_wider(names_from = grupo, values_from = promdesvest)

#Calculamos las pes, usando el código de cooking methods
listapesSymptomScales <- apply(dfestufas[, varSymptomScales[-1]], 2, function(x) t.test(x ~ dfestufas$grupo)$p.value)
listapesSymptomScales <- round(listapesSymptomScales, 3)
listapesSymptomScales <- ifelse(listapesSymptomScales < 0.001, '< 0.001', listapesSymptomScales)
dfTlistapesSymptomScales <- data.frame(variable = varSymptomScales[-1],
                                       p = listapesSymptomScales)
dfTvarSymptomScales <- dfTsymptomScales |>
  left_join(dfTlistapesSymptomScales, by = 'variable')

dfwsympscEng <- read_csv('csvs/dfTsymptscEng.csv')

dfTvarSymptScalFinalV1 <- dfTvarSymptomScales |>
  mutate(FinalName = dfwsympscEng$FinalName) |>
  select(FinalName, Control, Ecostove, p)
save(dfTvarSymptScalFinalV1, file =  'Rdata/dfTvarSymptScalFinalV1.Rda')


# ESTADÍSTICA NO PARAMÉTRICA
dfWsymptomScales <- dfestufas |>
  select(all_of(varSymptomScales)) |>
  #group_by(grupo) |>
  summarise(across(everything(), list(
    mediana = ~ round(median(.x, na.rm = TRUE), 2),
    p25 = ~ round(quantile(.x, probs = 0.25, na.rm = TRUE), 2),
    p75 = ~ round(quantile(.x, probs = 0.75, na.rm = TRUE), 2)
  )), .by = grupo) |>
  pivot_longer(cols = -grupo,
               names_sep = '_',
               names_to = c('variable', '.value')) |>
  mutate(mediqr = paste0(mediana, ' (', p25, ' - ', p75, ')')) |>
  select(grupo, variable, mediqr) |>
  pivot_wider(names_from = grupo, values_from = mediqr)

#Calculamos las pes, usando el código de cooking methods
listapesSymptomScales <- apply(dfestufas[, varSymptomScales[-1]], 2, function(x) wilcox.test(x ~ dfestufas$grupo)$p.value)
listapesSymptomScales <- round(listapesSymptomScales, 3)
listapesSymptomScales <- ifelse(listapesSymptomScales < 0.001, '< 0.001', listapesSymptomScales)
dfWlistapesSymptomScales <- data.frame(variable = varSymptomScales[-1],
                                       p = listapesSymptomScales)
dfWvarSymptomScales <- dfWsymptomScales |>
  left_join(dfWlistapesSymptomScales, by = 'variable')
save(dfWvarSymptomScales, file = 'Rdata/dfWvarSymptomScales.Rda')
#write_excel_csv(dfWvarSymptomScales, file = 'csvs/dfwsymptsc.csv')
dfwsympscEng <- read_csv('csvs/dfwsymptscEng.csv')

dfWvarSymptScalFinalV1 <- dfWvarSymptomScales |>
  mutate(FinalName = dfwsympscEng$FinalName) |>
  select(FinalName, Control, Ecostove, p)
save(dfWvarSymptScalFinalV1, file =  'Rdata/dfWvarSymptScalFinalV1.Rda')


# SYMPTOM SCALES CATEGORIC WITH CHI ------------------------------------------------

varSymptomScales <- c('grupo', 'epoctotal', 'mmrc', 'phq4total', 'psstotal', 'psqitotal')

dfsymptScalCat <- dfestufas |>
  select(all_of(varSymptomScales)) |>
  mutate(
    epoctotcateg = case_when(
      epoctotal <= 10 ~ '1',
      epoctotal %in% 11:20 ~ '2',
      epoctotal %in% 21:30 ~ '3',
      epoctotal %in% 31:40 ~ '4'),
    epoctotcateg = factor(epoctotcateg,
                          levels = c('1','2','3','4'),
                          labels = c('Low/no impact (≤10)',
                                     'Medium impact (11-20)',
                                     'High impact (21-30)',
                                     'Very high impact (31-40)')),
    mmrccateg = case_when(
      mmrc <= 1 ~ 1,
      mmrc == 2 ~ 2,
      mmrc %in% 3:4 ~ 3),
    mmrccateg = factor(mmrccateg,
                       levels = c('1', '2', '3'),
                       labels = c('Normal (0-1)',
                                  'Moderate (2)',
                                  'Severe (3-4)')),
    phq4categ = case_when(
      phq4total <= 2 ~ '1',
      phq4total %in% 3:5 ~ '2',
      phq4total %in% 6:8 ~ '3',
      phq4total %in% 9:12 ~ '4'),
    phq4categ = factor(phq4categ,
                       levels = c('1','2','3','4'),
                       labels = c('Normal (0-2)',
                                  'Mild (3-5)',
                                  'Moderate (6-8)',
                                  'Severe (9-12)')),
    psscateg = case_when(
      psstotal <= 13 ~ 1,
      psstotal %in% 14:26 ~ 2,
      psstotal %in% 27:40 ~ 3),
    psscateg = factor(psscateg,
                      levels = c('1', '2', '3'),
                      labels = c('Low (0-13)',
                                 'Moderate (14-26)',
                                 'High (27-40)')),
    psqicateg = case_when(
      psqitotal <= 4 ~ 1,
      psqitotal %in% 5:21 ~ 2),
    psqicateg = factor(psqicateg,
                       levels = c('1', '2'),
                       labels = c('Good sleep (0-4)',
                                  'Poor sleep (5-21)'))
    
  )

varCategsymptScales <- names(dfsymptScalCat)[(str_which(names(dfsymptScalCat), 'categ'))]
varCategsymptScales
dfPorcsymptScalCat <- f_frecCategos(columnas = varCategsymptScales, df = dfsymptScalCat )
# hay un NA, lo volvemos 0,
dfPorcsymptScalCat$Control <- with(dfPorcsymptScalCat,
                                   ifelse(is.na(Control), '0', Control))
#Añadimos la columna para el valor de p
dfPorcsymptScalCat <- dfPorcsymptScalCat |>
  mutate(p = NA)

chisSymptScalCateg <- apply(dfsymptScalCat[, varCategsymptScales], 2, function(x) chisq.test(x, dfsymptScalCat$grupo, simulate.p.value = TRUE)$p.value)
chisSymptScalCateg
# hacemos la df para relacionar las pes con las variables correspondientes
dfpesDeChis <- data.frame(variable = varCategsymptScales,
                          p = f_roundcond(chisSymptScalCateg, 3) )

#Construimos la tabla final:
dfcopdTitle <- data.frame(variable = NA,
                          categos = 'COPD Assessment Test',
                          Control = NA,
                          Ecostove = NA,
                          p = dfpesDeChis[which(dfpesDeChis$variable == 'epoctotcateg'), 'p'])
dfmmrcTitle <- data.frame(variable = NA,
                          categos = 'mMRC Dyspnea Scale',
                          Control = NA,
                          Ecostove = NA,
                          p = dfpesDeChis[which(dfpesDeChis$variable == 'mmrccateg'), 'p'])

dfphq4Title <- data.frame(variable = NA,
                          categos = 'PHQ-4',
                          Control = NA,
                          Ecostove = NA,
                          p = dfpesDeChis[which(dfpesDeChis$variable == 'phq4categ'), 'p'])

dfpssTitle <- data.frame(variable = NA,
                         categos = 'Perceived Stress Scale (PSS)',
                         Control = NA,
                         Ecostove = NA,
                         p = dfpesDeChis[which(dfpesDeChis$variable == 'psscateg'), 'p'])

dfpsqiTitle <- data.frame(variable = NA,
                          categos = 'Pittsburg Sleep Quality Index',
                          Control = NA,
                          Ecostove = NA,
                          p = dfpesDeChis[which(dfpesDeChis$variable == 'psqicateg'), 'p'])

#Unimos las bases:
dfRespSymptFinalV1 <- bind_rows(dfcopdTitle,
                              filter(dfPorcsymptScalCat, variable == 'epoctotcateg'),
                              dfmmrcTitle,
                              filter(dfPorcsymptScalCat, variable == 'mmrccateg'))
save(dfRespSymptFinalV1, file = 'Rdata/dfRespSymptFinalV1.Rda')

dfMentalSleepFinalV1 <- bind_rows (dfphq4Title,
                                 filter(dfPorcsymptScalCat, variable == 'phq4categ'),
                                 dfpssTitle,
                                 filter(dfPorcsymptScalCat, variable == 'psscateg'),
                                 dfpsqiTitle,
                                 filter(dfPorcsymptScalCat, variable == 'psqicateg'))
save(dfMentalSleepFinalV1, file = 'Rdata/dfMentalSleepFinalV1.Rda')


# OTHER SYMPTOMS ----------------------------------------------------------

dfestufas <- dfestufas |>
  mutate(sintrespninios = factor(sintrespninios, labels = c('No', 'Yes')))

varOtrosSintomas <- c('cefalea', 'irriojos', 'dolor', 'congenasal', 'sintrespninios')
dfOtrosSintomas <- f_frecCategos(varOtrosSintomas) |>
  mutate(across(all_of(c('Control', 'Ecostove')), ~ ifelse(is.na(.x), 0, .x)))
dfnum_sintrespninios <- f_PromVarNum('num_sintrespninios')

#Añadimos la columna para el valor de p
dfOtrosSintomas <- dfOtrosSintomas |>
  mutate(p = NA)


#Estadística chi
chisOtrosSintomas <- apply(dfestufas[, varOtrosSintomas], 2, function(x) chisq.test(x, dfestufas$grupo, simulate.p.value = TRUE)$p.value)
chisOtrosSintomas
# Estadística T para numsint_respninios
pNum_sintrespninios <- with(dfestufas, t.test(num_sintrespninios ~ grupo)$p.value)
dfnum_sintrespninios <- dfnum_sintrespninios |>
  mutate(p = f_roundcond(pNum_sintrespninios, 3),
         categos = 'Number of respiratory tract infections in past year (mean ± SD)')

# hacemos la df para relacionar las pes con las variables correspondientes
dfpesDeChis <- data.frame(variable = varOtrosSintomas,
                          p = f_roundcond(chisOtrosSintomas, 3) )

#Construimos la tabla final:
dfcefaleaTitle <- data.frame(variable = NA,
                          categos = 'Headache (N, %)',
                          Control = NA,
                          Ecostove = NA,
                          p = dfpesDeChis[which(dfpesDeChis$variable == 'cefalea'), 'p'])
dfirriojosTitle <- data.frame(variable = NA,
                          categos = 'Eye irritation (N, %)',
                          Control = NA,
                          Ecostove = NA,
                          p = dfpesDeChis[which(dfpesDeChis$variable == 'irriojos'), 'p'])

dfdolorTitle <- data.frame(variable = NA,
                          categos = 'MSK pain (N, %)',
                          Control = NA,
                          Ecostove = NA,
                          p = dfpesDeChis[which(dfpesDeChis$variable == 'dolor'), 'p'])
# 
dfcongenasalTitle <- data.frame(variable = NA,
                         categos = 'Nasal congestion (N, %)',
                         Control = NA,
                         Ecostove = NA,
                         p = dfpesDeChis[which(dfpesDeChis$variable == 'congenasal'), 'p'])
dfmenores5 <- data.frame(variable = NA,
                         categos = 'Children less than 5 years in household:',
                         Control = NA,
                         Ecostove = NA,
                         p = NA)
dfsintrespninios <- data.frame(variable = NA,
                               categos = 'Respiratory symptoms in the last 2 weeks (N, %)',
                               Control = NA,
                               Ecostove = NA,
                               p = dfpesDeChis[which(dfpesDeChis$variable == 'sintrespninios'), 'p'])
# 
# #Unimos las bases:
dfotrosSintomasV1 <- bind_rows(dfcefaleaTitle,
                               filter(dfOtrosSintomas, variable == 'cefalea'),
                               dfirriojosTitle,
                               filter(dfOtrosSintomas, variable == 'irriojos'),
                               dfdolorTitle,
                               filter(dfOtrosSintomas, variable == 'dolor'),
                               dfcongenasalTitle,
                               filter(dfOtrosSintomas, variable == 'congenasal'),
                               dfmenores5,
                               dfsintrespninios,
                               filter(dfOtrosSintomas, variable == 'sintrespninios'),
                               dfnum_sintrespninios)

save(dfotrosSintomasV1, file = 'Rdata/dfotrosSintomasV1.Rda')
# 

# HEALTH ASSESSMENT NUMERIC -----------------------------------------------

varHealth <- names(dfestufas)[c(66:70, 72)]

dfhealthNum <- f_PromVarNum(varHealth)
# NOTA: HAY QUE CHECAR CON REBECCA LO DE LA GLUCOSA EN AYUNAS.

listapesHealth <- apply(dfestufas[, varHealth], 2, function(x) t.test(x ~ dfestufas$grupo)$p.value)
listapesHealth <- round(listapesHealth, 2)

dfhealthNum <- dfhealthNum |>
  mutate(p = listapesHealth)

#write_excel_csv(dfhealthNum, file = 'csvs/dfhealthNum.csv')
dfhealthNumEng <- read_csv('csvs/dfhealthNumEng.csv')
dfhealth <- dfhealthNum |>
  left_join(dfhealthNumEng)
dfhealthNumV1 <- dfhealth |>
  select(c(1,5, 2:4))

#df que será la de flextable
save(dfhealthNumV1, file = 'Rdata/dfhealthNumV1.Rda')

# HEALTH ASSESSMENT CATEGORIC ---------------------------------------------

varHealth <- names(dfestufas)[c(66:70, 72)]
dfhealthCateg <- dfestufas |>
  select(all_of(c('grupo', varHealth))) |>
  mutate(
    tascateg = case_when(
      (tas < 130 & tad < 90) ~ '1',
      (tas >= 130 | tad >= 90) ~ '2'),
    tascateg = factor(tascateg,
                      levels = c('1','2'),
                      labels = c('Normal (SBP <130 and/or DBP<90)',
                                 'HTN (SBP ≥ 130 and/or DBP ≥ 90)')),
    spo2categ = case_when(
      spo2 >= 94 ~ '1',
      spo2 %in% 90:93 ~ '2',
      spo2 < 90 ~ '3'),
    spo2categ = factor(spo2categ, labels = c('Normal (≥94)',
                                             'Low normal (90-93%)',
                                             'Low (<90%)')),
    imccateg = case_when(
      imc >= 18.5 & imc <= 24.9 ~ '1',
      imc >= 25 & imc <= 29.9 ~ '2',
      imc >= 30 ~ '3'),
    imccateg = factor(imccateg, labels =c('Normal (18.5-24.9)',
                                          'Overweight (25-29.9)',
                                          'Obese ≥ 30'))
  )
varCategHealth <- names(dfhealthCateg)[(str_which(names(dfhealthCateg), 'categ'))]
dfPorHealthCat <- f_frecCategos(columnas = varCategHealth, df = dfhealthCateg )

chisHealthCateg <- apply(dfhealthCateg[, varCategHealth], 2, function(x) chisq.test(x, dfhealthCateg$grupo, simulate.p.value = TRUE)$p.value)
chisHealthCateg
# hacemos la df para relacionar las pes con las variables correspondientes
dfpesDeChis <- data.frame(variable = varCategHealth,
                          p = f_roundcond(chisHealthCateg, 2) )

dftasTitle <- data.frame(variable = NA,
                         categos = 'Blood pressure',
                         Control = NA,
                         Ecostove = NA,
                         p = dfpesDeChis[which(dfpesDeChis$variable == 'tascateg'), 'p'])

dfspo2Title <- data.frame(variable = NA,
                          categos = 'SpO2',
                          Control = NA,
                          Ecostove = NA,
                          p = dfpesDeChis[which(dfpesDeChis$variable == 'spo2categ'), 'p'])

dfimcTitle <- data.frame(variable = NA,
                         categos = 'BMI',
                         Control = NA,
                         Ecostove = NA,
                         p = dfpesDeChis[which(dfpesDeChis$variable == 'imccateg'), 'p'])
dfPorHealthCatFinalV1 <- bind_rows(dftasTitle,
                                   filter(dfPorHealthCat, variable == 'tascateg'),
                                   dfspo2Title,
                                   filter(dfPorHealthCat, variable == 'spo2categ'),
                                   dfimcTitle,
                                   filter(dfPorHealthCat, variable == 'imccateg'),)
save(dfPorHealthCatFinalV1, file = 'Rdata/dfPorHealthCatFinalV1.Rda')

# AIR QUALITY -------------------------------------------------------------

# NOTA: Correr la cabecera y FUNCIONES de ScrVisit1
names(dfestufas)
varAirQuality <- names(dfestufas)[c(76:77)]
varAirQuality

# Se modificó la función f_PromVarNum:
df <- dfestufas 
columnas <- varAirQuality

dfairQuality <- df |>
  select(all_of(c('grupo', 'estufaenuso', columnas))) |>
  filter(!is.na(estufaenuso)) |>
  mutate(estufaenuso = factor(estufaenuso, levels = c('1', '0'), labels = c('Yes', 'No'))) |>
  group_by(grupo, estufaenuso) |>
  summarise(across(everything(), list(
    promedio = ~round(mean(.x, na.rm = TRUE), 1),
    desvest = ~round(sd(.x, na.rm = TRUE),1)
  ), .names = '{.col}__{.fn}'))|>
  pivot_longer(cols = -c(1, 2),
               names_sep = '__',
               names_to = c('variable', '.value')) |>
  mutate(valor = paste0(promedio, ' ± ', desvest))|>
  select(grupo, estufaenuso, variable,valor) |>
  pivot_wider(names_from = grupo, values_from = valor) |>
  arrange(desc(variable))

listapesAirQNo <- apply(dfestufas[,varAirQuality], 2, function(x) t.test(x ~ dfestufas$grupo,
                                                                         subset = dfestufas$estufaenuso == '0')$p.value)
listapesAirQNo <- round(listapesAirQNo, 2)
listapesAirQNo

listapesAirQYes <- apply(dfestufas[,varAirQuality], 2, function(x) t.test(x ~ dfestufas$grupo,
                                                                          subset = dfestufas$estufaenuso == '1')$p.value)
listapesAirQYes <- round(listapesAirQYes, 2)
listapesAirQYes
names(listapesAirQNo)

#hacemos una base para unir por estufaenuso y variable la p
dfpesAirQuality <- data.frame(estufaenuso = c('Yes', 'Yes', 'No', 'No'),
                              variable = c(names(listapesAirQYes), names(listapesAirQNo)),
                              p = c(listapesAirQYes, listapesAirQNo))

write_csv(dfairQuality, file = 'csvs/dfairQuality.csv')
dfaqEng <- read_csv('csvs/dfairQualityEng.csv', show_col_types = FALSE)

dfairQualityFinalV1 <- dfairQuality |>
  left_join(dfpesAirQuality, by = c('estufaenuso', 'variable')) |>
  left_join(dfaqEng, by = c('estufaenuso', 'variable')) |>
  select(c(1,2,6,3:5))


save(dfairQualityFinalV1, file = 'Rdata/dfairQualityFinalV1.Rda')
