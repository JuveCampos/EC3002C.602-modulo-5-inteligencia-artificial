
# Librerias ----
library(tidyverse)
library(srvyr)
library(foreign)

# Datos ----
# concentrado_hogar <- read.dbf("01_datos/concentradohogar.dbf")
# ingresos <- read.dbf("01_datos/ingresos.dbf")
# trabajos <- read.dbf("01_datos/trabajos.dbf")
# 
# saveRDS(concentrado_hogar, "01_datos/concentrado_hogar.rds")
# saveRDS(ingresos, "01_datos/ingresos.rds")
# saveRDS(trabajos, "01_datos/trabajos.rds")

# Ejercicios: ----
# 1. Dada la presentación de resultados: https://www.inegi.org.mx/contenidos/programas/enigh/nc/2024/doc/enigh2024_ns_presentacion_resultados.pdf, replique los resultados de ingresos y gastos de las tablas resumen

concentrado_hogares <- read.dbf("01_datos/concentradohogar.dbf")
# concentrado_hogares <- readRDS("01_datos/concentrado_hogar.rds")

names(concentrado_hogares) %>% sort()
concentrado_hogares %>% 
  as_survey_design(weights = factor) %>% 
  summarise(ingreso_corriente = survey_mean(ing_cor, 
                                            vartype = "ci"))

concentrado_hogares %>% 
  as_survey_design(weights = factor) %>% 
  summarise(ingreso_corriente = survey_mean(ingtrab, 
                                            vartype = "ci"))

concentrado_hogares %>% 
  as_survey_design(weights = factor) %>% 
  summarise(ingreso_corriente = survey_mean(ingtrab, 
                                            vartype = "ci"))


concentrado_hogares %>% 
  as_survey_design(weights = factor) %>% 
  summarise(ingreso_corriente = survey_mean(rentas, 
                                            vartype = "ci"))

# 2. Obtenga los ingresos corrientes promedio y los gastos corrientes promedio para las 32 entidades de la república

ingreso_corriente_estados <- concentrado_hogares %>% 
  mutate(cve_ent = str_sub(folioviv, start = 1, end = 2)) %>% 
  as_survey_design(weights = factor) %>%
  group_by(cve_ent) %>% 
  summarise(ingreso_corriente = survey_mean(ing_cor,
                                            vartype = "ci")) %>% 
  arrange(ingreso_corriente) 

cat_edos <- read_csv("https://raw.githubusercontent.com/JuveCampos/Shapes_Resiliencia_CDMX_CIDE/master/Datos/cat_edos.csv")

left_join(ingreso_corriente_estados, cat_edos)


gasto_corriente_edos <- concentrado_hogares %>%
  mutate(cve_ent = str_sub(folioviv, start = 1, end = 2)) %>% 
  as_survey_design(weights = factor) %>%
  group_by(cve_ent) %>% 
  summarise(gasto_corriente = survey_mean(gasto_mon,
                                          vartype = "ci")) %>% 
  arrange(-gasto_corriente)

left_join(gasto_corriente_edos,cat_edos) %>% 
  relocate(entidad, .after = cve_ent)

# 3. Obtenga el ingreso por trabajo de hombres y mujeres y calcule la brecha entre ambos sexos

ingresos <- read.dbf("01_datos/ingresos.dbf")

sum(ingresos$factor)

total_ingresos <- ingresos %>% 
  group_by(folioviv, foliohog, numren) %>% 
  summarise(ing_tri = sum(ing_tri, na.rm = T)) %>% 
  ungroup()

poblacion <- read.dbf("01_datos/poblacion.dbf") 

sum(poblacion$factor)

datos_sexo <- read.dbf("01_datos/poblacion.dbf") %>% 
  as_tibble() %>% 
  select(folioviv, foliohog, numren, sexo, factor)

datos_brecha <- left_join(total_ingresos, datos_sexo) %>% 
  filter(!is.na(sexo))

resultados_brecha <- datos_brecha %>% 
  as_survey_design(weights = factor) %>% 
  group_by(sexo) %>% 
  summarise(ingreso_trimestral_promedio = survey_mean(ing_tri, 
                                                  vartype = "ci"
                                                      )) %>% 
  mutate(sexo = case_when(sexo == 1 ~ "Hombre", 
                          sexo == 2 ~ "Mujer"))

resultados_brecha %>% 
  select(-contains("low")) %>% 
  select(-contains("upp")) %>% 
  pivot_wider(values_from = ingreso_trimestral_promedio, 
              names_from = sexo) %>% 
  mutate(diferencia = Hombre-Mujer, 
         diferencia_pp = 100*((Hombre-Mujer)/Mujer))


# 4. En equipos de reto, defina, a partir del cuestionario, las variables de interés para su proyecto particular. Elabora la estrategia para poder obtener la información que requiere. 
