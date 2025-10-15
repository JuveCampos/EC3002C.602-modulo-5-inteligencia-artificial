# ENIGH y ENOE. TEMAS SELECTOS
# Variables ENIGH:  https://www.inegi.org.mx/contenidos/programas/enigh/nc/2024/microdatos/889463924494.pdf
# Resultados ENIGH: https://www.inegi.org.mx/contenidos/programas/enigh/nc/2024/doc/enigh2024_ns_presentacion_resultados.pdf 
# Variables ENOE: 


# Librerías ----
library(tidyverse) # Manejo de datos
library(srvyr)     # Herramientas para manejo de encuestas
library(foreign)   # Cargar datos de formatos externos
library(scales)
library(forcats)
library(sf)


# Ejercicios ----
# 1. Calcular los deciles por ingreso para la ENIGH 2024 ----

# 1. Cargamos los datos del concentrado hogar, que contiene el resumen de la ENIGH: 
conc_hogar <- read.dbf("01_datos/concentradohogar.dbf") %>% 
  as_tibble()

# 2. Calculamos el ingreso total del hogar entre el total de integrantes: 
ing_per_capita <- conc_hogar %>% 
  group_by(folioviv, foliohog, ing_cor, tot_integ, factor) %>% 
  transmute(ing_per_cap = ing_cor/tot_integ) %>% 
  ungroup() 

# 3. Obtenemos los cortes de los cuantiles: 
montos_cuantiles <- ing_per_capita %>% 
  as_survey_design(weights = factor) %>% 
  summarise(cuantil = survey_quantile(x = ing_cor, 
                                   quantiles = seq(0,1,0.1), 
                                   na.rm = T,
                                   vartype = NULL))

# 4. Hacemos la clasificación de los hogares por cuantiles: 
hogares_por_cuantil <- ing_per_capita %>% 
  mutate(decil = case_when(ing_per_cap < montos_cuantiles$cuantil_q10 ~ "I", 
                           between(ing_per_cap, montos_cuantiles$cuantil_q10, montos_cuantiles$cuantil_q20) ~ "II", 
                           between(ing_per_cap, montos_cuantiles$cuantil_q20, montos_cuantiles$cuantil_q30) ~ "III", 
                           between(ing_per_cap, montos_cuantiles$cuantil_q30, montos_cuantiles$cuantil_q40) ~ "IV", 
                           between(ing_per_cap, montos_cuantiles$cuantil_q40, montos_cuantiles$cuantil_q50) ~ "V", 
                           between(ing_per_cap, montos_cuantiles$cuantil_q50, montos_cuantiles$cuantil_q60) ~ "VI", 
                           between(ing_per_cap, montos_cuantiles$cuantil_q60, montos_cuantiles$cuantil_q70) ~ "VII", 
                           between(ing_per_cap, montos_cuantiles$cuantil_q70, montos_cuantiles$cuantil_q80) ~ "VIII", 
                           between(ing_per_cap, montos_cuantiles$cuantil_q80, montos_cuantiles$cuantil_q90) ~ "IX", 
                           ing_per_cap > montos_cuantiles$cuantil_q90 ~ "X", 
                           T ~ NA
                           ))

# hogares_por_cuantil %>% 
#   as_survey_design(weights = factor) %>% 
#   summarise(prom = survey_mean(ing_cor))

# 5. Verificamos que el número de hogares esté más o menos balanceado
# y que en cada cuantil de ingreso per cápita haya más o menos el 10% de los hogares: 
hogares_por_cuantil %>% 
  as_survey_design(weights = factor) %>% 
  group_by(decil) %>% 
  survey_count() %>% 
  ungroup() %>% 
  mutate(pp = 100*(n/sum(n)))
# Acá podemos ver que si está balanceado

# 2. Calcular el porcentaje de la población por grupos de edad ----

# En este caso, vamos a usar el tabulado de población de la ENIGH: 
pob <- read.dbf("01_datos/poblacion.dbf") %>% 
  as_tibble()

# 1. Verificamos que la suma del factor (ponderadores) sea igual al total de la población mexicana. 
sum(pob$factor) # 130,325,969
# Cifra correcta de acuerdo a la CONAVI: https://siesco.conavi.gob.mx/doc/analisis/2025/Principales_características_de_las_viviendas_en_México.pdf

# 2. Trabajamos con las edades: 
pob_quinquenal <- pob %>% 
  select(folioviv, foliohog, numren, parentesco, sexo, edad, factor) %>% 
  mutate(
    grupo_quinquenal_edad = case_when(
      edad < 5 ~ "0 a 4 años",
      between(edad, 5, 9) ~ "5 a 9 años",
      between(edad, 10, 14) ~ "10 a 14 años",
      between(edad, 15, 19) ~ "15 a 19 años",
      between(edad, 20, 24) ~ "20 a 24 años",
      between(edad, 25, 29) ~ "25 a 29 años",
      between(edad, 30, 34) ~ "30 a 34 años",
      between(edad, 35, 39) ~ "35 a 39 años",
      between(edad, 40, 44) ~ "40 a 44 años",
      between(edad, 45, 49) ~ "45 a 49 años",
      between(edad, 50, 54) ~ "50 a 54 años",
      between(edad, 55, 59) ~ "55 a 59 años",
      between(edad, 60, 64) ~ "60 a 64 años",
      between(edad, 65, 69) ~ "65 a 69 años",
      between(edad, 70, 74) ~ "70 a 74 años",
      between(edad, 75, 79) ~ "75 a 79 años",
      edad >= 80 ~ "80 años y más",
      TRUE ~ NA_character_
    ),
    grupo_quinquenal_edad = factor(
      grupo_quinquenal_edad,
      levels = c("0 a 4 años", "5 a 9 años", "10 a 14 años", "15 a 19 años",
                 "20 a 24 años", "25 a 29 años", "30 a 34 años", "35 a 39 años",
                 "40 a 44 años", "45 a 49 años", "50 a 54 años", "55 a 59 años",
                 "60 a 64 años", "65 a 69 años", "70 a 74 años", "75 a 79 años",
                 "80 años y más"),
      ordered = TRUE
    )
  )

# Acá calculamos el numero de personas por grupo de edad: 
datos_piramide <- pob_quinquenal %>% 
  as_survey_design(weights = factor) %>% 
  group_by(sexo, grupo_quinquenal_edad) %>% 
  survey_count(vartype = "ci")

# 1) Normaliza sexo y crea el conteo con signo
datos_plot <- datos_piramide %>%
  mutate(
    sexo = case_when(
      sexo == 1 ~ "Hombres",
      sexo == 2 ~ "Mujeres"),
    # Hombres negativos (izquierda), Mujeres positivos (derecha)
    n_signed = if_else(sexo == "Hombres", -n, n))


# Pirámide en números
ggplot(datos_plot, aes(x = grupo_quinquenal_edad, y = n_signed, fill = sexo)) +
  geom_col(width = 0.9) +
  geom_text(aes(label = prettyNum(abs(n_signed), big.mark = ","), 
                hjust = sign(n_signed) %>% as.character()
                )) + 
  scale_discrete_manual(aesthetics = "hjust", 
                        values = c("1" = -0.1, 
                                   "-1" = 1.1)
                        ) + 
  coord_flip() +
  scale_y_continuous(
    expand = expansion(c(0.1, 0.1)),
    labels = function(x) label_number(big.mark = ",")(abs(x))
  ) +
  scale_fill_manual(values = c("Hombres" = "#4575b4", "Mujeres" = "#d7301f")) +
  labs(
    title = "Pirámide poblacional por grupos quinquenales",
    x = "Grupo quinquenal de edad",
    y = "Población (ponderada)",
    fill = "Sexo"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(), 
    legend.position = "bottom", 
    axis.line.x = element_line()
  )

# 3. Calcular el porcentaje de personas de la tercera edad viviendo en condiciones de hacinamiento ----
conc_hogar %>% names()
# Para esto, vamos a ligar edades (población) y viviendas. 
viviendas <- read.dbf("01_datos/viviendas.dbf") %>% 
  as_tibble()

# De viviendas, nos traemos cuart_dorm: 
cuartos_por_vivienda <- viviendas %>% 
  select(folioviv, cuart_dorm)

# De población, calculamos los habitantes por vivienda: 
adultos_mayores_condicion_vivienda <- pob %>% 
  select(folioviv, foliohog, edad, factor) %>% 
  left_join(cuartos_por_vivienda) %>% 
  group_by(folioviv) %>% 
  mutate(total_habitantes = n()) %>% 
  mutate(habitantes_por_cuarto_dorm = total_habitantes/cuart_dorm) %>% 
  mutate(cond_hacinamiento = ifelse(habitantes_por_cuarto_dorm > 2.5, 
                                  yes = "Hacinamiento", 
                                  no = "No hacinamiento")) %>% 
  ungroup() %>% 
  filter(edad >= 60)



adultos_mayores_condicion_vivienda %>% 
  as_survey_design(weights = factor) %>% 
  group_by(cond_hacinamiento) %>% 
  survey_count() %>% 
  ungroup() %>% 
  mutate(pp = 100*(n/sum(n)))

# R: Sólo el 8.72% de los mayores de 60 años viven en condiciones de hacinamiento. 


# 4. Calcular el ingreso corriente promedio de las personas con trabajos artísticos ----

# Para esto usamos el tabulado de trabajos. 
trabajos <- read.dbf("01_datos/trabajos.dbf") %>% as_tibble()
# Usamos la variable sinco, que nos enlaza con el catálogo SINCO de ocupaciones. 
# Catálogo SINCO: https://www.inegi.org.mx/contenidos/productos/prod_serv/contenidos/espanol/bvinegi/productos/nueva_estruc/702825198411.pdf

trabajos$sinco # Verificamos a que cifras vienen los datos. 
# Podemos ver que vienen a cuatro dígitos. 

# Seleccionemos los siguientes conceptos: 
# 217 Artistas interpretativos
# 2171 Compositores y arreglistas
# 2172 Músicos
# 2173 Cantantes
# 2174 Bailarines y coreógrafos
# 2175 Actores
artistas <- trabajos %>% 
  select(folioviv, foliohog, numren, sinco) %>% 
  filter(sinco %in% c(2171,2172,2173,2174,2175)) %>% 
  unique()

# Nos traemos los factores de expansión de la población 
fac_exp <- pob %>% 
  select(folioviv, foliohog, numren, factor)

# Los ligamos con sus ingresos: 
ingresos <- read.dbf("01_datos/ingresos.dbf") %>% as_tibble() %>% 
  select(folioviv, foliohog, numren, ing_tri) %>% 
  group_by(folioviv, foliohog, numren) %>% # Sumamos todos los ingresos de la persona
  summarise(ing_tri_total = sum(ing_tri, na.rm = T))

# Ahora si, juntamos todo: 
ingresos_artistas <- artistas %>% 
  left_join(fac_exp) %>% 
  left_join(ingresos) %>% 
  as_survey_design(weights = factor) %>% 
  summarise(ingreso_corriente_promedio_trimestral = survey_mean(ing_tri_total, na.rm = T), 
            ingreso_corriente_promedio_mensual = ingreso_corriente_promedio_trimestral/3 )

# ¿Cuantos hay? 
artistas %>% 
  left_join(fac_exp) %>% 
  left_join(ingresos) %>% 
  as_survey_design(weights = factor) %>% 
  survey_count(vartype = c("ci", "se"))

# De acuerdo a la ENIGH, se estima que hay aproximadamente entre 192 mil y 225 mil artistas de los conceptos establecidos. 
# Para estar seguros que la estimación es de calidad, hay que sacar el Coeficiente de Variación. 

# CV = (EE/Estimación) * 100
# Si este es mayor a 25, tenemos una estimación de mala calidad y los resultados podrían no ser válidos. 

# Cálculo manual: 
# CV_población_artista = 100*(8384/209181) = 4.008012 < 25 ==> Si tiene representatividad estadística

# Estimación del ingreso: 
# CV_ingreso = 100*(2592/37878) = 6.843022 < 25 ==> También está bien 

# De acá, podemos concluir que el ingreso corriente trimestral de las personas que se dedican a actividades artísticas como las comentadas antes es de $37,878 pesos trimestrales o de $12,626 pesos mensuales. 

# 5. Calcular el ingreso promedio de los jóvenes en empleos formales e informales ----
# Esto se calcula con la ENOE. 

# 6. Calcular el ingreso corriente de los hogares que viven en casas con focos ahorradores ----

# Esto lo sacamos del tabulado de vivienda: 
viviendas_con_focos_ahorradores <- viviendas %>% 
  select(folioviv, focos_ahor, factor) %>% 
  mutate(tienen_focos_ahorradores = ifelse(focos_ahor > 0 & !is.na(focos_ahor), 
                                           yes = "Tienen focos ahorradores", 
                                           no = "No tienen focos ahorradores")) 

# ¿Cuantas viviendas tienen focos ahorradores? 
viviendas_con_focos_ahorradores %>% 
  as_survey_design(weights = factor) %>% 
  group_by(tienen_focos_ahorradores) %>% 
  survey_count() %>% 
  ungroup() %>% 
  mutate(pp = 100*(n/sum(n)))
  
# Verifiquemos su ingreso corriente del hogar: 
ingresos_tienen_no_focos_ahorradores <- conc_hogar %>% 
  select(folioviv, foliohog, factor, ing_cor) %>% 
  left_join(viviendas_con_focos_ahorradores) %>% 
  as_survey_design(weights = factor) %>% 
  group_by(tienen_focos_ahorradores) %>% 
  summarise(ingreso_corriente_hogares = survey_mean(ing_cor, vartype = "ci")) 
  
ingresos_tienen_no_focos_ahorradores %>% 
  ggplot(aes(x = tienen_focos_ahorradores, 
             fill = tienen_focos_ahorradores, 
             y = ingreso_corriente_hogares)) + 
  geom_col() + 
  geom_errorbar(aes(ymin = ingreso_corriente_hogares_low, ymax = ingreso_corriente_hogares_upp)) + 
  labs(title = "Montos promedio por hogares que tienen o no focos ahorradores")

# 7. Calcular los estados donde la población considera más fuertes sus redes sociales ----
# Acá cargamos el tabulado de población
poblacion <- read.dbf("01_datos/poblacion.dbf") %>% as_tibble()

# Variables de red social: 
# redsoc_1 Pedir ayuda para conseguir trabajo
# redsoc_2 Pedir ayuda para que lo (la) cuiden
# redsoc_3 Pedir la cantidad de dinero de un mes
# redsoc_4 Pedir que lo (la) acompañen al doctor
# redsoc_5 Pedir cooperación para mejoras en su colonia
# redsoc_6 Pedir que le cuiden a los (as) niños (as)

# Donde las opciones son: 
# Valor Etiqueta
# 1 Imposible conseguirla
# 2 Dificil conseguirla
# 3 Fácil conseguirla
# 4 Muy fácil conseguirla
# 5 Ni fácil ni difícil conseguirla (espontánea)

x = poblacion$redsoc_1

dimensiones_redes_sociales <- poblacion %>% 
  select(folioviv, foliohog, numren, edad, sexo, factor, contains("redsoc")) %>% 
  mutate(across(.cols = redsoc_1:redsoc_6, .fns = function(x){
    x = ifelse(is.na(x), yes = 0, no = x) 
    x = ifelse(x >= 3, yes = 1, no = 0)
    return(x)
  })) %>% 
  mutate(tot = redsoc_1+redsoc_2+redsoc_3+redsoc_4+redsoc_5+redsoc_6) %>% 
  filter(edad >= 18) # Quedarnos con mayores de edad

# Total de personas adultas que consideran que tienen más de 3 dimensión con respaldo de sus redes sociales: 
porcentaje_redes_sociales <- dimensiones_redes_sociales %>% 
  mutate(cve_ent = str_sub(folioviv, 1,2)) %>% 
  relocate(cve_ent, .before = folioviv) %>% 
  mutate(fortaleza_redes = ifelse(tot >= 4, yes = "Fuertes", no = "Débiles")) %>% 
  as_survey_design(weights = factor) %>% 
  group_by(fortaleza_redes, cve_ent) %>% 
  survey_count() %>% 
  arrange(cve_ent) %>% 
  group_by(cve_ent) %>% 
  mutate(pp = 100*(n/sum(n))) %>% 
  filter(fortaleza_redes == "Fuertes") %>% 
  ungroup()

shp <- st_read("https://raw.githubusercontent.com/JuveCampos/Shapes_Resiliencia_CDMX_CIDE/master/geojsons/Division%20Politica/DivisionEstatal.geojson") %>% 
  rename(cve_ent = CVE_EDO)

mapx <- left_join(shp, porcentaje_redes_sociales) 

mapx %>% 
  ggplot(aes(fill = pp)) + 
  geom_sf(color = "white") +
  scale_fill_gradientn(colors = viridis::viridis(n = 10)) + 
  labs(title = "Porcentaje de la población con 4 o más respuestas positivas a preguntas de red y apoyo social") + 
  theme_minimal() + 
  theme(legend.position = "bottom", 
        axis.text = element_blank())


