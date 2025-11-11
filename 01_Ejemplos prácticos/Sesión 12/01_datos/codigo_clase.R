
# Librerias 
library(tidyverse)
library(srvyr)

# Cargamos los datos de la ENOE ----
"enoes/enoe_proc_2025_2.rds"
enoe <- readRDS("enoes/enoe_proc_2025_2.rds")

sum(enoe$factor)

peao <- enoe %>% 
  as_tibble() %>% 
  filter(clase1 == 1) %>% 
  filter(clase2 == 1) %>% 
  filter(eda >= 15)
  
sum(peao$factor)

peao %>% 
  as_survey_design(weights = factor) %>% 
  # group_by(ent) %>% 
  survey_count(vartype = "ci")

# El ingreso de las personas en México. ----
peao %>% 
  as_survey_design(weights = factor) %>% 
  summarise(ingreso_promedio = survey_mean(ingreso_sm, 
                                           na.rm = T,
                                           vartype = "ci"))

# Ingreso desagregado ----
base <- peao %>% 
  as_survey_design(weights = factor) %>% 
  group_by(sector) %>% 
  summarise(ingreso_promedio = survey_mean(ingreso_sm, 
                                           na.rm = T,
                                           vartype = "ci")) %>% 
  arrange(-ingreso_promedio) 

# Gráfica ----

# Grupo de edad 
bd_plot <- peao %>% 
  as_survey_design(weights = factor) %>% 
  group_by(grupo_edad) %>% 
  summarise(ingreso_promedio = survey_mean(ingreso_sm, 
                                           na.rm = T,
                                           vartype = "ci"))

bd_plot %>% 
  ggplot(aes(x = str_wrap(grupo_edad, 5), 
             y = ingreso_promedio)) + 
  geom_col(fill = "skyblue") + 
  geom_text(aes(label = round(ingreso_promedio, 1), 
                y = ingreso_promedio_low
                ), 
            color = "white",
            vjust = 0.5, 
            hjust = 1,
            angle = 90) + 
  geom_errorbar(aes(ymin = ingreso_promedio_low, 
                    ymax = ingreso_promedio_upp), 
                color = "red") + 
  labs(title = "Ingreso promedio por grupo de edad", 
       subtitle = "ENOE. Segundo trimestre del 2025") + 
  theme_minimal()


# Serie ---
# primero, el caso de 2025

archivos_enoe <- list.files("enoes/", 
                            pattern = "rds",
                            full.names = T)


tasas_informalidad <- tibble()

for(contador in archivos_enoe){

    enoe <- readRDS(contador)
    
    peao <- enoe %>% 
      as_tibble() %>% 
      filter(clase1 == 1) %>% 
      filter(clase2 == 1) %>% 
      filter(eda >= 15)
    
    porcentaje_informalidad <- peao %>% 
      as_survey_design(weights = factor) %>% 
      group_by(tipo_formalidad, año) %>% 
      survey_count() %>% 
      ungroup() %>%
      mutate(pp = 100*(n/sum(n))) %>% 
      filter(tipo_formalidad == "Empleo informal")
    
    tasas_informalidad <- rbind(tasas_informalidad, porcentaje_informalidad)
    print(str_c("ya está listo el dato de ", porcentaje_informalidad$año))

}

tasas_informalidad %>% 
  ggplot(aes(x = año, y = pp, group = tipo_formalidad)) + 
  geom_line() + 
  geom_point() + 
  scale_y_continuous(limits = c(0, 60)) + 
  theme_minimal()

peao$sinco


