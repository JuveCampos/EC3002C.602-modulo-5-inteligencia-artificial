
library(tidyverse)
library(sf)

# [class()] 1. Que tipo de objeto se encuentra guardado en el archivo "datos_atus_2023.rds"? 

# 2. Los datos de atus (Accidentes de Tránsito Terrestre en Zonas Urbanas y Suburbanas (ATUS)) contienen información, a nivel accidente, de los accidentes registrados en zonas suburbanas en México. ¿Cuantos accidentes hay registrados para 2023? 

# 3. En la liga: https://www.inegi.org.mx/contenidos/programas/accidentes/doc/fd_bd_atus_2024.pdf se encuentra el descriptor de variables. Con esta información, podrías determinar en cuantos accidentes estuvieron involucrados bicicletas? ¿Y Motocicletas? 

# 4. [case_when(), group_by(), summarise()] ¿Cuantos accidentes de los registrados ocurrieron en Lunes? ¿En Martes? Elabore una gráfica con la incidencia de accidentes por día. Obtenga el porcentaje por día, de tal manera que la suma de los porcentajes de todos los días sume 100. 

# 5. [case_when(), group_by(), filter()] ¿Cuales fueron los cinco tipos de accidente más comunes?

# 6. [group_by(), summarise(), ggplot()] Elabore una gráfica en la cual clasifique los accidentes por si el presunto responsable tenía o no aliento alcoholico. Utilice porcentajes. 

# 7. Elabore una gráfica en la cual clasifique los accidentes por el sexo del presunto responsable. Utilice porcentajes. 

# 8. Elabore una gráfica de barras con las edades del presunto responsables de los accidentes registrados. 

# 9. Elabore un mapa en ggplot en el cual visualice los puntos donde ocurrieron los accidentes registrados en el estado de Morelos. Utilice el siguiente geojson para el polígono de los estados: "https://raw.githubusercontent.com/JuveCampos/Shapes_Resiliencia_CDMX_CIDE/master/geojsons/Division%20Politica/DivisionEstatal.geojson" y "https://raw.githubusercontent.com/JuveCampos/Shapes_Resiliencia_CDMX_CIDE/master/geojsons/Division%20Politica/mpios_con_menos_islas_aun.geojson"

# 10. Elabore un bucle para poder tener los 32 mapas de las 32 entidades, mostrando los puntos en los que ocurrieron los accidentes dentro de la entidad. 

