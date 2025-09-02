
# Repaso del tidyverse
# Los datos provienen de esta liga: https://www.sct.gob.mx/carreteras/direccion-general-de-servicios-tecnicos/datos-viales/ y localmente están guardados en el archivo "00_datos_transito/aforos_puebla.csv"

# 0. Cargue los datos del archivo

# 1. [nrow()] ¿Cuantos cruces hay con datos de aforo? 

# 2. [arrange(), min(), max()] ¿Entre que fechas se levantaron los datos? 

# 3. [filter()] ¿En qué cruce de Puebla es donde cruzaron más peatones? ¿Más Ciclistas? ¿Más Transporte público? ¿Más transporte de carga? ¿Más Total vehicular? 

# 4. [group_by(), count()] ¿Cual es la colonia en la que se instalaron más medidores de aforo? 

# 5. [slice(),  pivot_longer(), ggplot()] Seleccione el primer cruce peatonal. Elabore una gráfica para el total de cruces de todos los tipos que ocurrieron en este primer cruce, de tal forma que tenga una columna para "total ciclistas", una para "transporte público", una para "total vehicular", una para "transporte carga" y una para "total peatonal". Primero, quedese con las columnas que inicien con la palabra "Total", después haga pivot_longer() para tener una tabla en formato largo y finalmente, con esa tabla en formato largo, elabore la gráfica de ggplot(). 


# 6. [ggsave()]. Guarde la gráfica previa en un archivo *.png, de 10 puntos de ancho, 6 puntos de alto y 200 de dpi. 

# 7. [function()]. Genere una función que genere esta gráfica variando el número de renglón. 

# 8. [for()]. Genere un bucle que permita guardar las 199 gráficas en una carpeta dentro del directorio de trabajo. Llame a la carpeta "graficas_puebla". 
