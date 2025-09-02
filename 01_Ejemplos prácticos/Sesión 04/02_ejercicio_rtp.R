
# 1. Análisis RTP (Red de Transporte de Pasajeros)

# Variables: 
# Fecha: la fecha del servicio
# mes: El mes del servicio
# anio: El año del servicio
# servicio: El nombre del servicio
# tipo_pago: como pagaron
# afluencia: el numero de pasajeros
# temporal_fecha: El año_mes del registro
# `..anio_fecha`: El año de la fecha

# 1. [unique()] Investigue cuantos tipos de servicio hay en la base. Cuentelos. 

# 2. [Limpieza de datos, mutate(), case_when()] ¿Nota algo raro? ¿Está bien? Si considera que hay algo raro, resuelvalo. Si lo resolvió, ahora sí diga el número total de tipos de servicio. 

# 3. [unique()] ¿Para cuantos días hay información en la base? De la respuesta en número de días. 

# 4. [group_by(), summarise(), ggplot()] Genere una gráfica de afluencia diaria para el Ecobús para todos los días en la base. ¿Qué día tuvo más afluencia? 

# 5. [group_by(), summarise(), ggplot()] Genere una gráfica de afluencia mensual para el Ecobús. ¿Qué mes-año tuvo más afluencia? 

# 6. [group_by(), summarise(), ggplot()] En promedio, ¿Qué mes del año tiene más afluencia el Ecobús? Elabore una gráfica de afluencia promedio por mes.

# 7. [ggsave()] Guarde las gráficas previas en una carpeta que se llame "ejemplo_ecobus", en formato *.png, con ancho de 10 y alto de 6 unidades. 

# 8. [ggplot()] Genere una gráfica de afluencia diaria pero ahora para el servicio Atenea

# 9. [function()] Genere una función para guardar una gráfica de afluencia diaria variando solamente el nombre/tipo del servicio. 

# 10. [for()] Implemente el bucle necesario para generar las gráficas para todos los servicios. Guarde todas estas gráficas en una carpeta llamada "graficas_todos_servicios". 
