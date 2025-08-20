# 📊 Observatorio de Pobreza México - Aplicación Shiny

Una aplicación web interactiva para visualizar datos de pobreza por entidad federativa en México (2016-2024).

## 🚀 Características

### **Interfaz Profesional**
- **Diseño con colores pastel** suaves y elegantes
- **Dashboard responsivo** con shinydashboard
- **Controles intuitivos** con shinyWidgets
- **Gráficas interactivas** con plotly

### **Funcionalidades**
- ✅ **Selector de entidades**: Las 32 entidades federativas
- ✅ **Selector de variables**: 16 indicadores de pobreza
- ✅ **Personalización**: Color de línea, puntos, tema oscuro
- ✅ **Estadísticas**: Resumen automático de datos
- ✅ **Descargas**: PNG y PDF de alta calidad
- ✅ **Interactividad**: Gráficas con zoom y tooltips

## 📋 Requisitos

### **Paquetes de R necesarios:**
```r
install.packages(c(
  'shiny', 'shinydashboard', 'shinyWidgets', 
  'DT', 'plotly', 'tidyverse', 'ggplot2', 
  'scales', 'viridis'
))
```

### **Archivos requeridos:**
- `app.R` - Aplicación principal
- `grafica_pobreza.R` - Funciones de visualización
- `df_pob_ent.csv` - Datos de pobreza

## 🏃‍♂️ Cómo Ejecutar

### **Método 1: Desde R/RStudio**
```r
shiny::runApp('app.R')
```

### **Método 2: Desde línea de comandos**
```bash
R -e "shiny::runApp('app.R')"
```

### **Método 3: Instalación automática**
```r
# Ejecutar primero para instalar dependencias
source('instalar_paquetes.R')

# Luego ejecutar la aplicación
shiny::runApp('app.R')
```

## 🎨 Paleta de Colores Pastel

| Color | Código | Uso |
|-------|--------|-----|
| 🟦 Azul pastel | `#A8DADC` | Primario |
| 🟪 Rosa pastel | `#FFB3C6` | Acento 1 |
| 🟨 Amarillo pastel | `#FDEAA7` | Acento 2 |
| 🟫 Lavanda pastel | `#C7CEEA` | Acento 3 |
| 🟩 Verde pastel | `#B8E6B8` | Acento 4 |

## 📊 Variables de Pobreza Disponibles

### **Pobreza General**
- `pobreza` - Pobreza Total (%)
- `pobreza_m` - Pobreza Moderada (%)
- `pobreza_e` - Pobreza Extrema (%)

### **Vulnerabilidades**
- `vul_car` - Vulnerabilidad por Carencias (%)
- `vul_ing` - Vulnerabilidad por Ingresos (%)
- `no_pobv` - No Pobre y No Vulnerable (%)

### **Carencias Sociales**
- `carencias` - Población con Carencias (%)
- `carencias3` - Población con 3+ Carencias (%)
- `ic_rezedu` - Rezago Educativo (%)
- `ic_asalud` - Sin Acceso a Salud (%)
- `ic_segsoc` - Sin Seguridad Social (%)
- `ic_cv` - Calidad de Vivienda (%)
- `ic_sbv` - Servicios Básicos Vivienda (%)
- `ic_ali_nc` - Acceso a Alimentación (%)

### **Pobreza por Ingresos**
- `plp_e` - Pobreza Extrema por Ingresos (%)
- `plp` - Pobreza por Ingresos (%)

## 🗺️ Entidades Federativas

La aplicación incluye las 32 entidades federativas de México:

| Clave | Entidad | Clave | Entidad |
|-------|---------|-------|---------|
| 01 | Aguascalientes | 17 | Morelos |
| 02 | Baja California | 18 | Nayarit |
| 03 | Baja California Sur | 19 | Nuevo León |
| 04 | Campeche | 20 | Oaxaca |
| 05 | Coahuila | 21 | Puebla |
| 06 | Colima | 22 | Querétaro |
| 07 | Chiapas | 23 | Quintana Roo |
| 08 | Chihuahua | 24 | San Luis Potosí |
| 09 | Ciudad de México | 25 | Sinaloa |
| 10 | Durango | 26 | Sonora |
| 11 | Guanajuato | 27 | Tabasco |
| 12 | Guerrero | 28 | Tamaulipas |
| 13 | Hidalgo | 29 | Tlaxcala |
| 14 | Jalisco | 30 | Veracruz |
| 15 | México | 31 | Yucatán |
| 16 | Michoacán | 32 | Zacatecas |

## 📈 Funcionalidades de la Interfaz

### **Panel de Control (Sidebar)**
- 🗺️ **Selector de Entidad**: Dropdown con búsqueda
- 📈 **Selector de Variable**: Lista de indicadores
- 🎨 **Selector de Color**: Paleta de colores personalizable
- 🔘 **Mostrar Puntos**: Switch para puntos en la gráfica
- 🌙 **Tema Oscuro**: Alternativa visual
- 🔄 **Botón Actualizar**: Genera nueva gráfica

### **Área Principal**
- 📊 **Gráfica Interactiva**: Con zoom, pan y tooltips
- 📋 **Estadísticas**: Resumen automático
- 💾 **Descargas**: PNG y PDF de alta resolución

### **Información Contextual**
- 🏷️ **Value Boxes**: Entidad, variable y período
- 📊 **Tabla de Estadísticas**: Min, max, promedio, cambio
- ℹ️ **Panel de Información**: Fuente y descripción

## 🎯 Casos de Uso

### **Análisis Temporal**
- Evolución de la pobreza en una entidad específica
- Tendencias de indicadores sociales
- Impacto de políticas públicas

### **Comparación Geográfica**
- Diferencias entre estados
- Identificación de entidades prioritarias
- Análisis de desigualdades regionales

### **Investigación Académica**
- Datos para tesis y proyectos
- Visualizaciones para publicaciones
- Material didáctico

## 📁 Estructura de Archivos

```
proyecto/
├── app.R                    # Aplicación Shiny principal
├── grafica_pobreza.R        # Funciones de visualización
├── df_pob_ent.csv          # Datos de pobreza
├── instalar_paquetes.R     # Script de instalación
├── test_app.R              # Pruebas de funcionamiento
└── README_app.md           # Esta documentación
```

## 🔧 Resolución de Problemas

### **Error: Paquetes no encontrados**
```r
source('instalar_paquetes.R')
```

### **Error: Archivo CSV no encontrado**
Verificar que `df_pob_ent.csv` esté en el mismo directorio.

### **Error: Función no encontrada**
Verificar que `grafica_pobreza.R` esté en el mismo directorio.

### **Aplicación no responde**
Revisar la consola de R para mensajes de error.

## 📊 Datos

**Fuente**: CONEVAL (Consejo Nacional de Evaluación de la Política de Desarrollo Social)  
**Período**: 2016, 2018, 2020, 2022, 2024  
**Cobertura**: 32 entidades federativas de México  
**Frecuencia**: Bienal  

## 👨‍💻 Desarrollo

Esta aplicación fue desarrollada utilizando:
- **R 4.x** con **Shiny**
- **Tidyverse** para manipulación de datos
- **ggplot2** y **plotly** para visualizaciones
- **shinydashboard** para el diseño
- **shinyWidgets** para controles avanzados

---

**🚀 ¡Disfruta explorando los datos de pobreza en México!**