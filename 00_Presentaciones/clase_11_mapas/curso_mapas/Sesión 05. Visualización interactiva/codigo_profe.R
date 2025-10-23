# En Mac suele funcionar la opción que marca Segasi: 
Sys.setlocale("LC_ALL", "es_ES.UTF-8")
# En Windows/Linux suele funcionar esta función: 
Sys.setlocale("LC_ALL","Spanish")

# Librerias 
library(tidyverse) # Manejo de bases de datos 
library(plotly) # Graficas interactivas
library(gapminder)
library(viridis)
library(htmlwidgets)

# bd GAPMINDER: 
data <- gapminder %>% 
  filter(year=="2007") %>% 
  select(-year)

# Gráfica de burbujas: 
bd_gap <- data %>%
  arrange(desc(pop)) %>%
  mutate(country = factor(country, country)) 

plt <-  bd_gap %>%
  ggplot(aes(x=gdpPercap, y=lifeExp, size=pop, fill=continent)) +
  geom_point(alpha=0.5, shape=21, color="black") +
  scale_size(range = c(.1, 24), name="Population (M)") +
  scale_fill_viridis(discrete=TRUE, guide=FALSE, option="A") +
  theme_minimal() +
  labs(x = "Gdp per Capita", y = "Life Expectancy") +
  theme(legend.position = "bottom")

plt # Gráfica normal de ggplot
ggplotly(plt) # Gráfica de plotly


## Grafica 02: Líneas (BITCOIN) ----
bitcoin <- read_csv("Bases de datos/bitcoin.csv") %>% 
  mutate(Volume = as.numeric(Volume), 
         Close = as.numeric(Close)) %>% 
  filter(!is.na(Volume) | !is.na(Close)) %>% 
  mutate(popup = str_c("<b>Precio de Cierre:</b> $", 
                       prettyNum(Close, 
                                 big.mark = ","), "<br>", 
                       "<b>Fecha:</b> ", Date, "<br>",
                       "<b>Volumen de transacciones:</b> $", prettyNum(Volume, 
                                                                       big.mark = ",")))

plt = bitcoin %>% 
  ggplot(aes(x = Date, 
             y = Close, 
             text = popup, 
             group = "a")) + 
  geom_line() + 
  labs(title = "Bitcoin. Precio de Cierre diario.<br>Datos del 17 de mayo del 2020 al 16 de mayo del 2021.") + 
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5, family = "Arial")) + 
  scale_y_continuous(labels = scales::dollar_format())

# Gráfica de ggplot. 
plt 
# Gráfica interactiva de ggplot
plt_interactiva_btc <- plotly::ggplotly(plt, tooltip = "text") 
plt_interactiva_btc

class(plt_interactiva_btc)

# Lo podemos guardar como widget: 
saveWidget(plt_interactiva_btc, 
           "plt_interactiva_btc.html")

## Gráfica 03: Barras (COMPARATIVO INEGI) ----
educ <- readxl::read_xlsx("Bases de datos/grado_educacion_promedio.xlsx") %>% 
  mutate(popup = str_c("<b>Entidad Federativa: </b>", entidad_federativa, "<br>", 
                       "<b>Municipio: </b>", municipio, "<br>", 
                       "<b>Grado promedio educación: </b>", round(valor, 1), " años"))

plt = educ %>% 
  filter(entidad_federativa == "Morelos") %>% 
  ggplot(aes(x = reorder(municipio, valor), y = valor, text = popup)) + 
  geom_col(fill = "salmon") + 
  coord_flip() + 
  # geom_text(aes(label = str_c(round(valor, 2), " años")), hjust = 0.5) + 
  scale_y_continuous(breaks = 0:11, 
                     limits = c(0, 11), 
                     expand = expansion(c(0,0.001), 0)) +
  # ggthemes::theme_wsj() + 
  labs(title = "Grado Educativo para los municipios de Morelos", 
       y = "Municipios", x = "Años de escuela")

ggplotly(plt, tooltip = "text")
