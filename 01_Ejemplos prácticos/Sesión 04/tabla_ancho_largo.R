

library(tidyverse)

tabla_ancho <- tibble::tribble(
   ~Persona, ~Edad, ~Peso, ~Altura,
  "Roberto",   32L,  168L,    180L,
   "Alicia",   24L,  150L,    175L,
  "Esteban",   64L,  144L,    165L,
  "Juvenal",   29L,  200L,    185L)

tabla_ancho %>% 
  pivot_longer(cols = c(Edad, Peso, Altura))

formato_largo <- tibble::tribble(
   ~Persona, ~Variable, ~Valor,
  "Roberto",    "Edad",    32L,
  "Roberto",    "Peso",   168L,
  "Roberto",  "Altura",   180L,
   "Alicia",    "Edad",    24L,
   "Alicia",    "Peso",   150L,
   "Alicia",  "Altura",   175L,
  "Esteban",    "Edad",    64L,
  "Esteban",    "Peso",   144L,
  "Esteban",  "Altura",   165L,
  "Juvenal",    "Edad",    29L,
  "Juvenal",    "Peso",   200L,
  "Juvenal",  "Altura",   185L
  )

formato_largo %>% 
  pivot_wider(id_cols = Persona, names_from = Variable, 
              values_from = Valor)



