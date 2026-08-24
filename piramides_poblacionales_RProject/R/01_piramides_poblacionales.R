# _____________________________________________________________
# Análisis Demográfico I
# Taller 01. Pirámides poblacionales y calidad de la edad
# Censo de Población y Vivienda 2020, México
# Realizó: Mtra. Samara Michelle Rodríguez Saavedra
# El Colegio de México - Centro de Estudios Demográficos Urbanos y Ambientales
# Maestría en Demografía - Análisis Demográfico 1
# correo: smrodriguez@colmex.mx
# Fecha de actualización: 23 de agosto de 2026
# ____________________________________________________________

# Objetivos:
# 1. Construir una pirámide poblacional por edad simple y sexo.
# 2. Evaluar la calidad de la declaración de la edad:
#    - proporción de edad no especificada
#    - índice de Whipple
#    - índice combinado de Myers
#    - índice de exactitud edad-sexo de Naciones Unidas
# 3. Prorratear la edad no especificada.
# 4. Agrupar la población en grupos quinquenales.
# 5. Construir una pirámide quinquenal.
# 6. Desagrupar y suavizar mediante PCLM.
# 7. Repetir el ejercicio para otra entidad federativa.

# ____________________________________________________________
# 0. Paquetes ----
# ____________________________________________________________

library(tidyverse)
library(readxl)
library(scales)

# Para PCLM:
# install.packages("remotes")
# remotes::install_github("timriffe/DemoTools")
library(DemoTools)
library(ungroup)

# ____________________________________________________________
# 1. Rutas del proyecto ----
# ____________________________________________________________

input_file <- "data/censo2020.xlsx"

dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)

# ____________________________________________________________
# 2. Lectura y limpieza ----
# ____________________________________________________________

raw <- read_xlsx(
  input_file,
  skip = 12,
  col_names = FALSE
)

names(raw) <- c(
  "id", "entidad", "edad", "total", "hombres", "mujeres"
)

#Limpiar los nombres de la base
base <- raw %>%
  filter(!is.na(entidad)) %>%
  filter(
    edad != "Total",
    !str_detect(edad, "^De "),
    !str_detect(edad, "85 años y más")
  ) %>%
  transmute(
    entidad,
    edad_original = edad,
    edad = as.numeric(str_extract(edad, "\\d+")),
    hombres,
    mujeres
  ) %>%
  pivot_longer(
    cols = c(hombres, mujeres),
    names_to = "sexo",
    values_to = "poblacion"
  ) %>%
  mutate(
    sexo = recode(
      sexo,
      hombres = "Hombres",
      mujeres = "Mujeres"
    )
  )

write_csv(base, "data/processed/base_censo2020_limpia.csv")

# ____________________________________________________________
# 3. Selección del territorio ----
# ____________________________________________________________

# República Mexicana:
territorio <- "Total" #Total representa a toda la República Mexicana

# Para el ejercicio final, sustituir por otra entidad:
#unique sirve para ver todos los nombres únicos dentro de una columna, en este caso de las Entidades Federativas de México
unique(base$entidad)

# territorio <- "México"
# territorio <- "Coahuila de Zaragoza"
# territorio <- "Oaxaca"

pob <- base %>%
  filter(entidad == territorio)

# ____________________________________________________________
# 4. Pirámide por edad simple ----
# ____________________________________________________________

pob_simple <- pob %>%
  filter(!is.na(edad)) %>%
  mutate(
    poblacion_graf = if_else(
      sexo == "Hombres",
      -poblacion,
      poblacion  )
  )

max_abs <- max(abs(pob_simple$poblacion_graf), na.rm = TRUE)

#Graficar la pirámide poblacional simple (sin ningun ajuste)
#Esta primera pirámide permite hacer una evaluación gráfica de la preferencia de dígitos
g_piramide_simple <- ggplot(
  pob_simple,
  aes(x = edad, y = poblacion_graf, fill = sexo)
) +
  geom_col(width = 0.9) +
  coord_flip() +
  geom_hline(yintercept = 0) +
  scale_y_continuous(
    labels = label_number(scale = 1e-6, suffix = " M", accuracy = 0.1),
    limits = c(-max_abs, max_abs)
  ) +
  scale_x_continuous(breaks = seq(0, 100, 5)) +
  labs(
    title = paste("Pirámide poblacional por edad simple -", territorio),
    subtitle = "Censo de Población y Vivienda 2020",
    x = "Edad",
    y = "Población",
    fill = "Sexo"
  ) +
  theme_minimal()

# Opción para guardar pirámide en PNG
ggsave(
  "output/figures/01_piramide_edad_simple.png",
  g_piramide_simple,
  width = 8,
  height = 8,
  dpi = 320
)

# ____________________________________________________________
# 5. Edad no especificada ----
# ____________________________________________________________
#Evaluar la cantidad de personas por sexo que no especifican su edad
edad_ne <- pob %>%
  group_by(sexo) %>%
  summarise(
    poblacion_total = sum(poblacion, na.rm = TRUE),
    poblacion_NE = sum(poblacion[is.na(edad)], na.rm = TRUE),
    porcentaje_NE = 100 * poblacion_NE / poblacion_total,
    .groups = "drop"
  )

# Opción para guardar las edades no especificadas
# write_csv(edad_ne, "output/tables/01_edad_no_especificada.csv")

#Estimación de los Índices de preferencia de dígitos
# NOTA: ÍNDICES ----
# Estos índices son herramientas de diagnóstico, no algoritmos de corrección.
# Una vez se identifica el problema, el ajuste o suavizamiento requiere un supuesto o método adicional.

# ____________________________________________________________
# 6. Índice de Whipple ----
# ____________________________________________________________
#Detecta si existe preferencia por declarar edades terminadas en 0 y 5.
#La versión clásica se utiliza para corregir las edades entre 23 a 62 años.

whipple <- pob %>%
  filter(!is.na(edad)) %>%
  group_by(sexo) %>%
  summarise(
    numerador = sum(
      poblacion[
        edad >= 25 &
        edad <= 60 &
        (edad %% 10 == 0 | edad %% 10 == 5)
      ],
      na.rm = TRUE
    ),
    denominador = sum(
      poblacion[edad >= 23 & edad <= 62],
      na.rm = TRUE
    ),
    W = 5 * numerador / denominador * 100,
    .groups = "drop"
  ) %>%
  mutate(
    calidad = case_when(
      W < 105 ~ "Muy precisa",
      W < 110 ~ "Relativamente precisa",
      W < 125 ~ "Aproximada",
      W < 175 ~ "Mala",
      TRUE ~ "Muy mala"
    )
  )

# Opción para guardar el resultado del Índice
# write_csv(whipple, "output/tables/02_indice_whipple.csv")

# ____________________________________________________________
# 7. Índice combinado de Myers ----
# ____________________________________________________________
# Es un indicador resumen que evalúa el nivel de preferencia de dígitos por parte de la población. 
# Permite la evaluación de la preferencia o rechazo de cualquier dígito (0 a 9 años).

#Función para poder calcular la preferencia de dígitos 0 a 9
calcular_myers <- function(df) {

  paso1 <- df %>%
    filter(edad >= 10, edad <= 89) %>%
    mutate(digito = edad %% 10) %>%
    group_by(sexo, digito) %>%
    summarise(
      P1 = sum(poblacion, na.rm = TRUE),
      .groups = "drop"
    )

  paso2 <- df %>%
    filter(edad >= 20, edad <= 99) %>%
    mutate(digito = edad %% 10) %>%
    group_by(sexo, digito) %>%
    summarise(
      P2 = sum(poblacion, na.rm = TRUE),
      .groups = "drop"
    )

  tabla <- full_join(
    paso1,
    paso2,
    by = c("sexo", "digito")
  ) %>%
    mutate(
      P1 = replace_na(P1, 0),
      P2 = replace_na(P2, 0),
      peso1 = digito + 1,
      peso2 = 9 - digito,
      combinada = P1 * peso1 + P2 * peso2
    ) %>%
    group_by(sexo) %>%
    mutate(
      porcentaje = 100 * combinada / sum(combinada),
      desviacion = porcentaje - 10,
      desviacion_abs = abs(desviacion)
    ) %>%
    ungroup()

  indice <- tabla %>%
    group_by(sexo) %>%
    summarise(
      Myers = sum(desviacion_abs) / 2,
      .groups = "drop"
    )

  list(tabla = tabla, indice = indice)
}

myers <- calcular_myers(
  pob %>% filter(!is.na(edad))
)

# Opción para salvar Índice de Myers
write_csv(myers$tabla, "output/tables/03_myers_por_digito.csv")
write_csv(myers$indice, "output/tables/04_indice_myers.csv")

# ____________________________________________________________
# 8. Índice de exactitud edad-sexo de Naciones Unidas
# ____________________________________________________________

pob_5_un <- pob %>%
  filter(!is.na(edad), edad >= 5, edad <= 74) %>%
  mutate(grupo = floor(edad / 5) * 5) %>%
  group_by(grupo, sexo) %>%
  summarise(
    poblacion = sum(poblacion, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = sexo,
    values_from = poblacion
  ) %>%
  arrange(grupo) %>%
  mutate(
    sex_ratio = 100 * Hombres / Mujeres,
    dif_sex_ratio = abs(sex_ratio - lag(sex_ratio)),
    age_ratio_H = 200 * Hombres / (lag(Hombres) + lead(Hombres)),
    age_ratio_M = 200 * Mujeres / (lag(Mujeres) + lead(Mujeres)),
    dev_age_H = abs(age_ratio_H - 100),
    dev_age_M = abs(age_ratio_M - 100)
  )

sex_ratio_score <- pob_5_un %>%
  filter(grupo >= 10, grupo <= 65) %>%
  summarise(score = mean(dif_sex_ratio, na.rm = TRUE)) %>%
  pull(score)

age_ratio_score_H <- pob_5_un %>%
  filter(grupo >= 10, grupo <= 65) %>%
  summarise(score = mean(dev_age_H, na.rm = TRUE)) %>%
  pull(score)

age_ratio_score_M <- pob_5_un %>%
  filter(grupo >= 10, grupo <= 65) %>%
  summarise(score = mean(dev_age_M, na.rm = TRUE)) %>%
  pull(score)

indice_onu <- tibble(
  sex_ratio_score = sex_ratio_score,
  age_ratio_score_H = age_ratio_score_H,
  age_ratio_score_M = age_ratio_score_M,
  indice_ONU = 3 * sex_ratio_score +
    age_ratio_score_H +
    age_ratio_score_M
)

#Opción para salvar Índice de Naciones Unidas 
write_csv(indice_onu, "output/tables/05_indice_ONU_edad_sexo.csv")

# ____________________________________________________________
# 9. Prorrateo de la edad no especificada ----
# ____________________________________________________________
# El prorrateo distribuye de forma proporcional a las personas con edad ignorada (no declarada) entre los grupos de edad que sí se conocen, asumiendo que ambos subgrupos comparten la misma estructura demográfica.
# La proporción se calcula solo entre las edades conocidas.

pob_conocida <- pob %>%
  filter(!is.na(edad)) %>%
  group_by(sexo) %>%
  mutate(
    prop_edad = poblacion / sum(poblacion)
  ) %>%
  ungroup()

pob_NE <- pob %>%
  filter(is.na(edad)) %>%
  group_by(sexo) %>%
  summarise(
    poblacion_NE = sum(poblacion, na.rm = TRUE),
    .groups = "drop"
  )

#Base ya con la población prorrateada
pob_prorrateada <- pob_conocida %>%
  left_join(pob_NE, by = "sexo") %>%
  mutate(
    poblacion_ajustada =
      poblacion + poblacion_NE * prop_edad
  ) %>%
  select(
    entidad,
    edad,
    sexo,
    poblacion_original = poblacion,
    poblacion_ajustada
  )

#Salvar base ya con la población prorrateada 
write_csv(
  pob_prorrateada,
  "data/processed/poblacion_prorrateada.csv"
)

# ____________________________________________________________
# 10. Grupos quinquenales ----
# ____________________________________________________________

bins <- seq(0, 105, by = 5)

pob_5 <- pob_prorrateada %>%
  mutate(
    grupo_edad = cut(
      edad,
      breaks = bins,
      right = FALSE,
      include.lowest = TRUE,
      labels = paste(
        bins[-length(bins)],
        bins[-1] - 1,
        sep = "-"
      )
    )
  ) %>%
  filter(!is.na(grupo_edad)) %>%
  group_by(sexo, grupo_edad) %>%
  summarise(
    poblacion = sum(poblacion_ajustada),
    .groups = "drop"
  ) %>%
  mutate(
    poblacion_graf = if_else(
      sexo == "Hombres",
      -poblacion,
      poblacion
    )
  )

# Salvar base ya por grupos quinquenales
write_csv(
  pob_5,
  "data/processed/poblacion_grupos_quinquenales.csv"
)

#  ____________________________________________________________
# 11. Pirámide quinquenal ----
#  ____________________________________________________________

max_abs_5 <- max(abs(pob_5$poblacion_graf), na.rm = TRUE)

#Grafico pirámide poblacional por grupos quinquenales y prorrateada
g_piramide_5 <- ggplot(
  pob_5,
  aes(x = grupo_edad, y = poblacion_graf, fill = sexo)
) +
  geom_col() +
  coord_flip() +
  geom_hline(yintercept = 0) +
  scale_y_continuous(
    labels = label_number(scale = 1e-6, suffix = " M", accuracy = 0.1),
    limits = c(-max_abs_5, max_abs_5)
  ) +
  labs(
    title = paste("Pirámide poblacional quinquenal -", territorio),
    subtitle = "Población prorrateada",
    x = "Grupo de edad",
    y = "Población",
    fill = "Sexo"
  ) +
  theme_minimal()

# Guardar pirámide prorrateada y por grupos quinquenales en PNG
ggsave(
  "output/figures/02_piramide_quinquenal.png",
  g_piramide_5,
  width = 8,
  height = 8,
  dpi = 320
)

#  ____________________________________________________________
# 12. Desagregación y suavizamiento con PCLM ----
#  ____________________________________________________________
# La función PCLM sirve para ajustar un modelo de enlace compuesto penalizado (PCLM) univariante para desagregar datos de recuento agrupados 
# en intervalos (por ejemplo, distribuciones de edad al fallecer agrupadas por clases de edad).
# NOTA:
# PCLM no corrige directamente las edades terminadas en 0 y 5.
# Primero se agrupan las edades simples en intervalos quinquenales, con lo que se elimina gran parte de la irregularidad por dígito terminal.
# Posteriormente, PCLM desagrega esos totales quinquenales y estima una distribución suavizada por edad simple.

pob_5_H <- pob_5 %>%
  filter(sexo == "Hombres") %>%
  arrange(grupo_edad)

pob_5_M <- pob_5 %>%
  filter(sexo == "Mujeres") %>%
  arrange(grupo_edad)

x_pclm <- bins[-length(bins)]

# Aplicar PCLM para hombres
pclm_H <- pclm(
  x = x_pclm,
  y = pob_5_H$poblacion,
  nlast = 10
)

# Aplicar PCLM para mujeres
pclm_M <- pclm(
  x = x_pclm,
  y = pob_5_M$poblacion,
  nlast = 10
)

#Suavizamiento significa: estimar una curva continua/suave a través de las edades, 
# utilizando simultáneamente la información de los distintos grupos de edad y una penalización que evita cambios excesivamente bruscos.
pob_suavizada <- bind_rows(
  tibble(
    edad = seq_along(pclm_H$fitted) - 1,
    sexo = "Hombres",
    poblacion = pclm_H$fitted
  ),
  tibble(
    edad = seq_along(pclm_M$fitted) - 1,
    sexo = "Mujeres",
    poblacion = pclm_M$fitted
  )
) %>%
  mutate(
    poblacion_graf = if_else(
      sexo == "Hombres",
      -poblacion,
      poblacion
    )
  )

#Guardar pob suavizada 
write_csv(
  pob_suavizada,
  "data/processed/poblacion_suavizada_PCLM.csv"
)

#  ____________________________________________________________
# 13. Pirámide suavizada ----
#  ____________________________________________________________

max_abs_s <- max(abs(pob_suavizada$poblacion_graf), na.rm = TRUE)

g_piramide_suavizada <- ggplot(
  pob_suavizada,
  aes(x = edad, y = poblacion_graf, fill = sexo)
) +
  geom_col(width = 0.9) +
  coord_flip() +
  geom_hline(yintercept = 0) +
  scale_y_continuous(
    labels = label_number(scale = 1e-6, suffix = " M", accuracy = 0.1),
    limits = c(-max_abs_s, max_abs_s)
  ) +
  scale_x_continuous(breaks = seq(0, 110, 5)) +
  labs(
    title = paste("Pirámide poblacional suavizada -", territorio),
    subtitle = "Desagregación PCLM",
    x = "Edad",
    y = "Población",
    fill = "Sexo"
  ) +
  theme_minimal()

ggsave(
  "output/figures/03_piramide_suavizada_PCLM.png",
  g_piramide_suavizada,
  width = 8,
  height = 8,
  dpi = 320
)

#  ____________________________________________________________
# 14. Ejercicio para estudiantes ----
#  ____________________________________________________________

# Repetir el análisis cambiando:
#
# territorio <- "Nombre de otra entidad"
#
# Entregables:
# 1. Pirámide por edad simple.
# 2. Porcentaje de edad no especificada por sexo.
# 3. Índice de Whipple.
# 4. Índice de Myers.
# 5. Índice ONU edad-sexo.
# 6. Pirámide quinquenal después del prorrateo.
# 7. Pirámide suavizada mediante PCLM.
# 8. Análisis comparando la entidad con la República Mexicana.

#Felicidades por llegar hasta aquí. Se aceptan sugerencias o dudas al código al siguiente correo.
# correo: smrodriguez@colmex.mx

