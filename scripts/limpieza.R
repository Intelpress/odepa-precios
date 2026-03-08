# Este script carga los datos crudos de ODEPA, realiza una auditoría de calidad
# sobre el dataset original, investiga las anomalías encontradas, y construye
# el dataset limpio datos_clean listo para análisis.

library(tidyverse)
library(janitor)

# ── CARGA Y ESTANDARIZACIÓN ───────────────────────────────────────────────────

datos_raw <- read_csv(
  "data/raw/precio_consumidor_publico_2025.csv",
  locale = locale(
    decimal_mark  = ",",
    grouping_mark = "."
  )
) |>
  clean_names()

# ── AUDITORÍA DE CALIDAD (datos crudos) ───────────────────────────────────────

# PASO 1: NAs en precios
message("PASO 1: NAs en precios")
datos_raw |>
  summarise(
    na_minimo   = sum(is.na(precio_minimo)),
    na_maximo   = sum(is.na(precio_maximo)),
    na_promedio = sum(is.na(precio_promedio))
  ) |> print()

# PASO 2: NAs en categorías
message("PASO 2: NAs en categorías")
datos_raw |>
  summarise(
    na_region   = sum(is.na(region)),
    na_sector   = sum(is.na(sector)),
    na_grupo    = sum(is.na(grupo)),
    na_producto = sum(is.na(producto)),
    na_fecha    = sum(is.na(fecha_inicio))
  ) |> print()

# PASO 3: Duplicados
message("PASO 3: Duplicados (conteo total)")
datos_raw |>
  group_by(fecha_inicio, id_region, sector,
           tipo_de_punto_monitoreo, producto) |>
  filter(n() > 1) |>
  ungroup() |>
  nrow() |> print()

message("Duplicados con precios idénticos vs distintos")
datos_raw |>
  group_by(fecha_inicio, id_region, sector,
           tipo_de_punto_monitoreo, producto) |>
  filter(n() > 1) |>
  ungroup() |>
  mutate(
    precios_identicos = precio_minimo   == lag(precio_minimo) &
      precio_maximo   == lag(precio_maximo) &
      precio_promedio == lag(precio_promedio)
  ) |>
  count(precios_identicos) |> print()

# PASO 4: Consistencia de precios
message("PASO 4: Consistencia de precios (Min > Max)")
datos_raw |>
  filter(precio_minimo > precio_maximo) |>
  nrow() |> print()

message("Promedio fuera de rango")
datos_raw |>
  filter(
    precio_promedio < precio_minimo |
      precio_promedio > precio_maximo
  ) |>
  nrow() |> print()

# PASO 5: Valores imposibles
message("PASO 5: Valores imposibles (Precios <= 0)")
datos_raw |>
  filter(precio_promedio <= 0 | precio_minimo <= 0 | precio_maximo <= 0) |>
  nrow() |> print()

# PASO 6: Consistencia de unidades
message("PASO 6: Consistencia de unidades (Frecuencia)")
datos_raw |>
  count(unidad, sort = TRUE) |> print()

message("Productos con múltiples unidades")
datos_raw |>
  group_by(producto) |>
  summarise(
    n_unidades = n_distinct(unidad),
    unidades   = paste(unique(unidad), collapse = " | "),
    .groups    = "drop"
  ) |>
  filter(n_unidades > 1) |>
  arrange(desc(n_unidades)) |>
  print(n = 39)

# PASO 7: Outliers extremos (Z > 4)
message("PASO 7: Outliers extremos (Z > 4)")
datos_raw |>
  group_by(producto) |>
  mutate(
    z = (precio_promedio - mean(precio_promedio, na.rm = TRUE)) /
      sd(precio_promedio, na.rm = TRUE)
  ) |>
  ungroup() |>
  filter(abs(z) > 4) |>
  arrange(desc(abs(z))) |>
  select(fecha_inicio, region, sector, producto,
         unidad, precio_promedio, z) |>
  head(20) |> print()

# ── RESULTADOS AUDITORÍA DE CALIDAD ──────────────────────────────────────────
# El dataset 2025 pasó satisfactoriamente los controles de NAs, consistencia
# de precios y valores imposibles. Se identificaron tres áreas que requieren
# atención antes de construir datos_clean:
#
# 1. DUPLICADOS (8.932 filas en datos_raw): Al investigar las filas que
#    compartían la misma combinación de fecha/región/sector/canal/producto,
#    se encontró que el 99% tienen precios distintos — son múltiples vendedores
#    dentro del mismo sector (ej: varios puestos en una feria libre registrando
#    precios diferentes para el mismo producto). Son observaciones legítimas.
#    Solo 67 filas (0.020% del total) tienen precios exactamente idénticos.
#    Dado su impacto estadístico negligible y la imposibilidad de determinar
#    su causa con certeza, se retienen y documentan.
#
# 2. OUTLIERS (20 casos con Z > 4): precios extremadamente altos o bajos en
#    relación al promedio del mismo producto. Casos como Arvejas verdes partidas
#    a $21.200/kilo (Z=15.9) o Frambuesas a $31.286/kilo (Z=8.8) pueden
#    corresponder a errores de captura o precios estacionales extremos. Se
#    retienen en datos_clean para no perder información — el análisis posterior
#    deberá considerar su presencia.
#
# 3. UNIDADES INCONSISTENTES (39 productos): tres casos distintos:
#    - Huevos reportados en distintas presentaciones (bandeja 12, 20, 30
#      unidades; caja 100 y 180 unidades) — se normalizan a $/unidad.
#    - Aceite vegetal reportado en $/botella 900 ml con una fila errónea en
#      $/litro — se elimina la fila errónea y se normaliza todo a $/litro.
#    - Algunas verduras (Cebolla, Pepino ensalada, Sandía, Zapallo italiano)
#      reportadas ocasionalmente en $/unidad en vez de $/kilo — se marcan
#      con flag para análisis posterior, no se normalizan porque el precio
#      por unidad depende del tamaño del ejemplar.
#    - Legumbres reportadas en distintos envases ($/kilo, $/kilo en envase
#      de 1 kilo, $/kilo en saco de 25 kilos) — todas son $/kilo, solo
#      difieren en el formato de venta. No requieren normalización de precio.
# ─────────────────────────────────────────────────────────────────────────────

# ── INVESTIGACIÓN DE UNIDADES ─────────────────────────────────────────────────

# Volatilidad dentro de Lácteos - Huevos - Margarinas (confirma problema huevos)
message("Volatilidad por producto — Lácteos, Huevos, Margarinas")
datos_raw |>
  filter(grupo == "Lácteos - Huevos - Margarinas") |>
  group_by(producto) |>
  summarise(
    cv   = sd(precio_promedio, na.rm = TRUE) / mean(precio_promedio, na.rm = TRUE),
    mean = mean(precio_promedio, na.rm = TRUE),
    obs  = n(),
    .groups = "drop"
  ) |>
  filter(obs >= 20) |>
  arrange(desc(cv)) |>
  print()

# Distribución real de unidades en huevos
message("Unidades en huevos")
datos_raw |>
  filter(str_detect(producto, "Huevo")) |>
  count(producto, unidad, sort = TRUE) |>
  print(n = 50)

# Distribución de precios aceite vegetal por unidad
message("Precios aceite vegetal por unidad")
datos_raw |>
  filter(producto == "Aceite vegetal") |>
  group_by(unidad) |>
  summarise(
    min  = min(precio_promedio,  na.rm = TRUE),
    mean = mean(precio_promedio, na.rm = TRUE),
    max  = max(precio_promedio,  na.rm = TRUE),
    obs  = n(),
    .groups = "drop"
  ) |>
  print()

# Fila errónea de aceite ($/litro con precio de botella — error de captura)
message("Fila errónea aceite vegetal")
datos_raw |>
  filter(producto == "Aceite vegetal", unidad == "$/litro") |>
  select(fecha_inicio, region, sector, tipo_de_punto_monitoreo,
         unidad, precio_minimo, precio_maximo, precio_promedio) |>
  print()

# ── CONSTRUCCIÓN DEL DATASET LIMPIO ──────────────────────────────────────────

# Vector de verduras reportadas en $/unidad — se marcan con flag, no se
# normalizan, porque el precio por unidad depende del tamaño del ejemplar.
verduras_unidad_productos <- c(
  "Cebolla|Sin especificar|1a (cosecha)",
  "Cebolla|Sin especificar|1a (guarda)",
  "Cebolla|Sin especificar|1a nueva(o)",
  "Cebolla|Sin especificar|2a (cosecha)",
  "Cebolla|Sin especificar|2a (guarda)",
  "Cebolla|Sin especificar|2a nueva(o)",
  "Pepino ensalada|Sin especificar|Primera",
  "Pepino ensalada|Sin especificar|Segunda",
  "Sandia|Sin especificar|Primera",
  "Sandia|Sin especificar|Segunda",
  "Zapallo italiano|Sin especificar|Primera",
  "Zapallo italiano|Sin especificar|Segunda"
)

datos_clean <- datos_raw |>
  
  # PASO 1: eliminar fila errónea de aceite ANTES de normalizar
  # (1 fila: Araucanía, Recabarren, 2025-06-02, unidad $/litro con precio
  # de botella — error de captura confirmado por precio fuera de rango)
  filter(!(
    producto     == "Aceite vegetal"      &
      unidad       == "$/litro"             &
      fecha_inicio == as.Date("2025-06-02") &
      id_region    == 9
  )) |>
  
  mutate(
    
    # PASO 2: multiplicador para convertir precios de huevos a $/unidad
    multiplicador = case_when(
      str_detect(unidad, "180 unidades") ~ 180,
      str_detect(unidad, "100 unidades") ~ 100,
      str_detect(unidad,  "30 unidades") ~  30,
      str_detect(unidad,  "20 unidades") ~  20,
      str_detect(unidad,  "12 unidades") ~  12,
      TRUE                               ~ NA_real_
    ),
    
    # PASO 3: normalizar huevos a $/unidad
    precio_minimo = case_when(
      str_detect(producto, "Huevo") & !is.na(multiplicador) ~
        precio_minimo / multiplicador,
      TRUE ~ precio_minimo
    ),
    precio_maximo = case_when(
      str_detect(producto, "Huevo") & !is.na(multiplicador) ~
        precio_maximo / multiplicador,
      TRUE ~ precio_maximo
    ),
    precio_promedio = case_when(
      str_detect(producto, "Huevo") & !is.na(multiplicador) ~
        precio_promedio / multiplicador,
      TRUE ~ precio_promedio
    ),
    
    # PASO 4: normalizar aceite vegetal a $/litro
    # ($/botella 900 ml → $/litro: dividir por 0.9)
    precio_minimo = case_when(
      producto == "Aceite vegetal" & unidad == "$/botella 900 ml" ~
        precio_minimo / 0.9,
      TRUE ~ precio_minimo
    ),
    precio_maximo = case_when(
      producto == "Aceite vegetal" & unidad == "$/botella 900 ml" ~
        precio_maximo / 0.9,
      TRUE ~ precio_maximo
    ),
    precio_promedio = case_when(
      producto == "Aceite vegetal" & unidad == "$/botella 900 ml" ~
        precio_promedio / 0.9,
      TRUE ~ precio_promedio
    ),
    
    # PASO 5: estandarizar etiquetas de unidad
    unidad = case_when(
      str_detect(producto, "Huevo") & !is.na(multiplicador) ~ "$/unidad",
      producto == "Aceite vegetal" & unidad == "$/botella 900 ml" ~ "$/litro",
      TRUE ~ unidad
    ),
    
    # PASO 6: marcar verduras reportadas en $/unidad
    unidad_flag = case_when(
      producto %in% verduras_unidad_productos &
        unidad == "$/unidad" ~ "verdura_por_unidad",
      TRUE ~ "ok"
    )
    
  ) |>
  select(-multiplicador)

# ── VERIFICACIÓN ──────────────────────────────────────────────────────────────

message("1. Total filas (esperado: 333288)")
nrow(datos_clean) |> print()

message("2. Aceite: solo $/litro (esperado: 1 unidad, 1751 filas)")
datos_clean |>
  filter(producto == "Aceite vegetal") |>
  count(unidad) |>
  print()

message("3. Fila errónea eliminada (esperado: 6 filas legítimas restantes)")
datos_clean |>
  filter(
    producto     == "Aceite vegetal",
    fecha_inicio == as.Date("2025-06-02"),
    id_region    == 9
  ) |>
  nrow() |> print()

message("4. Distribución de flags")
datos_clean |>
  count(unidad_flag) |>
  mutate(pct = n / sum(n) * 100) |>
  print()

message("5. CV de huevos post-normalización (referencia: antes era > 0.9)")
datos_clean |>
  filter(str_detect(producto, "Huevo")) |>
  group_by(producto) |>
  summarise(
    mean = mean(precio_promedio, na.rm = TRUE),
    cv   = sd(precio_promedio, na.rm = TRUE) / mean,
    obs  = n(),
    .groups = "drop"
  ) |>
  arrange(desc(cv)) |>
  print()

# ── CERTIFICACIÓN FINAL ───────────────────────────────────────────────────────

stopifnot(
  "Filas incorrectas"    = nrow(datos_clean) == 333288,
  "Precios negativos"    = sum(datos_clean$precio_promedio <= 0) == 0,
  "Min > Max"            = sum(datos_clean$precio_minimo >
                                 datos_clean$precio_maximo) == 0,
  "Promedio fuera rango" = sum(
    (datos_clean$precio_promedio < datos_clean$precio_minimo) |
      (datos_clean$precio_promedio > datos_clean$precio_maximo)
  ) == 0,
  "NAs en precio"        = sum(is.na(datos_clean$precio_promedio)) == 0,
  "NAs en categorías"    = sum(is.na(datos_clean$region)) == 0,
  "Aceite unidad única"  = n_distinct(
    filter(datos_clean,
           producto == "Aceite vegetal")$unidad
  ) == 1,
  "Flags completos"      = sum(!datos_clean$unidad_flag %in%
                                 c("ok", "verdura_por_unidad")) == 0
)

cat("✅ Todas las verificaciones pasaron. datos_clean está listo.\n")