# Este script consulta el API de ODEPA para obtener el inventario completo
# de URLs de descarga de precios al consumidor (2008-2026).
# El inventario se guarda localmente en data/processed/ para no tener que
# consultar el API cada vez que trabajemos con los datos.


library(tidyverse)
library(janitor)
library(httr)
library(jsonlite)

# ── INVENTARIO DE FUENTES ODEPA ───────────────────────────────────────────────

url_api <- "https://datos.odepa.gob.cl/api/3/action/package_show?id=c3ca8246-3d84-4145-9e34-525b0ba95859"

respuesta <- GET(url_api)
datos_api <- fromJSON(content(respuesta, "text", encoding = "UTF-8"))

urls_odepa <- datos_api$result$resources |>
  as_tibble() |>
  select(name, url) |>
  mutate(anio = str_extract(url, "\\d{4}(?=\\.csv)")) |>
  filter(!is.na(anio)) |>
  distinct(anio, .keep_all = TRUE) |>
  arrange(anio)

# ── GUARDAR INVENTARIO ────────────────────────────────────────────────────────

saveRDS(urls_odepa, "data/processed/urls_odepa.rds")

cat("✅ Inventario guardado:", nrow(urls_odepa), "años disponibles\n")