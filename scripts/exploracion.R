# Este script genera visualizaciones exploratorias sobre datos_clean para
# identificar patrones, distribuciones y anomalías residuales tras la limpieza.
# Los gráficos están organizados en 5 categorías: distribución de precios,
# tiempo, geografía, productos y calidad de datos.
# Todos los gráficos se guardan en output/figures/.



# ── DEPENDENCIAS ──────────────────────────────────────────────────────────────

source("scripts/limpieza.R")

# ── DISTRIBUCIÓN DE PRECIOS ───────────────────────────────────────────────────

# 1. Histograma general de precio_promedio
ggplot(datos_clean, aes(x = precio_promedio)) +
  geom_histogram(bins = 100, fill = "steelblue", color = "white") +
  scale_x_continuous(
    labels = scales::comma_format(big.mark = ".", decimal.mark = ",")
  ) +
  labs(
    title = "Distribución general de precio promedio",
    x     = "Precio promedio (CLP)",
    y     = "Frecuencia"
  ) +
  theme_minimal()

ggsave("output/figures/01_histograma_precio_promedio.png",
       width = 10, height = 6, dpi = 150)

# 2. Densidad por grupo
ggplot(datos_clean, aes(x = precio_promedio, fill = grupo)) +
  geom_density(alpha = 0.4) +
  scale_x_continuous(
    labels = scales::comma_format(big.mark = ".", decimal.mark = ",")
  ) +
  labs(
    title = "Densidad de precio promedio por grupo",
    x     = "Precio promedio (CLP)",
    y     = "Densidad",
    fill  = "Grupo"
  ) +
  theme_minimal()

ggsave("output/figures/02_densidad_por_grupo.png",
       width = 10, height = 6, dpi = 150)

# 3. Densidad por unidad
ggplot(datos_clean, aes(x = precio_promedio, fill = unidad)) +
  geom_density(alpha = 0.4) +
  scale_x_continuous(
    labels = scales::comma_format(big.mark = ".", decimal.mark = ",")
  ) +
  labs(
    title = "Densidad de precio promedio por unidad",
    x     = "Precio promedio (CLP)",
    y     = "Densidad",
    fill  = "Unidad"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("output/figures/03_densidad_por_unidad.png",
       width = 12, height = 6, dpi = 150)

# 4. Densidad por canal
ggplot(datos_clean, aes(x = precio_promedio,
                      fill = tipo_de_punto_monitoreo)) +
  geom_density(alpha = 0.4) +
  scale_x_continuous(
    labels = scales::comma_format(big.mark = ".", decimal.mark = ",")
  ) +
  labs(
    title = "Densidad de precio promedio por canal",
    x     = "Precio promedio (CLP)",
    y     = "Densidad",
    fill  = "Canal"
  ) +
  theme_minimal()

ggsave("output/figures/04_densidad_por_canal.png",
       width = 10, height = 6, dpi = 150)

# ── TIEMPO ────────────────────────────────────────────────────────────────────

# 5. Precio promedio semanal por canal
datos_clean |>
  group_by(fecha_inicio, tipo_de_punto_monitoreo) |>
  summarise(avg_price = mean(precio_promedio, na.rm = TRUE),
            .groups = "drop") |>
  ggplot(aes(x = fecha_inicio, y = avg_price,
             color = tipo_de_punto_monitoreo)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 0.5) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  scale_y_continuous(
    labels = scales::comma_format(big.mark = ".", decimal.mark = ",")
  ) +
  labs(
    title  = "Precio promedio semanal por canal",
    x      = "Fecha",
    y      = "Precio promedio (CLP)",
    color  = "Canal"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("output/figures/05_precio_semanal_por_canal.png",
       width = 12, height = 6, dpi = 150)

# 6. Precio promedio semanal por grupo
datos_clean |>
  group_by(fecha_inicio, grupo) |>
  summarise(avg_price = mean(precio_promedio, na.rm = TRUE),
            .groups = "drop") |>
  ggplot(aes(x = fecha_inicio, y = avg_price, color = grupo)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 0.5) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  scale_y_continuous(
    labels = scales::comma_format(big.mark = ".", decimal.mark = ",")
  ) +
  labs(
    title = "Precio promedio semanal por grupo",
    x     = "Fecha",
    y     = "Precio promedio (CLP)",
    color = "Grupo"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("output/figures/06_precio_semanal_por_grupo.png",
       width = 12, height = 6, dpi = 150)

# 7. Número de observaciones por semana
datos_clean |>
  count(fecha_inicio) |>
  ggplot(aes(x = fecha_inicio, y = n)) +
  geom_col(fill = "steelblue") +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  labs(
    title = "Número de observaciones por semana",
    x     = "Fecha",
    y     = "Observaciones"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("output/figures/07_observaciones_por_semana.png",
       width = 12, height = 6, dpi = 150)

# ── GEOGRAFÍA ─────────────────────────────────────────────────────────────────

# 8. Boxplot de precios por región
ggplot(datos_clean, aes(x = reorder(region, precio_promedio, median),
                      y = precio_promedio)) +
  geom_boxplot(fill = "steelblue", outlier.size = 0.5, outlier.alpha = 0.3) +
  scale_y_continuous(
    labels = scales::comma_format(big.mark = ".", decimal.mark = ",")
  ) +
  labs(
    title = "Distribución de precios por región",
    x     = "Región",
    y     = "Precio promedio (CLP)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("output/figures/08_boxplot_precios_por_region.png",
       width = 12, height = 6, dpi = 150)

# 9. Número de observaciones por región
datos_clean |>
  count(region) |>
  ggplot(aes(x = reorder(region, n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Número de observaciones por región",
    x     = "Región",
    y     = "Observaciones"
  ) +
  theme_minimal()

ggsave("output/figures/09_observaciones_por_region.png",
       width = 10, height = 6, dpi = 150)

# 10. Número de sectores por región
datos_clean |>
  distinct(region, sector) |>
  count(region) |>
  ggplot(aes(x = reorder(region, n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Número de sectores monitoreados por región",
    x     = "Región",
    y     = "Sectores"
  ) +
  theme_minimal()

ggsave("output/figures/10_sectores_por_region.png",
       width = 10, height = 6, dpi = 150)

# ── PRODUCTOS ─────────────────────────────────────────────────────────────────

# 11. Top 20 productos más caros
datos_clean |>
  group_by(producto) |>
  summarise(media = mean(precio_promedio, na.rm = TRUE), .groups = "drop") |>
  slice_max(media, n = 20) |>
  ggplot(aes(x = reorder(producto, media), y = media)) +
  geom_col(fill = "tomato") +
  coord_flip() +
  scale_y_continuous(
    labels = scales::comma_format(big.mark = ".", decimal.mark = ",")
  ) +
  labs(
    title = "Top 20 productos más caros (precio promedio medio)",
    x     = "Producto",
    y     = "Precio promedio (CLP)"
  ) +
  theme_minimal()

ggsave("output/figures/11_top20_productos_caros.png",
       width = 10, height = 8, dpi = 150)

# 12. Top 20 productos más baratos
datos_clean |>
  group_by(producto) |>
  summarise(media = mean(precio_promedio, na.rm = TRUE), .groups = "drop") |>
  slice_min(media, n = 20) |>
  ggplot(aes(x = reorder(producto, media), y = media)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  scale_y_continuous(
    labels = scales::comma_format(big.mark = ".", decimal.mark = ",")
  ) +
  labs(
    title = "Top 20 productos más baratos (precio promedio medio)",
    x     = "Producto",
    y     = "Precio promedio (CLP)"
  ) +
  theme_minimal()

ggsave("output/figures/12_top20_productos_baratos.png",
       width = 10, height = 8, dpi = 150)

# 13. Volatilidad (CV) por grupo
datos_clean |>
  group_by(grupo, producto) |>
  summarise(
    cv  = sd(precio_promedio, na.rm = TRUE) /
      mean(precio_promedio, na.rm = TRUE),
    obs = n(),
    .groups = "drop"
  ) |>
  filter(obs >= 20) |>
  group_by(grupo) |>
  summarise(mean_cv = mean(cv), .groups = "drop") |>
  ggplot(aes(x = reorder(grupo, mean_cv), y = mean_cv)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Volatilidad promedio (CV) por grupo de producto",
    x     = "Grupo",
    y     = "Coeficiente de variación promedio"
  ) +
  theme_minimal()

ggsave("output/figures/13_volatilidad_por_grupo.png",
       width = 10, height = 6, dpi = 150)

# ── CALIDAD DE DATOS ──────────────────────────────────────────────────────────

# 14. Scatter precio_minimo vs precio_maximo
ggplot(datos_clean, aes(x = precio_minimo, y = precio_maximo)) +
  geom_point(alpha = 0.1, size = 0.5, color = "steelblue") +
  geom_abline(slope = 1, intercept = 0,
              color = "red", linetype = "dashed") +
  scale_x_continuous(
    labels = scales::comma_format(big.mark = ".", decimal.mark = ",")
  ) +
  scale_y_continuous(
    labels = scales::comma_format(big.mark = ".", decimal.mark = ",")
  ) +
  labs(
    title    = "Precio mínimo vs precio máximo",
    subtitle = "La línea roja indica min = max",
    x        = "Precio mínimo (CLP)",
    y        = "Precio máximo (CLP)"
  ) +
  theme_minimal()

ggsave("output/figures/14_scatter_min_vs_max.png",
       width = 10, height = 8, dpi = 150)

# 15. Distribución de outliers por región y canal
datos_clean |>
  group_by(producto) |>
  mutate(
    z = (precio_promedio - mean(precio_promedio, na.rm = TRUE)) /
      sd(precio_promedio, na.rm = TRUE)
  ) |>
  ungroup() |>
  filter(abs(z) > 4) |>
  count(region, tipo_de_punto_monitoreo) |>
  ggplot(aes(x = reorder(region, n),
             y = n,
             fill = tipo_de_punto_monitoreo)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Distribución de outliers (Z > 4) por región y canal",
    x     = "Región",
    y     = "Número de outliers",
    fill  = "Canal"
  ) +
  theme_minimal()

ggsave("output/figures/15_outliers_por_region_canal.png",
       width = 10, height = 6, dpi = 150)

message("✅ 15 gráficos guardados en output/figures/")