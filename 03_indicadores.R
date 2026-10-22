# ============================================================
# 03_indicadores.R
# Cálculo de indicadores de comercio exterior
# ============================================================

# 1. Verificar paquetes
paquetes <- c(
  "here",
  "readr",
  "dplyr",
  "tidyr",
  "scales"
)

faltantes <- paquetes[
  !vapply(paquetes, requireNamespace, logical(1), quietly = TRUE)
]

if (length(faltantes) > 0) {
  stop(
    paste(
      "Faltan paquetes:",
      paste(faltantes, collapse = ", ")
    )
  )
}

# 2. Leer información limpia
ruta_limpia <- here::here(
  "data",
  "processed",
  "comercio_limpio.csv"
)

if (!file.exists(ruta_limpia)) {
  stop(
    paste(
      "No se encontró comercio_limpio.csv.",
      "Ejecuta primero 02_limpieza_datos.R"
    )
  )
}

comercio_limpio <- readr::read_csv(
  ruta_limpia,
  show_col_types = FALSE,
  progress = FALSE
) |>
  dplyr::mutate(
    fecha = as.Date(.data$fecha),
    monto_usd = as.numeric(.data$monto_usd)
  )

# 3. Verificar columnas
columnas_necesarias <- c(
  "territorio",
  "fecha",
  "flujo",
  "monto_usd"
)

columnas_faltantes <- setdiff(
  columnas_necesarias,
  names(comercio_limpio)
)

if (length(columnas_faltantes) > 0) {
  stop(
    paste(
      "Faltan columnas:",
      paste(columnas_faltantes, collapse = ", ")
    )
  )
}

# 4. Función para dividir sin provocar errores
division_segura <- function(numerador, denominador) {
  dplyr::if_else(
    !is.na(denominador) & denominador != 0,
    numerador / denominador,
    NA_real_
  )
}

# 5. Clasificar el movimiento
clasificar_movimiento <- function(cambio) {
  dplyr::case_when(
    is.na(cambio) ~ "Sin dato comparable",
    cambio > 0 ~ "Aumentó",
    cambio < 0 ~ "Disminuyó",
    TRUE ~ "Se mantuvo sin cambio"
  )
}

# 6. Construir texto automático del movimiento
crear_texto_movimiento <- function(
    movimiento,
    variacion,
    comparacion
) {
  
  porcentaje <- scales::percent(
    abs(variacion),
    accuracy = 0.1,
    decimal.mark = "."
  )
  
  dplyr::case_when(
    is.na(variacion) ~ paste(
      "No existe información suficiente para comparar con",
      comparacion
    ),
    
    movimiento == "Aumentó" ~ paste(
      "Aumentó",
      porcentaje,
      "respecto a",
      comparacion
    ),
    
    movimiento == "Disminuyó" ~ paste(
      "Disminuyó",
      porcentaje,
      "respecto a",
      comparacion
    ),
    
    TRUE ~ paste(
      "Se mantuvo sin cambio respecto a",
      comparacion
    )
  )
}

# 7. Completar el calendario mensual
# Esto evita comparar meses incorrectos cuando falta algún periodo.
comercio_series <- comercio_limpio |>
  dplyr::select(
    .data$territorio,
    .data$fecha,
    .data$flujo,
    .data$monto_usd
  ) |>
  dplyr::group_by(
    .data$territorio,
    .data$flujo
  ) |>
  tidyr::complete(
    fecha = seq.Date(
      min(fecha),
      max(fecha),
      by = "month"
    )
  ) |>
  dplyr::arrange(
    .data$fecha,
    .by_group = TRUE
  )

# 8. Calcular cambios mensuales y anuales
indicadores_comercio <- comercio_series |>
  dplyr::mutate(
    anio = as.integer(format(.data$fecha, "%Y")),
    mes = as.integer(format(.data$fecha, "%m")),
    id_periodo = .data$anio * 100L + .data$mes,
    
    id_flujo = dplyr::if_else(
      .data$flujo == "Importaciones",
      1L,
      2L
    ),
    
    # Comparación mensual
    monto_mes_anterior = dplyr::lag(
      .data$monto_usd,
      n = 1
    ),
    
    cambio_mensual_usd =
      .data$monto_usd - .data$monto_mes_anterior,
    
    variacion_mensual = division_segura(
      .data$cambio_mensual_usd,
      .data$monto_mes_anterior
    ),
    
    variacion_mensual_pct =
      .data$variacion_mensual * 100,
    
    movimiento_mensual = clasificar_movimiento(
      .data$cambio_mensual_usd
    ),
    
    texto_mensual = crear_texto_movimiento(
      movimiento = .data$movimiento_mensual,
      variacion = .data$variacion_mensual,
      comparacion = "el mes anterior"
    ),
    
    # Comparación anual: mismo mes del año anterior
    monto_anio_anterior = dplyr::lag(
      .data$monto_usd,
      n = 12
    ),
    
    cambio_anual_usd =
      .data$monto_usd - .data$monto_anio_anterior,
    
    variacion_anual = division_segura(
      .data$cambio_anual_usd,
      .data$monto_anio_anterior
    ),
    
    variacion_anual_pct =
      .data$variacion_anual * 100,
    
    movimiento_anual = clasificar_movimiento(
      .data$cambio_anual_usd
    ),
    
    texto_anual = crear_texto_movimiento(
      movimiento = .data$movimiento_anual,
      variacion = .data$variacion_anual,
      comparacion = "el mismo mes del año anterior"
    )
  ) |>
  dplyr::ungroup() |>
  dplyr::arrange(
    .data$fecha,
    .data$territorio,
    .data$flujo
  )

# 9. Calcular comercio total y balance comercial
balance_comercial <- indicadores_comercio |>
  dplyr::select(
    .data$territorio,
    .data$fecha,
    .data$flujo,
    .data$monto_usd
  ) |>
  tidyr::pivot_wider(
    names_from = .data$flujo,
    values_from = .data$monto_usd
  ) |>
  dplyr::mutate(
    comercio_total_usd = dplyr::if_else(
      !is.na(.data$Exportaciones) &
        !is.na(.data$Importaciones),
      .data$Exportaciones + .data$Importaciones,
      NA_real_
    ),
    
    balance_comercial_usd = dplyr::if_else(
      !is.na(.data$Exportaciones) &
        !is.na(.data$Importaciones),
      .data$Exportaciones - .data$Importaciones,
      NA_real_
    ),
    
    condicion_balance = dplyr::case_when(
      is.na(.data$balance_comercial_usd) ~ "Sin dato",
      .data$balance_comercial_usd > 0 ~ "Superávit",
      .data$balance_comercial_usd < 0 ~ "Déficit",
      TRUE ~ "Equilibrio"
    )
  ) |>
  dplyr::arrange(
    .data$fecha,
    .data$territorio
  )

# 10. Calcular participación de Jalisco en México
participacion_jalisco <- indicadores_comercio |>
  dplyr::select(
    .data$fecha,
    .data$territorio,
    .data$flujo,
    .data$monto_usd
  ) |>
  tidyr::pivot_wider(
    names_from = .data$territorio,
    values_from = .data$monto_usd
  ) |>
  dplyr::mutate(
    participacion_jalisco = division_segura(
      .data[["Jalisco"]],
      .data[["México"]]
    ),
    
    participacion_jalisco_pct =
      .data$participacion_jalisco * 100
  ) |>
  dplyr::arrange(
    .data$fecha,
    .data$flujo
  )

# 11. Identificar el último periodo completo
periodos_completos <- indicadores_comercio |>
  dplyr::filter(!is.na(.data$monto_usd)) |>
  dplyr::distinct(
    .data$fecha,
    .data$territorio,
    .data$flujo
  ) |>
  dplyr::count(
    .data$fecha,
    name = "combinaciones"
  ) |>
  dplyr::filter(.data$combinaciones == 4)

if (nrow(periodos_completos) == 0) {
  stop("No existe un periodo completo para calcular indicadores.")
}

ultimo_periodo <- max(periodos_completos$fecha)

# 12. Resumen del último periodo
indicadores_actuales <- indicadores_comercio |>
  dplyr::filter(.data$fecha == ultimo_periodo) |>
  dplyr::left_join(
    participacion_jalisco |>
      dplyr::filter(.data$fecha == ultimo_periodo) |>
      dplyr::select(
        .data$fecha,
        .data$flujo,
        .data$participacion_jalisco,
        .data$participacion_jalisco_pct
      ),
    by = c("fecha", "flujo")
  )

balance_actual <- balance_comercial |>
  dplyr::filter(.data$fecha == ultimo_periodo)

# 13. Guardar resultados
ruta_indicadores <- here::here(
  "data",
  "processed",
  "indicadores_comercio.csv"
)

ruta_balance <- here::here(
  "data",
  "processed",
  "balance_comercial.csv"
)

ruta_participacion <- here::here(
  "data",
  "processed",
  "participacion_jalisco.csv"
)

ruta_actuales <- here::here(
  "data",
  "processed",
  "indicadores_actuales.csv"
)

readr::write_csv(
  indicadores_comercio,
  ruta_indicadores,
  na = ""
)

readr::write_csv(
  balance_comercial,
  ruta_balance,
  na = ""
)

readr::write_csv(
  participacion_jalisco,
  ruta_participacion,
  na = ""
)

readr::write_csv(
  indicadores_actuales,
  ruta_actuales,
  na = ""
)

# 14. Mostrar resultados
cat("\n========================================\n")
cat("INDICADORES CALCULADOS CORRECTAMENTE\n")
cat("========================================\n")

cat(
  "\nÚltimo periodo analizado:",
  format(ultimo_periodo, "%Y-%m"),
  "\n\n"
)

cat("Indicadores del último periodo:\n")

print(
  indicadores_actuales |>
    dplyr::select(
      .data$territorio,
      .data$flujo,
      .data$monto_usd,
      .data$cambio_mensual_usd,
      .data$variacion_mensual_pct,
      .data$movimiento_mensual,
      .data$cambio_anual_usd,
      .data$variacion_anual_pct,
      .data$movimiento_anual,
      .data$participacion_jalisco_pct
    )
)

cat("\nBalance comercial:\n")
print(balance_actual)

cat("\nArchivos guardados en data/processed.\n")