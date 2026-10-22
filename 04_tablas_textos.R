# ============================================================
# 04_tablas_textos.R
# Tablas y textos dinámicos para el reporte
# ============================================================

# 1. Verificar paquetes
paquetes <- c(
  "here",
  "readr",
  "dplyr",
  "tidyr",
  "tibble",
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

# 2. Rutas de los indicadores
ruta_indicadores <- here::here(
  "data", "processed", "indicadores_comercio.csv"
)

ruta_balance <- here::here(
  "data", "processed", "balance_comercial.csv"
)

ruta_participacion <- here::here(
  "data", "processed", "participacion_jalisco.csv"
)

archivos_necesarios <- c(
  ruta_indicadores,
  ruta_balance,
  ruta_participacion
)

if (!all(file.exists(archivos_necesarios))) {
  stop(
    paste(
      "Faltan archivos de indicadores.",
      "Ejecuta primero R/03_indicadores.R"
    )
  )
}

# 3. Leer información
indicadores <- readr::read_csv(
  ruta_indicadores,
  show_col_types = FALSE
) |>
  dplyr::mutate(fecha = as.Date(.data$fecha))

balances <- readr::read_csv(
  ruta_balance,
  show_col_types = FALSE
) |>
  dplyr::mutate(fecha = as.Date(.data$fecha))

participaciones <- readr::read_csv(
  ruta_participacion,
  show_col_types = FALSE
) |>
  dplyr::mutate(fecha = as.Date(.data$fecha))

# 4. Identificar último periodo completo
periodos_completos <- indicadores |>
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
  stop("No existe un periodo completo.")
}

ultimo_periodo <- max(periodos_completos$fecha)

# 5. Funciones de formato
meses_es <- c(
  "enero", "febrero", "marzo", "abril",
  "mayo", "junio", "julio", "agosto",
  "septiembre", "octubre", "noviembre",
  "diciembre"
)

nombre_periodo <- function(fecha) {
  fecha <- as.Date(fecha)
  
  paste(
    meses_es[as.integer(format(fecha, "%m"))],
    "de",
    format(fecha, "%Y")
  )
}

formato_monto <- function(monto) {
  ifelse(
    is.na(monto),
    "N/D",
    paste0(
      "US$",
      scales::number(
        monto / 1000000,
        accuracy = 0.1,
        big.mark = ",",
        decimal.mark = "."
      ),
      " M"
    )
  )
}

formato_porcentaje <- function(valor) {
  ifelse(
    is.na(valor),
    "N/D",
    scales::percent(
      valor,
      accuracy = 0.1,
      decimal.mark = "."
    )
  )
}

division_segura <- function(numerador, denominador) {
  if (
    length(denominador) == 0 ||
    is.na(denominador) ||
    denominador == 0
  ) {
    return(NA_real_)
  }
  
  numerador / denominador
}

# 6. Funciones para extraer valores
obtener_indicador <- function(territorio, flujo) {
  
  resultado <- indicadores |>
    dplyr::filter(
      .data$fecha == ultimo_periodo,
      .data$territorio == .env$territorio,
      .data$flujo == .env$flujo
    )
  
  if (nrow(resultado) != 1) {
    stop(
      paste(
        "No se encontró un registro único para",
        territorio,
        flujo
      )
    )
  }
  
  resultado
}

obtener_balance <- function(territorio) {
  
  resultado <- balances |>
    dplyr::filter(
      .data$fecha == ultimo_periodo,
      .data$territorio == .env$territorio
    )
  
  if (nrow(resultado) != 1) {
    stop(
      paste(
        "No se encontró un balance único para",
        territorio
      )
    )
  }
  
  resultado
}

# 7. Crear descripción de una variación
describir_variacion <- function(
    movimiento,
    variacion,
    referencia
) {
  
  if (is.na(variacion)) {
    return(
      paste(
        "no cuenta con información suficiente para compararse con",
        referencia
      )
    )
  }
  
  if (movimiento == "Se mantuvo sin cambio") {
    return(
      paste(
        "se mantuvo sin cambio respecto a",
        referencia
      )
    )
  }
  
  paste(
    tolower(movimiento),
    formato_porcentaje(abs(variacion)),
    "respecto a",
    referencia
  )
}

# 8. Crear párrafos dinámicos para cada flujo
crear_texto_flujo <- function(territorio, flujo) {
  
  fila <- obtener_indicador(territorio, flujo)
  
  descripcion_mensual <- describir_variacion(
    movimiento = fila$movimiento_mensual[[1]],
    variacion = fila$variacion_mensual[[1]],
    referencia = "el mes anterior"
  )
  
  descripcion_anual <- describir_variacion(
    movimiento = fila$movimiento_anual[[1]],
    variacion = fila$variacion_anual[[1]],
    referencia = "el mismo mes del año anterior"
  )
  
  paste0(
    "En ",
    nombre_periodo(ultimo_periodo),
    ", las ",
    tolower(flujo),
    " de ",
    territorio,
    " alcanzaron ",
    formato_monto(fila$monto_usd[[1]]),
    ". El monto ",
    descripcion_mensual,
    ". En la comparación anual, el monto ",
    descripcion_anual,
    "."
  )
}

# 9. Crear texto dinámico del balance
crear_texto_balance <- function(territorio) {
  
  fila <- obtener_balance(territorio)
  
  balance <- fila$balance_comercial_usd[[1]]
  condicion <- fila$condicion_balance[[1]]
  
  if (is.na(balance)) {
    return(
      paste(
        "No fue posible calcular el balance comercial de",
        territorio,
        "para",
        nombre_periodo(ultimo_periodo)
      )
    )
  }
  
  if (condicion == "Equilibrio") {
    return(
      paste(
        "En",
        nombre_periodo(ultimo_periodo),
        territorio,
        "presentó equilibrio comercial."
      )
    )
  }
  
  paste0(
    "En ",
    nombre_periodo(ultimo_periodo),
    ", ",
    territorio,
    " presentó un ",
    tolower(condicion),
    " comercial de ",
    formato_monto(abs(balance)),
    "."
  )
}

# 10. Crear texto de participación de Jalisco
crear_texto_participacion <- function(flujo) {
  
  fila <- participaciones |>
    dplyr::filter(
      .data$fecha == ultimo_periodo,
      .data$flujo == .env$flujo
    )
  
  if (
    nrow(fila) == 0 ||
    is.na(fila$participacion_jalisco[[1]])
  ) {
    return(
      paste(
        "No fue posible calcular la participación de Jalisco",
        "en las",
        tolower(flujo),
        "nacionales."
      )
    )
  }
  
  paste0(
    "Durante ese mismo mes, Jalisco representó ",
    formato_porcentaje(
      fila$participacion_jalisco[[1]]
    ),
    " de las ",
    tolower(flujo),
    " nacionales."
  )
}

# 11. Textos dinámicos finales
textos_dinamicos <- tibble::tibble(
  seccion = c(
    "mexico_exportaciones",
    "mexico_importaciones",
    "jalisco_exportaciones",
    "jalisco_importaciones",
    "participacion_exportaciones",
    "participacion_importaciones",
    "balance_mexico",
    "balance_jalisco"
  ),
  
  texto = c(
    crear_texto_flujo(
      "México",
      "Exportaciones"
    ),
    
    crear_texto_flujo(
      "México",
      "Importaciones"
    ),
    
    crear_texto_flujo(
      "Jalisco",
      "Exportaciones"
    ),
    
    crear_texto_flujo(
      "Jalisco",
      "Importaciones"
    ),
    
    crear_texto_participacion(
      "Exportaciones"
    ),
    
    crear_texto_participacion(
      "Importaciones"
    ),
    
    crear_texto_balance(
      "México"
    ),
    
    crear_texto_balance(
      "Jalisco"
    )
  )
)

# ============================================================
# TABLA 1: COMPARACIÓN MÉXICO Y JALISCO
# ============================================================

mexico_exportaciones <- obtener_indicador(
  "México",
  "Exportaciones"
)$monto_usd[[1]]

mexico_importaciones <- obtener_indicador(
  "México",
  "Importaciones"
)$monto_usd[[1]]

jalisco_exportaciones <- obtener_indicador(
  "Jalisco",
  "Exportaciones"
)$monto_usd[[1]]

jalisco_importaciones <- obtener_indicador(
  "Jalisco",
  "Importaciones"
)$monto_usd[[1]]

balance_mexico <- obtener_balance("México")
balance_jalisco <- obtener_balance("Jalisco")

tabla_comparacion <- tibble::tibble(
  indicador = c(
    "Exportaciones",
    "Importaciones",
    "Comercio total",
    "Balance comercial"
  ),
  
  mexico_millones_usd = round(
    c(
      mexico_exportaciones,
      mexico_importaciones,
      balance_mexico$comercio_total_usd[[1]],
      balance_mexico$balance_comercial_usd[[1]]
    ) / 1000000,
    1
  ),
  
  jalisco_millones_usd = round(
    c(
      jalisco_exportaciones,
      jalisco_importaciones,
      balance_jalisco$comercio_total_usd[[1]],
      balance_jalisco$balance_comercial_usd[[1]]
    ) / 1000000,
    1
  ),
  
  participacion_jalisco_pct = round(
    c(
      division_segura(
        jalisco_exportaciones,
        mexico_exportaciones
      ),
      
      division_segura(
        jalisco_importaciones,
        mexico_importaciones
      ),
      
      division_segura(
        balance_jalisco$comercio_total_usd[[1]],
        balance_mexico$comercio_total_usd[[1]]
      ),
      
      NA_real_
    ) * 100,
    1
  )
)

# ============================================================
# TABLA 2: VARIACIONES
# ============================================================

tabla_variaciones <- indicadores |>
  dplyr::filter(.data$fecha == ultimo_periodo) |>
  dplyr::transmute(
    territorio = .data$territorio,
    flujo = .data$flujo,
    
    monto_millones_usd = round(
      .data$monto_usd / 1000000,
      1
    ),
    
    cambio_mensual_millones_usd = round(
      .data$cambio_mensual_usd / 1000000,
      1
    ),
    
    variacion_mensual_pct = round(
      .data$variacion_mensual_pct,
      1
    ),
    
    movimiento_mensual = .data$movimiento_mensual,
    
    cambio_anual_millones_usd = round(
      .data$cambio_anual_usd / 1000000,
      1
    ),
    
    variacion_anual_pct = round(
      .data$variacion_anual_pct,
      1
    ),
    
    movimiento_anual = .data$movimiento_anual
  ) |>
  dplyr::arrange(
    .data$territorio,
    .data$flujo
  )

# ============================================================
# TABLA 3: ÚLTIMOS 12 MESES
# ============================================================

fechas_recientes <- indicadores |>
  dplyr::filter(
    .data$fecha <= ultimo_periodo,
    !is.na(.data$monto_usd)
  ) |>
  dplyr::distinct(.data$fecha) |>
  dplyr::arrange(dplyr::desc(.data$fecha)) |>
  dplyr::slice_head(n = 12) |>
  dplyr::pull(.data$fecha)

tabla_ultimos_meses <- indicadores |>
  dplyr::filter(.data$fecha %in% fechas_recientes) |>
  dplyr::mutate(
    serie = paste(
      .data$territorio,
      .data$flujo,
      sep = " - "
    ),
    
    monto_millones_usd = round(
      .data$monto_usd / 1000000,
      1
    )
  ) |>
  dplyr::select(
    .data$fecha,
    .data$serie,
    .data$monto_millones_usd
  ) |>
  tidyr::pivot_wider(
    names_from = .data$serie,
    values_from = .data$monto_millones_usd
  ) |>
  dplyr::arrange(.data$fecha) |>
  dplyr::mutate(
    periodo = nombre_periodo(.data$fecha),
    .before = 1
  ) |>
  dplyr::select(-.data$fecha)

# ============================================================
# TABLA 4: ÚLTIMOS CINCO AÑOS COMPLETOS
# ============================================================

base_anual <- indicadores |>
  dplyr::filter(!is.na(.data$monto_usd)) |>
  dplyr::mutate(
    anio = as.integer(format(.data$fecha, "%Y"))
  ) |>
  dplyr::group_by(
    .data$anio,
    .data$territorio,
    .data$flujo
  ) |>
  dplyr::summarise(
    meses_disponibles = dplyr::n_distinct(.data$fecha),
    monto_anual_usd = sum(.data$monto_usd),
    .groups = "drop"
  )

anios_completos <- base_anual |>
  dplyr::group_by(.data$anio) |>
  dplyr::summarise(
    combinaciones = dplyr::n(),
    minimo_meses = min(.data$meses_disponibles),
    .groups = "drop"
  ) |>
  dplyr::filter(
    .data$combinaciones == 4,
    .data$minimo_meses == 12
  ) |>
  dplyr::arrange(dplyr::desc(.data$anio)) |>
  dplyr::slice_head(n = 5) |>
  dplyr::pull(.data$anio)

tabla_anual <- base_anual |>
  dplyr::filter(.data$anio %in% anios_completos) |>
  dplyr::mutate(
    serie = paste(
      .data$territorio,
      .data$flujo,
      sep = " - "
    ),
    
    monto_anual_millones_usd = round(
      .data$monto_anual_usd / 1000000,
      1
    )
  ) |>
  dplyr::select(
    .data$anio,
    .data$serie,
    .data$monto_anual_millones_usd
  ) |>
  tidyr::pivot_wider(
    names_from = .data$serie,
    values_from = .data$monto_anual_millones_usd
  ) |>
  dplyr::arrange(.data$anio)

# 12. Guardar tablas y textos
readr::write_csv(
  tabla_comparacion,
  here::here(
    "data",
    "processed",
    "tabla_comparacion.csv"
  ),
  na = ""
)

readr::write_csv(
  tabla_variaciones,
  here::here(
    "data",
    "processed",
    "tabla_variaciones.csv"
  ),
  na = ""
)

readr::write_csv(
  tabla_ultimos_meses,
  here::here(
    "data",
    "processed",
    "tabla_ultimos_meses.csv"
  ),
  na = ""
)

readr::write_csv(
  tabla_anual,
  here::here(
    "data",
    "processed",
    "tabla_anual.csv"
  ),
  na = ""
)

readr::write_csv(
  textos_dinamicos,
  here::here(
    "data",
    "processed",
    "textos_dinamicos.csv"
  ),
  na = ""
)

# 13. Mostrar resultados
cat("\n========================================\n")
cat("TABLAS Y TEXTOS CREADOS CORRECTAMENTE\n")
cat("========================================\n")

cat(
  "\nPeriodo del reporte:",
  nombre_periodo(ultimo_periodo),
  "\n\n"
)

cat("Tabla comparativa:\n")
print(tabla_comparacion)

cat("\nTabla de variaciones:\n")
print(tabla_variaciones)

cat("\nTextos dinámicos:\n\n")
cat(textos_dinamicos$texto, sep = "\n\n")

cat("\n\nArchivos guardados en data/processed.\n")