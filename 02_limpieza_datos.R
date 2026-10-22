# ============================================================
# 02_limpieza_datos.R
# Limpieza y homologación de México y Jalisco
# ============================================================

# 1. Verificar paquetes
paquetes <- c(
  "here",
  "readr",
  "dplyr",
  "stringr",
  "janitor",
  "tibble"
)

faltantes <- paquetes[
  !vapply(paquetes, requireNamespace, logical(1), quietly = TRUE)
]

if (length(faltantes) > 0) {
  stop(
    paste(
      "Faltan paquetes:",
      paste(faltantes, collapse = ", "),
      "\nEjecuta primero 00_configuracion.R"
    )
  )
}

# 2. Rutas de los archivos originales
ruta_mexico <- here::here(
  "data",
  "raw",
  "comercio_mexico_raw.csv"
)

ruta_jalisco <- here::here(
  "data",
  "raw",
  "comercio_jalisco_raw.csv"
)

if (!file.exists(ruta_mexico)) {
  stop("No se encontró comercio_mexico_raw.csv")
}

if (!file.exists(ruta_jalisco)) {
  stop("No se encontró comercio_jalisco_raw.csv")
}

# 3. Función de limpieza
limpiar_comercio <- function(ruta_archivo, nombre_territorio) {
  
  datos <- readr::read_csv(
    ruta_archivo,
    show_col_types = FALSE,
    progress = FALSE
  ) |>
    janitor::clean_names()
  
  columnas_necesarias <- c(
    "month_id",
    "month",
    "flow_id",
    "flow",
    "trade_value"
  )
  
  columnas_faltantes <- setdiff(
    columnas_necesarias,
    names(datos)
  )
  
  if (length(columnas_faltantes) > 0) {
    stop(
      paste(
        "En la base de",
        nombre_territorio,
        "faltan las columnas:",
        paste(columnas_faltantes, collapse = ", ")
      )
    )
  }
  
  datos_limpios <- datos |>
    dplyr::transmute(
      territorio = nombre_territorio,
      
      id_periodo = as.integer(.data$month_id),
      
      fecha = as.Date(
        paste0(as.character(.data$month), "-01")
      ),
      
      anio = as.integer(format(.data$fecha, "%Y")),
      
      mes = as.integer(format(.data$fecha, "%m")),
      
      id_flujo = as.integer(.data$flow_id),
      
      flujo = dplyr::case_when(
        .data$id_flujo == 1 ~ "Importaciones",
        .data$id_flujo == 2 ~ "Exportaciones",
        stringr::str_detect(
          stringr::str_to_lower(.data$flow),
          "import"
        ) ~ "Importaciones",
        stringr::str_detect(
          stringr::str_to_lower(.data$flow),
          "export"
        ) ~ "Exportaciones",
        TRUE ~ NA_character_
      ),
      
      monto_usd = as.numeric(.data$trade_value)
    )
  
  # Validaciones
  if (any(is.na(datos_limpios$fecha))) {
    stop(
      paste(
        "Existen fechas que no pudieron convertirse en",
        nombre_territorio
      )
    )
  }
  
  if (any(is.na(datos_limpios$flujo))) {
    stop(
      paste(
        "Existen flujos no identificados en",
        nombre_territorio
      )
    )
  }
  
  if (any(is.na(datos_limpios$monto_usd))) {
    stop(
      paste(
        "Existen montos faltantes en",
        nombre_territorio
      )
    )
  }
  
  if (any(datos_limpios$monto_usd < 0)) {
    stop(
      paste(
        "Se encontraron montos negativos en",
        nombre_territorio
      )
    )
  }
  
  # Detectar duplicados
  duplicados <- datos_limpios |>
    dplyr::count(
      .data$territorio,
      .data$fecha,
      .data$flujo,
      name = "cantidad"
    ) |>
    dplyr::filter(.data$cantidad > 1)
  
  if (nrow(duplicados) > 0) {
    print(duplicados)
    
    stop(
      paste(
        "Se encontraron registros duplicados en",
        nombre_territorio
      )
    )
  }
  
  datos_limpios
}

# 4. Limpiar cada base
mexico_limpio <- limpiar_comercio(
  ruta_archivo = ruta_mexico,
  nombre_territorio = "México"
)

jalisco_limpio <- limpiar_comercio(
  ruta_archivo = ruta_jalisco,
  nombre_territorio = "Jalisco"
)

# 5. Unir las dos bases
comercio_limpio <- dplyr::bind_rows(
  mexico_limpio,
  jalisco_limpio
) |>
  dplyr::arrange(
    .data$fecha,
    .data$territorio,
    .data$flujo
  )

# 6. Verificar que cada mes tenga cuatro combinaciones:
# México/Jalisco × Exportaciones/Importaciones
revision_periodos <- comercio_limpio |>
  dplyr::distinct(
    .data$fecha,
    .data$territorio,
    .data$flujo
  ) |>
  dplyr::count(
    .data$fecha,
    name = "combinaciones"
  )

periodos_incompletos <- revision_periodos |>
  dplyr::filter(.data$combinaciones != 4)

periodos_completos <- revision_periodos |>
  dplyr::filter(.data$combinaciones == 4)

if (nrow(periodos_completos) == 0) {
  stop(
    paste(
      "No existe ningún mes completo con datos",
      "de México y Jalisco."
    )
  )
}

ultimo_periodo_comun <- max(periodos_completos$fecha)

# 7. Resumen de calidad
resumen_calidad <- comercio_limpio |>
  dplyr::group_by(.data$territorio) |>
  dplyr::summarise(
    filas = dplyr::n(),
    meses = dplyr::n_distinct(.data$fecha),
    fecha_inicial = min(.data$fecha),
    fecha_final = max(.data$fecha),
    montos_faltantes = sum(is.na(.data$monto_usd)),
    monto_minimo = min(.data$monto_usd),
    monto_maximo = max(.data$monto_usd),
    .groups = "drop"
  )

# 8. Guardar datos procesados
ruta_limpia <- here::here(
  "data",
  "processed",
  "comercio_limpio.csv"
)

ruta_calidad <- here::here(
  "data",
  "processed",
  "resumen_calidad.csv"
)

ruta_incompletos <- here::here(
  "data",
  "processed",
  "periodos_incompletos.csv"
)

readr::write_csv(
  comercio_limpio,
  ruta_limpia,
  na = ""
)

readr::write_csv(
  resumen_calidad,
  ruta_calidad,
  na = ""
)

readr::write_csv(
  periodos_incompletos,
  ruta_incompletos,
  na = ""
)

# 9. Resultados
cat("\n========================================\n")
cat("LIMPIEZA FINALIZADA CORRECTAMENTE\n")
cat("========================================\n")

cat(
  "\nÚltimo periodo común:",
  format(ultimo_periodo_comun, "%Y-%m"),
  "\n"
)

cat(
  "Periodos incompletos:",
  nrow(periodos_incompletos),
  "\n\n"
)

print(resumen_calidad)

cat("\nPrimeros registros limpios:\n")
print(head(comercio_limpio, 8))

cat("\nArchivo principal guardado en:\n")
cat(ruta_limpia, "\n")