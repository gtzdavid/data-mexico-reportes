  # ============================================================
  # 01_descarga_datos.R
  # Descarga de comercio exterior de México y Jalisco
  # ============================================================
  
  # Verificar paquetes
  paquetes <- c("here", "httr2", "tibble", "readr")
  
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
  
  # Direcciones oficiales que se intentarán
  urls_api <- c(
    "https://www.economia.gob.mx/datamexico/api/data",
    "https://www.economia.gob.mx/datamexico/api/data.jsonrecords"
  )
  
  # Función para consultar DataMéxico
  consultar_datamexico <- function(parametros, territorio) {
    
    errores <- character()
    
    for (url in urls_api) {
      
      resultado <- tryCatch({
        
        solicitud <- httr2::request(url)
        
        solicitud <- do.call(
          httr2::req_url_query,
          c(
            list(.req = solicitud),
            parametros
          )
        )
        
        solicitud <- solicitud |>
          httr2::req_user_agent(
            "Proyecto académico DataMéxico Jalisco"
          ) |>
          httr2::req_timeout(seconds = 60) |>
          httr2::req_retry(max_tries = 3)
        
        respuesta <- httr2::req_perform(solicitud)
        
        httr2::resp_check_status(respuesta)
        
        contenido <- httr2::resp_body_json(
          respuesta,
          simplifyVector = TRUE
        )
        
        if (is.null(contenido$data)) {
          stop("La respuesta no contiene la sección 'data'.")
        }
        
        datos <- tibble::as_tibble(contenido$data)
        
        if (nrow(datos) == 0) {
          stop("La consulta no devolvió registros.")
        }
        
        columnas_obligatorias <- c(
          "Flow",
          "Trade Value"
        )
        
        columnas_faltantes <- setdiff(
          columnas_obligatorias,
          names(datos)
        )
        
        if (length(columnas_faltantes) > 0) {
          stop(
            paste(
              "Faltan columnas:",
              paste(columnas_faltantes, collapse = ", ")
            )
          )
        }
        
        if (!any(c("Month", "Month ID") %in% names(datos))) {
          stop("No se encontró la columna de mes.")
        }
        
        datos
        
      }, error = function(e) {
        
        errores <<- c(
          errores,
          paste(url, conditionMessage(e), sep = " -> ")
        )
        
        NULL
      })
      
      if (!is.null(resultado)) {
        cat("\nConsulta de", territorio, "completada.\n")
        return(resultado)
      }
    }
    
    stop(
      paste(
        "No fue posible descargar los datos de",
        territorio,
        paste(errores, collapse = "\n"),
        sep = "\n"
      )
    )
  }
  
  # ------------------------------------------------------------
  # Consulta nacional: México
  # ------------------------------------------------------------
  
  parametros_mexico <- list(
    Nation = "mex",
    `Product Level` = 2,
    cube = "economy_foreign_trade_nat",
    drilldowns = "Month,Flow",
    measures = "Trade Value",
    parents = "false",
    sparse = "false",
    locale = "es"
  )
  
  datos_mexico_raw <- consultar_datamexico(
    parametros = parametros_mexico,
    territorio = "México"
  )
  
  # Guardar inmediatamente la descarga de México
  ruta_mexico <- here::here(
    "data",
    "raw",
    "comercio_mexico_raw.csv"
  )
  
  readr::write_csv(
    datos_mexico_raw,
    ruta_mexico,
    na = ""
  )
  
  # ------------------------------------------------------------
  # Consulta estatal: Jalisco
  # State ID 14
  # ------------------------------------------------------------
  
  parametros_jalisco <- list(
    State = 14,
    `Product Level` = 2,
    cube = "economy_foreign_trade_ent",
    drilldowns = "Month,Flow",
    measures = "Trade Value",
    parents = "false",
    sparse = "false",
    locale = "es"
  )
  
  datos_jalisco_raw <- consultar_datamexico(
    parametros = parametros_jalisco,
    territorio = "Jalisco"
  )
  
  # Guardar inmediatamente la descarga de Jalisco
  ruta_jalisco <- here::here(
    "data",
    "raw",
    "comercio_jalisco_raw.csv"
  )
  
  readr::write_csv(
    datos_jalisco_raw,
    ruta_jalisco,
    na = ""
  )
  
  # ------------------------------------------------------------
  # Resumen de las descargas
  # ------------------------------------------------------------
  
  cat("\n========================================\n")
  cat("DESCARGA FINALIZADA\n")
  cat("========================================\n")
  
  cat("\nMéxico:\n")
  cat("- Filas:", nrow(datos_mexico_raw), "\n")
  cat("- Columnas:", ncol(datos_mexico_raw), "\n")
  cat("- Archivo:", ruta_mexico, "\n")
  
  cat("\nJalisco:\n")
  cat("- Filas:", nrow(datos_jalisco_raw), "\n")
  cat("- Columnas:", ncol(datos_jalisco_raw), "\n")
  cat("- Archivo:", ruta_jalisco, "\n")
  
  cat("\nColumnas de México:\n")
  print(names(datos_mexico_raw))
  
  cat("\nColumnas de Jalisco:\n")
  print(names(datos_jalisco_raw))
  
  cat("\nPrimeros registros de México:\n")
  print(head(datos_mexico_raw, 3))
  
  cat("\nPrimeros registros de Jalisco:\n")
  print(head(datos_jalisco_raw, 3))