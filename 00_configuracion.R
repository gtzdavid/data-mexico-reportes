# ============================================================
# 00_configuracion.R
# Configuración inicial del proyecto DataMéxico
# ============================================================

# 1. Paquetes necesarios
paquetes <- c(
  "here",
  "httr2",
  "jsonlite",
  "dplyr",
  "tidyr",
  "purrr",
  "tibble",
  "readr",
  "stringr",
  "janitor",
  "lubridate",
  "glue",
  "scales",
  "knitr",
  "rmarkdown",
  "flextable",
  "officer"
)

# 2. Identificar paquetes faltantes
faltantes <- paquetes[
  !vapply(
    paquetes,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]

# 3. Instalar únicamente los paquetes faltantes
if (length(faltantes) > 0) {
  install.packages(faltantes, dependencies = TRUE)
}

# 4. Verificar que todos estén disponibles
no_disponibles <- paquetes[
  !vapply(
    paquetes,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]

if (length(no_disponibles) > 0) {
  stop(
    paste(
      "No fue posible instalar:",
      paste(no_disponibles, collapse = ", ")
    )
  )
}

# 5. Opciones generales
options(
  scipen = 999,
  dplyr.summarise.inform = FALSE
)

# 6. Crear estructura de carpetas
carpetas <- c(
  "R",
  file.path("data", "raw"),
  file.path("data", "processed"),
  "output"
)

for (carpeta in carpetas) {
  dir.create(
    file.path(here::here(), carpeta),
    recursive = TRUE,
    showWarnings = FALSE
  )
}

# 7. Mostrar resultado
cat("\nConfiguración completada correctamente.\n")
cat("Carpeta principal:", here::here(), "\n\n")
cat("Carpetas del proyecto:\n")
cat(paste0("- ", carpetas, collapse = "\n"))
cat("\n")