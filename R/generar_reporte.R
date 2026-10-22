# ============================================================
# R/generar_reporte.R
# Renderiza el reporte breve en Word y la guarda en reportes/ con
# el periodo de los datos en el nombre, para conservar el historial:
#   reportes/reporte_datamexico_jalisco_2026-06.docx
# Ejecutar DESPUÉS de 01 → 04.
# ============================================================

actuales <- readr::read_csv(
  here::here("data", "processed", "indicadores_actuales.csv"),
  show_col_types = FALSE,
  progress = FALSE
)

periodo <- format(max(as.Date(actuales$fecha)), "%Y-%m")

dir.create(here::here("reportes"), showWarnings = FALSE)

archivo <- sprintf("reporte_datamexico_jalisco_%s.docx", periodo)

rmarkdown::render(
  input         = here::here("reporte_datamexico_jalisco.Rmd"),
  output_format = "word_document",
  output_file   = archivo,
  output_dir    = here::here("reportes"),
  envir         = new.env(),
  quiet         = TRUE
)

cat("\nReporte generado:", file.path("reportes", archivo), "\n")
