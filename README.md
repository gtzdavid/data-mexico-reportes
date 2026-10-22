# Comercio exterior de Jalisco (DataMéxico)

Pipeline en R que descarga datos de la API de DataMéxico, calcula indicadores y genera un reporte en Word. GitHub Actions lo ejecuta cada mes y publica una página con el reporte descargable y un dashboard.

## Cómo funciona

```
01_descarga_datos.R → 02_limpieza_datos.R → 03_indicadores.R → 04_tablas_textos.R
                                                                      │
                                         R/generar_reporte.R  ◄───────┤  (Word → reportes/)
                                         sitio/ (Quarto)      ◄───────┘  (dashboard + descargas)
```

- `reportes/` guarda un `.docx` por periodo (`reporte_datamexico_jalisco_2026-06.docx`). La página muestra el más reciente y el historial.
- Si la API falla, el workflow usa los datos de la última descarga exitosa (quedan guardados en `data/raw`).
- El reporte de Word se genera desde `reporte_datamexico_jalisco.Rmd` (versión breve) y es lo único descargable. La ficha (`ficha_datamexico_jalisco.Rmd`) no se ejecuta ni se publica.

## Puesta en marcha (una sola vez)

1. Crea un repositorio en GitHub (público, para usar Pages gratis) y sube esta carpeta:
   ```bash
   git init
   git add .
   git commit -m "Proyecto inicial"
   git branch -M main
   git remote add origin https://github.com/TU_USUARIO/TU_REPO.git
   git push -u origin main
   ```
2. En GitHub: **Settings → Pages → Build and deployment → Source: GitHub Actions**.
3. En **Settings → Actions → General → Workflow permissions**, elige **Read and write permissions**.
4. Ve a **Actions → Actualizar reporte y publicar sitio → Run workflow**.
5. Al terminar (la primera vez tarda ~10 min por instalar paquetes), tu página queda en `https://TU_USUARIO.github.io/TU_REPO/`.

Después se actualiza sola el día 5 de cada mes, cuando cambias código o cuando pulsas *Run workflow*.

## Vista previa local

```r
source("R/generar_reporte.R")   # genera reportes/*.docx
```
```bash
quarto preview sitio            # requiere Quarto instalado
```
Los enlaces de descarga solo funcionan en el sitio publicado (el workflow copia los `.docx` a `descargas/`).

## Pendientes conocidos

- `R05_ranking_entidades.R` está vacío en el proyecto. El dashboard usa `data/processed/ranking_entidades.csv` (año 2025) tal como está, sin actualizarlo.
- Para agregar paquetes nuevos, añádelos a `DESCRIPTION` (sección `Imports`).
