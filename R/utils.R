
DATA_ROOT <- "./data"
DOWNNLOAD_CONFIG <- "./download_config"
FILTER="WD"
library(jsonlite)

fill_left <- function(v) {
  last <- NA
  for (i in seq_along(v)) {
    if (!is.na(v[i])) last <- v[i]
    else              v[i] <- last
  }
  v
}

sanitize <- function(ddf) {
  df <- as.data.frame((ddf))
  df[1, ] <- fill_left(df[1, ])

  df[2, ] <- paste0(df[1, ], "_", df[2, ])

  df[2, ] <- str_replace_all(df[2, ], "\\s+", "-")

  df <- df[-c(1, 22), ]

  colnames(df) <- unlist(df[1,])

  df <- df[-c(1), ]

  df <- df[ , !grepl(FILTER, names(df))]

  df <- na.omit(df)

  df
}

flipped_col_names <- c(
    "Postępowania wszczęte - bieżący okres",
    "Postępowania wszczęte - zmiana (różnica)",
    "Przestępstwa stwierdzone - bieżący okres",
    "Przestępstwa stwierdzone - zmiana (różnica)",
    "Przestępstwa stwierdzone z czynami nieletnich - bieżący okres",
    "Przestępstwa stwierdzone z czynami nieletnich - zmiana (różnica)",
    "Przestępstwa wykryte - bieżący okres",
    "Przestępstwa wykryte - zmiana (różnica)",
    "Procent wykrycia - bieżący okres",
    "Procent wykrycia - (różnica)",
    "Podejrzani ogółem - bieżący okres",
    "Podejrzani ogółem - zmiana (różnica)",
    "Podejrzani nieletni - bieżący okres",
    "Podejrzani nieletni - zmiana (różnica)",
    "date"
)

column_names <- c(
    # "KKP Warszawa", "KPP Warszawa Zach.", "KPP Grodzisk Maz." <- cols missing in some of dataframes
    "KPP Legionowo",
    "KPP Mińsk Maz.", "KPP Nowy Dwór Maz.", "KPP Otwock",
    "KPP Piaseczno", "KPP Pruszków",
    "KPP Wołomin", "KRP Warszawa I", "KRP Warszawa II",
    "KRP Warszawa III", "KRP Warszawa IV", "KRP Warszawa V",
    "KRP Warszawa VI", "KRP Warszawa VII", "Ogółem KSP"
)

plot_statistics <- function(x, y, region, column_name, data_tag) {
  if (!inherits(df$date, "Date")) df$date <- as.Date(df$date)

  tick_vals <- seq(min(df$date, na.rm = TRUE), max(df$date, na.rm = TRUE), by = "month")

  plotly::plot_ly(
    data = df,
    x = x,
    y = y,
    type = "scatter",
    mode = "lines",
    name = column_name
  ) %>%
    plotly::layout(
      title = list(text = paste(column_name, "==", region)),
      xaxis = list(
        title = "Rok i miesiąc",
        type = "date",
        tickmode = "array",
        tickvals = tick_vals,
        tickformat = "%Y-%m"
      ),
      yaxis = list(title = column_name)
    )
}

safe_filename <- function(x) {
  x <- gsub(" ", "-", x, fixed = TRUE)
  x <- gsub("/", "", x, fixed = TRUE)
  x <- gsub("%", "procent", x, fixed = TRUE)
  x <- gsub("[^\\p{L}\\p{N}._=-]+", "-", x, perl = TRUE)
  x
}

generate_widgets <- function(plot_specs, out_dir = "generated") {

  widgets <- list()
  manifest <- list()

  for (region in names(plot_specs)) {
    widgets <- list()
    manifest[[region]] <- list()
    counter <- 1
    for (data_tag in names(plot_specs[[region]])) {
      manifest[[region]][[data_tag]] <- list()
      for (column_name in names(plot_specs[[region]][[data_tag]])) {
        spec = plot_specs[[region]][[data_tag]][[column_name]]
        widget <- plot_statistics(x=as.Date(spec$x), y=spec$y, region, column_name, data_tag)
        parent_dir <- paste0("html/", out_dir, "/", region, "/", data_tag)
        dir.create(parent_dir, recursive = TRUE, showWarnings = FALSE)
        file_path <- paste0(parent_dir, "/", column_name, ".html")
        widgets[[length(widgets) + 1]] <- widget
        htmlwidgets::saveWidget(
          widget = widget,
          file = file_path,
          selfcontained = FALSE,
          libdir="libs"
        )
        manifest[[region]][[data_tag]][[column_name]] <- file_path
      }
    }
  }
  manifest_path <- paste0("html/", out_dir, "/manifest.json")
  write_json(
    manifest,
    manifest_path,
    pretty = TRUE,
    auto_unbox = TRUE
  )
  widgets
}

library(htmltools)

# ---- you implement this: spec -> htmlwidget (plotly, leaflet, etc.) ----
make_widget <- function(spec) {
  df <- spec$df
  region <- as.character(spec$region)
  column_name <- as.character(spec$column_name)
  data_tag <- if (!is.null(spec$data_tag)) as.character(spec$data_tag) else ""

  # Title similar to your previous code
  title <- paste(column_name, "==", region)

  # Create the htmlwidget (your existing function)
  w <- plot_statistics(df, region, column_name)

  # Make a stable-ish unique id (avoid spaces/special chars)
  # Using digest if available; fallback to a sanitized string
  wid <- paste0(
    "w_",
    gsub("[^A-Za-z0-9_]+", "_", paste(region, column_name, data_tag, sep = "__"))
  )

  # Wrap widget with metadata (still returns htmltools tags, which is fine)
  htmltools::div(
    id = wid,
    class = "plot-widget",
    `data-region` = region,
    `data-title`  = title,
    `data-tag`    = data_tag,
    w
  )
}


# Safer filenames for regions (works well for ids like "KRP Warszawa I")
region_to_filename <- function(region) {
  paste0(URLencode(region, reserved = TRUE), ".html")
}

# Main generator
write_region_pages <- function(plot_specs, html_dir = "html") {
  regions_dir <- file.path(html_dir, "regions")
  deps_dir    <- file.path(html_dir, "deps")

  dir.create(regions_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(deps_dir,    recursive = TRUE, showWarnings = FALSE)

  # ---- group specs by region ----
  regions <- split(plot_specs, vapply(plot_specs, `[[`, character(1), "region"))

  # ---- build widgets for each region (tagList of widgets) ----
  region_widgets <- lapply(regions, function(specs) {
    tagList(lapply(specs, make_widget))
  })

  # ---- compute dependencies ONCE, across all widgets ----
  all_widgets <- tagList(unname(region_widgets))
  deps <- htmlDependencies(all_widgets)

  # ---- copy deps ONCE into html/deps/ (shared) ----
  # copyDependencyToDir() returns a modified dependency object; we keep names, etc.
  deps_copied <- lapply(deps, copyDependencyToDir, deps_dir, mustWork = TRUE)

  # ---- write one region HTML file per region ----
  for (region in names(region_widgets)) {
    widgets <- region_widgets[[region]]

    # IMPORTANT: region pages live in html/regions/, so deps are at ../deps/
    deps_rel <- lapply(deps_copied, htmltools::makeDependencyRelative, basepath = regions_dir)
    deps_tags <- htmltools::renderDependencies(deps_rel)

    page <- tags$html(
      tags$head(
        tags$meta(charset = "utf-8"),
        tags$title(paste("Region:", region)),
        deps_tags,
        tags$style(HTML("
          body { font-family: sans-serif; margin: 12px; }
          .widget { margin: 10px 0; }
        "))
      ),
      tags$body(
        tags$h3(region),
        # wrap each widget (optional)
        lapply(as.list(widgets), function(w) div(class = "widget", w))
      )
    )

    out_file <- file.path(regions_dir, region_to_filename(region))
    writeLines(as.character(page), out_file, useBytes = TRUE)
  }

  invisible(list(
    regions_dir = regions_dir,
    deps_dir = deps_dir,
    regions = names(region_widgets)
  ))
}

