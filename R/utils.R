
DATA_ROOT <- "./data"
DOWNNLOAD_CONFIG <- "./download_config"
FILTER="WD"


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
    "Postępowania-wszczęte_bieżący-okres",
    "Postępowania-wszczęte_zmiana-(różnica)",
    "Przestępstwa-stwierdzone_bieżący-okres",
    "Przestępstwa-stwierdzone_zmiana-(różnica)",
    "Przestępstwa-stwierdzone-z-czynami-nieletnich_bieżący-okres",
    "Przestępstwa-stwierdzone-z-czynami-nieletnich_zmiana-(różnica)",
    "Przestępstwa-wykryte_bieżący-okres",
    "Przestępstwa-wykryte_zmiana-(różnica)",
    "%-wykrycia_bieżący-okres",
    "%-wykrycia_(różnica)",
    "Podejrzani-ogółem_bieżący-okres",
    "Podejrzani-ogółem_zmiana-(różnica)",
    "Podejrzani---nieletni_bieżący-okres",
    "Podejrzani---nieletni_zmiana-(różnica)",
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

plot_statistics <- function(df, region, column_name, data_tag) {
  stopifnot(all(c("date", column_name) %in% names(df)))
  if (!inherits(df$date, "Date")) df$date <- as.Date(df$date)

  tick_vals <- seq(min(df$date, na.rm = TRUE), max(df$date, na.rm = TRUE), by = "month")

  plotly::plot_ly(
    data = df,
    x = ~date,
    y = df[[column_name]],
    type = "scatter",
    mode = "lines",
    name = column_name
  ) %>%
    plotly::layout(
      title = list(text = paste(column_name, "==", region)),
      xaxis = list(
        title = "Year-Month",
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
  x <- gsub("[^A-Za-z0-9._=-]+", "-", x)   # extra safety
  x
}

save_plot_pages <- function(plot_specs, out_dir = "site", pages_dir = "plots") {
  # plot_specs: list of lists with df, region, column_name
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  plots_path <- file.path(out_dir, pages_dir)
  dir.create(plots_path, showWarnings = FALSE, recursive = TRUE)

  links <- character(0)

  for (spec in plot_specs) {
    df <- spec$df
    region <- spec$region
    column_name <- spec$column_name

    title <- paste(column_name, "==", region)
    fname <- safe_filename(paste0(title, ".html"))
    rel_path <- file.path(pages_dir, fname)
    full_path <- file.path(out_dir, rel_path)

    p <- plot_statistics(df, region, column_name)

    # Use selfcontained=FALSE for speed and reliability
    htmlwidgets::saveWidget(p, full_path, selfcontained = FALSE)

    links <- c(links, sprintf('<li><a href="%s">%s</a></li>', rel_path, title))
  }

  index_html <- paste0(
    "<!doctype html><html><head><meta charset='utf-8'>",
    "<meta name='viewport' content='width=device-width, initial-scale=1'>",
    "<title>Plots</title>",
    "<style>body{font-family:sans-serif;max-width:900px;margin:40px auto;padding:0 16px}",
    "li{margin:10px 0}</style>",
    "</head><body>",
    "<h1>Plots</h1>",
    "<ul>", paste(links, collapse = "\n"), "</ul>",
    "</body></html>"
  )

  writeLines(index_html, file.path(out_dir, "index.html"))
  invisible(file.path(out_dir, "index.html"))
}
