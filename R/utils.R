
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
      yaxis = list(title = data_tag)
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

  flog.debug("Started creating HTML files")

  for (region in names(plot_specs)) {
    flog.debug(paste0("  Processing region: ", region))
    widgets <- list()
    manifest[[region]] <- list()
    for (data_tag in names(plot_specs[[region]])) {
      manifest[[region]][[data_tag]] <- list()
      for (column_name in names(plot_specs[[region]][[data_tag]])) {
        flog.debug(paste0("    Processing column: ", column_name))
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
  flog.debug(paste0("Finished generating HTML files. Number of new files: ", length(widgets)))
  widgets
}
