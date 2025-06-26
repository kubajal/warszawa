
library(readxl)
library(stringr)
library(futile.logger)

DATA_ROOT <- "./data"

csv <- read.csv("paths.csv", header = TRUE)

dir.create(DATA_ROOT, showWarnings = FALSE)
flog.threshold(DEBUG)

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
  df <- na.omit(df)
  df
}

data <- list()

column_names <- c(
    # "KKP Warszawa", "KPP Warszawa Zach.", "KPP Grodzisk Maz." <- cols missing in some of dataframes
    "KPP Legionowo",
    "KPP Mińsk Maz.", "KPP Nowy Dwór Maz.", "KPP Otwock",
    "KPP Piaseczno", "KPP Pruszków",
    "KPP Wołomin", "KRP Warszawa I", "KRP Warszawa II",
    "KRP Warszawa III", "KRP Warszawa IV", "KRP Warszawa V",
    "KRP Warszawa VI", "KRP Warszawa VII", "Ogółem KSP"
)
previous_year <- NA
previous_df <- NA

flipped_col_names <- c(
    "Postępowania-wszczęte_bieżący-okres",
    "Postępowania-wszczęte_zmiana-(różnica)",
    "Postępowania-wszczęte_WD",
    "Przestępstwa-stwierdzone_bieżący-okres",
    "Przestępstwa-stwierdzone_zmiana-(różnica)",
    "Przestępstwa-stwierdzone_WD",
    "Przestępstwa-stwierdzone-z-czynami-nieletnich_bieżący-okres",
    "Przestępstwa-stwierdzone-z-czynami-nieletnich_zmiana-(różnica)",
    "Przestępstwa-stwierdzone-z-czynami-nieletnich_WD",
    "Przestępstwa-wykryte_bieżący-okres",
    "Przestępstwa-wykryte_zmiana-(różnica)",
    "Przestępstwa-wykryte_WD",
    "%-wykrycia_bieżący-okres",
    "%-wykrycia_(różnica)",
    "Podejrzani-ogółem_bieżący-okres",
    "Podejrzani-ogółem_zmiana-(różnica)",
    "Podejrzani-ogółem_WD",
    "Podejrzani---nieletni_bieżący-okres",
    "Podejrzani---nieletni_zmiana-(różnica)",
    "Podejrzani---nieletni_WD",
    "date"
)

for (i in 1:nrow(csv)) {
    id <- csv[i, ]$id
    url <- csv[i, ]$url
    year <- as.character(csv[i, ]$year)
    month <- as.character(csv[i, ]$month)
    collapse <- as.logical(csv[i, ]$collapse)
    new_name <- csv[i, ]$new_name

    if (is.na(previous_year) || previous_year != year) {
        previous_year <- year
        previous_df <- NA
    }

    dir_path = paste0(DATA_ROOT, "/", year)
    dir.create(dir_path, showWarnings = FALSE)
    new_path <- paste0(DATA_ROOT, "/", year, "/", new_name)
    flog.debug(paste("Processing", new_name))
    if (!file.exists(new_path)) {
        flog.info(paste("Downloading", new_path))
        system(paste("wget", url, "-O", new_path))
    }
    else {
        flog.info(paste("   File already downloaded."))
    }
    current_df <- suppressMessages({
        read_excel(new_path) %>% sanitize %>% na.omit
    })
    flipped <- setNames(
        as.data.frame( t(current_df[-1]) ),
        current_df[[1]]
    )
    flipped[] <- lapply(flipped, function(x) as.numeric(x))

    flipped <- flipped[column_names]
    flipped <- flipped[ !grepl("Poprzedni", rownames(flipped), fixed = TRUE), ]

    if (collapse && (is.data.frame(previous_df))) {
        flog.debug(paste("  Collapsing with previous"))
        df <- t(flipped - previous_df) %>% as.data.frame
    }
    else {
        df <- t(flipped) %>% as.data.frame
    }
    df["date"] <- as.Date(sprintf("%04d-%02d-01", as.integer(year), as.integer(month)), "%Y-%m-%d")
    colnames(df) <- flipped_col_names
    df <- df[rownames(df) != "Ogółem KSP",]
    df["region"] <- rownames(df)
    data[[new_name]] <- df
    previous_df <- flipped
}

data$data_2018_5.xls <- NULL

plot_statistics <- function(df, region, column_name) {
  file_name <- paste(column_name, "==", region, ".png")
  file_name <- str_replace_all(file_name, " ", "-")
  file_name <- str_replace_all(file_name, "/", "")
  file_name <- str_replace_all(file_name, "%", "procent")
  file_name <- paste0("./img/", file_name)
  flog.debug(paste("   PNG file name:", file_name))
  png(file_name)
  plot(df$date, df[[column_name]], type = "l", xaxt = "n",
      xlab = "Year-Month", ylab = column_name)      # line
  axis.Date(1,
            at = seq(min(df$date), max(df$date), by = "months"),
            format = "%Y-%m")
  dev.off()
}

x <- Reduce(rbind, data)

for (region in unique(x$region)) {
    current_df <- x[x$region == region,]
    for (col_name in names(current_df)) {
        plot_statistics(current_df, region, col_name)
    }
}
