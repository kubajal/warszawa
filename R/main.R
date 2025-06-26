#!/bin/R


library(readxl)
library(stringr)
library(magrittr)

# Set your directory
path <- "./data"

# List all xlsx files
files <- list.files(path, pattern = "xlsx|xls", full.names=TRUE)

# Load all files into a list of data frames
data_list <- lapply(files, read_excel)

# (optional) assign filenames as names in the list
names(data_list) <- basename(files)

# Create a function to summarize schema
get_schema <- function(df) {
  paste(names(df), sapply(df, class), sep = ":", collapse = "|")
}

# Identify schemas
schemas <- data.frame(name = files)
schemas$schema <- sapply(data_list, get_schema)
counts <- aggregate(name ~ schema, schemas, c)
counts$N <- lengths(counts$name)

frequent_schema_files <- counts[5,]$name
column_names <- frequent_schema_files[[1]] %>% str_remove(., "./data/")
has_month_range <- function(x) {
  pl_months <- c(
    "styczen", "luty", "marzec", "kwiecien", "maj", "czerwiec",
    "lipiec", "sierpien", "wrzesien", "pazdziernik", "listopad", "grudzien"
  )
  month_pat  <- paste(pl_months, collapse = "|")         # "styczen|luty|…|grudzien"
  range_pat  <- paste0("(", month_pat, ")-(", month_pat, ")")
  grepl(range_pat, tolower(x))
}
column_names <- column_names[!has_month_range(column_names)]

df_subset <- data_list[column_names]

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

y <- df_subset %>% lapply(sanitize) %>% na.omit

parse_month_year <- function(fnames) {
  fnames <- as.character(fnames)                       # accept factors, etc.
  ## ------------- local look-ups & regex parts ------------------------
  pl_months <- c(
    "styczen","luty","marzec","kwiecien","maj","czerwiec",
    "lipiec","sierpien","wrzesien","pazdziernik","listopad","grudzien"
  )
  month_pat <- paste(pl_months, collapse = "|")        # styczen|luty|…|grudzien
  # monthYYYY[r]          e.g. kwiecien2022  or  kwiecien2022r
  pat_single <- paste0("(", month_pat, ")(\\d{4})r?")
  # month-month (for early rejection)  e.g. styczen-czerwiec
  pat_two <- paste0("(", month_pat, ")-+(", month_pat, ")")
  ## -------------------------------------------------------------------
  parse_one <- function(x) {
    low <- tolower(basename(x))
    ## ---- skip if it’s a range file ----------------------------------
    if (grepl(pat_two, low))
      return(list(file = x,
                  month_pl  = NA_character_,
                  month_num = NA_integer_,
                  year      = NA_integer_))
    ## ---- try to grab one month + year -------------------------------
    caps <- regmatches(low, regexec(pat_single, low))[[1]]
    if (length(caps) == 0)          # nothing matched
      return(list(file = x,
                  month_pl  = NA_character_,
                  month_num = NA_integer_,
                  year      = NA_integer_))
    month_word <- caps[2]
    year_num   <- as.integer(caps[3])
    list(file = x,
         month_pl  = month_word,
         month_num = match(month_word, pl_months),
         year      = year_num)
  }
  do.call(rbind, lapply(fnames, parse_one))
}

parse_month_year(column_names)

df_augmented <- Map(function(d, fname) {
                      d$file <- fname        # new column with the file name
                      d                    },
                    y,
                    names(df_subset))          # second argument = file names

column_names <- colnames(df_augmented[[1]])
z <- lapply(df_augmented, function(x) { colnames(x) <- column_names; x })
zz <- do.call(rbind, z)
zzz <- as.data.frame(parse_month_year(zz$file))
zzzz <- cbind(zz, zzz)
zzzz$year       <- as.integer(as.character(zzzz$year))
zzzz$month_num  <- as.integer(as.character(zzzz$month_num))
zzzz$date <- as.Date(sprintf("%04d-%02d-01", zzzz$year, zzzz$month_num), "%Y-%m-%d")
zzzz <- zzzz[order(zzzz$date), ]        # make sure rows are in time order

regions <- df_augmented[[1]]$"Komisariaty-Specjalistyczne/-KPP/-KRP_NA"

plot_statistics <- function(df, df_name, column_name) {
  x <- paste0(df_name, "==", column_name)
  x_sanitized <- str_replace_all(x, " ", "-")
  x_sanitized <- str_replace_all(x_sanitized, "/", "")
  x_sanitized <- str_replace_all(x_sanitized, "%", "procent")
  x_sanitized <- paste0("./img/", x_sanitized, ".png")
  print(x_sanitized)
  png(x_sanitized)
  plot(df$date, df[[column_name]], type = "l", xaxt = "n",
      xlab = "Year-Month", ylab = column_name)      # line
  # points(df$date, df[[column_name]] == "character")                      # dots on observed months
  axis.Date(1,
            at = seq(min(df$date), max(df$date), by = "months"),
            format = "%Y-%m")
  dev.off()
}

for (region in regions) {
  values <- zzzz[zzzz["Komisariaty-Specjalistyczne/-KPP/-KRP_NA"] == region, ]
  for(column in column_names) {
    if (column %in% c("month",
                      "month_pl",
                      "year",
                      "date",
                      "month_num",
                      "Komisariaty-Specjalistyczne/-KPP/-KRP_NA",
                      "file")) {
      next
    }
    plot_statistics(values, region, column)
  }
}
