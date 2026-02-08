
# source("requirements.R")

library(readxl)
library(stringr)
library(futile.logger)

source("R/utils.R")

download_configs <- list.files(
  path = DOWNNLOAD_CONFIG,
  pattern = "\\.csv$",
  full.names = TRUE
)

flog.threshold(DEBUG)
flog.info(paste("Discovered", length(download_configs), "download configs"))
flog.debug(paste("Making sure that", DATA_ROOT, "exists"))
dir.create(DATA_ROOT, showWarnings = FALSE)

plot_specs <- list()
for (file in download_configs) {

    data_tag <- sub("\\.csv$", "", basename(file))

    csv <- read.csv(file, header = TRUE)
    data <- list()

    previous_year <- NA
    previous_df <- NA
    flog.info(paste0("  Processing ", file))
    flog.debug(paste0("    Creating ", paste0(DATA_ROOT, "/", data_tag)))
    dir.create(paste0(DATA_ROOT, "/", data_tag), showWarnings = FALSE)
    flog.debug(paste("Making sure that", paste0("./img/", data_tag, "/"), "exists"))
    dir.create("./img/", showWarnings = FALSE)
    dir.create(paste0("./img/", data_tag, "/"), showWarnings = FALSE)

    for (i in 1:nrow(csv)) {
        id <- csv[i, ]$id
        url <- csv[i, ]$url
        year <- as.character(csv[i, ]$year)
        month <- as.character(csv[i, ]$month)
        collapse <- as.logical(csv[i, ]$collapse)
        new_name <- csv[i, ]$new_name
        flog.info(paste(
            "id", id,
            "url", url,
            "year", year,
            "month", month,
            "collapse", collapse,
            "new_name", new_name
            ))

        if (is.na(previous_year) || previous_year != year) {
            previous_year <- year
            previous_df <- NA
        }

        dir_path = paste0(DATA_ROOT, "/", data_tag, "/", year)
        dir.create(dir_path, showWarnings = FALSE)
        new_path <- paste0(dir_path, "/", new_name)
        if (!file.exists(new_path)) {
            flog.info(paste("Downloading", new_path))
            system(paste("wget", "--no-check-certificate", url, "-O", new_path))
        }
        else {
            flog.info(paste("   File", new_path, "has already been downloaded."))
        }
        current_df <- suppressMessages({
            read_excel(new_path) %>% sanitize %>% na.omit
        })
        flipped <- setNames(
            as.data.frame( t(current_df[-1]) ),
            current_df[[1]]
        )
        flipped[] <- lapply(flipped, function(x) as.numeric(x))

        flipped <- flipped[intersect(column_names, names(flipped))]
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
    x <- Reduce(rbind, data)
    for (region in unique(x$region)) {
        if (is.null(plot_specs[[region]])) {
            plot_specs[[region]] <- list()
            plot_specs[[region]][[data_tag]] <- list()
        }
        current_df <- x[x$region == region,]
        for (col_name in setdiff(names(current_df), c("region", "date"))) {
            plot_specs[[region]][[data_tag]][[col_name]] <- list(
                x = current_df$date,
                y = current_df[[col_name]]
            )
        }
    }
}

widgets <- generate_widgets(plot_specs)
