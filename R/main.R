#!/bin/R

library(readxl)

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

# Count occurrences of each unique schema
schema_counts <- as.data.frame(table(schemas))

# Sort schemas by frequency
schema_counts <- schema_counts[order(-schema_counts$Freq), ]

# Get schemas with occurrences less than X
rare_schemas <- names(schema_counts[schema_counts < 2])

# Get filenames/dataframes that match rare schemas
rare_files <- basename(files[schemas %in% rare_schemas])

print(rare_files)
