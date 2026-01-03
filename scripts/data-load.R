library(data.table)
library(dplyr)
library(tidyverse)
library(tools)

clean_findex_data <- function(file_path, na_threshold = 0.3) {
  data <- read.csv(file_path)
  
  # removing variables that are mostly NA with the set threshold
  data <- data %>%
    select(where(~ mean(is.na(.)) < na_threshold))
  
  # removing metadata and unnecessary survey variables
  data <- data %>%
    select(-c(economy, year, economycode, regionwb, pop_adult, wpid_random, wgt)) %>%
    select(-starts_with("con"))
  
  binary_vars <- sapply(data, function(x) all(na.omit(x) %in% c(0,1)))
  
  numeric_vars <- sapply(data, is.numeric) & !binary_vars
  factor_vars  <- sapply(data, is.factor) | sapply(data, is.character) | binary_vars
  
  data[numeric_vars] <- lapply(data[numeric_vars], function(x) {
    x[is.na(x)] <- median(x, na.rm = TRUE)
    return(x)
  })
  
  data[factor_vars] <- lapply(data[factor_vars], function(x) {
    x[is.na(x)] <- names(sort(table(x), decreasing = TRUE))[1]
    return(x)
  })
  
  # saving into data folder for convenience
  file_name <- file_path_sans_ext(basename(file_path))
  dir_path <- dirname(file_path)
  output_path <- file.path(dir_path, paste0(file_name, "_clean.csv"))
  
  fwrite(data, output_path)
  return(data)
}

# uploading kz and it data
kz_data_clean <- clean_findex_data("data/Findex_Microdata_2025_Kazakhstan.csv")
it_data_clean <- clean_findex_data("data/Findex_Microdata_2025_Italy.csv")



