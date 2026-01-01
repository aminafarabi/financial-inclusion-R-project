library(data.table)
library(dplyr)
library(tidyverse)
library(tools)

clean_findex_data <- function(file_path, na_threshold = 0.2) {
  data <- read.csv(file_path)
  
  # removing variables that are mostly NA with the set threshold
  data <- data %>%
    select(where(~ mean(is.na(.)) < na_threshold))
  
  # removing metadata and unnecessary survey variables
  data <- data %>%
    select(-c(economy, year, economycode, regionwb, pop_adult, wpid_random, wgt)) %>%
    select(-starts_with("con"))
  
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
