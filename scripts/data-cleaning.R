library(data.table)
library(dplyr)
library(tidyverse)
library(tools)
library(igraph)

rm(list = ls())

# --------------------------------------------------
# DATA CLEANING
# --------------------------------------------------

kz_data <- read.csv("data/Findex_Microdata_2025_Kazakhstan.csv") # 183 vars

# removing metadata and unnecessary survey variables
kz_data <- kz_data %>%
  select(-c(economy, year, economycode, regionwb, pop_adult, wpid_random, wgt)) %>%
  select(-starts_with("con")) # 124 vars

# removing variables that are for more than 30% NA
# creating function cause we need to remove NA twice
null_values_data_clean <- function(data, na_threshold = 0.3) {
  data <- data %>%
    select(where(~ mean(is.na(.)) < na_threshold))
  return(data)
}

kz_data <- null_values_data_clean(kz_data) # 57 vars

# --------------------------------------------------
# MODIFYING VARIABLES 
# --------------------------------------------------
# most of the variables in the survey are coded
# we change the values to meet standard binary notation
# and remove answers like "I don't know" and "Refused"

# Binary vars --------------------------------------
# 1 - yes
# 2 - no
# 3 - DK
# 4 - refused

fin_binary_vars <- c(
  "fin2","fin3","fin4","fin8","fin9a","fin9b","fin10",
  "fin17a","fin17c","fin19","fin20",
  "fin22a","fin22b","fin22d","fin22e","fin22f",
  "fin25e1","fin25e2","fin26a","fin26b", "fh1", "fh2",
  "fh2a", "fin30","fin32","fin37","fin38","fin42"
)

# 1 - yes, 0 - no
code_to_binary <- function(x) {
  x[x %in% c(3,4)] <- NA 
  x <- ifelse(x == 1, 1,
              ifelse(x == 2, 0, NA))
  return(x)
}

kz_data[fin_binary_vars] <- lapply(kz_data[fin_binary_vars], code_to_binary)

# receive vars -------------------------------------
# 1 - received in account
# 2 - received in cash
# 3 - received via some other method
# 4 - didn't receive
# 5 - DK/refused
# we make these vars binary (received/not received) 
# because for k-means different values should be equally different

receive_vars <- c(
  "receive_wages",
  "receive_transfers",
  "receive_pensions",
  "receive_agriculture",
  "pay_utilities"
)

kz_data$domestic_remittances <- ifelse(
  kz_data$domestic_remittances %in% c(1, 2), 1,
  ifelse(kz_data$domestic_remittances == 3, 0, NA)
)

receive_to_binary <- function(x) {
  x[x %in% c(5)] <- NA
  x[x %in% c(1, 2, 3)] <- 1
  x[x == 4] <- 0
  return(x)
}

kz_data[receive_vars] <- lapply(kz_data[receive_vars], receive_to_binary)

# ordinal variables --------------------------------
# 1 - weekly 
# 2 - monthly
# 3 - less than a month
# 4 - never
# 5 and 6 - NA
# fin25e3 and fin24a have 3 codes instead of 4
fin_freq_vars <- c("fin5","fin6", "fin24b")

ordinal_to_numeric <- function(x, na_vals = c(5,6)) {
  x[x %in% na_vals] <- NA  # never, DK, refused → NA
  return(x)
}

kz_data$fin25e3 <- ordinal_to_numeric(kz_data$fin25e3, na_vals = c(4,5))
kz_data$fin24a <- ordinal_to_numeric(kz_data$fin24a, na_vals = c(4,5))
kz_data[fin_freq_vars] <- lapply(kz_data[fin_freq_vars], ordinal_to_numeric)

# nominal variables --------------------------------
kz_data$fin24 <- ordinal_to_numeric(kz_data$fin24, na_vals = c(8,9))
kz_data$fin45 <- ordinal_to_numeric(kz_data$fin45, na_vals = c(7,8))

# NA cleaning again cause DK/refused became NA
kz_data <- null_values_data_clean(kz_data) # 57 vars

# --------------------------------------------------
# CORRELATION MATRIX TO REMOVE UNNECESSARY VARIABLES
# --------------------------------------------------
corr_data <- kz_data %>%
  mutate(across(where(is.factor), ~ as.numeric(as.character(.)))) %>%
  select(where(is.numeric))

corr_matrix_all <- cor(
  corr_data,
  use = "pairwise.complete.obs"
)

corr_df <- as.data.frame(as.table(corr_matrix_all))
colnames(corr_df) <- c("var1", "var2", "correlation")

ggplot(corr_df, aes(x = var1, y = var2, fill = correlation)) +
  geom_tile() +
  scale_fill_gradient2(
    low = "blue",
    mid = "white",
    high = "red",
    midpoint = 0,
    limits = c(-1, 1),
    name = "Correlation"
  ) +
  theme_minimal(base_size = 5) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_blank()
  ) +
  coord_fixed() +
  labs(title = "Correlation Matrix (All Numeric Variables)")

corr_df_filtered <- corr_df %>%
  filter(var1 != var2) %>%
  mutate(
    var1 = as.character(var1),
    var2 = as.character(var2),
    pair = paste(pmin(var1, var2), pmax(var1, var2), sep = " ")
  ) %>%
  distinct(pair, .keep_all = TRUE)

# removing variables with correlation higher than 70%
corr_thresh <- 0.7

high_corr_pairs <- corr_df_filtered %>%
  filter(abs(correlation) >= corr_thresh) %>%
  select(var1, var2)

components <- components(graph_from_data_frame(high_corr_pairs, directed = FALSE))
group_list <- split(names(components$membership), components$membership)

# we leave only 1 attribute in the list of similar attributes
# we delete:
# anydigpayment dig_account merchantpay_dig fin3 fin25e2 account_fin = account 
# fin32 = receive_wages
# fin37 = receive_transfers
# fin38 = receive_pensions
# fin42 = receive_agriculture
# fin30 = pay_utilities
# fh1 and fh2 = domestic_remittances

kz_data <- kz_data %>%
  select(
    -anydigpayment,
    -dig_account,
    -merchantpay_dig,
    -fin3,
    -fin25e2,
    -account_fin,
    -fin32,
    -fin37,
    -fin38,
    -fin42,
    -fin30,
    -fh1,
    -fh2
  ) # 44 vars

# --------------------------------------------------
# DATA IMPUTATION
# --------------------------------------------------
# mode function for binary variables
get_mode <- function(x) {
  names(sort(table(na.omit(x)), decreasing = TRUE))[1]
}

impute_missing <- function(data) {
  for (col in names(data)) {
    x <- data[[col]]
    if (!any(is.na(x))) next
    if (col == "age") {  # the only continuous var
      data[[col]][is.na(x)] <- median(x, na.rm = TRUE)
    } else {  # all others are discrete
      mode_val <- get_mode(x)
      data[[col]][is.na(x)] <- mode_val
    }
  }
  return(data)
}

kz_data <- impute_missing(kz_data)
# checking there are no NAs
colSums(is.na(kz_data))[colSums(is.na(kz_data)) > 0]
# saving into the file
fwrite(kz_data, "data/Findex_Microdata_2025_Kazakhstan_clean.csv")






