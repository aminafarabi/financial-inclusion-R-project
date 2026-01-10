# ============================================
# CLASSIFICATION AND CLUSTERING ANALYSIS
# COUNTRY: ITALY
# ============================================

library(tidyverse)
library(caret)
library(randomForest)
library(e1071)
library(factoextra)
library(cluster)
# ----------------------------
# 1. DATA IMPORT AND PREPARATION
# ----------------------------

# Importing Italian data
it_data <- read.csv("data/Findex_Microdata_2025_Italy_clean.csv")


# Check for missing values
colSums(is.na(it_data))

# ----------------------------
# 2. DATA CLEANING
# ----------------------------

# Replace empty strings with NA
it_data[it_data == ""] <- NA

cluster_vars_it <- it_data %>%
  select(
    account,          # financial account ownership (convert to numeric)
    domestic_remittances, # domestic remittances
    fin2,             # financial inclusion indicator
    internet_use,     # internet usage
    age,              # age (numeric)
    emp_in,           # employment status
    inc_q             # income quartile
  )


# ============================================
# PCA + K-MEANS CLUSTERING (Italy)
# ============================================

# ----------------------------
# 1. DATA PREPARATION
# ----------------------------

# Выбираем переменные для кластеризации
cluster_vars_it <- it_data %>%
  select(
    account,               # фактор (0/1)
    #domestic_remittances,  # фактор
    fin2,                  # фактор
    internet_use,          # фактор
    age,                   # числовой
    emp_in,                # фактор
    inc_q                  # фактор (1-4)
  )

# Убираем пропуски
cluster_vars_complete <- na.omit(cluster_vars_it)

# Преобразуем факторы в числа
cluster_numeric <- cluster_vars_complete %>%
  mutate(across(where(is.factor), ~ as.numeric(as.character(.))))

# Проверим данные
summary(cluster_numeric)

# Масштабируем числовые данные
cluster_scaled <- scale(cluster_numeric)

# ----------------------------
# 2. PCA (Principal Component Analysis)
# ----------------------------

pca_res <- prcomp(cluster_scaled, center = TRUE, scale. = TRUE)
summary(pca_res)  # доля объясненной дисперсии

# Преобразуем PCA в датафрейм для визуализации
pca_df <- as.data.frame(pca_res$x)

# ----------------------------
# 3. DETERMINING OPTIMAL NUMBER OF CLUSTERS
# ----------------------------

sil_values <- sapply(2:10, function(k){
  km <- kmeans(cluster_scaled, centers = k, nstart = 25)
  ss <- silhouette(km$cluster, dist(cluster_scaled))
  mean(ss[, 3])  # среднее значение силуэта
})

optimal_k <- which.max(sil_values) + 1  # +1, так как индекс начинается с 2
cat("Оптимальное количество кластеров по Silhouette:", optimal_k, "\n")


wss <- sapply(1:10, function(k){
  kmeans(cluster_scaled, centers = k, nstart = 25)$tot.withinss
})

# Автоматический расчет локтя (метод "first significant drop")
# Простая эвристика: точка с наибольшим уменьшением WSS
wss_diff <- diff(wss)
optimal_k <- which.max(-wss_diff[-1]) + 1  # +1 из-за смещения индекса
cat("Оптимальное количество кластеров по Elbow Method:", optimal_k, "\n")

# ----------------------------
# 4. K-MEANS CLUSTERING
# ----------------------------

set.seed(123)
k_opt <- 3  # допустим, по Elbow метод определено k=3
kmeans_res <- kmeans(cluster_scaled, centers = k_opt, nstart = 25)

# Добавляем метки кластеров
cluster_results <- cluster_vars_complete %>%
  mutate(cluster = as.factor(kmeans_res$cluster))

# ----------------------------
# 5. VISUALIZATION IN PCA SPACE
# ----------------------------

ggplot(pca_df, aes(x = PC1, y = PC2, color = cluster_results$cluster)) +
  geom_point(alpha = 0.7, size = 2) +
  labs(title = "K-means Clustering (Italy) in PCA Space",
       x = "PC1",
       y = "PC2",
       color = "Cluster") +
  theme_minimal()

# ----------------------------
# 6. CLUSTER PROFILE
# ----------------------------

cluster_profile <- cluster_results %>%
  group_by(cluster) %>%
  summarise(
    n = n(),
    account_rate = mean(as.numeric(account) - 1),
    internet_use_rate = mean(as.numeric(internet_use) - 1),
    avg_age = mean(age, na.rm = TRUE),
    avg_income = mean(as.numeric(inc_q), na.rm = TRUE),
    employment_rate = mean(as.numeric(emp_in) - 1)
  )

print("Cluster Profiles:")
print(cluster_profile)

