# Unsupervised Learning:
# Clustering of financial behavior
# Country: Kazakhstan

library(tidyverse)

# importing the cleaned data
kz_data <- read.csv("data/Findex_Microdata_2025_Kazakhstan_clean.csv")

# only behavioral attributes without age, gender and other
# because we don't want to cluster based on financial behavior
cluster_vars <- kz_data %>%
  select(
    account, # do they have an account in the bank
    dig_account, # digital account
    anydigpayment,
    merchantpay_dig,
    pay_utilities,
    saved,
    borrowed,
    internet_use
  )

# scaling every attribute for the PCA algorithm, so they are weighted
cluster_scaled <- scale(cluster_vars)

pca_res <- prcomp(cluster_scaled, center = TRUE, scale. = TRUE)
summary(pca_res)

# saving PCA results for every row (person)
pca_df <- as.data.frame(pca_res$x)

pcaplot(
  pca_df$PC1,
  pca_df$PC2,
  xlab = "PC1",
  ylab = "PC2",
  main = "PCA of financial behavior (Kazakhstan)"
)


### k-means

#determine the optimal k
wss <- sapply(1:10, function(k){
  kmeans(cluster_scaled, centers = k, nstart = 25)$tot.withinss
})

# Plot Elbow
plot(1:10, wss, type = "b", pch = 19,
     xlab = "Number of clusters K",
     ylab = "Total within-clusters sum of squares",
     main = "Elbow Method for Optimal k")

# automatic elbow detection (first significant drop)
wss_diff <- diff(wss)
optimal_k_elbow <- which.max(-wss_diff[-1]) + 1
cat("Optimal k by Elbow method:", optimal_k_elbow, "\n")
abline(v = optimal_k_elbow, col = "red", lty = 2)
# the result shows 2 but I prefer 3

# Silhouette method for optimal k
# ----------------------------
sil_values <- sapply(2:10, function(k){
  km <- kmeans(cluster_scaled, centers = k, nstart = 25)
  ss <- silhouette(km$cluster, dist(cluster_scaled))
  mean(ss[, 3])
})

optimal_k_sil <- which.max(sil_values) + 1
cat("Optimal k by Silhouette method:", optimal_k_sil, "\n")

plot(2:10, sil_values, type = "b", pch = 19,
     xlab = "Number of clusters K",
     ylab = "Average Silhouette Width",
     main = "Silhouette Method for Optimal k")
abline(v = optimal_k_sil, col = "blue", lty = 2)
# shows k=2 as well but I will use k=3 for this data

set.seed(123)
# k_final <- max(optimal_k_elbow, optimal_k_sil)
########## I like k=3 more so
k_final = 3
kmeans_res <- kmeans(cluster_scaled, centers = k_final, nstart = 25)

pca_df$cluster <- factor(kmeans_res$cluster)
cluster_vars$cluster <- kmeans_res$cluster

plot(
  pca_df$PC1,
  pca_df$PC2,
  col = pca_df$cluster,
  pch = 19,
  xlab = "PC1",
  ylab = "PC2",
  main = "Clusters of financial behavior (k = 3)"
)

### adding demographics - aka profiling of clusters
# female
# age
# educ
# inc_q
# emp_in
# urbanicity

data_with_clusters <- kz_data
data_with_clusters$cluster_k3 <- kmeans_res$cluster


library(dplyr)

data_with_clusters %>%
  group_by(cluster_k3) %>%
  summarise(
    share_female = mean(female == 2),
    mean_age = mean(age),
    mean_educ = mean(educ),
    share_urban = mean(urbanicity == 1),
    share_employed = mean(emp_in == 1)
  )

library(ggplot2)

#gender distribution graphic
ggplot(data_with_clusters, aes(x = factor(cluster_k3), fill = factor(female))) +
  geom_bar(position = "fill") +
  labs(
    x = "Cluster",
    y = "Share",
    fill = "Female",
    title = "Gender composition by cluster"
  )

# age distibution graphic
ggplot(data_with_clusters, aes(x = factor(cluster_k3), y = age)) +
  geom_boxplot() +
  labs(
    x = "Cluster",
    y = "Age",
    title = "Age distribution by cluster"
  )


