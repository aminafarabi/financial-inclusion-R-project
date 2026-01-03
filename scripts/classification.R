# Unsupervised Learning:
# Clustering of financial behavior
# Country: Kazakhstan

library(tidyverse)
library(ggplot2) # for plotting
library(cluster) # for silhouette method

# importing the cleaned data
kz_data <- read.csv("data/Findex_Microdata_2025_Kazakhstan_clean.csv")

# only behavioral attributes without age, gender and other
# because we don't want to cluster based on financial behavior
cluster_vars <- kz_data %>%
  select(
    account, # do they have an account in the bank (binary)
    dig_account, # digital account (binary)
    anydigpayment, # digital payment (binary)
    merchantpay_dig, # purchasing in digital channels (binary)
    pay_utilities, # paying for house bills online (binary)
    saved, # did the person save money (binary)
    borrowed, # did the person borrow money (binary)
    internet_use # does the person use the internet (binary)
  )

# scaling every attribute for the PCA algorithm, so they are weighted
cluster_scaled <- scale(cluster_vars)

pca_res <- prcomp(cluster_scaled, center = TRUE, scale. = TRUE)
summary(pca_res)

# saving PCA results for every row (person)
pca_df <- as.data.frame(pca_res$x)

# plotting and saving pca visualisation
pca_plot <- ggplot(pca_df, aes(x = PC1, y = PC2)) +
  geom_point(size = 2, alpha = 0.7, , color="darkgreen") +
  labs(
    title = "PCA of Financial Behavior (Kazakhstan)",
    x = "Principal Component 1",
    y = "Principal Component 2"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5)
)

ggsave(
  filename = "plots/pca_plot_kz.png",
  plot = pca_plot,
  width = 8,
  height = 6,
  dpi = 300
)

### CLUSTERIZATION
### K-MEANS

# determine the optimal k
# elbow method
k_max = 10
wss <- sapply(1:k_max, function(k){
  kmeans(cluster_scaled, centers = k, nstart = 25)$tot.withinss
})

elbow_df <- data.frame(
  k = 1:k_max,
  wss = wss
)

wss_diff <- diff(elbow_df$wss)
optimal_k_elbow <- which.max(-wss_diff[-1]) + 1

# Plot Elbow
k_elbow_plot = ggplot(elbow_df, aes(x = k, y = wss)) +
  geom_line(color="darkgreen", linewidth = 0.75) +
  geom_point(color = "darkgreen", size = 2) +
  labs(
    title = "Elbow method for Optimal Number of Clusters",
    x = "k",
    y = "Within-Cluster Sum of Squares"
  ) +
  geom_vline(
    xintercept = optimal_k_elbow,
    linetype = "dashed",
    color = "red"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5)
  )
  
ggsave(
  filename = "plots/optimal_k_elbow_method.png",
  plot = k_elbow_plot,
  width = 8,
  height = 6,
  dpi = 300
)

# silhouette method
sil_values <- sapply(2:k_max, function(k){
  km_res <- kmeans(cluster_scaled, centers = k, nstart = 25)
  sil <- silhouette(km_res$cluster, dist(cluster_scaled))
  mean(sil[, 3])
})

optimal_k_sil = which.max(sil_values) + 1

sil_df <- data.frame(
  k = 2:k_max,
  silhouette_width = sil_values
)

sil_plot <- ggplot(sil_df, aes(x = k, y = silhouette_width)) +
  geom_line(color = "darkgreen", linewidth = 0.75) +
  geom_point(color = "darkgreen", size = 2) +
  geom_vline(
    xintercept = optimal_k_silhouette,
    linetype = "dashed",
    color = "red"
  ) +
  labs(
    title = "Silhouette Method for Optimal Number of Clusters",
    x = "Number of clusters (k)",
    y = "Average silhouette width"
  ) + 
  theme_minimal(base_size = 13)
# shows k=2 as well but I will use k=3 for this data

# clustering + visualization
set.seed(123)
# k_final <- max(optimal_k_elbow, optimal_k_sil)
########## I like k=3 more so
k_final = 3
kmeans_res <- kmeans(cluster_scaled, centers = k_final, nstart = 25)

pca_df$cluster <- factor(kmeans_res$cluster)
cluster_vars$cluster <- kmeans_res$cluster

clusters_plot <- ggplot(pca_df, aes(x = PC1, y = PC2, color = cluster)) +
  geom_point(size = 2, alpha = 0.7) +
  labs(
    title = "PCA of Financial Behavior with k-means clusters",
    x = "Principal Component 1",
    y = "Principal Component 2",
  ) +
  scale_color_brewer(palette = "Set1") +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "none"
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

###

pca_scores <- pca_res$x[, 1:2]
dist_mat <- dist(pca_scores, method = "euclidean")
hc <- hclust(dist_mat, method = "ward.D2")
plot(hc, labels = FALSE, main = "Hierarchical clustering dendrogram")
rect.hclust(hc, k = 3, border = 2:4)

clusters_hc <- cutree(hc, k = 3)
clustered_data <- cluster_vars %>%
  mutate(hc_cluster = clusters_hc)
library(dendextend)
dend <- as.dendrogram(hc)
dend_col <- color_branches(dend, k = 3)
plot(dend_col)

table(kmeans_cluster = kmeans_res$cluster,
      hc_cluster = clusters_hc)

library(mclust)

ari <- adjustedRandIndex(kmeans_res$cluster, clusters_hc)
cat("Adjusted Rand Index:", ari, "\n")


