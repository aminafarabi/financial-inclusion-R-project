library(ggplot2)
library(RColorBrewer)
library(glue)

rm(list = ls())

# --------------------------------------------------
# determine the optimal k
# --------------------------------------------------


cluster_vars <- read.csv("data/cluster_data.csv")
pca_data <- read.csv("data/pca.csv")

cluster_scaled <- scale(cluster_vars)

# elbow method
k_max = 8
wss <- sapply(1:k_max, function(k){
  kmeans(cluster_scaled, centers = k, nstart = 25)$tot.withinss
})

elbow_df <- data.frame(
  k = 1:k_max,
  wss = wss
)

wss_diff <- diff(elbow_df$wss)
optimal_k_elbow <- which.max(-wss_diff[-1]) + 1

# plot and save
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
  height = 4,
  dpi = 300
)

# k = 2

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

#plot sil
sil_plot <- ggplot(sil_df, aes(x = k, y = silhouette_width)) +
  geom_line(color = "darkgreen", linewidth = 0.75) +
  geom_point(color = "darkgreen", size = 2) +
  geom_vline(
    xintercept = optimal_k_sil,
    linetype = "dashed",
    color = "red"
  ) +
  labs(
    title = "Silhouette Method for Optimal Number of Clusters",
    x = "Number of clusters (k)",
    y = "Average silhouette width"
  ) + 
  theme_minimal(base_size = 13)

ggsave(
  filename = "plots/optimal_k_sil_method.png",
  plot = sil_plot,
  width = 8,
  height = 4,
  dpi = 300
)


# --------------------------------------------------
# K-MEANS k=3 k=4
# --------------------------------------------------
# clustering for k=3 and k=4
set.seed(123)

k3means_res <- kmeans(cluster_scaled, centers = 3, nstart = 25)
k4means_res <- kmeans(cluster_scaled, centers = 4, nstart = 25)

cluster_vars$km3 <- k3means_res$cluster
cluster_vars$km4 <- k4means_res$cluster

pca_data$km3 <- factor(k3means_res$cluster, levels = c(1,2,3))
pca_data$km4 <- factor(k4means_res$cluster, levels = c(1,2,3,4))

# the most different variables in the obtained clusters 

km3_summary <- cluster_vars %>%
  select(-km4) %>%
  group_by(km3) %>%
  summarise(
    cluster = first(km3),
    n = n(),
    across(where(is.numeric),mean),
    .groups = "drop"
  )

km4_summary <- cluster_vars %>%
  select(-km3) %>%
  group_by(km4) %>%
  summarise(
    cluster = first(km4),
    n = n(),
    across(where(is.numeric),mean),
    .groups = "drop"
  )

km3_cluster_range <- km3_summary %>%
  summarise(across(-km3, ~ max(.) - min(.))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "range") %>%
  arrange(desc(range)) %>%
  slice(1:12) %>%
  pull(variable)

km4_cluster_range <- km4_summary %>%
  summarise(across(-km4, ~ max(.) - min(.))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "range") %>%
  arrange(desc(range)) %>%
  slice(1:12) %>%
  pull(variable)

km3_summary <- km3_summary[km3_cluster_range]
km4_summary <- km4_summary[km4_cluster_range]

#---------------------------------------------------------
# HIERARCHICAL CLUSTERIZATION
#---------------------------------------------------------
dist_mat <- dist(cluster_scaled, method = "euclidean")
hc <- hclust(dist_mat, method = "ward.D2")

hc3 <- cutree(hc, k = 3)
hc4 <- cutree(hc, k = 4)

cluster_vars$hc3 <- hc3
cluster_vars$hc4 <- hc4

pca_data$hc3 <- factor(hc3, levels = c(1,2,3))
pca_data$hc4 <- factor(hc4, levels = c(2,3,4,1)) # so colors match on the plot

hc3_summary <- cluster_vars %>%
  select(-km3, -km4, -hc4) %>%
  group_by(hc3) %>%
  summarise(
    cluster = first(hc3),
    n = n(),
    across(where(is.numeric),mean))

hc4_summary <- cluster_vars %>%
  select(-km3, -km4, -hc3) %>%
  group_by(hc4) %>%
  summarise(
    cluster = first(hc4),
    n = n(),
    across(where(is.numeric),mean))

hc3_cluster_range <- hc3_summary %>%
  summarise(across(-hc3, ~ max(.) - min(.))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "range") %>%
  arrange(desc(range)) %>%
  slice(1:12) %>%
  pull(variable)

hc4_cluster_range <- hc4_summary %>%
  summarise(across(-hc4, ~ max(.) - min(.))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "range") %>%
  arrange(desc(range)) %>%
  slice(1:12) %>%
  pull(variable)

hc3_summary <- hc3_summary[hc3_cluster_range]
hc4_summary <- hc4_summary[hc4_cluster_range]

png("plots/dendrogram.png", width = 8, height = 6, units = "in", res = 300)
plot(
  hc,
  labels = FALSE,
  hang = -1,
  main = "Hierarchical clustering dendrogram",
  xlab = "",
  sub = ""
)
dev.off()

#COMPARING KM AND HC RESULTS
km3_hc3_table <- table(
  kmeans = pca_data$km3,
  hierarchical = pca_data$hc3
)

km4_hc4_table <- table(
  kmeans = pca_data$km4,
  hierarchical = pca_data$hc4
)

# -------------------------------------------------
# PLOTS
# -------------------------------------------------

save_plot <- function(cluster) {
  cluster_plot <- ggplot(pca_data, aes(x = PC1, y = PC2, color = .data[[cluster]])) +
    geom_point(size = 2, alpha = 0.7) +
    labs(
      title = glue("PCA of Financial Behavior with {cluster} clusters"),
      x = "Principal Component 1",
      y = "Principal Component 2",
    ) +
    scale_color_brewer(palette = "Set1") +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.position = c(0.9,0.85),
      legend.title = element_blank()
    )
  
  ggsave(
    filename = glue("plots/{cluster}_plot.png"),
    plot = cluster_plot,
    width = 8,
    height = 6,
    dpi = 300
  )
}

save_plot("km3")
save_plot("km4")
save_plot("hc3")
save_plot("hc4")

fwrite(pca_data, "data/pca.csv")
fwrite(cluster_vars, "data/cluster_data.csv")
