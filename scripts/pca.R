# --------------------------------------------------
# PCA of financial behavior
# --------------------------------------------------

library(tidyverse)
library(ggplot2) # for plotting
library(cluster) # for silhouette method
library(igraph)
library(dplyr)
library(ggdendro)

rm(list = ls())

# importing the cleaned data
kz_data <- read.csv("data/Findex_Microdata_2025_Kazakhstan_clean.csv")

# only behavioral attributes without age, gender and other
# because we don't want to cluster based on demographic features
# only based on financial behavior

cluster_vars <- kz_data %>%
  select(
    -female,
    -age,
    -educ,
    -inc_q,
    -emp_in,
    -urbanicity,
    -fin24
  )

# scaling every attribute for the PCA algorithm, so they are weighted
cluster_scaled <- scale(cluster_vars)

pca_res <- prcomp(cluster_scaled, center = TRUE, scale. = TRUE)

# saving PCA results for every row (person)
pca_data <- as.data.frame(pca_res$x)
#saving loadings of PCA to see what variables contribute the most in classification
loadings_df <- as.data.frame(pca_res$rotation)
loadings_df$variable <- rownames(loadings_df)
# top contributing attributes for PC1 and PC2
loadings_df_pc1 <- loadings_df %>%
  arrange(desc(abs(PC1)))
loadings_df_pc2 <- loadings_df %>%
  arrange(desc(abs(PC2)))

head(loadings_df_pc1, 10)
# fin20 30,7
# borrowed 30,3
# fin22a 30,4
# fin10	28,3
# receive_wages	25,3
# account_mob	24,5
# fin22b 22,5
# internet_use 21,1
# domestic_remittances 20,9
# fin19 20,4

head(loadings_df_pc2, 10)
# fin6 -37,0
# account -35,7
# fin25e1	32,8
# fin26b 32,2
# fin8 31,0
# fin2 -29,0
# fin26a 28,2
# fin9b 27,9
# fin25e3	-20,1
# fin4 15,2


# plotting and saving pca visualization
pca_plot <- ggplot(pca_data, aes(x = PC1, y = PC2)) +
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
  filename = "plots/pca_plot.png",
  plot = pca_plot,
  width = 8,
  height = 6,
  dpi = 300
)

fwrite(pca_data, "data/pca.csv")
fwrite(cluster_vars, "data/cluster_data.csv")

