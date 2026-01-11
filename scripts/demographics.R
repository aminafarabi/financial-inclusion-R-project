# -------------------------------------------------
# demographics analysis and visualization
# -------------------------------------------------

rm(list = ls())

kz_data <- read.csv("data/Findex_Microdata_2025_Kazakhstan_clean.csv")
cluster_vars <- read.csv("data/cluster_data.csv")
pca_data <- read.csv("data/pca.csv")

kz_data$km3 <- factor(cluster_vars$km3)

# modifying all attributes to factors with corresponding labels
kz_data$km3_label <- factor(
  kz_data$km3,
  levels = c(1, 2, 3),
  labels = c(
    "Basic account holders with limited digital engagement",
    "Digitally and financially active users",
    "Partially included and irregular users"
  )
)
kz_data$km4 <- factor(cluster_vars$km4)

kz_data <- kz_data %>%
  mutate(
    inc_q_label = factor(
      inc_q,
      levels = c(1, 2, 3, 4, 5),
      labels = c("Lowest", "Low-middle", "Middle", "Upper-middle", "Highest")
    )
  )

kz_data <- kz_data %>%
  mutate(
    educ_label = factor(
      educ,
      levels = c(1, 2, 3),
      labels = c("Primary or less", "Secondary", "Tertiary or more")
    )
  )

kz_data <- kz_data %>%
  mutate(
    urban_label = factor(
      urbanicity,
      levels = c(1, 2),
      labels = c("Rural area", "Urban area")
    )
  )

kz_data <- kz_data %>%
  mutate(
    emp_label = factor(
      emp_in,
      levels = c(1, 2),
      labels = c("In the workforce", "Out of the workforce")
    )
  )

# getting basic cluster distributions of demographic attributes
demo_binary <- kz_data %>%
  group_by(km3_label) %>%
  summarise(
    n = n(),
    female_share = mean(female == 1, na.rm = TRUE),
    avg_age = mean(age, na.rm = TRUE),
    employed_share = mean(emp_in == 1, na.rm = TRUE),
    urban_share = mean(urbanicity == 2, na.rm = TRUE)
  )

demo_educ <- kz_data %>%
  group_by(km3, educ) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(km3) %>%
  mutate(share = n / sum(n))

demo_income <- kz_data %>%
  group_by(km3, inc_q) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(km3) %>%
  mutate(share = n / sum(n))

demo_fin24 <- kz_data %>%
  group_by(km3, fin24) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(km3) %>%
  mutate(share = n / sum(n)) %>%
  arrange(km3, desc(share))

# -----------------------------------------
# plots
# -----------------------------------------
get_gradient <- function(n) {
  colorRampPalette(c(
    "darkgreen",
    "#728a7b"
  ))(n)
}
base_colors <- get_gradient(6)

age_dist_plot <- kz_data %>%
  ggplot(aes(x = age)) +
  geom_density(fill = "darkgreen", alpha = 0.8) +
  facet_wrap(~ km3_label, ncol = 1) +
  labs(
    title = "Age distribution by cluster",
    x = "Age",
    y = "Density"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

income_dist_plot <- kz_data %>%
  group_by(km3_label, inc_q_label) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(km3_label) %>%
  mutate(share = n / sum(n)) %>%
  ggplot(aes(x = share, y = "", fill = fct_rev(inc_q_label))) +
  geom_col(width = 0.8) +
  facet_wrap(~ km3_label, ncol = 1) +
  scale_x_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = base_colors) +
  labs(
    title = "Income level distribution by cluster",
    x = "",
    y = "Share of respondents",
    fill = "Income level"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

educ_dist_plot <-  kz_data %>%
  group_by(km3_label, educ_label) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(km3_label) %>%
  mutate(share = n / sum(n)) %>%
  ggplot(aes(x = share, y = "", fill = fct_rev(educ_label))) +
  geom_col(width = 0.8) +
  facet_wrap(~ km3_label, ncol = 1) +
  scale_x_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = base_colors[c(1, 3, 5)]) + 
  labs(
    title = "Education level distribution by cluster",
    x = "",
    y = "Share of respondents",
    fill = "Education level"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5),
  )

urban_dist_plot <-  kz_data %>%
  group_by(km3_label, urban_label) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(km3_label) %>%
  mutate(share = n / sum(n)) %>%
  ggplot(aes(x = share, y = "", fill = urban_label)) +
  geom_col(width = 0.8) +
  facet_wrap(~ km3_label, ncol = 1) +
  scale_x_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = base_colors[c(4, 1)]) + 
  labs(
    title = "Urbanicity distribution by cluster",
    x = "",
    y = "Share of respondents",
    fill = "Urbanicity"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5),
  )

emp_dist_plot <-  kz_data %>%
  group_by(km3_label, emp_label) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(km3_label) %>%
  mutate(share = n / sum(n)) %>%
  ggplot(aes(x = share, y = "", fill = emp_label)) +
  geom_col(width = 0.8) +
  facet_wrap(~ km3_label, ncol = 1) +
  scale_x_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = base_colors[c(1, 3)]) + 
  labs(
    title = "Employment distribution by cluster",
    x = "",
    y = "Share of respondents",
    fill = "Eployment"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5),
  )

ggsave(
  filename = "plots/age_distribution_plot.png",
  plot = age_dist_plot,
  width = 8,
  height = 4,
  dpi = 300
)

ggsave(
  filename = "plots/inc_distribution_plot.png",
  plot = income_dist_plot,
  width = 8,
  height = 4,
  dpi = 300
)

ggsave(
  filename = "plots/educ_distribution_plot.png",
  plot = educ_dist_plot,
  width = 8,
  height = 4,
  dpi = 300
)


ggsave(
  filename = "plots/emp_distribution_plot.png",
  plot = emp_dist_plot,
  width = 8,
  height = 4,
  dpi = 300
)

ggsave(
  filename = "plots/urban_distribution_plot.png",
  plot = urban_dist_plot,
  width = 8,
  height = 4,
  dpi = 300
)

