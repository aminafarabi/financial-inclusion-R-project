library(dplyr)
library(caret)
library(randomForest)

rm(list = ls())

kz_data <- read.csv("data/Findex_Microdata_2025_Kazakhstan_clean.csv")

kz_data$target <- factor(
  kz_data$fin24,
  levels = 1:7,
  labels = c(
    "Savings",
    "FamilyRelativesFriends",
    "Other",
    "Borrowing",
    "Other",
    "Other",
    "CouldNotComeUpWithmoney"
  )
)

sup_data <- kz_data %>%
  select(
    target,
    #age,
    female,
    educ,
    inc_q,
    emp_in,
    borrowed,
    receive_pensions,
    receive_wages,
    account_mob,
    domestic_remittances,
    fin22a,
    fin17c,
    fin19,
    fin24a
  )

sup_data <- sup_data %>%
  mutate(across(everything(), factor))

sup_all <- sup_data %>%
  filter(target != "Other") %>%
  droplevels()

set.seed(123)
train_index <- createDataPartition(sup_all$target, p = 0.7, list = FALSE)
train_data <- sup_all[train_index, ]
test_data  <- sup_all[-train_index, ]

# random forest

rf_model <- randomForest(target ~ ., data = train_data, ntree = 500, importance = TRUE)

# predictions
rf_pred <- predict(rf_model, newdata = test_data)

# confusion matrix
confusionMatrix(rf_pred, test_data$target)

importance(rf_model)
varImpPlot(rf_model)

# -----------------------------
# plotting
rf_prob <- predict(rf_model, newdata = test_data, type = "prob")
rf_prob_df <- as.data.frame(rf_prob)

# добавляем столбец с истинными классами
rf_prob_df$true_class <- test_data$target

# long формат для ggplot
prob_long_rf <- rf_prob_df %>%
  pivot_longer(
    cols = -true_class,
    names_to = "predicted_class",
    values_to = "probability"
  )

#rf_prob$true_class <- test_data$target

# Переводим в long формат для ggplot
prob_long_rf <- rf_prob_df %>%
  pivot_longer(
    cols = -true_class,
    names_to = "predicted_class",
    values_to = "probability"
  )

# plot
rf_prob_plot <- ggplot(prob_long_rf, aes(
  x = predicted_class,
  y = probability,
  fill = true_class
)) +
  stat_summary(fun = mean, geom = "col", position = "dodge") +
  theme_minimal(base_size = 13) +
  labs(
    title = "Average predicted probabilities by actual class (Random Forest)",
    x = "Predicted category",
    y = "Average predicted probability",
    fill = "Actual class"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

rf_prob_plot


# -----------------------------------
# CV method
train_control <- trainControl(
  method = "cv",
  number = 10,
  classProbs = TRUE,
  savePredictions = "final"   # <- важно для CV графика
)


rf_model_cv <- train(
  target ~ .,
  data = sup_all,
  method = "rf",
  trControl = train_control,
  ntree = 500
)


cv_preds <- rf_model_cv$pred

cv_preds$predicted_class <- factor(cv_preds$pred, levels = levels(sup_all$target))
cv_preds$true_class <- factor(cv_preds$obs, levels = levels(sup_all$target))

#plotting
class_cols <- levels(train_data$target)

prob_long_cv <- cv_preds %>%
  select(pred, obs, all_of(class_cols)) %>%
  pivot_longer(
    cols = all_of(class_cols),
    names_to = "predicted_class",
    values_to = "probability"
  ) %>%
  rename(true_class = obs)

rf_cv_plot <- ggplot(prob_long_cv, aes(
  x = predicted_class,
  y = probability,
  fill = true_class
)) +
  stat_summary(fun = mean, geom = "col", position = "dodge") +
  theme_minimal(base_size = 13) +
  labs(
    title = "Average predicted probabilities by actual class (Random Forest CV)",
    x = "Predicted class",
    y = "Average predicted probability",
    fill = "Actual class"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

rf_cv_plot

ggsave(
  filename = "plots/rf_plot.png",
  plot = rf_prob_plot,
  width = 8,
  height = 6,
  dpi = 300
)

ggsave(
  filename = "plots/rf_cv_plot.png",
  plot = rf_cv_plot,
  width = 8,
  height = 6,
  dpi = 300
)


