library(caret)
library(nnet)
library(tidyr)
library(ggplot2)
library(randomForest)
library(MLmetrics)

# -----------------------------------------------
# supervised learning
# -----------------------------------------------

rm(list = ls())

kz_data <- read.csv("data/Findex_Microdata_2025_Kazakhstan_clean.csv")

table(kz_data$fin24)

# removing "selling assets" options due to little number of people
kz_data$target5 <- factor(
  kz_data$fin24,
  levels = 1:7,
  labels = c(
    "Savings",
    "FamilyRelativesFriends",
    "MoneyFromWorking",
    "Borrowing",
    "Other",
    "Other",
    "CouldNotComeUpWithmoney"
  )
)

# removing "money from working" option due to little number of people
kz_data$target4 <- factor(
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

#------------------------------------------------
# ALL VARS
target4 <- "target4"
all_vars_sup_4 <- names(kz_data)[!names(kz_data) %in% c("fin24", "target5")]

sup_all_4 <- kz_data %>%
  select(all_of(c(target4, all_vars_sup_4)))

target5 <- "target5"
all_vars_sup_5 <- names(kz_data)[!names(kz_data) %in% c("fin24", "target4")]

sup_all_5 <- kz_data %>%
  select(all_of(c(target5, all_vars_sup_5)))

sup_all_4 <- sup_all_4 %>%
  filter(target4 != "Other") %>%
  droplevels()
#-------------------------------------------------
# train and test split (30/70)

set.seed(123)

train_index_4 <- createDataPartition(
  sup_all_4$target4,
  p = 0.7,
  list = FALSE
)

train_index_5 <- createDataPartition(
  sup_all_5$target5,
  p = 0.7,
  list = FALSE
)

train_data_4 <- sup_all_4[train_index_4, ]
test_data_4  <- sup_all_4[-train_index_4, ]

train_data_5 <- sup_all_5[train_index_5, ]
test_data_5  <- sup_all_5[-train_index_5, ]

#-------------------------------------------------
#Multinomial Logistic Regression
#-------------------------------------------------

mnl_model_4 <- multinom(
  target4 ~ .,
  data = train_data_4
)

mnl_model_5 <- multinom(
  target5 ~ .,
  data = train_data_5
)

pred_class4 <- predict(mnl_model_4, newdata = test_data_4, type = "class")
pred_prob4 <- predict(mnl_model_4, newdata = test_data_4, type = "probs")

pred_class5 <- predict(mnl_model_5, newdata = test_data_5, type = "class")
pred_prob5 <- predict(mnl_model_5, newdata = test_data_5, type = "probs")

cm4 <- confusionMatrix(pred_class4, test_data_4$target4)
cm5 <- confusionMatrix(pred_class5, test_data_5$target5)

#comparing 4 and 5 class target results
cm4$byClass[, c("Sensitivity", "Specificity", "Balanced Accuracy")]
cm5$byClass[, c("Sensitivity", "Specificity", "Balanced Accuracy")]

#-------------------------------------------------
# plots

prob_df <- as.data.frame(pred_prob4)
prob_df$true_class <- test_data_4$target4

prob_long <- prob_df %>%
  pivot_longer(
    cols = -true_class,
    names_to = "predicted_class",
    values_to = "probability"
  )

all_vars_4_plot <- ggplot(prob_long, aes(
  x = predicted_class,
  y = probability,
  fill = true_class
)) +
  stat_summary(fun = mean, geom = "col", position = "dodge") +
  theme_minimal(base_size = 13) +
  labs(
    title = "Average predicted probabilities by actual emergency funding source",
    x = "Predicted category",
    y = "Average predicted probability",
    fill = "Actual category"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
all_vars_4_plot

# --------------------------------------------------------
# ONLY SELECTED VARS

sup_data <- kz_data %>%
  select(
    target4,
    target5,
    age,
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
  mutate(across(
    c(target4,
      target5,
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
    ),
    factor
  ))

sup_data4 <- sup_data %>%
  filter(target4 != "Other") %>%
  droplevels()

sup_data5 <- sup_data %>%
  filter(target5 != "Other") %>%
  droplevels()

set.seed(123)

train_control <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  summaryFunction = multiClassSummary,
  savePredictions = "final"
)

mlr_data4 <- sup_data4 %>%
  select(-target5)

mlr_data5 <- sup_data5 %>%
  select(-target4)

mlr_model4 <- train(
  form = target4 ~ .,
  data = mlr_data4,
  method = "multinom",
  trControl = train_control,
  trace = FALSE
)

mlr_model5 <- train(
  form = target5 ~ .,
  data = mlr_data5,
  method = "multinom",
  trControl = train_control,
  trace = FALSE
)

mlr_model4$results

cv_preds4 <- mlr_model4$pred
cv_preds5 <- mlr_model5$pred

cm_4 <- confusionMatrix(cv_preds4$pred, cv_preds4$obs)
cm_4

cm_5 <- confusionMatrix(cv_preds5$pred, cv_preds5$obs)
cm_5

# ---------------------------
# plot

cm4_df <- as.data.frame(cm_4$table)
cm5_df <- as.data.frame(cm_5$table)

cm4_df <- cm4_df %>%
  mutate(Prediction = factor(Prediction),
         Reference = factor(Reference))

cm5_df <- cm5_df %>%
  mutate(Prediction = factor(Prediction),
         Reference = factor(Reference))

prob4_df <- predict(mlr_model4, mlr_data4, type = "prob")
prob4_df$true_class <- mlr_data4$target4
prob_long4 <- prob_df %>%
  pivot_longer(
    cols = -true_class,
    names_to = "predicted_class",
    values_to = "probability"
  )

selected_vars_4_plot <- ggplot(prob_long4, aes(
  x = predicted_class,
  y = probability,
  fill = true_class
)) +
  stat_summary(fun = mean, geom = "col", position = "dodge") +
  theme_minimal(base_size = 13) +
  labs(
    title = "Average predicted probabilities by actual funding source (4-class)",
    x = "Predicted category",
    y = "Average predicted probability",
    fill = "Actual category"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

selected_vars_4_plot

ggsave(
  filename = "plots/multinom_all_vars_plot.png",
  plot = all_vars_4_plot,
  width = 8,
  height = 6,
  dpi = 300
)

ggsave(
  filename = "plots/multinom_sel_vars_plot.png",
  plot = selected_vars_4_plot,
  width = 8,
  height = 6,
  dpi = 300
)
