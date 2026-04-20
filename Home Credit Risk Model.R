# R
# Final Kaggle Model - LightGBM only
# Minimal, self-contained pipeline: read data, preprocess, fit LightGBM, evaluate, write submission

library(tidyverse)
library(tidymodels)

# ----- CONFIG -----
data_path <- "C:/Users/Kayli/Downloads"
train_file <- "C:/Users/Kayli/Downloads/application_train.csv (1).zip"
test_file  <- "C:/Users/Kayli/Downloads/application_test.csv (1).zip"
submission_file <- "C:/Users/Kayli/Downloads/kaggle_submission_v1.csv"
set.seed(123)

# ----- READ DATA -----
train_full <- readr::read_csv(train_file, show_col_types = FALSE)
test_full  <- readr::read_csv(test_file,  show_col_types = FALSE)

# Ensure TARGET is a factor with event = "default"
if ("TARGET" %in% names(train_full)) {
  if (!is.factor(train_full$TARGET)) {
    if (all(train_full$TARGET %in% c(0, 1))) {
      train_full <- train_full |> mutate(TARGET = factor(TARGET, levels = c(0, 1), labels = c("no_default", "default")))
    } else {
      train_full <- train_full |> mutate(TARGET = as.factor(TARGET))
    }
  }
}

# ----- TRAIN/VALID SPLIT -----
split <- initial_split(train_full, prop = 0.8, strata = TARGET)
train_data <- training(split)
val_data   <- testing(split)

# ----- RECIPE -----
base_recipe <- recipe(TARGET ~ ., data = train_data) |>
  step_rm(any_of(c("SK_ID_CURR", "ID", "Id"))) |>
  step_zv(all_predictors()) |>
  step_impute_median(all_numeric_predictors()) |>
  step_other(all_nominal_predictors(), threshold = 0.01) |>
  step_dummy(all_nominal_predictors(), one_hot = TRUE) |>
  step_normalize(all_numeric_predictors())

# ----- LIGHTGBM SPEC (final) -----
lgbm_spec <- boost_tree(
  trees = 2000,
  tree_depth = 6,
  learn_rate = 0.03,
  mtry = 30,
  loss_reduction = 0,
  sample_size = 0.8
) |>
  set_engine("lightgbm", objective = "binary", verbosity = -1) |>
  set_mode("classification")

# ----- WORKFLOW & FIT -----
final_wf <- workflow() |>
  add_recipe(base_recipe) |>
  add_model(lgbm_spec)

final_fit <- final_wf |> fit(data = train_data)

# ----- VALIDATION METRICS -----
val_probs <- predict(final_fit, val_data, type = "prob")
val_class <- predict(final_fit, val_data, type = "class")

val_results <- val_probs |>
  bind_cols(val_class) |>
  bind_cols(val_data |> select(TARGET))

val_auc <- roc_auc(val_results, truth = TARGET, .pred_default, event_level = "second")
val_acc <- accuracy(val_results, truth = TARGET, .pred_class)

# ----- FINAL PREDICTIONS & SUBMISSION -----
test_probs <- predict(final_fit, test_full, type = "prob")

submission <- tibble(
  SK_ID_CURR = test_full$SK_ID_CURR,
  TARGET = test_probs$.pred_default
)

readr::write_csv(submission, submission_file)

# ----- OUTPUT OBJECT -----
results <- list(
  final_workflow = final_wf,
  final_fit = final_fit,
  val_auc = val_auc,
  val_accuracy = val_acc,
  submission_path = submission_file,
  submission = submission
)

invisible(results)