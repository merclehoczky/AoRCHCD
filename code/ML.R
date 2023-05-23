
# Load packages -----------------------------------------------------------

library(tidyverse)
library(tidymodels)


# Prepare data ------------------------------------------------------------

df <- df %>% 
  select(-hadm_id, -subject_id, -sepsis3, 
         -icu_intime, -icu_outtime, -deathtime) %>% # Remove identifier columns
  mutate(hospital_expire_flag = as.factor(hospital_expire_flag)) # Ensure target variable is a class (binary)

# Split the dataset -------------------------------------------------------

set.seed(42) # Set seed to easily reproduce results
df_split <- df %>% 
  initial_split(prop = 0.80, strata = hospital_expire_flag)
#hospital_expire_flag outcome
#strata by anticoagulants too!

train_data <- training(df_split)

test_data <- testing(df_split)

set.seed(42) # Set seed to easily reproduce results
train_folds <- vfold_cv(data = train_data, v = 10)

# Data pre-processing -----------------------------------------------------

# Define recipe specification
df_rec <- recipe(hospital_expire_flag ~ ., data = train_data)%>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_dummy(all_factor_predictors(), one_hot = TRUE)

# Specify model type and computational engine -----------------------------


dt_model <- 
  decision_tree() %>% 
  set_engine("C5.0") %>% 
  set_mode("classification") %>% 
  set_args(min_n = 1, tree_depth = tune())

rf_model <- 
  rand_forest() %>% 
  set_engine("randomForest") %>% 
  set_mode("classification") %>% 
  set_args(mtry = tune(), trees = tune(), min_n = 1, tree_depth = tune())

gbt_model <- 
  boost_tree() %>% # Model type: Gradient Boosted Tree 
  set_engine("xgboost") %>% # Computational engine: xgboost
  set_mode("classification") %>%  # Specify model mode
  set_args(mtry = tune(), trees = tune(), learn_rate = tune(), tree_depth = 6, min_n = 1) # Specify model arguments

# Create a regular tune grid ----------------------------------------------

df_grid <- grid_regular(range_set(tree_depth(), c(1,15)))

rf_grid <- grid_regular(range_set(mtry(), c(3,5)),
                        range_set(trees(), c(100,200)))

gbt_grid <- grid_regular(range_set(mtry(), c(3,20)),
                         range_set(trees(), c(100,300)),
                         range_set(learn_rate(trans = NULL), c(0.005, 0.1)),
                         levels = 4)

# Create model workflow ---------------------------------------------------

dt_wflow <- 
  workflow() %>% 
  add_model(dt_model) %>% 
  add_recipe(df_rec)

rf_wflow <-  
  workflow() %>% 
  add_model(rf_model) %>% 
  add_recipe(df_rec)


gbt_wflow <- 
  workflow() %>%  #container object
  add_model(gbt_model) %>% 
  add_recipe(df_rec)

# Analyse resamples with hyperparameter tuning ----------------------------

set.seed(42)

dt_res <- 
  dt_wflow %>% 
  fit_resamples(resamples = train_folds)

rf_res <- 
  rf_wflow %>% 
  tune_grid(resamples = train_folds, grid = rf_grid)

system.time({
  set.seed(42)
  gbt_res <- 
    gbt_wflow %>% 
    tune_grid(resamples = train_folds, grid = gbt_grid, control = control_grid(verbose = TRUE))
})




# CONTINUTE HERE ----------------------------------------------------------


# Collect metrics and compare ---------------------------------------------

collect_metrics(rf_res)

rf_res %>%
  collect_metrics() %>%
  mutate(mtry = factor(mtry)) %>%
  ggplot(aes(trees, mean, color = mtry)) +
  geom_line(size = 1.5, alpha = 0.6) +
  geom_point(size = 2) +
  facet_wrap(~ .metric, scales = "free", nrow = 2) +
  scale_x_log10(labels = scales::label_number()) +
  scale_color_viridis_d(option = "plasma", begin = .9, end = 0)

# OR #

autoplot(gbt_res)

# Select best tuning parameters -------------------------------------------

show_best(x = gbt_res, metric = "accuracy")

select_best(x = gbt_res, metric = "accuracy")

# Finalise model workflow -------------------------------------------------

best_gbt <- select_best(x = gbt_res, metric = "accuracy")

final_wf <- 
  gbt_wflow %>% 
  finalize_workflow(best_gbt)

# Fit and predict ---------------------------------------------------------

gbt_fit <- fit(final_wf, train_data) # Does prep() and fit() in one step

gbt_pred_class <- predict(gbt_fit, new_data = test_data, type = "class")

gbt_pred_prob <- predict(gbt_fit, new_data = test_data, type = "prob")

# CONT FROM HERE ----------------------------------------------------------


# Data frame from test set with “attached” the model predictions
predictions <- test_data %>% 
  select(hospital_expire_flag) %>% # keep target variable (also known as the truth)
  bind_cols(., gbt_pred_class, gbt_pred_prob)

# Calculate performance metrics -------------------------------------------

metrics(predictions, truth = hospital_expire_flag, estimate = .pred_class)

# Confusion matrix --------------------------------------------------------

conf_mat(predictions, truth = hospital_expire_flag, estimate = .pred_class)

# Receiver Operating Characteristic (ROC) curve ---------------------------

two_class_curve <- roc_curve(predictions, hospital_expire_flag, .pred_1, event_level = "second")

autoplot(two_class_curve)

roc_auc(predictions, hospital_expire_flag, .pred_1, event_level = "second")

# ROCAUC = 1, IS THE NO HEPA ALWAYS DEAD? ---------------------------------


