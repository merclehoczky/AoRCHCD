
# Load packages -----------------------------------------------------------

library(tidyverse)
library(tidymodels)
library(vip)


# Start Parallelisation ---------------------------------------------------

cores <- parallel::detectCores(logical = FALSE) # Check how many cores you have available
cl <- parallel::makePSOCKcluster(cores) # Create a parallel cluster
doParallel::registerDoParallel(cl) # Register the parallel backend
showConnections() # Shows the connections currently active (should match core numberer)

# Prepare data ------------------------------------------------------------
df <- df %>% 
  # Remove identifier, death and time columns
  select(-subject_id, -hadm_id, -stay_id, 
         -admittime, -dischtime, -intime, -outtime,
         -sepsis3,  -deathtime, 
         -intime_numeric, -deathtime_numeric, -discharge_time_numeric, -death) %>% 
  mutate(time_icu_elapsed = as.numeric(time_icu_elapsed),
         hospital_expire_flag = as.factor(hospital_expire_flag)) # Ensure target variable is a class (binary)

# Check

summary(df)

# Split the dataset -------------------------------------------------------

set.seed(42) # Set seed to easily reproduce results
df_split <- df %>% 
  initial_split(prop = 0.80, strata = hospital_expire_flag)
#hospital_expire_flag outcome
#strata by anticoagulants too!

train_data <- training(df_split)

test_data <- testing(df_split)

set.seed(42) # Set seed to easily reproduce results
train_folds <- vfold_cv(data = train_data, v = 5)

# Data pre-processing -----------------------------------------------------

# Define recipe specification
df_rec <- recipe(hospital_expire_flag ~ ., data = train_data)%>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_dummy(all_factor_predictors(), one_hot = TRUE)

# Specify model type and computational engine -----------------------------

#RF
rf_model <- 
  rand_forest() %>% 
  set_engine("randomForest") %>% 
  set_mode("classification") %>% 
  set_args(mtry = tune(), trees = tune(), min_n = 1, tree_depth = 5)

#GBT
gbt_model <- 
  boost_tree() %>% # Model type: Gradient Boosted Tree 
  set_engine("xgboost") %>% # Computational engine: xgboost
  set_mode("classification") %>%  # Specify model mode
  set_args(mtry = tune(), trees = tune(), learn_rate = tune(), tree_depth = 6, min_n = 1) # Specify model arguments

#NNET
nnet_model <- 
  mlp() %>%
  set_engine("nnet") %>%
  set_mode("classification") %>%
  set_args(hidden_units = tune(), penalty = tune(), epochs = tune())

# Create a regular tune grid ----------------------------------------------

#RF
rf_grid <- grid_regular(range_set(mtry(), c(3,5)),
                        range_set(trees(), c(100,200)))

#GBT
gbt_grid <- grid_regular(range_set(mtry(), c(3,5)),
                         range_set(trees(), c(100,300)),
                         range_set(learn_rate(trans = NULL), c(0.05, 0.5)),
                         levels = 4)
#NNET
nnet_grid <- grid_regular(range_set(hidden_units(), c(1,10)),
                          range_set(penalty(trans = NULL), c(0, 3)),
                          range_set(epochs(), c(100,1000)),
                          levels = 5)

# Create model workflow ---------------------------------------------------
#RF
rf_wflow <-  
  workflow() %>% 
  add_model(rf_model) %>% 
  add_recipe(df_rec)

#GBT
gbt_wflow <- 
  workflow() %>%  #container object
  add_model(gbt_model) %>% 
  add_recipe(df_rec)

#NNET
nnet_wflow <- 
  workflow() %>% 
  add_model(nnet_model) %>% 
  add_recipe(df_rec)


# Analyse resamples with hyperparameter tuning ----------------------------

set.seed(42)
#RF
system.time({
rf_res <- 
  rf_wflow %>% 
  tune_grid(resamples = train_folds, grid = rf_grid)
})

#GBT
system.time({
  gbt_res <- 
    gbt_wflow %>% 
    tune_grid(resamples = train_folds, grid = gbt_grid, control = control_grid(verbose = TRUE))
})

#NNET
system.time({
nnet_res <- 
  nnet_wflow %>% 
  tune_grid(resamples = train_folds, grid = nnet_grid)
})


# Collect metrics and compare ---------------------------------------------
#RF
rf_metrics <- collect_metrics(rf_res)
rf_autoplot <- autoplot(rf_res)

rf_plot <- rf_res %>%
  collect_metrics() %>%
  mutate(mtry = factor(mtry)) %>%
  ggplot(aes(trees, mean, color = mtry)) +
  geom_line(size = 1.5, alpha = 0.6) +
  geom_point(size = 2) +
  facet_wrap(~ .metric, scales = "free", nrow = 2) +
  scale_x_log10(labels = scales::label_number()) +
  scale_color_viridis_d(option = "plasma", begin = .9, end = 0)

#GBT
gbt_metrics <- collect_metrics(gbt_res)
gbt_autoplot <- autoplot(gbt_res)


gbt_plot <- gbt_res %>%
  collect_metrics() %>%
  mutate(learn_rate = factor(learn_rate)) %>%
  ggplot(aes(trees, mean, color = learn_rate)) +
  geom_line(size = 1.5, alpha = 0.6) +
  geom_point(size = 2) +
  facet_wrap(~ .metric, scales = "free", nrow = 2) +
  scale_x_log10(labels = scales::label_number()) +
  scale_color_viridis_d(option = "plasma", begin = .9, end = 0)

#NNET
#collect_metrics(nnet_res)
#autoplot(nnet_res)
nnet_plot <- nnet_res %>%
  collect_metrics() %>%
  mutate(penalty = factor(penalty)) %>%
  ggplot(aes(hidden_units, mean, color = penalty)) +
  geom_line(size = 1.5, alpha = 0.6) +
  geom_point(size = 2) +
  facet_wrap(~ .metric, scales = "free", nrow = 2) +
  scale_x_log10(labels = scales::label_number()) +
  scale_color_viridis_d(option = "plasma", begin = .9, end = 0)

# Select best tuning parameters -------------------------------------------
#RF
show_best(x = rf_res, metric = "accuracy")
select_best(x = rf_res, metric = "accuracy")

#GBT
show_best(x = gbt_res, metric = "accuracy")
select_best(x = gbt_res, metric = "accuracy")

#NNET
#show_best(x = nnet_res, metric = "accuracy")
#select_best(x = nnet_res, metric = "accuracy")

# Finalise model workflow -------------------------------------------------
# RF
best_rf <- select_best(x = rf_res, metric = "accuracy")

final_wf_rf <- 
  rf_wflow %>% 
  finalize_workflow(best_rf)

# GBT
best_gbt <- select_best(x = gbt_res, metric = "accuracy")

final_wf_gbt <- 
  gbt_wflow %>% 
  finalize_workflow(best_gbt)

#NNET
best_nnet <- select_best(x = nnet_res, metric = "accuracy")

final_wf_nnet <- 
  nnet_wflow %>% 
  finalize_workflow(best_nnet)

# Fit ---------------------------------------------------------
#RF
rf_fit <- fit(final_wf_rf, train_data) # Does prep() and fit() in one step 

#GBT
gbt_fit <- fit(final_wf_gbt, train_data) # Does prep() and fit() in one step

#NNET
nnet_fit <- fit(final_wf_nnet, train_data)


# Variable Importance -----------------------------------------------------

vi_df <-  nnet_fit %>% 
  extract_fit_parsnip() %>% # extract the fit object 
  vi(scale = TRUE) #scale the variable importance scores so that the largest is 100

ci_plot <- ggplot(vi_df, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_col(fill = "#0072B2") +
  coord_flip() +
  labs(title = "Variable Importance Plot",
       x = "Variable",
       y = "Importance")


# Predict -----------------------------------------------------------------

#RF

rf_pred_class <- predict(rf_fit, new_data = test_data, type = "class")
rf_pred_prob <- predict(rf_fit, new_data = test_data, type = "prob")

rf_predictions <- test_data %>% 
  select(hospital_expire_flag) %>% # keep target variable (also known as the truth)
  bind_cols(., rf_pred_class, rf_pred_prob)

#GBT

gbt_pred_class <- predict(gbt_fit, new_data = test_data, type = "class")
gbt_pred_prob <- predict(gbt_fit, new_data = test_data, type = "prob")

gbt_predictions <- test_data %>% 
  select(hospital_expire_flag) %>% # keep target variable (also known as the truth)
  bind_cols(., gbt_pred_class, gbt_pred_prob)

#NNET 

nnet_pred_class <- predict(nnet_fit, new_data = test_data, type = "class")
nnet_pred_prob <- predict(nnet_fit, new_data = test_data, type = "prob")

nnet_predictions <- test_data %>% 
  select(hospital_expire_flag) %>%
  bind_cols(., nnet_pred_class, nnet_pred_prob)


# Calculate performance metrics -------------------------------------------
#RF
rf_metrics <- metrics(rf_predictions, truth = hospital_expire_flag, estimate = .pred_class)

#GBT
gbt_metrics <- metrics(gbt_predictions, truth = hospital_expire_flag, estimate = .pred_class)

#NNET
multi_metric <- metric_set(accuracy, sens, spec)
nnet_metrics <- multi_metric(nnet_predictions, truth = hospital_expire_flag, estimate = .pred_class)


# Confusion matrix --------------------------------------------------------
#RF
rf_confusion <- conf_mat(rf_predictions, truth = hospital_expire_flag, estimate = .pred_class)

#GBT
gbt_confusion <- conf_mat(gbt_predictions, truth = hospital_expire_flag, estimate = .pred_class)

#NNET
nnet_confusion <- conf_mat(nnet_predictions, truth = hospital_expire_flag, estimate = .pred_class)

# Receiver Operating Characteristic (ROC) curve ---------------------------

#RF
rf_two_class_curve <- roc_curve(rf_predictions, hospital_expire_flag, .pred_1, event_level = "second")
rf_autoplot <- autoplot(rf_two_class_curve)
rf_rocauc <- roc_auc(rf_predictions, hospital_expire_flag, .pred_1, event_level = "second")

#GBT
gbt_two_class_curve <- roc_curve(gbt_predictions, hospital_expire_flag, .pred_1, event_level = "second")
gbt_autoplot <- autoplot(gbt_two_class_curve)
gbt_rocauc <- roc_auc(gbt_predictions, hospital_expire_flag, .pred_1, event_level = "second")

#NNET
nnet_two_class_curve <- roc_curve(nnet_predictions, hospital_expire_flag, .pred_1, event_level = "second")
nnet_autoplot <- autoplot(nnet_two_class_curve)
nnet_rocauc <- roc_auc(nnet_predictions, hospital_expire_flag, .pred_1, event_level = "second")


# ROCAUC = 1, IS THE NO HEPA ALWAYS DEAD? ---------------------------------

# End Parallelisation -----------------------------------------------------

parallel::stopCluster(cl)
closeAllConnections()
showConnections() # Shows the connections currently active (should match core numberer)
