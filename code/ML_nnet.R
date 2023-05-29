
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
  select(-admittime, -dischtime, -icu_intime, -icu_outtime, -deathtime, 
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
train_folds <- vfold_cv(data = train_data, v = 10, strata = hospital_expire_flag)

# Data pre-processing -----------------------------------------------------

# Define recipe specification
df_rec <- recipe(hospital_expire_flag ~ ., data = train_data)%>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_dummy(all_factor_predictors(), one_hot = TRUE) %>% 
  step_nzv()

# Specify model type and computational engine -----------------------------


#NNET
nnet_model <- 
  mlp() %>%
  set_engine("nnet") %>%
  set_mode("classification") %>%
  set_args(hidden_units = tune(), penalty = tune(), epochs = tune())

# Create a regular tune grid ----------------------------------------------

#NNET
nnet_grid <- grid_regular(range_set(hidden_units(), c(1,10)),
                          range_set(penalty(trans = NULL), c(0, 3)),
                          range_set(epochs(), c(100,1000)),
                          levels = 5)

# Create model workflow ---------------------------------------------------

#NNET
nnet_wflow <- 
  workflow() %>% 
  add_model(nnet_model) %>% 
  add_recipe(df_rec)


# Analyse resamples with hyperparameter tuning ----------------------------

set.seed(42)


#NNET
system.time({
  nnet_res <- 
    nnet_wflow %>% 
    tune_grid(resamples = train_folds, grid = nnet_grid)
})


# Collect metrics and compare ---------------------------------------------

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

ggsave("figures/Metrics NNet.png")
# Select best tuning parameters -------------------------------------------

#NNET
#show_best(x = nnet_res, metric = "accuracy")
#select_best(x = nnet_res, metric = "accuracy")

# Finalise model workflow -------------------------------------------------

#NNET
best_nnet <- select_best(x = nnet_res, metric = "accuracy")

final_wf_nnet <- 
  nnet_wflow %>% 
  finalize_workflow(best_nnet)

# Fit ---------------------------------------------------------

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

ggsave("figures/Variable importance NNet.png")
# Predict -----------------------------------------------------------------

#NNET 

nnet_pred_class <- predict(nnet_fit, new_data = test_data, type = "class")
nnet_pred_prob <- predict(nnet_fit, new_data = test_data, type = "prob")

nnet_predictions <- test_data %>% 
  select(hospital_expire_flag) %>%
  bind_cols(., nnet_pred_class, nnet_pred_prob)


# Calculate performance metrics -------------------------------------------

#NNET
multi_metric <- metric_set(accuracy, sens, spec)
nnet_metrics <- multi_metric(nnet_predictions, truth = hospital_expire_flag, estimate = .pred_class)


# Confusion matrix --------------------------------------------------------

#NNET
nnet_confusion <- conf_mat(nnet_predictions, truth = hospital_expire_flag, estimate = .pred_class)

# Receiver Operating Characteristic (ROC) curve ---------------------------

#NNET
nnet_two_class_curve <- roc_curve(nnet_predictions, hospital_expire_flag, .pred_1, event_level = "second")
nnet_autoplot <- autoplot(nnet_two_class_curve)
nnet_rocauc <- roc_auc(nnet_predictions, hospital_expire_flag, .pred_1, event_level = "second")


# ROCAUC = 1, IS THE NO HEPA ALWAYS DEAD? ---------------------------------

# End Parallelisation -----------------------------------------------------

parallel::stopCluster(cl)
closeAllConnections()
showConnections() # Shows the connections currently active (should match core numberer)
