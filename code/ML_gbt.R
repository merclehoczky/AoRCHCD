
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
  step_dummy(all_factor_predictors(), one_hot = TRUE)

# Specify model type and computational engine -----------------------------


#GBT
gbt_model <- 
  boost_tree() %>% # Model type: Gradient Boosted Tree 
  set_engine("xgboost") %>% # Computational engine: xgboost
  set_mode("classification") %>%  # Specify model mode
  set_args(mtry = tune(), trees = tune(), learn_rate = tune(), tree_depth = 6, min_n = 1) # Specify model arguments


# Create a regular tune grid ----------------------------------------------

#GBT
gbt_grid <- grid_regular(range_set(mtry(), c(3,5)),
                         range_set(trees(), c(100,300)),
                         range_set(learn_rate(trans = NULL), c(0.05, 0.5)),
                         levels = 4)

# Create model workflow ---------------------------------------------------

#GBT
gbt_wflow <- 
  workflow() %>%  #container object
  add_model(gbt_model) %>% 
  add_recipe(df_rec)


# Analyse resamples with hyperparameter tuning ----------------------------

set.seed(42)

#GBT
system.time({
  gbt_res <- 
    gbt_wflow %>% 
    tune_grid(resamples = train_folds, grid = gbt_grid, control = control_grid(verbose = TRUE))
})


# Collect metrics and compare ---------------------------------------------

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

ggsave("figures/Metrics GBT.png")
# Select best tuning parameters -------------------------------------------

#GBT
show_best(x = gbt_res, metric = "accuracy")
select_best(x = gbt_res, metric = "accuracy")

# Finalise model workflow -------------------------------------------------

# GBT
best_gbt <- select_best(x = gbt_res, metric = "accuracy")

final_wf_gbt <- 
  gbt_wflow %>% 
  finalize_workflow(best_gbt)

# Fit ---------------------------------------------------------

#GBT
gbt_fit <- fit(final_wf_gbt, train_data) # Does prep() and fit() in one step

# Variable Importance -----------------------------------------------------

vi_df <-  gbt_fit %>% 
  extract_fit_parsnip() %>% # extract the fit object 
  vi(scale = TRUE) #scale the variable importance scores so that the largest is 100

ci_plot <- ggplot(vi_df, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_col(fill = "#0072B2") +
  coord_flip() +
  labs(title = "Variable Importance Plot",
       x = "Variable",
       y = "Importance")

ggsave("figures/Variable importance GBT.png")
# Predict -----------------------------------------------------------------

#GBT

gbt_pred_class <- predict(gbt_fit, new_data = test_data, type = "class")
gbt_pred_prob <- predict(gbt_fit, new_data = test_data, type = "prob")

gbt_predictions <- test_data %>% 
  select(hospital_expire_flag) %>% # keep target variable (also known as the truth)
  bind_cols(., gbt_pred_class, gbt_pred_prob)

# Calculate performance metrics -------------------------------------------

#GBT
gbt_metrics <- metrics(gbt_predictions, truth = hospital_expire_flag, estimate = .pred_class)

# Confusion matrix --------------------------------------------------------

#GBT
gbt_confusion <- conf_mat(gbt_predictions, truth = hospital_expire_flag, estimate = .pred_class)


# Receiver Operating Characteristic (ROC) curve ---------------------------

#GBT
gbt_two_class_curve <- roc_curve(gbt_predictions, hospital_expire_flag, .pred_1, event_level = "second")
gbt_autoplot <- autoplot(gbt_two_class_curve)
gbt_rocauc <- roc_auc(gbt_predictions, hospital_expire_flag, .pred_1, event_level = "second")



# ROCAUC = 1, IS THE NO HEPA ALWAYS DEAD? ---------------------------------

# End Parallelisation -----------------------------------------------------

parallel::stopCluster(cl)
closeAllConnections()
showConnections() # Shows the connections currently active (should match core numberer)
