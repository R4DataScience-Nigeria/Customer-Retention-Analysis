# Load necessary libraries
library(tidymodels)
library(baguette)
library(xgboost)
library(finetune)
library(bundle)
library(readr)

# Load and preprocess data
churn_df <- read_rds("Team A/clean_churn.rds") %>%
  select(-c(credit_val, x21)) %>%
  rename(churn_status = "churn")

# Reduce dataset size if needed
set.seed(1)
churn_df <- churn_df %>% sample_n(50000)  # Keep 50,000 rows for efficiency

# Split data
set.seed(1)
churn_split <- initial_split(churn_df)
churn_train <- training(churn_split)
churn_test <- testing(churn_split)

# Reduce CV folds from 10 to 3
churn_folds <- vfold_cv(churn_train, v = 3)

# Feature engineering recipe
recipe_mixed <- recipe(churn_status ~ ., data = churn_train) %>%
  step_rm(acct_id) %>%  # Remove unique ID
  step_corr(all_numeric_predictors(), threshold = 0.9) %>%  # Remove highly correlated variables
  step_nzv(all_predictors()) %>%  # Remove near-zero variance predictors
  step_YeoJohnson(all_numeric_predictors()) %>%  
  step_normalize(all_numeric_predictors()) %>%
  step_dummy(all_nominal_predictors(), -all_outcomes()) %>%
  step_zv(all_predictors())  # Remove zero-variance predictors

# Choose two models (logistic regression and XGBoost)
spec_lr <- logistic_reg() %>%
  set_mode("classification") %>%
  set_engine("glm")

spec_xgb <- boost_tree(trees = 50, min_n = tune(), tree_depth = tune(),
                       learn_rate = 0.1) %>%
  set_engine("xgboost") %>%
  set_mode("classification")

# Performance metrics
churn_metrics <- metric_set(roc_auc, accuracy)

# Workflow set with fewer models
wf_set <- workflow_set(
  preproc = list(mixed = recipe_mixed),
  models = list(
    logistic_reg = spec_lr,
    boost_tree = spec_xgb
  )
)

# Define control settings (without grid argument)
ctrl <- control_grid(save_pred = TRUE, parallel_over = "everything")

# Train models with a smaller grid (grid = 5)
wf_set_fit <- workflow_map(
  wf_set, 
  fn = "tune_grid", 
  verbose = TRUE, 
  seed = 1,
  resamples = churn_folds, 
  metrics = churn_metrics,
  grid = 5,  # Specify grid size here, not in control_grid
  control = ctrl
)

save(wf_set_fit, file = "wf_set_fit.Rda")

wf_set_fit <-
  wf_set_fit[
    map_lgl(wf_set_fit$result, 
            ~pluck(., ".metrics", 1) %>% inherits("tbl_df"), 
            "tune_results"),
  ]

# First look at metrics:
metrics_wf_set <- collect_metrics(wf_set_fit, summarize = FALSE)

metrics_wf_set %>%
  filter(.metric == "roc_auc") %>%
  arrange(desc(.estimate))

save(metrics_wf_set, file = "metrics_wf_set.Rda")

# Extract best XGBoost model
xgb_res <- extract_workflow_set_result(wf_set_fit, "mixed_boost_tree")

xgb_wflow <-
  workflow() %>%
  add_recipe(recipe_mixed) %>%
  add_model(spec_xgb)

# Simulated annealing tuning
xgb_sim_anneal_fit <-
  tune_sim_anneal(
    object = xgb_wflow,
    resamples = churn_folds,
    iter = 25,
    metrics = churn_metrics,
    initial = xgb_res,
    control = control_sim_anneal(verbose = TRUE, parallel_over = "everything")
  )

save(xgb_sim_anneal_fit, file = "xgb_sim_anneal_fit.Rda")

metrics_xgb <- collect_metrics(xgb_sim_anneal_fit, summarize = FALSE)

save(metrics_xgb, file = "metrics_xgb.Rda")

# Final model fit and evaluation against test set
xgb_final_fit <-
  last_fit(
    finalize_workflow(xgb_wflow, select_best(xgb_sim_anneal_fit, metric = "roc_auc")),
    churn_split
  )

save(xgb_final_fit, file = "xgb_final_fit.Rda")
final_fit <- bundle(xgb_final_fit$.workflow[[1]])
save(final_fit, file = "final_fit.Rda")
## Deploying to Connect

#From here, all we need to do to deploy our fitted model is pass it off to vetiver for deployment to Posit Connect:
#final_fit_vetiver <- vetiver_model(final_fit, "mutagen")
#board <- board_connect()
#vetiver_pin_write(board, final_fit_vetiver)
#vetiver_deploy_rsconnect(board, "simon.couch/mutagen")

