# situating --------------------------------------------------------------------
library(tidymodels)
library(tidyverse)
library(baguette)
library(bundle)
library(doMC)
library(finetune)
library(here)
library(xgboost)
registerDoMC(cores = max(1, parallelly::availableCores() - 1))


# data setup -------------------------------------------------------------------

churn_df <- read_rds(here("Team A", "clean_churn.rds")) |>
                        select(-c(credit_val,x21)) |>
                        rename(churn_status = "churn")

glimpse(churn_df)

missing_values <- churn_df %>%
  summarise_all(~ sum(is.na(.)))

# Print the columns with missing values and their counts
missing_values[missing_values > 0]


set.seed(1)
churn_split <- initial_split(churn_df)
churn_train <- training(churn_split)
churn_test <- testing(churn_split)

set.seed(1)
churn_folds <- vfold_cv(churn_train)

# strategy: define basic + toxicosph.. recipes + some model specs, tune w workflowsets
# pursue best model further with iterative tuning
# preprocessors --------------------
recipe_basic <-
  recipe(churn_status ~ ., churn_train) %>%
  step_nzv(all_predictors())

recipe_normalize <-
  recipe_basic %>%
  step_YeoJohnson(all_double_predictors()) %>%
  step_normalize(all_double_predictors())

data_recipe <- recipe(~ ., churn_train) %>%
  step_dummy(all_nominal_predictors(), -all_outcomes()) %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_numeric_predictors())

# model specifications -------------
spec_lr <-
  logistic_reg() %>%
  set_mode("classification")

spec_bm <- 
  bag_mars(num_terms = tune(), prod_degree = tune()) %>%
  set_engine("earth") %>% 
  set_mode("classification")

spec_bt <- 
  bag_tree(cost_complexity = tune(), tree_depth = tune(), min_n = tune()) %>%
  set_engine("rpart") %>%
  set_mode("classification")

spec_nn <- 
  mlp(hidden_units = tune(), penalty = tune(), epochs = tune()) %>%
  set_engine("nnet", MaxNWts = 15000) %>%
  set_mode("classification")

spec_svm <- 
  svm_rbf(cost = tune(), rbf_sigma = tune(), margin = tune()) %>%
  set_mode("classification")

spec_xgb <-
  boost_tree(trees = tune(), min_n = tune(), tree_depth = tune(),
             learn_rate = tune(), stop_iter = 10) %>%
  set_engine("xgboost") %>%
  set_mode("classification")

# metrics ------
churn_metrics <- metric_set(roc_auc, accuracy, kap, mcc)

# combine into a workflow set -------
wf_set <-
  workflow_set(
    preproc = list(basic = recipe_basic),
    models = list(boost_tree = spec_xgb, logistic_reg = spec_lr)
  ) %>%
  bind_rows(
    workflow_set(
      preproc = list(normalize = recipe_normalize),
      models = list(
        bag_tree = spec_bt,
        bag_mars = spec_bm,
        svm_rbf = spec_svm,
        mlp = spec_nn
      )
    )
  ) %>%
  bind_rows( 
    workflow_set(
      preproc = list(churn = data_recipe),
      models = list(
        bag_tree = spec_bt,
        bag_mars = spec_bm,
        svm_rbf = spec_svm,
        mlp = spec_nn
      )
    )
  )

wf_set

wf_set_fit <-
  workflow_map(
    wf_set, 
    fn = "tune_grid", 
    verbose = TRUE, 
    seed = 1,
    resamples = churn_folds,
    metrics = churn_metrics ,
    control = control_grid(parallel_over = "everything")
  )

save(wf_set_fit, file = "data-raw/wf_set_fit.Rda")

wf_set_fit <-
  wf_set_fit[
    map_lgl(wf_set_fit$result, 
            ~pluck(., ".metrics", 1) %>% inherits("tbl_df"), 
            "tune_results"),
  ]

# first look at metrics:
metrics_wf_set <- collect_metrics(wf_set_fit, summarize = FALSE)

metrics_wf_set %>%
  filter(.metric == "roc_auc") %>%
  arrange(desc(.estimate))

save(metrics_wf_set, file = "data/metrics_wf_set.Rda")

# all of the xgboost models with no preprocessing were considerably
# more performant than the other proposed models. let's extract those
# results and use them as initial results in simulated annealing:
xgb_res <- extract_workflow_set_result(wf_set_fit, "basic_boost_tree")

xgb_wflow <-
  workflow() %>%
  add_recipe(recipe_basic) %>%
  add_model(spec_xgb)

xgb_sim_anneal_fit <-
  tune_sim_anneal(
    object = xgb_wflow,
    resamples = mutagen_folds,
    iter = 25,
    metrics = mutagen_metrics,
    initial = xgb_res,
    control = control_sim_anneal(verbose = TRUE, parallel_over = "everything")
  )

save(xgb_sim_anneal_fit, file = "data-raw/xgb_sim_anneal_fit.Rda")

metrics_xgb <- collect_metrics(xgb_sim_anneal_fit, summarize = FALSE)

save(metrics_xgb, file = "data/metrics_xgb.Rda")

# final model fit and evaluation against test set ----------
xgb_final_fit <-
  last_fit(
    finalize_workflow(xgb_wflow, select_best(xgb_sim_anneal_fit, metric = "roc_auc")),
    mutagen_split
  )

save(xgb_final_fit, file = "data/xgb_final_fit.Rda")

final_fit <- bundle(xgb_final_fit$.workflow[[1]])

save(final_fit, file = "data/final_fit.Rda")