library(mice)
library(tidymodels)
library(ranger)
library(future)
library(tidyverse)

# Set the WD to the project dir
setwd("~/Local Documents/GitHub/epinic_asah_antidepressants/")
source("source/functions.R")

# Read the imputed data and create 5 imputed datasets
imp_epinic <- complete(readr::read_rds("data/imp_data.rds"), action = "all")

for (i in 1:5) {
  message("Starting iteration ", i)
  
  tryCatch(
    {
      setwd("~/Local Documents/GitHub/epinic_asah_antidepressants/")
      
      # Read the data
      df <- imp_epinic[[i]] %>% 
        mutate(grp = forcats::fct('epinic')) %>%
        filter(yr >2011) %>%
        select(-any_imv, -yr) %>%
        full_join(readr::read_rds("data/df_opic.rds") %>% 
                    mutate(grp = forcats::fct("opic")) %>%
                    filter(yr > 2011) %>%
                    select (-yr)
        ) %>%
        mutate(across(where(is.logical), as.factor),
               CONT_ICU_LOS_DAYS = fct_collapse(CONT_ICU_LOS_DAYS,
                  "0-1 days" = "[0,1]",
                  "1-4 days" = "(1,4]",
                  "4-11 days" = "(4,11]",
                  ">11 days" = c("(11,80.1]","(11,318]")),
               edu = factor(edu, ordered = TRUE),
               SAPS_consciousness_level = factor(SAPS_consciousness_level, ordered = TRUE))
      
      # Set the working directory to data for saving plots and data
      dir_path <- paste0("data/imp_", i)
      dir.create(dir_path, showWarnings = FALSE, recursive = TRUE)
      setwd(dir_path)
      
      # Plan on how to resolve futures
      if (supportsMulticore()) {
        plan(multicore)
      } else {
        plan(multisession)
      }
      
      # Set a seed
      set.seed(535)
      
      # Split the data in 80/20-proportions using rsample
      data_split <- initial_split(df[, which(names(df) != "ad_treatment")], prop = 0.8)
      train_data <- training(data_split)
      test_data <- testing(data_split)
      
      # Define the Random Forest model using the ranger-engine with full support for
      # parallellization using all cores
      rf_model <- parsnip::rand_forest(mode = "classification", trees = tune(), min_n = tune(), mtry = tune()) %>%
        set_engine("ranger", num.threads = availableCores(constraints = "multicore"))
      
      # Create the recipe using all covariates except the outcome variable
      rf_recipe <- recipe(grp ~ ., data = train_data) %>%
        step_normalize(all_numeric_predictors()) %>%
        step_unknown(all_nominal_predictors()) %>%
        step_dummy(all_nominal_predictors()) %>%
        step_zv(all_predictors())
      
      # Create the workflow
      rf_workflow <- workflow() %>%
        add_model(rf_model) %>%
        add_recipe(rf_recipe)
      
      # Create a grid of hyperparameters ################
      rf_grid <- grid_space_filling(parameters(
        trees(range = c(100, 1000)),
        mtry(range = c(1, ncol(train_data) - 1)),
        min_n(range = c(1, 10))),
        size = 20,
        type = "latin_hypercube"
      )
      
      # Set up 5-fold CV on the train data #################
      cv_folds <- vfold_cv(train_data, v = 5)
      
      # Tune the model hyperparameters ----
      message("Beginning to tune hyperparameters")
      
      tune_results <- rf_workflow %>%
        tune_grid(
          resamples = cv_folds,
          grid = rf_grid,
          metrics = metric_set(roc_auc),
          control = control_grid(save_pred = TRUE)
        )
      
      autoplot_tune_results <- autoplot(tune_results)
      
      best_params <- tune_results %>%
        select_best(metric = "roc_auc")
      
      message("Finished  tuning, proceeding to finalize model")
      final_rf_model <- finalize_model(rf_model, best_params)
      
      final_rf_workflow <- workflow() %>%
        add_model(final_rf_model) %>%
        add_recipe(rf_recipe)
      
      # Fit the model to the training data
      rf_fit <- final_rf_workflow %>%
        fit(data = train_data)
      
      # Perform 10-fold CV for the final model ----
      # Fit the model
      
      message("Fitting model to bootstrapped resamples")
      
      cv_results <- final_rf_workflow %>%
        fit_resamples(resamples = cv_folds, control = control_resamples(save_pred = TRUE))
      
      # Evaluate cross-validation results
      cv_metrics <- cv_results %>%
        collect_metrics() %>%
        ggplot(aes(x = .metric, y = mean, fill = .metric)) +
        geom_boxplot() +
        theme_minimal() +
        labs(title = "Cross-Validation Performance Metrics",
             x = "Metric",
             y = "Value")
      
      # Collect the CV-predictions
      cv_predictions <- cv_results %>%
        collect_predictions() %>%
        mutate(.pred_class = factor(if_else(.pred_epinic >0.5, "epinic", "opic")))
      
      # Plot ROC curves
      cv_roc_curves <- cv_predictions %>%
        group_by(id) %>%
        roc_curve(truth = grp, .pred_epinic)
      
      autoplot_cv_roc_curves <- autoplot(cv_roc_curves) +
        labs(title = "ROC Curves for Cross-Validation Folds")
      
      # Calculate confusion matrices
      confusion_matrices <- cv_predictions %>%
        group_by(id) %>%
        conf_mat(truth = grp, estimate = .pred_class)
      
      # Visualize confusion matrices
      cowplots <- confusion_matrices %>%
        mutate(plot = map(conf_mat, ~ autoplot(.x, type = "heatmap"))) %>%
        pull(plot)
      
      grid_cowplot <- cowplot::plot_grid(plotlist = cowplots, ncol = 2)
      
      # Test the model on the 20% holdout ----
      
      message("Testing model on holdout")
      
      test_predictions <- predict(rf_fit, new_data = test_data, type = "prob") %>%
        mutate(.pred_class = factor(if_else(.pred_epinic >0.5, "epinic", "opic")))
      
      test_res <- test_data %>%
        bind_cols(test_predictions)
      
      accuracy <- test_res %>%
        accuracy(truth = grp, estimate = .pred_class)
      
      auroc <- test_res %>%
        roc_auc(truth = grp, .pred_epinic)

      brier <- test_res %>%
        brier_class(truth = grp, .pred_epinic)
      
      roc_curve <- test_res %>%
        roc_curve(truth = grp, .pred_epinic) %>%
        autoplot()
      
      confusion_matrix <- test_res %>%
        conf_mat(truth = grp, estimate = .pred_class)
      
      # Run the model on the full data set to predict PS ----
      
      message("Predicting PS")
      
      propensity_scores <- predict(rf_fit, new_data = df[,which(names(df) != "ad_treatment")], type = "prob") %>% 
        pull(.pred_epinic)
      
      # Add propensity scores to dataset
      df_ps <- df %>%
        mutate(propensity_score = propensity_scores,
               grp = if_else(grp == "epinic", 1, 0))
      
      message("Saving ..........")
      
      # Save the dataframe including propensity scores to disk along with other diagnostics ----
      readr::write_rds(df_ps, "df_ps_randforest.rds")
      readr::write_rds(accuracy, "model_accuracy.rds")
      readr::write_rds(auroc, "model_auroc.rds")
      readr::write_rds(brier, "model_brier.rds")
      readr::write_rds(roc_curve, "final_roc_curve.rds")
      readr::write_rds(confusion_matrix$table, "confusion_matrix.rds")
      ggplot2::ggsave("tune_results.png", plot = autoplot_tune_results)
      ggplot2::ggsave("cv_metrics.png", plot = cv_metrics)
      ggplot2::ggsave("cv_confusion_matrices.png", plot = grid_cowplot)
      ggplot2::ggsave("cv_roc_curves.png", plot = autoplot_cv_roc_curves)
      ggplot2::ggsave("final_roc_curve.png", plot = roc_curve)
      save.image("rand_forest_environ.RData")
      
      message("Iteration ", i, " completed successfully.")
      
    },
    error = function(e) {
      message("Error in iteration ", i, ": ", conditionMessage(e))}
  )
}

# Pool metrics across imputations

setwd("~/Local Documents/GitHub/epinic_asah_antidepressants/")

# Create a list to store metrics across imputations
metrics_list <- list()

for (i in 1:5) {
  # Load saved metrics from each imputation
  acc <- readr::read_rds(paste0("data/imp_", i, "/model_accuracy.rds"))
  auc <- readr::read_rds(paste0("data/imp_", i, "/model_auroc.rds"))
  brc <- readr::read_rds(paste0("data/imp_", i, "/model_brier.rds"))
  
  metrics_list[[i]] <- tibble(
    Imputation = paste("Imputation", i),
    Accuracy = acc$.estimate,
    AUROC = auc$.estimate,
    Brier = brc$.estimate
  )
}

# Combine all metrics
df_metrics <- bind_rows(metrics_list)

# Save summary table
readr::write_rds(df_metrics, "data/model_performance_summary.rds")

# Reshape for plotting
df_long <- df_metrics %>%
  pivot_longer(cols = c(Accuracy, AUROC, Brier), names_to = "Metric", values_to = "Score")

# Boxplot of metrics
bp_metrics <- ggplot(df_long, aes(x = Metric, y = Score, fill = Metric)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  theme_minimal() +
  labs(title = "Model Performance Metrics Across Imputations",
       x = "Metric", y = "Score") +
  scale_fill_brewer(palette = "Set2") +
  coord_cartesian(ylim = c(0,1))

readr::write_rds(bp_metrics, "data/bp_model_performance_summary.rds")

