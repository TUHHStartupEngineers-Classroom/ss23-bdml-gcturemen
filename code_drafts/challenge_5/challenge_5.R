# Load the libraries ----

library(tidyverse)
library(readxl)
library(rsample)
library(h2o)
library(recipes)
library(cowplot)
library(glue)

# Challenge 4 ----

product_backorders_tbl <- read_csv("~/GitHub/ss23-bdml-gcturemen/code_drafts/data/product_backorders.csv")

set.seed(seed = 777)
split_obj <- initial_split(product_backorders_tbl, prop = 0.80)

train_set_tbl <- training(split_obj)
test_set_tbl  <- testing(split_obj)

recipe_obj <- recipe(went_on_backorder ~., data = train_set_tbl) %>% 
  step_zv(all_predictors()) %>% 
  step_mutate_at(potential_issue,
                 deck_risk,
                 oe_constraint,
                 ppap_risk,
                 stop_auto_buy,
                 rev_stop,
                 went_on_backorder,
                 fn = as.factor) %>% 
  prep()

train_tbl <- bake(recipe_obj, new_data = train_set_tbl)
test_tbl  <- bake(recipe_obj, new_data = test_set_tbl)

# Specify the response and predictor variables

h2o.init()

split_h2o <- h2o.splitFrame(as.h2o(train_tbl), ratios = c(0.80), seed = 767)
train_h2o <- split_h2o[[1]]
valid_h2o <- split_h2o[[2]]
test_h2o  <- as.h2o(test_tbl)

y <- "went_on_backorder"
x <- setdiff(names(train_h2o), y)

# Run AutoML specifying the stopping criterion

automl_models_h2o <- h2o.automl(
  x = x,
  y = y,
  training_frame    = train_h2o,
  validation_frame  = valid_h2o,
  leaderboard_frame = test_h2o,
  max_runtime_secs  = 30,
  nfolds            = 5 
)

# View the leaderboard

automl_models_h2o@leaderboard

# Leaderboard visualization ----

model_leaderboard <- automl_models_h2o@leaderboard %>%
  as_tibble() %>%
  select(-c(aucpr, mean_per_class_error, rmse, mse)) %>%
  mutate(model_type = str_extract(model_id, "[^_]+")) %>%
  rownames_to_column(var = "row_name") %>%
  mutate(model_id   = as_factor(model_id) %>% reorder(auc), 
         model_type = as.factor(model_type)
  ) %>% 
  pivot_longer(cols = -c(model_id, model_type, row_name), 
               names_to = "key", 
               values_to = "value", 
               names_transform = list(key = forcats::fct_inorder)
  ) %>% 
  mutate(model_id = paste0(row_name, ". ", model_id) %>% as_factor() %>% fct_rev())

model_leaderboard %>%
  ggplot(aes(value, model_id, color = model_type)) +
  geom_point(size = 3) +
  geom_label(aes(label = round(value, 2), hjust = "inward")) +
  
  # Facet to break out logloss and auc
  facet_wrap(~ key, scales = "free_x") +
  labs(title = "Leaderboard Metrics",
       subtitle = paste0("Ordered by: ", "auc"),
       y = "Model Position, Model ID", x = "") + 
  theme(legend.position = "bottom")

# Tune a model with grid search ----

predictor_model <- h2o.getModel("GLM_1_AutoML_3_20230517_224632")

h2o.performance(predictor_model, newdata = as.h2o(test_tbl))

predictor_model_grid <- h2o.grid(
  algorithm = "glm",
  grid_id = "predictor_model_grid",
  x = x,
  y = y,
  training_frame = train_h2o,
  validation_frame = valid_h2o,
  nfolds = 5,
  hyper_params = list(
    alpha = c(0, 0.25, 0.50, 0.75, 1.00),
    lambda = c(0, 0.01, 0.1, 1, 10)
  )
)

h2o.getGrid(grid_id = "predictor_model_grid", sort_by = "auc", decreasing = TRUE)
predictor_model_grid_model_3 <- h2o.getModel("predictor_model_grid_model_3")
predictor_model_grid_model_3 %>% h2o.auc(train = T, valid = T, xval = T)
predictor_model_grid_model_3 %>% h2o.performance(newdata = as.h2o(test_tbl))

# Visualize the trade of between the precision and the recall and the optimal threshold ----

#stacked_ensemble_model <- h2o.getModel("StackedEnsemble_BestOfFamily_1_AutoML_2_20230517_165226")
#p_stacked_ensemble_model <- h2o.performance(stacked_ensemble_model, newdata = as.h2o(test_tbl))
#p_stacked_ensemble_model_tbl <- p_stacked_ensemble_model %>%
#  h2o.metric() %>%
#  as_tibble() 

p_predictor_model_grid_model_3 <- predictor_model_grid_model_3 %>% 
  h2o.performance(newdata = as.h2o(test_tbl))

p_predictor_model_grid_model_3_tbl <- p_predictor_model_grid_model_3 %>% 
  h2o.metric() %>%
  as_tibble() %>%
  mutate(auc = h2o.auc(p_predictor_model_grid_model_3))

p_predictor_model_grid_model_3_tbl %>%
  ggplot(aes(x = threshold)) +
  geom_line(aes(y = precision), color = "blue", size = 1) +
  geom_line(aes(y = recall), color = "red", size = 1) +
  
  # Insert line where precision and recall are harmonically optimized
  geom_vline(xintercept = h2o.find_threshold_by_max_metric(p_predictor_model_grid_model_3, "f1")) +
  labs(title = "Precision vs Recall", y = "value")

# ROC Plot ----

roc_plot <- p_predictor_model_grid_model_3_tbl %>% 
  mutate(auc  = auc %>% round(3) %>% as.character() %>% as_factor()) %>%
  ggplot(aes(fpr, tpr, linetype = auc)) +
  geom_line(size = 1) +
  geom_abline(color = "red", linetype = "dotted") +
  labs(
    title = "ROC Plot",
    subtitle = "-"
  )

# Precision vs Recall Plot ----

pr_plot <- p_predictor_model_grid_model_3_tbl %>%
  mutate(auc  = auc %>% round(3) %>% as.character() %>% as_factor()) %>%
  ggplot(aes(recall, precision, linetype = auc)) +
  geom_line(size = 1) +
  labs(
    title = "Precision vs Recall Plot",
    subtitle = "-"
  )

# Gain Plot ----

gain_lift_tbl <- p_predictor_model_grid_model_3 %>%
  h2o.gainsLift() %>%
  as_tibble()

gain_transformed_tbl <- gain_lift_tbl %>% 
  select(group, cumulative_data_fraction, cumulative_capture_rate, cumulative_lift) %>%
  select(-contains("lift")) %>%
  mutate(baseline = cumulative_data_fraction) %>%
  rename(gain     = cumulative_capture_rate) %>%
  # prepare the data for the plotting (for the color and group aesthetics)
  pivot_longer(cols = c(gain, baseline), values_to = "value", names_to = "key")

gain_plot <- gain_transformed_tbl %>%
  ggplot(aes(x = cumulative_data_fraction, y = value, color = key)) +
  geom_line(size = 1.5) +
  labs(
    title = "Gain Chart",
    x = "Cumulative Data Fraction",
    y = "Gain"
  )

# Lift Plot ----

lift_transformed_tbl <- gain_lift_tbl %>% 
  select(group, cumulative_data_fraction, cumulative_capture_rate, cumulative_lift) %>%
  select(-contains("capture")) %>%
  mutate(baseline = 1) %>%
  rename(lift = cumulative_lift) %>%
  pivot_longer(cols = c(lift, baseline), values_to = "value", names_to = "key")

lift_plot <- lift_transformed_tbl %>%
  ggplot(aes(x = cumulative_data_fraction, y = value, color = key)) +
  geom_line(size = 1.5) +
  labs(
    title = "Lift Chart",
    x = "Cumulative Data Fraction",
    y = "Lift"
  )

# Dashboard with cowplot ----

  # Combine using cowplot
  
  # cowplot::get_legend extracts a legend from a ggplot object
  p_legend <- get_legend(roc_plot)
  # Remove legend from p1
  roc_plot <- roc_plot + theme(legend.position = "none")
  
  # cowplot::plt_grid() combines multiple ggplots into a single cowplot object
  p <- plot_grid(
    roc_plot,
    pr_plot,
    gain_plot,
    lift_plot,
    ncol = 2)
  
  # cowplot::ggdraw() sets up a drawing layer
  p_title <- ggdraw() + 
    
    # cowplot::draw_label() draws text on a ggdraw layer / ggplot object
    draw_label("H2O Model Metrics", size = 18, fontface = "bold", 
               color = "#2C3E50")
  
  p_subtitle <- ggdraw() + 
    draw_label(glue("-"), size = 10,  
               color = "#2C3E50")
  
  # Combine everything
  combined_plot <- plot_grid(p_title, p_subtitle, p, p_legend, 
                   
                   # Adjust the relative spacing, so that the legends always fits
                   ncol = 1, rel_heights = c(0.05, 0.05, 1, 0.05 * 4))
  combined_plot
