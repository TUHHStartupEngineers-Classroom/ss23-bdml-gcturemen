# Load Libraries ----

library(h2o)
library(recipes)
library(readxl)
library(tidyverse)
library(tidyquant)
library(lime)

# Load Data ----
employee_attrition_tbl <- read_csv("code_drafts/data/datasets-1067-1925-WA_Fn-UseC_-HR-Employee-Attrition.csv")
definitions_raw_tbl    <- read_excel("code_drafts/data/data_definitions.xlsx", sheet = 1, col_names = FALSE)

# Processing Pipeline ----
source("code_drafts/data/data_processing_pipeline.R")

employee_attrition_readable_tbl <- process_hr_data_readable(employee_attrition_tbl, definitions_raw_tbl)

# Split into test and train ----
set.seed(seed = 1113)
split_obj <- rsample::initial_split(employee_attrition_readable_tbl, prop = 0.85)

# Assign training and test data ----
train_readable_tbl <- rsample::training(split_obj)
test_readable_tbl  <- rsample::testing(split_obj)

# ML Preprocessing Recipe ----
recipe_obj <- recipe(Attrition ~ ., data = train_readable_tbl) %>%
  step_zv(all_predictors()) %>%
  step_mutate_at(c("JobLevel", "StockOptionLevel"), fn = as.factor) %>% 
  prep()

recipe_obj

train_tbl <- bake(recipe_obj, new_data = train_readable_tbl)
test_tbl  <- bake(recipe_obj, new_data = test_readable_tbl)

# Models ----

h2o.init()

split_h2o <- h2o.splitFrame(as.h2o(train_tbl), ratios = c(0.80), seed = 767)
train_h2o <- split_h2o[[1]]
valid_h2o <- split_h2o[[2]]
test_h2o  <- as.h2o(test_tbl)

y <- "Attrition"
x <- setdiff(names(train_h2o), y)

automl_models_h2o <- h2o.automl(
  x = x,
  y = y,
  training_frame    = train_h2o,
  validation_frame  = valid_h2o,
  leaderboard_frame = test_h2o,
  max_runtime_secs  = 30,
  nfolds            = 5 
)

automl_models_h2o@leaderboard

automl_leader <- h2o.getModel("StackedEnsemble_BestOfFamily_3_AutoML_1_20230518_131755")

automl_leader

# Making predictions ----

predictions_tbl <- automl_leader %>% 
  h2o.predict(newdata = as.h2o(test_tbl)) %>%
  as.tibble() %>%
  bind_cols(
    test_tbl %>%
      select(Attrition, EmployeeNumber)
  )

predictions_tbl

# Explainer ----

explainer <- train_tbl %>%
  select(-Attrition) %>%
  lime(
    model           = automl_leader,
    bin_continuous  = TRUE,
    n_bins          = 4,
    quantile_bins   = TRUE
  )

explainer

# Single Explanation ----

explanation <- test_tbl %>%
  slice(2) %>%
  select(-Attrition) %>%
  lime::explain(
    
    # Pass our explainer object
    explainer = explainer,
    # Because it is a binary classification model: 1
    n_labels   = 1,
    # number of features to be returned
    n_features = 8,
    # number of localized linear models
    n_permutations = 5000,
    # Let's start with 1
    kernel_width   = 1
  )

explanation

# Plotting the single explanation ----

g <- plot_features(explanation = explanation, ncol = 1)
g

# Multiple explanations ----

explanation_multiple <- test_tbl %>%
  slice(1:20) %>%
  select(-Attrition) %>%
  lime::explain(
    explainer = explainer,
    n_labels   = 1,
    n_features = 8,
    n_permutations = 5000,
    kernel_width   = 0.5
  )
 
# Plotting multiple explanations ----

g2 <- plot_features(explanation_multiple, ncol = 4)
g2

g3 <- plot_explanations(explanation_multiple)
g3

# custom_plot_features ----

custom_plot_features <- function(expl) {
  
  case <- expl$case[1]
  lab <- expl$label[1]
  prob <- expl$label_prob[1] %>% round(2)
  expl_fit <- expl$model_r2[1] %>% round(2)
  
  text <- paste(" Case: ", case, "\n", "Label: ", lab, "\n", "Probability: ", prob, "\n", "Explanation Fit: ", expl_fit)
  
  g <- expl %>% 
    arrange(desc(abs(feature_weight))) %>%
    mutate(feature_type = case_when(
      (feature_weight) >= 0 ~ "Supports",
      TRUE ~ "Contradicts") %>% as.factor()) %>%
    ggplot(aes(x = feature_desc, y = feature_weight, group = feature_weight)) +
    geom_col(aes(reorder(feature_desc, abs(feature_weight)), fill = feature_type)) + 
    scale_fill_manual("", values = c("Supports" = "blue", "Contradicts" = "red")) + 
    coord_flip() +
    labs(x = "Feature", y = "Weight", subtitle = text) +
    theme(legend.position = "bottom", plot.subtitle = element_text(size = 10))
    
  return(g)
}

custom_plot_features(explanation)

# custom_plot_explanations ----

custom_plot_explanations <- function(expl) {
  
  expl$case <- as_factor(expl$case)
  
  g <- expl %>%
    ggplot(aes(x = case, y = feature_desc, fill = feature_weight)) +
    geom_tile() + 
    facet_wrap(~ label) +
    scale_fill_gradient2("Feature Weight", low = "red", high = "blue", mid = "white") + 
    labs(x = "Case", y = "Feature") +
    theme()
  
  return(g)
}

custom_plot_explanations(explanation_multiple)

# custom_plot_features2 ----

custom_plot_features2 <- function(expl, ncol) {
  
  expl$case <- as_factor(expl$case)
  
  case <- expl$case
  lab <- expl$label
  prob <- expl$label_prob %>% round(2)
  expl_fit <- expl$model_r2 %>% round(2)
  
  text_tbl <- tibble(
    id = c(1:length(case)),
    text = c()
  )
  
  for (i in 1:length(case)) {
    paste(" Case: ", case[i], "\n", "Label: ", lab[i], "\n", "Probability: ", prob[i], "\n", "Explanation Fit: ", expl_fit[i]) %>%
      append(text_tbl$text)
  }
  
  g <- expl %>% 
    arrange(desc(abs(feature_weight))) %>%
    mutate(feature_type = case_when(
      (feature_weight) >= 0 ~ "Supports",
      TRUE ~ "Contradicts") %>% as.factor()) %>%
    ggplot(aes(x = feature_desc, y = feature_weight, group = feature_weight)) +
    geom_col(aes(reorder(feature_desc, abs(feature_weight)), fill = feature_type)) + 
    scale_fill_manual("", values = c("Supports" = "blue", "Contradicts" = "red")) + 
    coord_flip() +
    labs(x = "Feature", y = "Weight") +
    theme(legend.position = "bottom", plot.subtitle = element_text(size = 10)) +
    facet_wrap(~case, scales = "free_y", ncol = ncol) + 
    geom_text(data = text_tbl, mapping = aes(label = text))
  
  return(g)
}

custom_plot_features2(explanation_multiple, 4)