# Load the libraries ----

library(tidyverse)
library(readxl)
library(rsample)
library(h2o)
library(recipes)

# Load the training & test dataset ----

product_backorders_tbl <- read_csv("~/GitHub/ss23-bdml-gcturemen/code_drafts/data/product_backorders.csv")

#product_backorders_tbl <- product_backorders_tbl %>%
#  mutate_if(is.character, as.factor) %>%
#  glimpse()

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

train_tbl %>% glimpse()
test_tbl %>% glimpse()

# Specify the response and predictor variables ----

h2o.init()

split_h2o <- h2o.splitFrame(as.h2o(train_tbl), ratios = c(0.80), seed = 767)
train_h2o <- split_h2o[[1]]
valid_h2o <- split_h2o[[2]]
test_h2o  <- as.h2o(test_tbl)

y <- "went_on_backorder"
x <- setdiff(names(train_h2o), y)

# Run AutoML specifying the stopping criterion ----

automl_models_h2o <- h2o.automl(
  x = x,
  y = y,
  training_frame    = train_h2o,
  validation_frame  = valid_h2o,
  leaderboard_frame = test_h2o,
  max_runtime_secs  = 60,
  nfolds            = 5 
)

# View the leaderboard ----

automl_models_h2o@leaderboard

# Predicting using Leader Model ----

#h2o.loadModel("code_drafts/h20_models/StackedEnsemble_AllModels_1_AutoML_1_20230516_142806")

predictor_model <- h2o.getModel("StackedEnsemble_AllModels_1_AutoML_1_20230516_142806")

predictions <- h2o.predict(predictor_model, newdata = as.h2o(test_tbl))

predictions_tbl <- predictions %>% as_tibble()

predictions_tbl

# Save the leader model ----

predictor_model %>% 
  h2o.saveModel(path = "code_drafts/h20_models/")


