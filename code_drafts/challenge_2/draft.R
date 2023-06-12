# 1. Load the libraries ----

library(tidyverse)  # Standard)
library(parsnip)  # Modeling
library(recipes)  # Pre-processing & Sampling
library(rsample)  # Pre-processing & Sampling
library(yardstick)  # Modeling Error Metrics
library(rpart.plot)  # Plotting Decision Trees
library(workflows)

# 2. Data exploration ----

bike_orderlines_tbl <- readRDS(
  "~/GitHub/ss23-bdml-gcturemen/code_drafts/data/bike_orderlines.rds")

bike_features_tbl <- readRDS(
  "~/GitHub/ss23-bdml-gcturemen/code_drafts/data/bike_features_tbl.rds")

model_num_sold_tbl <- bike_orderlines_tbl %>% 
  select(quantity, model) %>%
  group_by(model) %>%
  summarise(num_sold = sum(quantity)) %>%
  ungroup() %>%
  arrange(desc(num_sold))

model_sales_tbl <- bike_orderlines_tbl %>% 
  select(total_price, model, category_2, frame_material) %>% 
  group_by(model, category_2, frame_material) %>%
  summarise(revenue = sum(total_price)) %>%
  ungroup() %>%
  arrange(desc(revenue))

model_sales_tbl <- model_sales_tbl %>% inner_join(model_num_sold_tbl)

model_sales_tbl %>% 
  mutate(category_2 = as_factor(category_2) %>% 
           fct_reorder(revenue, .fun = max) %>% 
           fct_rev()) %>% 
  ggplot(aes(frame_material, revenue)) +
  geom_violin() +
  geom_jitter(width = 0.1, alpha = 0.5, color = "#2c3e50") +
  #coord_flip() +
  facet_wrap(~ category_2) +
  scale_y_continuous(labels = scales::dollar_format(scale = 1e-6, 
                                                    suffix = "M", 
                                                    accuracy = 0.1)) +
  tidyquant::theme_tq() +
  labs(title = "Total Sales for Each Model",
    x = "Frame Material", y = "Revenue")

# 3. Splitting the data ----

set.seed(1036) #517  0.75
split_obj <- initial_split(bike_features_tbl, prop = 0.80, strata = "category_2")

split_obj %>% training() %>% distinct(category_2)
split_obj %>% testing() %>% distinct(category_2)

train_tbl <- training(split_obj)
test_tbl  <- testing(split_obj)

train_tbl <- train_tbl %>% set_names(str_replace_all(names(train_tbl), " |-", "_"))
test_tbl  <- test_tbl  %>% set_names(str_replace_all(names(test_tbl),  " |-", "_"))

# 4. Creating the recipe ----

sales_rec <- 
  recipe(price ~ ., data = train_tbl %>% select(-c(bike_id:model_year), -weight, -category_1, -c(category_3:Fork), -c(Front_Derailleur:Brake_Rotor))) %>%
  step_dummy(all_nominal(), -all_outcomes(), one_hot = T) %>%
  step_zv(all_predictors()) %>%
  prep()

#summary(sales_rec) %>% print(n = 65)
summary(sales_rec)

# 5. Creating the model ----

linear_model_01 <- 
  linear_reg("regression") %>%
  set_engine("lm")

# 6. Creating the workflow ----

sales_wflow <- 
  workflow() %>% 
  add_model(linear_model_01) %>% 
  add_recipe(sales_rec)

sales_wflow

sales_fit <- 
  sales_wflow %>% 
  fit(data = train_tbl)

# 7. Metrics helper function ----

calc_metrics <- function(model, new_data = test_tbl) {
  
  model %>%
    predict(new_data = new_data) %>%
    
    bind_cols(new_data %>% select(price)) %>%
    metrics(truth = price, estimate = .pred)
  
}

# 8. Evaluate the model ----

sales_fit %>% calc_metrics(test_tbl)

# 9. Model explanation ??
