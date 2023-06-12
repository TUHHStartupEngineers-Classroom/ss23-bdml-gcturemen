# 1. Initializing Libraries ----

library(dplyr)
library(tidyverse)
library(tidyquant)
library(broom)
library(umap)

# 2. Read Data ----

sp_500_prices_tbl <- read_rds("~/GitHub/ss23-bdml-gcturemen/code_drafts/data/sp_500_prices_tbl.rds")
sp_500_index_tbl <- read_rds("~/GitHub/ss23-bdml-gcturemen/code_drafts/data/sp_500_index_tbl.rds")

# 3. Calculate Daily Returns ----

sp_500_prices_tbl <- sp_500_prices_tbl %>% filter(date >= '2018-01-01')
sp_500_prices_tbl <- sp_500_prices_tbl %>% group_by(symbol)
sp_500_prices_tbl <- sp_500_prices_tbl %>% mutate(lag = lag(adjusted))
sp_500_prices_tbl <- sp_500_prices_tbl %>% na.omit()
sp_500_prices_tbl <- sp_500_prices_tbl %>% mutate(diff = adjusted - lag)
sp_500_prices_tbl <- sp_500_prices_tbl %>% mutate(pct_return = diff / lag)
sp_500_daily_returns_tbl <- sp_500_prices_tbl %>% select(symbol, date, pct_return)

# 4. Convert to user-item format ----

stock_date_matrix_tbl <- sp_500_daily_returns_tbl %>% 
  pivot_wider(names_from = date, values_from = pct_return, values_fill = 0) %>%
  ungroup()

# 5. Perform K-Means Clustering ----

kmeans_obj <- stock_date_matrix_tbl %>%
  select(-symbol) %>%
  kmeans(centers = 4, nstart = 20)

glance(kmeans_obj)

# 6. Find the Optimal Value of K ----

kmeans_mapper <- function(center = 3) {
  stock_date_matrix_tbl %>%
    select(-symbol) %>%
    kmeans(centers = center, nstart = 20)
}

k_means_mapped_tbl <- tibble(centers = 1:30) %>%
  mutate(k_means = centers %>% map(kmeans_mapper)) %>%
  mutate(glance  = k_means %>% map(glance))

k_means_mapped_tbl %>%
  unnest(glance) %>%
  select(centers, tot.withinss) %>%
  
  ggplot(aes(centers, tot.withinss)) +
  geom_point(color = "#2DC6D6", size = 4) +
  geom_line(color = "#2DC6D6", size = 1) +
  ggrepel::geom_label_repel(aes(label = centers), color = "#2DC6D6") + 
  
  labs(title = "Scree Plot",
       subtitle = "Measures the distance each of the customer are from the closes K-Means center",
       caption = "Conclusion: Based on the Scree Plot, we select 3 clusters to segment the customer base.")

# 7. Apply UMAP ----

umap_results <- stock_date_matrix_tbl %>% 
  select(-symbol) %>% 
  umap()

umap_results_tbl <- umap_results$layout %>%
  as_tibble(.name_repair = "unique") %>%
  set_names(c("x", "y")) %>%
  bind_cols(stock_date_matrix_tbl %>% select(symbol))

umap_results_tbl %>%
  ggplot(aes(x, y)) +
  geom_point(alpha = 0.5) + 
  geom_label_repel(aes(label = symbol), size = 3) + 
  theme_tq() + 
  labs(title = 'UMAP Projection')

# 8. Combine K-Means and UMAP ----

k_means_obj <- k_means_mapped_tbl %>%
  pull(k_means) %>%
  pluck(10)

k_means_10_clusters_tbl <- k_means_obj %>% 
  augment(stock_date_matrix_tbl) %>%
  select(symbol, .cluster)

umap_kmeans_results_tbl <- umap_results_tbl %>%
  left_join(k_means_10_clusters_tbl)

umap_kmeans_results_tbl %>%
  mutate(label_text = str_glue("Symbol: {symbol}
                                 Cluster: {.cluster}")) %>%
  
  ggplot(aes(x, y, color = .cluster)) +
  
  # Geometries
  geom_point(alpha = 0.5) +
  geom_label_repel(aes(label = label_text), size = 2, fill = "#282A36") +
  
  # Formatting
  scale_color_discrete() +
  labs(title = "Customer Segmentation: 2D Projection",
       subtitle = "UMAP 2D Projection with K-Means Cluster Assignment",
       caption = "Conclusion: 3 Customer Segments identified using 2 algorithms") +
  theme(legend.position = "none")


