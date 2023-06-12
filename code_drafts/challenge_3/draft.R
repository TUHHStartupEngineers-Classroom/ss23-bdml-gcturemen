# 1. Importing the libraries ----

library(tidyverse)
library(readxl)
library(skimr)
library(GGally)

# 2. Reading the data ----

employee_attrition_tbl <- read_csv("~/GitHub/ss23-bdml-gcturemen/code_drafts/data/datasets-1067-1925-WA_Fn-UseC_-HR-Employee-Attrition.csv")

path_data_definitions <- "~/GitHub/ss23-bdml-gcturemen/code_drafts/data/data_definitions.xlsx"
definitions_raw_tbl   <- read_excel(path_data_definitions, sheet = 1, col_names = FALSE)

# 3. Useful functions ----

plot_ggpairs <- function(data, color = NULL, density_alpha = 0.5) {
  
  color_expr <- enquo(color)
  
  if (rlang::quo_is_null(color_expr)) {
    
    g <- data %>%
      ggpairs(lower = "blank") 
    
  } else {
    
    color_name <- quo_name(color_expr)
    
    g <- data %>%
      ggpairs(mapping = aes_string(color = color_name), 
              lower = "blank", legend = 1,
              diag = list(continuous = wrap("densityDiag", 
                                            alpha = density_alpha))) +
      theme(legend.position = "bottom")
  }
  
  return(g)
  
}

# 4. Compensation features: HourlyRate, MonthlyIncome, StockOptionLevel ----

employee_attrition_tbl %>%
  select(Attrition, contains("income"), contains("rate"), contains("salary"), contains("stock")) %>%
  plot_ggpairs(Attrition)

# 1) c
# 2) c however groups are relatively similar
# 3) b

# 5. Survey Results: Satisfaction level, WorkLifeBalance ----

employee_attrition_tbl %>%
  select(Attrition, contains("satisfaction"), contains("life")) %>%
  plot_ggpairs(Attrition)

# 4) a
# 5) b

# 6. Performance Data: Job Involvement, Performance Rating ----

employee_attrition_tbl %>%
  select(Attrition, contains("performance"), contains("involvement")) %>%
  plot_ggpairs(Attrition)

# 6) a

# 7. Work-Life Features ----

employee_attrition_tbl %>%
  select(Attrition, contains("overtime"), contains("travel")) %>%
  plot_ggpairs(Attrition)

# 7) b

# 8. Training and Education ---- 

employee_attrition_tbl %>%
  select(Attrition, contains("training"), contains("education")) %>%
  plot_ggpairs(Attrition)

# 8) b

# 9. Time-Based Features: Years at company, years in current role ----

employee_attrition_tbl %>%
  select(Attrition, contains("years")) %>%
  plot_ggpairs(Attrition)

# 9) b
# 10) c