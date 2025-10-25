#https://www.r-bloggers.com/2020/10/tune-and-interpret-decision-trees-for-tidytuesday-wind-turbines/
library(tidyverse)

turbines <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-10-27/wind-turbine.csv")
turbines

# cleaning
turbines_df <- turbines %>%
  transmute(
    turbine_capacity = turbine_rated_capacity_k_w,
    rotor_diameter_m,
    hub_height_m,
    commissioning_date = parse_number(commissioning_date),
    province_territory = fct_lump_n(province_territory, 10),
    model = fct_lump_n(model, 10)
  ) %>%
  filter(!is.na(turbine_capacity)) %>%
  mutate_if(is.character, factor)

# How is the capacity related to other characteristics like the year of commissioning or size of the turbines?
turbines_df %>%
  select(turbine_capacity:commissioning_date) %>%
  pivot_longer(rotor_diameter_m:commissioning_date) %>%
  ggplot(aes(turbine_capacity, value)) +
  geom_hex(bins = 15, alpha = 0.8) +
  geom_smooth(method = "lm") +
  facet_wrap(~name, scales = "free_y") +
  labs(y = NULL) +
  scale_fill_gradient(high = "cyan3")

# build model
library(tidymodels)
set.seed(123)
wind_split <- initial_split(turbines_df, strata = turbine_capacity)
wind_train <- training(wind_split)
wind_test <- testing(wind_split)
set.seed(234)
wind_folds <- vfold_cv(wind_train, strata = turbine_capacity)
wind_folds

# Tune tree
tree_spec <- decision_tree(
  cost_complexity = tune(),
  tree_depth = tune(),
  min_n = tune()
) %>%
  set_engine("rpart") %>%
  set_mode("regression")

tree_spec

## Decision Tree Model Specification (regression)
## 
## Main Arguments:
##   cost_complexity = tune()
##   tree_depth = tune()
##   min_n = tune()
## 
## Computational engine: rpart

# We need a set of possible parameter values to try out for the decision tree.

tree_grid <- grid_regular(cost_complexity(), tree_depth(), min_n(), levels = 4)

tree_grid

## # A tibble: 64 x 3
##    cost_complexity tree_depth min_n
##              <dbl>      <int> <int>
##  1    0.0000000001          1     2
##  2    0.0000001             1     2
##  3    0.0001                1     2
##  4    0.1                   1     2
##  5    0.0000000001          5     2
##  6    0.0000001             5     2
##  7    0.0001                5     2
##  8    0.1                   5     2
##  9    0.0000000001         10     2
## 10    0.0000001            10     2
## # … with 54 more rows
