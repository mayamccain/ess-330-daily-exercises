library(tidymodels)
library(dplyr)
library(tidyverse)
data("penguins")
set.seed(101991)
(split_penguins <- initial_split(penguins, prop = 0.7))

penguins_training <- training(split_penguins)
penguins_testing <- testing(split_penguins)

penguins_training |> glimpse() 
nrow(penguins_training) * 1/10
vfold_cv(penguins_training, v = 10)


# Model fitting and workflow
library(parsnip)
library(glmnet)

logistic_model <- logistic_reg() %>%
  set_engine("glm") %>%
  set_mode("classification")

rand_forest_model <- rand_forest() %>% 
  set_engine("ranger") %>%
  set_mode("classification")

library(purrr)
library(workflows)
library(workflowsets)

lm_wf <- workflow() %>%
  add_formula(species ~ .) %>%
  add_model(logistic_model) %>%
  fit(data = penguins_training)

rf_wf <- workflow() %>%
  add_formula(species ~ .) %>%
  add_model(rand_forest_model) %>%
  fit(data = penguins_training)

wf_set <- workflow_set(
  preproc = list(lm = lm_wf, rf = rf_wf),
  models = list(logistic = logistic_model, random_forest = rand_forest_model)
)

set.seed(123)
cv_folds <- vfold_cv(penguins_training, v = 5) 

results <- wf_set %>%
  workflow_map("fit_resamples", resamples = cv_folds, metrics = metric_set(accuracy))

ranked_results <- rank_results(results, rank_metric = "accuracy")
print(ranked_results)

# I think the logistic regression model is better for this data because it has better accuracy. 

