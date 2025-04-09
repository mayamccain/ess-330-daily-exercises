library(tidyverse)
library(tidymodels)

# URLs
covid_url <-  'https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv'
pop_url   <- 'https://www2.census.gov/programs-surveys/popest/datasets/2020-2023/counties/totals/co-est2023-alldata.csv'

# Ingest

data   <- readr::read_csv(covid_url)
census_raw <- readr::read_csv(pop_url)

census <- census_raw |>
  filter(COUNTY == "000") |>
  mutate(fips = STATE) |>
  select(fips, POPESTIMATE2021, DEATHS2021, BIRTHS2021)

state_data <- data |>
  group_by(fips) |>
  mutate(new_cases = pmax(0, cases - lag(cases)),
         new_deaths = pmax(deaths - lag(deaths))) |>
  ungroup() |>
  left_join(census, by = "fips") |>
  mutate(y = year(date), m = month(date),
         season = case_when(
           m %in% c(12,1,2) ~ "Winter",
           m %in% 3:5 ~ "Spring",
           m %in% 6:8 ~ "Summer",
           m %in% 9:11 ~ "Fall"
         )) |>
  group_by(state, y, season) |>
  mutate(season_cases = sum(new_cases, na.rm = TRUE),
         season_deaths = sum(new_deaths, na.rm = TRUE)) |>
  distinct(state, y, season, .keep_all = TRUE) |>
  ungroup() |>
  select(state, contains("season"), contains("2021")) |>
  drop_na() |>
  mutate(logC = log(season_cases + 1))

skimr::skim(state_data)

### ML Applications

set.seed(123)
split <- initial_split(state_data, prop = .8, strata = season)
training <- training(split)
testing <- testing(split)
folds <- vfold_cv(training, v = 10)

rec <- recipe(logC ~ ., data = training) |>
  step_rm(state, season_cases) |>
  step_dummy(all_nominal_predictors()) |>
  step_scale(all_numeric_predictors()) |>
  step_center(all_numeric_predictors())

lm_mod <- linear_reg() |>
  set_engine('lm') |>
  set_mode("regression")

rf_mod <- rand_forest() |>
  set_engine('ranger', importance = 'impurity') |>
  set_mode("regression")

rf_mod2 <- rand_forest() |>
  set_engine('randomForest') |>
  set_mode("regression")

b_mod <- boost_tree() |>
  set_engine('xgboost') |>
  set_mode("regression")

lgbm_model <- boost_tree() |>
  set_engine("lightgbm") |>
  set_mode("regression")

nn_mod <- mlp(hidden = 10) |>
  set_engine('nnet') |>
  set_mode("regression")

wf <- workflow_set(list(rec), list(lm_mod, rf_mod, rf_mod2, b_mod, lgbm_model, nn_mod)) |>
  workflow_map(resamples = folds)

autoplot(wf)
rank_results(wf, rank_metric = "rsq", select_best = TRUE)

b_fit = workflow() |>
  add_recipe(rec) |>
  add_model(rf_mod) |>
  fit(data = training)

a = augment(b_fit, new_data = training)

ggplot(a, aes(x = .pred, y = logC)) + 
  scale_color_viridis_c() +
  geom_point() +
  geom_abline() +
  theme_linedraw()

vip::vip(b_fit)

# Day 2

?boost_tree

b_model_tuned = boost_tree(trees = tune(),
                           tree_depth = tune(),
                           min_n = tune()) |>
  set_mode("regression") |>
  set_engine("lightgbm")

wf_tune <- workflow(rec, b_model_tuned)

covid_metric = metric_set(mae, rsq, rmse)

dials <- extract_parameter_set_dials(wf_tune)
dials$object

my.grid <- dials |>
  grid_latin_hypercube(size = 20)

ploty::plot_ly(my.grid,
               x = trees,
               y = ~min_n,
               z = ~ tree_depth)
model_params <- tune_grid(
  wf_tune,
  resamples = folds,
  grid = my.grid,
  metrics = covid_metric
)

autoplot(model_params)

show_best(model_params, metrics = "rmse")

hp = select_best(model_params, metric = "mae")

final_wf = finalize_workflow(wf_tune, hp)

last_f <- last_fit(final_wf, split, metrics = covid_metric)

collect_metrics(last_f)

pred = collect_predictions(last_f)

ggplot(pred, aes(x = logC, y = .pred)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_abline(col = 'red') +
  theme_linedraw()

full_fit <- fit(final_wf, data = state_data)


