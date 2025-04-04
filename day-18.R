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

nn_mod <- mlp(hidden = 10) |>
  set_engine('nnet') |>
  set_mode("regression")

wf <- workflow_set(list(rec), list(lm_mod, rf_mod, rf_mod2, b_mod, nn_mod)) |>
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

