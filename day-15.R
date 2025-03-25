library(tidymodels)
data("penguins")
set.seed(101991)
(split_penguins <- initial_split(penguins, prop = 0.7))

penguins_training <- training(split_penguins)
penguins_testing <- testing(split_penguins)

penguins_training |> glimpse() 
nrow(penguins_training) * 1/10
vfold_cv(penguins_training, v = 10)
