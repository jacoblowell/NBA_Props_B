#load libraries
options(scipen=999)
library(tidymodels)
library(plotly)
library(tidyverse)
library(lubridate)



set.seed(69)


##


data_split <- initial_split(results_min,  p = 0.8)

# could do by strata ll

train <- training(data_split)
test <- testing(data_split)








bet_recipe <- recipe(
  Win ~  . ,
  data = train
) %>% #update_role(prop_id  , new_role = "ID") %>%  #step_woe(  Prop , outcome =  vars(win)) %>% 
  #step_zv(all_predictors()) %>%   #step_other(all_nominal(), threshold = 0.03, other = "other") %>% 
  prep()

train_tbl <-bake(bet_recipe  , new_data  = train)
test_tbl <- bake(bet_recipe  , new_data = test)


# train_tbl <- train_tbl[1:2000 ,]


# fit model with reciple

logistic_glm  <- 
  logistic_reg(mode = "classification") %>% 
  set_engine("glm") %>% 
  fit( Win ~ . , data = train_tbl)


rstudioapi::jobRunScript(   "model_scripts/glm_model.R"   , exportEnv = "R_GlobalEnv")



predictions_glm <- logistic_glm %>%  predict(new_data = test_tbl) %>%  bind_cols( test_tbl)


###

bet_model_workflow <- workflow() %>% 
  add_model(lr_mod) %>%  add_recipe(bet_recipe)


bets_fit <- bet_model_workflow %>% fit(data = train)

flights_fit %>% 
  pull_workflow_fit() %>% 
  tidy()
