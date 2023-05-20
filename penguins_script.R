library(tidyverse)
library(tidymodels)

df <- palmerpenguins::penguins

df %>% 
nest_by(sex) %>% 
  mutate(mod=list(lm(bill_depth_mm~bill_length_mm+species*island,data=data))) %>% 
  mutate(
    tidy=list(broom::tidy(mod))
    ,glance=list(broom::glance(mod))
    ) %>% 
  unnest(tidy) %>% print(n=100)


rec_pca <- recipe(bill_length_mm~.,data=df) %>% 
  step_naomit(all_predictors()) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_rm(year) %>% 
  step_dummy(all_nominal_predictors())

rec_basic<- recipe(bill_length_mm~.,data=df) %>% 
  step_naomit(all_predictors()) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_rm(year)

df_processed <- rec_pca %>% prep() %>% juice()

# choose models


log_mod <- linear_reg() %>% 
  set_mode("regression")

rang_mod <- rand_forest() %>%
  set_engine("ranger", importance = "impurity", seed = 63233, quantreg = TRUE) %>%
  set_mode("regression")



stan_mod <- linear_reg() %>% set_engine("stan") 


linear_models <- workflow_set(
  preproc = list(
    pca=rec_pca
    ,basic=rec_basic
    ),
  models = list(
               rang=rang_mod
               ,stan=stan_mod
               ,lm=log_mod
               )
  )

fitted_models <- linear_models %>% 
  mutate(fit = map(info, ~ fit(.x$workflow[[1]], df)))


test <- linear_models %>% workflow_map(fn = "tune_grid")

fitted_models %>% collect_metrics()

workflowsets::two_class_set

log_wf <- workflow() %>% 
  add_recipe(rec_pca) %>% 
  add_model(log_mod) %>% 
  fit(data=df)


rand_wf <- workflow() %>% 
  add_recipe(rec_pca) %>% 
  add_model(log_mod) %>% 
  fit(data=df)

stan_wf <- workflow() %>% 
  add_recipe(rec_pca) %>% 
  add_model(stan_mod) %>% 
  fit(data=df)


rand_wf %>% tidy()
stan_wf %>% broom.mixed::tidy()
pred_log <- log_wf %>% augment(new_data=df %>% na.omit())
pred_rand <- rand_wf %>% augment(new_data=df %>% na.omit())

library(broom.mixed)

#get metrics

mae(data = pred_rand,truth =bill_length_mm,estimate = .pred )

penguins_metric <- metric_set(rmse, rsq, mae)

  penguins_metric(data = pred_rand,truth =bill_length_mm,estimate = .pred)
  

  
x=1:1000
sd(x)
mad(x,center = mean(x))


tibble(
  x=1:100
  ,var=x^2
  ,sum_var=sum(var)
  ,stdev=sd(x)
  ,mean=mean(x)
  ,var_abs=abs(mean-x)
  ,sum_var_abs=sum(var_abs)
  ,stdev_abs=sum_var_abs/100
  )



