library(easystats)
library(lme4)

# lm

if (interactive()) {
  mod <- lm(wt ~ mpg, mtcars)
  
  # with default options
  model_dashboard(mod)
  
  # customizing 'parameters' output: standardize coefficients
  model_dashboard(mod, parameters_args = list(standardize = "refit"))
  
  # customizing 'performance' output: only show selected performance metrics
  model_dashboard(mod, performance_args = list(metrics = c("AIC", "RMSE")))
  
  # customizing output of model assumptions plot: don't show dots (faster plot)
  model_dashboard(mod, check_model_args = list(show_dots = FALSE))
}

# lmm


if (interactive()) {
  fm1 <-  lmer(Reaction ~ Days + (Days | Subject), sleepstudy, REML=FALSE)
  
  # with default options
  model_dashboard(fm1)
  
  # customizing 'performance' output: only show selected performance metrics
  model_dashboard(fm1, performance_args = list(metrics = c("AIC", "BIC")))
  
  # customizing output of model assumptions plot: don't show dots (faster plot)
  model_dashboard(fm1, check_model_args = list(show_dots = FALSE))
}




