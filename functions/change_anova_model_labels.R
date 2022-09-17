# Use name of fitted model objects to label models in anova() output
library(lme4)
fm1 <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy, REML=FALSE)
fm2 <- lmer(Reaction ~ Days + (Days || Subject), sleepstudy, REML=FALSE)
anova(fm2, fm1)

# alternative labels
models_list <- list("fitted m1" = fm1, "fitted m2" = fm2)
eval(
  do.call(
    call, 
    c(list("anova"), 
      lapply(names(models_list), as.symbol)), 
    quote = TRUE), 
  models_list)
