library(lme4)
set.seed(7658943) 

d_temp = expand.grid(group = c("PLA", "FUT"),
                     time = c("Baseline", "3month"))
n_varcombo = nrow(d_temp)
d_temp$exp = c(29, 30,  28, 24)
n_ind = 30
dat_long = d_temp[rep(1:n_varcombo, each = n_ind), ]
dat_long$ID = rep(1:n_ind, each = n_varcombo)
set.seed(6)
dat_long$ID_effect = rep(rnorm(n_ind, sd = 3), each = n_varcombo)
dat_long$vo2 = dat_long$exp + rnorm(n_varcombo * n_ind, sd = 3)

# RK: convert to factor 
dat_long$Subj <- factor(dat_long$ID)

# Model without interaction
res0 <- lmer(vo2 ~ group + time + (1 | Subj), data = dat_long)
summary(res0)

# Two (equivalent) models with interaction
res1 <- lmer(vo2 ~  1 + time*group + (1  | Subj), 
             data = dat_long, REML=FALSE,
             control=lmerControl(calc.derivs = FALSE))
summary(res1)

res2 <- lmer(vo2 ~  1 + time + group:time + (1 | Subj),
             data = dat_long, REML=FALSE,
             control=lmerControl(calc.derivs = FALSE))
summary(res2)

anova(res1, res2)


# RK: specify contrasts
contrasts(dat_long$group) <- contr.treatment(2)
contrasts(dat_long$time) <- contr.treatment(2)

mm3 <- model.matrix(~ 1 + time*group, data=dat_long)
head(mm3)
t <- mm3[,2]
g <- mm3[,3]
t_x_g <- mm3[,4]
res3 <- lmer(vo2 ~  1 + t + t_x_g + (1 | Subj),
             data = dat_long, REML=FALSE,
             control=lmerControl(calc.derivs = FALSE))
summary(res3)

anova(res3, res2)

# compare with res1
res1b <- lmer(vo2 ~  1 + t + g + t_x_g + (1 | Subj),
             data = dat_long, REML=FALSE,
             control=lmerControl(calc.derivs = FALSE))
summary(res1b)
summary(res1)

# Different contrasts
contrasts(dat_long$group) <- contr.sum(2)
contrasts(dat_long$time) <- contr.sum(2)

mm4 <- model.matrix(~ 1 + time*group, data=dat_long)
head(mm4)
t <- mm4[,2]
t_x_g <- mm4[,4]
res4 <- lmer(vo2 ~  1 + t + t_x_g + (1 | Subj),
             data = dat_long, REML=FALSE,
             control=lmerControl(calc.derivs = FALSE))
summary(res4)

anova(res4, res3, res2)
