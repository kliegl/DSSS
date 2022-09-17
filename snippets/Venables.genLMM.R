###
# Venables: Generate data
fph <- 0.4 

Sigh <- sqrt(0.0002)    # Sigma between hospitals
Sigi <- sqrt(0.04)      # Sigma between individuals (residual)

reH  <- rnorm(90, fph, Sigh)  # random effect for Hospital
data <- within(expand.grid(Hospital = 1:90, empid = 1:80), 
               fpi1 <- reH[Hospital] + rnorm(7200, 0 , Sigi)) 

# 
(fm1 <- lmer(fpi1 ~ (1|Hospital), data))
str(fm1)
plot(fm1@ranef, reH-fph) 
abline(0,1) 

cor(fm1@ranef, reH) 
##
