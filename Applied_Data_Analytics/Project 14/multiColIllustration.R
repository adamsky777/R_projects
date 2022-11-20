set.seed(123)

#The correlation between x and x^2 depends on the region of where we have x-observations.
#This can be seen visually when considering the geometric interpretation of correlation:

n <- 500
x <- seq(-10,10, length.out=n)
cor(x, x^2) #0
plot(x,x^2)

x <- seq(90,100, length.out=n)
cor(x, x^2) #practically 1
plot(x,x^2)

x <- seq(0,4, length.out=n)
plot(x,x^2)
cor(x, x^2) #0.968125
#This correlation is high, but this does not need
#to be a problem for estimation:

u <- rnorm(n)
beta0 <- 0
beta1 <- -1
beta2 <- 1
trend <- beta0 + beta1*x + beta2*x^2 
y <- trend + u

plot(x,y)
points(x,trend, type="l", col="red", lwd=4)
cor(x,x^2)

lm.simple <- lm(y~x)
abline(lm.simple, lwd=4) #complete miss
#We really do need to include the quadratic trend to get an acceptable
#approximation to the trend:
lm.quad <- lm(y~x+I(x^2))
summary(lm.quad)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.05168    0.13014   0.397    0.691    
# x           -1.07479    0.15029  -7.152 3.09e-12 ***
#   I(x^2)       1.02482    0.03637  28.174  < 2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.9739 on 497 degrees of freedom
# Multiple R-squared:  0.9358,	Adjusted R-squared:  0.9356 
# F-statistic:  3625 on 2 and 497 DF,  p-value: < 2.2e-16

#Let's plot the estimated trend with a quadratic:
betahat <- coefficients(lm.quad)
trendhat <- betahat[1] + betahat[2]*x + betahat[3]*x^2 
points(x,trendhat, type="l", col="blue", lwd=4)
#Almost a perfect match, so the quadratic model
#performs very well even though we have a high degree of
#correlation between x and x^2.

#We are also able to successfully detect that a cubic is not needed:


lm.cubic <- lm(y~x+I(x^2) + I(x^3))
summary(lm.cubic)

#We still correctly identify that a higher order term
#is not needed:

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.022114   0.173072   0.128  0.89838    
# x           -0.985638   0.375088  -2.628  0.00886 ** 
#   I(x^2)       0.969040   0.218028   4.445 1.09e-05 ***
#   I(x^3)       0.009296   0.035828   0.259  0.79538    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.9748 on 496 degrees of freedom
# Multiple R-squared:  0.9359,	Adjusted R-squared:  0.9355 
# F-statistic:  2412 on 3 and 496 DF,  p-value: < 2.2e-16


#Things become more complicated if the model is misspecified,
#which clearly is the case in practice! Complex phenomena -do not-
#follow simple polynomial trends!

#Let's study a more complex trend:
beta0 <- 0
beta1 <- -1
beta2 <- 1
trend <- beta0 + beta1*x + beta2*x^2 + 2*sin(x)
y <- trend + u

cor(x, x^2) #0.968125, as before

plot(x,y)
points(x,trend, type="l", col="red", lwd=4)
#The trend has some curvature, though slightly more
#complex than a simple quadratic.

summary(lm(y~x+I(x^2))) #x and x^2 significant
summary(lm(y~x+I(x^2) + I(x^3))) #x and x^3 significant
summary(lm(y~x+I(x^2) + I(x^3) + I(x^4))) #x^2, x^3, x^4 significant
summary(lm(y~x+I(x^2) + I(x^3) + I(x^4) + I(x^5))) #no covariates are -individually- significant (!)
#Some of these p-values are affected by multicollinearity, though
#we will not study this in technical detail. 