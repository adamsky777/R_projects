rm(list=ls())

beauty <- read.table("https://raw.githubusercontent.com/adamsky777/R_projects/main/Data/beauty.csv",
                     sep=",", header=TRUE)

colnames(beauty)

#Assignment 1
#A) 

#Since "hot" is in both models, we do not assess whether it is zero when
#performing the F-test. The F-test only tests whether the extra parameters
#in the biggest model under comparison are zero. Here, this means we
#only test the coefficients of the interaction terms, whose interpretation
#is the difference in b and c in the stated equation for the quadratic.

#B)

beauty$hot <- (beauty$looks>3) * 1
View(beauty)



lm.hotInteraction <- with(beauty, lm(lwage ~ hot + exper + I(exper^2)  + hot*exper + hot*I(exper^2)))



lm.hotNoHotness <- with(beauty, lm(lwage ~ exper + I(exper^2)))


#during anova first always pass the restricted then the unrestricted model.
anova(lm.hotNoHotness, lm.hotInteraction)

# p-value is 6% really close to 5%, meaning we have some proofs that the trends
# We have some proof altought it is not higly significant that the trends in the
# groups differ.

"
p-value is still higher than 5%, but it is lower now (and close to 5%) 
compared to when when we tested only the joint significance of the two interaction terms. 
It now seems that we do have some proof for the statement that
the trends in the two groups differ, though the proof is
not very strong."


#C)
# use the simplified model, to test in what degree we have evidence for a beauty effect
# on lwage

# simplified model:
lm.hotNoHotness <- lm(beauty$lwage ~ beauty$exper + I(beauty$exper^2)) 

# add hot indicator to the model:
lm.hotExper <- lm(beauty$lwage ~ beauty$exper + I(beauty$exper^2) + beauty$hot)

summary(lm.hotExper)
# the p value is around 8%, meaning it is not extremely significant that beauty 
# has an effect on lwage. The effect is rather weak, 
# when only taking experience into account.




#Assignment 2
#A)

#ADD MORE TO THE FOLLOWING CODE:
hotIndex <- which(beauty$looks > 3)
beautyHot <- beauty[hotIndex, ]
notHotIndex <- which(beauty$looks <= 3)
beautyNotHot <- beauty[notHotIndex, ]

par(mfrow=c(1,2))

# For Hot people

with(beautyHot, plot(exper, lwage, cex = 2.5, pch = 21,main="Hot people", ylim=c(0,5)))
with(beautyHot, lines(smooth.spline(exper, lwage), col = "red", lwd=3))
legend("topright", c("Smooth spline", "Estimated quadratic"),
       col=c("red", "blue"), lwd=c(3,3))

experRange <- with(beautyHot, c(min(exper), max(exper)))
experPredict <- seq(from= experRange[1], to=experRange[2], length.out=100)

lm.log.quadratic = with(beautyHot, lm(lwage ~ exper + I(exper^2)))
betahatQuadratic.log <- lm.log.quadratic$coefficients

x <- experPredict
lwagePredicted <- betahatQuadratic.log[1] + betahatQuadratic.log[2]*x  + betahatQuadratic.log[3]*x^2
lines(experPredict, lwagePredicted, col="blue", lwd=3)


# For not Hot people

with(beautyNotHot, plot(exper, lwage, cex = 2.5, pch = 21, main="Not hot people", ylim=c(0,5)))
with(beautyNotHot, lines(smooth.spline(exper, lwage), col = "red", lwd=3))
legend("topright", c("Smooth spline", "Estimated quadratic"),
       col=c("red", "blue"), lwd=c(3,3))



experRange <- with(beautyNotHot, c(min(exper), max(exper)))
experPredict <- seq(from= experRange[1], to=experRange[2], length.out=100)

lm.log.quadratic = with(beautyNotHot, lm(lwage ~ exper + I(exper^2)))
betahatQuadratic.log <- lm.log.quadratic$coefficients

x <- experPredict
lwagePredicted <- betahatQuadratic.log[1] + betahatQuadratic.log[2]*x  + betahatQuadratic.log[3]*x^2
lines(experPredict, lwagePredicted, col="blue", lwd=3)

par(mfrow=c(1,1))

#B)

#Code to illustrate the Weierstrass approximation theorem (higher order "Taylor 
#expansions" work better and better with an increasing order)


#Let us consider the smooth spline function as the true trend (it is actually just an approximation!)
trendFunction <- smooth.spline(beautyHot$exper, beautyHot$lwage)
#From ?smooth.spline, we see that we can use predict to evaluate the estimated spline
#at arbitrary points:

#Let's check that this works as intended by using this function to re-produce the just produced plot:

x <- seq(min(beautyHot$exper), max(beautyHot$exper), length.out=100)
pred.obj <- predict(trendFunction, x)
#pred.obj includes both x and y values. We access the y-values like this:
pred.obj$y

with(beautyHot, plot(exper, lwage, cex = 2.5, pch = 21,main="Hot people", ylim=c(0,5)))
with(beautyHot, lines(smooth.spline(exper, lwage), col = "red", lwd=3))
lines(x,pred.obj$y, col = "green", lwd=3)
#verified.


#Let us now treat this x-y-pair as the noiseless population data:
#Fitting a linear regression model with polynomials in x to this dataset identifies the
#best fitting polynomial of the given order.

trend.data <- as.data.frame(pred.obj)

plot(trend.data, type="l")
#Let's fit a second and third order polynomial:

model2 <- lm(y ~ x + I(x^2), data = trend.data)
predict.model2 <- predict(model2, data.frame(x=x))
lines(x, predict.model2)


model3 <- lm(y ~ x + I(x^2) + I(x^3), data = trend.data)
predict.model3 <- predict(model3, data.frame(x=x))
lines(x, predict.model3, col="red")


#The third order polynomial fit is pretty good, but the mentioned result
#says that the approximation will continue to improve as we increase the 
#polynomial order (NOTE: as mentioned in the project, fitting high degree 
#polynomials directly is usually NOT a good idea!)

#Let us use a package which allows us to fit higher order polynomials
#using easy commands (not manually writing many terms of the form I(x^5) etc)

library(caret) #run first: install.packages("caret")
#gives access to the poly-function as below:

#Estimates a fifth order polynomial, with minimal work:
model5 <- lm(y ~ poly(x, 5, raw = TRUE), data = trend.data)
predict.model5 <- predict(model5, data.frame(x=x))
plot(trend.data)
lines(x, predict.model5,col="red")
#pretty good

#Can play around with the order:
p <- 8
model5 <- lm(y ~ poly(x, p, raw = TRUE), data = trend.data)
predict.model5 <- predict(model5, data.frame(x=x))
plot(trend.data)
lines(x, predict.model5,col="red")
#very close match


p <- 13
model5 <- lm(y ~ poly(x, p, raw = TRUE), data = trend.data)
predict.model5 <- predict(model5, data.frame(x=x))
plot(trend.data)
lines(x, predict.model5,col="red")
#near exact match

#Let's plot the trend as a line as well:
plot(trend.data, type="l")
lines(x, predict.model5,col="red")



#C
# plot the residuals for hot and not hot

#  Residuals normally have zero average

# For hot
lm.hotExper = with(beautyHot, lm(lwage ~ exper + I(exper^2) + hot))

residuals <- lm.hotExper$residuals

plot(beautyHot$exper, residuals, xlab="years of experience ", main="Hot group")
lines(smooth.spline(beautyHot$exper, residuals), col="red", lwd=3)
abline(h=0, lwd=2)

"looking at the smooth spline one can see that the residuals does not have zero
average. In general residuals should have zero average.
Meaning that the smooth spline curve should be flat, however it is not the case.
"

# For not hot:


lm.NotHotExper = with(beautyNotHot, lm(lwage ~ exper + I(exper^2)))

residuals <- lm.NotHotExper$residuals

plot(beautyNotHot$exper, residuals, xlab="years of experience ", main="Not Hot group")
lines(smooth.spline(beautyNotHot$exper, residuals), col="red", lwd=3)
abline(h=0, lwd=2)


"We can see that the residuals for the not hot group is exactly matches with the
abline h=0, on the y axis. meaning that the residauls are zero for the not hot group.
It indicates that there is no extreme high or extreme low observations in our data, that
might be invalid or by occured by random chance, 
and could potentially distort our regression."


#D
#Code from hint: #For the illustration in the hint:
lm.hotSimple <- with(beauty, lm(lwage ~ hot))
betahat <- lm.hotSimple$coefficients
Yhat <- betahat[1] + betahat[2]*beauty$hot
Y <- beauty$lwage
uhatManual <- Y - Yhat


#The modified code:
uhat <- lm.hotInteraction$residuals
betahat <- lm.hotInteraction$coefficients
Yhat <- betahat[1] + betahat[2]*beauty$hot + betahat[3]*beauty$exper + betahat[4]*beauty$exper^2 + betahat[5]*beauty$exper*beauty$hot + betahat[6]*beauty$exper^2*beauty$hot
Y <- beauty$lwage
uhatManual <- Y - Yhat
plot(uhat, uhatManual) #perfect match
abline(a=0,b=1) #the line y=x



#E,
# Estimate the following model for hot people:
# lwagei = β0 + β1experi + β2exper^2i + β3expe^3i + ui
# Based on the regression output does the cubic term seems important?

lm.HotCubic <- with(beautyHot, lm(lwage ~ exper + I(exper^2) + I(exper^3)))
summary(lm.HotCubic)
"Based on the regression output the cubic term is 1.20% which seems quite significant"

# Coefficients:
#               Estimate Std. Error t value Pr(>|t|) 
# (Intercept)  8.306e-01  1.075e-01   7.727 9.96e-14 ***
# exper        1.200e-01  2.180e-02   5.505 6.81e-08 ***
# I(exper^2)  -4.278e-03  1.215e-03  -3.519 0.000485 ***
# I(exper^3)   4.811e-05  1.906e-05   2.524 0.012019 *  


# let's see it for not hot people:

lm.NotHotCubic <- with(beautyNotHot, lm(lwage ~ exper + I(exper^2) + I(exper^3)))
summary(lm.NotHotCubic)

"The cubic term for the not hot model, seems not significant at all, 47.5%. "
# I(exper^3)   8.061e-06  1.129e-05   0.714 0.475606  


# in order to end the discussion and find out whether the cubic term is important to
# include in our model:
# Consider also the residuals for the "hot"-group vs the experience variable:

plot(beautyHot$exper, lm.HotCubic$residuals, main="hot group", ylab= "residuals with cubic term")
lines(smooth.spline(beautyHot$exper, lm.HotCubic$residuals), col="red", lwd=3)
abline(h=0, lwd=2)

" Looks great, the cubic model resolved the issue with the residuals compared to the,
quadratic model where it was not equal to zero.
With the cubic term the residuals are zero for the hot group,
thus we can conclude it is indeed needed in our model."

# for the sake of curiossity I'm gonna check the residauls for the not hot group
# with the cubic term.

plot(beautyNotHot$exper, lm.NotHotCubic$residuals, main = "Not hot group", ylab = "residuals with cubic term")
lines(smooth.spline(beautyNotHot$exper, lm.NotHotCubic$residuals), col="red", lwd=3)
abline(h=0)
# looks good, in this case the cubic term did not make any differece, since the residuals were
# already zero, with the quadratic term, the cubic term did not change anything.

#F, based on the the data for hot people, Make a scatter plot of lwage vs exerp.

# Hot group

plot(x=beautyHot$exper, y=beautyHot$lwage, cex = 2.5, pch = 21, main="Hot group",
     ylab="lwage", xlab="exper", ylim = c(-0.1,5))
lines(smooth.spline(x=beautyHot$exper, y=beautyHot$lwage), lwd=3, col="red")

# estimated trend from quadratic
lm.hot <- with(beautyHot, lm(lwage ~ exper + I(exper^2)))

x <- seq(from=min(beautyHot$exper), to=max(beautyHot$exper), length.out = 100)
y <- lm.hot$coefficients[1] + lm.hot$coefficients[2]*x + lm.hot$coefficients[3] * x^2
lines(x, y,col="blue", lwd=3)


# estimated trend from cubic
x <- seq(from=min(beautyHot$exper), to=max(beautyHot$exper), length.out = 100)
y <- lm.HotCubic$coefficients[1] + lm.HotCubic$coefficients[2]*x + lm.HotCubic$coefficients[3] * x^2 + lm.HotCubic$coefficients[4] * x^3
lines(x, y,col="green", lwd=3)
legend("topright", legend = c("Smoother", 'Estimated Trend Quadtratic', 'Estimated Trend Cubic'), lwd=3, col = c("red", "blue", "green"))



# Not Hot group

summary(lm.NotHotCubic)

plot(x=beautyNotHot$exper, y=beautyNotHot$lwage, cex = 2.5, pch = 21, main="Not Hot group",
     ylab="lwage", xlab="exper", ylim = c(-0.1,5))
lines(smooth.spline(x=beautyNotHot$exper, y=beautyNotHot$lwage), lwd=3, col="red")

# estimated trend from cubic
lm.NotHotCubic
x <- seq(min(beautyNotHot$exper), max(beautyNotHot$exper), length.out=100)
y <- lm.notHotCubic$coefficients[1] + lm.notHotCubic$coefficients[2]*x + lm.notHotCubic$coefficients[3]*x^2 + lm.notHotCubic$coefficients[4]*x^3
lines(x,y, col="green", lwd=3)


# Estimated trend from quadratic

lm.notHot
x <- seq(min(beautyNotHot$exper), max(beautyNotHot$exper), length.out=100)
y <- lm.notHot$coefficients[1] + lm.notHot$coefficients[2]*x + lm.notHot$coefficients[3]*x^2
lines(x,y, col="blue", lwd=3)
legend("topright", legend = c("Smoother", 'Estimated Trend Quadtratic', 'Estimated Trend Cubic'), lwd=3, col = c("red", "blue", "green"))




#G, Compare the baseline model lwagei = β0 + β1experi + β2exper2i + u
# with a model where hot people have a cubic trend and not hot people have only 
# a quadratic trend in experience. 
# lwagei = β0 + β1hoti + β2experi + β3exper2i + β4hoti · experi
# + β5hot · exper2i + β6hot · exper3i + ui


lm.hotInteractionCubic <- with(beauty, lm(lwage ~ hot + exper + I(exper^2)  + hot*exper + hot*I(exper^2) + hot*I(exper^3) - I(exper^3)))


summary(lm.hotNoHotness)   # F-statistic: 98.68 
summary(lm.hotInteractionCubic) # F-statistic: 35.36%

# ask steffen wheter it is correct 
"Comparing the two models based on the F statistics
the model with a cubic term scored much less than the model with the quadratic term.
The F-statistics tells us the overall health of our model, 
and it might happen by chance that some of the terms that we added are having 
statistically significant p-values, but in reality it might not be the case,
or the terms are correlated.
"

anova(lm.hotInteractionCubic, lm.hotNoHotness)
#p-value is now 1%, much lower than the first p-value
# of 0.1245. From this test, it would appear that we
#have evidence for a difference in the trend between
#not hot and hot people.

# H , Test for multicolinearity

cor(beauty$exper, beauty$exper^2) # 0.96 correlation
cor(beauty$exper, beauty$exper^3) # 0.90 correlation
cor(beauty$exper^2, beauty$exper^3) # 0.98 correlation

" all of the terms are highly correcelated, which indicate  multicolinearity."

#Based on plots, supported mainly by the smooth spline approximation,
# I would have preferred the model with a quadratic trend for not hot people, 
#and a cubic trend for hot people, that is, the model lm.hotInteractionCubic,


