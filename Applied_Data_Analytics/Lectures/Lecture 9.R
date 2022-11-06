
#Lecture 9
# simple and multiple linear regression.
rm(list=ls()) # clears the memory



house_data = read.table("https://raw.githubusercontent.com/adamsky777/R_projects/main/Data/house-prices.csv",
                      header = TRUE, sep=",")

house_data$SqM = round(house_data$SqFt * 0.093)

#using the lm command we create a linear model Price against sqm.
house_lm = with(house_data, lm(Price ~ SqM))

summary(house_lm)

"to predict the house price with 182 SqM 
we create a new datafreame with SqM = 182"
house_182 = data.frame("SqM" = 182)

"Then we use the predict command, and we invoke the our linear model's data that 
we saved previously, then asssigning the the previously created data frame to our new data."
predict(house_lm, newdata = house_182)
house_182


# Multiple Linear Regression

rm(list=ls()) # clears the memory


beauty = read.table("https://raw.githubusercontent.com/adamsky777/R_projects/main/Data/beauty.csv",
                    , sep=",", header = TRUE)

hist(beauty$wage)
hist(beauty$lwage)
boxplot(beauty$wage)

# there  is one extreme observation. We will for now just remove,
# we will retrun this obs. at later stage.

which(beauty$wage <60)
beautyReduced  = beauty[which(beauty$wage < 60),]


with(beautyReduced, plot(exper, wage))
with(beautyReduced, lines(smooth.spline(exper, wage), col = "red", lwd=3))

par(mfrow=c(2,1))
with(beautyReduced, hist(wage))
with(beautyReduced, hist(lwage))
par(mfrow=c(1,1))

#What is happening when taking log? Let us first
#plot f(x) = log(x) in the relevant region:


with(beautyReduced, plot(exper, lwage))
with(beautyReduced, lines(smooth.spline(exper, lwage), col = "red", lwd=3))

lm.qudratic <- with(beautyReduced, lm(lwage ~ exper + I(exper^2)))
"NB: it is also feasible to calculate the experience ^2 
and save it then we don't need to add the I command above."

summary(lm.qudratic)

maxVal <- with(beautyReduced, max(wage))
minVal <- with(beautyReduced, min(wage))
x <- seq(minVal+0.01, maxVal, length.out=100)
y <- log(x)
plot(x, y, type="l", ylim=c(min(y),max(x)))
points(x,x, type="l")

#We push large observations close together,
#hence going from a very right-skewed distribution
#to a more symmetric distribution.

with(beautyReduced, plot(exper, lwage))
with(beautyReduced, lines(smooth.spline(exper, lwage), col = "red", lwd=3))


lm.simple = with(beautyReduced, lm(lwage ~ exper))
abline(lm.simple, col="blue", lwd=3)
# simple linear regression models, as it plots -lines- and not curves.
# the blue line is not a trend in this case!
summary(lm.simple)


#Output:
#
# Call:
#   lm(formula = lwage ~ exper)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -1.7737 -0.3470  0.0188  0.3607  2.1072 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 1.376556   0.028786   47.82   <2e-16 ***
#   exper       0.015379   0.001321   11.64   <2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.5607 on 1257 degrees of freedom
# Multiple R-squared:  0.09732,	Adjusted R-squared:  0.0966 
# F-statistic: 135.5 on 1 and 1257 DF,  p-value: < 2.2e-16



lm.qudratic <- with(beautyReduced, lm(lwage ~ exper + I(exper^2)))
summary(lm.qudratic)

names(lm.qudratic) # display all the elements that are accessible in the object.

betahat = lm.qudratic$coefficients
betahat[1]
betahat[2]
betahat[3]


expRange = c(min(beautyReduced$exper), max(beauty$exper))

expPredic <- seq(from=expRange[1], to=expRange[2], length.out=100)
lwagePredicted <- betahat[1] + betahat[2]*expPredic + betahat[3] * (expPredic^2)

with(beautyReduced, plot(exper, lwage))
with(beautyReduced, lines(smooth.spline(exper, lwage), col="red", lwd=3))
points(expPredic, lwagePredicted, col="green", lwd=3, type="l" )
abline(lm.simple, col="blue", lwd=3)
legend("topleft", legend = c("Smoother", "Simple Linear Regressin", 
                             "Quadratic Linear Regression"), 
       col=c("red", "green", "blue"), lwd=3)



# METHOD 2: Using the "predict"

expRange = with(beautyReduced, c(min(exper), max(exper)))
# seq creates evenly spaced numbers between the intervals.

expPredic <- seq(from= expRange[1], to=expRange[2], length.out=100)
lwagePredicted <- predict(lm.qudratic, data.frame(exper=expPredic))

#The two methods give the same result:
with(beautyReduced, plot(exper, lwage))
with(beautyReduced, lines(smooth.spline(exper, lwage), col = "red", lwd=3))
points(expPredic, lwagePredicted, col="green", lwd=3, type="l")
abline(lm.simple, col="blue", lwd=3) #NB: abline is restricted to
legend("topleft", legend=c("Smoother", "Simple linear regression", "Quadratic linear regression"), lwd=3, col=c("red", "blue", "green"))



#As a final illustration, let us look at a simple vizual interpretation
#of the quadratic term.
with(beautyReduced, plot(exper, lwage))
with(beautyReduced, lines(smooth.spline(exper, lwage), col = "red", lwd=3))
straightLine <- betahat[1] + betahat[2]*expPredic #+ betahat[3]*(expPredict^2)
#NB: straightLine is based on the lm.quadratic estimates, and I have
#simply taken away the quadratic term.
points(expPredic, straightLine, type="l", col="red")
betahat[3] #-0.0008801415, a small negative number, which becomes important
#as expPredict^2 increases.


summary(lm.simple)
summary(lm.quadratic)

#can also access R^2 using the following commands:
lm.summary.simple <- summary(lm.simple)

lm.summary.simple$r.squared #9.7%

lm.summary.quadratic <- summary(lm.qudratic)
lm.summary.quadratic$r.squared #14%

betahat = lm.qudratic$coefficients
expRange = with(beautyReduced, c(min(exper), max(exper)))
expPredic <- seq(from=expRange[1], to=expRange[2], length.out=100)
lwagePredicted <- betahat[1] + betahat[2] * expPredic + betahat[3] *(expPredic^2)
with(beautyReduced, plot(exper, lwage, pch=21, cex=0.5))
points(expPredic, lwagePredicted, type = "l", col="green",lwd=3)


# residuals for OLS -estimates:
Yhat <- betahat[1] + betahat[2]*beautyReduced$exper + betahat[3] * (beautyReduced$exper^2)
Y = beautyReduced$lwage
u = Y -Yhat
sum(u^2) # sum of squared residuals.

b0 = betahat[1] *1.4
b1 = betahat[2] *1.4
b2 = betahat[3] *1.4
YhatTest = b0+ b1* beautyReduced$exper + b2 * (beautyReduced$exper^2)
uTest = Y - YhatTest
sum(uTest^2)

lwagePredictedTest <- b0 + b1*expPredic + b2*(expPredic^2)
points(expPredic, lwagePredictedTest, type="l", col="blue", lwd=3)

# the blue line is a blown up version of the green just as a representations.

