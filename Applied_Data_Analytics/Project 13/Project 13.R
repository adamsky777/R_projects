rm(list=ls())


achievement <- read.table("https://raw.githubusercontent.com/adamsky777/R_projects/main/Data/achievement_scores.csv",
           sep = ",", header = TRUE)

View(achievement)
#A,
hist(achievement$x)
hist(achievement$y)
#NOTE: Since this is ordinal data, we should strictly speaking
#make bar-plots and not histograms. This can be done as follows.


par(mfrow=c(2,1))
barplot(table(achievement$x), main="x")
barplot(table(achievement$y),main="y")
par(mfrow=c(1,1))

# main difference is the scale of the test. 
# aptitude test (x) was graded on a scale 1-5 whereas 
# the academic achievemnt scores were registered on a 1-9 scale.
# or up to 10? but no one scored 10.

#C, 

plot(achievement$x, achievement$y, xlab = "Before training", ylab = "After traininig", 
     col=achievement$method)

# We don't have so many observations, especially in the lower range. 
# Method 2 scoored the highest before and after training altough we can't conclude 
# so much since we don't have enough observations.
# We also see that there are points on the top of each other thefore we ought to 
# add some random noise.



n <- length(achievement$x)

set.seed(4111)
plot(achievement$x + runif(n, -0.2, 0.2), achievement$y + runif(n, -0.2, 0.2), xlab = "Before training", ylab = "After traininig", 
     col=achievement$method, cex=1, pch=21)

#D, make an indicator variable for the 3 training method:


achievement$method1 <- (achievement$method == 1)*1 
achievement$method2 <- (achievement$method == 2)*1 
achievement$method3 <- (achievement$method == 3)*1 

mean(achievement$x[(which(achievement$method1 == 1))])  #2.142857
mean(achievement$x[(which(achievement$method2 == 1))])  #3.428571
mean(achievement$x[(which(achievement$method3 == 1))])  #2.714286


mean(achievement$y[(which(achievement$method1 == 1))]) #4.428571
mean(achievement$y[(which(achievement$method2 == 1))]) #7.571429
mean(achievement$y[(which(achievement$method3 == 1))]) #6.714286

# correlation XY for method1 
cor((achievement$x[(which(achievement$method1 == 1))]),
                     (achievement$y[(which(achievement$method1 == 1))])) # 0.9240617

# correlation XY for method2 
cor((achievement$x[(which(achievement$method2 == 1))]),
                      (achievement$y[(which(achievement$method2 == 1))])) # 0.7326066

# correlation XY for method3 
cor((achievement$x[(which(achievement$method3 == 1))]),
                      (achievement$y[(which(achievement$method3 == 1))])) # 0.697378

# plotting them into one plot for easy comparison.
par(mfrow=c(3,1))

plot(
  achievement$x[which(achievement$method1 == 1)],
  achievement$y[which(achievement$method1 == 1)],
  xlim = c(min(achievement$x), max(achievement$x)),
  ylim = c(min(achievement$y), max(achievement$y)), col=1, main = "Method1"
)


plot(
  achievement$x[which(achievement$method2 == 1)],
  achievement$y[which(achievement$method2 == 1)],
  xlim = c(min(achievement$x), max(achievement$x)),
  ylim = c(min(achievement$y), max(achievement$y)), col=2, main = "Method2"
)

plot(
  achievement$x[which(achievement$method3 == 1)],
  achievement$y[which(achievement$method3 == 1)],
  xlim = c(min(achievement$x), max(achievement$x)),
  ylim = c(min(achievement$y), max(achievement$y)), col=3, main = "Method3"
)



" one can see that method2 has the highest scoore among the other training methods,
altought it may not justifies that method 2 is the best training method amongst the  
other training methods.
"

par(mfrow=c(1,1))

# plotting them separately

plot(
  achievement$x[which(achievement$method1 == 1)],
  achievement$y[which(achievement$method1 == 1)],
  xlim = c(min(achievement$x), max(achievement$x)),
  ylim = c(min(achievement$y), max(achievement$y)), col=1, main = "Method1"
)


plot(
  achievement$x[which(achievement$method2 == 1)],
  achievement$y[which(achievement$method2 == 1)],
  xlim = c(min(achievement$x), max(achievement$x)),
  ylim = c(min(achievement$y), max(achievement$y)), col=2, main = "Method2"
)

plot(
  achievement$x[which(achievement$method3 == 1)],
  achievement$y[which(achievement$method3 == 1)],
  xlim = c(min(achievement$x), max(achievement$x)),
  ylim = c(min(achievement$y), max(achievement$y)), col=3, main = "Method3"
)


# E, for each of the training methods estimate the model:

lm.simple1 <- lm(achievement$y[which(achievement$method1 == 1)] ~ achievement$x[which(achievement$method1 == 1)])
lm.simple2 <- lm(achievement$y[which(achievement$method2 == 1)] ~ achievement$x[which(achievement$method2 == 1)])
lm.simple3 <- lm(achievement$y[which(achievement$method3 == 1)] ~ achievement$x[which(achievement$method3 == 1)])


par(mfrow=c(1,1))

plot(achievement$x, achievement$y, xlab = "Before training", ylab = "After traininig", 
     col=achievement$method)
abline(lm.simple1, col=1)
abline(lm.simple2, col=2)
abline(lm.simple3, col=3)
legend("topleft", 
       c("Training method 1", "Training method 2","Training method 3"),
       lty=c(1,1,1),
       col=c(1,2,3))
"Method 1 has the lowest score. Method 3 scores bettr than method 1 and finally,
Method 2 is marginally better than method 3.
Notice also that method 2 and 3 has parallel lines." 


#F, 


lm(achievement$y ~  achievement$x + achievement$method1 + achievement$method2 + achievement$method3)
"
in the previous models we considered each group individually, 
however in the model above we included all training methods hence the intercept varies.
"
lm.simple1$coefficients
lm.simple2$coefficients 
lm.simple3$coefficients

#The effect from the aptitude test is constrained to be the same for
#all teaching methods. For each method, the estimated relations between
#x and y are still straight lines, but only their intercepts can differ.


# G, Estimate the model using the full dataset.

lm.noInteraction <- (with(achievement, (lm(y ~ x + method1 + method2))))


lm.Interaction <- (with(achievement, (lm(y ~ x + method1 + method2 
                                         + method1 * x + method2 * x))))
anova(lm.noInteraction, lm.Interaction)
# we can see from the high p value for the interaction terms 60.52% meaning that,
# the interaction terms are not important.


#H,

plot(achievement$x, achievement$y, xlab = "Before training", ylab = "After traininig", 
     col=achievement$method)


betahat <- coef(lm.noInteraction)

betahat

#For group 1:
abline(a=betahat[1]+betahat[3], b=betahat[2], col=1)

#For group 2:
abline(a=betahat[1]+betahat[4], b=betahat[2], col=2)

#For group 3
abline(a=betahat[1], b=betahat[2], col=3)

legend("topleft",
       c("Training method 1", "Training method 2","Training method 3"),
       lty=c(1,1,1),
       col=c(1,2,3)
)

#I,
lm.noInteraction
lm.noInteraction$residuals
hist(lm.noInteraction$residuals)

# Easier to assess normality with the so-called QQ-plot
qqnorm(lm.noInteraction$residuals)

# The residuals show modest deviations from normality, but there 
#is no cause for serious concern.

























