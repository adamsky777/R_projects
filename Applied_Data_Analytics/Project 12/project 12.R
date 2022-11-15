rm(list=ls())
beauty = read.table("https://raw.githubusercontent.com/adamsky777/R_projects/main/Data/beauty.csv",
                    sep=",", header = TRUE)

colnames(beauty)

#A)
beauty$hot <- (beauty$looks > 3)*1
lm.interaction <- with(beauty, lm(lwage ~ hot + female + hot*female))
summary(lm.interaction)
# Identify the p-value for the test  H0 :β3 = 0, versus HA :β3 ≠ 0.

"Looking at the p-values for β3 (hot female) we can see that 0.28 is not significant
 at any level 1%, 5%, hence we can conclude that we did not find statstical 
 evidence to reject H0. (there is no evidence that 'd say that this number is 
 different from zero.)
"




#B)
# Estimate the same model as in the previous task, but without the interaction term. 
# Place the resulting estimated model in lm.simple.

lm.simple <- with(beauty, lm(lwage ~ hot + female))


# C,
anova(lm.interaction, lm.simple)
# the p value mathces with A  28.6%

# H0 : The simplest model holds, versus HA : The most complex model holds
# Here we basically want to test which model is valid, which model make sense to use.
# We use Anova test which gives us the p-values for both models.
# One can see that the interaction model (hot*female) has p-value of 28% which 
# high p value for the second model means that, we did not find evidence against
# H0, the simple model holds.

# D, 
# Create two new data-frames: One containing only people with looks > 3), 
# and one with the rest. Let us for ease of reference call those with looks > 3 for hot.

# to clarify we have already hot in the dataframe where looks above 3  beauty hot = 1
all.equal((beauty$hot == 1), (beauty$looks >3))


hotIndex <- which(beauty$hot == 1)
notHotIndex <- which(beauty$hot == 0)

beautyHot <- beauty[hotIndex, ]
beautyNotHot <- beauty[notHotIndex,]

par(mfrow=c(1,2))
plot(beautyHot$exper, beautyHot$lwage, main="Hot", ylim = c(
  min(beautyNotHot$lwage), max(beautyNotHot$lwage)),
                              xlab = "Experience", 
                              ylab = "Log Wage",
                              cex = 2.5, pch = 21)
lines(smooth.spline(beautyHot$exper, beautyHot$lwage), lwd=3, col="red")

plot(beautyNotHot$exper, beautyNotHot$lwage, main="Not Hot", ylim = c(
  min(beautyNotHot$lwage), max(beautyNotHot$lwage)),
                               xlab = "Experience", 
                               ylab = "Log Wage",
                              cex = 2.5, pch = 21)
lines(smooth.spline(beautyNotHot$exper, beautyNotHot$lwage), lwd=3, col="red")

par(mfrow=c(1,1))


# E,
# Estimate the model for both dataframes:
# Yi = β0 + β1experi + β2exper2i + ui

lm.hot <-  lm(beautyHot$lwage ~ beautyHot$exper + I(beautyHot$exper^2))
summary(lm.hot)
# experience seems really significant (p-value close to zero)
# when we test it in our model, for people
# with looks above 3. Seems experience should be included in the model.


lm.notHot <-  lm(beautyNotHot$lwage ~ beautyNotHot$exper + I(beautyNotHot$exper^2))
summary(lm.notHot)
# can we say this ?
# experience in this case also really significiant (p-value close to zero),
# with looks below 3 experience should be also included in the model when testing for
# logwage.

# The coefficients are somewhat different, especially the quadratic
# coefficients (i.e., the coefficients in from of the square term).

# F,

lm.hotInderaction <- with(beauty, lm(lwage ~ hot + exper + I(exper^2)  + hot*exper + hot*I(exper^2)))
summary(lm.hotInderaction)

# The interaction effects are (individually) significant at the 
# 5% level, which indicates the need for interaction effects.
# The p-values are both just under 5%, so we do not have very
# strong evidence for requiring the interaction terms.


# G, Show that the predictions of the model estimated in Task F is the same as 
# those in Task E.


betahat.interaction <- lm.hotInderaction$coefficients
betahat.hot <- lm.hot$coefficients
betahat.nothot <- lm.notHot$coefficients

betahat.interaction
betahat.hot

betahat.hot
betahat.interaction
all.equal((betahat.interaction[1] + betahat.interaction[2]),betahat.hot[1])


(betahat.interaction[3] + betahat.interaction[5])
betahat.hot[2]


betahat.interaction[4] + betahat.interaction[6]
betahat.hot[3]
#which is also a match.


# H,
lm.hotNoInderaction <- with(beauty, lm(lwage ~ hot + exper + I(exper^2)))

anova(lm.hotInderaction, lm.hotNoInderaction)
# high p-value (12.45%) for the second model indicates that interatcitons
# are not significant -> they are not important in our model.
# This is not the same conclusion as when we look at each 
# interaction coefficient one by one. Such discrepancies may occur,
# as we are testing different things. We will follow this up in the
# next lecture's project.


















