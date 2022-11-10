# Project 10
"Assignment 1"

rm(list=ls())
heights = read.table("https://raw.githubusercontent.com/adamsky777/R_projects/main/Data/heights.csv",
           sep=",", header = TRUE)

plot(heights)
View(heights)
set.seed(4110)
n = length(heights$mother)
rand_unif = runif(n, -0.5, 0.5)
heights$mother_mod <- (heights$mother + rand_unif) * 0.0254

rand_unif2 = runif(n, -0.5, 0.5)
heights$daughter_mod <- (heights$daughter + rand_unif2)  * 0.0254


# mother
plot(x= heights$mother_mod, y=heights$daughter_mod)

#B

heights$mother <- heights$mother * 0.0254
heights$daughter <- heights$daughter * 0.0254

plot(x=heights$mother, y=heights$daughter)
lines(smooth.spline(x=heights$mother,y=heights$daughter), col="red", lwd=3)

# seems like there is a trend in the data linear regression would work.


heights_lm = lm(heights$daughter ~  heights$mother)
abline(heights_lm, col="green", lwd=1)

#D
b0 = 0
b1= 1
abline(b0,b1)

#The y = x line would be the result if mothers on
#average get daughters that are equally tall as them.
#But this is not the trend estimated in data.
#Indeed, the estimated trend indicate that very tall
#mothers get somewhat shorter daughters, and very
#short mothers get daughters that are somewhat taller.
#This effect is often observed in biology, and 
#has been described as "regression towards 
#mediocracy", and is the etymological root of the name
#of "linear regression". 


# E,

summary(heights_lm)
confint(heights_lm)
#The p-value is practically zero. There is a positive effect
#from the height of the mother to the height of the child.
# In other words the mother influences the daughters height.

# with alpha 5% we can say that it is 97% is the case.


"Assignment 2"
#A 
rm(list=ls()) # clears the memory

brain_data = read.table("https://raw.githubusercontent.com/adamsky777/R_projects/main/Data/BrainSize.tsv",
           header = TRUE, 
           sep    = "\t")

brain_data[brain_data < 0] = NA # assign NA to values that are less than zero in the dataframe
brain_data = na.omit(brain_data) # omit records with NA from the dataframe.


n = length(brain_data$Height)  # first getting the sample size
rownames(brain_data) = (1:n) #assigning the new row names based on the sample size.

cm_multiplier <- 2.54
kg_multiplier <- 0.45

brain_data$WeightKg = round(brain_data$Weight * kg_multiplier)
brain_data$HeightCm = round(brain_data$Height * cm_multiplier, 1)


n = length(brain_data$Height)  # first getting the sample size
rownames(brain_data) = (1:n) #assigning the new row names based on the sample size.
View(brain_data)


hist(brain_data$HeightCm)
hist(brain_data$HeightCm[brain_data$Gender == "Male"], add=TRUE, col="red")


#B,

hist(brain_data$FSIQ)
boxplot(brain_data$FSIQ)
" the box-plot is tend to be less impformative to use in this case,
Since it does not show the frequency of the observations, additionally, 
it is hard to imagine at which FSIQ we have no records.
Generally, saying boxplot would not be a good choice in this case.
We here see clearly that there are no observations within
a region, and we see clear indication of a two-group situation, which means 
we need to be careful. 

"



#B, the line in the boxplot is the media, confrim by letting R to calculate it.

boxplot(brain_data$FSIQ)

median(brain_data$FSIQ)
points(median(brain_data$FSIQ), col="red")


# calculating th median by hand 
sorted_FSIQ = sort(brain_data$FSIQ)

length(sort(brain_data$FSIQ))
"
since the lenght of out set is positive, it means that we need to find the 
number located in the center plus also use the number which is located 1+ to the center.
"
center = length(sort(brain_data$FSIQ)) / 2 


median_by_hand =  (sorted_FSIQ[center] + sorted_FSIQ[center+1]) / 2
median_by_hand == median(brain_data$FSIQ)


#C

plot(y=brain_data$FSIQ, x=brain_data$VIQ)
mean(brain_data$FSIQ)
abline(h=mean(brain_data$FSIQ), col="red")
abline(v=mean(brain_data$VIQ), col="red")

" the averages does not represent typical observations.
it looks like the observations either located in the high or low extreme ranges
but not in the middle."


#D,
highIQ_indexes <- which((brain_data$FSIQ >= 130))

highFSIQ <- brain_data$FSIQ[highIQ_indexes]
highVIQ <- brain_data$VIQ[highIQ_indexes]
plot(highVIQ, highFSIQ)
abline(h=mean(highFSIQ), col="red")
abline(v=mean(highVIQ),col="red")

# when looking at the observations we can see that they are closer to the averages, 
# however we still see some extreme values, located far from the center.
#The means are centres of the distributions for both variables. 





restIQ_indexes <- which((brain_data$FSIQ < 130))
restFSIQ <- brain_data$FSIQ[restIQ_indexes]
restVIQ <- brain_data$VIQ[restIQ_indexes]
plot(restVIQ, restFSIQ, )
abline(h=mean(restFSIQ), col="red")
abline(v=mean(restVIQ), col="red")

# at the restIQ sub-group, the observations are centered at the middle,
# they are close to the average.

#E, compute the correlation
cor(brain_data$FSIQ, brain_data$VIQ)
# correlation between FSIQ and VIQ is really high 0.95

cor(brain_data$FSIQ[highIQ_indexes], brain_data$VIQ[highIQ_indexes])
"correlation between FSIQ and VIQ between for the sub-group where FSIQ is above 130,
the correlation dropped to 0.62, still positive correlation but less than
for the whole group."

cor(brain_data$FSIQ[restIQ_indexes], brain_data$VIQ[restIQ_indexes])
# for the rest of the group, is positively corrlated 0.7

"Conclusion : the correlation for the group is much higher 
than the correlation for each sub group."

#F,
FSIQ_centered = scale(brain_data$FSIQ)
VIQ_centered = scale(brain_data$VIQ)

pointSize = abs(FSIQ_centered * VIQ_centered) 
plot(FSIQ_centered, VIQ_centered,cex = pointSize, pch = 21, bg = (prodXY <= 0)) # change the points color with negative contribution
abline(h=0)
abline(v=0)
legend("topleft", legend="negativ contribution area", col="blue", lwd=2)
legend("bottomright", legend="negativ contribution area", col="blue", lwd=2)
legend(x=0,y=1.73, legend="post. cont. area", pch=3, col="red",)
legend(x=-0.8, y=-1.5, legend="post. cont. area", pch=3, col="red",)


" as we can see the the small dots have little influnce on the correlation,
whereas large dots have significantly higher influence on the correlation.
The large dots dominate the sum and have more infuence."

#There are no negative contributions to the correlation, as there are no points
#in the upper left or lower right quadrant. Most of the contributions
#are large and positive (the sign of the contributions are seen by the 
#quadrants the points are in). It appears that the reason for this is
#the observations that are removed, which therefore inflates the correlation.


#G, 
# Simulate the missing data:

set.seed(4110) 
require(MASS) 
rho <- 0.7
sigmaX <- 10
sigmaY <- 10
Sigma <- matrix(c(sigmaX^2,rho*sigmaX*sigmaY,rho*sigmaX*sigmaY,sigmaY^2), nrow=2, ncol=2, byrow=TRUE)
muX <- 115
muY <- 115
mu <- c(muX,muY) 
n <- 200
simData <- mvrnorm(n = n, mu=mu, Sigma = Sigma)
simData <- data.frame(X = simData[,1], Y = simData[,2]) #enables us to refer to simData$X, simdata$Y

with(simData, plot(X, Y)) #does the same as plot(simData$X, simData$Y)
abline(h=130)
abline(h=103)



#i)

#We re-use the code above.
#First, for the full data:
Xstandardized <- scale(simData$X)
Ystandardized <- scale(simData$Y)
prodXY <- brain_data$FSIQ * brain_data$VIQ
pointSize = abs(Xstandardized * Ystandardized) 
plot(Xstandardized, Ystandardized,cex = pointSize, pch = 21, bg = (prodXY <= 0))
abline(h=0)
abline(v=0)


# With reduced data

"we want to standardize the values for the sumulated data where
Y >=130 or Y < 103, we need the data for X and Y as well hence"

"NB specifying the col name at the and will return the column of the dataframe 
$X and $Y"


Xstandardized <- scale(simData[simData$Y >= 130 | simData$Y < 103,]$X)
Ystandardized <- scale(simData[simData$Y >= 130 | simData$Y < 103,]$Y)
prodXY = abs(brain_data$FSIQ * brain_data$VIQ)
pointSize = abs(Xstandardized * Ystandardized) 
plot(Xstandardized, Ystandardized,cex = pointSize, pch = 21, bg = (prodXY <= 0))
abline(h=0)
abline(v=0)


#ii,
"Compute the correlation for all observations, and compute it for for the reduced data."

cor(simData$X, simData$Y)
#0.71

cor(simData$X[simData$Y <= 103], simData$Y[simData$Y <= 103])
#0.27

cor(simData$X[simData$Y >=130], simData$Y[simData$Y >= 130])
#0.28


cor(simData$X[simData$Y >=130 | simData$Y <= 103], simData$Y[
  simData$Y >= 130 | simData$Y <= 103 ])
#0.84

#When it comes to the Simulated Data, the relation between the three other 
#cases is similar to the original data example,
#except, now we know what the population correlation for the
#full correlation is, and we know that two are too low, and one is too high:


#iii)

#The pattern in the plot for the reduced data is similar to the 
#plot using the dataset. We see an inflation of the correlation
#caused by the "gap", and most negative and small contributions
#are removed. 
#In the data example, it appears likely that the true 
#correlation is somewhere in between the high and the two low estimates.
#The simulation makes this story more plausible.
#A mathematical analysis of the problem would be required
#to further pin-point the true correlation. 

#H,
"brais size - MRI_count
Intelligence - FSIQ"
# Does simple linar regression seem reasoable for the two groups?

# high IQ

plot(brain_data$MRI_Count[highIQ_indexes], brain_data$FSIQ[highIQ_indexes], 
     cex = 2.5, pch = 21)

"First looking at the plot we can see no deviations from linearity.
Linear trend is visible but not strong, sample size is small 19"
length(brain_data$MRI_Count[highIQ_indexes])

#plotting the smooth.spline to see the linearity.
lines(smooth.spline(brain_data$MRI_Count[highIQ_indexes], 
                   brain_data$FSIQ[highIQ_indexes]), col="red", lwd=3)

#The smooth spline plot agrees with this assessment, producing a perfect straight line.


# rest IQ
plot(brain_data$MRI_Count[restIQ_indexes], brain_data$FSIQ[restIQ_indexes],
     cex = 2.5, pch =21)

length(brain_data$MRI_Count[restIQ_indexes])

lines(smooth.spline(brain_data$MRI_Count[restIQ_indexes], 
                     brain_data$FSIQ[restIQ_indexes]), col="red", lwd=3)

"The same can be said in this case, a weak trend, and at this sample size
not easy to say much more than that the trend appears compatible with a straight
line plus noise trend."

# fit both models:

# low IQ

brain_lm_low = lm(brain_data$FSIQ[restIQ_indexes] ~ brain_data$MRI_Count[restIQ_indexes])
summary(brain_lm_low)
 # beta hat1 = 6.485e-05

"Based on this output, the estimated relationship
is (the estimate) FSIQ hat  = 3.308e+01 + 6.485e-05* MRI_Count

The p-value of MRI_Count is 1.68%, therefore it is indicating a statistical proof
against the statment of the null hypothersis that says
no effect between itelligence and brain size."

brain_lm_high = lm(brain_data$FSIQ[highIQ_indexes] ~ brain_data$MRI_Count[highIQ_indexes])
summary(brain_lm_high)
#beta hat1 = 2.751e-05

"based on the output the estimated relationship is (the
estimate ) FSIQ hat = 1.108e  + 2.751e-05 * MRI_Count

The p-value of MRI_Count is quite low 2.1%, therefore it is indicating a
statisitcal proof against the stament of the H0 which says that there is zero effect
between intelligent and brain size.
"
"Hence we can conclude that we have found eveidence that intelligence is positively
influenced by brain size, altough we must note that in order to precisely declare the degree
of this effect further analysis needed and other variables need to be considered into 
the model.

The effect on the group with low IQ  (beta hat1 = 6.485e-05
is somewhat larger than the effect on the group with highIQ beta hat1 = 2.751e-05".

"When it comes to the R^2 both model has low Rsquared 29% and 27.5%, meaning
there is noise in the data the model is not perfect."

plot(brain_data$MRI_Count[highIQ_indexes], brain_data$FSIQ[highIQ_indexes], 
     cex=2.5, pch=21)

#plotting the regression.
abline(brain_lm_high)

"Example how to estimate based on the model."
# estimating the IQ based on the model for someone with brain size 950000 MRI_Count 
# Yhat = b0 hat + b1 hat * MRI_Count
110.8+(2.751 * (10 ^ (-05))*950000)

"Notice the very high scale of the MRI_count-variable. Even if
the hat beta is a small number, it is multiplied by a large number,
and we visually see that the effect is large.
If the MRI_count increases from 850 000 to 900 000, which is an increase
that is practically relevant considering the variation in the variable
as seen from the plot, the model predicts that the average IQ increase
is"
beta1hat <- coef(brain_lm_high)[2]
beta1hat #2.75126e-05
beta1hat*(900000 - 850000) #1.37563
#which is not a very large increase. 

plot(brain_data$MRI_Count[restIQ_indexes], brain_data$FSIQ[restIQ_indexes], 
     cex=2.5, pch=21)

abline(brain_lm_low)

" plotting the regression line for the group with low FSIQ indicates that
the line touches few points, meaning there our "


#Here, the scale of the FSIQ variable is higher:
beta1hat <- coef(brain_lm_low)[2]
beta1hat #6.484842e-05
beta1hat*(900000 - 850000) #3.242421
#Also visually, we see that the estimated average increase
#plays a larger role. Again, this is just average effects,
#without taking into account any other variables, and so
#we are not estimateding causal effects. 


#I, We want to use the linear regression model to approximate a prerson's FSIQ 
# based on MRI scan. That is, suppose we want to predict the person's FSIQ when
# we know the person's MRI_Count, but we don't know the FSIQ.
# Explain why the models in Task H can't be used in that context.

"We have two models for each group, therefore we don't know which model we should use,
to, we have no knowledge on which group the person would belong to, since we don't 
know the IQ of the person."

#We have two models, one for the high IQ group, and one for the non-high IQ group.
#Since we would like to use MRI_count to predict IQ, i.e., brain size
#to predict intelligence, we do not know the IQ of the person at hand,
#and so we do not know which model to use. We therefore cannot use these
#models in this particular setting.



#J,

with(brain_data, plot(MRI_Count, FSIQ, cex = 2.5, pch = 21, bg = (FSIQ >= 130)))

brain_lm_all = lm(brain_data$FSIQ ~ brain_data$MRI_Count)
summary(brain_lm_all) 
# gives hat beta1 = 1.095e-04 with a p-value of 4%.
# also notice that the R^2 is really low 11% 
abline(brain_lm)

# The positive effect is estimated to be stronger
# in the full dataset (now 1.095e-04, earlier
#1.083e-05 and6.485e-05). The validity of this
# regression is questionable due to the gap.

#The estimated straight line misses all observations,
#which is worrying:


#We should be very skeptical to the estimated model
#as it misses all observations. 
#From our simulation experience, we should also be
#very skeptical to the conclusions from the linear regression,
#which is based on correlation and standard deviations, both
#of which (we only considered correlation above) will be affected
#by the gap.





































