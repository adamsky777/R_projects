rm(list=ls()) # clears the memory

housePrices = read.table("https://raw.githubusercontent.com/adamsky777/R_projects/main/Data/boston.csv",
           sep = ",", header = TRUE)

# How many rows and columns do we have?
dim(housePrices)
#[1] 506  15

# What are the variable names?
colnames(housePrices)
#[1] "ID"      "crim"    "zn"      "indus"   "chas"   
#[6] "nox"     "rm"      "age"     "dis"     "rad"    
#[11] "tax"     "ptratio" "black"   "lstat"   "medv"  

View(housePrices)


# B) 

# Remove the ID column

housePrices = (housePrices[-1])

# C)

housePrices$lmedv <- log(housePrices$medv)
housePrices$lcrim <- log(housePrices$crim)
#The log-variables removes the heavy tails, and makes the distributions
#more symmetric, more "normal". A more detailed analysis of the change from the 
#crim-variable to the log-crim variable is as follows:



#(ii)
par(mfrow=c(3,1))
with(housePrices, hist(crim))
with(housePrices, hist(lcrim))
x <- seq(min(housePrices$crim), max(housePrices$crim), length.out=100)
plot(x,log(x), type="l", ylim=c(min(log(x)), max(x)))
points(x,x, type="l", col="red")
par(mfrow=c(1,1))

#The very slow increase of the log function, 
#will "push together" the very heavy right tail of the crim-distribution.



#(iii)
par(mfrow=c(3,1))
with(housePrices[which(housePrices$crim < 1), ], hist(crim))
with(housePrices[which(housePrices$crim < 1), ], hist(lcrim))
x <- seq(0.001, 1, length.out=100)
plot(x,log(x), type="l", ylim=c(min(log(x)), max(x)))
points(x,x, type="l", col="red")
par(mfrow=c(1,1))

#Even "zoomed in" to crim < 1, we get a very right skewed distribution 
#in the crim-variable, with a large concentration of values close to
#(but not equal to) zero.
#We see that the log-function near zero will "separate" these values more, 
#bringing values quite close together further apart, resulting in the more gradual
#distribution in the left tail of lcrim.

#iv, consider a histogram log(crim+1) Why does this distribution look so different?
hist((housePrices$crim))
hist((housePrices$crim+1))

hist(log(housePrices$crim))
hist(log(housePrices$crim+1))
hist(log(housePrices$crim+0.5))

#(iv)
#Note that this effect is as striking as it is because of the very strong
#concentration of values of crim near zero. log around
#specifically zero behaves as seen in the above plot. If this "towering" effect
#were to be anywhere else, just taking log would not have the same effect.
#For example, crim+1 has a "tower" around 1, and not 0.
#Taking log then does "push a part" this "tower"

#The right-tail is compressed, following
#the argument in (ii). However, the values close to zero
#are not spread out as in (iii).
#Note that we get a bit of this effect if we add e.g. just 0.5 instead of 1:



# D)

highMedv <- which(housePrices$medv > 40)
housePrices$medv[highMedv]

which(housePrices$medv == 50)
length(which(housePrices$medv == 50))
length(which(housePrices$medv > 50))

(length(which(housePrices$medv == 50))) / length(housePrices$medv) *100
#3.16% of the observations are affected by data censoring.


# This censoring means that for regions with medv > 50, we
# cannot quantify the dependence between medv and other 
# variables. This may affect our statistical analysis,
# but "hopefully" not by much. Attempts at quantifying
# the effect can be pursued, but we will not. 

# what is the corressponding maxumum limit of lmedv?
log(max(housePrices$medv))
max(housePrices$lmedv)
log(50)


#E, 

hist(housePrices$crim, col="grey")
boxplot(housePrices$crim, col="grey")

#crime is very right skewed, i.e., there are 
#some places which have a much higher crime rate
#than most other places.

plot(x = housePrices$crim, y=housePrices$lmedv,cex = 2.5, pch = 21, bg = 0)

# this two command basically does the same,
# we want to specify the obs, where medv >=50, either we can choose,
# saying where it is large >= log 50 lmedv (where lmedv is the log of medv)
# or simply we can specify medv >=50 the outcome is the same

with(housePrices, plot(crim, lmedv, cex = 2.5, pch = 21, bg = 3*(lmedv >=log(50))))

with(housePrices, plot(crim, lmedv, cex = 2.5, pch = 21, bg = 3*(medv >=50)))


# F, Fit the simple linear regression model:
# lmedv = β0 + β1crim + ui"

lm.simple = lm(housePrices$lmedv ~ housePrices$crim)
summary(lm.simple)

with(housePrices, plot(crim, lmedv, cex = 2.5, pch = 21, bg = 3*(medv >=50)))
abline(lm.simple, col="blue", lwd=3)
with(housePrices, lines(smooth.spline(crim, lmedv), lwd=3, col="red"))


# then fit the multiple linear regression model:
# lmedv = β0 + β1crim + β2crim2 + ui
# Plot the quadratic curve

#(I where i is needed when we use multiple linear regression)
lm.quadratic <- with(housePrices, lm(lmedv ~ crim + I(crim^2)))


crimRange = c(min(housePrices$crim), max(housePrices$crim))
crimPredict = seq(from=crimRange[1], to=crimRange[2], length.out=100)

# coefficients 
betahatQuadratic <- lm.quadratic$coefficients

x <- crimPredict #for simpler notation in the next line
lmedvPredicted <- betahatQuadratic[1] + betahatQuadratic[2]*x  + betahatQuadratic[3]*x^2
lines(crimPredict, lmedvPredicted, col="green", lwd=3)
legend("topright", legend = c("Smoother", 'Simple Linear Regression', 'Quadratic Linear Regression'), lwd=3, col = c("red", "blue", "green"))

# Predict the lmedv for an area with crim = 80 using both models

"For simple linear regression:"

x = 80 

betahatSimple = lm.simple$coefficients
lmedvPredicted = betahatSimple[1] + betahatSimple[2] * x
lmedvPredicted
#1.12


"For quadratic"

lmedvPredicted = betahatQuadratic[1] + betahatQuadratic[2]*x + betahatQuadratic[3]*x^2
lmedvPredicted
# 2.25

#The two predictions are very different.
#Which should we trust? 
#This cannot be said with great certainty, since there are
#few observations in this region (only 2-3 observations have
#crim on this order of magnitude).
#
#Let us therefore go outside what is strictly given by 
#the observations seen in isolation, and think about what the
#variables means. "crim" is the crime-rate in a region, and
#lmedv is the log-value of a region.
# medv is the median value of owner-occupied homes in $1000s

#It seems likely that crime will have a negative effect on 
#the value of a region, and so the decreasing trend seen 
#in most of the plot for both models seems reasonable.
#However, both models do not seem to work well
#in regions with very high crime-rates.
#Firstly, it seems unlikely, and is indeed not supported by
#the average effects as estimated by the smoother estimate,
#that this effect is linear also for very high crime rates.

#Secondly, it seems highly unlikely that the value of regions
#all of a sudden -increase- after the crime rate hits around 50.
#This highly unreasonable increase that the quadratic
#model has at the end of the crime region is driven firstly
#by the last few observations (and is also influencing the 
#smoother estimate), as well as the shape of the quadratic function.
#Note that we could work with a more complicated function class 
#that e.g. is restricted to being decreasing (in contrast to the 
#quadratic, which always has a "U" shape globally), but that this
#is outside the scope of the course.
#
#It therefore seems that both models have some problems, and
#most likely are not very good at predicting the lmedvd for
#very high crime rates. 




#G,

with(housePrices, plot(lcrim, lmedv, cex = 2.5, pch = 21, bg=3*(medv==50)))
with(housePrices, lines(smooth.spline(lcrim, lmedv), lwd=3, col="red"))

#with simple linear regression

lm.log.simple = with(housePrices, lm(lmedv ~ lcrim))
abline(lm.log.simple, lwd=3, col="blue")


# with quadratic linear regression:

lm.log.quadratic = with(housePrices, lm(lmedv ~ lcrim + I(lcrim^2)))
summary(lm.log.quadratic)

#NOTE:   

"The p-value of the quadratic term 5.28e-07 (5.25*10^-7) is extremely small 
number, meaning the quandtratic term is statistically significant,
indicating that the quadratic term is indeed needed in our model."
# I(lcrim^2)  5.28e-07



lcrimRange <- with(housePrices, c(min(lcrim), max(lcrim)))
lcrimPredict <- seq(from= lcrimRange[1], to=lcrimRange[2], length.out=100)

betahatQuadratic.log <- lm.log.quadratic$coefficients

x <- lcrimPredict

lmedvPredicted <- betahatQuadratic.log[1] + betahatQuadratic.log[2]*x  + betahatQuadratic.log[3]*x^2
lines(lcrimPredict, lmedvPredicted, col="green", lwd=3)
legend("bottomleft", legend = c("Smoother", 'Simple Linear Regression', 'Quadratic Linear Regression'), lwd=3, col = c("red", "blue", "green"))


#This relationship seems to be more stable, and we would 
#prefer to use the log-log type model (i.e., a model
#where we use both the log of medv and the log of crim) with
#a quadratic term.

#Note also that the behaviour of the quadratic corresponds to
#the trend of the data, and has no strange artefacts as
#going against the direction we would see as natural. 
#While the smoother has a very slight increase at the very
#highest lcrim values, the quadratic trend is not influenced by
#this, since the scale of both variables are now compressed
#to such a degree that the increase is not seen as important.
#Recall in this respect that the log is a strictly increasing
#transformation, meaning that it is has an inverse function
#(which we know is the exponential-function). Therefore, 
#the scatter plot of the lmedv versus lcrim really shows 
#the exact same observations as when plotting medv versus crim,
#except that the scale is changed -non-linearly-!
#This non-linear change of scale dramatically changes the
#behavior of the variables, resulting in the seeming stability
#we see in this plot. 
#Note also that a quadratic in the log-log case is a very different
#model from a quadratic model with medv as the Y variable and
#crim as the X variable.

#An exploration of a related topic is found in the 2020 home exam



rm(list=ls()) # clears the memory
#Assignment 2,
# A,


beauty = read.table("https://raw.githubusercontent.com/adamsky777/R_projects/main/Data/beauty.csv",
           sep=",", header = TRUE)
X1 = (beauty$female == 1) * 1
X2 = (beauty$looks > 3) * 1
lm.interaction <- lm(beauty$lwage ~ X1 + X2 + X1 * X2)
summary(lm.interaction)

hatbeta0 = lm.interaction$coefficients[1]
hatbeta1 = lm.interaction$coefficients[2]
hatbeta2 = lm.interaction$coefficients[3]
hatbeta3 = lm.interaction$coefficients[4]

hotFemale = (beauty$looks > 3 ) & (beauty$female == 1) 
notHotFemale = (beauty$looks <= 3) & (beauty$female == 1)
hotMales = (beauty$looks > 3 ) & (beauty$female == 0) 
notHotMales = (beauty$looks <= 3 ) & (beauty$female == 0) 

#preditciton of log wage for hot females (looks above 3)

X1p = 1               # female 
X2p = 1               # looks above 3
X3p = X1p * X2p       # term  (X1 * X2)

hatbeta0 + hatbeta1 * X1p + hatbeta2 * X2p + hatbeta3 * X3p 

#average log wage of female with looks above 3
mean(beauty$lwage[(which((beauty$female==1) & (beauty$looks > 3)))])



# prediction of log wage for not hot females (looks <= 3)

X1p = 1               # female 
X2p = 0               # looks above 3
X3p = X1p * X2p       # term  (X1 * X2)


hatbeta0 + hatbeta1 * X1p + hatbeta2 * X2p + hatbeta3 * X3p 

mean(beauty$lwage[(which((beauty$female==1) & (beauty$looks <= 3)))])

# prediction of log wage for hot males (looks > 3)

X1p = 0               # male 
X2p = 1               # looks above 3
X3p = X1p * X2p       # term  (X1 * X2)

hatbeta0 + hatbeta1 * X1p + hatbeta2 * X2p + hatbeta3 * X3p 

mean(beauty$lwage[(which((beauty$female == 0) & (beauty$looks > 3)))])

# prediction of log wage for not hot males (looks <= 3)

X1p = 0               # male 
X2p = 0               # looks above 3
X3p = X1p * X2p       # term  (X1 * X2)

hatbeta0 + hatbeta1 * X1p + hatbeta2 * X2p + hatbeta3 * X3p 

mean(beauty$lwage[(which((beauty$female == 0) & (beauty$looks <= 3)))])

#Note that the coefficients are just re-written averages, and so there
#is nothing special with having an effect (estimated by hatbeta3) for
#hot women. If we flipped the binary variables, we would instead get a
#coefficient belonging to not hot men.


















