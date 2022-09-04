# 1A) 

X1 <- 3
Y1 <- 2

X2 <- -4
Y2 <- 100

a = (Y2*X1 - Y1*X2)/(X1-X2)
b = (Y1-Y2)/(X1-X2)

a
b


# 1B) 

X1 <- 0
Y1 <- -2

X2 <- -11
Y2 <- -100

a = (Y2*X1 - Y1*X2)/(X1-X2)
b = (Y1-Y2)/(X1-X2)

a
b

# 1C)

X <- rnorm(500)
hist(X)
summary(X) # the median can be read out of this output

# 1D)

X <- rnorm(20)
# Let us look at X:
X
tildeX <- X - mean(X)
# tildeX is now "mean centred":
mean(tildeX)
# the mean is not exactly zero, but this is due to the limited 
# numerical precision we use.

# 1E)
X <- rnorm(20)
X
tildeX <- (X - mean(X))/sd(X)
# tildeX is now "fully standardized":

#Again, we (approximately) confirm the results we earlier know from mathematics
#to be exactly true.
mean(tildeX) #very close to zero
sd(tildeX) #R usually reports 1, though this is 1.0000000000000000000, i.e.,
#1 with many decimals afterwards, so that the best approximation to the number
#within the numerical precision used, is 1. We know from math that the answer is
#the integer 1, i.e., 1.0 with an infinite number of zeros afterwards.

#1F

#Only a mathematical derivation can show that the mentioned
#property holds for -all- numbers. The reason for this is
#that there is an infinite collection of numbers, and so
#a computer cannot check this property in a finite amount of time.
#Also, the computer can only do operations with finite precision,
#and so we cannot check that the average is exactly zero and that
#the standard deviation is exactly one. The exactness of this
#result (i.e., not that the average is approximately zero, but 
#that the real answer, without approximation error, really is
#exactly equal to zero) cannot be assessed in general by a 
#computer.
#Note still that having seen that the e.g. the mean is approximately
#zero each time does make this result more and more plausible,
#and may be the starting point for a formal mathematical investigation.

#1G
n <- 15
x <- 0.5
series <- x^(0:n)

# "series" now contains x^i for i = 0, 1, 2, ..., n.
# That is, the first element is 0.5^0 = 0,
# the second element is 0.5^1 = 0.5,
# the third element is 0.5^2 = 0.25,
# ETC.
# The sum of "series" is therefore a geometric sum
# with n = 15 and x = 0.5

sum(series)

# This agrees with the formula:
(1-x^(n+1))/(1-x)




