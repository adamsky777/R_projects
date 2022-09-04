# Math Problems and notes
"
https://share.goodnotes.com/s/UV6yFGW3RimdFQDNATKvN9   
"


# Problem (A)



X1 =3
X2=-4

Y1=2
Y2=100

#slope
b = (Y1-Y2)/(X1-X2)

# intercept
a = ((Y2*X1) - (Y1*X2))  / (X1-X2)

a
b
# correct
# Problem (B)

X1 = 0
X2 = -11

Y1 = -2
Y2 = -100

#slope
b = (Y1-Y2)/(X1-X2)

# intercept
a = ((Y2*X1) - (Y1*X2))  / (X1-X2)

a
b

# correct


# Problem (C)

random500= rnorm(500)
hist((random500))
summary(random500)

# Problem (D)
# Verify that the average of tildeX equals 0 (up to numerical precision).
#Hint 1: Recall the command result <- X + 1 in intro.R.
#It performs an operation (adding one) on each element of X, and places the result in the variable result.

X = rnorm(20)
X

meanX = mean(X)

tildeX = X - meanX
tildeX
mean(tildeX)
# indeed tildeX is equals to zero

mean(tildeX) < .0001



#  Problem (E)
X = rnorm(20)

mean(X)
sd(X)

tildeX = (X - mean(X)) / (sd(X))


mean(tildeX) < .0001

# indeed smaller than .0001, we can conclude it is close zero.
sd(tildeX) > 0.99999
# and the Standard deviation is 1

# Problem  (F)

X = rnorm(20)*10^10

mean(X)
sd(X)

tildeX = (X - mean(X)) / (sd(X))


mean(tildeX) < .0001
# indeed smaller than .0001, even if we repeat it with 10¨^10 times, we can conclude it is zero.

# Problem (G)


n  = 15
x = -0.5

series = x^(0:n)

# pay attention to the parenthesis at the first fraction 1-x^, when copying the formula.

(1-x^(n+1))/(x-1)

series
sum(series)








