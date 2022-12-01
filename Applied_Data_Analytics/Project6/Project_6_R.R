# Project 6

rm(list = ls())
yrsSchool = read.table("https://raw.githubusercontent.com/adamsky777/R_projects/main/Applied_Data_Analytics/Project%204/yrsSchool.csv",
                    sep = ",",
                    header = FALSE)

yrsSchool = yrsSchool$V1
View(yrsSchool)
"Assignment 1B"

length(yrsSchool)
# 2458285 number of observations in the dataset 

# 1D
# Count the number of ppl with 10 years of schooling
length(which(yrsSchool==10))

# 562837 number of ppl with 10 years of schooling 

p = length(which(yrsSchool == 10)) / length(yrsSchool) 
#probability that we select one with 10 years of schooling # 0.2289552

# also works if we sum the true values where yrs of school == 10 and divide it with the sample size N.
N <- length(yrsSchool)
sum(yrsSchool==10) / N

# 1E
"Sample 100 observations (use code from Project 4. 
Remember to fix the random seed so that your results are re- producible). 
Identify the proportion of sampled observations that have 10 years of schooling.
Call this proportion pˆ (p-hat)"
# What is the probability that we select an idividual with 10 years of schooling out of 100 people.
set.seed(4110)
n = 100
N = length(yrsSchool)
indexes = sample.int(N, size=n, replace=FALSE) #we randomly select 100 from our sample, and save the indexes.
p_hat = sum(yrsSchool[indexes] == 10)/n #0.21, pretty close to the true value (0.2289551, calculated in (D))
# sum the values for all the indexes where the values equal to 10. 
# divide it with the sample size.

#F
(yrsSchool == 10)
(yrsSchool == 10) *1
#these are true false values and we multiply by one to convert them 1 or 0
indicatorVar <- (yrsSchool == 10)*1 # see here

sigmaSq = var(yrsSchool==10)*1
# Variance = sigmaSq = 0.1765348

mu = sum(yrsSchool == 10)/N
#mu is the population average of indicatorVar, which was found in D.
#0.2289551

# G

X <- indicatorVar #We now refer to indicatorVar by X.
set.seed(4110)
n <- 100
averages <- NULL
for (i in (1:1000)) {
  averages[i] <- mean(X[sample.int(N, size=n)]) #NB: note the change: X
}
par(bg="white")
hist(averages, freq=FALSE, col = "grey")
x <- seq(-3*sqrt(sigmaSq/n) + mu ,3*sqrt(sigmaSq/n) + mu, length.out = 400)
y <- (1/(sqrt(2*pi)*(sqrt(sigmaSq/n))))*exp(-0.5*(x - mu)^2/(sigmaSq/n))
points(x,y, type="l", col="red")
#Looks good!

# Assignment 2 C

"Take a randomn sample from indicatorVar with n =100 and compute the confidnece interval."

set.seed(4110)
n <- 100
foundSample <- X[sample.int(N, size=n)]

foundSample
lowerLimit <- mean(foundSample) - 1.96*sqrt(sigmaSq)/sqrt(n)
upperLimit <- mean(foundSample) + 1.96 *sqrt(sigmaSq)/sqrt(n)
#yes the mean is in between the lower and the uppeer bound, we can confirm.
#D justification:
(mean(indicatorVar) <= upperLimit) & (mean(indicatorVar) >= lowerLimit)



#E

set.seed(4110)
n <- 100
foundSample <- X[sample.int(N, size=n)]
lowerLimit <- mean(foundSample) - 1.96*sqrt(sigmaSq)/sqrt(n)
upperLimit <- mean(foundSample) + 1.96*sqrt(sigmaSq)/sqrt(n)
print(c(lowerLimit,upperLimit))
(lowerLimit <= mu) & (upperLimit >= mu) 





# F

set.seed(4110)
n <- 100
R <- 1000 # repeat
counter <- 0

for ( i in (1:R)){
  foundSample <- X[sample.int(N, size = n)]
  lowerLimit <- mean(foundSample) - 1.96*sqrt(sigmaSq)/sqrt(n)
  upperLimit <- mean(foundSample) + 1.96*sqrt(sigmaSq)/sqrt(n)
  counter <- counter + ((upperLimit >= mu) & (lowerLimit <= mu))
  
}

counter/R
# it is 0.95 whihc is exaclty 95% confidnece that the mean is in between the interval.

# Alternatively a complex way of calculation.
"
set.seed(4110)
n = 100
counter= 0
foundSample = NULL
for (i in (1:1000)) {
  if (i <= 1000){
    found_sample <- indicatorVar[sample.int(N, size=n)]
    
    lowerBound <- mean(found_sample) - 1.96*sqrt(sigmaSq)/sqrt(n)
    upperBound <- mean(found_sample) + 1.96 *sqrt(sigmaSq)/sqrt(n)
    print((mean(indicatorVar) <= upperBound) & (mean(indicatorVar) >= lowerBound))
    if ((mean(indicatorVar) <= upperBound) & (mean(indicatorVar) >= lowerBound) == TRUE )
    {
      counter <- counter +1
      print(counter)
      found_sample = NULL
      }
    else 
      {
      print(counter)
      {break}}
  }
    else{
      {break}
    }
}
"    


    
#G,

set.seed(4110)
n <- 100
foundSample <- X[sample.int(N, size=n)]
sd(foundSample)


indexes <-seq(1,50)*100
# gives the indexes that we wish to use
resSd <- NULL
for (n in indexes){
  print(n)
  foundSample <- X[sample.int(N, size=n)]
  resSd <- c(resSd, sd(foundSample))
}
resSd

plot(indexes, resSd, type= "b")
abline(h=sqrt(sigmaSq), col="red")

plot(indexes, resSd, type="b", ylim=c(0,1))
abline(h=sqrt(sigmaSq), col="red")


# practice:
set.seed(4110)
indexes = seq(1,50) *2

resSd <- NULL
for ( n in indexes ){
  foundSample <- X[sample.int(N, size=n)]
  resSd <- c(resSd, sd(foundSample))
}
resSd
plot(indexes, resSd, type="b")
abline(h=sqrt(sigmaSq), col="red")


# Problem H
# Assess for n = 100 wheter the probablity that mu is contained within this interval
# is approximately 95%

set.seed(4110)
n = 100
R = 1000
counter = 0

for (i in (1:R)){
  foundSample <- X[sample.int(N, size = n)]
  lowerLimit <- mean(foundSample) - 1.96*sd(foundSample)/sqrt(n)
  upperLimit <- mean(foundSample) + 1.96*sd(foundSample)/sqrt(n)
  counter <- counter + ((upperLimit >= mu) & (lowerLimit <= mu))
}
counter/R
# 0.948 approximately the probability that mu is in the confidence interval is 95%.


# Problem I repeat with n = 10 and n = 500, Comment!


set.seed(4110)
n = 10
R = 1000
counter = 0

for (i in (1:R)){
  foundSample <- X[sample.int(N, size = n)]
  lowerLimit <- mean(foundSample) - 1.96*sd(foundSample)/sqrt(n)
  upperLimit <- mean(foundSample) + 1.96*sd(foundSample)/sqrt(n)
  counter <- counter + ((upperLimit >= mu) & (lowerLimit <= mu))
}
counter/R
# when sampling n = 10, the confidence drops to 90%. Meaning that we take less from the sample,
# the probability that the mean is not going to be in the sample increases.



set.seed(4110)
n = 500
R = 1000
counter = 0

for (i in (1:R)){
  foundSample <- X[sample.int(N, size = n)]
  lowerLimit <- mean(foundSample) - 1.96*sd(foundSample)/sqrt(n)
  upperLimit <- mean(foundSample) + 1.96*sd(foundSample)/sqrt(n)
  counter <- counter + ((upperLimit >= mu) & (lowerLimit <= mu))
}
counter/R

" if we increase the the sampling from 100 o 500, the probability did not increased,
meaning it could be that we can't trust truly in this method, the reason for we get
95% probability and the first time with sample size is 100 is seems to be fluke.
"

# J
mu = mean(yrsSchool)
N = length(yrsSchool)
set.seed(4110)
n = 1000
R = 1000
foundSample <- NULL
counter = 0

for (i in (1:R)){
  foundSample <- yrsSchool[sample.int(N, size = n)]
  lowerLimit <- mean(foundSample) - 1.96*sd(foundSample)/sqrt(n)
  upperLimit <- mean(foundSample) + 1.96*sd(foundSample)/sqrt(n)
  counter <- counter + ((upperLimit >= mu) & (lowerLimit <= mu))
}
counter/R





