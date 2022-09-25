# Project 5 (Lab1)

number = 675248
set.seed(number)

number**2

floor(number**2/1000) %% 1000000 # to get the 6 digits from the middle


midSquareRand = function(seed, lengthOut) {
  randvector = NULL
  for(i in 1:lengthOut) {
    value = seed * seed 
    seed = floor(value/1000) %% 1000000
    randvector = c(randvector, seed)
  }   
  return(randvector)
}

X =  midSquareRand(675248, 500)
X
plot(hist(X))

unique(X)
plot(hist(unique(X)))
length(unique(X))

n = 1000

set.seed(4110)
X = runif(n)


"Assginmnet 2 B"

"Generating 1000 random numbers unoformally distributed between 10 and 50"
X = floor(runif(1000,10,50))
barplot(table(X))

barplot(table(2*X))
barplot(table(2*X+1))
"Both 2*X and 2*X+1 are unifromally distributed."

"Assignment 2C"
"Generate random integers from 0 to 9, Use floor. Asses unifromity through bar plot"

X = floor(runif(9,0,9))
barplot(table(X))
"If we generate only 9 numbers the numbers might not get totally unifromly distributed altought it is close to be unifromly distributed."

"Assighnment 2C"
"How do you get random numbers unifrom on the integers from 5 to 10?"

X = floor(runif(n,5,10))
barplot(table(X))
"To get random numbers unfromly distributed we use the runif command specifying how many numbers we want to generate and the ranges."


#Project 5 Assignment 3
"
We here continue working with the Beauty-dataset, 
introduced in Lecture 4. 
The dataset is included in the file beauty.csv, 
and is briefly described in the attached file “beauty description.txt”.
"
"(3A) 
Compute the average log-wage for people with looks assessed to be greater than 3. 
Compute the average log-wage for all not in this group."


beauty = read.table("https://raw.githubusercontent.com/adamsky777/R_projects/main/Applied_Data_Analytics/Project%204/beauty.csv",
           sep = ",",
           header = TRUE)

View(beauty)
beauty_above3 = which(beauty$looks >3)
mean(beauty$lwage[beauty_above3])
# the average of log-wage for people with looks assessed to be greater than 3 is 1.652

"(B) Let us say that a person is attractive if looks > 3, or not attractive if looks ≤ 3.
We want to compute the average log-wage for attractive females, 
not attractive females, attractive males and not attractive males. 
This will be done in the next sub-task. 
Let us first see how to identify these categories of people."



attractiveFemale = (beauty$female == 1) & (beauty$looks > 3)
female_Not_Attractive = (beauty$female == 1) & (beauty$looks <= 3)

attractiveMale = (beauty$female == 0) & (beauty$looks >3)
male_Not_Attractive = (beauty$female == 0) & (beauty$looks <= 3)


mean(beauty$lwage[which(attractiveFemale)])
"The average log-wage for attractive females is 1.34"

mean(beauty$lwage[which(female_Not_Attractive)])
"For Non-attractive females is 1.28"

mean(beauty$lwage[which(attractiveMale)])
"For Attractive males 1.84"

mean(beauty$lwage[which(male_Not_Attractive)])
"For non-Attractoive Males 1.85"


# C count the number of people in each category

sum(attractiveFemale)
# we have 144 Attractive females.

sum(female_Not_Attractive)
# 292 not attractive females.

sum(attractiveMale)
# 239 attractive males

sum(male_Not_Attractive)
#585 not attractive males


"
(D) 
Make histograms of the log-wages of attractive females, not attractive fe males,
attractive males and not attractive males. 
Show all four in one single graph, 
and make the titles above the histograms 
display which histogram is related to which category
(so set for example main = attractive malein the plotting options)"


par(mfrow = c(2, 2)) # when plotting 4 grahs together,  it's better to use 2,2, plot 2 on the left 2 on the right.
hist(beauty$lwage[which(attractiveFemale)], main = "Attractive Females")
hist(beauty$lwage[which(female_Not_Attractive)], main="Not Attractive Female")
hist(beauty$lwage[which(attractiveMale)], main = "Attractive Male")
hist(beauty$lwage[which(male_Not_Attractive)], main= "Nt Attractive Male")

" (E)
Find the number of people that are female and married and living in the south 
and also lives in a big city (the answer is 6).
"
sum((beauty$female == 1) & (beauty$married == 1) & (beauty$south == 1) & (beauty$bigcity ==1))
# 6 people matches with the criteria.


"F
Find the average log wage for people with good health, 
that further has either 14 years or more education or has more than ten years 
of experience (the answer is 1.783991). 
Also make a histogram of the log wage of these people.
"
which(((beauty$goodhlth == 1 ) & (beauty$educ >= 14) | (beauty$exper > 10)))
highEduc_or_highExp_and_Goodhlth = ((beauty$educ >= 14) | (beauty$exper > 10)) & (beauty$goodhlth == 1)

mean(beauty$lwage[which(highEduc_or_highExp_and_Goodhlth)])
#average log wage for people with the following conditions = 1.78
hist(beauty$lwage[which(highEduc_or_highExp_and_Goodhlth)]) # plot a histogram aabout the group.



"(G) 
Consider attractiveFemale generated in Task B. Use the NOT operator 
(“!” in R, see the end of the appendix) to generate a Boolean vector 
with everyone that are not an attractive female. 
Compute the mean of the log-wages of these two groups
"
View(beauty)

not_attractive_Female =! attractiveFemale

mean(beauty$lwage[which(not_attractive_Female)])
#the log-wage for the group that is not an attractive female is 1.69

"(H)
Verify the following rule:
!(A&F ) = (!A)|(!F )
NOT Attractive & NOT Femlale = NOT Attractive OR NOT Female"

not_attractive_or_not_Female = ((beauty$female != 1) | (beauty$looks <=3))

# We can identify if the two vectors are the same either by summing them and checking if their values are the same.
# Or we can use the all.equal command which checks for every elements,
# and it also reports mismatches. 
sum(not_attractive_Female)
sum(not_attractive_or_not_Female)
all.equal(not_attractive_Female, not_attractive_or_not_Female)

all.equal(c(TRUE,TRUE),c(TRUE,FALSE))

A = c(TRUE, TRUE, FALSE,FALSE)
B = c(TRUE, FALSE, TRUE, FALSE)
A&B
A|B 
# the xor operator reverse of OR
xor(A, B)
# the not operator
!A
!B



