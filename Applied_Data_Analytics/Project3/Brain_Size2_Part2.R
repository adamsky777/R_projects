# Project 3 - Brain size 
# Brainsize 2 part 2
rm(list=ls())
#2(a)

# Read the data into R
brain_data <- read.table(file   = 'BrainSize.tsv', 
                         header = TRUE, 
                         sep    = "\t")

brain_data[brain_data < 0] = NA # assign NA to values that are less than zero in the dataframe
brain_data = na.omit(brain_data) # omit records with NA from the dataframe
View(brain_data)

 "since we omitted records we need to assign new row numbers to the record by using this command:
 by default if we omit rows in R rows counts will be the follwong 1,2,4,5 (since we omitted row 3rd)
 We want to have adequate rownames to the date so we have to assign to our dataframe again.
 Hence we will get row 1,2,3,4"
n = length(brain_data$Height)  # first getting the sample size
rownames(brain_data) = (1:n) #assigning the new row names based on the sample size.
View(brain_data)

# 2(b)

first_twenty = (1:20)
brain_data$Height[first_twenty]

#2(c)
brain_data$Gender == "Male"
View(brain_data)
# yes it's true

#2(d)
# which command returns the indexes where the record == to Male.
indexes = which(brain_data$Gender == "Male")

men_height = brain_data[c(indexes), "Height"] # accessing all the heights of men by indexes, we can pass the row indexes combining them into a list, 
#and also specifying the column name in this case Height.
hist(men_height)

mean(men_height)
#mean height of men = 71.4


#histogram and mean height of women.
w_indexes = which(brain_data$Gender == "Female")
women_height = brain_data[c(w_indexes), "Height"]
hist(women_height)
mean(women_height)
# Women height = 65.7

# plot the two graphs at once:
par(mfrow = c(1, 2))
hist(men_height)
hist(women_height)

# histogram with weights of all ppl in the dataset
par(mfrow = c(1, 1))
hist(brain_data$Height)

# not part of this question but I added a density line just to showcase how it height is distribute.
hist(brain_data$Height, freq = FALSE)
with(brain_data, lines(density(Height), col = "red"))

#F
# converting booleans to binary
Female_booleans = brain_data$Gender == "Female"
Female_booleans *1



# explain this code
n <- length(brain_data$Gender)  # sample size of total records / observations in column Gender
nMale <- sum(brain_data$Gender == "Male")  # total number of males in the dataset.
nFemale <- sum(brain_data$Gender == "Female") # total number of females in the dataset.
n # sample size
nMale + nFemale # which is equivalent of sample size of Male  + sample size of Female in the dataset.





