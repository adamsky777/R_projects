# Before you start the analysis you need to set the proper working directory 
# (with respect to this R script); i.e. to the location where the data file 
# "BrainSize.tsv" is located. This can be done by locating the dropdown menu 
# "Session" (in RStudio), then selecting "Set Working Directory" and finally 
# using "Choose Directory..." where you locate and open the directory where you 
# have stored the file "BrainSize.tsv".

# Read the data into R
brain_data <- read.table(file   = 'BrainSize.tsv', 
                         header = TRUE, 
                         sep    = "\t")


# By inspecting the table from View(...), we find negative values in individual 
# number 2 and 21, the following code remove these

k          <- 2
l          <- 21
brain_data <- brain_data[-c(k, l), ] # this command "removes" row k and l

# but also a better way to remove the data
"
cleaned_brain_data = brain_data
cleaned_brain_data[cleaned_brain_data < 0] = NA # assign NA to values that are less than zero in the dataframe
cleaned_brain_data = na.omit(cleaned_brain_data) # omit records with NA from the dataframe
View(cleaned_brain_data)
"



# The next lines of code creates two new variables/columns that include 
# "Height" and "Weight" transformed to centimetres and kilograms and stored in
# two new variables/columns called "HeightCm" and "WeightKg"
a <- 2.54
b <- 0.45
brain_data$HeightCm <- with(brain_data, round(a*Height))
brain_data$WeightKg <- with(brain_data, round(b*Weight, 1))



# b)
# 
# The following line(s) plot the estiamated density for FSIQ
with(brain_data, plot(density(FSIQ, bw = 6),
                      col ="red",
                      main = "",
                      xlab = "FSIQ (red), VIQ (blue) and PIQ (green)"))

with(brain_data, lines(density(VIQ, bw = 6), col = "blue"))

with(brain_data, lines(density(PIQ, bw = 6), col = "green"))



#c)

# pairwise correlation of 3 columns
cor(brain_data[c("FSIQ","VIQ","PIQ")])


# c command combines values into a vector or list.

c(2,1,3) + c(3,4,6)
c(2,1,3) + 5
cbind(c(2,1,3), c(3,4,6))

cbind(brain_data$FSIQ, brain_data$VIQ, brain_data$PIQ) # concat by column-vectos into a new matrix.
brain_data[c("FSIQ", "VIQ", "PIQ")] # concats by dataframe

c(brain_data$FSIQ, brain_data$VIQ, brain_data$PIQ) # combines values into a a vetor or a list

# whereas cbind concatenates column-vectors into a new matrix by columns and rows.

# also command brain_data[c("FSIQ", "VIQ", "PIQ")] is not the same as 
# c(brain_data$FSIQ, brain_data$VIQ, brain_data$PIQ)
# since  the first command concatenates by dateframes the second command cocats by vectos, and cbind also concatunates by column vectos.


#c (iii)
# why is the correlation matrix symmetric?
cor(brain_data[c("FSIQ","VIQ","PIQ")])

"formula http://andymath.com/wp-content/uploads/2019/06/Correlation-Coefficient-1024x332.jpg

In terms of correlation by looking at the formulam it does not matter if we swap x and y.
meaning cor(X,Y) = cor (Y,X)

"

# 3c (iiii)


pairs(brain_data[c("FSIQ", "VIQ", "PIQ")])


#d)

#d (i)
with(brain_data, cor(MRI_Count, FSIQ))
# it is a low correlation between MRI count and FSIQ


with(brain_data, plot(MRI_Count, FSIQ, cex = 2.5, pch = 21, bg = "grey")) 
with(brain_data, lines(smooth.spline(MRI_Count, FSIQ, df = 5), col = "red"))

# yes it's able to differentiate between two groups, it might be explainable with linear regression.
#the relationship for each group looks linear altough there is a gap between 90k and 95k


with(brain_data, plot(MRI_Count, FSIQ, cex = 2.5, pch = 21, bg = (FSIQ >=130)))


#e)
#
# The following lines of code compute the correlation and makes 
# the corresponding scatter plot for HeightCm and FSIQ
with(brain_data, cor(HeightCm, FSIQ))
with(brain_data, plot(HeightCm, FSIQ, cex = 2.5, pch = 21, bg = "grey"))
with(brain_data, lines(smooth.spline(HeightCm, FSIQ, df = 2), col = "red"))
# there is a slightly negative correlation between FSIQ and Heightcm, Height and IQ are not correlated.


# E(ii)
with(brain_data, cor(WeightKg, FSIQ))
with(brain_data, plot(WeightKg, FSIQ, cex = 2.5, pch = 21, bg = "grey"))
with(brain_data, lines(smooth.spline(WeightKg, FSIQ, df = 2), col = "red"))
# we can conclude there is no correlation ath all but it could be there is a corrleation if we divide the sample into two parts.


#E (iii)


with(brain_data, boxplot(FSIQ ~ Gender, horizontal = TRUE))


#F (i)
cor(brain_data[c("MRI_Count", "HeightCm", "WeightKg")]) 
pairs(brain_data[c("MRI_Count", "HeightCm", "WeightKg")])
# all are strongly correlated.


# f(ii) relationship between  brain size (MRI count) and gender

with(brain_data, boxplot(MRI_Count ~ Gender, horizontal = TRUE))
# Normally the brain size differs between genders.(there is difference between height and weight between the two groups.)

# Exercise 2

 
