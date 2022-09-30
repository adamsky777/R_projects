# Project 4

rm(list = ls())
beauty = read.table("https://raw.githubusercontent.com/adamsky777/R_projects/main/Applied_Data_Analytics/Project%204/beauty.csv",
                    sep = ",",
                    header = TRUE)


par(mfrow=c(1,1))

plot(beauty$educ, beauty$lwage)
lines(smooth.spline(beauty$educ,beauty$lwage), col="red", lwd=3)

dim(beauty) # shape of the dataframe 

head(beauty$exper, 20) # first 20 records from the column experience

classes = unique(beauty$exper) # all unique values of the vector / column.
classes # unsoerted unique numbers
sort(classes) # sorted unique numbers

"How mamny unique numbers we have in the column?"
d= length(classes) # length of the vector 49 unique values. ( we have observarions from 0-48)



expClassAverage = NULL
for (i in (0:48)) {
  expClass = which(beauty$exper == i)
  expClassAverage[i+1] = mean(beauty$lwage[expClass])
}


which(beauty$exper==0)


for (i in (sort(classes))) {
  expClass = which(beauty$exper == i)
}


for (i in (0:48)) {
  expClass = which(beauty$exper == i)}




expClass = which(beauty$exper ==0) # finding the indexes where experience is 0
beauty$lwage[expClass] # lwage column where experience is 0
mean(beauty$lwage[expClass]) # average of the lwage column where expeirence is 0











