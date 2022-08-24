#in-class exercise: part I on 9/27/21

# learn to implement a principal component analysis
# graph using screeplot and biplot

rawdata <- read.csv("BFnature_exercise3.csv") #import the file glopnet from Shipley
head(rawdata)
glopnet2 = rawdata[c(10,11,12,16)] #create new version of glopnet with columns 10,11,12,16
glopnet2 <- na.omit(glopnet2) #omit NA from glopnet2
head(glopnet2)

colMeans(glopnet2) #calculate the means of the variables in all four columns of glopnet2
sapply(glopnet2,mean) #another way to calculate the means of the variables in all four columns of glopnet2
#log.LL   log.LMA log.Nmass log.Amass 
#0.9505081 2.0279878 0.2200122 1.9733333 
sapply(glopnet2,sd) #calculate the standard deviations of the variables in all four columns of glopnet2
#log.LL   log.LMA log.Nmass log.Amass 
#0.4304570 0.2644662 0.1936105 0.3149297

#create a panel plot of histograms to insure all assumptions are met

library(datasets)
library(ggplot2)
library(MASS)
library(robustbase)
library(moments)

pairs(glopnet2) #create a pairwise scatterplot to show linearity. It shows there are roughly linear relationships between all variables.

#examine distributions of four variables in glopnet2 using a panel plot of four histograms, to check if these variables are roughly symmetrical.
par(mar=c(4,2,3,2),mfrow=c(2,2)) #set the parameters of the graph
for (i in 1:4) {hist(glopnet2[,i], #while running a for-loop cycling from 1 through 4, make a histogram pertaining to the variables. All four variables examined seem roughly symmetrical.
                     main=names(glopnet2)[i])} #label the histograms with the variable names

#use the princomp() function to perform a PCA

pca1 = princomp(glopnet2,cor=FALSE,scores=TRUE,covmat=NULL,fix_sign=TRUE) #run a PCA that uses a covariance matrix (not a correlation matrix), with scores for each PCA axis for every row of data, using original values as our input dataframe, and assigning a positive value to the largest loading of each axis.
summary(pca1)

#Importance of components:
#  Comp.1     Comp.2     Comp.3     Comp.4
#Standard deviation     0.5726022 0.17852282 0.13011892 0.12012552 #the standard deviation is a measure of the amount of variability across the axes; we can calculate variance for each PCA axis by squaring these standard deviations, and the sum of these variances is the total variance. Standard deviation is similar to the the scalars related to eigenvalues.
#Proportion of Variance 0.8383260 0.08148813 0.04329002 0.03689584 #the proportion of variation explained by individual axes. it is the  standard deviation squared divided by the sum of the all variables' standard deviations squared. 
#Cumulative Proportion  0.8383260 0.91981414 0.96310416 1.00000000 #explains the cumulative proportion of variance, moving from PCA axis 1 to both 1 and 2, et cetera.


pca1$sdev #look at the standard deviation

pca1$sdev^2/sum(pca1$sdev^2) #to check the proportion of variance, square the standard devation and divide by the sum of all those squared values
#Comp.1     Comp.2     Comp.3     Comp.4 
#0.83832601 0.08148813 0.04329002 0.03689584 

pca1$loadings #the values of a variable's position on two principal component axes; in other words, the values of two original variables (x,y) that determine the scores (Shipley p.27).
#Comp.1 Comp.2 Comp.3 Comp.4
#SS loadings      1.00   1.00   1.00   1.00
#Proportion Var   0.25   0.25   0.25   0.25
#Cumulative Var   0.25   0.50   0.75   1.00

pca1$center #taking the individual datapoints and subtracting the mean from each gives us center values. These are the values we used to center our data.
#log.LL   log.LMA log.Nmass log.Amass 
#0.9505081 2.0279878 0.2200122 1.9733333 
sapply(glopnet2,mean) #to check that the above data we used to center our data are the means of the data. They are.
#log.LL   log.LMA log.Nmass log.Amass 
#0.9505081 2.0279878 0.2200122 1.9733333 

pca1$scale #scaling asks whether we turned the covariances into correlations by adjusting the covariance matrix. Since our PCA1 used the covariance matrix, and it was not adjusted, this command returns the value 1, as expected.
#log.LL   log.LMA log.Nmass log.Amass 
#1         1         1         1 

head(pca1$scores) #view the first six rows of the scores; these are the coordinates, or values on each principal component axis, for every original observation.

#next week: graphing
