#in-class Exercise 2 on 9/20/21

#part I: assumptions

library(datasets)
library(ggplot2)
library(MASS)
library(robustbase)
library(moments)

data(iris) #read in the datasheet iris
attach(iris) #every time I reference a variable in iris, it is attached to this datasheet
head(iris) #look at the beginning of the iris datasheet

#examine distributions of four variables in iris using histograms, using a panel plot to show all four together.Creating these four histograms allows us to check if they are roughly symmetrical. We see that sepal width is roughly symmetrical.

par(mar=c(4,2,3,2),mfrow=c(2,2)) #set the parameters of the graph
for (i in 1:4) {hist(iris[,i], #while running a for-loop cycling from 1 through 4, make a histogram pertaining to the variables: sepal length, speal width, petal length, and petal width
                     main=names(iris)[i])} #label the histograms with the variable names

newvalues = iris[c(1:4)] #create a new dataframe with only columns 1 through 4 from iris, corresponding to the variables Sepal.Length, Sepal.Width, Petal.Length, and Petal.Width 
newvalues=as.matrix(newvalues) #format the new dataframe as a matrix
colMeans(newvalues) #find the means for each of the four variables
colMedians(newvalues) #find the medians for each of the four variables

#assess assumption 1: a roughly symmetrical distribution
#follow a Box-Cox approach with Sepal Length and Petal Length.
#skewed data can be transformed with natural log, square root, or power transformations, which take our original variable to some exponent (lambda). Here, we use a power transformation.
library(MASS)
out<-boxcox(iris$Sepal.Length~1) #randomly apply transformations to find the peak, an optimized outcome which is closest to the normal curve.
lambda1<-out$x[out$y==max(out$y)] #select the x-value associated with the max y value 
lambda1 #lambda is -0.1414141
t_1 = iris$Sepal.Length^(lambda1) #apply the box-cox transformation
hist(t_1) #plot the transformed sepal length data in a histogram

out<-boxcox(iris$Petal.Length~1) #repeat the above power transformation for petal length
lambda2<-out$x[out$y==max(out$y)] #select the x-value associated with the max y value 
lambda2 #lambda is 0.9494949, very close to 1
t_2 = iris$Petal.Length^(lambda2) #apply the box-cox transformation
hist(t_2)

#examine skewness using the moments package
skewness(iris$Sepal.Length) #skewness of the variable Sepal.Length is about 0.3117531
skewness(t_1) #skewness of the variable Sepal.Length after transformation is about -0.006026232 (slightly better)
skewness(iris$Petal.Length) #skewness of the variable Petal.Length is about -0.2721277
skewness(t_2) #skewness of the variable Petal.Length after transformation is about -0.2913221 (about the same)

#assess assumption 2: linearity
pairs(newvalues) #create a scatterplot matrix, revealing that these variables do not meet the assumption of linearity

#part II: variance, covariance, and correlation

plot(iris$Sepal.Length, iris$Sepal.Width, xlab="Sepal Length ", ylab="Sepal Width")
abline(v=mean(iris$Sepal.Length), h=mean(iris$Sepal.Width)) #there is a negative pattern of covariance

#calculate the sum of squares, which is the sum of squared deviations of the data points from their mean
ss_length=sum((iris$Sepal.Length-mean(iris$Sepal.Length))^2) #calculate the sum of squares for sepal length   
ss_length #it is about 102.1683

ss_width=sum((iris$Sepal.Width-mean(iris$Sepal.Width))^2) #calculate the sum of squares for sepal width   
ss_width #it is about 28.30693

#can also use this formula to calculate sum of squares:
var(iris$Sepal.Length)*df #sum of squares is variance multiplied by DF = 102.1683
var(iris$Sepal.Width)*df #sum of squares is variance multiplied by DF = 28.30693

#calculate the degrees of freedom
n=length(iris$Sepal.Length) #calculate n, the number of observations
df = n-1 #calculate the degrees of freedom, n-1, which estimates the total variation while accounting for our data to be biased toward the estimated mean.
df #the degrees of freedom is the same (149) for both sepal length and sepal width

#calculate variance, which is the sum of squares divided by the degrees of freedom
var_length = ss_length / df #calculate the variance of sepal length
var_length #0.6856935
var(iris$Sepal.Length) #check the variance: 0.6856935

var_width = ss_width / df #calculate the variance of sepal width
var_width #0.1899794
var(iris$Sepal.Width) #check the variance: 0.1899794

#calculate the sum of the crossproducts
sum_of_crossproducts = sum((iris$Sepal.Length - mean(iris$Sepal.Length)) * (iris$Sepal.Width - mean(iris$Sepal.Width))) #to calculate the sum of the crossproducts, sum the deviations of sepal length from its mean multiplied by the deviations of sepal width from its mean
covariance = sum_of_crossproducts / df #calculate covariance, which is the sum of the crossproducts divided by the degrees of freedom
covariance #the covariance is -0.042434
cov(iris$Sepal.Length, iris$Sepal.Width) #check the covariance: -0.042434

#calculate the correlation coefficient
correlation_coefficient = covariance / (sd(iris$Sepal.Length) * sd(iris$Sepal.Width)) #the correlation coefficient is the covariance divided by the product of the standard deviations for sepal length and sepal width
correlation_coefficient #the correlation coefficient is -0.1175698
cor(iris$Sepal.Length, iris$Sepal.Width) #check the correlation coefficient: -0.1175698


#calculate variance, covariance, and correlation coefficients for all four variables.

var(iris$Petal.Length) #3.116278, 
var(iris$Petal.Width) #0.5810063, 
var(iris$Sepal.Length) #0.6856935, which matches the previous calculation
var(iris$Sepal.Width) #0.1899794, which matches the previous calculation

iris_numeric = iris[c(1:4)] #isolate only the first four columns, since the fifth is not numeric

cov(iris_numeric) #create a covariance matrix for sepal length and width, and petal length and width. The values match the previous calculations (-0.0424340 is the covariance between sepal length and sepal width)

#Sepal.Length Sepal.Width Petal.Length Petal.Width
#Sepal.Length    0.6856935  -0.0424340    1.2743154   0.5162707
#Sepal.Width    -0.0424340   0.1899794   -0.3296564  -0.1216394
#Petal.Length    1.2743154  -0.3296564    3.1162779   1.2956094
#Petal.Width     0.5162707  -0.1216394    1.2956094   0.5810063

cor(iris_numeric) #create a correlation matrix for sepal length and width, and petal length and width. The diagonals are 1 (each variable is perfectly correlated with itself) and other values match the previous calculations (-0.1175698 is the correlation coefficient between sepal length and sepal width)

#Sepal.Length Sepal.Width Petal.Length Petal.Width
#Sepal.Length    1.0000000  -0.1175698    0.8717538   0.8179411
#Sepal.Width    -0.1175698   1.0000000   -0.4284401  -0.3661259
#Petal.Length    0.8717538  -0.4284401    1.0000000   0.9628654
#Petal.Width     0.8179411  -0.3661259    0.9628654   1.0000000

