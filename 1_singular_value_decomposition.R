#in-class Exercise 1 on 9/13/21
#Daniela Pierro

#following Shipley (p. 24-25), center a dataset and find axes that explain the most variation using the singular value decomposition (SVD)

#first, install the packages car, rgl, and ggplot2

set.seed(10) #initialize a random number generator
x<-rnorm(n=100,mean=0, sd=1) #100 independent values for the variable x following a normal distribution, such that there is a mean of 0 and standard deviation of 1 
y<-rnorm(n=100,mean=0, sd=1) #100 independent values for the variable y following a normal distriution, such that there is mean of 0 and standard deviation of 1 
z1<-0.7*x-0.7*y+rnorm(n=100,mean=0,sd=sqrt(1-2*0.7^2)) #creates the variable z1, a linear mixture of x and y and a new variable, following a normal distribution such that there is a mean of 0 and standard deviation of sqrt(1-2*0.7^2)
sim1<-data.frame(x=x,y=y,z=z1) #creates a dataframe with 3 columns (representing x, y, and z1) and 100 rows
sim1 #look at sim1

pairs(sim1) #visualize sim1 as a pairwise scatterplot (Fig. 2.2). x and y are independent, and both are partially correlated to z. x is weakly positively correlated with z and y is weakly negatively correlated with z.

open3d() #create a new window for viewing the trivariable scatterplot (Fig 2.3) generated in the next step, allowing the ability to rotate this scatterplot.
scatter3d(x,y,z1) #creates a 3-d projection of the variables x, y, and z1.

sim2=sim1[c(1,3)] #selects x and z1 from sim1 and creates a new dataframe sim2

sim2m = colMeans(sim2) #sim2m is defined as the means of sim2
tocenter=matrix(rep(sim2m,each=100),100) #create a matrix with the vector sim2m for which each element is repeated 100 times
csim2=tocenter-sim2 #create a variable csim2 such that each value from the matrix is subtracted by sim2 (x and z1)

svd(csim2) #create a single value decomposition to find the axis that best explains the most variation. To graph the two variables x and z1, we need to create two orthogonal (perpendicular) axes.
sinval=svd(csim2) #assign the SVD to the variable sinval, such that loadings for the new, rotated axes are stored in sinval$v.
sinval$v #look at the loadings for the new, rotated axes

tpoints = rbind(cbind(-3,-3),cbind(-1,-1),cbind(0,0),cbind(1,1),cbind(3,3)) #recalculate an arbitrary list of points, in order to make a scatterplot that replicates parts of Fig. 2.4
tpoints=as.data.frame(tpoints) #create a dataframe of the data from tpoints such that column headings are not treated as variables.
tpoints=transform(tpoints, ax1a=V1*sinval$v[1,1], ax1b=V2*sinval$v[2,1]) #multiply the original x-value by its adjustment for the new primary axis, and do the same for the y-value.
tpoints #look at the transformed dataframe called tpoints, which includes the adjusted datapoints relative to each of their axes

plot1 = ggplot(aes(x=x, y=z),data=csim2) + geom_point() #create a scatterplot of variables x vs. z 
plot1 #view this scatterplot

plot1=plot1+geom_segment(aes(x=tpoints$ax1a[1],y=tpoints$ax1b[1],xend=tpoints$ax1a[5],yend=tpoints$ax1b[5])) #add axis 1 as a line segment
plot1 #view this scatterplot

tpoints=transform(tpoints, ax2a=V1*sinval$v[1,2], ax2b=V2*sinval$v[2,2]) #repeat the transformation step using the second column in sinval$v, rather than the first column

plot1=plot1+geom_segment(aes(x=tpoints$ax2a[1],y=tpoints$ax2b[1],xend=tpoints$ax2a[5],yend=tpoints$ax2b[5])) #add axis 2 as a line segment
plot1 #view this scatterplot

