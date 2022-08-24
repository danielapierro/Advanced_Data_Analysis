
#exercise 5 in-class 10/11/21

library(vegan)
library(MASS)
library(smacof)
library(heplots)
library(mice)
library(weights)
library(nnls)
library(polynom)
library(e1071)
library(wordcloud)
library(candisc)

pyramid = as.data.frame(rbind(cbind(0,0,0),cbind(1,0,0),cbind(.5,.5,1),cbind(0,1,0),cbind(1,1,0))) #create a dataframe based on X,Y, and Z in Shipley's table 4.6.
pyramid

pyramid.ED<-vegdist(pyramid,method="euclidean",diag=T) #perform a metric multidimensional scaling with ratio scaling. The stress 1 value is 0.247, and after 1 iteration the algorithm converges.
def <- mds(pyramid.ED,ndim=2)
def
summary(def) #see the scores on the two axes. Points 1, 2, 4, and 5 each contribute 15% to total stress, while point 3 contributes 40%. The lower the stress, the better the fit.

pyramid2D.explore <- icExplore(pyramid.ED,ndim=2,type="ratio",ties="primary",returnfit=T) #randomly renerate 100 different starting configurations to explore tha data. Returnfit=T allows us to see the final stress value and configuration. When calling this function we demanded two axes and a ratio metric multidimensional scaling analysis.
pyramid2D.explore

#to plot, first eliminate differences not driven by the data, through a Procrustes analysis.Compute distances between teh remaining final configurations, and copare their structure using a final metric multidimensional scaling analysis. The plot shows how each of the final configurations is positioned in a 2D MDS space.
plot(pyramid2D.explore,main="Pyramid stress landscape",xlab="",ylab="") #examine different local minimums in the pyramid data's stress landscape based on a ratio metric multidimensional scaling analysis

jack.pyramid <- jackmds(def) #re-run the analysis for each row to determine our uncertainty about the results, seeing if there is consistency between different answers
jack.pyramid #the small cross-validity indicates that the result is not robust to removing datapoints.

plot(jack.pyramid,col.p="black",col.l="black",main="Jackknife plot of pyramid data") #examine the average scores from each row (labeled points), and how much jackknifed MDS solutions differ from these averages (radiating lines)  

#practice with dune data and learning functions isoMDS() and metaMDS()

data(dune) #place the dune dataset into memory
data(dune.env) #place the dune dataset's associated environmental variables into memory
head(dune)
head(dune.env)

dune_bray = vegdist(dune,method="bray",diag=T) #calculate the Bray-Curtis distance
dune_bray
help(mds)
dune_bray_mds <- mds(dune_bray,ndim=2) #calculate a multidimensional scaling analysis with two dimensions
dune_bray_mds
plot(dune_bray_mds) #plot the MDS output
summary(dune_bray_mds) #see the scores on the two axes. Point 14 contributes the most (8.23%) to the stress, while point 8 contributes 1.79% to the stress.

help(isoMDS)
dune_iso_mds <-isoMDS(dune_bray) #when running Kruskal's Non-metric Multidimensional Scaling, the values converged
summary(dune_iso_mds) 
dune_iso_mds #the final stress value is about 12.02

stressplot(dune_iso_mds,dune_bray) #create a stressplot to visualize the results of Kruskal's non-metric multidimensional scaling

ordiplot(dune_iso_mds,type="t") #create an alternative plot and identify functions for ordination, inputting a result from an ordination, and labeling the points with their row number. Species scores will not be plotted.

dune_meta <- metaMDS(dune_bray,k=2,trymax=100) #using multiple starting points to optimize the new axes, choose the most optimal axes and present values as in isoMDS. Trymax specifies the maximum number of potential starting points to try. 
summary(dune_meta) 
dune_meta #the final stress value is about 0.1183, which is much smaller than the iso MDS final stress value.

stressplot(dune_meta,dune_bray) #create a stressplot to visualize the results of the non-metric multidimensional scaling witha stable solution from random starts, axis scaling, and species scores
ordiplot(dune_meta,type="t") #create an alternative plot and identify functions for ordination
