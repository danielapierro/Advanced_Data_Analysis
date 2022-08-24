#exercise 7

#use PERMANOVA to test for differences between groups in multivariate data sets

library(vegan)
library(RVAideMemoire)

thatchdata <- read.csv("/Users/danielapierro/Desktop/Pomona/Junior Year/Data_Analysis/march2020thatchdata.csv") #read the data file collected by Elena in March, 2020
View(thatchdata)

full1 <- thatchdata[c(4:14)] #remove three non-numeric columns
head(full1)

unlist(lapply(full1,class)) #check that all columns are numeric

ord1=metaMDS(full1,distance="bray",k=2,trymax=500) #use meta MDS to "drop in" 500 times in the parameter space to search for a local best-fit solution and converge on a global optimum.
stressplot(ord1) #visualize the ord1 resulting from metaMDS in a stressplot

plot(ord1,type="n")
ordispider(ord1,thatchdata$loc,col=colorvals3,label=TRUE,cex=0.66) #there are three distinct clusters
priSpp=diversity(full1,index="invsimpson",MARGIN=2) #determine the most abundant (priority) species labels
orditorp(ord1,display="species",priority=priSpp,col="black",cex=0.75) #include species names for priority species


bc=vegdist(full1,"bray") #calculate bray-curtis distance using the numeric data
results=meandist(bc,thatchdata$loc) #calculate mean distances among the three locations
results

presults=adonis(full1~thatchdata$loc,method="bray",perm=999) #run a PERMANOVA. We will use the Bray Curtis distance measure, and assess whether any locations are significantly different from other locations. We will run 999 permutations.
presults

pairwise.perm.manova(bc,thatchdata$loc,p.method="BH",nperm=999) #adjust p-values for multiple comparisons, using the Benjamini-Hochberg method

betadisper(bc,thatchdata$loc) #check the homogeneity of variances, by assessing multivariate homogeneity of group dispersions.

#For these results, the locations NE and SW have similar within-group variation, while SE has lower within-group variation. This result is evidence for some heterogeneity of variances, but likely not enough for us to question the validity of the overall results.
