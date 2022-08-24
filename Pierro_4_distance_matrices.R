#10/4/21: in-class exercise 3 part II, and exercise 4

# calculate Manhattan distance, Euclidean distance, chord distance, and Bray-Curtis distance 
# use PcOA and biplots to see how different distance measures affect distance matrices

library(vegan)

rawdata <- read.csv("BFnature_exercise3.csv") #import the file glopnet from Shipley
glopnet2 = rawdata[c(10,11,12,16)] #create new version of glopnet with columns 10,11,12,16
glopnet2 <- na.omit(glopnet2) #omit NA from glopnet2

pca1 = princomp(glopnet2,cor=FALSE,scores=TRUE,covmat=NULL,fix_sign=TRUE) #run a PCA that uses a covariance matrix, with scores for each PCA axis for every row of data, using original values as our input dataframe, and assigning a positive value to the largest loading of each axis.
summary(pca1)

screeplot(pca1, bstick=TRUE) #create a SCREE plot of eigenvalues associated with the PCA plot of the glopnet2 data using a covariance matrix. SCREE plots show how many PCA axes you should show. In this case, the primary axis explains the vast majority of the data's variation.

biplot(pca1) #create a biplot with the PCA plot of the glopnet2 data. No intelligible pattern can be observed; revisions should be applied.
help(biplot)

biplot(pca1,cex=c(0.3,0.6)) #create abiplot with smaller text points and arrows. We are changing the character expansion factor for labeling points.

biplot(pca1,scale=0, cex=c(0.3,0.6)) #it is easier to see the distribution of points accross the PCA axes, although less information can be gathered from the angles between these axes

biplot(pca1,scale=0,cex=c(0.3,0.6),xlabs=rep(("."),nrow(glopnet2))) #turn each point's label into a tiny dot. This function creates a list of "." and uses it for the labels.

biplot(pca1,scale=1,cex=c(0.3,0.6),xlabs=rep(("."),nrow(glopnet2))) #now that the scale is 1, we can better interpret the angles between the axes

#part III (optional)

pca2=prcomp(glopnet2,retx=TRUE,center=TRUE,scale.=FALSE) #create a PCA plot using covariances, not correlations, centering the data, and returning x (calculating new coordinates for every row)
summary(pca2)
pca2$rotation
pca2$scale
pca2$center
head(pca2$x)

#exercise 4: part I: practice distance matrices

library(vegan)
library(ape)

bird1_1 = as.data.frame(cbind(rbind('A','B','C','D'),rbind(2,3,2,1),rbind(2,3,3,1))) #create a dataframe based on Table 4.1 on Shipley p.195. column 1 represents the female mother, olumn 2 represents the fledglings produced, and column 3 represents the eggs laid
colnames(bird1_1)=c("Female","Fledg", "Eggs")

bird1_1$Fledg=as.numeric(bird1_1$Fledg) #treat these columns as numeric
bird1_1$Eggs=as.numeric(bird1_1$Eggs)
bird1_1

bird1_2 = bird1_1[c(2,3)] #create a matrix with only the numeric (not categorical) values, by removing column 1
bird1_2

bird1_2_manhattan = vegdist(bird1_2,method="manhattan",diag=T) #create a Manhattan distance matrix with the bird data, including showing the 0 values on the diagonal.
bird1_2_manhattan

bird1_2_manhattan_calc =abs(bird2[1,1]-bird2[2,1])+abs(bird2[1,2]-bird2[2,2]) #calculate the Manhattan distance ( row 1 column 1 - row 2 column 1 ) + the same for column 2
bird1_2_manhattan_calc

bird1_2_euclidian = vegdist(bird1_2,method="euclidian",diag=T)
bird1_2_euclidian

bird1_2_euclidian_calc = sqrt((bird1_2[1,1]-bird1_2[2,1])^2+(bird1_2[1,2]-bird1_2[2,2])^2)
bird1_2_euclidian_calc

bird1_2_chord = vegdist(decostand(bird1_2,"norm"),method="euclidean",diag=T) #calculate the chord distances by normalizing the data and then calculating the euclidean distance
bird1_2_chord

vegdist(decostand(bird1_2,"total"),method="euclidean",diag=T) #calculate species profiles difference
vegdist(decostand(bird1_2,"hellinger"),method="euclidean",diag=T) #calculate Hellinger distance
vegdist(decostand(bird1_2,"chi.square"),method="euclidean",diag=T) #calcuate the chi squared distance

bird1_2_bray = vegdist(decostand(bird1_2, "norm"),method="bray",diag=T) #calcuate the bray curtis distance
bird1_2_bray

#exercise 4 part II: seeing effects of different distance measures

library(ape)
bird2_1 = as.data.frame(cbind(rbind('A','B','C','D','E','F','G'),rbind(2,3,2,1,2,1,0),rbind(2,3,3,1,4,3,1))) #create a new dataframe including 3 more birds with 2,1,and 0 fledglings and 4,3,and 1 eggs respectively
colnames(bird2_1)=c("Female","Fledg", "Eggs")

bird2_1$Fledg=as.numeric(bird2_1$Fledg) #treat these columns as numeric
bird2_1$Eggs=as.numeric(bird2_1$Eggs)
bird2_1

bird2_2 = bird2_1[c(2,3)] #create a matrix with only the numeric (not categorical) values of bird 2_1, by removing the first column
bird2_2


#recalculate the above distances for this data

bird2_2_manhattan = vegdist(bird2_2,method="manhattan",diag=T) #create a Manhattan distance matrix with the new bird data, including showing the 0 values on the diagonal.
bird2_2_manhattan

bird2_2_manhattan_calc =abs(bird2_2[1,1]-bird2_2[2,1])+abs(bird2_2[1,2]-bird2_2[2,2]) #calculate the Manhattan distance ( row 1 column 1 - row 2 column 1 ) + the same for column 2
bird2_2_manhattan_calc

bird2_2_euclidian = vegdist(bird2_2,method="euclidian",diag=T)
bird2_2_euclidian

bird2_2_euclidian_calc = sqrt((bird2_2[1,1]-bird2_2[2,1])^2+(bird2_2[1,2]-bird2_2[2,2])^2)
bird2_2_euclidian_calc

bird2_2_chord = vegdist(decostand(bird2_2,"norm"),method="euclidean",diag=T) #calculate the chord distances by normalizing the data and then calculating the euclidean distance
bird2_2_chord

vegdist(decostand(bird2_2,"total"),method="euclidean",diag=T) #calculate species profiles difference
vegdist(decostand(bird2_2,"hellinger"),method="euclidean",diag=T) #calculate Hellinger distance
vegdist(decostand(bird2_2,"chi.square"),method="euclidean",diag=T) #calcuate the chi squared distance

bird2_2_bray = vegdist(decostand(bird2_2, "norm"),method="bray",diag=T) #calcuate the Bray-Curtis distance
bird2_2_bray

#run a PcOA (principal coordinate analysis) on the Manhattan, Euclidian, chord, and bray curtis distance matrices for the seven-bird dataset

help(pcoa)

p_manhattan = pcoa(bird2_2_manhattan) #use PcOA on the manhattan distance matrix
head(p_manhattan)

p_euclidian = pcoa(bird2_2_euclidian) #use PcOA on the manhattan distance matrix
head(p_euclidian)

p_chord = pcoa(bird2_2_chord) #use PcOA on the chord distance matrix
head(p_chord)

p_bray = pcoa(bird2_2_bray) #use PcOA on the Bray-Curtis distance matrix
head(p_bray)

#use biplot to see how different distance matrices affect our analysis, specifying to plot axes 1 and 2.

biplot(x=p_manhattan, plot.axes=c(1,2))
biplot(x=p_euclidian, plot.axes=c(1,2))
biplot(x=p_chord, plot.axes=c(1,2))
biplot(x=p_bray, plot.axes=c(1,2))
