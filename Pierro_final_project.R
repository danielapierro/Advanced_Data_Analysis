#Daniela Pierro
#Dec 10, 2021
#Dr. T
#Advanced Data Analysis
#Final Project: Examining Plant Community Composition during Fire Recovery the Bernard Field Station

# Examining data collected after the Bernard Field Station fire, comparing plant cover between treatments C and R: control, vs. removal of non-native plants.

#install relevant packages and libraries
library(vegan)
library(tidyr)
library(purrr)
library(dplyr)
library(RVAideMemoire)

#read the data files shared by Dr. T on Nov 30th, containing post-fire BFS plant coverage data.
#all NA values were replaced with 0, all species columns with 0 total counts were removed, and the counts of species in each treatment group were aggregated into one sum number.
#to limit the number of columns, we grouped species both taxonomically and by life history categories
aggregated_data <- read.csv("/Users/danielapierro/Desktop/Pomona/Junior Year/Data_Analysis/BFS_project/Aggregated_BFS_cover_data.csv") #import aggregated numeric data with species grouped taxonomically
categorized_data <- read.csv("/Users/danielapierro/Desktop/Pomona/Junior Year/Data_Analysis/BFS_project/BFS_cover_data.csv") #import aggregated numeric data with species grouped by life history categories
View(aggregated_data)
View(categorized_data)
#read the data file for Treatments shared by Dr. T
treat <- read.csv("/Users/danielapierro/Desktop/Pomona/Junior Year/Data_Analysis/BFS_project/data/Treatments_corrected.csv") 

#remove the X column
aggregated_data_subset <- subset(aggregated_data,select = -c(X))
categorized_data_subset <- subset(categorized_data,select = -c(X))

dim(aggregated_data) #check that the column was removed
dim(aggregated_data_subset) 
dim(categorized_data)
dim(categorized_data_subset)

#run metaMDS using bray curtis distance
ord_agg=metaMDS(aggregated_data_subset,distance="bray",k=2,trymax=5000) #use meta MDS to "drop in" 5000 times in the parameter space to search for a local best-fit solution and converge on a global optimum.
ord_cat=metaMDS(categorized_data_subset,distance="bray",k=2,trymax=5000) #repeat for the categorized dataset

#visualize metaMDS results in stressplots
stressplot(ord_agg) #visualize the ord data in a stressplot, showing 2-dimensional distances that capture the multidimensional distances between data points. This plot compares observed dissimilarity with ordination distance
stressplot(ord_cat)

#plot the aggregated data

colorvalues <- c("#D55E00","#009E73")

plot(ord_agg,type="n") #plot our data, beginning with a generic blank plot
ordispider(ord_agg,treat$x,col=colorvalues,label=TRUE,cex=0.7) #examine variation among years (center points) across plots sampled within years (tips of radiating lines)

#text(ord_agg,display="species",cex=0.7) #label the species
#points(ord_agg,disp="sites") #show locations of each plot

#to simplify the ordispider plot, we will filter out species labels to only include the most relevant species: the most abundant as determined by an inverse Simpson measure of diversity
relevant_species_agg = diversity(aggregated_data_subset,index="invsimpson",MARGIN=2)
orditorp(ord_agg,display="species",priority=relevant_species_agg,col="black",cex=0.75) #remake the plot with syntax to include on priority species lables, drawing on priSpp to determine which species are plotted


#repeat ordispider for data categorized by life history

plot(ord_cat,type="n") #plot our data, beginning with a generic blank plot
ordispider(ord_cat,treat$x,col=colorvalues,label=TRUE,cex=0.7)

relevant_species_cat = diversity(categorized_data_subset,index="invsimpson",MARGIN=2)
orditorp(ord_cat,display="species",priority=relevant_species_cat,col="black",cex=0.75) #remake the plot with syntax to include on priority species lables, drawing on priSpp to determine which species are plotted

#calculate bray-curtis distances

unlist(lapply(aggregated_data_subset,class)) #check all columns are numeric
unlist(lapply(categorized_data_subset,class)) #check all columns are numeric

bc_agg = vegdist(aggregated_data_subset, "bray") #calculate bray-curtis distance using the numeric data
results_agg=meandist(bc_agg,treat) #calculate mean distances among treatments
summary(results_agg) #the bray-curtis distance is about 0.3096085

bc_cat = vegdist(categorized_data_subset, "bray") #calculate bray-curtis distance using the numeric data
results_cat=meandist(bc_cat,treat) #calculate mean distances among treatments
summary(results_cat) #the bray-curtis distance is slightly smaller: 0.227758

#PERMANOVA
permanova_agg=adonis(aggregated_data~treat$x,method="bray",perm=999) #run a PERMANOVA. We use the Bray Curtis distance, and assess whether the treatments are significantly different. We will run 999 permutations.
permanova_agg #0.964, insignificant difference between the treatment groups

permanova_cat=adonis(categorized_data~treat$x,method="bray",perm=999) #run a PERMANOVA. We use the Bray Curtis distance, and assess whether the treatments are significantly different. We will run 999 permutations.
permanova_cat #0.904, insignificant difference between the treatment groups

#check the homogeneity of variances, by assessing multivariate homogeneity of group dispersions 
betadisper(bc_agg,treat$x) 
betadisper(bc_cat,treat$x)

#in analyses comparing the control and treatment groups for both species categorizations, there is about as much variation between groups as within groups.
#future directions could include a random effects model