
#exercise 6, in-class work 10/25

cover <- read.csv("/Users/danielapierro/Desktop/Pomona/Junior Year/Data_Analysis/All_years_cover_data.csv")
head(cover)

cover= cover[c(1,3:27)] #omit column 2, with dates

treats <- read.csv("/Users/danielapierro/Desktop/Pomona/Junior Year/Data_Analysis/treats.csv")
head(treats)

cover=merge(cover,treats,by=c("Block","Plot")) #merge the cover data file with the treatment group file by matching rows in the two dataframes.

cover=subset(cover,Year<2014&Weed==0|Year>2013) #remove plant plots that were weeded from pre-fire years. Take data that was pre-fire (before 2014) and not weeded, or post-fire (after 2013) data.

dim(cover) #there are 14238 rows

cover$Plot=NULL #removing Plot allows the aggregate function to run
cover

acover=aggregate(.~Block+Year,cover,mean,na.rm=TRUE) #combine the points into one mean per year/plot. This mean estimates the proportion of plot cover each plant has. Total cover is > 1 if points have multiple plants touching them.
acover

acover$Plot=acover$Block=acover$Point=acover$X=acover$Date=acover$Weed=acover$Seed=NULL #take out variables not used in this analysis.
head(acover)

acovermet=as.data.frame(acover$Year) #take out year but save its information first
colnames(acovermet)=c("Year")
acover$Year=NULL

acover$Cmel=acover$Ugrass=NULL #since unusual species can confound analyses, two species that occur very infrequently should be removed.

#run NMDS!
library(vegan)

ord1=metaMDS(acover,distance="bray",k=2,trymax=500) #use meta MDS to "drop in" 500 times in the parameter space to search for a local best-fit solution and converge on a global optimum.
stressplot(ord1) #visualize the ord1 data in a stressplot, showing 2-dimensional distances that capture the multidimensional distances between data points.

plot(ord1,type="n") #plot our data, beginning with a generic blank plot
points(ord1,disp="sites") #add site and species points and text
text(ord1,disp="species")

colorvals3=c("#009E73","#009E73","#009E73","#009E73","brown","#D55E00","#E69F00","black") #create a color palette

#designate numbers that represent the year of data collection in terms of time before or after the fire.
acovermet=transform(acovermet,fire=0)
acovermet$fire[acovermet$Year==2014]=1
acovermet$fire[acovermet$Year==2015]=2
acovermet$fire[acovermet$Year==2016]=3
acovermet$fire[acovermet$Year==2017]=4
acovermet$fire[acovermet$Year==2013]=-1
acovermet$fire[acovermet$Year==2012]=-2
acovermet$fire[acovermet$Year==2011]=-3
acovermet$fire[acovermet$Year==2010]=-4

acovermet$Year=NULL #delete Year from acovermet
head(acovermet)


plot(ord1,type="n")
ordispider(ord1,acovermet$fire,col=colorvals3,label=TRUE,cex=0.7) #examine variation among years (center points) across plots sampled within years (tips of radiating lines)
text(ord1,display="species",cex=0.7) #label the species
points(ord1,disp="sites") #show locations of each plot


priSpp = diversity(acover,index="invsimpson",MARGIN=2)#simplify the plot by filtering out species labels, only using the most relevant species (the most abundant ones) through an inverse Simpson measure of diversity
priSpp #we want to show Bdi, BR, and Vul.

plot(ord1,type="n")
ordispider(ord1,acovermet$fire,col=colorvals3,label=TRUE,cex=0.66)
orditorp(ord1,display="species",priority=priSpp,col="black",cex=0.75) #remake the plot with syntax to include on priority species lables, drawing on priSpp to determine which species are plotted

tiff(file="Pierro_practice_plot_data_analysis.tiff",width=12,height=12,units="cm",res=600) #open a new .tiff image type file
priSpp=diversity(acover,index="invsimpson",MARGIN=2) #determine the most abundant (priority) species labels
plot(ord1,type="n") #make a default blank graph
ordispider(ord1,acovermet$fire,col=colorvals3,label=TRUE,cex=.66) #use the designated colors to plot axes
orditorp(ord1,display="species",priority=priSpp,col="black",cex=.75) #include only priority species labels
dev.off() #done making the image. It is saved into the working directory.
