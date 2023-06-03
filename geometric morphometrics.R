### Year : 2021
### Authors : Camille Leblanc, Bjarni K. Kristjansson, Marie Joigneau (Erwan Le Bechec, Sarah Steele)
### Organism : Holar University
### Project : Is the morphologic variation (body and head shape) of Arctic charr  linked to some ecological factors? 
###           For this question I will use data from all caves in June 2020 and June 2021

###################
### INTRODUCTION ##
###################

### Working directory we are working in :
setwd("D:/stageCESURE-Islande/Statistic_study/R analysis shape - project 1/R analysis")

### If your environment is empty (environment is in .RData)
load("D:/stageCESURE-Islande/Statistic_study/R analysis shape - project 1/R analysis/.RData")

### Opens the libraries we are going to use :
library(geomorph)
library(ggplot2)
library(MASS)
library(dplyr)
library(tidyverse) 
library(scales) 
library(ggpubr)
library(factoextra)
library(ggplot2)
library (gridExtra)
library(grid)
library(lattice)
library(vegan)
library(cowplot)
library(Rmisc)
library(reshape2)
library(broom)
library(emmeans)
library(lsmeans)
library(multcomp)
library(readxl)
library(ecodist)
library(xlsx)
library(FactoMineR)
library(caret)

#######################################################
### PART 0 : ordering and merging 2020 and 2021 June ##
#######################################################

### ---------------- Add genetic cluster for after ----------------------------------

### Here we linked the caves to the genetic clusters to be able to add the genetic cluster linked to the cave in our dataset later

### Caves by Genetic cluster
# H-C : C7, C12 and C25 
# H-N : C5,C6, C10, C11, C27
# H-S : C1, C2, C26
# V-E : C21, C22, C23, C24
# V-W : C17, C17B, C18, C19, C20

### The order of the genetic cluster is adapted to the order of the cave (not chosen)
Caves.Right.Order.Genetic.Cluster.C <-c("C1","C10","C11","C12","C17","C17B","C18",
                                       "C19","C2","C20","C21","C22","C23","C24",
                                       "C25","C26","C27","C5","C6","C7")
Caves.Right.Order.Genetic.Cluster <-c("1","10","11","12","17","17B","18",
                                      "19","2","20","21","22","23","24",
                                      "25","26","27","5","6","7")
Genetic.Cluster.Linked <- c("H-S","H-N","H-N","H-C","V-W","V-W","V-W",
                              "V-W","H-S","V-W","V-E","V-E","V-E","V-E",
                              "H-C","H-S","H-N","H-N","H-N","H-C")
ID.Genetic.Cluster.Model <- data.frame(Location=Caves.Right.Order.Genetic.Cluster.C,Genetic.Cluster=Genetic.Cluster.Linked)

### ---------------- YEAR 2021 --------------------

### Here we sort the year 2021 csv according to the 2021 tps.

morph.data2021<-readland.tps("2021June4_modif_after_meeting.tps",specID="ID",negNA = TRUE)
ID2021 <- read.csv("2021June6.csv", header=TRUE, sep=";", dec=".",stringsAsFactors = TRUE)

### We add the genetic cluster before the sorting because adding it modify the order
ID2021 <- merge(ID2021, ID.Genetic.Cluster.Model, by ="Location")

sliders2<-matrix(c(2,3,4,5,6,7,10,11,12,14,15,16,15,16,17,16,17,18),ncol=3,byrow=TRUE)
cave.gpa2021<-gpagen(morph.data2021,curves=sliders2) 

### A programm to order the csv file ID2021
# j the row of the new dataframe
# i the row of the csv chosen to be put in the right order
listeIDTps <- dimnames(cave.gpa2021$coords)[[3]]
IDLast2021 <- ID2021
j<-1
space <- ""
for (j in (1:length(listeIDTps))){ # we visit every ID of the tps file
  for (i in (1:length(ID2021$IDPicture))){ # we visit every row of the csv file
    # we check if it's the same ID, taking into account that some ID from the tps has a space (it is "123456 " or "123456") :
    if ((as.character(ID2021$IDPicture[i]) == listeIDTps[j]) || (paste(as.character(ID2021$IDPicture[i]),space)==listeIDTps[j])){
      IDLast2021[j,] <- ID2021[i,]
      j <- j+1 # we go find the next ID of the tps file
    }
  }
}
ID2021 <- IDLast2021

### Here I check my programm has worked well with the dataframe:
Check.It.Is.Fine.2021 <- data.frame(IDtps=listeIDTps,IDcsv=ID2021$IDPicture)

### Here the dataframe finished :
write.csv(ID2021,"D:/stageCESURE-Islande/Statistic_study/R analysis shape - project 1/2021JuneOrdered3.csv", row.names = FALSE)

### ---------------- YEAR 2020 --------------------

### Here we sort the year 2020 csv according to the 2020 tps.

morph.data2020<-readland.tps("2020June.tps",specID="ID",negNA = TRUE)
ID2020 <- read.csv("2020June.csv", header=TRUE, sep=";", dec=".",stringsAsFactors = TRUE)

### We add the genetic cluster before the sorting because adding it modify the order
ID2020 <- merge(ID2020, ID.Genetic.Cluster.Model, by ="Location")

sliders2<-matrix(c(2,3,4,5,6,7,10,11,12,14,15,16,15,16,17,16,17,18),ncol=3,byrow=TRUE)
cave.gpa2020<-gpagen(morph.data2020,curves=sliders2) 

### A programm to order the csv file ID2020 (same as the 2021 year)
# j the row of the new dataframe
# i the row of the csv chosen to be put in the right order
listeIDTps <- dimnames(cave.gpa2020$coords)[[3]]
IDLast2020 <- ID2020
j<-1
space <- ""
for (j in (1:length(listeIDTps))){
  for (i in (1:length(ID2020$IDPicture))){
    if ((as.character(ID2020$IDPicture[i]) == listeIDTps[j]) || (paste(as.character(ID2020$IDPicture[i]),space)==listeIDTps[j])){ #the 5th row for the 1st of the list
      IDLast2020[j,] <- ID2020[i,]
      j <- j+1
    }
  }
}
ID2020 <- IDLast2020

### Here I check my programm has worked well with the dataframe:
Check.It.Is.Fine.2020 <- data.frame(IDtps=listeIDTps,IDcsv=ID2020$IDPicture)

### Here the dataframe finished :
write.csv(ID2020,"D:/stageCESURE-Islande/Statistic_study/R analysis shape - project 1/2020JuneOrdered2.csv", row.names = FALSE)

### ---------------- Merge 2020 2021 ----------------------------------

ID2021 <- read.csv("2021JuneOrdered3.csv", header=TRUE, sep=",", dec=".",stringsAsFactors = TRUE)
ID2020 <- read.csv("2020JuneOrdered2.csv", header=TRUE, sep=",", dec=".",stringsAsFactors = TRUE)

### Let's merge the 2 dataframe ordered
IDall <- rbind(ID2020, ID2021)
write.csv(IDall,"D:/stageCESURE-Islande/Statistic_study/R analysis shape - project 1/2020and2021June_ordered3.csv", row.names = FALSE)

###########################
### PART I.1 : import data ##
###########################

### The data has been unbent
# Here the protocol :
# https://www.youtube.com/watch?v=c3LuRTaax4M&list=PL_Koeo1H0v_DTBsnnQ8_LsQXh6soJwXn9&index=1&ab_channel=WilliamPerry
# https://www.academia.edu/498700/Guide_to_the_Unbend_specimens_module_in_tpsUtil


### morph.data = reads the landmarks in, link between ID and landmarks
# x & y coordinates multiplicated by the scale here
# here we don't use "negNA = TRUE" because the tps has been unbend, so has negative values
Morph.Data<-readland.tps("2020and2021June_modif_after_meeting_unbend_outliers_corrected4_point.tps",specID="ID")

### Taking care of the case of open mouth individuals
# Below I remove for May unbent data landmarks 1 and 20 for those fish that have open mouth
# Open.Mouth.2020.2021 : list of the number of fishes with opened mouth
# 1-415 : year 2020, 416-825 (410 fishes) : year 2021
Open.Mouth.2020.2021 <- c(4,5,8,13,17,20,23,24,36,40,43,44,45,46,53,63,64,70,79,81,85,
                              87,95,107,114,115,118,127,131,169,182,183,204,206,211,220,
                              233,263,267,299,306,308,311,313,314,318,344,345,346,348,349,
                              351,360,372,378,389,390,392,397,401,405,411,415,417,418,419,
                              420,422,423,425,428,430,432,435,436,440,447,454,463,475,476,
                              478,483,489,490,505,508,518,524,526,545,549,560,561,563,564,
                              566,575,577,580,585,586,634,637,639,640,641,644,646,652,658,
                              679,690,712,713,719,729,758,799,800,801,804,808,815,816,818,
                              821)
length(Open.Mouth.2020.2021) # number of fishes with mouth opened : 127
(length(Open.Mouth.2020.2021)/825)*100 # pourcentage to visualise better  : 15% of fishes with mouth opened in the wole dataset !
Morph.Data[c(1,20),,Open.Mouth.2020.2021]<-NA
Morph.Data<-estimate.missing(Morph.Data,method="TPS")

### ID = link between individual and data (lenght, cave ...)
# (Warning : here in the right order, 2020 first, 2021 second)
ID <- read.csv("2020and2021June_ordered3.csv", header=TRUE, sep=",", dec=".",stringsAsFactors = TRUE)

###################################################################
### PART I.2 : fusion of .tps and .csv : obtention of the dataframe #
###################################################################

### define semi landmarks by creating a matrix 
# sliding landmarks here : 3 (between 2 and 4), 5 (4-6), 11 (10-12), 15 (14-16), 16 (15-17), 17(16-18)
sliders2<-matrix(c(2,3,4,5,6,7,10,11,12,14,15,16,15,16,17,16,17,18),ncol=3,byrow=TRUE)

### procrustes surperposition (aligned and scaled coordinates), try to match datas to compare really the landmarks
# coords = new datas we will be working on that is now aligned and scaled
# Csize = Centroid size is the measure of size used almost universally in geometric morphometrics: it is the square root of the sum of squared distances of all the landmarks of an object from their centroid (center of gravity, whose location is obtained by averaging the x and y coordinates of all landmarks).
# The rest can be ignored
cave.gpa2<-gpagen(Morph.Data,curves=sliders2) 

### Check if it is into the right format
class(ID$Location)
class(ID$year)
class(ID$Length..mm.)
ID$Location <- as.factor(ID$Location) #caves
ID$year <- as.factor(ID$year)
ID$Length..mm. <- as.numeric(ID$Length..mm.)

### Melting of the 2 datas which match
# log limit variability, only for quantitative data (length, weight...)
# log2 instead of log for easier interpretation, 2 columns for each, one with, and one without (warning: interpreation of the graphs!)
# log2 explaination : https://rstudio-pubs-static.s3.amazonaws.com/13988_bb11d85b79b2436280de434988558140.html 
Datafr.gdf2<-geomorph.data.frame(cave.gpa2, caves=ID$Location, year=ID$year, length=ID$Length..mm.,cavesyear=interaction(ID$Location,ID$year),logCsize=log2(cave.gpa2$Csize),loglength=log2(ID$Length..mm.),Genetic.Cluster=ID$Genetic.Cluster)

###############################
### PART I.3 : outliers?        #
###############################

### plots of identified outlier specimens compared to the mean shape (plot each landmark, and inspect outliers, scroll the plots to see the outliers)
# we see that even with unbend specimens we have outliers
plotOutliers(cave.gpa2$coords, inspect.outliers = TRUE)    
plot(cave.gpa2)
print(plotOutliers(cave.gpa2$coords, inspect.outliers = TRUE))
#### List of the outliers in the whole dataset :
### Print = here the list of the whole dataset, those on top are the outliers (need to know the first outlier with the plot for that, from bottom to top, from right to left)
# Permit to know the line of the fish to after know the year / cave
# 897881  CAL20-4515      981927      655457  CAL20-4503      123950  
# 312         134         608         131          48         313 
# CAL20-4524  
# 77
list_outliers <- c(312,134,608,131,48,313)
# We have 6 outliers
length(list_outliers)
# That lead to 0.73% of the dataset, it's really good !!
(length(list_outliers)/length(ID$IDPicture))*100

### Finding outliers with a huge Csize
# for (i in 1:length(ID$IDPicture)){
#   if (Datafr.gdf2$logCsize[i]>0){
#     print(paste("ID :",ID$IDPicture[i]," caves:",ID$Location[i]," logCsize:",Datafr.gdf2$logCsize[i]," year:",ID$year[i]))
#   }
# }

### Why are they outliers ? A df is created with only them :
ID_outliers <- ID[(list_outliers),]
write.csv(ID_outliers,"D:/stageCESURE-Islande/Statistic_study/R analysis shape - project 1/2020and2021June_outliers4.csv", row.names = FALSE)
### Checking the landmarks :
# - they are ok
# - I have checked some big and small fishes not outliers to see if I haven't done good landmarks on the 7-8-9-10 but no
### Checking the length:
# The outliers have a length more important
sum(ID$Length..mm.)/length(ID$Length..mm.) #average (82.9mm)
sum(ID_outliers$Length..mm.)/length(ID_outliers$Length..mm.) #outliers (99.67mm)
### Checking the weight
# no weight for fish line 465, so I don't count it for the calcul
# The outliers have a weight more important !
IDweight <- ID
IDweight <- ID[-465,]
sum(IDweight$Weight..g)/length(IDweight$Weight..g.) #average (6.4g)
sum(ID_outliers$Weight..g.)/length(ID_outliers$Weight..g.) #outliers (13.03g)
### Checking the caves : they don't come from a specific cave
### Checking the year : 5 from 2020/6
### Checking if dead fishes? : no


#####################################
### PART I.4 : basic statistical study#
#####################################

### ---------------- correlation or not?--------------------------------------

### Correlations between logCsize and length?
# a correlation is strong when in [-1,-0.5]U[0.5,1]
# positive correlation (R²=0.3; P<0.001), so the 2 values augment together but not that high
# it means that I can have not the two measures of size in the same model
cor.test(Datafr.gdf2$logCsize, Datafr.gdf2$loglength) # log² with corr = 0.26
# cor.test(Datafr.gdf2$Csize, Datafr.gdf2$length) # with corr = 0.29
# cor.test(log(Datafr.gdf2$Csize), log(Datafr.gdf2$length)) # log with corr = 0.26 = log²

plot(Datafr.gdf2$loglength, Datafr.gdf2$logCsize, type="n", xlab="log2(Fork length in mm)", ylab="log2 Centroid Size", main = "Correlation")
points(Datafr.gdf2$loglength, Datafr.gdf2$logCsize,col="red")
abline(lm(Datafr.gdf2$logCsize~Datafr.gdf2$loglength) ,col="black") 

### ---------------- effects or not by cave and year? ----------------
### -- effects or not on logCsize?-----------------------------------------

### Here we test the analyse of variance model with 2 factors with interaction (ANOVA)
# (Analyse of variance model : qualitative sources of variability)
# (so not the quantitative explanatory variable which is the length)
# model that we'll test :
# ∀i,j,k with (i,j,k)≠(i’j,’,k’)
# εijk ∼ N (0, σ) and cov(εijk,εi’j’k’)≠0
# Y(i,j,k) = µ + α(i) + β(j) + (αβ)(i,j) + ε(k)
# Y(i,j,k) logCsize 
# µ average logCsize 
# α(i) effect of the cave
# β(j) effect of the year
# (αβ)(i,j) effect of the interaction caves:year

### I choose the colors related to the genetic cluster and the order of the caves in the boxplot
# Red = H-C, yellow = H-N, green = H-S, blue =V-E, violet = V-W
Color_caves_genetic_cluster <- c("green","yellow","yellow","red","violet","violet","violet",
                                  "violet","green","violet","blue","blue","blue","blue",
                                  "red","green","yellow","yellow","yellow","red")

### CAVE EFFECT : yes
# model here : logCsize ~ cave
# p-value < 0.05 so significant effect !
ANCsize<-aov(Datafr.gdf2$logCsize~Datafr.gdf2$caves)
summary(ANCsize) 
boxplot(Datafr.gdf2$logCsize~Datafr.gdf2$caves, xlab= "cave", ylab= "log2(Centroid Size)", main="Cave effect on Csize",las=2,col="grey") # color by year
boxplot(Datafr.gdf2$logCsize~Datafr.gdf2$caves, xlab= "cave", ylab= "log2(Centroid Size)", main="Cave effect on Csize",las=2,col=Color_caves_genetic_cluster) # color by genetic cluster
# mu, C18 and C23 are significant
AovSum(Datafr.gdf2$logCsize~Datafr.gdf2$caves)

### YEAR EFFECT : yes
# model here : logCsize ~ year
# p-value < 0.05 so significant effect !
ANCsize2<-aov(Datafr.gdf2$logCsize~Datafr.gdf2$year)
summary(ANCsize2)
boxplot(Datafr.gdf2$logCsize~Datafr.gdf2$year, xlab= "year", ylab= "log2(Centroid Size)", col= c("red", "orange"), main = "Year effect on Csize")
# all year (2020 and 2021) significant
AovSum(Datafr.gdf2$logCsize~Datafr.gdf2$year)

### CAVE*YEAR EFFECT : yes 
# model here : logCsize ~ cave*year
# p-value < 0.05 here
ANCsize3<-aov(Datafr.gdf2$logCsize~Datafr.gdf2$caves:Datafr.gdf2$year)
summary(ANCsize3)
boxplot(Datafr.gdf2$logCsize~Datafr.gdf2$caves:Datafr.gdf2$year, xlab= "Cave and year", ylab="log2(Centroid Size)", main="Cave x year effect on Csize",col=c("red","red","red","red","red","red","red","red","red","red","red","red","red","red","red","red","red","red","red","red","orange","orange","orange","orange","orange","orange","orange","orange","orange","orange","orange","orange","orange","orange","orange","orange","orange","orange","orange","orange")) # color by year
boxplot(Datafr.gdf2$logCsize~Datafr.gdf2$caves:Datafr.gdf2$year,las=2, xlab= "Cave and year", ylab="log2(Centroid Size)", main="Cave x year effect on Csize",col=c(Color_caves_genetic_cluster,Color_caves_genetic_cluster)) # color by genetic cluster
# C7:2021, C18:2021, C7:2020 and C18:2020 are significant :
AovSum(Datafr.gdf2$logCsize~Datafr.gdf2$caves*Datafr.gdf2$year)

### Normality of the residuals of ANCsize3 : all seems ok ! 
# for shapiro test, if p-value < 0.05, the test it is ok
# the residuals need to have a bell form close to 0 to be normals, it's not a proof but can say if there is a problem
hist(ANCsize3$residuals)
shapiro.test(ANCsize3$residuals)

### Post hoc test
# = to see if the groups that are particularly different, works only with factors
# we have many interactions significantly different from each other, mostly between the 2 years
getOption("max.print") #we have just 1000, it's not sufficient
options(max.print=99999) #so we change the limit
TukeyHSD(ANCsize3, group = interaction(Datafr.gdf2$caves, Datafr.gdf2$year))
plot(TukeyHSD(ANCsize3, conf.level=.95), las = 2)
options(max.print=1000) #we put the limit back at normal

### Model here that we keep :
# ∀i,j,k with (i,j,k)≠(i’j,’,k’)
# εijk ∼ N (0, σ) and cov(εijk,εi’j’k’)≠0
# Y(i,j,k) = µ + α(i) + (αβ)(i,j) + ε(k)
# Y(i,j,k) logCsize 
# µ average logCsize 
# β(j) effect of the year
# α(i) effect of the cave
# (αβ)(i,j) effect of the interaction caves:year

### -- effects or not on loglength?-----------------------------------------

### Here we test the analyse of variance model with 2 factors with interaction (ANOVA)
# (Analyse of variance model : qualitative sources of variability)
# (so not the quantitative explanatory variable which is the length)
# model ANOVA that we'll test :
# ∀i,j,k with (i,j,k)≠(i’j,’,k’)
# εijk ∼ N (0, σ) and cov(εijk,εi’j’k’)≠0
# Y(i,j,k) = mu + α(i) + β(j) + (αβ)(i,j) + ε(k)
# Y(i,j,k) loglength 
# mu average loglength
# α(i) effect of the cave
# β(j) effect of the year
# (αβ)(i,j) effect of the interaction caves:year

### CAVE EFFECT : yes
# model here : loglength ~ cave
# p-value < 0.05 here so significant effect !
ANCsizeb<-aov(Datafr.gdf2$loglength~Datafr.gdf2$caves)
summary(ANCsizeb) 
boxplot(Datafr.gdf2$loglength~Datafr.gdf2$caves, xlab= "caves", ylab="log2(Fork length in mm)",las=2, col.box= NULL, main = "Caves effect on fork length",col="grey") # color by year
boxplot(Datafr.gdf2$loglength~Datafr.gdf2$caves, xlab= "caves", ylab="log2(Fork length in mm)",las=2, col.box= NULL, main = "Caves effect on fork length",col=Color_caves_genetic_cluster) # color by genetic cluster
# mu, C1, C22, C23, C25, C27 are significant
AovSum(Datafr.gdf2$loglength~Datafr.gdf2$caves)

### YEAR EFFECT : yes
# model here : loglength ~ year
# p-value < 0.05 so significant effect !
ANCsize2b<-aov(Datafr.gdf2$loglength~Datafr.gdf2$year)
summary(ANCsize2b)
boxplot(Datafr.gdf2$loglength~Datafr.gdf2$year, xlab= "year", ylab= "log2(Fork length in mm)", col= c("red", "orange"), main = "Year effect on fork length")
# all year (2020 and 2021) significant
AovSum(Datafr.gdf2$loglength~Datafr.gdf2$year)

### CAVE*YEAR EFFECT : yes
# model here : loglength ~ cave*year
# p-value < 0.05 here so significant !
ANCsize3b<-aov(Datafr.gdf2$loglength~Datafr.gdf2$caves:Datafr.gdf2$year)
summary(ANCsize3b)
boxplot(Datafr.gdf2$loglength~Datafr.gdf2$caves:Datafr.gdf2$year, xlab= "Cave and year", ylab="log2(Fork length in mm)", main="Cave x year effect on fork length", col=c("red","red","red","red","red","red","red","red","red","red","red","red","red","red","red","red","red","red","red","red","orange","orange","orange","orange","orange","orange","orange","orange","orange","orange","orange","orange","orange","orange","orange","orange","orange","orange","orange","orange"))
boxplot(Datafr.gdf2$loglength~Datafr.gdf2$caves:Datafr.gdf2$year, xlab= "Cave and year", ylab="log2(Fork length in mm)", main="Cave x year effect on fork length",col=c(Color_caves_genetic_cluster,Color_caves_genetic_cluster))
# C11:2020, C25:2020, C26:2020, C5:2020, C11:2021, C25:2021, C26:2021, C5:2021 are significant :
AovSum(Datafr.gdf2$loglength~Datafr.gdf2$caves*Datafr.gdf2$year)

### Normality of the residuals of ANCsize3 : all seems ok ! 
# for shapiro test, if p-value < 0.05, the test it is ok
# the residuals need to have a bell form close to 0 to be normals, it's not a proof but can say if there is a problem
hist(ANCsize3b$residuals)
shapiro.test(ANCsize3b$residuals)

### Post hoc test
# = to see if the groups that are particularly different, works only with factors
getOption("max.print") #we have just 1000, it's not sufficient
options(max.print=99999) #so we change the limit
TukeyHSD(ANCsize3b, group = interaction(Datafr.gdf2$caves, Datafr.gdf2$year))
plot(TukeyHSD(ANCsize3, conf.level=.95), las = 2)
options(max.print=1000) #we put the limit back at normal

### Model ANOVA here that we keep :
# ∀i,j,k with (i,j,k)≠(i’j,’,k’)
# εijk ∼ N (0, σ) and cov(εijk,εi’j’k’)≠0
# Y(i,j,k) = mu + α(i) + β(j) + (αβ)(i,j) + ε(k)
# Y(i,j,k) loglength 
# mu average loglength 
# α(i) effect of the cave
# β(j) effect of the year
# (αβ)(i,j) effect of the interaction caves:year

### ---------------- effects or not by genetic cluster and year?------
### -- effects or not on logCsize?-----------------------------------------

### Here we test the analyse of variance model with 2 factors with interaction (ANOVA)
# (Analyse of variance model : qualitative sources of variability)
# (so not the quantitative explanatory variable which is the length)
# model that we'll test :
# ∀i,j,k with (i,j,k)≠(i’j,’,k’)
# εijk ∼ N (0, σ) and cov(εijk,εi’j’k’)≠0
# Y(i,j,k) = µ + α(i) + β(j) + (αβ)(i,j) + ε(k)
# Y(i,j,k) logCsize 
# µ average logCsize 
# α(i) effect of the genetic cluster
# β(j) effect of the year
# (αβ)(i,j) effect of the interaction genetic cluster:year

### GENETIC CLUSTER EFFECT : yes
# model here : logCsize ~ Genetic.Cluster
# p-value < 0.05 so significant effect !
ANCsize<-aov(Datafr.gdf2$logCsize~Datafr.gdf2$Genetic.Cluster)
summary(ANCsize) 
boxplot(Datafr.gdf2$logCsize~Datafr.gdf2$Genetic.Cluster, xlab= "genetic cluster", ylab= "log2(Centroid Size)", main="Genetic.Cluster effect on Csize",las=2,col="grey")
# mu, H-C, H-S and V-E are significant
AovSum(Datafr.gdf2$logCsize~Datafr.gdf2$Genetic.Cluster)

### GENETIC CLUSTER*YEAR EFFECT : yes 
# model here : logCsize ~ Genetic.Cluster*year
# p-value < 0.05 here
ANCsize3<-aov(Datafr.gdf2$logCsize~Datafr.gdf2$Genetic.Cluster:Datafr.gdf2$year)
summary(ANCsize3)
boxplot(Datafr.gdf2$logCsize~Datafr.gdf2$Genetic.Cluster:Datafr.gdf2$year, xlab= "Genetic.Cluster and year", ylab="log2(Centroid Size)", main="Genetic.Cluster x year effect on Csize",col="grey")
AovSum(Datafr.gdf2$logCsize~Datafr.gdf2$Genetic.Cluster*Datafr.gdf2$year)

### Normality of the residuals of ANCsize3 : all seems ok ! 
# for shapiro test, if p-value < 0.05, the test it is ok
# the residuals need to have a bell form close to 0 to be normals, it's not a proof but can say if there is a problem
hist(ANCsize3$residuals)
shapiro.test(ANCsize3$residuals)

### Post hoc test
# = to see if the groups that are particularly different, works only with factors
# we have many interactions significantly different from each other, mostly between the 2 years
getOption("max.print") #we have just 1000, it's not sufficient
options(max.print=99999) #so we change the limit
TukeyHSD(ANCsize3, group = interaction(Datafr.gdf2$caves, Datafr.gdf2$year))
plot(TukeyHSD(ANCsize3, conf.level=.95), las = 2)
options(max.print=1000) #we put the limit back at normal

### Model here that we keep :
# ∀i,j,k with (i,j,k)≠(i’j,’,k’)
# εijk ∼ N (0, σ) and cov(εijk,εi’j’k’)≠0
# Y(i,j,k) = µ + α(i) + (αβ)(i,j) + ε(k)
# Y(i,j,k) logCsize 
# µ average logCsize 
# β(j) effect of the year
# α(i) effect of the Genetic.Cluster
# (αβ)(i,j) effect of the interaction Genetic.Cluster:year

### -- effects or not on loglength?-----------------------------------------

### Here we test the analyse of variance model with 2 factors with interaction (ANOVA)
# (Analyse of variance model : qualitative sources of variability)
# (so not the quantitative explanatory variable which is the length)
# model ANOVA that we'll test :
# ∀i,j,k with (i,j,k)≠(i’j,’,k’)
# εijk ∼ N (0, σ) and cov(εijk,εi’j’k’)≠0
# Y(i,j,k) = mu + α(i) + β(j) + (αβ)(i,j) + ε(k)
# Y(i,j,k) loglength 
# mu average loglength
# α(i) effect of the Genetic.Cluster
# β(j) effect of the year
# (αβ)(i,j) effect of the interaction Genetic.Cluster:year

### GENETIC CLUSTER EFFECT : yes
# model here : loglength ~ Genetic.Cluster
# p-value < 0.05 here so significant effect !
ANCsizeb<-aov(Datafr.gdf2$loglength~Datafr.gdf2$Genetic.Cluster)
summary(ANCsizeb) 
boxplot(Datafr.gdf2$loglength~Datafr.gdf2$Genetic.Cluster, xlab= "Genetic.Cluster", ylab="log2(Fork length in mm)",las=2, col.box= NULL, main = "Genetic.Cluster effect on fork length",col="grey")
AovSum(Datafr.gdf2$loglength~Datafr.gdf2$Genetic.Cluster)

### GENETIC CLUSTER*YEAR EFFECT : yes
# model here : loglength ~ Genetic.Cluster*year
# p-value < 0.05 here so significant !
ANCsize3b<-aov(Datafr.gdf2$loglength~Datafr.gdf2$Genetic.Cluster:Datafr.gdf2$year)
summary(ANCsize3b)
boxplot(Datafr.gdf2$loglength~Datafr.gdf2$Genetic.Cluster:Datafr.gdf2$year, xlab= "Genetic.Cluster and year", ylab="log2(Fork length in mm)", main="Genetic.Cluster x year effect on fork length",col="grey")
AovSum(Datafr.gdf2$loglength~Datafr.gdf2$Genetic.Cluster*Datafr.gdf2$year)

### Normality of the residuals of ANCsize3 : all seems ok ! 
# for shapiro test, if p-value < 0.05, the test it is ok
# the residuals need to have a bell form close to 0 to be normals, it's not a proof but can say if there is a problem
hist(ANCsize3b$residuals)
shapiro.test(ANCsize3b$residuals)

### Post hoc test
# = to see if the groups that are particularly different, works only with factors
getOption("max.print") #we have just 1000, it's not sufficient
options(max.print=99999) #so we change the limit
TukeyHSD(ANCsize3b, group = interaction(Datafr.gdf2$caves, Datafr.gdf2$year))
plot(TukeyHSD(ANCsize3, conf.level=.95), las = 2)
options(max.print=1000) #we put the limit back at normal

### Model ANOVA here that we keep :
# ∀i,j,k with (i,j,k)≠(i’j,’,k’)
# εijk ∼ N (0, σ) and cov(εijk,εi’j’k’)≠0
# Y(i,j,k) = mu + α(i) + β(j) + (αβ)(i,j) + ε(k)
# Y(i,j,k) loglength 
# mu average loglength 
# α(i) effect of the cave
# β(j) effect of the year
# (αβ)(i,j) effect of the interaction caves:year

### ---------------- models selection : allometry and cave on  bodyshape ---------------------

### Here the multiple regression linear model :
# (Regression model : Quantitative sources of variability)
# cov(εi,εk)≠0 and εk ∼ N (0, σ²) normal distribution
# Yk= β0 + β1x1 + .. + εk     
# Yk coords
# x1 loglength
# x2 caves
# x3 year
# procd.lm = Procrustes ANOVA/regression for Procrustes shape variables (with lm = linear model)
# so it's not exactly a multiple regression linear model, it's a mix with ANOVA

### Understanding of the model
# RRPP : Randomization of Residuals in a Permutation Procedure
# SS.type= III : ensure that you dont have a hierarchical order (i.e. the order of factors do not matter)
# Rsq = R square show % of variation of data explained by the factor. 
# find a model that reduce the variation RSS (variation) need to be reduce and be significant

### Link to understand the model
# https://statisticsbyjim.com/regression/interaction-effects/
# https://stats.libretexts.org/Bookshelves/Applied_Statistics/Book%3A_Natural_Resources_Biometrics_(Kiernan)/06%3A_Two-way_Analysis_of_Variance/6.01%3A_Main_Effects_and_Interaction_Effect

### Model selection: (called manual backward selection as you start with the full model and drop term as you go)
# - full model is logFL*cave*year based on my question and hypothesis...
# - (I expect to have an effect of body size, cave and year on body shape and I expect that those effects may vary across years and cave, therefore I have an interaction of the 3 factors).
# - what is results of this model, can I simpify the model based on removel of non significant terms? in your case the next try is to cretae models with all two terms interactions, and drop 1 way interaction that are not significant. Compare models with an f test
# - once you have a model, check assumptions are met, and distrivution of the residuals. 

### -- MODEL 1 ------

# S1_1IO3 mean model 1a with 1 interaction of order 3 (no order 4 or more)
# s1f_3IO1 = model 1f with 3 interactions of order 1 (no order 2 or more)

#### MODEL WITH LOGLENGTH, CAVES, YEAR
s1a_1IO3 <- procD.lm(coords~loglength*caves*year, iter=999,  RRPP=TRUE, SS.type = "III", data=Datafr.gdf2)
summary(s1a_1IO3)
s1b_3IO2 <- procD.lm(coords~loglength+caves+year+loglength*caves+loglength*year+caves*year, iter=999,  RRPP=TRUE, SS.type = "III", data=Datafr.gdf2)
summary(s1b_3IO2)
s1c_2IO2 <- procD.lm(coords~loglength+caves+year+loglength*caves+caves*year, iter=999,  RRPP=TRUE, SS.type = "III", data=Datafr.gdf2)
summary(s1c_2IO2)
s1d_2IO2 <- procD.lm(coords~loglength+caves+year+loglength*year+caves*year, iter=999,  RRPP=TRUE, SS.type = "III", data=Datafr.gdf2)
summary(s1d_2IO2)
s1e_2IO2 <- procD.lm(coords~loglength+caves+year+loglength*year+caves*loglength, iter=999,  RRPP=TRUE, SS.type = "III", data=Datafr.gdf2)
summary(s1e_2IO2)
s1f_3IO1 <- procD.lm(coords~loglength+caves+year, iter=999,  RRPP=TRUE, SS.type = "III", data=Datafr.gdf2)
summary(s1f_3IO1)

### Selection of model :
# if p-value < 0.05, model1 fit less than model2 with anova(model1,model2)
# see if more complex model fit more than simpler model 
anova(s1b_3IO2,s1a_1IO3) #p-value = 0.0011 s1a_1IO3>s1d_2IO2
anova(s1c_2IO2,s1b_3IO2) #p-value = 0.001 s1b_3IO2>s1c_2IO2
anova(s1d_2IO2,s1c_2IO2) #p-value = 0.001 s1c_2IO2>s1d_2IO2
anova(s1e_2IO2,s1d_2IO2) #p-value = 0.001 s1d_2IO2>s1e_2IO2
anova(s1f_3IO1,s1e_2IO2) #p-value = 0.001 s1e_2IO2>s1f_3IO1

### So model s1a_1IO3 chosen :
# Interaction of order 1, order 2 and order 3
# coords~loglength*caves*year
s1a_1IO3 <- procD.lm(coords~loglength*caves*year, iter=999,  RRPP=TRUE, SS.type = "III", data=Datafr.gdf2)
summary(s1a_1IO3)

### Inspect the residual of the model
# not that good
plot(s1a_1IO3)

### -- MODEL 2 ------ 


# S2_1IO3 mean model 1a with 1 interaction of order 3 (no order 4 or more)
# s2f_3IO1 = model 1f with 3 interactions of order 1 (no order 2 or more)

#### MODEL WITH LOGCSIZE, CAVES, YEAR
s2a_1IO3 <- procD.lm(coords~logCsize*caves*year, iter=999,  RRPP=TRUE, SS.type = "III", data=Datafr.gdf2)
summary(s2a_1IO3)
s2b_3IO2 <- procD.lm(coords~logCsize+caves+year+logCsize*caves+logCsize*year+caves*year, iter=999,  RRPP=TRUE, SS.type = "III", data=Datafr.gdf2)
summary(s2b_3IO2)
s2c_2IO2 <- procD.lm(coords~logCsize+caves+year+logCsize*caves+caves*year, iter=999,  RRPP=TRUE, SS.type = "III", data=Datafr.gdf2)
summary(s2c_2IO2)
s2d_2IO2 <- procD.lm(coords~logCsize+caves+year+logCsize*year+caves*year, iter=999,  RRPP=TRUE, SS.type = "III", data=Datafr.gdf2)
summary(s2d_2IO2)
s2e_2IO2 <- procD.lm(coords~logCsize+caves+year+logCsize*year+caves*logCsize, iter=999,  RRPP=TRUE, SS.type = "III", data=Datafr.gdf2)
summary(s2e_2IO2)
s2f_3IO1 <- procD.lm(coords~logCsize+caves+year, iter=999,  RRPP=TRUE, SS.type = "III", data=Datafr.gdf2)
summary(s2f_3IO1)

### Selection of model :
# if p-value < 0.05, model1 fit less than model2 with anova(model1,model2)
# see if more complex model fit more than simpler model 
anova(s2b_3IO2,s2a_1IO3) #p-value = 0.016 s2a_1IO3>s2b_3IO2
anova(s2c_2IO2,s2b_3IO2) #p-value = 0.006 s2b_3IO2>s2c_2IO2
anova(s2d_2IO2,s2c_2IO2) #p-value = 0.001 s2c_2IO2>s2d_2IO2
anova(s2e_2IO2,s2d_2IO2) #p-value = 0.001 s2d_2IO2>s2e_2IO2
anova(s2f_3IO1,s2e_2IO2) #p-value = 0.001 s2e_2IO2>s2f_3IO1

### So model s1a_1IO3 chosen :
# Interaction of order 1, order 2 and order 3
# coords~coords~logCsize*caves*year
s2a_1IO3 <- procD.lm(coords~logCsize*caves*year, iter=999,  RRPP=TRUE, SS.type = "III", data=Datafr.gdf2)
summary(s2a_1IO3)

### Inspect the residual of the model
# not that good
plot(s2a_1IO3)

### -- MODEL 3 : cave x year ----
s3a_1IO2 <- procD.lm(coords~caves*year, iter=999,  RRPP=TRUE, SS.type = "III", data=Datafr.gdf2)
summary(s3a_1IO2)
s3b_2IO1 <- procD.lm(coords~caves+year, iter=999,  RRPP=TRUE, SS.type = "III", data=Datafr.gdf2)
summary(s3b_2IO1)

### Best model : s3a_1IO2
anova(s3b_2IO1,s3a_1IO2) # p-valye = 0.001 so s3a_1IO2>s3b_2IO1

summary(s3a_1IO2)

### ---------------- Plotting regression ------------------------

## YEAR EFFECT
plot(Datafr.gdf2$loglength, Datafr.gdf2$logCsize, type="n", xlab="log2(Fork length in mm)", ylab="log2 Centroid Size", main = "Year effect regression")
points(Datafr.gdf2$loglength, Datafr.gdf2$logCsize, col=Datafr.gdf2$year)
legend ("topright", legend= levels(Datafr.gdf2$year), col=1:length(Datafr.gdf2$year),pch=1, cex=0.8)

abline(lm(Datafr.gdf2$logCsize~Datafr.gdf2$loglength, subset=(subset = Datafr.gdf2$year== "2020")) ,col="black") 
abline(lm(Datafr.gdf2$logCsize~Datafr.gdf2$loglength, subset=(subset = Datafr.gdf2$year== "2021")) ,col="red")

### ---------------- Procrustes variance  -----------------------------------

### Procrustes variance, taking size into account:
# Procrustes variance is the measure of disparity within tested groups
# V= ( Σ (x-μ)² ) / N         μ mean and N number of individual

### LOGCSIZE
# For all caves, procrustes variance: 0.0007073292
Morph.dis<-morphol.disparity(coords~logCsize, groups=NULL, data=Datafr.gdf2, iter=999)
# For each cave :
Morph.disgroups<-morphol.disparity(coords~logCsize*caves, groups=~caves, data=Datafr.gdf2, iter=999)
Morph.disgroups$Procrustes.var

### LOGLENGTH
# For all caves, procrustes variance: 0.0006244824
Morph.dis2<-morphol.disparity(coords~loglength, groups=NULL, data=Datafr.gdf2, iter=999)
# For each cave :
Morph.disgroups2<-morphol.disparity(coords~loglength*caves, groups=~caves, data=Datafr.gdf2, iter=999)
Morph.disgroups2$Procrustes.var


###############################################
### PART I.5 : PCA and DFA study 2020-2021 June #
###############################################

### ---------------- DFA Analysis -----------------------------------

## Understanding of DFA : read article Rainville 2021 "Parallel evolution of morphological traits and body shape in littoral and pelagic brook charr, Salvelinus fontinalis, along a gradient of interspecific competition"

### Need to change the landmarks to a two-dimension form fro DFA
# 1 line = 1 fish, and 22*2 columns for the coordinates (X then Y) of each landmarks
y2<-two.d.array(Datafr.gdf2$coords)
### Here I make the matrix in to a data frame - needed for the discriminant analysis.
# So same as y2
z2<-as.data.frame(y2)

### discriminant analysis
# give prior probabilities of groups (2% C1, 3% C10...)
# give mean of landmarks for each group
# then give coefficients of linear discriminants, from LD1 to LD19
# and proportion of trace for each LD
Discr2 <- lda(formula = Datafr.gdf2$caves~., data = z2)
Discr2

### Calculates propabilities for each axis of the distribution
likur2 <- Discr2$svd^2/sum(Discr2$svd^2)
likur2

# Here I find classification and the axis scores and print them in a file in the directory 
Pred.Discr2<- predict(Discr2) # computes LD scores, coefficients, etc. 
write.csv(Pred.Discr2$class, "D:/stageCESURE-Islande/Statistic_study/R analysis shape - project 1/DFAbodyclass4.csv") 


### -- Matrix of confusion by cave ----

### Matrix of confusion : I will see how good is the DFA classification
# https://www.journaldev.com/46732/confusion-matrix-in-r

### Here a dataframe if I want to visualize the difference for each fish
df_matrix_confusion <- data.frame(expected=ID$Location,predicted=Pred.Discr2$class)

### Here the values
# Creates vectors having data points
expected_value <- factor(ID$Location)
predicted_value <- factor(Pred.Discr2$class)

### Creating confusion matrix
# Accuracy = 0.7 (Accuracy is calculated as the total number of two correct predictions divided by the total number of a dataset) (https://classeval.wordpress.com/introduction/basic-evaluation-measures/)
# Here I can see that the DFA has some difficulties:
# - 12 of reference C12 were predicted as C25
# - 18 of reference C7 were predicted as C25
# - 6 of reference C18 were predicted as C25
# For C12 and C7, it can be explained by the fact that they are in the same genetic clusters as C25
Matrix_confusion_DFA <- caret::confusionMatrix(data=predicted_value, reference = expected_value)
Matrix_confusion_DFA

### -- Matrix of confusion by genetic cluster ----

### Caves by Genetic cluster
# H-C : C7, C12 and C25 
# H-N : C5,C6, C10, C11, C27
# H-S : C1, C2, C26
# V-E : C21, C22, C23, C24
# V-W : C17, C17B, C18, C19, C20

### Convert Pred.Discr2H$class from cave to genetic cluster
# If I use merge, I will change the order so I do a little program
Pred_GCbody <- ID$Genetic.Cluster
for (i in 1:length(Pred.Discr2$class)){
  if ((Pred.Discr2$class[i]=="C7")||(Pred.Discr2$class[i]=="C12")||(Pred.Discr2$class[i]=="C25")){
    Pred_GCbody[i] <- "H-C"
  }
  if ((Pred.Discr2$class[i]=="C5")||(Pred.Discr2$class[i]=="C6")||(Pred.Discr2$class[i]=="C10")||(Pred.Discr2$class[i]=="C11")||(Pred.Discr2$class[i]=="C27")){
    Pred_GCbody[i] <- "H-N"
  }
  if ((Pred.Discr2$class[i]=="C1")||(Pred.Discr2$class[i]=="C2")||(Pred.Discr2$class[i]=="C26")){
    Pred_GCbody[i] <- "H-S"
  }
  if ((Pred.Discr2$class[i]=="C21")||(Pred.Discr2$class[i]=="C22")||(Pred.Discr2$class[i]=="C23")||(Pred.Discr2$class[i]=="C24")){
    Pred_GCbody[i] <- "V-E"
  }
  if ((Pred.Discr2$class[i]=="C17")||(Pred.Discr2$class[i]=="C17B")||(Pred.Discr2$class[i]=="C18")||(Pred.Discr2$class[i]=="C19")||(Pred.Discr2$class[i]=="C20")){
    Pred_GCbody[i] <- "V-W"
  }
}

expected_valueH_GCbody <- factor(ID$Genetic.Cluster)
predicted_valueH_GCbody <- factor(Pred_GCbody)

Matrix_confusion_DFAH_GCbody <- caret::confusionMatrix(data=predicted_valueH_GCbody, reference = expected_valueH_GCbody)
Matrix_confusion_DFAH_GCbody

### ---------------- DFA visualise proportion of each LD ----------

### Here : visualise the propotion of each LD in explaining the variance

### Here a graph to visualise the % of var explained by each LD
# Putting the theme
mytheme <- theme_bw(base_size = 18) +                                           # Set custom theme.
  theme(panel.grid.major = element_blank(),                                     # Remove grey panel grid on graphs 
        panel.grid.minor = element_blank())
# Then writing down the probabilities of each axis of the distribution (cf likur2)
df_Trace2 <- data.frame(LD = c("LD1", "LD2", "LD3", "LD4", "LD5", "LD6", "LD7", "LD8"),        # Make a scree plot. 
                       pv = c(likur2[1]*100, likur2[2]*100, likur2[3]*100, likur2[4]*100, likur2[5]*100, likur2[6]*100, likur2[7]*100, likur2[8]*100))             # % var explained by each LD.(Likur in the script above)
# And then plot it
ggplot(df_Trace2) + geom_bar(aes(x = LD, y = pv, fill = LD), stat = "identity") +              # load into ggplot
  scale_fill_manual(values = c("grey20", "grey20", "grey60", "grey60", "grey60",              # darken first two axes
                               "grey60", "grey60", "grey60")) + mytheme +                    
  xlab("") + ylab("Variation explained (%)") + theme(legend.position = "none",                # remove legend.
                                                     axis.text = element_text(size = 18),     # bigger axis labels.
                                                     axis.text.x = element_text(angle = 90))  # remove legend, adjust titles. 

### ---------------- PCA general Analysis--------------------------

### Creation of the PCA
# gm.prcomp : Function performs principal components analysis (PCA) or phylogenetically-aligned components (PaCA) on Procrustes shape coordinates.
PCA2<-gm.prcomp(Datafr.gdf2$coords)

### The PCA scores/ cumulative proportions :
summary(PCA2)
PCA2$d[1]/sum(PCA2$d) # Eigenvalue for the first principal component
PCA2$d[2]/sum(PCA2$d)
plot(PCA2$d) # Eigenvalues plot

### Plot PCA
# bg is how to color the points
# pch=21 is the type of point, here cercles colored and full
plot(PCA2,main = "PCA body", pch=21,bg=Datafr.gdf2$caves) # here by cave
plot(PCA2,main = "PCA body", pch=21,bg=Datafr.gdf2$year) # here by year

### ---------------- PC1 PC2.. correlation Csize FL?--------------------------
# PC1 is PCA2$x[,1] ect

### -- correlation with logCsize? ----
# corr logCsize/PC1 = 0.373841
cor.test(Datafr.gdf2$logCsize, PCA2$x[,1]) 
plot(Datafr.gdf2$logCsize, PCA2$x[,1], type="n", xlab="log2(Csize)", ylab="PC1", main = "Correlation")
points(Datafr.gdf2$logCsize, PCA2$x[,1],col="red")
abline(lm(PCA2$x[,1]~Datafr.gdf2$logCsize) ,col="black")

# corr logCsize/PC2 = -0.05680916
cor.test(Datafr.gdf2$logCsize, PCA2$x[,2]) 

# corr logCsize/PC3 = 0.2240988
cor.test(Datafr.gdf2$logCsize, PCA2$x[,3]) 
plot(Datafr.gdf2$logCsize, PCA2$x[,3], type="n", xlab="log2(Csize)", ylab="PC3", main = "Correlation")
points(Datafr.gdf2$logCsize, PCA2$x[,3],col="red")
abline(lm(PCA2$x[,3]~Datafr.gdf2$logCsize) ,col="black")

# corr logCsize/PC4 = -0.2531257
cor.test(Datafr.gdf2$logCsize, PCA2$x[,4]) 
plot(Datafr.gdf2$logCsize, PCA2$x[,4], type="n", xlab="log2(Csize)", ylab="PC4", main = "Correlation")
points(Datafr.gdf2$logCsize, PCA2$x[,4],col="red")
abline(lm(PCA2$x[,4]~Datafr.gdf2$logCsize) ,col="black")

### -- correlation with loglength? ----
# corr loglength/PC1 = 0.7743775 
cor.test(Datafr.gdf2$loglength, PCA2$x[,1]) 
plot(Datafr.gdf2$loglength, PCA2$x[,1], type="n", xlab="log2(Fork length in mm)", ylab="PC1", main = "Correlation")
points(Datafr.gdf2$loglength, PCA2$x[,1],col="red")
abline(lm(PCA2$x[,1]~Datafr.gdf2$loglength) ,col="black")

# corr loglength/PC2 = -0.07162977
cor.test(Datafr.gdf2$loglength, PCA2$x[,2]) 

# corr loglength/PC3 = -0.2263121
cor.test(Datafr.gdf2$loglength, PCA2$x[,3])
plot(Datafr.gdf2$loglength, PCA2$x[,3], type="n", xlab="log2(Fork length in mm)", ylab="PC3", main = "Correlation")
points(Datafr.gdf2$loglength, PCA2$x[,3],col="red")
abline(lm(PCA2$x[,3]~Datafr.gdf2$loglength) ,col="black")

# corr loglength/PC4 = -0.1140176
cor.test(Datafr.gdf2$loglength, PCA2$x[,4]) 

### ---------------- Plotting DFA-----------------------------
# Now to the figures for DFA

### Here I calculate the mean of each cave and create new variables
# This is the mean shape in each cave (mean coordinates along dfa1 and dfa2)
Df1b<-tapply(Pred.Discr2$x[,1], Datafr.gdf2$caves, mean )
Df2b<-tapply(Pred.Discr2$x[,2], Datafr.gdf2$caves, mean )

### R does not have an easy way to calculate standard error - lets teach it
st.err <- function(x) {  +     sd(x)/sqrt(length(x)) }
# Now we can calculate standard errors
Df1SE2<- tapply(Pred.Discr2$x[,1], Datafr.gdf2$caves, st.err)
Df2SE2<- tapply(Pred.Discr2$x[,2], Datafr.gdf2$caves, st.err)


DF2<-data.frame(Df1=Df1b,Df2=Df2b, SE1=Df1SE2, SE2= Df2SE2, Labels=Caves.Right.Order.Genetic.Cluster, gene=Genetic.Cluster.Linked)
DF2$gene<-as.factor(DF2$gene)

### Here the graph
ggplot(DF2, aes(x=Df1,y=Df2, color=gene)) + geom_point(aes(Df1, Df2)) +  geom_errorbar(aes(ymin=Df2-SE2, ymax=Df2+SE2)) +geom_errorbarh(aes(xmin=Df1-SE1, xmax=Df1+SE1))+ geom_text(aes(label=Labels), size=3, hjust=-1) + xlab("Discriminant Axis 1 (21.77%)") + ylab("Discriminant Axis 2(20.19%)")+ labs(title="Discriminant Function Analysis")+ theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) +labs(color="Genetic Clusters") +theme(plot.title = element_text(hjust = 0.5))


# Now we need to create the deformation grids - package geomorph - NOTE that we set the extreme of the axis here. 
# We choose the number depending of the DFA graph (we take the bottom right point the most extreme)
#If we want magnification, we have to increase these numbers
DF1pred2<-shape.predictor(Datafr.gdf2$coords,Pred.Discr2$x[,1], Intercept=FALSE, pred1=-1, pred2=2 )
DF2pred2<-shape.predictor(Datafr.gdf2$coords,Pred.Discr2$x[,2], Intercept=FALSE, pred1=-1, pred2=2 )
Mean_morph2<-mshape(Datafr.gdf2$coords) # Here we find the mean morphology or assign mean shape for use with plotRefToTarget below
# Now we compare shape at extreme of axis to mean morphology
# Mag = magnification = the ability of a microscope to produce an image of an object at a scale larger (or even smaller) than its actual size
plotRefToTarget(Mean_morph2, DF1pred2$pred1, mag= 3) #min DF1 , can change magnification 
plotRefToTarget(Mean_morph2, DF1pred2$pred2, mag= 3) #max DF1
plotRefToTarget(Mean_morph2, DF2pred2$pred1, mag= 3) #min DF2
plotRefToTarget(Mean_morph2, DF2pred2$pred2, mag= 3) #max DF2
# similar but we see shape change with vector for each LM, can help to describe what the changes are along the PCA axes: 
plotRefToTarget(Mean_morph2, DF1pred2$pred1, method="vector", mag=3) #min DF1 , can change magnification 
plotRefToTarget(Mean_morph2, DF1pred2$pred2, method="vector",mag=3) #max DF1
plotRefToTarget(Mean_morph2, DF2pred2$pred1, method="vector",mag=3) #min DF2
plotRefToTarget(Mean_morph2, DF2pred2$pred2, method="vector",mag=3) #max DF2

### -- correlation DFA with logCsize? loglength? ----
# with DF1?
cor.test(Datafr.gdf2$logCsize, Pred.Discr2$x[,1]) #0.0134011
cor.test(Datafr.gdf2$loglength, Pred.Discr2$x[,1]) #-0.13283

# with DF2?
cor.test(Datafr.gdf2$logCsize, Pred.Discr2$x[,2]) #0.1713626
cor.test(Datafr.gdf2$loglength, Pred.Discr2$x[,2]) #0.2173798 

### ---------------- Graphing PCA, mean of each cave (PC1 PC2)------------------

#Now to the figures for PCA - similar as DFA above 

### Average shape, we visualize average shape in each cave and for each year along the two PCA axes that explained most variation.
PC1b<-tapply(PCA2$x[,1], Datafr.gdf2$caves, mean )
PC2b<-tapply(PCA2$x[,2], Datafr.gdf2$caves, mean )
PC1SE2<-tapply(PCA2$x[,1], Datafr.gdf2$caves, st.err )
PC2SE2<-tapply(PCA2$x[,2], Datafr.gdf2$caves, st.err )

PCb<-data.frame(PC1=PC1b,PC2=PC2b, SE1=PC1SE2, SE2= PC2SE2, Labels=Caves.Right.Order.Genetic.Cluster, gene=Genetic.Cluster.Linked)
PCb$gene<-as.factor(PCb$gene)

### Here the graph
ggplot(PCb, aes(x=PC1,y=PC2, color=gene)) + geom_point(aes(PC1, PC2)) +  geom_errorbar(aes(ymin=PC2-SE2, ymax=PC2+SE2)) +geom_errorbarh(aes(xmin=PC1-SE1, xmax=PC1+SE1))+ geom_text(aes(label=Labels), size=4, hjust=-1) + xlab("PCA 1 (24.89%)") + ylab("PCA 2 (12.62%)")+ labs(title="PCA Analysis")+ theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) +labs(color="Genetic Clusters") +theme(plot.title = element_text(hjust = 0.5))

# Now we need to create the deformation grids - package geomorph - ATH:  we set the extreme of the axes here with pred1 and pred2 for each axis, look at the min and max values of PCA plot 
#If we want magnification, we have to increase these numbers, 
PCA1pred2<-shape.predictor(Datafr.gdf2$coords,PCA2$x[,1],Intercept=FALSE, pred1=-0.01, pred2=0.01)
PCA2pred2<-shape.predictor(Datafr.gdf2$coords,PCA2$x[,2],Intercept=FALSE, pred1=-0.01, pred2=0.01)
Mean_morph2<-mshape(Datafr.gdf2$coords) # Here we find the mean morphology or assign mean shape for use with plotRefToTarget below
# Now we compare shape at extreme of axis to mean morpholog using deformation grids
plotRefToTarget(Mean_morph2, PCA1pred2$pred1, mag=3) #min PC1 , can change magnification
plotRefToTarget(Mean_morph2, PCA1pred2$pred2, mag=3) #max PC1
plotRefToTarget(Mean_morph2, PCA2pred2$pred1, mag=3) #min PC2
plotRefToTarget(Mean_morph2, PCA2pred2$pred2, mag=3) #max PC2
# similar but we see shape change with vector for each LM, can help to describe what the changes are along the PCA axes: 
plotRefToTarget(Mean_morph2, PCA1pred2$pred1, method="vector", mag=3) #min PC1 , can change magnification 
plotRefToTarget(Mean_morph2, PCA1pred2$pred2, method="vector",mag=3) #max PC1
plotRefToTarget(Mean_morph2, PCA2pred2$pred1, method="vector",mag=3) #min PC2
plotRefToTarget(Mean_morph2, PCA2pred2$pred2, method="vector",mag=3) #max PC2

### -- correlation PCA with logCsize? loglength? ----
# with PC1?
cor.test(Datafr.gdf2$logCsize, PCA2$x[,3]) #0.373841
cor.test(Datafr.gdf2$loglength, PCA2$x[,3]) #0.7743649

# with PC2?
cor.test(Datafr.gdf2$logCsize, PCA2$x[,2]) #-0.05680916
cor.test(Datafr.gdf2$loglength, PCA2$x[,2]) #-0.07113411

### ---------------- Graphing PCA, mean of each cave (PC3 PC4 NEW)------------------

#Now to the figures for PCA - similar as DFA above 

### Average shape, we visualize average shape in each cave and for each year along the two PCA axes that explained most variation.
PC4b<-tapply(PCA2$x[,4], Datafr.gdf2$caves, mean )
PC3b<-tapply(PCA2$x[,3], Datafr.gdf2$caves, mean )
PC4SE2<-tapply(PCA2$x[,4], Datafr.gdf2$caves, st.err )
PC3SE2<-tapply(PCA2$x[,3], Datafr.gdf2$caves, st.err )

PCb3<-data.frame(PC4=PC4b,PC3=PC3b, SE4=PC4SE2, SE3= PC3SE2, Labels=Caves.Right.Order.Genetic.Cluster, gene=Genetic.Cluster.Linked)
PCb3$gene<-as.factor(PC3b$gene)

### Here the graph
ggplot(PCb3, aes(x=PC4,y=PC3, color=gene)) + geom_point(aes(PC4, PC3)) +  geom_errorbar(aes(ymin=PC3-SE3, ymax=PC3+SE3)) +geom_errorbarh(aes(xmin=PC4-SE4, xmax=PC4+SE4))+ geom_text(aes(label=Labels), size=4, hjust=-1) + xlab("PCA 4 (8.12%)") + ylab("PCA 3 (9.40%)")+ labs(title="PCA Analysis")+ theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) +labs(color="Genetic Clusters") +theme(plot.title = element_text(hjust = 0.5))

# Now we need to create the deformation grids - package geomorph - ATH:  we set the extreme of the axes here with pred1 and pred2 for each axis, look at the min and max values of PCA plot 
#If we want magnification, we have to increase these numbers, 
PCA1pred2<-shape.predictor(Datafr.gdf2$coords,PCA2$x[,4],Intercept=FALSE, pred1=-0.005, pred2=0.005)
PCA2pred2<-shape.predictor(Datafr.gdf2$coords,PCA2$x[,3],Intercept=FALSE, pred1=-0.005, pred2=0.005)
Mean_morph2<-mshape(Datafr.gdf2$coords) # Here we find the mean morphology or assign mean shape for use with plotRefToTarget below
# Now we compare shape at extreme of axis to mean morpholog using deformation grids
plotRefToTarget(Mean_morph2, PCA1pred2$pred1, mag=3) #min PC4 , can change magnification
plotRefToTarget(Mean_morph2, PCA1pred2$pred2, mag=3) #max PC4
plotRefToTarget(Mean_morph2, PCA2pred2$pred1, mag=3) #min PC3
plotRefToTarget(Mean_morph2, PCA2pred2$pred2, mag=3) #max PC3
# similar but we see shape change with vector for each LM, can help to describe what the changes are along the PCA axes: 
plotRefToTarget(Mean_morph2, PCA1pred2$pred1, method="vector", mag=3) #min PC 4, can change magnification
plotRefToTarget(Mean_morph2, PCA1pred2$pred2, method="vector",mag=3) #max PC4
plotRefToTarget(Mean_morph2, PCA2pred2$pred1, method="vector",mag=3) #min PC3
plotRefToTarget(Mean_morph2, PCA2pred2$pred2, method="vector",mag=3) #max PC3

##########################################################
### PART I.6 : PCA and DFA study 2020 June then 2021 June #
##########################################################

### maybe good to create a plot for 2020 and a plot for 2021 at first to visualise

### ---------------- 2020 and 2021 Introduction of data ----------

### We separe the datas in 2 after procrustes surperposition (aligned and scaled coordinates)
# so after having cave.gpa2<-gpagen(Morph.Data,curves=sliders2) and after Datafr.gdf2 (more easy that way)
### 1-415 : year 2020, 416-825 (410 fishes) : year 2021
Datafr.gdf2020 <- geomorph.data.frame(coords=cave.gpa2$coords[1:22,1:2,1:415],Csize=cave.gpa2$Csize[1:415],caves=ID$Location[1:415], year=ID$year[1:415], length=ID$Length..mm.[1:415],cavesyear=interaction(ID$Location[1:415],ID$year[1:415]),logCsize=log2(cave.gpa2$Csize[1:415]),loglength=log2(ID$Length..mm.[1:415]))
Datafr.gdf2021 <- geomorph.data.frame(coords=cave.gpa2$coords[1:22,1:2,416:825],Csize=cave.gpa2$Csize[416:825],caves=ID$Location[416:825], year=ID$year[416:825], length=ID$Length..mm.[416:825],cavesyear=interaction(ID$Location[416:825],ID$year[416:825]),logCsize=log2(cave.gpa2$Csize[416:825]),loglength=log2(ID$Length..mm.[416:825]))

### ---------------- 2020 and 2021 DFA Analysis -----------------------------------

## Understanding of DFA : read article Rainville 2021 "Parallel evolution of morphological traits and body shape in littoral and pelagic brook charr, Salvelinus fontinalis, along a gradient of interspecific competition"

### -- YEAR 2020 ----

### Need to change the landmarks to a two-dimension form fro DFA
# 1 line = 1 fish, and 22*2 columns for the coordinates (X then Y) of each landmarks
y2020<-two.d.array(Datafr.gdf2020$coords)
### Here I make the matrix in to a data frame - needed for the discriminant analysis. Discriminant analysis needs the package MASS
# So same as y2
z2020<-as.data.frame(y2020)

### discriminant analysis
# give prior probabilities of groups (2% C1, 3% C10...)
# give mean of landmarks for each group
# then give coefficients of linear discriminants, from LD1 to LD19
# and proportion of trace for each LD
Discr2020 <- lda(formula = Datafr.gdf2020$caves~., data = z2020)
Discr2020

### Calculates propabilities for each axis of the distribution
likur2020 <- Discr2020$svd^2/sum(Discr2020$svd^2)
likur2020

# Here I find classification and the axis scores and print them in a file in the directory 
Pred.Discr2020<- predict(Discr2020) # computes LD scores, coefficients, etc. 
write.csv(Pred.Discr2020$class, "D:/stageCESURE-Islande/Statistic_study/R analysis shape - project 1/DFAbodyclass2020.csv") 

### -- Matrix of confusion 2020 ----

### Matrix of confusion : I will see how good is the DFA classification
# https://www.journaldev.com/46732/confusion-matrix-in-r

### Here a dataframe if I want to visualize the difference for each fish
df_matrix_confusion2020 <- data.frame(expected=ID2020$Location,predicted=Pred.Discr2020$class)

### Here the values
# Creates vectors having data points
expected_value2020 <- factor(ID2020$Location)
predicted_value2020 <- factor(Pred.Discr2020$class)

### Creating confusion matrix
# Accuracy = 0.8 (Accuracy is calculated as the total number of two correct predictions divided by the total number of a dataset) (https://classeval.wordpress.com/introduction/basic-evaluation-measures/)
# Here I can see that the DFA has some difficulties:
# - 5 of reference C12 were predicted as C25
# For C12, it can be explained by the fact that it is in the same genetic clusters as C25
# For accuracy by cave, it's just below, in statistics by class
Matrix_confusion_DFA2020 <- caret::confusionMatrix(data=predicted_value2020, reference = expected_value2020)
Matrix_confusion_DFA2020

### -- Matrix of confusion 2020 by genetic cluster ----

### Caves by Genetic cluster
# H-C : C7, C12 and C25 
# H-N : C5,C6, C10, C11, C27
# H-S : C1, C2, C26
# V-E : C21, C22, C23, C24
# V-W : C17, C17B, C18, C19, C20

### Convert Pred.Discr2H$class from cave to genetic cluster
# If I use merge, I will change the order so I do a little program
Pred_GCbody2020 <- ID$Genetic.Cluster[1:415]
for (i in 1:length(Pred.Discr2020$class)){
  if ((Pred.Discr2020$class[i]=="C7")||(Pred.Discr2020$class[i]=="C12")||(Pred.Discr2020$class[i]=="C25")){
    Pred_GCbody2020[i] <- "H-C"
  }
  if ((Pred.Discr2020$class[i]=="C5")||(Pred.Discr2020$class[i]=="C6")||(Pred.Discr2020$class[i]=="C10")||(Pred.Discr2020$class[i]=="C11")||(Pred.Discr2020$class[i]=="C27")){
    Pred_GCbody2020[i] <- "H-N"
  }
  if ((Pred.Discr2020$class[i]=="C1")||(Pred.Discr2020$class[i]=="C2")||(Pred.Discr2020$class[i]=="C26")){
    Pred_GCbody2020[i] <- "H-S"
  }
  if ((Pred.Discr2020$class[i]=="C21")||(Pred.Discr2020$class[i]=="C22")||(Pred.Discr2020$class[i]=="C23")||(Pred.Discr2020$class[i]=="C24")){
    Pred_GCbody2020[i] <- "V-E"
  }
  if ((Pred.Discr2020$class[i]=="C17")||(Pred.Discr2020$class[i]=="C17B")||(Pred.Discr2020$class[i]=="C18")||(Pred.Discr2020$class[i]=="C19")||(Pred.Discr2020$class[i]=="C20")){
    Pred_GCbody2020[i] <- "V-W"
  }
}

expected_valueH_GCbody2020 <- factor(ID$Genetic.Cluster[1:415])
predicted_valueH_GCbody2020 <- factor(Pred_GCbody2020)

Matrix_confusion_DFAH_GCbody2020 <- caret::confusionMatrix(data=predicted_valueH_GCbody2020, reference = expected_valueH_GCbody2020)
Matrix_confusion_DFAH_GCbody2020

### -- YEAR 2021 ----

### Need to change the landmarks to a two-dimension form fro DFA
# 1 line = 1 fish, and 22*2 columns for the coordinates (X then Y) of each landmarks
y2021<-two.d.array(Datafr.gdf2021$coords)
### Here I make the matrix in to a data frame - needed for the discriminant analysis. Discriminant analysis needs the package MASS
# So same as y2
z2021<-as.data.frame(y2021)

### discriminant analysis
# give prior probabilities of groups (2% C1, 3% C10...)
# give mean of landmarks for each group
# then give coefficients of linear discriminants, from LD1 to LD19
# and proportion of trace for each LD
Discr2021 <- lda(formula = Datafr.gdf2021$caves~., data = z2021)
Discr2021

### Calculates propabilities for each axis of the distribution
likur2021 <- Discr2021$svd^2/sum(Discr2021$svd^2)
likur2021

# Here I find classification and the axis scores and print them in a file in the directory 
Pred.Discr2021<- predict(Discr2021) # computes LD scores, coefficients, etc. 
write.csv(Pred.Discr2021$class, "D:/stageCESURE-Islande/Statistic_study/R analysis shape - project 1/DFAbodyclass2021.csv") 

### -- Matrix of confusion 2021 ----

### Matrix of confusion : I will see how good is the DFA classification
# https://www.journaldev.com/46732/confusion-matrix-in-r

### Here a dataframe if I want to visualize the difference for each fish
df_matrix_confusion2021 <- data.frame(expected=ID2021$Location,predicted=Pred.Discr2021$class)

### Here the values
# Creates vectors having data points
expected_value2021 <- factor(ID2021$Location)
predicted_value2021 <- factor(Pred.Discr2021$class)

### Creating confusion matrix
# Accuracy = 0.8 (Accuracy is calculated as the total number of two correct predictions divided by the total number of a dataset) (https://classeval.wordpress.com/introduction/basic-evaluation-measures/)
# Here I can see that the DFA has some difficulties:
# - 5 of reference C12 were predicted as C25
# For C12, it can be explained by the fact that it is in the same genetic clusters as C25
Matrix_confusion_DFA2021 <- caret::confusionMatrix(data=predicted_value2021, reference = expected_value2021)
Matrix_confusion_DFA2021

### -- Matrix of confusion 2021 by genetic cluster ----

### Caves by Genetic cluster
# H-C : C7, C12 and C25 
# H-N : C5,C6, C10, C11, C27
# H-S : C1, C2, C26
# V-E : C21, C22, C23, C24
# V-W : C17, C17B, C18, C19, C20

### Convert Pred.Discr2H$class from cave to genetic cluster
# If I use merge, I will change the order so I do a little program
Pred_GCbody2021 <- ID$Genetic.Cluster[416:825]
for (i in 1:length(Pred.Discr2021$class)){
  if ((Pred.Discr2021$class[i]=="C7")||(Pred.Discr2021$class[i]=="C12")||(Pred.Discr2021$class[i]=="C25")){
    Pred_GCbody2021[i] <- "H-C"
  }
  if ((Pred.Discr2021$class[i]=="C5")||(Pred.Discr2021$class[i]=="C6")||(Pred.Discr2021$class[i]=="C10")||(Pred.Discr2021$class[i]=="C11")||(Pred.Discr2021$class[i]=="C27")){
    Pred_GCbody2021[i] <- "H-N"
  }
  if ((Pred.Discr2021$class[i]=="C1")||(Pred.Discr2021$class[i]=="C2")||(Pred.Discr2021$class[i]=="C26")){
    Pred_GCbody2021[i] <- "H-S"
  }
  if ((Pred.Discr2021$class[i]=="C21")||(Pred.Discr2021$class[i]=="C22")||(Pred.Discr2021$class[i]=="C23")||(Pred.Discr2021$class[i]=="C24")){
    Pred_GCbody2021[i] <- "V-E"
  }
  if ((Pred.Discr2021$class[i]=="C17")||(Pred.Discr2021$class[i]=="C17B")||(Pred.Discr2021$class[i]=="C18")||(Pred.Discr2021$class[i]=="C19")||(Pred.Discr2021$class[i]=="C20")){
    Pred_GCbody2021[i] <- "V-W"
  }
}

expected_value_GCbody2021 <- factor(ID$Genetic.Cluster[416:825])
predicted_value_GCbody2021 <- factor(Pred_GCbody2021)

Matrix_confusion_DFA_GCbody2021 <- caret::confusionMatrix(data=predicted_value_GCbody2021, reference = expected_value_GCbody2021)
Matrix_confusion_DFA_GCbody2021
### ---------------- 2020 and 2021 PCA general Analysis -----------

### Here we do like before, just splitting in 2 years so not a lot of comment really needed

### -- YEAR 2020 ----

### Creation of the PCA
PCA2020<-gm.prcomp(Datafr.gdf2020$coords)
### The PCA scores/ cumulative proportions :
summary(PCA2020)
PCA2020$d[1]/sum(PCA2020$d) # Eigenvalue for the first principal component
PCA2020$d[2]/sum(PCA2020$d)
plot(PCA2020$d) # Eigenvalues plot
### Plot PCA
plot(PCA2020,main = "2020 PCA body", pch=21,bg=Datafr.gdf2020$caves) # here by cave

### -- YEAR 2021 ----

### Creation of the PCA
PCA2021<-gm.prcomp(Datafr.gdf2021$coords)
### The PCA scores/ cumulative proportions :
summary(PCA2021)
PCA2021$d[1]/sum(PCA2021$d) # Eigenvalue for the first principal component
PCA2021$d[2]/sum(PCA2021$d)
plot(PCA2021$d) # Eigenvalues plot
### Plot PCA
plot(PCA2021,main = "2021 PCA body", pch=21,bg=Datafr.gdf2021$caves) # here by cave

### ---------------- 2020 and 2021 Plotting DFA (NEW)-----------------------------

### -- YEAR 2020 ----

# Now to the figures for DFA

### Here I calculate the mean of each cave and create new variables
# This is the mean shape in each cave (mean coordinates along dfa1 and dfa2)
# We take the 2020 part in the general PCA to be able to compare the 2 years
# 1-415 : year 2020, 416-825 (410 fishes) : year 2021
Df1b2020<-tapply(Pred.Discr2$x[,1][1:415], Datafr.gdf2020$caves, mean )
Df2b2020<-tapply(Pred.Discr2$x[,2][1:415], Datafr.gdf2020$caves, mean )

### R does not have an easy way to calculate standard error - lets teach it
st.err <- function(x) {  +     sd(x)/sqrt(length(x)) }
# Now we can calculate standard errors
Df1SE2020<- tapply(Pred.Discr2$x[,1][1:415], Datafr.gdf2020$caves, st.err)
Df2SE2020<- tapply(Pred.Discr2$x[,2][1:415], Datafr.gdf2020$caves, st.err)

DF2020<-data.frame(Df1=Df1b2020,Df2=Df2b2020, SE1=Df1SE2020, SE2= Df2SE2020, Labels=Caves.Right.Order.Genetic.Cluster, gene=Genetic.Cluster.Linked)
DF2020$gene<-as.factor(DF2020$gene)

#OK lets try to draw something. Here we use ggplot2 which is powerfull, but hard to operate with other figure systems. 
#Thus in the end we combine everything using Powerpoint (sigh :( )

### Here the graph
ggplot(DF2020, aes(x=Df1,y=Df2, color=gene)) + geom_point(aes(Df1, Df2)) +  geom_errorbar(aes(ymin=Df2-SE2, ymax=Df2+SE2)) +geom_errorbarh(aes(xmin=Df1-SE1, xmax=Df1+SE1))+ geom_text(aes(label=Labels), size=3, hjust=-1) + xlab("Discriminant Axis 1 (21.77%)") + ylab("Discriminant Axis 2 (20.19%)")+ labs(title="Discriminant Function Analysis 2020")+ theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) +labs(color="Genetic Clusters") +theme(plot.title = element_text(hjust = 0.5))

# Now we need to create the deformation grids - package geomorph - NOTE that we set the extreme of the axis here. 
#If we want magnification, we have to increase these numbers
DF1pred2020<-shape.predictor(Datafr.gdf2020$coords,Pred.Discr2$x[,1][1:415], Intercept=FALSE, pred1=-1, pred2=2 )
DF2pred2020<-shape.predictor(Datafr.gdf2020$coords,Pred.Discr2$x[,2][1:415], Intercept=FALSE, pred1=-1, pred2=2 )
Mean_morph2020<-mshape(Datafr.gdf2020$coords) # Here we find the mean morphology or assign mean shape for use with plotRefToTarget below
# Now we compare shape at extreme of axis to mean morphology
plotRefToTarget(Mean_morph2020, DF1pred2020$pred1, mag= 3) #min DF1 , can change magnification 
plotRefToTarget(Mean_morph2020, DF1pred2020$pred2, mag= 3) #max DF1
plotRefToTarget(Mean_morph2020, DF2pred2020$pred1, mag= 3) #min DF2
plotRefToTarget(Mean_morph2020, DF2pred2020$pred2, mag= 3) #max DF2
# Same but with vector :
plotRefToTarget(Mean_morph2020, DF1pred2020$pred1, method="vector", mag= 3) #min DF1 , can change magnification 
plotRefToTarget(Mean_morph2020, DF1pred2020$pred2, method="vector", mag= 3) #max DF1
plotRefToTarget(Mean_morph2020, DF2pred2020$pred1, method="vector", mag= 3) #min DF2
plotRefToTarget(Mean_morph2020, DF2pred2020$pred2, method="vector", mag= 3) #max DF2

### -- YEAR 2021 ----

# Now to the figures for DFA

### Here I calculate the mean of each cave and create new variables
# This is the mean shape in each cave (mean coordinates along dfa1 and dfa2)
# We take the 2020 part in the general PCA to be able to compare the 2 years
# 1-415 : year 2020, 416-825 (410 fishes) : year 2021
Df1b2021<-tapply(Pred.Discr2$x[,1][416:825], Datafr.gdf2021$caves, mean )
Df2b2021<-tapply(Pred.Discr2$x[,2][416:825], Datafr.gdf2021$caves, mean )

### R does not have an easy way to calculate standard error - lets teach it
st.err <- function(x) {  +     sd(x)/sqrt(length(x)) }
# Now we can calculate standard errors
Df1SE2021<- tapply(Pred.Discr2$x[,1][416:825], Datafr.gdf2021$caves, st.err)
Df2SE2021<- tapply(Pred.Discr2$x[,2][416:825], Datafr.gdf2021$caves, st.err)

DF2021<-data.frame(Df1=Df1b2021,Df2=Df2b2021, SE1=Df1SE2021, SE2= Df2SE2021, Labels=Caves.Right.Order.Genetic.Cluster, gene=Genetic.Cluster.Linked)
DF2021$gene<-as.factor(DF2021$gene)

ggplot(DF2021, aes(x=Df1,y=Df2, color=gene)) + geom_point(aes(Df1, Df2)) +  geom_errorbar(aes(ymin=Df2-SE2, ymax=Df2+SE2)) +geom_errorbarh(aes(xmin=Df1-SE1, xmax=Df1+SE1))+ geom_text(aes(label=Labels), size=3, hjust=-1) + xlab("Discriminant Axis 1 (21.77%)") + ylab("Discriminant Axis 2 (20.19%)")+ labs(title="Discriminant Function Analysis 2021")+ theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) +labs(color="Genetic Clusters") +theme(plot.title = element_text(hjust = 0.5))

# Now we need to create the deformation grids - package geomorph - NOTE that we set the extreme of the axis here. 
#If we want magnification, we have to increase these numbers
DF1pred2021<-shape.predictor(Datafr.gdf2021$coords,Pred.Discr2$x[,1][416:825], Intercept=FALSE, pred1=-1.5, pred2=2 )
DF2pred2021<-shape.predictor(Datafr.gdf2021$coords,Pred.Discr2$x[,2][416:825], Intercept=FALSE, pred1=-1.5, pred2=2 )
Mean_morph2021<-mshape(Datafr.gdf2021$coords) # Here we find the mean morphology or assign mean shape for use with plotRefToTarget below
# Now we compare shape at extreme of axis to mean morphology
plotRefToTarget(Mean_morph2021, DF1pred2021$pred1, mag= 3) #min DF1 , can change magnification 
plotRefToTarget(Mean_morph2021, DF1pred2021$pred2, mag= 3) #max DF1
plotRefToTarget(Mean_morph2021, DF2pred2021$pred1, mag= 3) #min DF2
plotRefToTarget(Mean_morph2021, DF2pred2021$pred2, mag= 3) #max DF2
# same but with vector
plotRefToTarget(Mean_morph2021, DF1pred2021$pred1, method="vector", mag= 3) #min DF1 , can change magnification 
plotRefToTarget(Mean_morph2021, DF1pred2021$pred2, method="vector", mag= 3) #max DF1
plotRefToTarget(Mean_morph2021, DF2pred2021$pred1, method="vector", mag= 3) #min DF2
plotRefToTarget(Mean_morph2021, DF2pred2021$pred2, method="vector", mag= 3) #max DF2

### -- correlation DFA with logCsize? loglength? ----
# with DF1?
cor.test(Datafr.gdf2$logCsize[1:415], Pred.Discr2$x[,1][1:415]) #-0.04536536
cor.test(Datafr.gdf2$logCsize[416:825], Pred.Discr2$x[,1][416:825]) #-0.1204175

cor.test(Datafr.gdf2$loglength[1:415], Pred.Discr2$x[,1][1:415]) #-0.07672108
cor.test(Datafr.gdf2$loglength[416:825], Pred.Discr2$x[,1][416:825]) #-0.1879902

# with DF2?
cor.test(Datafr.gdf2$logCsize[1:415], Pred.Discr2$x[,2][1:415]) #0.1815114
cor.test(Datafr.gdf2$logCsize[416:825], Pred.Discr2$x[,2][416:825]) #0.2818935

cor.test(Datafr.gdf2$loglength[1:415], Pred.Discr2$x[,2][1:415]) #0.05135513
cor.test(Datafr.gdf2$loglength[416:825], Pred.Discr2$x[,2][416:825]) #0.3930611

### -- putting YEAR 2020 2021 in 1 graph ----

### I will put a "+" for the year 2021, to differenciate them from 2020
Caves.Right.Order.Genetic.Cluster.2021 <-c("1+","10+","11+","12+","17+","17B+","18+",
                                           "19+","2+","20+","21+","22+","23+","24+",
                                           "25+","26+","27+","5+","6+","7+")
DF2021_new<-data.frame(Df1=Df1b2021,Df2=Df2b2021, SE1=Df1SE2021, SE2= Df2SE2021, Labels=Caves.Right.Order.Genetic.Cluster.2021, gene=Genetic.Cluster.Linked)
DF2021_new$gene<-as.factor(DF2021_new$gene)

### I melt the years 2020 and 2021
DF2020_2021 <- rbind(DF2020,DF2021_new)

ggplot(DF2020_2021, aes(x=Df1,y=Df2, color=gene)) + geom_point(aes(Df1, Df2)) +  geom_errorbar(aes(ymin=Df2-SE2, ymax=Df2+SE2)) +geom_errorbarh(aes(xmin=Df1-SE1, xmax=Df1+SE1))+ geom_text(aes(label=Labels), size=4, hjust=-1) + xlab("Discriminant Axis 1 (21.77%)") + ylab("Discriminant Axis 2 (20.19%)")+ labs(title="Discriminant Function Analysis 2020-2021")+ theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) +labs(color="Genetic Clusters") +theme(plot.title = element_text(hjust = 0.5))

### ---------------- 2020 and 2021 Graphing PCA, mean of each cave (PC1 PC2)------------------

### Here we do like before, just splitting in 2 years so no a lot of comment really needed

### -- YEAR 2020 ----

### Average shape, we visualize average shape in each cave and for each year along the two PCA axes that explained most variation.
# We take the 2020 part in the general PCA to be able to compare the 2 years
### 1-415 : year 2020, 416-825 (410 fishes) : year 2021
PC1b2020<-tapply(PCA2$x[,1][1:415], Datafr.gdf2020$caves, mean )
PC2b2020<-tapply(PCA2$x[,2][1:415], Datafr.gdf2020$caves, mean )
PC1SE2020<-tapply(PCA2$x[,1][1:415], Datafr.gdf2020$caves, st.err )
PC2SE2020<-tapply(PCA2$x[,2][1:415], Datafr.gdf2020$caves, st.err )

PCb2020<-data.frame(PC1=PC1b2020,PC2=PC2b2020, SE1=PC1SE2020, SE2= PC2SE2020, Labels=Caves.Right.Order.Genetic.Cluster, gene=Genetic.Cluster.Linked)
PCb2020$gene<-as.factor(PCb2020$gene)
ggplot(PCb2020, aes(x=PC1,y=PC2, color=gene)) + geom_point(aes(PC1, PC2)) +  geom_errorbar(aes(ymin=PC2-SE2, ymax=PC2+SE2)) +geom_errorbarh(aes(xmin=PC1-SE1, xmax=PC1+SE1))+ geom_text(aes(label=Labels), size=3, hjust=-1) + xlab("PCA 1 (24.89%)") + ylab("PCA 2 (12.62%)")+ labs(title="2020 PCA Analysis")+ theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) +labs(color="Genetic Clusters") +theme(plot.title = element_text(hjust = 0.5))

# Now we need to create the deformation grids - package geomorph - ATH:  we set the extreme of the axes here with pred1 and pred2 for each axis, look at the min and max values of PCA plot 
#If we want magnification, we have to increase these numbers, 
PCA1pred2020<-shape.predictor(Datafr.gdf2020$coords,PCA2$x[,1][1:415],Intercept=FALSE, pred1=-0.015, pred2=0.015)
PCA2pred2020<-shape.predictor(Datafr.gdf2020$coords,PCA2$x[,2][1:415],Intercept=FALSE, pred1=-0.015, pred2=0.015)
Mean_morph2020<-mshape(Datafr.gdf2020$coords) # Here we find the mean morphology or assign mean shape for use with plotRefToTarget below
# Now we compare shape at extreme of axis to mean morpholog using deformation grids
plotRefToTarget(Mean_morph2020, PCA1pred2020$pred1, mag=3) #min PC1 , can change magnification 
plotRefToTarget(Mean_morph2020, PCA1pred2020$pred2, mag=3) #max PC1
plotRefToTarget(Mean_morph2020, PCA2pred2020$pred1, mag=3) #min PC2
plotRefToTarget(Mean_morph2020, PCA2pred2020$pred2, mag=3) #max PC2
# similar but we see shape change with vector for each LM, can help to describe what the changes are along the PCA axes: 
plotRefToTarget(Mean_morph2020, PCA1pred2020$pred1, method="vector",mag=3) #min PC1 , can change magnification 
plotRefToTarget(Mean_morph2020, PCA1pred2020$pred2, method="vector",mag=3) #max PC1
plotRefToTarget(Mean_morph2020, PCA2pred2020$pred1, method="vector",mag=3) #min PC2
plotRefToTarget(Mean_morph2020, PCA2pred2020$pred2, method="vector",mag=3) #max PC2

### -- YEAR 2021 ----

### Average shape, we visualize average shape in each cave and for each year along the two PCA axes that explained most variation.
# We take the 2021 part in the general PCA to be able to compare the 2 years correctly
### 1-415 : year 2020, 416-825 (410 fishes) : year 2021
PC1b2021<-tapply(PCA2$x[,1][416:825], Datafr.gdf2021$caves, mean )
PC2b2021<-tapply(PCA2$x[,2][416:825], Datafr.gdf2021$caves, mean )
PC1SE2021<-tapply(PCA2$x[,1][416:825], Datafr.gdf2021$caves, st.err )
PC2SE2021<-tapply(PCA2$x[,2][416:825], Datafr.gdf2021$caves, st.err )

PCb2021<-data.frame(PC1=PC1b2021,PC2=PC2b2021, SE1=PC1SE2021, SE2= PC2SE2021, Labels=Caves.Right.Order.Genetic.Cluster, gene=Genetic.Cluster.Linked)
PCb2021$gene<-as.factor(PCb2021$gene)
ggplot(PCb2021, aes(x=PC1,y=PC2, color=gene)) + geom_point(aes(PC1, PC2)) +  geom_errorbar(aes(ymin=PC2-SE2, ymax=PC2+SE2)) +geom_errorbarh(aes(xmin=PC1-SE1, xmax=PC1+SE1))+ geom_text(aes(label=Labels), size=3, hjust=-1) + xlab("PCA 1 (24.89%)") + ylab("PCA 2 (12.62%)")+ labs(title="2021 PCA Analysis")+ theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) +labs(color="Genetic Clusters") +theme(plot.title = element_text(hjust = 0.5))

# Now we need to create the deformation grids - package geomorph - ATH:  we set the extreme of the axes here with pred1 and pred2 for each axis, look at the min and max values of PCA plot 
#If we want magnification, we have to increase these numbers, 
PCA1pred2021<-shape.predictor(Datafr.gdf2021$coords,PCA2$x[,1][416:825],Intercept=FALSE, pred1=-0.01, pred2=0.015)
PCA2pred2021<-shape.predictor(Datafr.gdf2021$coords,PCA2$x[,2][416:825],Intercept=FALSE, pred1=-0.01, pred2=0.015)
Mean_morph2021<-mshape(Datafr.gdf2021$coords) # Here we find the mean morphology or assign mean shape for use with plotRefToTarget below
# Now we compare shape at extreme of axis to mean morpholog using deformation grids
plotRefToTarget(Mean_morph2021, PCA1pred2021$pred1, mag=3) #min PC1 , can change magnification 
plotRefToTarget(Mean_morph2021, PCA1pred2021$pred2, mag=3) #max PC1
plotRefToTarget(Mean_morph2021, PCA2pred2021$pred1, mag=3) #min PC2
plotRefToTarget(Mean_morph2021, PCA2pred2021$pred2, mag=3) #max PC2
# similar but we see shape change with vector for each LM, can help to describe what the changes are along the PCA axes: 
plotRefToTarget(Mean_morph2021, PCA1pred2021$pred1, method="vector",mag=3) #min PC1 , can change magnification 
plotRefToTarget(Mean_morph2021, PCA1pred2021$pred2, method="vector",mag=3) #max PC1
plotRefToTarget(Mean_morph2021, PCA2pred2021$pred1, method="vector",mag=3) #min PC2
plotRefToTarget(Mean_morph2021, PCA2pred2021$pred2, method="vector",mag=3) #max PC2

### -- correlation PCA with logCsize? loglength? ----
# with PC1?
cor.test(Datafr.gdf2$logCsize[1:415], PCA2$x[,1][1:415]) #0.4869306
cor.test(Datafr.gdf2$logCsize[416:825], PCA2$x[,1][416:825]) #0.5591499

cor.test(Datafr.gdf2$loglength[1:415], PCA2$x[,1][1:415]) #0.831683
cor.test(Datafr.gdf2$loglength[416:825], PCA2$x[,1][416:825]) #0.7485167

# with PC2?
cor.test(Datafr.gdf2$logCsize[1:415], PCA2$x[,2][1:415]) #-0.09496582
cor.test(Datafr.gdf2$logCsize[416:825], PCA2$x[,2][416:825]) #0.03866801 

cor.test(Datafr.gdf2$loglength[1:415], PCA2$x[,2][1:415]) #-0.0762196
cor.test(Datafr.gdf2$loglength[416:825], PCA2$x[,2][416:825]) #-0.07603059

### -- putting YEAR 2020 2021 in 1 graph ----

PCb2021_new<-data.frame(PC1=PC1b2021,PC2=PC2b2021, SE1=PC1SE2021, SE2= PC2SE2021, Labels=Caves.Right.Order.Genetic.Cluster.2021, gene=Genetic.Cluster.Linked)
PCb2021_new$gene<-as.factor(PCb2021_new$gene)

### I melt the years 2020 and 2021
PCb2020_2021 <- rbind(PCb2020,PCb2021_new)

ggplot(PCb2020_2021, aes(x=PC1,y=PC2, color=gene)) + geom_point(aes(PC1, PC2)) +  geom_errorbar(aes(ymin=PC2-SE2, ymax=PC2+SE2)) +geom_errorbarh(aes(xmin=PC1-SE1, xmax=PC1+SE1))+ geom_text(aes(label=Labels), size=4, hjust=-1) + xlab("PCA 1 (24.89%)") + ylab("PCA 2 (12.62%)")+ labs(title="2021 2020 PCA Analysis")+ theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) +labs(color="Genetic Clusters") +theme(plot.title = element_text(hjust = 0.5))

### ---------------- 2020 and 2021 Graphing PCA, mean of each cave (PC3 PC4)------------------

### Here we do like before, just splitting in 2 years so no a lot of comment really needed

### -- YEAR 2020 ----

### Average shape, we visualize average shape in each cave and for each year along the two PCA axes that explained most variation.
# We take the 2020 part in the general PCA to be able to compare the 2 years
### 1-415 : year 2020, 416-825 (410 fishes) : year 2021
PC1b2020<-tapply(PCA2$x[,3][1:415], Datafr.gdf2020$caves, mean )
PC2b2020<-tapply(PCA2$x[,4][1:415], Datafr.gdf2020$caves, mean )
PC1SE2020<-tapply(PCA2$x[,3][1:415], Datafr.gdf2020$caves, st.err )
PC2SE2020<-tapply(PCA2$x[,4][1:415], Datafr.gdf2020$caves, st.err )

PCb2020<-data.frame(PC1=PC1b2020,PC2=PC2b2020, SE1=PC1SE2020, SE2= PC2SE2020, Labels=Caves.Right.Order.Genetic.Cluster, gene=Genetic.Cluster.Linked)
PCb2020$gene<-as.factor(PCb2020$gene)
ggplot(PCb2020, aes(x=PC1,y=PC2, color=gene)) + geom_point(aes(PC1, PC2)) +  geom_errorbar(aes(ymin=PC2-SE2, ymax=PC2+SE2)) +geom_errorbarh(aes(xmin=PC1-SE1, xmax=PC1+SE1))+ geom_text(aes(label=Labels), size=3, hjust=-1) + xlab("PCA 1 (24.89%)") + ylab("PCA 2 (12.62%)")+ labs(title="2020 PCA Analysis")+ theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) +labs(color="Genetic Clusters") +theme(plot.title = element_text(hjust = 0.5))

# Now we need to create the deformation grids - package geomorph - ATH:  we set the extreme of the axes here with pred1 and pred2 for each axis, look at the min and max values of PCA plot
#If we want magnification, we have to increase these numbers,
PCA3pred2020<-shape.predictor(Datafr.gdf2020$coords,PCA2$x[,3][1:415],Intercept=FALSE, pred1=-0.010, pred2=0.015)
PCA4pred2020<-shape.predictor(Datafr.gdf2020$coords,PCA2$x[,4][1:415],Intercept=FALSE, pred1=-0.010, pred2=0.015)
Mean_morph2020<-mshape(Datafr.gdf2020$coords) # Here we find the mean morphology or assign mean shape for use with plotRefToTarget below
# Now we compare shape at extreme of axis to mean morpholog using deformation grids
plotRefToTarget(Mean_morph2020, PCA3pred2020$pred1, mag=3) #min PC1 , can change magnification
plotRefToTarget(Mean_morph2020, PCA3pred2020$pred2, mag=3) #max PC1
plotRefToTarget(Mean_morph2020, PCA4pred2020$pred1, mag=3) #min PC2
plotRefToTarget(Mean_morph2020, PCA4pred2020$pred2, mag=3) #max PC2
# similar but we see shape change with vector for each LM, can help to describe what the changes are along the PCA axes:
plotRefToTarget(Mean_morph2020, PCA3pred2020$pred1, method="vector",mag=3) #min PC1 , can change magnification
plotRefToTarget(Mean_morph2020, PCA3pred2020$pred2, method="vector",mag=3) #max PC1
plotRefToTarget(Mean_morph2020, PCA4pred2020$pred1, method="vector",mag=3) #min PC2
plotRefToTarget(Mean_morph2020, PCA4pred2020$pred2, method="vector",mag=3) #max PC2

### -- YEAR 2021 ----
# 
# ### Average shape, we visualize average shape in each cave and for each year along the two PCA axes that explained most variation.
# # We take the 2021 part in the general PCA to be able to compare the 2 years correctly
# ### 1-415 : year 2020, 416-825 (410 fishes) : year 2021
# PC1b2021<-tapply(PCA2$x[,3][416:825], Datafr.gdf2021$caves, mean )
# PC2b2021<-tapply(PCA2$x[,4][416:825], Datafr.gdf2021$caves, mean )
# PC1SE2021<-tapply(PCA2$x[,3][416:825], Datafr.gdf2021$caves, st.err )
# PC2SE2021<-tapply(PCA2$x[,4][416:825], Datafr.gdf2021$caves, st.err )
# 
# PCb2021<-data.frame(PC1=PC1b2021,PC2=PC2b2021, SE1=PC1SE2021, SE2= PC2SE2021, Labels=Caves.Right.Order.Genetic.Cluster, gene=Genetic.Cluster.Linked)
# PCb2021$gene<-as.factor(PCb2021$gene)
# ggplot(PCb2021, aes(x=PC1,y=PC2, color=gene)) + geom_point(aes(PC1, PC2)) +  geom_errorbar(aes(ymin=PC2-SE2, ymax=PC2+SE2)) +geom_errorbarh(aes(xmin=PC1-SE1, xmax=PC1+SE1))+ geom_text(aes(label=Labels), size=3, hjust=-1) + xlab("PCA 1 (24.89%)") + ylab("PCA 2 (12.62%)")+ labs(title="2021 PCA Analysis")+ theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) +labs(color="Genetic Clusters") +theme(plot.title = element_text(hjust = 0.5))
# 
# # Now we need to create the deformation grids - package geomorph - ATH:  we set the extreme of the axes here with pred1 and pred2 for each axis, look at the min and max values of PCA plot 
# #If we want magnification, we have to increase these numbers, 
# PCA1pred2021<-shape.predictor(Datafr.gdf2021$coords,PCA2$x[,3][416:825],Intercept=FALSE, pred1=-0.01, pred2=0.005)
# PCA2pred2021<-shape.predictor(Datafr.gdf2021$coords,PCA2$x[,4][416:825],Intercept=FALSE, pred1=-0.01, pred2=0.005)
# Mean_morph2021<-mshape(Datafr.gdf2021$coords) # Here we find the mean morphology or assign mean shape for use with plotRefToTarget below
# # Now we compare shape at extreme of axis to mean morpholog using deformation grids
# plotRefToTarget(Mean_morph2021, PCA1pred2021$pred1, mag=3) #min PC1 , can change magnification 
# plotRefToTarget(Mean_morph2021, PCA1pred2021$pred2, mag=3) #max PC1
# plotRefToTarget(Mean_morph2021, PCA2pred2021$pred1, mag=3) #min PC2
# plotRefToTarget(Mean_morph2021, PCA2pred2021$pred2, mag=3) #max PC2
# # similar but we see shape change with vector for each LM, can help to describe what the changes are along the PCA axes: 
# plotRefToTarget(Mean_morph2021, PCA1pred2021$pred1, method="vector",mag=3) #min PC1 , can change magnification 
# plotRefToTarget(Mean_morph2021, PCA1pred2021$pred2, method="vector",mag=3) #max PC1
# plotRefToTarget(Mean_morph2021, PCA2pred2021$pred1, method="vector",mag=3) #min PC2
# plotRefToTarget(Mean_morph2021, PCA2pred2021$pred2, method="vector",mag=3) #max PC2

#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################
### PART II : head morphology   #######################################################
#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################


###################################################################
### PART II.2 : fusion of .tps and .csv : obtention of the dataframe (head) #
###################################################################

### As the second part is the same as the 1st, there will be less explaination and comments

### Here we just want to keep the landmarks of the head (so 11 of them)
# So 1, 2, 14, 15, 16, 17, 18, 19, 20, 21, 22
X<-two.d.array(Morph.Data)
Head2<-X[,-c(5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26)] # remove LMs 3 to 13 included
Head_LM2<-arrayspecs(Head2,11,2)

### New slider
# sliding landmarks for all the body : 3 (between 2 and 4), 5 (4-6), 11 (10-12), 15 (14-16), 16 (15-17), 17(16-18)
# 15 (14-16), 16 (15-17), 17(16-18) become 4(3-5),5(4-6),6(5-7)
# Change to slide : 
sliders_Head<-matrix(c(3,4,5,4,5,6,5,6,7),ncol=3,byrow=TRUE)

head.gpa2<-gpagen(Head_LM2,curves=sliders_Head) 

HDatafr.gdf2<-geomorph.data.frame(head.gpa2, caves=ID$Location, year=ID$year, length=ID$Length..mm.,cavesyear=interaction(ID$Location,ID$year),logCsize=log2(head.gpa2$Csize),loglength=log2(ID$Length..mm.),Genetic.Cluster=ID$Genetic.Cluster)

### ---------------- Obtention of the datas (without LM 14)-----------------------------------------

# So we keep 1, 2, 15, 16, 17, 18, 19, 20, 21, 22
Head2W14<-X[,-c(5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28)] # remove LMs 3 to 14 included
Head_LM2W14<-arrayspecs(Head2W14,10,2)

# Sliding landmarks : 15 (14-16), 16 (15-17), 17(16-18) become 3(2-4),4(3-5),5(4-6)
sliders_HeadW14<-matrix(c(2,3,4,3,4,5,4,5,6),ncol=3,byrow=TRUE)

head.gpa2W14<-gpagen(Head_LM2W14,curves=sliders_HeadW14) 

HDatafr.gdf2W14<-geomorph.data.frame(head.gpa2W14, caves=ID$Location, year=ID$year, length=ID$Length..mm.,cavesyear=interaction(ID$Location,ID$year),logCsize=log2(head.gpa2W14$Csize),loglength=log2(ID$Length..mm.))


###############################
### PART II.3 : outliers? (head)     #
###############################

### plots of identified outlier specimens compared to the mean shape (plot each landmark, and inspect outliers, scroll the plots to see the outliers)
plotOutliers(head.gpa2$coords, inspect.outliers = TRUE)    
plot(head.gpa2)
print(plotOutliers(head.gpa2$coords, inspect.outliers = TRUE))
#### List of the outliers in the whole dataset :
### Print = here the list of the whole dataset, those on top are the outliers (need to know the first outlier with the plot for that, from bottom to top, from right to left)
# Permit to know the line of the fish to after know the year / cave
# 981927      184622  CAL20-4449      184305      C97019  CAL20-4421      123669      655457  
# 608         471         255         429         484         127         802         131 
# 981923      123482      183F4E  CAL20-4502      981955      897881  CAL20-4521      123950  
# 106          25         427          47         260         312          30         313 
# CAL20-4425      897978  CAL20-4410  
# 16         418         100
list_outliersH <- c(608,471,255,429,484,127,802,131,106,25,427,47,260,312,30,313,16,418,100)
# We have 19 outliers
length(list_outliersH)
# That lead to 2.3% of the dataset, it's correct
(length(list_outliersH)/length(ID$IDPicture))*100

### Outliers with huge Csize?
# No here
# for (i in 1:length(ID$IDPicture)){
#   if (HDatafr.gdf2$logCsize[i]>0){
#     print(paste("ID :",ID$IDPicture[i]," caves:",ID$Location[i]," logCsize:",Datafr.gdf2$logCsize[i]," year:",ID$year[i]))
#   }
# }

### Why are they outliers ? A df is created with only them :
ID_outliersH <- ID[(list_outliersH),]
write.csv(ID_outliersH,"D:/stageCESURE-Islande/Statistic_study/R analysis shape - project 1/2020and2021June_outliers_head.csv", row.names = FALSE)
### Checking the landmarks :
# - they are ok
# - I have checked some big and small fishes not outliers to see if I haven't done good landmarks on the 7-8-9-10 but no
### Checking the length:
# The outliers have a length more important
sum(ID$Length..mm.)/length(ID$Length..mm.) #average (82.9mm)
sum(ID_outliersH$Length..mm.)/length(ID_outliersH$Length..mm.) #outliers (91.75mm)
### Checking the weight
# no weight for fish line 465, so I don't count it for the calcul
# The outliers have a weight more important !
IDweight <- ID
IDweight <- ID[-465,]
sum(IDweight$Weight..g)/length(IDweight$Weight..g.) #average (6.4g)
sum(ID_outliersH$Weight..g.)/length(ID_outliersH$Weight..g.) #outliers (10.7g)
### Checking the caves : they don't come from a specific cave
### Checking if dead fishes? : no

#####################################
### PART II.4 : basic statistical study (head)#
#####################################

### ---------------- correlation or not?--------------------------------------

### Correlations between logCsize and length?
# a correlation is strong when in [-1,-0.5]U[0.5,1]
# positive correlation (R²=0.23; P<0.001), so the 2 values augment together but not that high
# it means that I can not have the two measures of size in the same model
cor.test(HDatafr.gdf2$logCsize, HDatafr.gdf2$loglength) # log² with corr = 0.23
cor.test(HDatafr.gdf2$Csize, HDatafr.gdf2$length) # with corr = 0.25
cor.test(log(HDatafr.gdf2$Csize), log(HDatafr.gdf2$length)) # log with corr = 0.26 = log²

plot(HDatafr.gdf2$loglength, HDatafr.gdf2$logCsize, type="n", xlab="log2(Fork length in mm)", ylab="log2 Centroid Size", main = "Correlation (head)")
points(HDatafr.gdf2$loglength, HDatafr.gdf2$logCsize,col="red")
abline(lm(HDatafr.gdf2$logCsize~HDatafr.gdf2$loglength) ,col="black") 


### ---------------- effects or not by cave and year? ----------------
### -- effects or not on logCsize?-----------------------------------------

### Here we test the analyse of variance model with 2 factors with interaction (ANOVA)
# (Analyse of variance model : qualitative sources of variability)
# (so not the quantitative explanatory variable which is the length)
# model that we'll test :
# ∀i,j,k with (i,j,k)≠(i’j,’,k’)
# εijk ∼ N (0, σ) and cov(εijk,εi’j’k’)≠0
# Y(i,j,k) = µ + α(i) + β(j) + (αβ)(i,j) + ε(k)
# Y(i,j,k) logCsize 
# µ average logCsize 
# α(i) effect of the cave
# β(j) effect of the year
# (αβ)(i,j) effect of the interaction caves:year

Color_caves_genetic_cluster <- c("green","yellow","yellow","red","violet","violet","violet",
                                 "violet","green","violet","blue","blue","blue","blue",
                                 "red","green","yellow","yellow","yellow","red")

### CAVE EFFECT : yes
# model here : logCsize ~ cave
# p-value < 0.05 so significant effect !
ANCsizeH<-aov(HDatafr.gdf2$logCsize~HDatafr.gdf2$caves)
summary(ANCsizeH) 
boxplot(HDatafr.gdf2$logCsize~HDatafr.gdf2$caves, xlab= "cave", ylab= "log2(Centroid Size)", main="Cave effect on Csize (head)",las=2,col="grey")
boxplot(HDatafr.gdf2$logCsize~HDatafr.gdf2$caves, xlab= "cave", ylab= "log2(Centroid Size)", main="Cave effect on Csize (head)",las=2,col=Color_caves_genetic_cluster)
# mu, C22,C23 and C27 are significant
AovSum(HDatafr.gdf2$logCsize~HDatafr.gdf2$caves)

### YEAR EFFECT : yes
# model here : logCsize ~ year
# p-value < 0.05 so significant effect !
ANCsize2H<-aov(HDatafr.gdf2$logCsize~HDatafr.gdf2$year)
summary(ANCsize2H)
boxplot(HDatafr.gdf2$logCsize~HDatafr.gdf2$year, xlab= "year", ylab= "log2(Centroid Size)", col= c("red", "orange"), main = "Year effect on Csize (head)")
# all year (2020 and 2021) significant
AovSum(HDatafr.gdf2$logCsize~HDatafr.gdf2$year)

### CAVE*YEAR EFFECT : yes 
# model here : logCsize ~ cave*year
# p-value < 0.05 here
ANCsize3H<-aov(HDatafr.gdf2$logCsize~HDatafr.gdf2$caves:HDatafr.gdf2$year)
summary(ANCsize3H)
boxplot(HDatafr.gdf2$logCsize~HDatafr.gdf2$caves:HDatafr.gdf2$year, xlab= "Cave and year", ylab="log2(Centroid Size)", main="Cave x year effect on Csize (head)",col=c("red","red","red","red","red","red","red","red","red","red","red","red","red","red","red","red","red","red","red","red","orange","orange","orange","orange","orange","orange","orange","orange","orange","orange","orange","orange","orange","orange","orange","orange","orange","orange","orange","orange"))
boxplot(HDatafr.gdf2$logCsize~HDatafr.gdf2$caves:HDatafr.gdf2$year, xlab= "Cave and year", ylab="log2(Centroid Size)", main="Cave x year effect on Csize (head)",col=c(Color_caves_genetic_cluster,Color_caves_genetic_cluster))
# Here the interactions that are significant :
AovSum(HDatafr.gdf2$logCsize~HDatafr.gdf2$caves*HDatafr.gdf2$year)

### Normality of the residuals of ANCsize3 : all seems ok ! 
# for shapiro test, if p-value < 0.05, the test it is ok
# the residuals need to have a bell form close to 0 to be normals, it's not a proof but can say if there is a problem
hist(ANCsize3H$residuals)
shapiro.test(ANCsize3H$residuals)

### Post hoc test
# = to see if the groups that are particularly different, works only with factors
# we have many interactions significantly different from each other, mostly between the 2 years
getOption("max.print") #we have just 1000, it's not sufficient
options(max.print=99999) #so we change the limit
TukeyHSD(ANCsize3H, group = interaction(HDatafr.gdf2$caves, HDatafr.gdf2$year))
plot(TukeyHSD(ANCsize3H, conf.level=.95), las = 2)
options(max.print=1000) #we put the limit back at normal

### Model here that we keep :
# ∀i,j,k with (i,j,k)≠(i’j,’,k’)
# εijk ∼ N (0, σ) and cov(εijk,εi’j’k’)≠0
# Y(i,j,k) = µ + α(i) + (αβ)(i,j) + ε(k)
# Y(i,j,k) logCsize 
# µ average logCsize 
# β(j) effect of the year
# α(i) effect of the cave
# (αβ)(i,j) effect of the interaction caves:year


### -- effects or not on logCsize? (without LM 14)-----------------------------------------

### We change the datas

### Here we test the analyse of variance model with 2 factors with interaction (ANOVA)
# (Analyse of variance model : qualitative sources of variability)
# (so not the quantitative explanatory variable which is the length)
# model that we'll test :
# ∀i,j,k with (i,j,k)≠(i’j,’,k’)
# εijk ∼ N (0, σ) and cov(εijk,εi’j’k’)≠0
# Y(i,j,k) = µ + α(i) + β(j) + (αβ)(i,j) + ε(k)
# Y(i,j,k) logCsize 
# µ average logCsize 
# α(i) effect of the cave
# β(j) effect of the year
# (αβ)(i,j) effect of the interaction caves:year

### CAVE EFFECT : yes
# model here : logCsize ~ cave
# p-value < 0.05 so significant effect !
ANCsizeHW14<-aov(HDatafr.gdf2W14$logCsize~HDatafr.gdf2W14$caves)
summary(ANCsizeHW14) 
boxplot(HDatafr.gdf2W14$logCsize~HDatafr.gdf2W14$caves, xlab= "cave", ylab= "log2(Centroid Size)", main="Cave effect on Csize (head W14)",las=2,col="grey")
AovSum(HDatafr.gdf2W14$logCsize~HDatafr.gdf2W14$caves)

### YEAR EFFECT : yes
# model here : logCsize ~ year
# p-value < 0.05 so significant effect !
ANCsize2HW14<-aov(HDatafr.gdf2W14$logCsize~HDatafr.gdf2W14$year)
summary(ANCsize2HW14)
boxplot(HDatafr.gdf2W14$logCsize~HDatafr.gdf2W14$year, xlab= "year", ylab= "log2(Centroid Size)", col= c("red", "orange"), main = "Year effect on Csize (head)")
# all year (2020 and 2021) significant
AovSum(HDatafr.gdf2W14$logCsize~HDatafr.gdf2W14$year)

### CAVE*YEAR EFFECT : yes 
# model here : logCsize ~ cave*year
# p-value < 0.05 here
ANCsize3HW14<-aov(HDatafr.gdf2W14$logCsize~HDatafr.gdf2W14$caves:HDatafr.gdf2W14$year)
summary(ANCsize3HW14)
boxplot(HDatafr.gdf2W14$logCsize~HDatafr.gdf2W14$caves:HDatafr.gdf2W14$year, xlab= "Cave and year", ylab="log2(Centroid Size)", main="Cave x year effect on Csize (head W14)",col=c("red","red","red","red","red","red","red","red","red","red","red","red","red","red","red","red","red","red","red","red","orange","orange","orange","orange","orange","orange","orange","orange","orange","orange","orange","orange","orange","orange","orange","orange","orange","orange","orange","orange"))
# Here the interactions that are significant :
AovSum(HDatafr.gdf2W14$logCsize~HDatafr.gdf2W14$caves*HDatafr.gdf2W14$year)

### Normality of the residuals of ANCsize3 : all seems ok ! 
# for shapiro test, if p-value < 0.05, the test it is ok
# the residuals need to have a bell form close to 0 to be normals, it's not a proof but can say if there is a problem
hist(ANCsize3H$residuals)
shapiro.test(ANCsize3H$residuals)

### Post hoc test
# = to see if the groups that are particularly different, works only with factors
# we have many interactions significantly different from each other, mostly between the 2 years
getOption("max.print") #we have just 1000, it's not sufficient
options(max.print=99999) #so we change the limit
TukeyHSD(ANCsize3H, group = interaction(HDatafr.gdf2$caves, HDatafr.gdf2$year))
plot(TukeyHSD(ANCsize3H, conf.level=.95), las = 2)
options(max.print=1000) #we put the limit back at normal

### Model here that we keep :
# ∀i,j,k with (i,j,k)≠(i’j,’,k’)
# εijk ∼ N (0, σ) and cov(εijk,εi’j’k’)≠0
# Y(i,j,k) = µ + α(i) + (αβ)(i,j) + ε(k)
# Y(i,j,k) logCsize 
# µ average logCsize 
# β(j) effect of the year
# α(i) effect of the cave
# (αβ)(i,j) effect of the interaction caves:year
### -- effects or not on loglength?-----------------------------------------

### Here we test the analyse of variance model with 2 factors with interaction (ANOVA)
# (Analyse of variance model : qualitative sources of variability)
# (so not the quantitative explanatory variable which is the length)
# model ANOVA that we'll test :
# ∀i,j,k with (i,j,k)≠(i’j,’,k’)
# εijk ∼ N (0, σ) and cov(εijk,εi’j’k’)≠0
# Y(i,j,k) = mu + α(i) + β(j) + (αβ)(i,j) + ε(k)
# Y(i,j,k) loglength 
# mu average loglength
# α(i) effect of the cave
# β(j) effect of the year
# (αβ)(i,j) effect of the interaction caves:year

Color_caves_genetic_cluster <- c("green","yellow","yellow","red","violet","violet","violet",
                                 "violet","green","violet","blue","blue","blue","blue",
                                 "red","green","yellow","yellow","yellow","red")

### CAVE EFFECT : yes
# model here : loglength ~ cave
# p-value < 0.05 here so significant effect !
ANCsizebH<-aov(HDatafr.gdf2$loglength~HDatafr.gdf2$caves)
summary(ANCsizebH) 
boxplot(HDatafr.gdf2$loglength~HDatafr.gdf2$caves, xlab= "caves", ylab="log2(Fork length in mm)",las=2, col.box= NULL, main = "Caves effect on fork length (head)",col="grey") # plot how headsize vary among caves (similar to the plot FL by caves plot for body shape in manuscript).
boxplot(HDatafr.gdf2$loglength~HDatafr.gdf2$caves, xlab= "caves", ylab="log2(Fork length in mm)",las=2, col.box= NULL, main = "Caves effect on fork length (head)",col=Color_caves_genetic_cluster) # plot how headsize vary among caves (similar to the plot FL by caves plot for body shape in manuscript).
# mu, C1, C22, C23, C25, C27 are significant
AovSum(HDatafr.gdf2$loglength~HDatafr.gdf2$caves)

### YEAR EFFECT : yes
# model here : loglength ~ year
# p-value < 0.05 so significant effect !
ANCsize2bH<-aov(HDatafr.gdf2$loglength~HDatafr.gdf2$year)
summary(ANCsize2bH)
boxplot(HDatafr.gdf2$loglength~HDatafr.gdf2$year, xlab= "year", ylab= "log2(Fork length in mm)", col= c("red", "orange"), main = "Year effect (head)")
# all year (2020 and 2021) significant
AovSum(HDatafr.gdf2$loglength~HDatafr.gdf2$year)

### CAVE*YEAR EFFECT : yes
# model here : loglength ~ cave*year
# p-value < 0.05 here so significant !
ANCsize3bH<-aov(HDatafr.gdf2$loglength~HDatafr.gdf2$caves:HDatafr.gdf2$year)
summary(ANCsize3bH)
boxplot(HDatafr.gdf2$loglength~HDatafr.gdf2$caves:HDatafr.gdf2$year, xlab= "Cave and year", ylab="log2(Fork length in mm)", main="Cave x year effect on fork length (head)", col=c("red","red","red","red","red","red","red","red","red","red","red","red","red","red","red","red","red","red","red","red","orange","orange","orange","orange","orange","orange","orange","orange","orange","orange","orange","orange","orange","orange","orange","orange","orange","orange","orange","orange"))
boxplot(HDatafr.gdf2$loglength~HDatafr.gdf2$caves:HDatafr.gdf2$year, xlab= "Cave and year", ylab="log2(Fork length in mm)", main="Cave x year effect on fork length (head)", col=c(Color_caves_genetic_cluster,Color_caves_genetic_cluster))
# Here the interactions that are significant :
AovSum(HDatafr.gdf2$loglength~HDatafr.gdf2$caves*HDatafr.gdf2$year)

### Normality of the residuals of ANCsize3 : all seems ok ! 
# for shapiro test, if p-value < 0.05, the test it is ok
# the residuals need to have a bell form close to 0 to be normals, it's not a proof but can say if there is a problem
hist(ANCsize3bH$residuals)
shapiro.test(ANCsize3bH$residuals)

### Post hoc test
# = to see if the groups that are particularly different, works only with factors
# we have not so many interactions significantly different from each other
getOption("max.print") #we have just 1000, it's not sufficient
options(max.print=99999) #so we change the limit
TukeyHSD(ANCsize3bH, group = interaction(HDatafr.gdf2$caves, HDatafr.gdf2$year))
plot(TukeyHSD(ANCsize3, conf.level=.95), las = 2)
options(max.print=1000) #we put the limit back at normal

### Model ANOVA here that we keep :
# ∀i,j,k with (i,j,k)≠(i’j,’,k’)
# εijk ∼ N (0, σ) and cov(εijk,εi’j’k’)≠0
# Y(i,j,k) = mu + α(i) + β(j) + (αβ)(i,j) + ε(k)
# Y(i,j,k) loglength 
# mu average loglength 
# α(i) effect of the cave
# β(j) effect of the year
# (αβ)(i,j) effect of the interaction caves:year

### ---------------- effects or not by genetic cluster and year?------
### -- effects or not on logCsize?-----------------------------------------

### Here we test the analyse of variance model with 2 factors with interaction (ANOVA)
# (Analyse of variance model : qualitative sources of variability)
# (so not the quantitative explanatory variable which is the length)
# model that we'll test :
# ∀i,j,k with (i,j,k)≠(i’j,’,k’)
# εijk ∼ N (0, σ) and cov(εijk,εi’j’k’)≠0
# Y(i,j,k) = µ + α(i) + β(j) + (αβ)(i,j) + ε(k)
# Y(i,j,k) logCsize 
# µ average logCsize 
# α(i) effect of the genetic cluster
# β(j) effect of the year
# (αβ)(i,j) effect of the interaction genetic cluster:year

### GENETIC CLUSTER EFFECT : yes
# model here : logCsize ~ Genetic.Cluster
# p-value < 0.05 so significant effect !
ANCsize<-aov(HDatafr.gdf2$logCsize~HDatafr.gdf2$Genetic.Cluster)
summary(ANCsize) 
boxplot(HDatafr.gdf2$logCsize~HDatafr.gdf2$Genetic.Cluster, xlab= "cave", ylab= "log2(Centroid Size)", main="Genetic.Cluster effect on Csize (head)",las=2,col="grey")
# mu, H-C, H-S and V-E are significant
AovSum(HDatafr.gdf2$logCsize~HDatafr.gdf2$Genetic.Cluster)

### GENETIC CLUSTER*YEAR EFFECT : yes 
# model here : logCsize ~ Genetic.Cluster*year
# p-value < 0.05 here
ANCsize3<-aov(HDatafr.gdf2$logCsize~HDatafr.gdf2$Genetic.Cluster:HDatafr.gdf2$year)
summary(ANCsize3)
boxplot(HDatafr.gdf2$logCsize~HDatafr.gdf2$Genetic.Cluster:HDatafr.gdf2$year, xlab= "Genetic.Cluster and year", ylab="log2(Centroid Size)", main="Genetic.Cluster x year effect on Csize (head)",col="grey")
AovSum(HDatafr.gdf2$logCsize~HDatafr.gdf2$Genetic.Cluster*HDatafr.gdf2$year)

### Normality of the residuals of ANCsize3 : all seems ok ! 
# for shapiro test, if p-value < 0.05, the test it is ok
# the residuals need to have a bell form close to 0 to be normals, it's not a proof but can say if there is a problem
hist(ANCsize3$residuals)
shapiro.test(ANCsize3$residuals)

### Post hoc test
# = to see if the groups that are particularly different, works only with factors
# we have many interactions significantly different from each other, mostly between the 2 years
getOption("max.print") #we have just 1000, it's not sufficient
options(max.print=99999) #so we change the limit
TukeyHSD(ANCsize3, group = interaction(Datafr.gdf2$caves, Datafr.gdf2$year))
plot(TukeyHSD(ANCsize3, conf.level=.95), las = 2)
options(max.print=1000) #we put the limit back at normal

### Model here that we keep :
# ∀i,j,k with (i,j,k)≠(i’j,’,k’)
# εijk ∼ N (0, σ) and cov(εijk,εi’j’k’)≠0
# Y(i,j,k) = µ + α(i) + (αβ)(i,j) + ε(k)
# Y(i,j,k) logCsize 
# µ average logCsize 
# β(j) effect of the year
# α(i) effect of the Genetic.Cluster
# (αβ)(i,j) effect of the interaction Genetic.Cluster:year

### -- effects or not on loglength?-----------------------------------------

### Here we test the analyse of variance model with 2 factors with interaction (ANOVA)
# (Analyse of variance model : qualitative sources of variability)
# (so not the quantitative explanatory variable which is the length)
# model ANOVA that we'll test :
# ∀i,j,k with (i,j,k)≠(i’j,’,k’)
# εijk ∼ N (0, σ) and cov(εijk,εi’j’k’)≠0
# Y(i,j,k) = mu + α(i) + β(j) + (αβ)(i,j) + ε(k)
# Y(i,j,k) loglength 
# mu average loglength
# α(i) effect of the Genetic.Cluster
# β(j) effect of the year
# (αβ)(i,j) effect of the interaction Genetic.Cluster:year

### GENETIC CLUSTER EFFECT : yes
# model here : loglength ~ Genetic.Cluster
# p-value < 0.05 here so significant effect !
ANCsizeb<-aov(HDatafr.gdf2$loglength~HDatafr.gdf2$Genetic.Cluster)
summary(ANCsizeb) 
boxplot(HDatafr.gdf2$loglength~HDatafr.gdf2$Genetic.Cluster, xlab= "Genetic.Cluster", ylab="log2(Fork length in mm)",las=2, col.box= NULL, main = "Genetic.Cluster effect on fork length (head)",col="grey")
AovSum(HDatafr.gdf2$loglength~HDatafr.gdf2$Genetic.Cluster)

### GENETIC CLUSTER*YEAR EFFECT : yes
# model here : loglength ~ Genetic.Cluster*year
# p-value < 0.05 here so significant !
ANCsize3b<-aov(HDatafr.gdf2$loglength~HDatafr.gdf2$Genetic.Cluster:HDatafr.gdf2$year)
summary(ANCsize3b)
boxplot(HDatafr.gdf2$loglength~HDatafr.gdf2$Genetic.Cluster:HDatafr.gdf2$year, xlab= "Genetic.Cluster and year", ylab="log2(Fork length in mm)", main="Genetic.Cluster x year effect on fork length",col="grey")
AovSum(HDatafr.gdf2$loglength~HDatafr.gdf2$Genetic.Cluster*HDatafr.gdf2$year)

### Normality of the residuals of ANCsize3 : all seems ok ! 
# for shapiro test, if p-value < 0.05, the test it is ok
# the residuals need to have a bell form close to 0 to be normals, it's not a proof but can say if there is a problem
hist(ANCsize3b$residuals)
shapiro.test(ANCsize3b$residuals)

### Post hoc test
# = to see if the groups that are particularly different, works only with factors
getOption("max.print") #we have just 1000, it's not sufficient
options(max.print=99999) #so we change the limit
TukeyHSD(ANCsize3b, group = interaction(Datafr.gdf2$caves, Datafr.gdf2$year))
plot(TukeyHSD(ANCsize3, conf.level=.95), las = 2)
options(max.print=1000) #we put the limit back at normal

### Model ANOVA here that we keep :
# ∀i,j,k with (i,j,k)≠(i’j,’,k’)
# εijk ∼ N (0, σ) and cov(εijk,εi’j’k’)≠0
# Y(i,j,k) = mu + α(i) + β(j) + (αβ)(i,j) + ε(k)
# Y(i,j,k) loglength 
# mu average loglength 
# α(i) effect of the cave
# β(j) effect of the year
# (αβ)(i,j) effect of the interaction caves:year

### ---------------- models selection : allometry and cave on  bodyshape ---------------------

### Here the multiple regression linear model :
# (Regression model : Quantitative sources of variability)
# cov(εi,εk)≠0 and εk ∼ N (0, σ²) normal distribution
# Yk= β0 + β1x1 + .. + εk     
# Yk coords
# x1 loglength
# x2 caves
# x3 year
# procd.lm = Procrustes ANOVA/regression for Procrustes shape variables (with lm = linear model)
# so it's not exactly a multiple regression linear model, it's a mix with ANOVA

### Understanding of the model
# RRPP : Randomization of Residuals in a Permutation Procedure
# SS.type= III : ensure that you dont have a hierarchical order (i.e. the order of factors do not matter)
# Rsq = R square show % of variation of data explained by the factor. 
# find a model that reduce the variation RSS (variation) need to be reduce and be significant

### Link to understand the model
# https://statisticsbyjim.com/regression/interaction-effects/
# https://stats.libretexts.org/Bookshelves/Applied_Statistics/Book%3A_Natural_Resources_Biometrics_(Kiernan)/06%3A_Two-way_Analysis_of_Variance/6.01%3A_Main_Effects_and_Interaction_Effect

### Model selection: (called manual backward selection as you start with the full model and drop term as you go)
# - full model is logFL*cave*year based on my question and hypothesis...
# - (I expect to have an effect of body size, cave and year on body shape and I expect that those effects may vary across years and cave, therefore I have an interaction of the 3 factors).
# - what is results of this model, can I simpify the model based on removel of non significant terms? in your case the next try is to cretae models with all two terms interactions, and drop 1 way interaction that are not significant. Compare models with an f test
# - once you have a model, check assumptions are met, and distrivution of the residuals. 

### -- MODEL 1 ------

# S1_1IO3 mean model 1a with 1 interaction of order 3 (no order 4 or more)
# s1f_3IO1 = model 1f with 3 interactions of order 1 (no order 2 or more)
# the H is added because I study the head of the fish

#### MODEL WITH LOGLENGTH, CAVES, YEAR
s1aH_1IO3 <- procD.lm(coords~loglength*caves*year, iter=999,  RRPP=TRUE, SS.type = "III", data=HDatafr.gdf2)
summary(s1aH_1IO3)
s1bH_3IO2 <- procD.lm(coords~loglength+caves+year+loglength*caves+loglength*year+caves*year, iter=999,  RRPP=TRUE, SS.type = "III", data=HDatafr.gdf2)
summary(s1bH_3IO2)
s1cH_2IO2 <- procD.lm(coords~loglength+caves+year+loglength*caves+caves*year, iter=999,  RRPP=TRUE, SS.type = "III", data=HDatafr.gdf2)
summary(s1cH_2IO2)
s1dH_2IO2 <- procD.lm(coords~loglength+caves+year+loglength*year+caves*year, iter=999,  RRPP=TRUE, SS.type = "III", data=HDatafr.gdf2)
summary(s1dH_2IO2)
s1eH_2IO2 <- procD.lm(coords~loglength+caves+year+loglength*year+caves*loglength, iter=999,  RRPP=TRUE, SS.type = "III", data=HDatafr.gdf2)
summary(s1eH_2IO2)
s1fH_3IO1 <- procD.lm(coords~loglength+caves+year, iter=999,  RRPP=TRUE, SS.type = "III", data=HDatafr.gdf2)
summary(s1fH_3IO1)

### Selection of model :
# if p-value < 0.05, model1 fit less than model2 with anova(model1,model2)
# see if more complex model fit more than simpler model 
anova(s1b_3IO2,s1aH_1IO3) #p-value = 0.011 s1aH_1IO3>s1bH_13IO2
anova(s1cH_2IO2,s1bH_3IO2) #p-value = 0.001 s1bH_3IO2>s1cH_3IO2
anova(s1dH_2IO2,s1cH_2IO2) #p-value = 0.001 s1cH_2IO2>s1dH_2IO2
anova(s1eH_2IO2,s1dH_2IO2) #p-value = 0.001 s1dH_2IO2>s1eH_2IO2
anova(s1fH_3IO1,s1eH_2IO2) #p-value = 0.001 s1eH_2IO2>s1fH_3IO1

### So model s1bH_3IO2 chosen :
# Interaction of order 1 and order 2
# coords~loglength*caves*year
s1aH_1IO3 <- procD.lm(coords~loglength*caves*year, iter=999,  RRPP=TRUE, SS.type = "III", data=HDatafr.gdf2)
summary(s1aH_1IO3)

### Inspect the residual of the model
# not that good
plot(s1aH_1IO3)

### -- MODEL 2 ------ 


# S2_1IO3 mean model 1a with 1 interaction of order 3 (no order 4 or more)
# s2f_3IO1 = model 1f with 3 interactions of order 1 (no order 2 or more)
# the H is added because I study the head of the fish


#### MODEL WITH LOGCSIZE, CAVES, YEAR
s2aH_1IO3 <- procD.lm(coords~logCsize*caves*year, iter=999,  RRPP=TRUE, SS.type = "III", data=HDatafr.gdf2)
summary(s2aH_1IO3)
s2bH_3IO2 <- procD.lm(coords~logCsize+caves+year+logCsize*caves+logCsize*year+caves*year, iter=999,  RRPP=TRUE, SS.type = "III", data=HDatafr.gdf2)
summary(s2bH_3IO2)
s2cH_2IO2 <- procD.lm(coords~logCsize+caves+year+logCsize*caves+caves*year, iter=999,  RRPP=TRUE, SS.type = "III", data=HDatafr.gdf2)
summary(s2cH_2IO2)
s2dH_2IO2 <- procD.lm(coords~logCsize+caves+year+logCsize*year+caves*year, iter=999,  RRPP=TRUE, SS.type = "III", data=HDatafr.gdf2)
summary(s2dH_2IO2)
s2eH_2IO2 <- procD.lm(coords~logCsize+caves+year+logCsize*year+caves*logCsize, iter=999,  RRPP=TRUE, SS.type = "III", data=HDatafr.gdf2)
summary(s2eH_2IO2)
s2fH_3IO1 <- procD.lm(coords~logCsize+caves+year, iter=999,  RRPP=TRUE, SS.type = "III", data=HDatafr.gdf2)
summary(s2fH_3IO1)

### Selection of model :
# if p-value < 0.05, model1 fit less than model2 with anova(model1,model2)
# see if more complex model fit more than simpler model 
anova(s2bH_3IO2,s2aH_1IO3) #p-value = 0.006 s2aH_1IO3>s2cH_2IO2
anova(s2cH_2IO2,s2bH_3IO2) #p-value = 0.006 s2cH_2IO2>s2bH_3IO2
anova(s2dH_2IO2,s2cH_2IO2) #p-value = 0.001 s2cH_2IO2>s2dH_2IO2
anova(s2eH_2IO2,s2dH_2IO2) #p-value = 0.001 s2dH_2IO2>s2eH_2IO2
anova(s2fH_3IO1,s2eH_2IO2) #p-value = 0.001 s2eH_2IO2>s2fH_3IO1

### So model s2aH_1IO3 chosen :
# Interaction of order 1, order 2 and order 3
# coords~logCsize*caves*year
s2aH_1IO3 <- procD.lm(coords~logCsize*caves*year, iter=999,  RRPP=TRUE, SS.type = "III", data=HDatafr.gdf2)
summary(s2aH_1IO3)

### Inspect the residual of the model
# not that good
plot(s2aH_1IO3)

### -- MODEL 3 : cave x year ----
s3aH_1IO2 <- procD.lm(coords~caves*year, iter=999,  RRPP=TRUE, SS.type = "III", data=HDatafr.gdf2)
summary(s3aH_1IO2)
s3bH_2IO1 <- procD.lm(coords~caves+year, iter=999,  RRPP=TRUE, SS.type = "III", data=HDatafr.gdf2)
summary(s3bH_2IO1)

### Best model : s3a_1IO2
anova(s3bH_2IO1,s3aH_1IO2) # p-valye = 0.001 so s3aH_1IO2>s3bH_2IO1

summary(s3aH_1IO2)

### ---------------- Plotting regression ------------------------

## YEAR EFFECT
plot(HDatafr.gdf2$loglength, HDatafr.gdf2$logCsize, type="n", xlab="log2(Fork length in mm)", ylab="log2 Centroid Size", main = "Year effect regression (head)")
points(HDatafr.gdf2$loglength, HDatafr.gdf2$logCsize, col=HDatafr.gdf2$year)
legend ("topright", legend= levels(HDatafr.gdf2$year), col=1:length(HDatafr.gdf2$year),pch=1, cex=0.8)

abline(lm(HDatafr.gdf2$logCsize~HDatafr.gdf2$loglength, subset=(subset = HDatafr.gdf2$year== "2020")) ,col="black") 
abline(lm(HDatafr.gdf2$logCsize~HDatafr.gdf2$loglength, subset=(subset = HDatafr.gdf2$year== "2021")) ,col="red")

### ---------------- Procrustes variance  -----------------------------------

### Procrustes variance, taking size into account:
# Procrustes variance is the measure of disparity within tested groups
# V= ( Σ (x-μ)² ) / N         μ mean and N number of individual

### LOGCSIZE
# For all caves, procrustes variance: 0.002002304
Morph.disH<-morphol.disparity(coords~logCsize, groups=NULL, data=HDatafr.gdf2, iter=999)
# For each cave :
Morph.disgroupsH<-morphol.disparity(coords~logCsize*caves, groups=~caves, data=HDatafr.gdf2, iter=999)
Morph.disgroupsH$Procrustes.var

### LOGLENGTH
# For all caves, procrustes variance: 0.001945045
Morph.dis2H<-morphol.disparity(coords~loglength, groups=NULL, data=HDatafr.gdf2, iter=999)
# For each cave :
Morph.disgroups2H<-morphol.disparity(coords~loglength*caves, groups=~caves, data=HDatafr.gdf2, iter=999)
Morph.disgroups2H$Procrustes.var

###############################################
### PART II.5 : PCA and DFA study 2020-2021 June (head) #
###############################################

### ---------------- DFA Analysis -----------------------------------

## Understanding of DFA : read article Rainville 2021 "Parallel evolution of morphological traits and body shape in littoral and pelagic brook charr, Salvelinus fontinalis, along a gradient of interspecific competition"

### Need to change the landmarks to a two-dimension form fro DFA
# 1 line = 1 fish, and 22*2 columns for the coordinates (X then Y) of each landmarks
y2H<-two.d.array(HDatafr.gdf2$coords)
### Here I make the matrix in to a data frame - needed for the discriminant analysis. Discriminant analysis needs the package MASS
# So same as y2
z2H<-as.data.frame(y2H)

### discriminant analysis
# give prior probabilities of groups (2% C1, 3% C10...)
# give mean of landmarks for each group
# then give coefficients of linear discriminants, from LD1 to LD19
# and proportion of trace for each LD
Discr2H <- lda(formula = HDatafr.gdf2$caves~., data = z2H)
Discr2H

### Calculates propabilities for each axis of the distribution
likur2H <- Discr2H$svd^2/sum(Discr2H$svd^2)
likur2H

# Here I find classification and the axis scores and print them in a file in the directory 
Pred.Discr2H<- predict(Discr2H) # computes LD scores, coefficients, etc. 
write.csv(Pred.Discr2H$class, "D:/stageCESURE-Islande/Statistic_study/R analysis shape - project 1/DFAbodyclass_head.csv") 


### -- Matrix of confusion by cave ----

### Matrix of confusion : I will see how good is the DFA classification
# https://www.journaldev.com/46732/confusion-matrix-in-r

### Here a dataframe if I want to visualize the difference for each fish
df_matrix_confusionH <- data.frame(expected=ID$Location,predicted=Pred.Discr2H$class)

### Here the values
# Creates vectors having data points
expected_valueH <- factor(ID$Location)
predicted_valueH <- factor(Pred.Discr2H$class)

### Creating confusion matrix
# Accuracy = 0.7 (Accuracy is calculated as the total number of two correct predictions divided by the total number of a dataset) (https://classeval.wordpress.com/introduction/basic-evaluation-measures/)
# Here I can see that the DFA has some difficulties:
# - 14 of reference C12 were predicted as C25
# - 14 of reference C18 were predicted as C25
# - 11 of reference C21 were predicted as C25
# - 15 of reference C25 were predicted as C26, 10 as C7
# - 14 of reference C27 were predicted as C25
# - 17 of reference C7 were predicted as C25
# For C12 and C7, it can be explained by the fact that they are in the same genetic clusters as C25
### Accuracy by cave just below
Matrix_confusion_DFAH <- caret::confusionMatrix(data=predicted_valueH, reference = expected_valueH)
Matrix_confusion_DFAH


### -- Matrix of confusion by genetic cluster ----

### Caves by Genetic cluster
# H-C : C7, C12 and C25 
# H-N : C5,C6, C10, C11, C27
# H-S : C1, C2, C26
# V-E : C21, C22, C23, C24
# V-W : C17, C17B, C18, C19, C20

### Convert Pred.Discr2H$class from cave to genetic cluster
# If I use merge, I will change the order so I do a little program
Pred_GC <- ID$Genetic.Cluster
for (i in 1:length(Pred.Discr2H$class)){
  if ((Pred.Discr2H$class[i]=="C7")||(Pred.Discr2H$class[i]=="C12")||(Pred.Discr2H$class[i]=="C25")){
    Pred_GC[i] <- "H-C"
  }
  if ((Pred.Discr2H$class[i]=="C5")||(Pred.Discr2H$class[i]=="C6")||(Pred.Discr2H$class[i]=="C10")||(Pred.Discr2H$class[i]=="C11")||(Pred.Discr2H$class[i]=="C27")){
    Pred_GC[i] <- "H-N"
  }
  if ((Pred.Discr2H$class[i]=="C1")||(Pred.Discr2H$class[i]=="C2")||(Pred.Discr2H$class[i]=="C26")){
    Pred_GC[i] <- "H-S"
  }
  if ((Pred.Discr2H$class[i]=="C21")||(Pred.Discr2H$class[i]=="C22")||(Pred.Discr2H$class[i]=="C23")||(Pred.Discr2H$class[i]=="C24")){
    Pred_GC[i] <- "V-E"
  }
  if ((Pred.Discr2H$class[i]=="C17")||(Pred.Discr2H$class[i]=="C17B")||(Pred.Discr2H$class[i]=="C18")||(Pred.Discr2H$class[i]=="C19")||(Pred.Discr2H$class[i]=="C20")){
    Pred_GC[i] <- "V-W"
  }
}
# I check in a data.frame
# test <- data.frame(Pred_GC,Pred.Discr2H$class)

expected_valueH_GC <- factor(ID$Genetic.Cluster)
predicted_valueH_GC <- factor(Pred_GC)
Matrix_confusion_DFAH_GC <- caret::confusionMatrix(data=predicted_valueH_GC, reference = expected_valueH_GC)
Matrix_confusion_DFAH_GC


### ---------------- DFA visualise proportion of each LD ----------

### from braden DFA : visualise the propotion of each LD in explaining the variance

### Here a graph to visualise the % of var explained by each LD
# Putting the theme
mytheme <- theme_bw(base_size = 18) +                                           # Set custom theme.
  theme(panel.grid.major = element_blank(),                                     # Remove grey panel grid on graphs 
        panel.grid.minor = element_blank())
# Then writing down the probabilities of each axis of the distribution (cf likur2)
df_Trace2H <- data.frame(LD = c("LD1", "LD2", "LD3", "LD4", "LD5", "LD6", "LD7", "LD8"),        # Make a scree plot. 
                        pv = c(likur2H[1]*100, likur2H[2]*100, likur2H[3]*100, likur2H[4]*100, likur2H[5]*100, likur2H[6]*100, likur2H[7]*100, likur2H[8]*100))             # % var explained by each LD.(Likur in the script above)
# And then plot it
ggplot(df_Trace2H) + geom_bar(aes(x = LD, y = pv, fill = LD), stat = "identity") +              # load into ggplot
  scale_fill_manual(values = c("grey20", "grey20", "grey60", "grey60", "grey60",              # darken first two axes
                               "grey60", "grey60", "grey60")) + mytheme +                    
  xlab("") + ylab("Variation explained (%)") + theme(legend.position = "none",                # remove legend.
                                                     axis.text = element_text(size = 18),     # bigger axis labels.
                                                     axis.text.x = element_text(angle = 90))  # remove legend, adjust titles. 

### ---------------- PCA general Analysis--------------------------

### Creation of the PCA
# gm.prcomp : Function performs principal components analysis (PCA) or phylogenetically-aligned components (PaCA) on Procrustes shape coordinates.
PCA2H<-gm.prcomp(HDatafr.gdf2$coords)

### The PCA scores/ cumulative proportions :
summary(PCA2H)
PCA2H$d[1]/sum(PCA2H$d) # Eigenvalue for the first principal component
PCA2H$d[2]/sum(PCA2H$d)
plot(PCA2H$d) # Eigenvalues plot

### Plot PCA
# bg is how to color the points
# pch=21 is the type of point, here cercles colored and full
plot(PCA2H,main = "PCA body (head)", pch=21,bg=HDatafr.gdf2$caves) # here by cave
plot(PCA2H,main = "PCA body (head)", pch=21,bg=HDatafr.gdf2$year) # here by year


### ---------------- PC1 PC2.. correlation Csize FL? (head)--------------------------
# PC1 is PCA2$x[,1] ect

### -- correlation with logCsize? ----
# corr logCsize/PC1 = 0.2271911
cor.test(HDatafr.gdf2$logCsize, PCA2H$x[,1]) 
plot(HDatafr.gdf2$logCsize, PCA2H$x[,1], type="n", xlab="log2(Csize)", ylab="PC1", main = "Correlation (head)")
points(HDatafr.gdf2$logCsize, PCA2H$x[,1],col="red")
abline(lm(PCA2H$x[,1]~HDatafr.gdf2$logCsize) ,col="black")

# corr logCsize/PC2 = 0.07895592
cor.test(HDatafr.gdf2$logCsize, PCA2H$x[,2]) 

# corr logCsize/PC3 = -0.1100952
cor.test(HDatafr.gdf2$logCsize, PCA2H$x[,3]) 

# corr logCsize/PC4 = 0.1647456
cor.test(HDatafr.gdf2$logCsize, PCA2H$x[,4]) 

### -- correlation with loglength? ----
# corr loglength/PC1 = 0.1976172  
cor.test(HDatafr.gdf2$loglength, PCA2H$x[,1]) 

# corr loglength/PC2 = 0.408831
cor.test(HDatafr.gdf2$loglength, PCA2H$x[,2]) 
plot(HDatafr.gdf2$loglength, PCA2H$x[,2], type="n", xlab="log2(fork length)", ylab="PC2", main = "Correlation (head)")
points(HDatafr.gdf2$loglength, PCA2H$x[,2],col="red")
abline(lm(PCA2H$x[,2]~HDatafr.gdf2$loglength) ,col="black")

# corr loglength/PC3 = 0.2803205
cor.test(HDatafr.gdf2$loglength, PCA2H$x[,3])
plot(HDatafr.gdf2$loglength, PCA2H$x[,3], type="n", xlab="log2(Fork length in mm)", ylab="PC3", main = "Correlation (head)")
points(HDatafr.gdf2$loglength, PCA2H$x[,3],col="red")
abline(lm(PCA2H$x[,3]~HDatafr.gdf2$loglength) ,col="black")

# corr loglength/PC4 = -0.02073671
cor.test(HDatafr.gdf2$loglength, PCA2H$x[,4]) 

### ---------------- Plotting DFA-----------------------------
# Now to the figures for DFA

### Here I calculate the mean of each cave and create new variables
# This is the mean shape in each cave (mean coordinates along dfa1 and dfa2)
Df1bH<-tapply(Pred.Discr2H$x[,1], HDatafr.gdf2$caves, mean )
Df2bH<-tapply(Pred.Discr2H$x[,2], HDatafr.gdf2$caves, mean )

### R does not have an easy way to calculate standard error - lets teach it
st.err <- function(x) {  +     sd(x)/sqrt(length(x)) }
# Now we can calculate standard errors
Df1SE2H<- tapply(Pred.Discr2H$x[,1], HDatafr.gdf2$caves, st.err)
Df2SE2H<- tapply(Pred.Discr2H$x[,2], HDatafr.gdf2$caves, st.err)

DF2H<-data.frame(Df1=Df1bH,Df2=Df2bH, SE1=Df1SE2H, SE2= Df2SE2H, Labels=Caves.Right.Order.Genetic.Cluster, gene=Genetic.Cluster.Linked)
DF2H$gene<-as.factor(DF2H$gene)

### Here the graph
ggplot(DF2H, aes(x=Df1,y=Df2, color=gene)) + geom_point(aes(Df1, Df2)) +  geom_errorbar(aes(ymin=Df2-SE2, ymax=Df2+SE2)) +geom_errorbarh(aes(xmin=Df1-SE1, xmax=Df1+SE1))+ geom_text(aes(label=Labels), size=4, hjust=-1) + xlab("Discriminant Axis 1 (27.27%)") + ylab("Discriminant Axis 2 (21.83%)")+ labs(title="Discriminant Function Analysis (head)")+ theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) +labs(color="Genetic Clusters") +theme(plot.title = element_text(hjust = 0.5))

# Now we need to create the deformation grids - package geomorph - NOTE that we set the extreme of the axis here. 
#If we want magnification, we have to increase these numbers
DF1pred2H<-shape.predictor(HDatafr.gdf2$coords,Pred.Discr2H$x[,1], Intercept=FALSE, pred1=-1, pred2=1 )
DF2pred2H<-shape.predictor(HDatafr.gdf2$coords,Pred.Discr2H$x[,2], Intercept=FALSE, pred1=-1, pred2=1 )
Mean_morph2H<-mshape(HDatafr.gdf2$coords) # Here we find the mean morphology or assign mean shape for use with plotRefToTarget below
# Now we compare shape at extreme of axis to mean morphology
plotRefToTarget(Mean_morph2H, DF1pred2H$pred1, mag= 3) #min DF1 , can change magnification 
plotRefToTarget(Mean_morph2H, DF1pred2H$pred2, mag= 3) #max DF1
plotRefToTarget(Mean_morph2H, DF2pred2H$pred1, mag= 3) #min DF2
plotRefToTarget(Mean_morph2H, DF2pred2H$pred2, mag= 3) #max DF2
# Same with vectors
plotRefToTarget(Mean_morph2H, DF1pred2H$pred1, method="vector", mag= 3) #min DF1 , can change magnification 
plotRefToTarget(Mean_morph2H, DF1pred2H$pred2, method="vector", mag= 3) #max DF1
plotRefToTarget(Mean_morph2H, DF2pred2H$pred1, method="vector", mag= 3) #min DF2
plotRefToTarget(Mean_morph2H, DF2pred2H$pred2, method="vector", mag= 3) #max DF2


### -- correlation DFA with logCsize? loglength? ----
# with DF1?
cor.test(HDatafr.gdf2$logCsize, Pred.Discr2H$x[,1]) #-0.1701872
cor.test(HDatafr.gdf2$loglength, Pred.Discr2H$x[,1]) #-0.1284323

# with DF2?
cor.test(HDatafr.gdf2$logCsize, Pred.Discr2H$x[,2]) #0.04426506
cor.test(HDatafr.gdf2$loglength, Pred.Discr2H$x[,2]) #-0.01579849 

### ---------------- Graphing PCA, mean of each cave (head PC1 PC2)------------------

### C27 is H-N

#Now to the figures for PCA - similar as DFA above 

### Average shape, we visualize average shape in each cave and for each year along the two PCA axes that explained most variation.
PC1bH<-tapply(PCA2H$x[,1], HDatafr.gdf2$caves, mean )
PC2bH<-tapply(PCA2H$x[,2], HDatafr.gdf2$caves, mean )
PC1SE2H<-tapply(PCA2H$x[,1], HDatafr.gdf2$caves, st.err )
PC2SE2H<-tapply(PCA2H$x[,2], HDatafr.gdf2$caves, st.err )

PCbH<-data.frame(PC1=PC1bH,PC2=PC2bH, SE1=PC1SE2H, SE2= PC2SE2H, Labels=Caves.Right.Order.Genetic.Cluster, gene=Genetic.Cluster.Linked)
PCbH$gene<-as.factor(PCbH$gene)

ggplot(PCbH, aes(x=PC1,y=PC2, color=gene)) + geom_point(aes(PC1, PC2)) +  geom_errorbar(aes(ymin=PC2-SE2, ymax=PC2+SE2)) +geom_errorbarh(aes(xmin=PC1-SE1, xmax=PC1+SE1))+ geom_text(aes(label=Labels), size=4, hjust=-1) + xlab("PCA 1 (26.98%)") + ylab("PCA 2 (19.72%)")+ labs(title="PCA Analysis (head)")+ theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) +labs(color="Genetic Clusters") +theme(plot.title = element_text(hjust = 0.5))

# Now we need to create the deformation grids - package geomorph - ATH:  we set the extreme of the axes here with pred1 and pred2 for each axis, look at the min and max values of PCA plot 
#If we want magnification, we have to increase these numbers, 
PCA1pred2H<-shape.predictor(HDatafr.gdf2$coords,PCA2H$x[,1],Intercept=FALSE, pred1=-0.02, pred2=0.01)
PCA2pred2H<-shape.predictor(HDatafr.gdf2$coords,PCA2H$x[,2],Intercept=FALSE, pred1=-0.02, pred2=0.01)
Mean_morph2H<-mshape(HDatafr.gdf2$coords) # Here we find the mean morphology or assign mean shape for use with plotRefToTarget below
# Now we compare shape at extreme of axis to mean morpholog using deformation grids
plotRefToTarget(Mean_morph2H, PCA1pred2H$pred1, mag=3) #min PC1 , can change magnification 
plotRefToTarget(Mean_morph2H, PCA1pred2H$pred2, mag=3) #max PC1
plotRefToTarget(Mean_morph2H, PCA2pred2H$pred1, mag=3) #min PC2
plotRefToTarget(Mean_morph2H, PCA2pred2H$pred2, mag=3) #max PC2
# similar but we see shape change with vector for each LM, can help to describe what the changes are along the PCA axes: 
plotRefToTarget(Mean_morph2H, PCA1pred2H$pred1, method="vector",mag=3) #min PC1 , can change magnification 
plotRefToTarget(Mean_morph2H, PCA1pred2H$pred2, method="vector",mag=3) #max PC1
plotRefToTarget(Mean_morph2H, PCA2pred2H$pred1, method="vector",mag=3) #min PC2
plotRefToTarget(Mean_morph2H, PCA2pred2H$pred2, method="vector",mag=3) #max PC2

### -- correlation PCA with logCsize? loglength? ----
# with PC1?
cor.test(HDatafr.gdf2$logCsize, PCA2H$x[,1]) #0.2271911 
cor.test(HDatafr.gdf2$loglength, PCA2H$x[,1]) #0.1976387 

# with PC2?
cor.test(HDatafr.gdf2$logCsize, PCA2H$x[,2]) #0.07895592
cor.test(HDatafr.gdf2$loglength, PCA2H$x[,2]) #0.4094947

### ---------------- Graphing PCA, mean of each cave (head PC3 PC4 NEW)------------------

### C27 is H-N

#Now to the figures for PCA - similar as DFA above 

### Average shape, we visualize average shape in each cave and for each year along the two PCA axes that explained most variation.
PC1bH<-tapply(PCA2H$x[,3], HDatafr.gdf2$caves, mean )
PC2bH<-tapply(PCA2H$x[,4], HDatafr.gdf2$caves, mean )
PC1SE2H<-tapply(PCA2H$x[,3], HDatafr.gdf2$caves, st.err )
PC2SE2H<-tapply(PCA2H$x[,4], HDatafr.gdf2$caves, st.err )

PCbH<-data.frame(PC1=PC1bH,PC2=PC2bH, SE1=PC1SE2H, SE2= PC2SE2H, Labels=Caves.Right.Order.Genetic.Cluster, gene=Genetic.Cluster.Linked)
PCbH$gene<-as.factor(PCbH$gene)

ggplot(PCbH, aes(x=PC1,y=PC2, color=gene)) + geom_point(aes(PC1, PC2)) +  geom_errorbar(aes(ymin=PC2-SE2, ymax=PC2+SE2)) +geom_errorbarh(aes(xmin=PC1-SE1, xmax=PC1+SE1))+ geom_text(aes(label=Labels), size=4, hjust=-1) + xlab("PCA 3 (13.68%)") + ylab("PCA 4 (8.84%)")+ labs(title="PCA Analysis (head)")+ theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) +labs(color="Genetic Clusters") +theme(plot.title = element_text(hjust = 0.5))

# Now we need to create the deformation grids - package geomorph - ATH:  we set the extreme of the axes here with pred1 and pred2 for each axis, look at the min and max values of PCA plot 
#If we want magnification, we have to increase these numbers, 
PCA1pred2H<-shape.predictor(HDatafr.gdf2$coords,PCA2H$x[,3],Intercept=FALSE, pred1=-0.01, pred2=0.015)
PCA2pred2H<-shape.predictor(HDatafr.gdf2$coords,PCA2H$x[,4],Intercept=FALSE, pred1=-0.01, pred2=0.015)
Mean_morph2H<-mshape(HDatafr.gdf2$coords) # Here we find the mean morphology or assign mean shape for use with plotRefToTarget below
# Now we compare shape at extreme of axis to mean morpholog using deformation grids
plotRefToTarget(Mean_morph2H, PCA1pred2H$pred1, mag=3) #min PC3 , can change magnification 
plotRefToTarget(Mean_morph2H, PCA1pred2H$pred2, mag=3) #max PC3
plotRefToTarget(Mean_morph2H, PCA2pred2H$pred1, mag=3) #min PC4
plotRefToTarget(Mean_morph2H, PCA2pred2H$pred2, mag=3) #max PC4
# similar but we see shape change with vector for each LM, can help to describe what the changes are along the PCA axes: 
plotRefToTarget(Mean_morph2H, PCA1pred2H$pred1, method="vector",mag=3) #min PC1 , can change magnification 
plotRefToTarget(Mean_morph2H, PCA1pred2H$pred2, method="vector",mag=3) #max PC1
plotRefToTarget(Mean_morph2H, PCA2pred2H$pred1, method="vector",mag=3) #min PC2
plotRefToTarget(Mean_morph2H, PCA2pred2H$pred2, method="vector",mag=3) #max PC2

##########################################################
### PART II.6 : PCA and DFA study 2020 June then 2021 June (head) #
##########################################################

### maybe good to create a plot for 2020 and a plot for 2021 at first to visualise

### ---------------- 2020 and 2021 Introduction of data (head) ----------

### Here we do like in the beggining of the script, just splitting in 2 years
# so no comment really needed

### We separe the datas in 2 after procrustes surperposition (aligned and scaled coordinates)
# so after having cave.gpa2<-gpagen(Morph.Data,curves=sliders2) and after Datafr.gdf2 (more easy that way)
### 1-415 : year 2020, 416-825 (410 fishes) : year 2021
HDatafr.gdf2020 <- geomorph.data.frame(coords=head.gpa2$coords[1:11,1:2,1:415],Csize=head.gpa2$Csize[1:415],caves=ID$Location[1:415], year=ID$year[1:415], length=ID$Length..mm.[1:415],cavesyear=interaction(ID$Location[1:415],ID$year[1:415]),logCsize=log2(head.gpa2$Csize[1:415]),loglength=log2(ID$Length..mm.[1:415]))
HDatafr.gdf2021 <- geomorph.data.frame(coords=head.gpa2$coords[1:11,1:2,416:825],Csize=head.gpa2$Csize[416:825],caves=ID$Location[416:825], year=ID$year[416:825], length=ID$Length..mm.[416:825],cavesyear=interaction(ID$Location[416:825],ID$year[416:825]),logCsize=log2(head.gpa2$Csize[416:825]),loglength=log2(ID$Length..mm.[416:825]))

### ---------------- 2020 and 2021 DFA Analysis (head) -----------------------------------

## Understanding of DFA : read article Rainville 2021 "Parallel evolution of morphological traits and body shape in littoral and pelagic brook charr, Salvelinus fontinalis, along a gradient of interspecific competition"

### -- YEAR 2020 ----

### Need to change the landmarks to a two-dimension form fro DFA
# 1 line = 1 fish, and 22*2 columns for the coordinates (X then Y) of each landmarks
y2020H<-two.d.array(HDatafr.gdf2020$coords)
### Here I make the matrix in to a data frame - needed for the discriminant analysis. Discriminant analysis needs the package MASS
# So same as y2
z2020H<-as.data.frame(y2020H)

### discriminant analysis
# give prior probabilities of groups (2% C1, 3% C10...)
# give mean of landmarks for each group
# then give coefficients of linear discriminants, from LD1 to LD19
# and proportion of trace for each LD
Discr2020H <- lda(formula = HDatafr.gdf2020$caves~., data = z2020H)
Discr2020H

### Calculates propabilities for each axis of the distribution
likur2020H <- Discr2020H$svd^2/sum(Discr2020H$svd^2)
likur2020H

# Here I find classification and the axis scores and print them in a file in the directory 
Pred.Discr2020H<- predict(Discr2020H) # computes LD scores, coefficients, etc. 
write.csv(Pred.Discr2020H$class, "D:/stageCESURE-Islande/Statistic_study/R analysis shape - project 1/DFAbodyclass2020head.csv") 

### -- Matrix of confusion 2020 ----

### Matrix of confusion : I will see how good is the DFA classification
# https://www.journaldev.com/46732/confusion-matrix-in-r

### Here a dataframe if I want to visualize the difference for each fish
df_matrix_confusion2020H <- data.frame(expected=ID2020$Location,predicted=Pred.Discr2020H$class)

### Here the values
# Creates vectors having data points
expected_value2020H <- factor(ID2020$Location)
predicted_value2020H <- factor(Pred.Discr2020H$class)

### Creating confusion matrix
# Accuracy = 0.6 (Accuracy is calculated as the total number of two correct predictions divided by the total number of a dataset) (https://classeval.wordpress.com/introduction/basic-evaluation-measures/)
# Here I can see that the DFA has some difficulties
Matrix_confusion_DFA2020H <- caret::confusionMatrix(data=predicted_value2020H, reference = expected_value2020H)
Matrix_confusion_DFA2020H


### -- Matrix of confusion 2020 by genetic cluster ----

### Caves by Genetic cluster
# H-C : C7, C12 and C25 
# H-N : C5,C6, C10, C11, C27
# H-S : C1, C2, C26
# V-E : C21, C22, C23, C24
# V-W : C17, C17B, C18, C19, C20

### Convert Pred.Discr2H$class from cave to genetic cluster
# If I use merge, I will change the order so I do a little program
Pred_GCbodyH2020 <- ID$Genetic.Cluster[1:415]
for (i in 1:length(Pred.Discr2020H$class)){
  if ((Pred.Discr2020H$class[i]=="C7")||(Pred.Discr2020H$class[i]=="C12")||(Pred.Discr2020H$class[i]=="C25")){
    Pred_GCbodyH2020[i] <- "H-C"
  }
  if ((Pred.Discr2020H$class[i]=="C5")||(Pred.Discr2020H$class[i]=="C6")||(Pred.Discr2020H$class[i]=="C10")||(Pred.Discr2020H$class[i]=="C11")||(Pred.Discr2020H$class[i]=="C27")){
    Pred_GCbodyH2020[i] <- "H-N"
  }
  if ((Pred.Discr2020H$class[i]=="C1")||(Pred.Discr2020H$class[i]=="C2")||(Pred.Discr2020H$class[i]=="C26")){
    Pred_GCbodyH2020[i] <- "H-S"
  }
  if ((Pred.Discr2020H$class[i]=="C21")||(Pred.Discr2020H$class[i]=="C22")||(Pred.Discr2020H$class[i]=="C23")||(Pred.Discr2020H$class[i]=="C24")){
    Pred_GCbodyH2020[i] <- "V-E"
  }
  if ((Pred.Discr2020H$class[i]=="C17")||(Pred.Discr2020H$class[i]=="C17B")||(Pred.Discr2020H$class[i]=="C18")||(Pred.Discr2020H$class[i]=="C19")||(Pred.Discr2020H$class[i]=="C20")){
    Pred_GCbodyH2020[i] <- "V-W"
  }
}

expected_valueH_GCbody2020 <- factor(ID$Genetic.Cluster[1:415])
predicted_valueH_GCbody2020 <- factor(Pred_GCbodyH2021)

Matrix_confusion_DFAH_GCbody2020 <- caret::confusionMatrix(data=predicted_valueH_GCbody2020, reference = expected_valueH_GCbody2020)
Matrix_confusion_DFAH_GCbody2020
### -- YEAR 2021 ----

### Need to change the landmarks to a two-dimension form fro DFA
# 1 line = 1 fish, and 22*2 columns for the coordinates (X then Y) of each landmarks
y2021H<-two.d.array(HDatafr.gdf2021$coords)
### Here I make the matrix in to a data frame - needed for the discriminant analysis. Discriminant analysis needs the package MASS
# So same as y2
z2021H<-as.data.frame(y2021H)

### discriminant analysis
# give prior probabilities of groups (2% C1, 3% C10...)
# give mean of landmarks for each group
# then give coefficients of linear discriminants, from LD1 to LD19
# and proportion of trace for each LD
Discr2021H <- lda(formula = HDatafr.gdf2021$caves~., data = z2021H)
Discr2021H

### Calculates propabilities for each axis of the distribution
likur2021H <- Discr2021H$svd^2/sum(Discr2021H$svd^2)
likur2021H

# Here I find classification and the axis scores and print them in a file in the directory 
Pred.Discr2021H<- predict(Discr2021H) # computes LD scores, coefficients, etc. 
write.csv(Pred.Discr2021H$class, "D:/stageCESURE-Islande/Statistic_study/R analysis shape - project 1/DFAbodyclass2021head.csv") 

### -- Matrix of confusion 2021 ----

### Matrix of confusion : I will see how good is the DFA classification
# https://www.journaldev.com/46732/confusion-matrix-in-r

### Here a dataframe if I want to visualize the difference for each fish
df_matrix_confusion2021H <- data.frame(expected=ID2021$Location,predicted=Pred.Discr2021H$class)

### Here the values
# Creates vectors having data points
expected_value2021H <- factor(ID2021$Location)
predicted_value2021H <- factor(Pred.Discr2021H$class)

### Creating confusion matrix
# Accuracy = 0.8 (Accuracy is calculated as the total number of two correct predictions divided by the total number of a dataset) (https://classeval.wordpress.com/introduction/basic-evaluation-measures/)
# Here I can see that the DFA has some difficulties:
# - 5 of reference C12 were predicted as C25
# For C12, it can be explained by the fact that it is in the same genetic clusters as C25
Matrix_confusion_DFA2021H <- caret::confusionMatrix(data=predicted_value2021H, reference = expected_value2021H)
Matrix_confusion_DFA2021H

### -- Matrix of confusion 2021 by genetic cluster ----

### Caves by Genetic cluster
# H-C : C7, C12 and C25 
# H-N : C5,C6, C10, C11, C27
# H-S : C1, C2, C26
# V-E : C21, C22, C23, C24
# V-W : C17, C17B, C18, C19, C20

### Convert Pred.Discr2H$class from cave to genetic cluster
# If I use merge, I will change the order so I do a little program
Pred_GCbodyH2021 <- ID$Genetic.Cluster[416:825]
for (i in 1:length(Pred.Discr2021H$class)){
  if ((Pred.Discr2021H$class[i]=="C7")||(Pred.Discr2021H$class[i]=="C12")||(Pred.Discr2021H$class[i]=="C25")){
    Pred_GCbodyH2021[i] <- "H-C"
  }
  if ((Pred.Discr2021H$class[i]=="C5")||(Pred.Discr2021H$class[i]=="C6")||(Pred.Discr2021H$class[i]=="C10")||(Pred.Discr2021H$class[i]=="C11")||(Pred.Discr2021H$class[i]=="C27")){
    Pred_GCbodyH2021[i] <- "H-N"
  }
  if ((Pred.Discr2021H$class[i]=="C1")||(Pred.Discr2021H$class[i]=="C2")||(Pred.Discr2021H$class[i]=="C26")){
    Pred_GCbodyH2021[i] <- "H-S"
  }
  if ((Pred.Discr2021H$class[i]=="C21")||(Pred.Discr2021H$class[i]=="C22")||(Pred.Discr2021H$class[i]=="C23")||(Pred.Discr2021H$class[i]=="C24")){
    Pred_GCbodyH2021[i] <- "V-E"
  }
  if ((Pred.Discr2021H$class[i]=="C17")||(Pred.Discr2021H$class[i]=="C17B")||(Pred.Discr2021H$class[i]=="C18")||(Pred.Discr2021H$class[i]=="C19")||(Pred.Discr2021H$class[i]=="C20")){
    Pred_GCbodyH2021[i] <- "V-W"
  }
}

expected_valueH_GCbody2021 <- factor(ID$Genetic.Cluster[416:825])
predicted_valueH_GCbody2021 <- factor(Pred_GCbodyH2021)

Matrix_confusion_DFAH_GCbody2021 <- caret::confusionMatrix(data=predicted_valueH_GCbody2021, reference = expected_valueH_GCbody2021)
Matrix_confusion_DFAH_GCbody2021
### ---------------- 2020 and 2021 PCA general Analysis (head) -----------

### Here we do like before, just splitting in 2 years so not a lot of comment really needed

### -- YEAR 2020 ----

### Creation of the PCA
PCA2020H<-gm.prcomp(HDatafr.gdf2020$coords)
### The PCA scores/ cumulative proportions :
summary(PCA2020H)
PCA2020H$d[1]/sum(PCA2020H$d) # Eigenvalue for the first principal component
PCA2020H$d[2]/sum(PCA2020H$d)
plot(PCA2020H$d) # Eigenvalues plot
### Plot PCA
plot(PCA2020H,main = "2020 PCA body (head)", pch=21,bg=HDatafr.gdf2020$caves) # here by cave

### -- YEAR 2021 ----

### Creation of the PCA
PCA2021H<-gm.prcomp(HDatafr.gdf2021$coords)
### The PCA scores/ cumulative proportions :
summary(PCA2021H)
PCA2021H$d[1]/sum(PCA2021H$d) # Eigenvalue for the first principal component
PCA2021H$d[2]/sum(PCA2021H$d)
plot(PCA2021H$d) # Eigenvalues plot
### Plot PCA
plot(PCA2021H,main = "2021 PCA body (head)", pch=21,bg=HDatafr.gdf2021$caves) # here by cave

### ---------------- 2020 and 2021 Plotting DFA (head NEW)-----------------------------

### -- YEAR 2020 ----

# Now to the figures for DFA

### Here I calculate the mean of each cave and create new variables
# This is the mean shape in each cave (mean coordinates along dfa1 and dfa2)
Df1b2020H<-tapply(Pred.Discr2H$x[,1][1:415], HDatafr.gdf2020$caves, mean )
Df2b2020H<-tapply(Pred.Discr2H$x[,2][1:415], HDatafr.gdf2020$caves, mean )

### R does not have an easy way to calculate standard error - lets teach it
st.err <- function(x) {  +     sd(x)/sqrt(length(x)) }
# Now we can calculate standard errors
Df1SE2020H<- tapply(Pred.Discr2H$x[,1][1:415], HDatafr.gdf2020$caves, st.err)
Df2SE2020H<- tapply(Pred.Discr2H$x[,2][1:415], HDatafr.gdf2020$caves, st.err)

DF2020H<-data.frame(Df1=Df1b2020H,Df2=Df2b2020H, SE1=Df1SE2020H, SE2= Df2SE2020H, Labels=Caves.Right.Order.Genetic.Cluster, gene=Genetic.Cluster.Linked)
DF2020H$gene<-as.factor(DF2020H$gene)

ggplot(DF2020H, aes(x=Df1,y=Df2, color=gene)) + geom_point(aes(Df1, Df2)) +  geom_errorbar(aes(ymin=Df2-SE2, ymax=Df2+SE2)) +geom_errorbarh(aes(xmin=Df1-SE1, xmax=Df1+SE1))+ geom_text(aes(label=Labels), size=3, hjust=-1) + xlab("Discriminant Axis 1 (27.27%)") + ylab("Discriminant Axis 2 (21.83%)")+ labs(title="Discriminant Function Analysis 2020(head)")+ theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) +labs(color="Genetic Clusters") +theme(plot.title = element_text(hjust = 0.5))

# Now we need to create the deformation grids - package geomorph - NOTE that we set the extreme of the axis here. 
#If we want magnification, we have to increase these numbers
DF1pred2020H<-shape.predictor(HDatafr.gdf2020$coords,Pred.Discr2H$x[,1][1:415], Intercept=FALSE, pred1=-1, pred2=1 )
DF2pred2020H<-shape.predictor(HDatafr.gdf2020$coords,Pred.Discr2H$x[,2][1:415], Intercept=FALSE, pred1=-1, pred2=1 )
Mean_morph2020H<-mshape(HDatafr.gdf2020$coords) # Here we find the mean morphology or assign mean shape for use with plotRefToTarget below
# Now we compare shape at extreme of axis to mean morphology
plotRefToTarget(Mean_morph2020H, DF1pred2020H$pred1, mag= 3) #min DF1 , can change magnification 
plotRefToTarget(Mean_morph2020H, DF1pred2020H$pred2, mag= 3) #max DF1
plotRefToTarget(Mean_morph2020H, DF2pred2020H$pred1, mag= 3) #min DF2
plotRefToTarget(Mean_morph2020H, DF2pred2020H$pred2, mag= 3) #max DF2
# Same with vector
plotRefToTarget(Mean_morph2020H, DF1pred2020H$pred1, method="vector", mag= 3) #min DF1 , can change magnification 
plotRefToTarget(Mean_morph2020H, DF1pred2020H$pred2, method="vector", mag= 3) #max DF1
plotRefToTarget(Mean_morph2020H, DF2pred2020H$pred1, method="vector", mag= 3) #min DF2
plotRefToTarget(Mean_morph2020H, DF2pred2020H$pred2, method="vector", mag= 3) #max DF2

### -- YEAR 2021 ----

# Now to the figures for DFA

### Here I calculate the mean of each cave and create new variables
# This is the mean shape in each cave (mean coordinates along dfa1 and dfa2)
Df1b2021H<-tapply(Pred.Discr2H$x[,1][416:825], HDatafr.gdf2021$caves, mean )
Df2b2021H<-tapply(Pred.Discr2H$x[,2][416:825], HDatafr.gdf2021$caves, mean )

### R does not have an easy way to calculate standard error - lets teach it
st.err <- function(x) {  +     sd(x)/sqrt(length(x)) }
# Now we can calculate standard errors
Df1SE2021H<- tapply(Pred.Discr2H$x[,1][416:825], HDatafr.gdf2021$caves, st.err)
Df2SE2021H<- tapply(Pred.Discr2H$x[,2][416:825], HDatafr.gdf2021$caves, st.err)

DF2021H<-data.frame(Df1=Df1b2021H,Df2=Df2b2021H, SE1=Df1SE2021H, SE2= Df2SE2021H, Labels=Caves.Right.Order.Genetic.Cluster, gene=Genetic.Cluster.Linked)
DF2021H$gene<-as.factor(DF2021H$gene)

ggplot(DF2021H, aes(x=Df1,y=Df2, color=gene)) + geom_point(aes(Df1, Df2)) +  geom_errorbar(aes(ymin=Df2-SE2, ymax=Df2+SE2)) +geom_errorbarh(aes(xmin=Df1-SE1, xmax=Df1+SE1))+ geom_text(aes(label=Labels), size=3, hjust=-1) + xlab("Discriminant Axis 1 (27.27%)") + ylab("Discriminant Axis 2 (21.83%)")+ labs(title="Discriminant Function Analysis 2021 (head)")+ theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) +labs(color="Genetic Clusters") +theme(plot.title = element_text(hjust = 0.5))


# Now we need to create the deformation grids - package geomorph - NOTE that we set the extreme of the axis here. 
#If we want magnification, we have to increase these numbers
DF1pred2021H<-shape.predictor(HDatafr.gdf2021$coords,Pred.Discr2H$x[,1][416:825], Intercept=FALSE, pred1=-1, pred2=1 )
DF2pred2021H<-shape.predictor(HDatafr.gdf2021$coords,Pred.Discr2H$x[,2][416:825], Intercept=FALSE, pred1=-1, pred2=1 )
Mean_morph2021H<-mshape(HDatafr.gdf2021$coords) # Here we find the mean morphology or assign mean shape for use with plotRefToTarget below
# Now we compare shape at extreme of axis to mean morphology
plotRefToTarget(Mean_morph2021H, DF1pred2021H$pred1, mag= 3) #min DF1 , can change magnification 
plotRefToTarget(Mean_morph2021H, DF1pred2021H$pred2, mag= 3) #max DF1
plotRefToTarget(Mean_morph2021H, DF2pred2021H$pred1, mag= 3) #min DF2
plotRefToTarget(Mean_morph2021H, DF2pred2021H$pred2, mag= 3) #max DF2
# Same for vector
plotRefToTarget(Mean_morph2021H, DF1pred2021H$pred1, method="vector", mag= 3) #min DF1 , can change magnification 
plotRefToTarget(Mean_morph2021H, DF1pred2021H$pred2, method="vector", mag= 3) #max DF1
plotRefToTarget(Mean_morph2021H, DF2pred2021H$pred1, method="vector", mag= 3) #min DF2
plotRefToTarget(Mean_morph2021H, DF2pred2021H$pred2, method="vector", mag= 3) #max DF2

### -- putting YEAR 2020 2021 in 1 graph (head) ----

DF2021_newH<-data.frame(Df1=Df1b2021H,Df2=Df2b2021H, SE1=Df1SE2021H, SE2= Df2SE2021H, Labels=Caves.Right.Order.Genetic.Cluster.2021, gene=Genetic.Cluster.Linked)
DF2021_newH$gene<-as.factor(DF2021_newH$gene)

### I melt the years 2020 and 2021
DF2020_2021H <- rbind(DF2020H,DF2021_newH)

ggplot(DF2020_2021H, aes(x=Df1,y=Df2, color=gene)) + geom_point(aes(Df1, Df2)) +  geom_errorbar(aes(ymin=Df2-SE2, ymax=Df2+SE2)) +geom_errorbarh(aes(xmin=Df1-SE1, xmax=Df1+SE1))+ geom_text(aes(label=Labels), size=4, hjust=-1) + xlab("Discriminant Axis 1 (27.27%)") + ylab("Discriminant Axis 2 (21.83%)")+ labs(title="Discriminant Function Analysis 2020-2021 (head)")+ theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) +labs(color="Genetic Clusters") +theme(plot.title = element_text(hjust = 0.5))


### ---------------- 2020 and 2021 Graphing PCA, mean of each cave (head NEW)------------------

### Here we do like before, just splitting in 2 years so no a lot of comment really needed

### -- YEAR 2020 ----

### Average shape, we visualize average shape in each cave and for each year along the two PCA axes that explained most variation.
PC1b2020H<-tapply(PCA2H$x[,1][1:415], HDatafr.gdf2020$caves, mean )
PC2b2020H<-tapply(PCA2H$x[,2][1:415], HDatafr.gdf2020$caves, mean )
PC1SE2020H<-tapply(PCA2H$x[,1][1:415], HDatafr.gdf2020$caves, st.err )
PC2SE2020H<-tapply(PCA2H$x[,2][1:415], HDatafr.gdf2020$caves, st.err )

PCb2020H<-data.frame(PC1=PC1b2020H,PC2=PC2b2020H, SE1=PC1SE2020H, SE2= PC2SE2020H, Labels=Caves.Right.Order.Genetic.Cluster, gene=Genetic.Cluster.Linked)
PCb2020H$gene<-as.factor(PCb2020H$gene)
ggplot(PCb2020H, aes(x=PC1,y=PC2, color=gene)) + geom_point(aes(PC1, PC2)) +  geom_errorbar(aes(ymin=PC2-SE2, ymax=PC2+SE2)) +geom_errorbarh(aes(xmin=PC1-SE1, xmax=PC1+SE1))+ geom_text(aes(label=Labels), size=3, hjust=-1) + xlab("PCA 1 (26.98%)") + ylab("PCA 2 (19.72%)")+ labs(title="2020 PCA Analysis (head)")+ theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) +labs(color="Genetic Clusters") +theme(plot.title = element_text(hjust = 0.5))

# Now we need to create the deformation grids - package geomorph - ATH:  we set the extreme of the axes here with pred1 and pred2 for each axis, look at the min and max values of PCA plot 
#If we want magnification, we have to increase these numbers, 
PCA1pred2020H<-shape.predictor(HDatafr.gdf2020$coords,PCA2H$x[,1][1:415],Intercept=FALSE, pred1=-0.02, pred2=0.02)
PCA2pred2020H<-shape.predictor(HDatafr.gdf2020$coords,PCA2H$x[,2][1:415],Intercept=FALSE, pred1=-0.02, pred2=0.02)
Mean_morph2020H<-mshape(HDatafr.gdf2020$coords) # Here we find the mean morphology or assign mean shape for use with plotRefToTarget below
# Now we compare shape at extreme of axis to mean morpholog using deformation grids
plotRefToTarget(Mean_morph2020H, PCA1pred2020H$pred1, mag=3) #min PC1 , can change magnification 
plotRefToTarget(Mean_morph2020H, PCA1pred2020H$pred2, mag=3) #max PC1
plotRefToTarget(Mean_morph2020H, PCA2pred2020H$pred1, mag=3) #min PC2
plotRefToTarget(Mean_morph2020H, PCA2pred2020H$pred2, mag=3) #max PC2
# similar but we see shape change with vector for each LM, can help to describe what the changes are along the PCA axes: 
plotRefToTarget(Mean_morph2020H, PCA1pred2020H$pred1, method="vector",mag=2) #min PC1 , can change magnification
plotRefToTarget(Mean_morph2020H, PCA1pred2020H$pred2, method="vector",mag=2) #max PC1
plotRefToTarget(Mean_morph2020H, PCA2pred2020H$pred1, method="vector",mag=2) #min PC2
plotRefToTarget(Mean_morph2020H, PCA2pred2020H$pred2, method="vector",mag=2) #max PC2

### -- YEAR 2021 ----

### Average shape, we visualize average shape in each cave and for each year along the two PCA axes that explained most variation.
PC1b2021H<-tapply(PCA2H$x[,1][416:825], HDatafr.gdf2021$caves, mean )
PC2b2021H<-tapply(PCA2H$x[,2][416:825], HDatafr.gdf2021$caves, mean )
PC1SE2021H<-tapply(PCA2H$x[,1][416:825], HDatafr.gdf2021$caves, st.err )
PC2SE2021H<-tapply(PCA2H$x[,2][416:825], HDatafr.gdf2021$caves, st.err )

PCb2021H<-data.frame(PC1=PC1b2021H,PC2=PC2b2021H, SE1=PC1SE2021H, SE2= PC2SE2021H, Labels=Caves.Right.Order.Genetic.Cluster, gene=Genetic.Cluster.Linked)
PCb2021H$gene<-as.factor(PCb2021H$gene)
ggplot(PCb2021H, aes(x=PC1,y=PC2, color=gene)) + geom_point(aes(PC1, PC2)) +  geom_errorbar(aes(ymin=PC2-SE2, ymax=PC2+SE2)) +geom_errorbarh(aes(xmin=PC1-SE1, xmax=PC1+SE1))+ geom_text(aes(label=Labels), size=3, hjust=-1) + xlab("PCA 1 (26.98%)") + ylab("PCA 2 (19.72%)")+ labs(title="2021 PCA Analysis (head)")+ theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) +labs(color="Genetic Clusters") +theme(plot.title = element_text(hjust = 0.5))

# Now we need to create the deformation grids - package geomorph - ATH:  we set the extreme of the axes here with pred1 and pred2 for each axis, look at the min and max values of PCA plot 
#If we want magnification, we have to increase these numbers, 
PCA1pred2021H<-shape.predictor(HDatafr.gdf2021$coords,PCA2H$x[,1][416:825],Intercept=FALSE, pred1=-0.02, pred2=0.01)
PCA2pred2021H<-shape.predictor(HDatafr.gdf2021$coords,PCA2H$x[,2][416:825],Intercept=FALSE, pred1=-0.02, pred2=0.01)
Mean_morph2021H<-mshape(HDatafr.gdf2021$coords) # Here we find the mean morphology or assign mean shape for use with plotRefToTarget below
# Now we compare shape at extreme of axis to mean morpholog using deformation grids
plotRefToTarget(Mean_morph2021H, PCA1pred2021H$pred1, mag=3) #min PC1 , can change magnification 
plotRefToTarget(Mean_morph2021H, PCA1pred2021H$pred2, mag=3) #max PC1
plotRefToTarget(Mean_morph2021H, PCA2pred2021H$pred1, mag=3) #min PC2
plotRefToTarget(Mean_morph2021H, PCA2pred2021H$pred2, mag=3) #max PC2
# similar but we see shape change with vector for each LM, can help to describe what the changes are along the PCA axes: 
plotRefToTarget(Mean_morph2021H, PCA1pred2021H$pred1, method="vector",mag=3) #min PC1 , can change magnification
plotRefToTarget(Mean_morph2021H, PCA1pred2021H$pred2, method="vector",mag=3) #max PC1
plotRefToTarget(Mean_morph2021H, PCA2pred2021H$pred1, method="vector",mag=3) #min PC2
plotRefToTarget(Mean_morph2021H, PCA2pred2021H$pred2, method="vector",mag=3) #max PC2

### -- putting YEAR 2020 2021 in 1 graph ----

PCb2021_newH<-data.frame(PC1=PC1b2021H,PC2=PC2b2021H, SE1=PC1SE2021H, SE2= PC2SE2021H, Labels=Caves.Right.Order.Genetic.Cluster.2021, gene=Genetic.Cluster.Linked)
PCb2021_newH$gene<-as.factor(PCb2021_newH$gene)

### I melt the years 2020 and 2021
PCb2020_2021H <- rbind(PCb2020H,PCb2021_newH)

ggplot(PCb2020_2021H, aes(x=PC1,y=PC2, color=gene)) + geom_point(aes(PC1, PC2)) +  geom_errorbar(aes(ymin=PC2-SE2, ymax=PC2+SE2)) +geom_errorbarh(aes(xmin=PC1-SE1, xmax=PC1+SE1))+ geom_text(aes(label=Labels), size=4, hjust=-1) + xlab("PCA 1 (27.89%)") + ylab("PCA 2 (22.19%)")+ labs(title="2021 2020 PCA Analysis (head)")+ theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) +labs(color="Genetic Clusters") +theme(plot.title = element_text(hjust = 0.5))

#####################################
### MEETING #
#####################################

### --------- meeting 12/11/2021  ---------------------
### landmarks : 
# - remove number 1 and 20 for those who have open mouth
# - then remplace missed landmarks (with average), using morph.data<-estimate.missing(morph.data, method = "TPS")
# - change : 
# number 20 (put at the posterior point of maxilla, middle not top)
# 7 and 10 (Posterior and Ventral insertion of the caudal fin, more right)
# 8 (Posterior point of the hypural bone at the lateral midline)

### Csize :
# - check if it's correlated with fork lenght, and if it's the case, we don't need both
# - here not a good indicator si not a problem

### Outliers :
# - not forget to use arrow to see all the graph !
# - in the graph, good proportion
# - vectors on the pictures tell where specimen is different from the rest
# - unbend the tps file with tps util program and then see if there are as many outliers as now. 

### procD.lm :
# - ok for qualitative or quantitative variation
# - iter = iteration
# - the order of the model matters
# - in the models :
# Rsq = R square show % of variation of data explained by the factor. 
# Rsq = 0.08972 = 9% for log(Csize). Not a lot, so we can't have log(Csize), it would bring a wrong interpretation
# Rsq = 0.11964 = 12% for caves for the model coords~caves+year+caves*year. We keep that model.

### aov()
# - also interesting

### to do list for now:
# 1) change the landmark positions
# 2) unbend the tps file

### --------- meeting 19/11/2021  ---------------------
### outliers:
# - count the number (how many, what the proportion %)
# - group them by cave and then by year, i.e. are they spread out in the data set ?
# - see which fishes are outliers? Fork length for example,a re many small fish outliers? or dead fish? see line below
# - check the size of the outliers? Maybe it can be a variation (maybe a lot of small fishes)
# - on the graph logCsize~caves:year and logCsize~loglength = 2 very odd outliers !
#     - check what are these 2 outliers, checking with logCsize

### Loglength and logCsize
# - there is a positive correlation (R2=0.2; P<0.001)  but not that high. it means that I can not have the two measures of size in the same model 
# for the moment I chose to keep fork length (log2 FL)

### Model selection: (called manual backward selection as you start with the full model and drop term as you go)
# - full model is logFL*cave*year based on my question and hypothesis...
# - (I expect to have an effect of body size, cave and year on body shape and I expect that those effects may vary across years and cave, therefore I have an interaction of the 3 factors).
# - what is results of this model, can I simpify the model based on removel of non significant terms? in your case the next try is to cretae models with all two terms interactions, and drop 1 way interaction that are not significant. Compare models with an f test
# - once you have a model, check assumptions are met, and distrivution of the residuals. 

# - then try the equivalent model without size:

### PCA and DFA (= visualisation of the body shape change , above was only a statistical test) 
# - PCA: 
# explore PCA script and see if you follow and understand the steps,
# average shape, we visualize average shape in each cave and for each year along the two PCA axes that explained most variation.  
#  maybe good to create a plot for 2020 and a plot for 2021 at first to visualise

# DFA :  Discriminant function analysis or linear discriminant analysis, allows to visualise shape chnage based on grouping/classfying variable
## read about DFA test (general knowledge), read a few papers using LDA/DFA with geometric morphometric paper (rainville et al. 2021) and more that you can find. 
#     - explore the script and run classification, put groups (grp= cave or grp= year)
#     - see how many fish can be correclty classified based on shape. (we can later look at which fish are nt well classified, small large one , etc....)


### Unbending / fish orientation, see why your tps file has fish facing rigth and not left. this will not change the statistical results but your visualisation will be wrong. 
# - fix this in tpsdig!

### --------- meeting 07/12/2021  ---------------------

### For the 2nd project
# - it would be good to rename pictures like it has been done for the brains

### In the script
# - the part "Check if it is into the right format" to do shorter
# - the part "Outliers grouped by cave and by year" to delete

### We keep the outliers, no worry about it

### Correlation
# - It has been done with log2 of length and Csize. Not a strong correlation. 
# - Is it better with just log ? I need to check

### Boxplot of the effect of cave x year on log2Csize really relevant
# - so we keep the Csize model instead of the length one :  coords~logCsize+caves+year+logCsize*caves+caves*year
# - I need to check it again and understand better the problem why the interaction logCsize:year is not present in the best model, even though it's relevant in the boxplot
# - it's good to see that it has kept the interactions with cave factor
# - The QQplot and residualplot of this model are ok

### PCA / DFA
# - need to add the 2 years in the same graph
# - DFA : it groups the similar shapes. We see that genetic factors explain a lot of the variations

### What I have to do until Friday afternoon in the following order:
# - 1) add a 3rd landmarks for unbending
# - 2) Check the correlation with log instead of log2 
# - 3) See if the DFA is correct with a matrix
# - 4) for PCA and DFA, add the 2 years in the same graph with arrow if possible, if not, 2 graphs next to each other
# - 5) Just do the same for the head
# - 6) group all the pictures + models relevant for the next meeting

### --------- meeting 10/12/2021  ---------------------

### QUestions for Bjarni
# - Is the new model ok?
# - Outliers : what do we do with them?
# - With the correlation between logCsize and loglength, which one we keep for the model?

### For the 1st project:
# - Do all the analysis again with a different unbending (18,21,8,9)

### For the 2nd project:
# - we choose C10
# - study morphology depending of time and environment
# - The 1st step: see when we have data on each fish, including size
# And think about renaming pictures
# Then pictures and landmarks

### Idea to vizualise
# - create vectors for each individual

### To read for the 2nd project:
# - Rainville 2021

### --------- meeting 13/12/2021  ---------------------

### log² = see more the reason why we use that

### On boxplot, Csize much lower in a year, for body and head
# - some kind of condition business ?
# - not human because I am the only one putting landmarks in these
# - try that head analysis without LM 14.

### Model : try coords~caves+year+cave:year
# - The shape not exactly change the same for each cave
# - Is the year significant?

### Boxplot
# - Color the boxplot according to their genetic cluster to see if we have something with it

### Regression Csize / length
# - we see a huge difference between the 2 years : was 2021 a bad year for the fish?

### Matrix of confusion
# - Big error between some caves that are in the same genetic cluster
# - Maybe a very good assignement with a DFA based on genetic clusters

### PCA : error in % of PC1 and PC2 that need to be checked again

### Pred : Error in magnification and choosing of pred1 and pred2

### When we split 2 years, we split after the gpagen to be sure they can be compared

### --------- meeting 17/12/2021  ---------------------

### csv file project 2
# - some individual are seen twice because the tag changed
# - some are maybe retaged?

### Powerpoint
# - 15) axis 1 : change in head / caudal
# - 16) PC2 a little bit not well unbend. Another way to unbend = regression between PC and LM
# - For DFA and PCA 2020 2021, do the analysis together and then separate in the graph
#   (with factor = year)
# - for PCA, maybe look at PC3 and PC4 because PC2 describe bending, if the % is not too low
# - difference between standart error and deviation error, we use std error

### --------- meeting 22/12/2021  ---------------------

### DFA 2020 2021 head : purple & blue don't spread out (Vindlbelgur)

### Correlation?
# - between PC1,PC2,PC3,PC4 and FL, Csize ?
# - graph + test to do !

### Matrix of confusion
# - add % of proper classification /caves + /genetic cluster

### Graphs PCA DFA
# - add vector or augment magnification to study them
# - label more tiny, arrow 
# - PCA head : DF2 opercule, DF1 head
# - do PC3 and PC4 for 2020 and 2021

### Boxplot
# - Check statistical test with genetic cluster instead of cave : should not have a significant cluster
