##Library
library(asnipe)
library(ggplot2)
library(datawizard)
library(lme4)
library(lmerTest)
library(brms)
library(tidybayes)
library(dbplyr)
library(rstan)
library(devtools)
library(rethinking)
library(car)
library(rptR)
library(data.table)
library(psych)
library(corrplot)
library(report)

##########
######ORDER OF ARRIVAL ON FEEDERS  (ALSO PART OF SCENARIO 2)


##Preparation of the data
load("data/gmm.summer.RData")
net.data.summer <- read.delim("data/Mill.data.summer.txt", sep=" ", row.names = 1)
head(net.data.summer)

gmm.summer$metadata$Location2 <- substr(gmm.summer$metadata$Location, 8, 13)

# for each group, we have the start and end time and a location
# we add a new column called 'group' that will be filled by using a function
net.data.summer[, "group"] <- NA
head(net.data.summer)


for(i in 1:length(gmm.summer$metadata$Start)){ # for each start time
  i.loc <- gmm.summer$metadata$Location2[i] # we extract the location
  rows.i <-  rownames(subset(net.data.summer, 
                             net.data.summer$Date.Time>=gmm.summer$metadata$Start[i] &
                               net.data.summer$Date.Time<=gmm.summer$metadata$End[i] &
                               net.data.summer$location == gmm.summer$metadata$Location2[i]))
  net.data.summer[rows.i, "group"] <- i
  
}

net.data.summer[which(is.na(net.data.summer$group)),]

length(which(is.na(net.data.summer$group)))
# SW: 2397 observations are not part of a group. Perhaps these were birds that arrived before I officially started the observations? Or then they were dropped for some reason by the gmm function? I might need to rerun the gmm function to see what's going on there. 

# for now, we subset it to exclude those
net.data.summer <- subset(net.data.summer, !is.na(net.data.summer$group))

length(unique(net.data.summer$group))
# 9913 groups remain

# make sure it's in correct order (first ordered by groups, then time)
net.data.summer <- net.data.summer[with(net.data.summer, order(group, Date.Time)), ]

# we create a new column called 'order' all filled with NA
net.data.summer$order <- NA
# to the very first visitor, we assign a 1
net.data.summer$order[1] <- 1
# now we loop through the entire data frame
for(i in 2:length(net.data.summer$group)){
  # we extract the PIT tag and the group number of bird i and the bird and group number that came just before
  group.i <- net.data.summer$group[i]
  ID.i <- net.data.summer$PIT[i]
  group.i_minus1 <- net.data.summer$group[i-1]
  ID.i_minus1 <- net.data.summer$PIT[i-1]
  
  # we also don't want to assign a new order number, if the bird had already arrived as part of the current group, so we extract the PIT tags that are already part of the current group
  sub <- net.data.summer[1:(i-1),]
  sub <- subset(sub, sub$group==group.i)
  already.present <- unique(sub$PIT)
  
  if(ID.i %in% already.present){ # if the bird hard already arrived as part of the current group
    net.data.summer$order[i] <- 'already.present' # we assign 'already.present'
  } else if(group.i==group.i_minus1 & ID.i!=ID.i_minus1){
    # if it's the same group but a new bird, we add the order+1
    net.data.summer$order[i] <- max(as.numeric(na.omit(sub$order[sub$order != "already.present"])))+1
  } else if(group.i!=group.i_minus1){
    # if it's a new group, we start over with the order=1
    net.data.summer$order[i] <- 1
  }
}


# we remove the rows that contain 'already present' to only have each bird in each group once
net.data.summer <- subset(net.data.summer, !(is.na(net.data.summer$order)) & net.data.summer$order != "already.present")

head(net.data.summer, 40)
net.data.summer$order <- as.numeric(net.data.summer$order)
max(net.data.summer$order)
hist(net.data.summer$order)

# save it as the object with the order
save(net.data.summer, file="data/net.data.summer.w.order.RData")
load("data/net.data.summer.w.order.RData")

##Take the same data but for each season (the part above has been used to get the following objects)
#NB: I don't have the spring data (in the data file on Github)
# data
net.data.autumn <- read.delim("data/Mill.data.autumn.txt", sep=",", row.names = 1)
View(net.data.autumn)
net.data.summer <- read.delim("data/Mill.data.summer.txt", sep=",", row.names = 1)
net.data.winter <- read.delim("data/Mill.data.winter.txt", sep=",", row.names = 1)

#species_age_sex
load("data/species_age_sex.RDA") 
View(species_age_sex)
save(species_age_sex.RDA, file="species_age_sex.RDA") 
load("data/species_age_sex.RDA")
head(species_age_sex)

#order data
load("data/gmm.autumn.RData")
head(gmm.autumn) 
load("data/gmm.summer.RData")
head(gmm.summer)
load("data/gmm.winter.RData")
head(gmm.winter) 
load("data/net.data.summer.w.order.RData")
bd <- load("C:/Documents d'Emilie/Sonja Wild/R code/Great-Tits/data/net.data.summer.w.order.RData")
bd # it says "net.data.summer" --> the name is no longer net.data.summer.w.order

head(net.data.summer) 
load("data/net.data.autumn.w.order.RData")
bload("C:/Documents d'Emilie/Sonja Wild/R code/Great-Tits/data/net.data.autumn.w.order.RData")
head(net.data.autumn)
load("data/net.data.winter.w.order.RData")
load("C:/Documents d'Emilie/Sonja Wild/R code/Great-Tits/data/net.data.winter.w.order.RData")
head(net.data.winter) 


#make a dataframe (for each season)
#summer
sp_class_summ <- merge(net.data.summer, species_age_sex, by.x= "PIT")
str(sp_class_summ$species)
sp_class_summ$species <- as.factor(sp_class_summ$species)
sp_class_summ$age_in_2020 <- as.factor(sp_class_summ$age_in_2020)
#leave out the "already present". 
sp_class_summ$order[sp_class_summ$order == "already.present"] <- 0
sp_class_summ <- subset(sp_class_summ, sp_class_summ$order!=0)

head(sp_class_summ)
str(sp_class_summ$order)
sp_class_summ$order <- as.numeric(sp_class_summ$order)

# SW: You can kick out the COATI and GREFI - COATI are so few and GREFI is most likely a mistake.
sp_class_summ <- subset(sp_class_summ, sp_class_summ$species %in% c("BLUTI", "GRETI", "MARTI", "NUTHA"))

# SW: I'm saving the image here again so I can pick up here next time:
save.image(file="R.image2.RData")
load("R.image2.RData")


#autumn
sp_class_aut <- merge(net.data.autumn, species_age_sex, by.x= "PIT")
head(sp_class_aut)
str(sp_class_aut$species)
sp_class_aut$species <- as.factor(sp_class_aut$species)
sp_class_aut$age_in_2020 <- as.factor(sp_class_aut$age_in_2020)
sp_class_aut$order[sp_class_aut$order == "already.present"] <- 0
sp_class_aut <- subset(sp_class_aut, sp_class_aut$order!=0)
sp_class_aut <- subset(sp_class_aut, sp_class_aut$species %in% c("BLUTI", "GRETI", "MARTI", "NUTHA"))
head(sp_class_aut)
str(sp_class$order)
sp_class_aut$order <- as.numeric(sp_class_aut$order)


#winter
sp_class_wint <- merge(net.data.winter, species_age_sex, by.x= "PIT")
head(sp_class_wint)
sp_class_wint$species <- as.factor(sp_class_wint$species)
sp_class_wint$age_in_2020 <- as.factor(sp_class_wint$age_in_2020)
sp_class_wint$order[sp_class_wint$order == "already.present"] <- 0
sp_class_wint <- subset(sp_class_wint, sp_class_wint$order!=0)
sp_class_wint <- subset(sp_class_wint, sp_class_wint$species %in% c("BLUTI", "GRETI", "MARTI", "NUTHA"))
head(sp_class_wint)
str(sp_class_wint$order)
str(sp_class_wint$species)
sp_class_wint$order <- as.numeric(sp_class_wint$order)

## data with all seasons together
library(dplyr)
sp_class_summ$season <- "summer"
sp_class_aut$season <- "autumn"
sp_class_wint$season <- "winter"
sp_class_season <- rbind(sp_class_summ, sp_class_aut, sp_class_wint)
View(sp_class_season)
sp_class_season$species <- as.factor(sp_class_season$species)
sp_class_season$season <- as.factor(sp_class_season$season)
sp_class_season$age_in_2020 <- as.factor(sp_class_season$age_in_2020)
unique(sp_class_season$order)
sp_class_season$order<- as.numeric(sp_class_season$order)
sp_class_season <- subset(sp_class_season, sp_class_season$species %in% c("BLUTI", "GRETI", "MARTI", "NUTHA"))


##Models

model_order_offset <- glmer(order ~species*season + species*age_in_2020 + season*species+ offset(group)+ (1|PIT), family=poisson, data= sp_class_season)
#not the correct use of offset for the moment

#Otherwise with weight, considering the flock size
#Knowing that every individual in one group has been at least once on the feeder
sp_class_season <- sp_class_season %>%
  group_by(group) %>% #"for each group"
  mutate(weight = 1 / n()) #n being the total number of observations within each group. 

model_order_weight <- glmer(order ~ species*age_in_2020 + species*season+ season*age_in_2020 + (1|PIT), family=poisson, weights= weight, data= sp_class_season)
#model failed to converge
drop1(model_order_weight, test="Chisq")
#leave out the non significant interactions
model_order_weight2 <- glmer(order ~ age_in_2020 + species*season + (1|PIT), family=poisson, weights= weight, data= sp_class_season)
summary(model_order_weight2)#model failed to converge

#Random effects:
#Groups Name        Variance Std.Dev.
#PIT    (Intercept) 0.04784  0.2187  
#Number of obs: 25997, groups:  PIT, 269

#Fixed effects:
#                            Estimate Std. Error z value Pr(>|z|)    
#(Intercept)                1.38324    0.14927   9.266  < 2e-16 ***
#age_in_2020juvenile        0.17034    0.03899   4.369 1.25e-05 ***
#speciesGRETI              -0.06816    0.16874  -0.404   0.6863    
#speciesMARTI              -0.46149    0.18820  -2.452   0.0142 *  
#speciesNUTHA              -0.53505    0.24475  -2.186   0.0288 *  
#seasonsummer              -0.70835    0.16259  -4.357 1.32e-05 ***
#seasonwinter               0.88665    0.15109   5.869 4.40e-09 ***
#speciesGRETI:seasonsummer -0.07790    0.17985  -0.433   0.6649    
#speciesMARTI:seasonsummer  0.19104    0.19250   0.992   0.3210    
#speciesNUTHA:seasonsummer  0.31332    0.24293   1.290   0.1971    
#speciesGRETI:seasonwinter -0.07428    0.17148  -0.433   0.6649    
#speciesMARTI:seasonwinter -0.48085    0.19021  -2.528   0.0115 *  
#speciesNUTHA:seasonwinter -0.30128    0.26304  -1.145   0.2521   



