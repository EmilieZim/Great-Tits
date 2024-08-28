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
######ORDER OF ARRIVAL ON FEEDERS 


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
# SW: sorry about that! Just added it
# data
net.data.summer <- read.delim("data/Mill.data.summer.txt", sep=",", row.names = 1)
net.data.autumn <- read.delim("data/Mill.data.autumn.txt", sep=",", row.names = 1)
View(net.data.autumn)
net.data.winter <- read.delim("data/Mill.data.winter.txt", sep=",", row.names = 1)
net.data.spring <- read.delim("data/Mill.data.spring.txt", sep=",", row.names = 1)

#species_age_sex
load("data/species_age_sex.RDA") 
View(species_age_sex)
save(species_age_sex.RDA, file="species_age_sex.RDA") 
load("data/species_age_sex.RDA")
head(species_age_sex)

#order data
load("data/net.data.summer.w.order.RData")
bd <- load("C:/Documents d'Emilie/Sonja Wild/R code/Great-Tits/data/net.data.summer.w.order.RData")
bd # it says "net.data.summer" --> the name is no longer net.data.summer.w.order
head(net.data.summer) 
load("data/net.data.autumn.w.order.RData")
load("C:/Documents d'Emilie/Sonja Wild/R code/Great-Tits/data/net.data.autumn.w.order.RData")
head(net.data.autumn)
load("data/net.data.winter.w.order.RData")
load("C:/Documents d'Emilie/Sonja Wild/R code/Great-Tits/data/net.data.winter.w.order.RData")
head(net.data.winter) 
load("data/net.data.spring.w.order.RData")
load("C:/Documents d'Emilie/Sonja Wild/R code/Great-Tits/data/net.data.spring.w.order.RData")
head(net.data.spring) 


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
str(sp_class_aut$order)
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

#spring
sp_class_spr <- merge(net.data.spring, species_age_sex, by.x= "PIT")
head(sp_class_spr)
sp_class_spr$species <- as.factor(sp_class_spr$species)
sp_class_spr$age_in_2020 <- as.factor(sp_class_spr$age_in_2020)
sp_class_spr$order[sp_class_spr$order == "already.present"] <- 0
sp_class_spr <- subset(sp_class_spr, sp_class_spr$order!=0)
sp_class_spr <- subset(sp_class_spr, sp_class_spr$species %in% c("BLUTI", "GRETI", "MARTI", "NUTHA"))
head(sp_class_spr)
str(sp_class_spr$order)
str(sp_class_spr$species)
sp_class_spr$order <- as.numeric(sp_class_spr$order)


## data with all seasons together
library(dplyr)
sp_class_summ$season <- "summer"
sp_class_aut$season <- "autumn"
sp_class_wint$season <- "winter"
sp_class_spr$season <- "spring"
sp_class_season <- rbind(sp_class_summ, sp_class_aut, sp_class_wint, sp_class_spr)
View(sp_class_season)
sp_class_season$species <- as.factor(sp_class_season$species)
sp_class_season$season <- as.factor(sp_class_season$season)
sp_class_season$age_in_2020 <- as.factor(sp_class_season$age_in_2020)
unique(sp_class_season$order)
sp_class_season$order<- as.numeric(sp_class_season$order)
sp_class_season <- subset(sp_class_season, sp_class_season$species %in% c("BLUTI", "GRETI", "MARTI", "NUTHA"))
sp_class_season$order <- as.integer(sp_class_season$order)



#####Check if the SN position can explain these patterns 
#data
load("data/gmm.autumn.RData")
head(gmm.autumn) 
load("data/gmm.summer.RData")
head(gmm.summer)
load("data/gmm.winter.RData")
head(gmm.winter) 
load("data/gmm.spring.RData")
head(gmm.spring) 


#take the subsets before making the networks
#subsets for summer
gbi.summ <- gmm.summer$gbi
metadata.summ <- gmm.summer$metadata

#subsets for autumn
gbi.aut <- gmm.autumn$gbi
metadata.aut <- gmm.autumn$metadata

#subset for winter
gbi.wint <- gmm.winter$gbi
metadata.wint<- gmm.winter$metadata


#subset for spring
gbi.spr <- gmm.spring$gbi
metadata.spr<- gmm.spring$metadata

#####generate the network/week for each season 
library(asnipe)
library(igraph)

#add week column to gbi
#For that, I have to put a week column into the metadata 

#transform Start column into a date, for each season first

##SUMMER
head(metadata.summ)
metadata.summ$Date <- as.POSIXct(as.character(metadata.summ$Start), format = "%y%m%d%H%M%S")
head(metadata.summ)

#subtracting the 6 first numbers of the Start column
metadata.summ$date <- substr(metadata.summ$Start , 1, 6)
head(metadata.summ)
View(metadata.summ)
max(metadata.summ$Start)#go until august
max(metadata.summ$date)
min(metadata.summ$Start)#start in May
min(metadata.summ$date)
str(metadata.summ$date)
metadata.summ$date <- as.numeric(metadata.summ$date)

#lets change the $date so I have the year 2020 --> add 20000000 to each $date
metadata.summ$date2 <- 20000000 + metadata.summ$date
metadata.summ$date2 <- as.character(metadata.summ$date2 )
metadata.summ$date3 <- as.Date(metadata.summ$date2, format="%Y%m%d", origin= "20200505")

week_boundaries <- seq(min(metadata.summ$date3), max(metadata.summ$date3) + 7, by="7 days")

# Create a new column "week" based on the boundaries
metadata.summ$week <- cut(metadata.summ$date3, breaks = week_boundaries, labels = FALSE)

# Convert the numeric labels to factors
metadata.summ$week <- as.factor(metadata.summ$week)

#NB: here the first week correspond to the first seven days of data
head(metadata.summ)
str(metadata.summ$week)#14 weeks



#AUTUMN
head(metadata.aut)#the Starts are not ranked progressively by date 
min(metadata.aut$Start)#200929 --> first day of data 2020-09-29
str(metadata.aut$Start)
metadata.aut <- metadata.aut[order(metadata.aut$Start,decreasing = FALSE), ] #now in chronological order
head(metadata.aut)
metadata.aut$date <- substr(metadata.aut$Start , 1, 6)
str(metadata.aut$date)
metadata.aut$date <- as.numeric(metadata.aut$date)
metadata.aut$date <- 20000000 + metadata.aut$date
metadata.aut$date <- as.character(metadata.aut$date)
metadata.aut$date <- as.Date(metadata.aut$date, format="%Y%m%d", origin= "20200929")
week_boundaries.aut <- seq(min(metadata.aut$date), max(metadata.aut$date) + 7, by="7 days")
metadata.aut$week <- cut(metadata.aut$date, breaks = week_boundaries.aut, labels = FALSE)
metadata.aut$week <- as.factor(metadata.aut$week)
head(metadata.aut)
View(metadata.aut)
str(metadata.aut$week)# 3 weeks


#WINTER
head(metadata.wint)#the Starts are not ranked progressively by date 
min(metadata.wint$Start)#201117 --> first day of data 2020-11-17
str(metadata.wint$Start)
metadata.wint <- metadata.wint[order(metadata.wint$Start,decreasing = FALSE), ] #now in chronological order
head(metadata.wint)
metadata.wint$date <- substr(metadata.wint$Start , 1, 6)
str(metadata.wint$date)
metadata.wint$date <- as.numeric(metadata.wint$date)
metadata.wint$date <- 20000000 + metadata.wint$date
metadata.wint$date <- as.character(metadata.wint$date)
metadata.wint$date <- as.Date(metadata.wint$date, format="%Y%m%d", origin= "20200929")
week_boundaries.wint <- seq(min(metadata.wint$date), max(metadata.wint$date) + 7, by="7 days")
metadata.wint$week <- cut(metadata.wint$date, breaks = week_boundaries.wint, labels = FALSE)
metadata.wint$week <- as.factor(metadata.wint$week)
head(metadata.wint)
View(metadata.wint)
str(metadata.wint$week)# 3 weeks

#SPRING
head(metadata.spr)#the Starts are not ranked progressively by date 
min(metadata.spr$Start)#210223140011 --> first day of data 2021-03-02
str(metadata.spr$Start)
metadata.spr <- metadata.spr[order(metadata.spr$Start,decreasing = FALSE), ] #now in chronological order
head(metadata.spr)
metadata.spr$date <- substr(metadata.spr$Start , 1, 6)
str(metadata.spr$date)
metadata.spr$date <- as.numeric(metadata.spr$date)
metadata.spr$date <- 20000000 + metadata.spr$date
metadata.spr$date <- as.character(metadata.spr$date)
metadata.spr$date <- as.Date(metadata.spr$date, format="%Y%m%d", origin= "20200929")
week_boundaries.spr <- seq(min(metadata.spr$date), max(metadata.spr$date) + 7, by="7 days")
metadata.spr$week <- cut(metadata.spr$date, breaks = week_boundaries.spr, labels = FALSE)
metadata.spr$week <- as.factor(metadata.spr$week)
head(metadata.spr)
View(metadata.spr)
str(metadata.spr$week)# 3 weeks

##THE WEEKS
####make network with gbi: select the rows corresponding to each week.

# SW: here is the biggest change - calculating the network position each week is too noisy. We might as well calculate it across the three-week period. Following our publication from earlier this year, we will subset the summer network to the final three weeks of the 14 week period when all fledglings are independent.
# calculating it across the three weeks, we can put it all into one big model

# SW: I wrote a funciton to extract the social network position for each individual in each season

calculate.soc.network.pos <- function(gbi, metadata, threshold=5, season){
  
  # subset to the threshold
  gbi.sub <- gbi[,colSums(gbi)>=threshold]
  
  # calculate the network
  network_season <- get_network(gbi.sub, data_format="GBI",
                               association_index="SRI")
  

  dim(network_season)
  
  net <- graph_from_adjacency_matrix(network_season,mode= c("undirected"), diag=FALSE, weighted=TRUE)
  net_deg <- degree(net)
  Tag <- V(net)$name
  
  centrality_table <- data.frame(
    PIT = Tag,
    degree = net_deg)
  
  dc <- merge (centrality_table, sp_class_season[sp_class_season$season==season,], by.x= "PIT")
  head(dc)
  
  # Create a dataframe that includes degree, betweenness, the individuals and the order 
  btw <- betweenness(net, v = V(net),directed = F)
  betweenness_table <- data.frame(
    PIT = Tag,
    betweenness = btw)
  
  table_week <- merge(dc,betweenness_table, by.x= "PIT" )
  table_week$season <- season
  
  return(table_week)
  
}


network.pos.summer <- calculate.soc.network.pos(gbi=gbi.summ[which(metadata.summ$week %in% c(12,13,14)),], metadata = metadata.summ[which(metadata.summ$week %in% c(12,13,14)),], threshold=5, season="summer")
# here, we subset it to weeks 12-14 to be consistent with the other seasons
network.pos.summer <- subset(network.pos.summer, network.pos.summer$week %in% c(12,13,14))

network.pos.autumn <- calculate.soc.network.pos(gbi=gbi.aut, metadata = metadata.aut, threshold=5, season="autumn")
network.pos.winter <- calculate.soc.network.pos(gbi=gbi.wint, metadata = metadata.wint, threshold=5, season="winter")
network.pos.spring <- calculate.soc.network.pos(gbi=gbi.spr, metadata = metadata.spr, threshold=5, season="spring")


# here is a function that extracts group sizes (max order number of each group)
extract.group.size <- function(network.pos.object, net.data){
  for(i in 1:length(network.pos.object$group)){
    group.max <- max(as.numeric(subset(net.data$order[net.data$order!="already.present"], net.data[net.data$order!="already.present", "group"]==network.pos.object$group[i])))
    network.pos.object[i, "group.size"] <- group.max
  }
  return(network.pos.object)
}

# we run this on each season object
network.pos.summer <- extract.group.size(network.pos.object = network.pos.summer, net.data=net.data.summer)
network.pos.autumn <- extract.group.size(network.pos.object = network.pos.autumn, net.data=net.data.autumn)
network.pos.winter <- extract.group.size(network.pos.object = network.pos.winter, net.data=net.data.winter)
network.pos.spring <- extract.group.size(network.pos.object = network.pos.spring, net.data=net.data.spring)


# combine them into one data frame
network.pos.all.seasons <- rbind.data.frame(network.pos.summer, network.pos.autumn, network.pos.winter, network.pos.spring)


# SW: now we can actually put it all into one model

model_order_offset <- glmer(order ~ species*season + species*age_in_2020 + scale(degree) + scale(betweenness) + offset(log(group.size))+ (1|PIT), family=poisson, data= network.pos.all.seasons)
# SW: I have used the 'log' of group.size (not group) - this should now be the correct model specification
# glmer fails to converge, we'll still have a quick look at the output
summary(model_order_offset)


# SW: we can use brms instead, often converges better
library(brms)

model_order <- brm(
  order ~ species * season + species * age_in_2020 + scale(degree) + scale(betweenness) + offset(log(group.size)) + (1|PIT),
  family = poisson,
  data = network.pos.all.seasons,
  chains = 2,
  iter = 4000,
  cores = 4
)

summary(model_order)


pp_check(model_order)
# looks good!

plot(conditional_effects(model_order))

# SW: this is how far I got - there definitely seem to be some patterns with species/age/seasons - I'll let you look in detail

# As a next step, I recommend looking at repeatability of the arrival order - you can use the model_order 

# to not have to rerun everything, I have saved my output here:
save.image("image.RData")
load("image.RData")


####REPEATABILITY
library(rptR)
rep <- rpt(order ~ species * season + species * age_in_2020 + scale(degree) + scale(betweenness) + offset(log(group.size)) + (1|PIT),
grname = "PIT",data= network.pos.all.seasons, datatype="Poisson", nboot=2000, npermut=1000, adjusted=FALSE)

print(rep)
#use the link scale to interpret

##!!  The package rptR relies on mixed-effects models fitted by the lmer and glmer functions from the lme4 package https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/2041-210X.12797

###What if I kept out the non significant interactions in the model and made the nboot and npermut easier
rep <- rpt(order ~ species*season + age_in_2020 + scale(degree) + scale(betweenness) + offset(log(group.size)) + (1|PIT),
           grname = "PIT",data= network.pos.all.seasons, datatype="Poisson", nboot=10, npermut=0, adjusted=FALSE)
rep
#it elapsed







#####In the litterature, what has been done so far and is similar to this study
#https://www.sciencedirect.com/science/article/abs/pii/S0003347220301251
#https://www.nature.com/articles/s41598-017-00929-8
#https://royalsocietypublishing.org/doi/full/10.1098/rspb.2014.2804#d1e1575


