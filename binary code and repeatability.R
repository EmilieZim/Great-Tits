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

#I have saved my output here:
save.image("image.RData")
load("image.RData")


# make a binary variable out of the order with 'leader' versus 'follower'
network.pos.all.seasons$leader.follower <- network.pos.all.seasons$order
network.pos.all.seasons$leader.follower[network.pos.all.seasons$leader.follower!=1] <- "follower"
network.pos.all.seasons$leader.follower[network.pos.all.seasons$leader.follower==1] <- "leader"
network.pos.all.seasons$leader.follower <- as.factor(network.pos.all.seasons$leader.follower)


# SW: now we can actually put it all into one model
library(lme4)
# we first run a simpler regression (no interactions) to look at collinearity between predictors
model_order_vif <- glmer(leader.follower ~  species + season + age_in_2020 + scale(degree) + scale(betweenness) + group.size + (1|PIT), family= binomial, data= network.pos.all.seasons)

library(aed)
vif(model_order_vif)
#GVIF Df GVIF^(1/(2*Df))
# species            1.481536  3        1.067707
# season             2.908755  3        1.194771
# age_in_2020        1.206738  1        1.098516
# scale(degree)      2.811435  1        1.676733
# scale(betweenness) 1.214774  1        1.102168
# group.size         1.393187  1        1.180333

# all vifs <5, so we can include all of them


# we run the global model with interactions 
model_order_global <- glmer(
  leader.follower ~  species * season + species * age_in_2020 + scale(betweenness) *
    season + scale(betweenness) *
    species + scale(betweenness) * age_in_2020 + scale(degree) *
    season + scale(degree) *
    species + scale(degree) * age_in_2020 + group.size * species + group.size *
    age_in_2020 + group.size * season + (1 |
                                           PIT),
  family = binomial,
  data = network.pos.all.seasons
)
# we are having convergence issues

# Warning messages:
#   1: In (function (fn, par, lower = rep.int(-Inf, n), upper = rep.int(Inf,  :
#                                                                         failure to converge in 10000 evaluations
#                                                                       2: In optwrap(optimizer, devfun, start, rho$lower, control = control,  :
#                                                                                       convergence code 4 from Nelder_Mead: failure to converge in 10000 evaluations
#                                                                                     3: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#                                                                                                       Model failed to converge with max|grad| = 0.136374 (tol = 0.002, component 1)

# we therefore opt for a Bayesian model

glob_model <- brm(
  leader.follower ~  species * season + species * age_in_2020 + scale(betweenness) *
    season + scale(betweenness) *
    species + scale(betweenness) * age_in_2020 + scale(degree) *
    season + scale(degree) *
    species + scale(degree) * age_in_2020 + group.size * species + group.size *
    age_in_2020 + group.size * season + (1 | PIT),
  family = bernoulli, 
  data = network.pos.all.seasons,
  chains = 2,
  iter = 4000,
  cores = 4,
  save_pars = save_pars(all = TRUE)
)

summary(glob_model)

# Family: bernoulli 
# Links: mu = logit 
# Formula: leader.follower ~ species * season + species * age_in_2020 + scale(betweenness) * season + scale(betweenness) * species + scale(betweenness) * age_in_2020 + scale(degree) * season + scale(degree) * species + scale(degree) * age_in_2020 + group.size * species + group.size * age_in_2020 + group.size * season + (1 | PIT) 
# Data: network.pos.all.seasons (Number of observations: 13394) 
# Draws: 2 chains, each with iter = 4000; warmup = 2000; thin = 1;
# total post-warmup draws = 4000
# 
# Multilevel Hyperparameters:
#   ~PIT (Number of levels: 225) 
# Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sd(Intercept)     0.39      0.04     0.31     0.48 1.00     1706     2609
# 
# Regression Coefficients:
#   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept                               -0.33      0.59    -1.48     0.84 1.00      674     1245
# speciesGRETI                            -0.64      0.38    -1.41     0.08 1.00      862     1692
# speciesMARTI                             0.52      0.41    -0.28     1.32 1.01      754     1459
# speciesNUTHA                             1.81      0.60     0.63     2.98 1.00      989     2067
# seasonspring                            -0.88      0.59    -2.05     0.28 1.00      717      972
# seasonsummer                             0.41      0.62    -0.79     1.67 1.00      807     1122
# seasonwinter                            -1.88      0.63    -3.12    -0.64 1.00      696     1207
# age_in_2020juvenile                      0.03      0.23    -0.42     0.49 1.00     1919     2441
# scalebetweenness                        -0.54      0.24    -1.02    -0.08 1.00      549     1334
# scaledegree                              0.36      0.53    -0.66     1.40 1.00      628     1171
# group.size                              -0.16      0.03    -0.21    -0.11 1.00     1234     2374
# speciesGRETI:seasonspring                0.64      0.32     0.02     1.25 1.00     1153     2010
# speciesMARTI:seasonspring               -0.72      0.39    -1.49     0.02 1.01      930     2140
# speciesNUTHA:seasonspring               -1.77      0.57    -2.86    -0.65 1.00     1303     2525
# speciesGRETI:seasonsummer                0.66      0.36    -0.06     1.36 1.00     1140     2469
# speciesMARTI:seasonsummer               -0.40      0.41    -1.22     0.40 1.00     1026     2064
# speciesNUTHA:seasonsummer               -0.36      0.49    -1.32     0.61 1.00     1409     2570
# speciesGRETI:seasonwinter                1.27      0.45     0.38     2.11 1.01      923     1919
# speciesMARTI:seasonwinter                0.99      0.49     0.06     1.96 1.01      962     1935
# speciesNUTHA:seasonwinter               -1.35      0.74    -2.85     0.10 1.00      885     1790
# speciesGRETI:age_in_2020juvenile         0.19      0.24    -0.30     0.67 1.00     1962     2032
# speciesMARTI:age_in_2020juvenile        -0.01      0.58    -1.16     1.06 1.00     2467     3009
# speciesNUTHA:age_in_2020juvenile         1.11      0.54     0.09     2.19 1.00     1569     2213
# seasonspring:scalebetweenness            0.43      0.24    -0.04     0.92 1.00      632     1422
# seasonsummer:scalebetweenness            0.56      0.25     0.07     1.06 1.00      590     1471
# seasonwinter:scalebetweenness            0.62      0.21     0.21     1.05 1.00      581     1508
# speciesGRETI:scalebetweenness            0.21      0.15    -0.08     0.50 1.00     2466     2564
# speciesMARTI:scalebetweenness            0.23      0.13    -0.03     0.49 1.00     1750     2546
# speciesNUTHA:scalebetweenness            0.17      0.23    -0.28     0.64 1.00      990     2101
# age_in_2020juvenile:scalebetweenness    -0.23      0.11    -0.45    -0.01 1.00     3501     3300
# seasonspring:scaledegree                -0.72      0.52    -1.76     0.27 1.00      635     1168
# seasonsummer:scaledegree                -0.41      0.52    -1.44     0.62 1.00      641     1171
# seasonwinter:scaledegree                -0.42      0.51    -1.42     0.56 1.00      666     1352
# speciesGRETI:scaledegree                -0.18      0.18    -0.53     0.18 1.00     1382     2086
# speciesMARTI:scaledegree                -0.17      0.23    -0.64     0.29 1.00     1372     2286
# speciesNUTHA:scaledegree                 0.79      0.45    -0.09     1.66 1.00     1524     2435
# age_in_2020juvenile:scaledegree          0.08      0.11    -0.13     0.29 1.00     3009     3069
# speciesGRETI:group.size                 -0.02      0.01    -0.04     0.01 1.00     3529     3017
# speciesMARTI:group.size                 -0.03      0.01    -0.06    -0.01 1.00     3579     3126
# speciesNUTHA:group.size                 -0.02      0.02    -0.07     0.02 1.00     3504     2580
# age_in_2020juvenile:group.size          -0.00      0.01    -0.02     0.01 1.00     3871     3575
# seasonspring:group.size                  0.08      0.03     0.02     0.13 1.00     1261     2220
# seasonsummer:group.size                 -0.15      0.04    -0.22    -0.08 1.00     1777     2535
# seasonwinter:group.size                  0.12      0.03     0.07     0.17 1.00     1269     2230
# 
# Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
# and Tail_ESS are effective sample size measures, and Rhat is the potential
# scale reduction factor on split chains (at convergence, Rhat = 1).

# remove non-significant interactions (those with credible intervals spanning 0)

red_model <- brm(
  leader.follower ~  species * season + species * age_in_2020 + scale(betweenness) *
    season + scale(betweenness) * age_in_2020 + group.size * species +  group.size * season + (1 | PIT),
  family = bernoulli, 
  data = network.pos.all.seasons,
  chains = 2,
  iter = 4000,
  cores = 4,
  save_pars = save_pars(all = TRUE)
)

summary(red_model)

# Family: bernoulli 
# Links: mu = logit 
# Formula: leader.follower ~ species * season + species * age_in_2020 + scale(betweenness) * season + scale(betweenness) * age_in_2020 + group.size * species + group.size * season + (1 | PIT) 
# Data: network.pos.all.seasons (Number of observations: 13394) 
# Draws: 2 chains, each with iter = 4000; warmup = 2000; thin = 1;
# total post-warmup draws = 4000
# 
# Multilevel Hyperparameters:
#   ~PIT (Number of levels: 225) 
# Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sd(Intercept)     0.38      0.04     0.30     0.46 1.00     1744     2466
# 
# Regression Coefficients:
#   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept                               -0.55      0.29    -1.14     0.03 1.01      575     1069
# speciesGRETI                            -0.57      0.29    -1.13     0.01 1.00      680     1434
# speciesMARTI                             0.50      0.32    -0.13     1.13 1.00      591     1190
# speciesNUTHA                             0.93      0.44     0.05     1.80 1.01      628     1327
# seasonspring                            -0.36      0.31    -0.96     0.26 1.00      611     1253
# seasonsummer                             0.73      0.38    -0.02     1.45 1.01      604     1460
# seasonwinter                            -1.65      0.34    -2.33    -0.98 1.00      561     1417
# age_in_2020juvenile                     -0.06      0.20    -0.46     0.31 1.00     1305     1839
# scalebetweenness                        -0.20      0.12    -0.44     0.03 1.00     1007     1833
# group.size                              -0.15      0.03    -0.21    -0.10 1.00     1196     1688
# speciesGRETI:seasonspring                0.76      0.30     0.15     1.33 1.00      685     1650
# speciesMARTI:seasonspring               -0.35      0.35    -1.03     0.33 1.00      727     1915
# speciesNUTHA:seasonspring               -1.30      0.52    -2.36    -0.28 1.00      801     2180
# speciesGRETI:seasonsummer                0.66      0.36    -0.05     1.35 1.01      649     1318
# speciesMARTI:seasonsummer               -0.24      0.40    -1.01     0.55 1.01      653     1754
# speciesNUTHA:seasonsummer               -0.42      0.45    -1.30     0.48 1.01      535     1455
# speciesGRETI:seasonwinter                1.14      0.33     0.48     1.76 1.00      614     1615
# speciesMARTI:seasonwinter                0.99      0.37     0.27     1.71 1.00      589     1486
# speciesNUTHA:seasonwinter               -0.23      0.48    -1.21     0.71 1.01      584     1527
# speciesGRETI:age_in_2020juvenile         0.19      0.23    -0.25     0.63 1.00     1218     1799
# speciesMARTI:age_in_2020juvenile         0.12      0.50    -0.88     1.11 1.00     1674     2333
# speciesNUTHA:age_in_2020juvenile         0.72      0.43    -0.10     1.58 1.00     1672     2543
# seasonspring:scalebetweenness            0.10      0.16    -0.21     0.42 1.00     1251     2058
# seasonsummer:scalebetweenness            0.38      0.16     0.07     0.69 1.00     1119     2181
# seasonwinter:scalebetweenness            0.42      0.14     0.14     0.70 1.00     1044     2116
# age_in_2020juvenile:scalebetweenness    -0.20      0.09    -0.38    -0.02 1.00     2519     2754
# speciesGRETI:group.size                 -0.02      0.01    -0.04    -0.00 1.00     2537     2897
# speciesMARTI:group.size                 -0.04      0.01    -0.06    -0.01 1.00     2863     2468
# speciesNUTHA:group.size                 -0.01      0.02    -0.06     0.02 1.00     3412     2931
# seasonspring:group.size                  0.06      0.03     0.01     0.11 1.00     1209     1705
# seasonsummer:group.size                 -0.16      0.03    -0.22    -0.09 1.00     1572     2518
# seasonwinter:group.size                  0.12      0.03     0.07     0.17 1.00     1179     1583
# 
# Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
# and Tail_ESS are effective sample size measures, and Rhat is the potential
# scale reduction factor on split chains (at convergence, Rhat = 1).


# check how well it fits
pp_check(red_model, ndraws = 100)
# looks like a very good fit
# the asymmetry means that birds are more likely to be followers (0) than leaders (1), which makes sense

plot(red_model)
# stationarity and mixing are good


# SW: here is where we would need to make some nice plots
plot(conditional_effects(red_model))
# you can check my code from the ontogeny of social networks for some input on how to make plots from brms objects using ggplot
# a 0 here means to be a follower (first in the alphabet)

library(performance)
# extract an R2 for our model
performance::r2_bayes(red_model)
# Bayesian R2 with Compatibility Interval
# 
# Conditional R2: 0.185 (95% CI [0.173, 0.198])
# Marginal R2: 0.151 (95% CI [0.125, 0.172])


#### repeatability of arrival (leader versus follower)

# we can use the function icc (intra class correlation coefficient) from package performance
performance::icc(red_model)

# # Intraclass Correlation Coefficient
# 
# Adjusted ICC: 0.041
# Unadjusted ICC: 0.024

# looks like there is very low repeatability within individuals


# to double check that the value is right, we use an alternative approach:
# Extract variance components
var_components <- VarCorr(red_model)
print(var_components)


# Extract the variance for the random effect PIT (squaring the standard deviation)
pit_var <- as.numeric(var_components$PIT$sd[1])^2

# Residual variance for Bernoulli-logit model
residual_variance <- pi^2 / 3

# Calculate repeatability
repeatability <- pit_var / (pit_var + residual_variance)
repeatability

# [1] 0.04100636

# same result as above!

save.image(file="image_final.RData")
load(file="image_final.RData")

#Calculate CI
rethinking::HPDI(repeatability, prob = 0.95)
# |0.95      0.95| 
#Does not work

#######My suggestion:
#> ---- Extract the variance components and calculate repeatability ----
library(tidybayes)
get_variables(red_model)#This just tells us what parameters we can pull from the model
#Extract the variance components
post.data = red_model %>% spread_draws(sd_PIT__Intercept) 
#Among-individual variance
post.data$Va_PIT = post.data$sd_PIT__Intercept^2 # we square it because it is currently a standard deviation, and by squaring this value we turn it into a variance

#Within-individual variance 
#For Bernouilli: sigma^2 = pi^2/3
sigma_sq = (pi^2)/3

#Repeatability
repeatability <- post.data$Va_PIT / (post.data$Va_PIT + sigma_sq)
repeatability
hist(repeatability)
mean(repeatability) #same as above, with the other methods

#Calculate CI
rethinking::HPDI(repeatability, prob = 0.95)
# |0.95      0.95| 
#0.02487687 0.05688738
#Our value does fall within the IC

#This last part was inspired by the study: https://datadryad.org/stash/dataset/doi:10.25338/B88P8W











