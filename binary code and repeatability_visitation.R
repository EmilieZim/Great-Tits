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


# ##Preparation of the data
# load("data/gmm.summer.RData")
# net.data.summer <- read.delim("data/Mill.data.summer.txt", sep=" ", row.names = 1)
# head(net.data.summer)
# 
# gmm.summer$metadata$Location2 <- substr(gmm.summer$metadata$Location, 8, 13)
# 
# # for each group, we have the start and end time and a location
# # we add a new column called 'group' that will be filled by using a function
# net.data.summer[, "group"] <- NA
# head(net.data.summer)
# 
# 
# for(i in 1:length(gmm.summer$metadata$Start)){ # for each start time
#   i.loc <- gmm.summer$metadata$Location2[i] # we extract the location
#   rows.i <-  rownames(subset(net.data.summer, 
#                              net.data.summer$Date.Time>=gmm.summer$metadata$Start[i] &
#                                net.data.summer$Date.Time<=gmm.summer$metadata$End[i] &
#                                net.data.summer$location == gmm.summer$metadata$Location2[i]))
#   net.data.summer[rows.i, "group"] <- i
#   
# }
# 
# net.data.summer[which(is.na(net.data.summer$group)),]
# 
# length(which(is.na(net.data.summer$group)))
# # SW: 2397 observations are not part of a group. Perhaps these were birds that arrived before I officially started the observations? Or then they were dropped for some reason by the gmm function? I might need to rerun the gmm function to see what's going on there. 
# 
# # for now, we subset it to exclude those
# net.data.summer <- subset(net.data.summer, !is.na(net.data.summer$group))
# 
# length(unique(net.data.summer$group))
# # 9913 groups remain
# 
# # make sure it's in correct order (first ordered by groups, then time)
# net.data.summer <- net.data.summer[with(net.data.summer, order(group, Date.Time)), ]
# 
# # we create a new column called 'order' all filled with NA
# net.data.summer$order <- NA
# # to the very first visitor, we assign a 1
# net.data.summer$order[1] <- 1
# # now we loop through the entire data frame
# for(i in 2:length(net.data.summer$group)){
#   # we extract the PIT tag and the group number of bird i and the bird and group number that came just before
#   group.i <- net.data.summer$group[i]
#   ID.i <- net.data.summer$PIT[i]
#   group.i_minus1 <- net.data.summer$group[i-1]
#   ID.i_minus1 <- net.data.summer$PIT[i-1]
#   
#   # we also don't want to assign a new order number, if the bird had already arrived as part of the current group, so we extract the PIT tags that are already part of the current group
#   sub <- net.data.summer[1:(i-1),]
#   sub <- subset(sub, sub$group==group.i)
#   already.present <- unique(sub$PIT)
#   
#   if(ID.i %in% already.present){ # if the bird hard already arrived as part of the current group
#     net.data.summer$order[i] <- 'already.present' # we assign 'already.present'
#   } else if(group.i==group.i_minus1 & ID.i!=ID.i_minus1){
#     # if it's the same group but a new bird, we add the order+1
#     net.data.summer$order[i] <- max(as.numeric(na.omit(sub$order[sub$order != "already.present"])))+1
#   } else if(group.i!=group.i_minus1){
#     # if it's a new group, we start over with the order=1
#     net.data.summer$order[i] <- 1
#   }
# }
# 
# 
# # we remove the rows that contain 'already present' to only have each bird in each group once
# net.data.summer <- subset(net.data.summer, !(is.na(net.data.summer$order)) & net.data.summer$order != "already.present")
# 
# head(net.data.summer, 40)
# net.data.summer$order <- as.numeric(net.data.summer$order)
# max(net.data.summer$order)
# hist(net.data.summer$order)
# 
# # save it as the object with the order
# save(net.data.summer, file="data/net.data.summer.w.order.RData")
# load("data/net.data.summer.w.order.RData")
# 
# ##Take the same data but for each season (the part above has been used to get the following objects)
# # data
# net.data.summer <- read.delim("data/Mill.data.summer.txt", sep=",", row.names = 1)
# net.data.autumn <- read.delim("data/Mill.data.autumn.txt", sep=",", row.names = 1)
# View(net.data.autumn)
# net.data.winter <- read.delim("data/Mill.data.winter.txt", sep=",", row.names = 1)
# net.data.spring <- read.delim("data/Mill.data.spring.txt", sep=",", row.names = 1)
# 
# #species_age_sex
# load("data/species_age_sex.RDA") 
# View(species_age_sex)
# save(species_age_sex.RDA, file="species_age_sex.RDA") 
# load("data/species_age_sex.RDA")
# head(species_age_sex)
# 
# #order data
# load("data/net.data.summer.w.order.RData")
# bd <- load("C:/Documents d'Emilie/Sonja Wild/R code/Great-Tits/data/net.data.summer.w.order.RData")
# bd # it says "net.data.summer" --> the name is no longer net.data.summer.w.order
# head(net.data.summer) 
# load("data/net.data.autumn.w.order.RData")
# load("C:/Documents d'Emilie/Sonja Wild/R code/Great-Tits/data/net.data.autumn.w.order.RData")
# head(net.data.autumn)
# load("data/net.data.winter.w.order.RData")
# load("C:/Documents d'Emilie/Sonja Wild/R code/Great-Tits/data/net.data.winter.w.order.RData")
# head(net.data.winter) 
# load("data/net.data.spring.w.order.RData")
# load("C:/Documents d'Emilie/Sonja Wild/R code/Great-Tits/data/net.data.spring.w.order.RData")
# head(net.data.spring) 
# 
# 
# #make a dataframe (for each season)
# #summer
# sp_class_summ <- merge(net.data.summer, species_age_sex, by.x= "PIT")
# str(sp_class_summ$species)
# sp_class_summ$species <- as.factor(sp_class_summ$species)
# sp_class_summ$age_in_2020 <- as.factor(sp_class_summ$age_in_2020)
# #leave out the "already present". 
# sp_class_summ$order[sp_class_summ$order == "already.present"] <- 0
# sp_class_summ <- subset(sp_class_summ, sp_class_summ$order!=0)
# 
# head(sp_class_summ)
# str(sp_class_summ$order)
# sp_class_summ$order <- as.numeric(sp_class_summ$order)
# 
# # SW: You can kick out the COATI and GREFI - COATI are so few and GREFI is most likely a mistake.
# sp_class_summ <- subset(sp_class_summ, sp_class_summ$species %in% c("BLUTI", "GRETI", "MARTI", "NUTHA"))
# 
# #autumn
# sp_class_aut <- merge(net.data.autumn, species_age_sex, by.x= "PIT")
# head(sp_class_aut)
# str(sp_class_aut$species)
# sp_class_aut$species <- as.factor(sp_class_aut$species)
# sp_class_aut$age_in_2020 <- as.factor(sp_class_aut$age_in_2020)
# sp_class_aut$order[sp_class_aut$order == "already.present"] <- 0
# sp_class_aut <- subset(sp_class_aut, sp_class_aut$order!=0)
# sp_class_aut <- subset(sp_class_aut, sp_class_aut$species %in% c("BLUTI", "GRETI", "MARTI", "NUTHA"))
# head(sp_class_aut)
# str(sp_class_aut$order)
# sp_class_aut$order <- as.numeric(sp_class_aut$order)
# 
# 
# #winter
# sp_class_wint <- merge(net.data.winter, species_age_sex, by.x= "PIT")
# head(sp_class_wint)
# sp_class_wint$species <- as.factor(sp_class_wint$species)
# sp_class_wint$age_in_2020 <- as.factor(sp_class_wint$age_in_2020)
# sp_class_wint$order[sp_class_wint$order == "already.present"] <- 0
# sp_class_wint <- subset(sp_class_wint, sp_class_wint$order!=0)
# sp_class_wint <- subset(sp_class_wint, sp_class_wint$species %in% c("BLUTI", "GRETI", "MARTI", "NUTHA"))
# head(sp_class_wint)
# str(sp_class_wint$order)
# str(sp_class_wint$species)
# sp_class_wint$order <- as.numeric(sp_class_wint$order)
# 
# #spring
# sp_class_spr <- merge(net.data.spring, species_age_sex, by.x= "PIT")
# head(sp_class_spr)
# sp_class_spr$species <- as.factor(sp_class_spr$species)
# sp_class_spr$age_in_2020 <- as.factor(sp_class_spr$age_in_2020)
# sp_class_spr$order[sp_class_spr$order == "already.present"] <- 0
# sp_class_spr <- subset(sp_class_spr, sp_class_spr$order!=0)
# sp_class_spr <- subset(sp_class_spr, sp_class_spr$species %in% c("BLUTI", "GRETI", "MARTI", "NUTHA"))
# head(sp_class_spr)
# str(sp_class_spr$order)
# str(sp_class_spr$species)
# sp_class_spr$order <- as.numeric(sp_class_spr$order)
# 
# 
# ## data with all seasons together
# library(dplyr)
# sp_class_summ$season <- "summer"
# sp_class_aut$season <- "autumn"
# sp_class_wint$season <- "winter"
# sp_class_spr$season <- "spring"
# sp_class_season <- rbind(sp_class_summ, sp_class_aut, sp_class_wint, sp_class_spr)
# View(sp_class_season)
# sp_class_season$species <- as.factor(sp_class_season$species)
# sp_class_season$season <- as.factor(sp_class_season$season)
# sp_class_season$age_in_2020 <- as.factor(sp_class_season$age_in_2020)
# unique(sp_class_season$order)
# sp_class_season$order<- as.numeric(sp_class_season$order)
# sp_class_season <- subset(sp_class_season, sp_class_season$species %in% c("BLUTI", "GRETI", "MARTI", "NUTHA"))
# sp_class_season$order <- as.integer(sp_class_season$order)
# 
# 
# 
# #####Check if the SN position can explain these patterns 
# #data
# load("data/gmm.autumn.RData")
# head(gmm.autumn) 
# load("data/gmm.summer.RData")
# head(gmm.summer)
# load("data/gmm.winter.RData")
# head(gmm.winter) 
# load("data/gmm.spring.RData")
# head(gmm.spring) 
# 
# 
# #take the subsets before making the networks
# #subsets for summer
# gbi.summ <- gmm.summer$gbi
# metadata.summ <- gmm.summer$metadata
# 
# #subsets for autumn
# gbi.aut <- gmm.autumn$gbi
# metadata.aut <- gmm.autumn$metadata
# 
# #subset for winter
# gbi.wint <- gmm.winter$gbi
# metadata.wint<- gmm.winter$metadata
# 
# 
# #subset for spring
# gbi.spr <- gmm.spring$gbi
# metadata.spr<- gmm.spring$metadata
# 
# #####generate the network/week for each season 
# library(asnipe)
# library(igraph)
# 
# #add week column to gbi
# #For that, I have to put a week column into the metadata 
# 
# #transform Start column into a date, for each season first
# 
# ##SUMMER
# head(metadata.summ)
# metadata.summ$Date <- as.POSIXct(as.character(metadata.summ$Start), format = "%y%m%d%H%M%S")
# head(metadata.summ)
# 
# #subtracting the 6 first numbers of the Start column
# metadata.summ$date <- substr(metadata.summ$Start , 1, 6)
# head(metadata.summ)
# View(metadata.summ)
# max(metadata.summ$Start)#go until august
# max(metadata.summ$date)
# min(metadata.summ$Start)#start in May
# min(metadata.summ$date)
# str(metadata.summ$date)
# metadata.summ$date <- as.numeric(metadata.summ$date)
# 
# #lets change the $date so I have the year 2020 --> add 20000000 to each $date
# metadata.summ$date2 <- 20000000 + metadata.summ$date
# metadata.summ$date2 <- as.character(metadata.summ$date2 )
# metadata.summ$date3 <- as.Date(metadata.summ$date2, format="%Y%m%d", origin= "20200505")
# 
# week_boundaries <- seq(min(metadata.summ$date3), max(metadata.summ$date3) + 7, by="7 days")
# 
# # Create a new column "week" based on the boundaries
# metadata.summ$week <- cut(metadata.summ$date3, breaks = week_boundaries, labels = FALSE)
# 
# # Convert the numeric labels to factors
# metadata.summ$week <- as.factor(metadata.summ$week)
# 
# #NB: here the first week correspond to the first seven days of data
# head(metadata.summ)
# str(metadata.summ$week)#14 weeks
# 
# 
# 
# #AUTUMN
# head(metadata.aut)#the Starts are not ranked progressively by date 
# min(metadata.aut$Start)#200929 --> first day of data 2020-09-29
# str(metadata.aut$Start)
# metadata.aut <- metadata.aut[order(metadata.aut$Start,decreasing = FALSE), ] #now in chronological order
# head(metadata.aut)
# metadata.aut$date <- substr(metadata.aut$Start , 1, 6)
# str(metadata.aut$date)
# metadata.aut$date <- as.numeric(metadata.aut$date)
# metadata.aut$date <- 20000000 + metadata.aut$date
# metadata.aut$date <- as.character(metadata.aut$date)
# metadata.aut$date <- as.Date(metadata.aut$date, format="%Y%m%d", origin= "20200929")
# week_boundaries.aut <- seq(min(metadata.aut$date), max(metadata.aut$date) + 7, by="7 days")
# metadata.aut$week <- cut(metadata.aut$date, breaks = week_boundaries.aut, labels = FALSE)
# metadata.aut$week <- as.factor(metadata.aut$week)
# head(metadata.aut)
# View(metadata.aut)
# str(metadata.aut$week)# 3 weeks
# 
# 
# #WINTER
# head(metadata.wint)#the Starts are not ranked progressively by date 
# min(metadata.wint$Start)#201117 --> first day of data 2020-11-17
# str(metadata.wint$Start)
# metadata.wint <- metadata.wint[order(metadata.wint$Start,decreasing = FALSE), ] #now in chronological order
# head(metadata.wint)
# metadata.wint$date <- substr(metadata.wint$Start , 1, 6)
# str(metadata.wint$date)
# metadata.wint$date <- as.numeric(metadata.wint$date)
# metadata.wint$date <- 20000000 + metadata.wint$date
# metadata.wint$date <- as.character(metadata.wint$date)
# metadata.wint$date <- as.Date(metadata.wint$date, format="%Y%m%d", origin= "20200929")
# week_boundaries.wint <- seq(min(metadata.wint$date), max(metadata.wint$date) + 7, by="7 days")
# metadata.wint$week <- cut(metadata.wint$date, breaks = week_boundaries.wint, labels = FALSE)
# metadata.wint$week <- as.factor(metadata.wint$week)
# head(metadata.wint)
# View(metadata.wint)
# str(metadata.wint$week)# 3 weeks
# 
# #SPRING
# head(metadata.spr)#the Starts are not ranked progressively by date 
# min(metadata.spr$Start)#210223140011 --> first day of data 2021-03-02
# str(metadata.spr$Start)
# metadata.spr <- metadata.spr[order(metadata.spr$Start,decreasing = FALSE), ] #now in chronological order
# head(metadata.spr)
# metadata.spr$date <- substr(metadata.spr$Start , 1, 6)
# str(metadata.spr$date)
# metadata.spr$date <- as.numeric(metadata.spr$date)
# metadata.spr$date <- 20000000 + metadata.spr$date
# metadata.spr$date <- as.character(metadata.spr$date)
# metadata.spr$date <- as.Date(metadata.spr$date, format="%Y%m%d", origin= "20200929")
# week_boundaries.spr <- seq(min(metadata.spr$date), max(metadata.spr$date) + 7, by="7 days")
# metadata.spr$week <- cut(metadata.spr$date, breaks = week_boundaries.spr, labels = FALSE)
# metadata.spr$week <- as.factor(metadata.spr$week)
# head(metadata.spr)
# View(metadata.spr)
# str(metadata.spr$week)# 3 weeks
# 
# ##THE WEEKS
# ####make network with gbi: select the rows corresponding to each week.
# 
# # SW: here is the biggest change - calculating the network position each week is too noisy. We might as well calculate it across the three-week period. Following our publication from earlier this year, we will subset the summer network to the final three weeks of the 14 week period when all fledglings are independent.
# # calculating it across the three weeks, we can put it all into one big model
# 
# # SW: I wrote a funciton to extract the social network position for each individual in each season
# 
# calculate.soc.network.pos <- function(gbi, metadata, threshold=5, season){
#   
#   # subset to the threshold
#   gbi.sub <- gbi[,colSums(gbi)>=threshold]
#   
#   # calculate the network
#   network_season <- get_network(gbi.sub, data_format="GBI",
#                                 association_index="SRI")
#   
#   
#   dim(network_season)
#   
#   net <- graph_from_adjacency_matrix(network_season,mode= c("undirected"), diag=FALSE, weighted=TRUE)
#   net_deg <- degree(net)
#   Tag <- V(net)$name
#   
#   centrality_table <- data.frame(
#     PIT = Tag,
#     degree = net_deg)
#   
#   dc <- merge (centrality_table, sp_class_season[sp_class_season$season==season,], by.x= "PIT")
#   head(dc)
#   
#   # Create a dataframe that includes degree, betweenness, the individuals and the order 
#   btw <- betweenness(net, v = V(net),directed = F)
#   betweenness_table <- data.frame(
#     PIT = Tag,
#     betweenness = btw)
#   
#   table_week <- merge(dc,betweenness_table, by.x= "PIT" )
#   table_week$season <- season
#   
#   return(table_week)
#   
# }
# 
# 
# network.pos.summer <- calculate.soc.network.pos(gbi=gbi.summ[which(metadata.summ$week %in% c(12,13,14)),], metadata = metadata.summ[which(metadata.summ$week %in% c(12,13,14)),], threshold=5, season="summer")
# # here, we subset it to weeks 12-14 to be consistent with the other seasons
# network.pos.summer <- subset(network.pos.summer, network.pos.summer$week %in% c(12,13,14))
# 
# network.pos.autumn <- calculate.soc.network.pos(gbi=gbi.aut, metadata = metadata.aut, threshold=5, season="autumn")
# network.pos.winter <- calculate.soc.network.pos(gbi=gbi.wint, metadata = metadata.wint, threshold=5, season="winter")
# network.pos.spring <- calculate.soc.network.pos(gbi=gbi.spr, metadata = metadata.spr, threshold=5, season="spring")
# 
# 
# # here is a function that extracts group sizes (max order number of each group)
# extract.group.size <- function(network.pos.object, net.data){
#   for(i in 1:length(network.pos.object$group)){
#     group.max <- max(as.numeric(subset(net.data$order[net.data$order!="already.present"], net.data[net.data$order!="already.present", "group"]==network.pos.object$group[i])))
#     network.pos.object[i, "group.size"] <- group.max
#   }
#   return(network.pos.object)
# }
# 
# # we run this on each season object
# network.pos.summer <- extract.group.size(network.pos.object = network.pos.summer, net.data=net.data.summer)
# network.pos.autumn <- extract.group.size(network.pos.object = network.pos.autumn, net.data=net.data.autumn)
# network.pos.winter <- extract.group.size(network.pos.object = network.pos.winter, net.data=net.data.winter)
# network.pos.spring <- extract.group.size(network.pos.object = network.pos.spring, net.data=net.data.spring)
# 
# 
# # combine them into one data frame
# network.pos.all.seasons <- rbind.data.frame(network.pos.summer, network.pos.autumn, network.pos.winter, network.pos.spring)
# 
# #I have saved my output here:
# save.image("image.RData")
# load("image.RData")
# 
# 
# # make a binary variable out of the order with 'leader' versus 'follower'
# network.pos.all.seasons$leader.follower <- network.pos.all.seasons$order
# network.pos.all.seasons$leader.follower[network.pos.all.seasons$leader.follower!=1] <- "follower"
# network.pos.all.seasons$leader.follower[network.pos.all.seasons$leader.follower==1] <- "leader"
# network.pos.all.seasons$leader.follower <- as.factor(network.pos.all.seasons$leader.follower)

# this contains the complete data set for analysis the leader follower dynamics
network.pos.all.seasons <- read.csv("data/leader_follower_data_all_seasons.csv", row.names = 1)
network.pos.all.seasons$leader.follower <- as.factor(network.pos.all.seasons$leader.follower)
head(network.pos.all.seasons)
# columns explained: (SW: please add some explanation here as to what the different columns are)

# PIT: 	
# degree	
# Date.Time	
# Antenna	
# location	
# week	
# group	
# leader.follower	
# species	
# age_in_2020	
# season	
# betweenness	
# group.size


library(lme4)
# we first run a simple regression (no interactions) to look at collinearity between predictors
model_order_vif <- glmer(leader.follower ~  species + season + age_in_2020 + scale(degree) + scale(betweenness) + group.size + (1|PIT), family= binomial, data= network.pos.all.seasons)

summary(model_order_vif)

library(car)
vif(model_order_vif)
# GVIF Df GVIF^(1/(2*Df))
# species            1.378070  3        1.054901
# season             2.693207  3        1.179537
# age_in_2020        1.132574  1        1.064225
# scale(degree)      2.834961  1        1.683734
# scale(betweenness) 1.298273  1        1.139418
# group.size         1.150594  1        1.072658

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
#                                                                                                       Model failed to converge with max|grad| = 0.0348467 (tol = 0.002, component 1)

# we therefore opt for a Bayesian model
library(brms)
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
# Data: network.pos.all.seasons (Number of observations: 27163) 
# Draws: 2 chains, each with iter = 4000; warmup = 2000; thin = 1;
# total post-warmup draws = 4000
# 
# Multilevel Hyperparameters:
#   ~PIT (Number of levels: 234) 
# Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sd(Intercept)     0.29      0.03     0.24     0.34 1.00     1719     2872
# 
# Regression Coefficients:
#   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept                                1.98      0.31     1.35     2.58 1.00     1401     2137
# speciesGRETI                            -0.22      0.22    -0.64     0.20 1.00     2022     2429
# speciesMARTI                             0.38      0.23    -0.07     0.83 1.00     2158     2706
# speciesNUTHA                             1.31      0.34     0.63     1.99 1.00     2549     2609
# seasonspring                            -0.61      0.31    -1.21     0.03 1.00     1452     2184
# seasonsummer                            -0.50      0.38    -1.24     0.25 1.00     1776     2279
# seasonwinter                            -1.91      0.32    -2.53    -1.27 1.00     1431     2223
# age_in_2020juvenile                     -0.12      0.13    -0.37     0.14 1.00     3211     3130
# scalebetweenness                        -0.02      0.17    -0.35     0.30 1.00     1927     2707
# scaledegree                              0.15      0.26    -0.36     0.67 1.00     1487     1795
# group.size                              -0.69      0.04    -0.77    -0.62 1.00     2666     2870
# speciesGRETI:seasonspring                0.10      0.18    -0.25     0.44 1.00     2165     2361
# speciesMARTI:seasonspring               -0.27      0.22    -0.69     0.16 1.00     2251     2606
# speciesNUTHA:seasonspring               -0.77      0.30    -1.37    -0.19 1.00     2613     3124
# speciesGRETI:seasonsummer                0.50      0.25     0.02     0.99 1.00     2742     3216
# speciesMARTI:seasonsummer                0.13      0.27    -0.41     0.66 1.00     2525     2869
# speciesNUTHA:seasonsummer               -0.12      0.33    -0.76     0.52 1.00     2754     2824
# speciesGRETI:seasonwinter                0.08      0.22    -0.36     0.48 1.00     1962     2389
# speciesMARTI:seasonwinter                0.27      0.23    -0.17     0.72 1.00     2334     2758
# speciesNUTHA:seasonwinter               -0.09      0.32    -0.72     0.53 1.00     2081     2785
# speciesGRETI:age_in_2020juvenile        -0.09      0.14    -0.37     0.18 1.00     3152     3030
# speciesMARTI:age_in_2020juvenile        -0.01      0.35    -0.71     0.67 1.00     4372     3233
# speciesNUTHA:age_in_2020juvenile        -0.02      0.31    -0.63     0.59 1.00     3378     2811
# seasonspring:scalebetweenness            0.08      0.14    -0.19     0.35 1.00     1886     2793
# seasonsummer:scalebetweenness            0.37      0.17     0.04     0.71 1.00     2682     3075
# seasonwinter:scalebetweenness            0.24      0.11     0.02     0.47 1.00     2036     3007
# speciesGRETI:scalebetweenness           -0.17      0.14    -0.44     0.11 1.00     3250     3317
# speciesMARTI:scalebetweenness           -0.00      0.13    -0.26     0.25 1.00     3065     3050
# speciesNUTHA:scalebetweenness           -0.22      0.20    -0.62     0.18 1.00     3032     2954
# age_in_2020juvenile:scalebetweenness    -0.12      0.08    -0.27     0.03 1.00     4903     3239
# seasonspring:scaledegree                -0.34      0.26    -0.85     0.17 1.00     1512     2330
# seasonsummer:scaledegree                -0.26      0.28    -0.81     0.29 1.00     1580     2476
# seasonwinter:scaledegree                -0.28      0.26    -0.77     0.22 1.00     1525     2428
# speciesGRETI:scaledegree                 0.13      0.09    -0.05     0.32 1.00     2719     3027
# speciesMARTI:scaledegree                 0.02      0.12    -0.20     0.25 1.00     2677     2962
# speciesNUTHA:scaledegree                 0.26      0.21    -0.16     0.68 1.00     3440     3174
# age_in_2020juvenile:scaledegree         -0.05      0.06    -0.17     0.05 1.00     5396     3477
# speciesGRETI:group.size                  0.02      0.02    -0.01     0.05 1.00     6316     3236
# speciesMARTI:group.size                 -0.11      0.02    -0.15    -0.06 1.00     5184     2987
# speciesNUTHA:group.size                 -0.25      0.04    -0.32    -0.18 1.00     5449     3094
# age_in_2020juvenile:group.size           0.05      0.01     0.02     0.08 1.00     6400     3160
# seasonspring:group.size                  0.19      0.04     0.11     0.26 1.00     2790     2831
# seasonsummer:group.size                  0.05      0.04    -0.03     0.13 1.00     3051     3089
# seasonwinter:group.size                  0.46      0.03     0.40     0.53 1.00     2838     2922
# 
# Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
# and Tail_ESS are effective sample size measures, and Rhat is the potential
# scale reduction factor on split chains (at convergence, Rhat = 1).

# remove non-significant interactions

#age.degree
#species.degree
#season.degree
#age.between
#species.between
#species.age

red_model <- brm(
  leader.follower ~  species * season + scale(betweenness) *
    season + + scale(degree) + group.size * species + group.size *
    age_in_2020 + group.size * season + (1 | PIT),
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
# Formula: leader.follower ~ species * season + scale(betweenness) * season + +scale(degree) + group.size * species + group.size * age_in_2020 + group.size * season + (1 | PIT) 
# Data: network.pos.all.seasons (Number of observations: 27163) 
# Draws: 2 chains, each with iter = 4000; warmup = 2000; thin = 1;
# total post-warmup draws = 4000
# 
# Multilevel Hyperparameters:
#   ~PIT (Number of levels: 234) 
# Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sd(Intercept)     0.28      0.03     0.23     0.33 1.00     2011     2915
# 
# Regression Coefficients:
#                                  Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept                          1.76      0.18     1.40     2.10 1.00     1098     1772
# speciesGRETI                      -0.29      0.16    -0.61     0.03 1.00     1415     2370
# speciesMARTI                       0.31      0.20    -0.07     0.70 1.00     1216     2072
# speciesNUTHA                       0.99      0.25     0.50     1.48 1.00     1178     1863
# seasonspring                      -0.33      0.18    -0.68     0.02 1.00     1125     2200
# seasonsummer                      -0.34      0.26    -0.85     0.17 1.00     1371     2388
# seasonwinter                      -1.76      0.18    -2.11    -1.39 1.00     1194     2017
# scalebetweenness                  -0.05      0.08    -0.21     0.11 1.00     1866     2256
# scaledegree                       -0.08      0.04    -0.15    -0.01 1.00     3150     3060
# group.size                        -0.69      0.04    -0.76    -0.62 1.00     1739     2558
# age_in_2020juvenile               -0.13      0.08    -0.29     0.03 1.00     3361     3022
# speciesGRETI:seasonspring          0.13      0.17    -0.20     0.45 1.00     1471     2521
# speciesMARTI:seasonspring         -0.16      0.20    -0.55     0.21 1.00     1301     2086
# speciesNUTHA:seasonspring         -0.52      0.27    -1.03     0.02 1.00     1517     2413
# speciesGRETI:seasonsummer          0.55      0.25     0.06     1.05 1.00     1552     2638
# speciesMARTI:seasonsummer          0.25      0.28    -0.28     0.78 1.00     1461     2354
# speciesNUTHA:seasonsummer          0.03      0.29    -0.55     0.59 1.00     1338     2191
# speciesGRETI:seasonwinter          0.23      0.17    -0.11     0.56 1.00     1371     2223
# speciesMARTI:seasonwinter          0.45      0.19     0.09     0.82 1.00     1238     1908
# speciesNUTHA:seasonwinter          0.27      0.24    -0.20     0.73 1.00     1213     2139
# seasonspring:scalebetweenness      0.03      0.10    -0.18     0.23 1.00     2462     2811
# seasonsummer:scalebetweenness      0.24      0.13    -0.02     0.50 1.00     2867     3040
# seasonwinter:scalebetweenness      0.16      0.08    -0.01     0.32 1.00     1862     2372
# speciesGRETI:group.size            0.02      0.02    -0.01     0.05 1.00     4355     3474
# speciesMARTI:group.size           -0.11      0.02    -0.15    -0.07 1.00     3988     3015
# speciesNUTHA:group.size           -0.24      0.03    -0.31    -0.17 1.00     4940     3293
# group.size:age_in_2020juvenile     0.04      0.01     0.02     0.07 1.00     4416     3104
# seasonspring:group.size            0.19      0.04     0.11     0.26 1.00     2018     2826
# seasonsummer:group.size            0.05      0.04    -0.03     0.13 1.00     2136     2735
# seasonwinter:group.size            0.46      0.03     0.40     0.53 1.00     1977     2653
# 
# Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
# and Tail_ESS are effective sample size measures, and Rhat is the potential
# scale reduction factor on split chains (at convergence, Rhat = 1).

# remove non-significant interaction: season-betweenness (newly non-significant)

red_model <- brm(
  leader.follower ~  species * season + scale(betweenness) + scale(degree) + group.size * species + group.size *
    age_in_2020 + group.size * season + (1 | PIT),
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
# Formula: leader.follower ~ species * season + scale(betweenness) + scale(degree) + group.size * species + group.size * age_in_2020 + group.size * season + (1 | PIT) 
# Data: network.pos.all.seasons (Number of observations: 27163) 
# Draws: 2 chains, each with iter = 4000; warmup = 2000; thin = 1;
# total post-warmup draws = 4000
# 
# Multilevel Hyperparameters:
#   ~PIT (Number of levels: 234) 
# Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sd(Intercept)     0.27      0.02     0.22     0.32 1.00     2288     3020
# 
# Regression Coefficients:
#                                  Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept                          1.83      0.17     1.50     2.16 1.00     1384     2351
# speciesGRETI                      -0.30      0.16    -0.62     0.02 1.00     1544     2126
# speciesMARTI                       0.19      0.18    -0.16     0.53 1.00     1444     2168
# speciesNUTHA                       0.84      0.23     0.38     1.29 1.00     1917     2773
# seasonspring                      -0.38      0.18    -0.73    -0.05 1.00     1316     2453
# seasonsummer                      -0.46      0.25    -0.95     0.04 1.00     1525     2453
# seasonwinter                      -1.85      0.18    -2.20    -1.50 1.00     1414     2536
# scalebetweenness                   0.07      0.03     0.01     0.13 1.00     3903     3052
# scaledegree                       -0.07      0.04    -0.14     0.00 1.00     3579     3214
# group.size                        -0.69      0.04    -0.76    -0.62 1.00     2039     2577
# age_in_2020juvenile               -0.13      0.08    -0.28     0.03 1.00     2636     2897
# speciesGRETI:seasonspring          0.15      0.16    -0.17     0.46 1.00     1500     2002
# speciesMARTI:seasonspring         -0.11      0.18    -0.46     0.25 1.00     1610     2524
# speciesNUTHA:seasonspring         -0.30      0.25    -0.78     0.19 1.00     2174     2995
# speciesGRETI:seasonsummer          0.56      0.24     0.07     1.03 1.00     1631     2387
# speciesMARTI:seasonsummer          0.44      0.25    -0.04     0.92 1.00     1680     2580
# speciesNUTHA:seasonsummer          0.20      0.27    -0.34     0.72 1.00     1781     2533
# speciesGRETI:seasonwinter          0.24      0.17    -0.11     0.56 1.00     1580     2426
# speciesMARTI:seasonwinter          0.62      0.17     0.27     0.95 1.00     1602     1987
# speciesNUTHA:seasonwinter          0.49      0.21     0.05     0.89 1.00     1940     2870
# speciesGRETI:group.size            0.02      0.02    -0.01     0.05 1.00     4369     3470
# speciesMARTI:group.size           -0.10      0.02    -0.15    -0.06 1.00     4271     3405
# speciesNUTHA:group.size           -0.25      0.04    -0.32    -0.18 1.00     5305     3405
# group.size:age_in_2020juvenile     0.04      0.01     0.02     0.07 1.00     4533     3013
# seasonspring:group.size            0.19      0.04     0.11     0.26 1.00     2074     2539
# seasonsummer:group.size            0.05      0.04    -0.02     0.13 1.00     2331     2903
# seasonwinter:group.size            0.46      0.03     0.40     0.53 1.00     2068     2504
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


# SW: here is where you can make some nice plots
plot(conditional_effects(red_model, re_formula = NULL))
# you can check my code from the ontogeny of social networks for some input on how to make plots from brms objects using ggplot
# a 0 here means to be a follower (first in the alphabet)


library(performance)
# extract an R2 for our model
performance::r2_bayes(red_model)
# Bayesian R2 with Compatibility Interval
# 
# Conditional R2: 0.279 (95% CI [0.271, 0.286])
# Marginal R2: 0.266 (95% CI [0.254, 0.277])


#another way to check:
#EZ: unique(network.pos.all.seasons$leader.follower)
#the output gives me: Levels: follower leader 
#This shows me that the model codes 0 as follower and 1 as leader. So an observation that at the y axis (leader.follower) has 0.7 this means that that observation has a 70% chance to be a leader.  

#EZ: the estimates are expressed in their logit scale so here I will transform them into odds ratio's
exp(fixef(red_model))
#                               Estimate Est.Error      Q2.5     Q97.5
#Intercept                      6.2360245  1.183515 4.4729000 8.6347363
#speciesGRETI                   0.7414825  1.174583 0.5405447 1.0153580
#speciesMARTI                   1.2131941  1.197297 0.8505379 1.7026213
#speciesNUTHA                   2.3099318  1.259745 1.4561138 3.6398616
#seasonspring                   0.6811412  1.192938 0.4812093 0.9556056
#seasonsummer                   0.6336102  1.286639 0.3873332 1.0392139
#seasonwinter                   0.1567820  1.198130 0.1113226 0.2236494
#scalebetweenness               1.0701891  1.031872 1.0075880 1.1388003
#scaledegree                    0.9329881  1.037965 0.8677520 1.0028921
#group.size                     0.5007497  1.035680 0.4675892 0.5365744
#age_in_2020juvenile            0.8818019  1.082190 0.7524324 1.0293745
#speciesGRETI:seasonspring      1.1598376  1.177471 0.8395441 1.5793571
#speciesMARTI:seasonspring      0.8927832  1.198392 0.6326125 1.2786754
#speciesNUTHA:seasonspring      0.7418912  1.286254 0.4585684 1.2149408
#speciesGRETI:seasonsummer      1.7556040  1.273339 1.0676062 2.8139627
#speciesMARTI:seasonsummer      1.5593494  1.282125 0.9615748 2.5178782
#speciesNUTHA:seasonsummer      1.2215843  1.311744 0.7132434 2.0472639
#speciesGRETI:seasonwinter      1.2663062  1.184704 0.8984816 1.7475260
#speciesMARTI:seasonwinter      1.8525636  1.186825 1.3146408 2.5799669
#speciesNUTHA:seasonwinter      1.6243868  1.234344 1.0534402 2.4295114
#speciesGRETI:group.size        1.0238100  1.016111 0.9925374 1.0563734
#speciesMARTI:group.size        0.9003698  1.021554 0.8631160 0.9381772
#speciesNUTHA:group.size        0.7800924  1.036202 0.7250571 0.8361557
#group.size:age_in_2020juvenile 1.0446176  1.012107 1.0198476 1.0695567
#seasonspring:group.size        1.2034483  1.037620 1.1193526 1.2946633
#seasonsummer:group.size        1.0540326  1.040867 0.9761517 1.1404275
#seasonwinter:group.size        1.5904795  1.033548 1.4911585 1.6994103

#EZ: I want to see the difference between great tits and nuthaches for example. Because in the summary output of the model, only blue tits during autumn are the intercept

#Here are attempts to make a comparison between the seasons within each species
#not sure which is better

library(emmeans)
#attempt1
emm <- emmeans(red_model, ~ species | season)
pairs(emm, by = "species") #this gives me the IC interval to interpret (I think that if it contains zero, it is not significant)
#now, by adding type= response, I should have the logit transformation
emmip(emm, species ~ season, type = "response") +
  labs(
    y = "Estimated Probability (Leader-Follower)",
    x = "Season",
    title = "Leader-Follower Probability by Season and Species"
  )
emmeans_table <- summary(emmeans(red_model, ~ season | species, type = "response")) #adding type= response gives the logit transformed values of the y axis
print(emmeans_table) #this doesn't seem right

#attempt2
rg <- ref_grid(red_model)
em_species <- emmeans(rg, "species") 
summary(em_species)
#or with the interaction in mind
em_species_season <- emmeans(rg,  ~ species | season, type="response")
summary(em_species_season, point.est=mean)
contrast(em_species_season, method="pairwise")

#attempt3
#when transforming with the logit 
em_species_logit <- regrid(em_species_season,
                           transform = "logit")
contrast(em_species_logit, method = "pairwise")
#I think this is the best attempt as it contains the interaction and the logit transformation. In the paper :
#https://royalsocietypublishing.org/doi/full/10.1098/rsos.221344#d1e413
#they do the log(response variable) transformaiton within the brm function. 
#It seems that they already change the response variable with the logit scale as I am doing but later with the transform="logit" in the regrid function

##In both studies: they used a brm model and then just the emmeans package, without using the ref_grid. So I decide to follow their approach
#https://royalsocietypublishing.org/doi/full/10.1098/rstb.2022.0137#d1e1626
#https://royalsocietypublishing.org/doi/full/10.1098/rsos.221344#d1e413

#attempt4
em4 <- emmeans(red_model, ~ season | species, type = "response")
em_species_logit4 <- regrid(em4,
                            transform = "logit")
contrast(em_species_logit4, method = "pairwise")
#attempt5
em5 <- emmeans(red_model, ~ species | season)
pairs(em5)






#### repeatability of arrival (leader versus follower)

# we can use the function icc (intra class correlation coefficient) from package performance
performance::icc(red_model)

# # Intraclass Correlation Coefficient
# 
# Adjusted ICC: 0.022
# Unadjusted ICC: 0.012 

# looks like there is very low repeatability within individuals

#EZ: I prefer to use as the double check the version below (with the post.data)
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

# [1] 0.0219684

# same result as above!


#> ---- Extract the variance components and calculate repeatability ----
#> other method, same value
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
#0.02213161

#Calculate CI
rethinking::HPDI(repeatability, prob = 0.95)
# |0.95      0.95| 
#0.01509294 0.03026423 
#Our value does fall within the IC

#This last part was inspired by the study: https://datadryad.org/stash/dataset/doi:10.25338/B88P8W















# SW: here you could do those extra couple of analyses with the visitation data:

visits_all_season <- read.csv("data/visits_all_season.csv", row.names = 1)


# 1) visits ~ species*season + (1|PIT)
# this should tell use something about caching - caching species are expected to fly back and forth more often within one flock visit
head(visits_all_season) #group=flock
#visits are counts thus we consider a Poisson distribution
library(lme4)
glmer_visits <- glmer(visit ~ species*season + (1|PIT), family = poisson, data= visits_all_season) #the birds go eat in flocks, hence their PIT is nested in group
# SW: hmm I see the thought why you want to include group as a random effect, but the group as we use it is essentially a random integer - I think I would run the model without it.


summary(glmer_visits)#model failed to converge

#with brm:
library(brms)
brm_visits <- brm(
  visit ~ species*season + (1|PIT),
  family = poisson,
  data = visits_all_season,
  chains = 2,
  iter = 4000,
  cores = 4,
  seed=2
)

summary(brm_visits)


# Family: poisson 
# Links: mu = log 
# Formula: visit ~ species * season + (1 | PIT) 
# Data: visits_all_season (Number of observations: 26819) 
# Draws: 2 chains, each with iter = 4000; warmup = 2000; thin = 1;
# total post-warmup draws = 4000
# 
# Multilevel Hyperparameters:
#   ~PIT (Number of levels: 254) 
# Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sd(Intercept)     0.11      0.01     0.10     0.13 1.00     1538     2167
# 
# Regression Coefficients:
#   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept                     0.52      0.05     0.43     0.61 1.00      551     1001
# speciesGRETI                 -0.06      0.05    -0.17     0.05 1.00      599     1117
# speciesMARTI                  0.09      0.06    -0.03     0.21 1.00      682     1210
# speciesNUTHA                 -0.09      0.07    -0.23     0.05 1.00      581     1281
# seasonspring                 -0.20      0.05    -0.29    -0.11 1.00      531     1259
# seasonsummer                  0.03      0.07    -0.12     0.18 1.00      812     1636
# seasonwinter                 -0.30      0.05    -0.39    -0.20 1.00      531     1009
# speciesGRETI:seasonspring     0.09      0.06    -0.03     0.19 1.00      567     1359
# speciesMARTI:seasonspring     0.16      0.06     0.05     0.28 1.00      746     1664
# speciesNUTHA:seasonspring     0.24      0.08     0.08     0.40 1.00      945     1840
# speciesGRETI:seasonsummer     0.13      0.08    -0.03     0.29 1.00      845     1698
# speciesMARTI:seasonsummer    -0.01      0.08    -0.17     0.15 1.00      876     1843
# speciesNUTHA:seasonsummer    -0.12      0.09    -0.29     0.05 1.00      869     1853
# speciesGRETI:seasonwinter     0.04      0.06    -0.07     0.15 1.00      585     1219
# speciesMARTI:seasonwinter     0.18      0.05     0.07     0.28 1.00      603     1097
# speciesNUTHA:seasonwinter     0.25      0.07     0.12     0.38 1.00      735     1487
# 
# Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
# and Tail_ESS are effective sample size measures, and Rhat is the potential
# scale reduction factor on split chains (at convergence, Rhat = 1).

# check for over/under dispsersion

mean_visit <- mean(visits_all_season$visit)
var_visit <- var(visits_all_season$visit)
overdispersion_ratio <- var_visit / mean_visit
overdispersion_ratio
# [1] 0.7237296

# seems okay - we can stick with a poisson

pp_check(brm_visits, ndraws = 100)

# again - here room for some plots
plot(conditional_effects(brm_visits), re_formula = NULL)

#EZ: the estimates are expressed in their logit scale so here I will transform them into odds ratio's
exp(fixef(brm_visits))
#                          Estimate Est.Error      Q2.5     Q97.5
#Intercept                 1.6810983  1.047473 1.5307977 1.8381222
#speciesGRETI              0.9437601  1.055937 0.8470645 1.0515658
#speciesMARTI              1.0956541  1.062371 0.9661981 1.2366192
#speciesNUTHA              0.9117945  1.072400 0.7962509 1.0485899
#seasonspring              0.8183903  1.049575 0.7455680 0.8997160
#seasonsummer              1.0319820  1.076223 0.8882663 1.1918842
#seasonwinter              0.7437556  1.048169 0.6794740 0.8168139
#speciesGRETI:seasonspring 1.0897109  1.059251 0.9734231 1.2151324
#speciesMARTI:seasonspring 1.1787379  1.062215 1.0469839 1.3280472
#speciesNUTHA:seasonspring 1.2711452  1.083867 1.0815065 1.4862291
#speciesGRETI:seasonsummer 1.1351588  1.082347 0.9717953 1.3308017
#speciesMARTI:seasonsummer 0.9854760  1.084638 0.8405785 1.1577483
#speciesNUTHA:seasonsummer 0.8835419  1.090368 0.7470332 1.0520307
#speciesGRETI:seasonwinter 1.0423715  1.056838 0.9343634 1.1611997
#speciesMARTI:seasonwinter 1.1938795  1.055657 1.0752670 1.3250180
#speciesNUTHA:seasonwinter 1.2861171  1.068658 1.1301792 1.4592192


# SW: looks like there are some species/season differences!


# 2) number of feeders visited ~ species*season + (1|PIT)
# this will tell use about territoriality - the more territorial species should have fewer feeders that they use
# you will need to use this data frame:

network.pos.all.seasons
# the 'location' are the different feeders - so you will need to find a way to summarize it with columns:
# PIT, season, species, age, num.feeders.visited

head(network.pos.all.seasons)

library(dplyr)
subset_network.pos.all.seasons <- network.pos.all.seasons %>% select(PIT, season, species, age_in_2020, location)
#I want to add the column "num.feeders.visited" that contains the total number of feeders each bird visited
subset2_network.pos.all.seasons <- subset_network.pos.all.seasons %>%
  group_by(PIT) %>%
  mutate(num.feeders.visited = n_distinct(location)) %>%
  ungroup()

head(subset2_network.pos.all.seasons) #some rows are duplicates so I decide to use the function distinct() to eliminate this
subset2_network.pos.all.seasons <- subset2_network.pos.all.seasons %>%
  distinct()

hist(subset2_network.pos.all.seasons$num.feeders.visited) #poisson distribution

glmer_visited_feeders <- glmer(num.feeders.visited ~ species*season + (1|PIT), family = poisson, data= subset2_network.pos.all.seasons) #I no longer consider that the group is important for this question as the question is more linked to the territoriality of an individual (species) in general. What is the big picture of their territoriality?
summary(glmer_visited_feeders)
#the model failed to converge so I will try with brm

brm_visited_feeders <- brm(
  num.feeders.visited ~ species*season + (1|PIT),
  family = poisson,
  data =subset2_network.pos.all.seasons,
  chains = 2,
  iter = 4000,
  cores = 4,
  save_pars = save_pars(all = TRUE)
)

# check for over/under dispsersion

mean_feeders_visited <- mean(subset2_network.pos.all.seasons$num.feeders.visited)
var_feeders_visited <- var(subset2_network.pos.all.seasons$num.feeders.visited)
overdispersion_ratio <- var_feeders_visited / mean_feeders_visited
overdispersion_ratio
# [1] 0.5448532

# our model is underdispersed

# we try a binomial model with the maximum number of feeders included as trials (=6)
brm_visited_feeders <- brm(
  num.feeders.visited | trials(6) ~ species * season + (1 | PIT),
  family = binomial(),
  data = subset2_network.pos.all.seasons,
  chains = 2,
  iter = 4000,
  cores = 4,
  save_pars = save_pars(all = TRUE)
)

summary(brm_visited_feeders)
# 
# Family: binomial 
# Links: mu = logit 
# Formula: num.feeders.visited | trials(6) ~ species * season + (1 | PIT) 
# Data: subset2_network.pos.all.seasons (Number of observations: 683) 
# Draws: 2 chains, each with iter = 4000; warmup = 2000; thin = 1;
# total post-warmup draws = 4000
# 
# Multilevel Hyperparameters:
#   ~PIT (Number of levels: 234) 
# Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sd(Intercept)     0.64      0.05     0.54     0.75 1.00     1862     2705
# 
# Regression Coefficients:
#   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept                    -1.07      0.27    -1.60    -0.55 1.01      907     1576
# speciesGRETI                  0.34      0.30    -0.24     0.95 1.01      905     1716
# speciesMARTI                  0.55      0.39    -0.19     1.33 1.00     1219     1724
# speciesNUTHA                  0.03      0.54    -1.06     1.05 1.00     1562     2283
# seasonspring                 -0.00      0.29    -0.58     0.57 1.01      958     1734
# seasonsummer                  0.17      0.35    -0.54     0.85 1.01      990     1787
# seasonwinter                  0.07      0.28    -0.46     0.61 1.01      916     1682
# speciesGRETI:seasonspring    -0.06      0.33    -0.70     0.58 1.01     1021     1858
# speciesMARTI:seasonspring    -0.04      0.43    -0.86     0.80 1.00     1409     2242
# speciesNUTHA:seasonspring    -0.33      0.69    -1.68     1.02 1.00     1963     2737
# speciesGRETI:seasonsummer    -0.24      0.38    -1.00     0.51 1.01     1022     1769
# speciesMARTI:seasonsummer    -0.23      0.49    -1.19     0.74 1.01     1273     2063
# speciesNUTHA:seasonsummer    -0.33      0.62    -1.52     0.86 1.00     1577     2616
# speciesGRETI:seasonwinter    -0.13      0.31    -0.74     0.49 1.01      977     1658
# speciesMARTI:seasonwinter    -0.06      0.39    -0.81     0.70 1.00     1198     2038
# speciesNUTHA:seasonwinter    -0.28      0.59    -1.44     0.86 1.01     1630     2534
# 
# Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
# and Tail_ESS are effective sample size measures, and Rhat is the potential
# scale reduction factor on split chains (at convergence, Rhat = 1).
plot(conditional_effects(brm_visited_feeders), re_formula = NULL)
pp_check(brm_visits, ndraws = 100)

#EZ: the estimates are expressed in their logit scale so here I will transform them into odds ratio's
exp(fixef(brm_visited_feeders))
#                           Estimate Est.Error      Q2.5     Q97.5
#Intercept                 0.3437172  1.307035 0.2012589 0.5764168
#speciesGRETI              1.4058312  1.350440 0.7851912 2.5768413
#speciesMARTI              1.7312951  1.472959 0.8258750 3.7845579
#speciesNUTHA              1.0270962  1.723797 0.3462797 2.8592185
#seasonspring              0.9975586  1.331673 0.5606729 1.7732364
#seasonsummer              1.1799883  1.417223 0.5803926 2.3469515
#seasonwinter              1.0764972  1.320237 0.6333581 1.8484572
#speciesGRETI:seasonspring 0.9401974  1.387519 0.4969973 1.7894335
#speciesMARTI:seasonspring 0.9634001  1.535721 0.4248499 2.2307164
#speciesNUTHA:seasonspring 0.7202678  2.002280 0.1871803 2.7712765
#speciesGRETI:seasonsummer 0.7857776  1.465931 0.3674684 1.6665917
#speciesMARTI:seasonsummer 0.7962126  1.636502 0.3034658 2.0898122
#speciesNUTHA:seasonsummer 0.7191998  1.853582 0.2194186 2.3558108
#speciesGRETI:seasonwinter 0.8818258  1.369415 0.4790953 1.6385013
#speciesMARTI:seasonwinter 0.9406638  1.479777 0.4433652 2.0062292
#speciesNUTHA:seasonwinter 0.7535031  1.809710 0.2361637 2.3577343








#########Descriptive statistics
#for the leader-follower status
View(network.pos.all.seasons)
network.pos.all.seasons$species <- as.factor(network.pos.all.seasons$species)
nbr_species <- network.pos.all.seasons %>%
  distinct(PIT, species, age_in_2020) %>% # Keep only unique combinations of ID and species
  group_by(species, age_in_2020) %>%
  summarise(count = n()) 
66+144+15+9



#for visitation data
View(visits_all_season)
visits_all_season$species <- as.factor(visits_all_seasons$species)
nbr_species_visits <- visits_all_season %>%
  distinct(PIT, species) %>% # Keep only unique combinations of ID and species
  group_by(species) %>%
  summarise(count = n())
#not the same amount of individuals for this part