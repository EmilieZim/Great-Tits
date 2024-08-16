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


#####Check if the SN position can explain these patterns (I don't have spring)
#data
load("data/gmm.autumn.RData")
head(gmm.autumn) 
load("data/gmm.summer.RData")
head(gmm.summer)
load("data/gmm.winter.RData")
head(gmm.winter) 

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

##THE WEEKS
####make network with gbi: select the rows corresponding to each week.

#SUMMER
#14 weeks, hence 14 networks
###week1
which(metadata.summ$week == 1)#from row 1 to 138

gbi1.summ <- gbi.summ[1:138,]
# I will need rows 1:138 (for week 1), but I will need all the columns
# since these are the individuals
# Also, subset to individuals that have been seen at least x times so that we have more robust estimates for their network position. 
# we can for now set that to 5 
# we can do this by using column sums - i.e. we only include columns (individuals) with a column sum
# of at least 5

threshold <- 5
gbi1.sub <- gbi1[,colSums(gbi1.summ)>=threshold]
dim(gbi1.sub)
# that gives 138 rows and 12 columns (=individuals)
#So 12 individuals that have been seen at least 5 times in the first week. 

library(asnipe)
network_week1 <- get_network(gbi1.sub, data_format="GBI",
                             association_index="SRI")


net1 <- graph_from_adjacency_matrix(network_week1,mode= c("undirected"), diag=FALSE, weighted=TRUE)
net1_deg <- degree(net1)
Tag <- V(net1)$name

# Create a data frame with individual names and degree values and order of arrival on the feeders
centrality_table1 <- data.frame(
  PIT = Tag,
  degree = net1_deg)
dc <- merge (centrality_table1, sp_class_season, by.x= "PIT")
head(dc)

# Create a dataframe that includes degree, betweenness, the individuals and the order 
btw_1 <- betweenness(net1,v = V(net1),directed = F)
betweenness_table1 <- data.frame(
  PIT = Tag,
  betweenness = btw_1)

table_week1 <- merge(dc,betweenness_table1, by.x= "PIT" )
table_week1$Week <- 1 


###week2
which(metadata.summ$week == 2)#from row 139 to 559
gbi2 <- gbi.summ[139:559,] 
gbi2.sub <- gbi2[,colSums(gbi2)>=threshold]
network_week2 <- get_network(gbi2.sub, data_format="GBI",
                             association_index="SRI")  
net2 <- graph_from_adjacency_matrix(network_week2,mode= c("undirected"), diag=FALSE, weighted=TRUE)
net2_deg <- degree(net2)
Tag <- V(net2)$name

centrality_table2 <- data.frame(
  PIT = Tag,
  degree = net2_deg)
dc2 <- merge (centrality_table2, sp_class_season, by.x= "PIT")

btw_2 <- betweenness(net2,v = V(net2),directed = F)
betweenness_table2 <- data.frame(
  PIT = Tag,
  betweenness = btw_2)

table_week2 <- merge(dc2,betweenness_table2, by.x= "PIT" )
table_week2$Week <- 2 

####week3
which(metadata.summ$week == 3)
gbi3 <- gbi.summ[560:1388,]
gbi3.sub <- gbi3[,colSums(gbi3)>=threshold]
network_week3 <- get_network(gbi3.sub, data_format="GBI",
                             association_index="SRI")

net3 <- graph_from_adjacency_matrix(network_week3,mode= c("undirected"), diag=FALSE, weighted=TRUE)
net3_deg <- degree(net3)
Tag <- V(net3)$name

centrality_table3 <- data.frame(
  PIT = Tag,
  degree = net3_deg)
dc3 <- merge (centrality_table3, sp_class_season, by.x= "PIT")

btw_3 <- betweenness(net3,v = V(net3),directed = F)
betweenness_table3 <- data.frame(
  PIT = Tag,
  betweenness = btw_3)

table_week3 <- merge(dc3,betweenness_table3, by.x= "PIT" )
table_week3$Week <- 3 

###week4 
which(metadata.summ$week == 4)
gbi4 <- gbi.summ[1389:2309,]
gbi4.sub <- gbi4[,colSums(gbi4)>=threshold]
network_week4 <- get_network(gbi4.sub, data_format="GBI",
                             association_index="SRI")

net4 <- graph_from_adjacency_matrix(network_week4,mode= c("undirected"), diag=FALSE, weighted=TRUE)
net4_deg <- degree(net4)
Tag <- V(net4)$name
centrality_table4 <- data.frame(
  PIT = Tag,
  degree = net4_deg)

dc4 <- merge(centrality_table4, sp_class_season, by.x= "PIT")

btw_4 <- betweenness(net4,v = V(net4),directed = F)
betweenness_table4 <- data.frame(
  PIT = Tag,
  betweenness = btw_4)

table_week4 <- merge(dc4,betweenness_table4, by.x= "PIT" )
table_week4$Week <- 4 

###week5 
which(metadata.summ$week == 5)
gbi5 <- gbi.summ[2310:3044,]
gbi5.sub <- gbi5[,colSums(gbi5)>=threshold]

network_week5 <- get_network(gbi5.sub, data_format="GBI",
                             association_index="SRI")
net5 <- graph_from_adjacency_matrix(network_week5,mode= c("undirected"), diag=FALSE, weighted=TRUE)
net5_deg <- degree(net5)

Tag <- V(net5)$name
centrality_table5 <- data.frame(
  PIT = Tag,
  degree = net5_deg)

dc5 <- merge(centrality_table5, sp_class_season, by.x= "PIT")

btw_5 <- betweenness(net5,v = V(net5),directed = F)
betweenness_table5 <- data.frame(
  PIT = Tag,
  betweenness = btw_5)

table_week5 <- merge(dc5,betweenness_table5, by.x= "PIT" )
table_week5$Week <- 5 

###week6 
which(metadata.summ$week == 6)
gbi6 <- gbi.summ[3045:3999,]
gbi6.sub <- gbi6.summ[,colSums(gbi6)>=threshold]

network_week6 <- get_network(gbi6.sub, data_format="GBI",
                             association_index="SRI")
net6 <- graph_from_adjacency_matrix(network_week6,mode= c("undirected"), diag=FALSE, weighted=TRUE)
net6_deg <- degree(net6)

Tag <- V(net6)$name
centrality_table6 <- data.frame(
  PIT = Tag,
  degree = net6_deg)

dc6 <- merge(centrality_table6, sp_class_season, by.x= "PIT")

btw_6 <- betweenness(net6,v = V(net6),directed = F)
betweenness_table6 <- data.frame(
  PIT = Tag,
  betweenness = btw_6)
table_week6 <- merge(dc6,betweenness_table6, by.x= "PIT" )
table_week6$Week <- 6 

###week7 
which(metadata.summ$week == 7)
gbi7 <- gbi.summ[4000:4690,]
gbi7.sub <- gbi7[,colSums(gbi7)>=threshold]

network_week7 <- get_network(gbi7.sub, data_format="GBI",
                             association_index="SRI")

net7 <- graph_from_adjacency_matrix(network_week7,mode= c("undirected"), diag=FALSE, weighted=TRUE)
net7_deg <- degree(net7)

Tag <- V(net7)$name
centrality_table7 <- data.frame(
  PIT = Tag,
  degree = net7_deg)

dc7 <- merge(centrality_table7, sp_class_season, by.x= "PIT")

btw_7 <- betweenness(net7,v = V(net7),directed = F)
betweenness_table7 <- data.frame(
  PIT = Tag,
  betweenness = btw_7)

table_week7 <- merge(dc7,betweenness_table7, by.x= "PIT" )
table_week7$Week <- 7 

###week8 
which(metadata.summ$week == 8)
gbi8 <- gbi.summ[4692:5289,]
gbi8.sub <- gbi8[,colSums(gbi8)>=threshold]

network_week8 <- get_network(gbi8.sub, data_format="GBI",
                             association_index="SRI")
net8 <- graph_from_adjacency_matrix(network_week8,mode= c("undirected"), diag=FALSE, weighted=TRUE)
net8_deg <- degree(net8)

Tag <- V(net8)$name
centrality_table8 <- data.frame(
  PIT = Tag,
  degree = net8_deg)

dc8 <- merge(centrality_table8, sp_class_season, by.x= "PIT")

btw_8 <- betweenness(net8,v = V(net8),directed = F)
betweenness_table8 <- data.frame(
  PIT = Tag,
  betweenness = btw_8)

table_week8 <- merge(dc8,betweenness_table8, by.x= "PIT" )
table_week8$Week <- 8 

###week9 
which(metadata.summ$week == 9)
gbi9 <- gbi.summ[4290:5933,]
gbi9.sub <- gbi9[,colSums(gbi9)>=threshold]

network_week9 <- get_network(gbi9.sub, data_format="GBI",
                             association_index="SRI")
net9 <- graph_from_adjacency_matrix(network_week9,mode= c("undirected"), diag=FALSE, weighted=TRUE)
net9_deg <- degree(net9)

Tag <- V(net9)$name
centrality_table9 <- data.frame(
  PIT = Tag,
  degree = net9_deg)

dc9 <- merge(centrality_table9, sp_class_season, by.x= "PIT")

btw_9 <- betweenness(net9,v = V(net9),directed = F)
betweenness_table9 <- data.frame(
  PIT = Tag,
  betweenness = btw_9)

table_week9 <- merge(dc9,betweenness_table9, by.x= "PIT" )
table_week9$Week <- 9 

###week10
which(metadata.summ$week == 10)
gbi10 <- gbi.summ[5934:6879,]
gbi10.sub <- gbi10[,colSums(gbi10)>=threshold]

network_week10 <- get_network(gbi10.sub, data_format="GBI",
                              association_index="SRI")
net10 <- graph_from_adjacency_matrix(network_week10,mode= c("undirected"), diag=FALSE, weighted=TRUE)
net10_deg <- degree(net10)

Tag <- V(net10)$name
centrality_table10 <- data.frame(
  PIT = Tag,
  degree = net10_deg)

dc10 <- merge(centrality_table10, sp_class_season, by.x= "PIT")

btw_10 <- betweenness(net10,v = V(net10),directed = F)
betweenness_table10 <- data.frame(
  PIT = Tag,
  betweenness = btw_10)

table_week10 <- merge(dc10,betweenness_table10, by.x= "PIT" )
table_week10$Week <- 10 


###week11
which(metadata.summ$week == 11)
gbi11 <- gbi.summ[6880:7724,]
gbi11.sub <- gbi11[,colSums(gbi11)>=threshold]

network_week11 <- get_network(gbi11.sub, data_format="GBI",
                              association_index="SRI")
net11 <- graph_from_adjacency_matrix(network_week11,mode= c("undirected"), diag=FALSE, weighted=TRUE)
net11_deg <- degree(net11)

Tag <- V(net11)$name
centrality_table11 <- data.frame(
  PIT = Tag,
  degree = net11_deg)

dc11 <- merge(centrality_table11, sp_class_season, by.x= "PIT")

btw_11 <- betweenness(net11,v = V(net11),directed = F)
betweenness_table11 <- data.frame(
  PIT = Tag,
  betweenness = btw_11)

table_week11 <- merge(dc11,betweenness_table11, by.x= "PIT" )
table_week11$Week <- 11 

###week12
which(metadata.summ$week == 12)
gbi12 <- gbi.summ[7725:8428,]
gbi12.sub <- gbi12[,colSums(gbi12)>=threshold]

network_week12 <- get_network(gbi12.sub, data_format="GBI",
                              association_index="SRI")
net12 <- graph_from_adjacency_matrix(network_week12,mode= c("undirected"), diag=FALSE, weighted=TRUE)
net12_deg <- degree(net12)

Tag <- V(net12)$name
centrality_table12 <- data.frame(
  PIT = Tag,
  degree = net12_deg)

dc12 <- merge(centrality_table12, sp_class_season, by.x= "PIT")

btw_12 <- betweenness(net12,v = V(net12),directed = F)
betweenness_table12 <- data.frame(
  PIT = Tag,
  betweenness = btw_12)

table_week12 <- merge(dc12,betweenness_table12, by.x= "PIT" )
table_week12$Week <- 12 

###week13
which(metadata.summ$week == 13)
gbi13 <- gbi.summ[8429:9164,]
gbi13.sub <- gbi13[,colSums(gbi13)>=threshold]

network_week13 <- get_network(gbi13.sub, data_format="GBI",
                              association_index="SRI")
net13 <- graph_from_adjacency_matrix(network_week13,mode= c("undirected"), diag=FALSE, weighted=TRUE)
net13_deg <- degree(net13)

Tag <- V(net13)$name
centrality_table13 <- data.frame(
  PIT = Tag,
  degree = net13_deg)

dc13 <- merge(centrality_table13, sp_class_season, by.x= "PIT")

btw_13 <- betweenness(net13,v = V(net13),directed = F)
betweenness_table13 <- data.frame(
  PIT = Tag,
  betweenness = btw_13)

table_week13 <- merge(dc13,betweenness_table13, by.x= "PIT" )
table_week13$Week <- 13 

###week14
which(metadata.summ$week == 14)
gbi14 <- gbi.summ[9165:9913,]
gbi14.sub <- gbi14[,colSums(gbi14)>=threshold]

network_week14 <- get_network(gbi14.sub, data_format="GBI",
                              association_index="SRI")
net14 <- graph_from_adjacency_matrix(network_week14,mode= c("undirected"), diag=FALSE, weighted=TRUE)
net14_deg <- degree(net14)

Tag <- V(net14)$name
centrality_table14 <- data.frame(
  PIT = Tag,
  degree = net14_deg)

dc14 <- merge(centrality_table14, sp_class_season, by.x= "PIT")

btw_14 <- betweenness(net14,v = V(net14),directed = F)
betweenness_table14 <- data.frame(
  PIT = Tag,
  betweenness = btw_14)

table_week14 <- merge(dc14,betweenness_table14, by.x= "PIT" )
table_week14$Week <- 14


#Merge all weeks together to create one dataframe for the summer
table_week_summer <- rbind(table_week1, table_week2, table_week3, table_week4,table_week5, table_week6, table_week7, table_week8, table_week9, table_week10, table_week11, table_week12, table_week13, table_week14)


###AUTUMN
#week1
which(metadata.aut$week == 1)
gbi.aut.1 <- gbi.aut[1:104,]
threshold <- 5
gbi.aut.1.sub <- gbi.aut.1 [,colSums(gbi.aut.1 )>=threshold]

network.aut.week1 <- get_network(gbi.aut.1.sub, data_format="GBI",
                                 association_index="SRI")

net.aut.week1 <- graph_from_adjacency_matrix(network.aut.week1,mode= c("undirected"), diag=FALSE, weighted=TRUE)
net.aut.1_deg <- degree(net.aut.week1)

Tag <- V(net.aut.week1)$name
degree.aut_table1 <- data.frame(
  PIT = Tag,
  degree = net.aut.1_deg)

dc.aut1 <- merge(degree.aut_table1, sp_class_season, by.x= "PIT" )

btw.aut.1 <- betweenness(net.aut.week1,v = V(net.aut.week1),directed = F)
betweenness.aut_table1 <- data.frame(
  PIT = Tag,
  betweenness = btw.aut.1)

table.aut.week1 <- merge(dc.aut1,betweenness.aut_table1, by.x= "PIT" )
table.aut.week1$Week <- 1
head(table.aut.week1)

#week2
which(metadata.aut$week == 2)
gbi.aut.2 <- gbi.aut[105:203,]
threshold <- 5
gbi.aut.2.sub <- gbi.aut.2 [,colSums(gbi.aut.2 )>=threshold]

network.aut.week2 <- get_network(gbi.aut.2.sub, data_format="GBI",
                                 association_index="SRI")
net.aut.week2 <- graph_from_adjacency_matrix(network.aut.week2,mode= c("undirected"), diag=FALSE, weighted=TRUE)
net.aut.2_deg <- degree(net.aut.week2)

Tag <- V(net.aut.week2)$name
degree.aut_table2 <- data.frame(
  PIT = Tag,
  degree = net.aut.2_deg)

dc.aut2 <- merge(degree.aut_table2, sp_class_season, by.x= "PIT" )

btw.aut.2 <- betweenness(net.aut.week2,v = V(net.aut.week2),directed = F)
betweenness.aut_table2 <- data.frame(
  PIT = Tag,
  betweenness = btw.aut.2)

table.aut.week2 <- merge(dc.aut2,betweenness.aut_table2, by.x= "PIT" )
table.aut.week2$Week <- 2
head(table.aut.week2)

#week3
which(metadata.aut$week == 3)
gbi.aut.3 <- gbi.aut[204:371,]
threshold <- 5
gbi.aut.3.sub <- gbi.aut.3[,colSums(gbi.aut.3 )>=threshold]

network.aut.week3 <- get_network(gbi.aut.3.sub, data_format="GBI",
                                 association_index="SRI")
net.aut.week3 <- graph_from_adjacency_matrix(network.aut.week3,mode= c("undirected"), diag=FALSE, weighted=TRUE)
net.aut.3_deg <- degree(net.aut.week3)

Tag <- V(net.aut.week3)$name
degree.aut_table3 <- data.frame(
  PIT = Tag,
  degree = net.aut.3_deg)

dc.aut3 <- merge(degree.aut_table3, sp_class_season, by.x= "PIT" )

btw.aut.3 <- betweenness(net.aut.week3,v = V(net.aut.week3),directed = F)
betweenness.aut_table3 <- data.frame(
  PIT = Tag,
  betweenness = btw.aut.3)

table.aut.week3 <- merge(dc.aut3,betweenness.aut_table3, by.x= "PIT" )
table.aut.week3$Week <- 3
head(table.aut.week3)

###Merge all weeks together to create one dataframe for the summer
library(dplyr)
table_week_aut <- rbind(table.aut.week1, table.aut.week2, table.aut.week3)



###WINTER
#week1
which(metadata.wint$week == 1)
gbi.wint.1 <- gbi.wint[1:166,]
threshold <- 5
gbi.wint.1.sub <- gbi.wint.1 [,colSums(gbi.wint.1 )>=threshold]

network.wint.week1 <- get_network(gbi.wint.1.sub, data_format="GBI",
                                 association_index="SRI")

net.wint.week1 <- graph_from_adjacency_matrix(network.wint.week1,mode= c("undirected"), diag=FALSE, weighted=TRUE)
net.wint.1_deg <- degree(net.wint.week1)

Tag <- V(net.wint.week1)$name
degree.wint_table1 <- data.frame(
  PIT = Tag,
  degree = net.wint.1_deg)

dc.wint1 <- merge(degree.wint_table1, sp_class_season, by.x= "PIT" )

btw.wint.1 <- betweenness(net.wint.week1,v = V(net.wint.week1),directed = F)
betweenness.wint_table1 <- data.frame(
  PIT = Tag,
  betweenness = btw.wint.1)

table.wint.week1 <- merge(dc.wint1,betweenness.wint_table1, by.x= "PIT" )
table.wint.week1$Week <- 1
head(table.wint.week1)

#week2
which(metadata.wint$week == 2)
gbi.wint.2 <- gbi.wint[167:293,]
threshold <- 5
gbi.wint.2.sub <- gbi.wint.2 [,colSums(gbi.wint.2 )>=threshold]

network.wint.week2 <- get_network(gbi.wint.2.sub, data_format="GBI",
                                  association_index="SRI")
net.wint.week2 <- graph_from_adjacency_matrix(network.wint.week2,mode= c("undirected"), diag=FALSE, weighted=TRUE)
net.wint.2_deg <- degree(net.wint.week2)

Tag <- V(net.wint.week2)$name
degree.wint_table2 <- data.frame(
  PIT = Tag,
  degree = net.wint.2_deg)

dc.wint2 <- merge(degree.wint_table2, sp_class_season, by.x= "PIT" )

btw.wint.2 <- betweenness(net.wint.week2,v = V(net.wint.week2),directed = F)
betweenness.wint_table2 <- data.frame(
  PIT = Tag,
  betweenness = btw.wint.2)

table.wint.week2 <- merge(dc.wint2,betweenness.wint_table2, by.x= "PIT" )
table.wint.week2$Week <- 2
head(table.wint.week2)

#week3
which(metadata.wint$week == 3)
gbi.wint.3 <- gbi.wint[294:421,]
threshold <- 5
gbi.wint.3.sub <- gbi.wint.3 [,colSums(gbi.wint.3 )>=threshold]

network.wint.week3 <- get_network(gbi.wint.3.sub, data_format="GBI",
                                  association_index="SRI")
net.wint.week3 <- graph_from_adjacency_matrix(network.wint.week3,mode= c("undirected"), diag=FALSE, weighted=TRUE)
net.wint.3_deg <- degree(net.wint.week3)

Tag <- V(net.wint.week3)$name
degree.wint_table3 <- data.frame(
  PIT = Tag,
  degree = net.wint.3_deg)

dc.wint3 <- merge(degree.wint_table3, sp_class_season, by.x= "PIT" )

btw.wint.3 <- betweenness(net.wint.week3,v = V(net.wint.week3),directed = F)
betweenness.wint_table3 <- data.frame(
  PIT = Tag,
  betweenness = btw.wint.3)

table.wint.week3 <- merge(dc.wint3,betweenness.wint_table3, by.x= "PIT" )
table.wint.week3$Week <- 3
head(table.wint.week3)

###Merge all weeks together to create one dataframe for the winter
table_week_wint <- rbind(table.wint.week1, table.wint.week2, table.wint.week3)

###Merge all dataframes created above in rder to have one object with all the seasons, and the ind with their respective degree's, betwennesses and species
table_all <- rbind(table_week_summer, table_week_aut, table_week_wint)
head(table_all)


#Now that I have a table where I can see each ind (PIT) with its respective degree and betweenness and order of arrival,
#I can now see whether the SN position explains or not the order of arrival towards the feeders
#To test this, I will use models

###MODELS TO TEST SN POSITION ~ ORDER OF ARRIVAL

##All of these next models are too heavy for my computer :/ 
library(brms)
m1 <-
  brm(
    mvbind(degree, betweenness) ~ order + (1|PIT), data=table_all)


summary(m1)
pp_check(m1, resp="degree")
pp_check(m1, resp="betweenness")
# these look like pretty poor models

plot(m1)

#including weights
m2 <-
  brm(
    mvbind(degree|weights=weight, betweenness|weights=weight) ~ order*season + (1|PIT), data=table_all)


##Maybe interesting to include betweenness and degree in the very first models of this script. And thus use only one dataframe: table_all


####Maybe also interesting to look at the repeatbility again, in order to see whether the order of arrival of tits is something they ajust according to the circumstances or is it a behaviour that is not that flexible.
#We know that birds sample their environment when looking for food but also rely on other in order to collect social cues, but what if the foraging behaviour is also influenced by their personality




#####In the litterature, what has been done so far and is similar to this study
#https://www.sciencedirect.com/science/article/abs/pii/S0003347220301251
#https://www.nature.com/articles/s41598-017-00929-8
#https://royalsocietypublishing.org/doi/full/10.1098/rspb.2014.2804#d1e1575




