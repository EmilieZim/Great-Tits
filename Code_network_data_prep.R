# this code details the data preparation from raw data, but data sets ready for analysis can be loaded directly in the main code. 

#install.packages("asnipe")
library(asnipe)


# 1) load raw network data ----------------------------------------------

net.data.summer <- read.delim("Data/Mill.data.summer.txt", sep=" ", row.names = 1)
net.data.autumn <- read.delim("Data/Mill.data.autumn.txt", sep=",", row.names = 1)
net.data.winter <- read.delim("Data/Mill.data.winter.txt", sep=",", row.names = 1)
net.data.spring <- read.delim("Data/Mill.data.spring.txt", sep=",", row.names = 1)

# Date.Time = yymmddHHMMSS
# Antenna: A for auxiliary, M for main
# PIT = 10 digit alphanumeric code unique to each individual
# location = Mill1-Mill6 (6 feeders around study area)
# week = experimental week within each season (during each week, we collected data for 48 hours). summer has 14 weeks, all other season 3 weeks

# note that we subset the summer data to the final three weeks of data collection to be consistent with the other seasons (week 12-14)

net.data.summer <- subset(net.data.summer, net.data.summer$week %in% c(12,13,14))

# to each object, we add a unique identifier of location_day - this will speed up the gmm function

net.data.summer$loc_day <- paste(substr(net.data.summer$Date.Time, 1, 6), net.data.summer$location, sep="_")
net.data.autumn$loc_day <- paste(substr(net.data.autumn$Date.Time, 1, 6), net.data.autumn$location, sep="_")
net.data.winter$loc_day <- paste(substr(net.data.winter$Date.Time, 1, 6), net.data.winter$location, sep="_")
net.data.spring$loc_day <- paste(substr(net.data.spring$Date.Time, 1, 6), net.data.spring$location, sep="_")

# and load the species and age data
#species_age_sex
load("data/species_age_sex.RDA") 
#View(species_age_sex)
#save(species_age_sex.RDA, file="species_age_sex.RDA") 
head(species_age_sex)




# 2) Run gmm function to create flocks ------------------------------------


# run gmm function to assign birds to flocks based on their arrival times at the same feeders

# summer
gmm.summer <- gmmevents(
  time = net.data.summer$Date.Time,
  identity = net.data.summer$PIT,
  location = net.data.summer$loc_day,
  verbose = TRUE,
  splitGroups = TRUE
)

save(gmm.summer, file="data/gmm.summer.RData")


# autumn
gmm.autumn <- gmmevents(
  time = net.data.autumn$Date.Time,
  identity = net.data.autumn$PIT,
  location = net.data.autumn$loc_day,
  verbose = TRUE,
  splitGroups = TRUE
)

save(gmm.autumn, file="data/gmm.autumn.RData")

# winter
gmm.winter <- gmmevents(
  time = net.data.winter$Date.Time,
  identity = net.data.winter$PIT,
  location = net.data.winter$loc_day,
  verbose = TRUE,
  splitGroups = TRUE
)

save(gmm.winter, file="data/gmm.winter.RData")

# spring
gmm.spring <- gmmevents(
  time = net.data.spring$Date.Time,
  identity = net.data.spring$PIT,
  location = net.data.spring$loc_day,
  verbose = TRUE,
  splitGroups = TRUE
)

save(gmm.spring, file="data/gmm.spring.RData")


# 3) assign leader-follower and visits ------------------------------------



# 3.1) summer -------------------------------------------------------------


load("data/gmm.summer.RData")
net.data.summer <- read.delim("data/Mill.data.summer.txt", sep=" ", row.names = 1)
# subset net data summer to the final 3 weeks
net.data.summer <- subset(net.data.summer, net.data.summer$week %in% c(12,13,14))
head(net.data.summer)


gmm.summer$metadata$Location2 <- substr(gmm.summer$metadata$Location, 8, 13)

# for each group, we have the start and end time and a location
# we add a new column called 'group' that will be filled by using a function
net.data.summer[, "group"] <- NA
# we also extract the visit duration
net.data.summer[, "visit.duration"] <- NA
head(net.data.summer)


for(i in 1:length(gmm.summer$metadata$Start)){ # for each start time
  i.loc <- gmm.summer$metadata$Location2[i] # we extract the location
  rows.i <-  rownames(subset(net.data.summer, 
                             net.data.summer$Date.Time>=gmm.summer$metadata$Start[i] &
                               net.data.summer$Date.Time<=gmm.summer$metadata$End[i] &
                               net.data.summer$location == gmm.summer$metadata$Location2[i]))
  net.data.summer[rows.i, "group"] <- i
  net.data.summer[rows.i, "visit.duration"] <- 
  as.numeric(difftime(as.POSIXct(substr(gmm.summer$metadata$End[i],7,12), format="%H%M%S"),as.POSIXct(substr(gmm.summer$metadata$Start[i],7,12), format="%H%M%S")), units="mins")
}

net.data.summer[which(is.na(net.data.summer$group)),]

length(unique(net.data.summer$group))
# 2166 groups 

# make sure it's in correct order (first ordered by groups, then time)
net.data.summer <- net.data.summer[with(net.data.summer, order(group, Date.Time)), ]

# we create a new column called 'leader_follower' all filled with NA
net.data.summer$leader.follower <- NA
# to the very first visitor, we assign a 'leader'
net.data.summer$leader.follower[1] <- "leader"
# we also create a column 'visit' to count how many times the same bird flew back and forth in a given visit by the entire flock
# if it was absent for at least 5 seconds, we count is as a new visit
net.data.summer$visit <- NA
net.data.summer$visit[1] <- 1

# now we loop through the entire data frame
for(i in 2:length(net.data.summer$group)){
  # we extract the PIT tag, the group number, and visit time of bird i and the bird, group number and visit time that came just before
  group.i <- net.data.summer$group[i]
  ID.i <- net.data.summer$PIT[i]
  time.i <- net.data.summer$Date.Time[i]
  group.i_minus1 <- net.data.summer$group[i-1]
  ID.i_minus1 <- net.data.summer$PIT[i-1]
  time.i_minus1 <- net.data.summer$Date.Time[i-1]
  
  # first we look at the leader/follower assignment
  
  # we also don't want to assign a new leader.follower number, if the bird had already arrived as part of the current group, so we extract the PIT tags that are already part of the current group
  sub <- net.data.summer[1:(i-1),]
  sub <- subset(sub, sub$group==group.i)
  already.present <- unique(sub$PIT)
  
  if(ID.i %in% already.present){ # if the bird hard already arrived as part of the current group
    net.data.summer$leader.follower[i] <- 'already.present' # we assign 'already.present'
  } else if(group.i==group.i_minus1 & ID.i!=ID.i_minus1){
    # if it's the same group but a new bird, we assign 'follower'
    net.data.summer$leader.follower[i] <- 'follower'
  } else if(group.i!=group.i_minus1){
    # if it's a new group, we start over with the leader.follower=1
    net.data.summer$leader.follower[i] <- 'leader'
  }
  
  
  # then we look at the visits:
  # if the current entry i is by a bird already present in the groupthe same bird within the same group and within 5 s of the previous entry, it is considered the same visit. Otherwise a new visit (visit+1)
  if(group.i==group.i_minus1 & ID.i %in% already.present & as.numeric(difftime(as.POSIXct(substr(time.i,7,12), format="%H%M%S"),as.POSIXct(substr(time.i_minus1,7,12), format="%H%M%S")))<=5){
    net.data.summer$visit[i] <- max(na.omit(subset(net.data.summer$visit, net.data.summer$PIT==ID.i & net.data.summer$group==group.i)))
  } else if(group.i==group.i_minus1 & ID.i %in% already.present & as.numeric(difftime(as.POSIXct(substr(time.i,7,12), format="%H%M%S"),as.POSIXct(substr(time.i_minus1,7,12), format="%H%M%S")))>5){ # if it's the same group and the same ID, but more than 5 s, we assign its visit +1
    net.data.summer$visit[i] <- max(na.omit(subset(net.data.summer$visit, net.data.summer$PIT==ID.i & net.data.summer$group==group.i)))+1
  } else if(group.i==group.i_minus1 & !(ID.i %in% already.present )){ # if it's a new bird, then assign visit 1
    net.data.summer$visit[i] <- 1
  } else if(group.i!=group.i_minus1){
    net.data.summer$visit[i] <- 1
  }
}



# add the species and ages
sp_class_summ <- merge(net.data.summer, species_age_sex, by.x= "PIT")
str(sp_class_summ$species)
sp_class_summ$species <- as.factor(sp_class_summ$species)
sp_class_summ$age_in_2020 <- as.factor(sp_class_summ$age_in_2020)
# reduce to great tits, blue tits, marsh tits and nuthatches
sp_class_summ <- subset(sp_class_summ, sp_class_summ$species %in% c("BLUTI", "GRETI", "MARTI", "NUTHA"))
sp_class_summ$season <- "summer"

# extract the data frame for the visit data
summer.visit.df <- unique(sp_class_summ[,c("PIT", "visit", "species", "group", "season", "visit.duration")])
# save it
#save(summer.visit.df, file="summer.visit.df.RDA")


# for the leader follower data, leave out the "already present". 
sp_class_summ$leader.follower[sp_class_summ$leader.follower == "already.present"] <- 0
sp_class_summ <- subset(sp_class_summ, sp_class_summ$leader.follower!=0)
# remove the visit column (to make sure we do not use this one for the visitation data) 
sp_class_summ <- sp_class_summ[,!(names(sp_class_summ) %in% c("sex", "visit"))]


# 3.2) autumn -------------------------------------------------------------


load("data/gmm.autumn.RData")
net.data.autumn <- read.delim("data/Mill.data.autumn.txt", sep=",", row.names = 1)
head(net.data.autumn)

gmm.autumn$metadata$Location2 <- substr(gmm.autumn$metadata$Location, 8, 13)

# for each group, we have the start and end time and a location
# we add a new column called 'group' that will be filled by using a function
net.data.autumn[, "group"] <- NA
# we also extract the visit duration
net.data.autumn[, "visit.duration"] <- NA
head(net.data.autumn)


for(i in 1:length(gmm.autumn$metadata$Start)){ # for each start time
  i.loc <- gmm.autumn$metadata$Location2[i] # we extract the location
  rows.i <-  rownames(subset(net.data.autumn, 
                             net.data.autumn$Date.Time>=gmm.autumn$metadata$Start[i] &
                               net.data.autumn$Date.Time<=gmm.autumn$metadata$End[i] &
                               net.data.autumn$location == gmm.autumn$metadata$Location2[i]))
  net.data.autumn[rows.i, "group"] <- i
  net.data.autumn[rows.i, "visit.duration"] <- 
    as.numeric(difftime(as.POSIXct(substr(gmm.autumn$metadata$End[i],7,12), format="%H%M%S"),as.POSIXct(substr(gmm.autumn$metadata$Start[i],7,12), format="%H%M%S")), units="mins")
}

net.data.autumn <- net.data.autumn[which(!(is.na(net.data.autumn$group))),]

length(unique(net.data.autumn$group))
# 1579 groups remain

# make sure it's in correct order (first ordered by groups, then time)
net.data.autumn <- net.data.autumn[with(net.data.autumn, order(group, Date.Time)), ]

# we create a new column called 'leader_follower' all filled with NA
net.data.autumn$leader.follower <- NA
# to the very first visitor, we assign a 'leader'
net.data.autumn$leader.follower[1] <- "leader"
# we also create a column 'visit' to count how many times the same bird flew back and forth in a given visit by the entire flock
# if it was absent for at least 5 seconds, we count is as a new visit
net.data.autumn$visit <- NA
net.data.autumn$visit[1] <- 1

# now we loop through the entire data frame
for(i in 2:length(net.data.autumn$group)){
  # we extract the PIT tag, the group number, and visit time of bird i and the bird, group number and visit time that came just before
  group.i <- net.data.autumn$group[i]
  ID.i <- net.data.autumn$PIT[i]
  time.i <- net.data.autumn$Date.Time[i]
  group.i_minus1 <- net.data.autumn$group[i-1]
  ID.i_minus1 <- net.data.autumn$PIT[i-1]
  time.i_minus1 <- net.data.autumn$Date.Time[i-1]
  
  # first we look at the leader/follower assignment
  
  # we also don't want to assign a new leader.follower number, if the bird had already arrived as part of the current group, so we extract the PIT tags that are already part of the current group
  sub <- net.data.autumn[1:(i-1),]
  sub <- subset(sub, sub$group==group.i)
  already.present <- unique(sub$PIT)
  
  if(ID.i %in% already.present){ # if the bird hard already arrived as part of the current group
    net.data.autumn$leader.follower[i] <- 'already.present' # we assign 'already.present'
  } else if(group.i==group.i_minus1 & ID.i!=ID.i_minus1){
    # if it's the same group but a new bird, we assign 'follower'
    net.data.autumn$leader.follower[i] <- 'follower'
  } else if(group.i!=group.i_minus1){
    # if it's a new group, we start over with the leader.follower=1
    net.data.autumn$leader.follower[i] <- 'leader'
  }
  
  
  # then we look at the visits:
  # if the current entry i is by a bird already present in the groupthe same bird within the same group and within 5 s of the previous entry, it is considered the same visit. Otherwise a new visit (visit+1)
  if(group.i==group.i_minus1 & ID.i %in% already.present & as.numeric(difftime(as.POSIXct(substr(time.i,7,12), format="%H%M%S"),as.POSIXct(substr(time.i_minus1,7,12), format="%H%M%S")))<=5){
    net.data.autumn$visit[i] <- max(na.omit(subset(net.data.autumn$visit, net.data.autumn$PIT==ID.i & net.data.autumn$group==group.i)))
  } else if(group.i==group.i_minus1 & ID.i %in% already.present & as.numeric(difftime(as.POSIXct(substr(time.i,7,12), format="%H%M%S"),as.POSIXct(substr(time.i_minus1,7,12), format="%H%M%S")))>5){ # if it's the same group and the same ID, but more than 5 s, we assign its visit +1
    net.data.autumn$visit[i] <- max(na.omit(subset(net.data.autumn$visit, net.data.autumn$PIT==ID.i & net.data.autumn$group==group.i)))+1
  } else if(group.i==group.i_minus1 & !(ID.i %in% already.present )){ # if it's a new bird, then assign visit 1
    net.data.autumn$visit[i] <- 1
  } else if(group.i!=group.i_minus1){
    net.data.autumn$visit[i] <- 1
  }
}


# add the species and ages
sp_class_aut <- merge(net.data.autumn, species_age_sex, by.x= "PIT")
str(sp_class_aut$species)
sp_class_aut$species <- as.factor(sp_class_aut$species)
sp_class_aut$age_in_2020 <- as.factor(sp_class_aut$age_in_2020)
# reduce to great tits, blue tits, marsh tits and nuthatches
sp_class_aut <- subset(sp_class_aut, sp_class_aut$species %in% c("BLUTI", "GRETI", "MARTI", "NUTHA"))
sp_class_aut$season <- "autumn"

# extract the data frame for the visit data
autumn.visit.df <- unique(sp_class_aut[,c("PIT", "visit", "species", "group", "season", "visit.duration")])
# save it
#save(autumn.visit.df, file="autumn.visit.df.RDA")


# for the leader follower data, leave out the "already present". 
sp_class_aut$leader.follower[sp_class_aut$leader.follower == "already.present"] <- 0
sp_class_aut <- subset(sp_class_aut, sp_class_aut$leader.follower!=0)
# remove the visit column (to make sure we do not use this one for the visitation data) 
sp_class_aut <- sp_class_aut[,!(names(sp_class_aut) %in% c("sex", "visit"))]


# 3.3) winter -------------------------------------------------------------



load("data/gmm.winter.RData")
net.data.winter <- read.delim("data/Mill.data.winter.txt", sep=",", row.names = 1)
head(net.data.winter)


gmm.winter$metadata$Location2 <- substr(gmm.winter$metadata$Location, 8, 13)

# for each group, we have the start and end time and a location
# we add a new column called 'group' that will be filled by using a function
net.data.winter[, "group"] <- NA
# we also extract the visit duration
net.data.winter[, "visit.duration"] <- NA
head(net.data.winter)


for(i in 1:length(gmm.winter$metadata$Start)){ # for each start time
  i.loc <- gmm.winter$metadata$Location2[i] # we extract the location
  rows.i <-  rownames(subset(net.data.winter, 
                             net.data.winter$Date.Time>=gmm.winter$metadata$Start[i] &
                               net.data.winter$Date.Time<=gmm.winter$metadata$End[i] &
                               net.data.winter$location == gmm.winter$metadata$Location2[i]))
  net.data.winter[rows.i, "group"] <- i
  net.data.winter[rows.i, "visit.duration"] <- 
    as.numeric(difftime(as.POSIXct(substr(gmm.winter$metadata$End[i],7,12), format="%H%M%S"),as.POSIXct(substr(gmm.winter$metadata$Start[i],7,12), format="%H%M%S")), units="mins")
}

net.data.winter <- net.data.winter[which(!(is.na(net.data.winter$group))),]

length(unique(net.data.winter$group))
# 2867 groups remain

# make sure it's in correct order (first ordered by groups, then time)
net.data.winter <- net.data.winter[with(net.data.winter, order(group, Date.Time)), ]

# we create a new column called 'leader_follower' all filled with NA
net.data.winter$leader.follower <- NA
# to the very first visitor, we assign a 'leader'
net.data.winter$leader.follower[1] <- "leader"
# we also create a column 'visit' to count how many times the same bird flew back and forth in a given visit by the entire flock
# if it was absent for at least 5 seconds, we count is as a new visit
net.data.winter$visit <- NA
net.data.winter$visit[1] <- 1

# now we loop through the entire data frame
for(i in 2:length(net.data.winter$group)){
  # we extract the PIT tag, the group number, and visit time of bird i and the bird, group number and visit time that came just before
  group.i <- net.data.winter$group[i]
  ID.i <- net.data.winter$PIT[i]
  time.i <- net.data.winter$Date.Time[i]
  group.i_minus1 <- net.data.winter$group[i-1]
  ID.i_minus1 <- net.data.winter$PIT[i-1]
  time.i_minus1 <- net.data.winter$Date.Time[i-1]
  
  # first we look at the leader/follower assignment
  
  # we also don't want to assign a new leader.follower number, if the bird had already arrived as part of the current group, so we extract the PIT tags that are already part of the current group
  sub <- net.data.winter[1:(i-1),]
  sub <- subset(sub, sub$group==group.i)
  already.present <- unique(sub$PIT)
  
  if(ID.i %in% already.present){ # if the bird hard already arrived as part of the current group
    net.data.winter$leader.follower[i] <- 'already.present' # we assign 'already.present'
  } else if(group.i==group.i_minus1 & ID.i!=ID.i_minus1){
    # if it's the same group but a new bird, we assign 'follower'
    net.data.winter$leader.follower[i] <- 'follower'
  } else if(group.i!=group.i_minus1){
    # if it's a new group, we start over with the leader.follower=1
    net.data.winter$leader.follower[i] <- 'leader'
  }
  
  
  # then we look at the visits:
  # if the current entry i is by a bird already present in the groupthe same bird within the same group and within 5 s of the previous entry, it is considered the same visit. Otherwise a new visit (visit+1)
  if(group.i==group.i_minus1 & ID.i %in% already.present & as.numeric(difftime(as.POSIXct(substr(time.i,7,12), format="%H%M%S"),as.POSIXct(substr(time.i_minus1,7,12), format="%H%M%S")))<=5){
    net.data.winter$visit[i] <- max(na.omit(subset(net.data.winter$visit, net.data.winter$PIT==ID.i & net.data.winter$group==group.i)))
  } else if(group.i==group.i_minus1 & ID.i %in% already.present & as.numeric(difftime(as.POSIXct(substr(time.i,7,12), format="%H%M%S"),as.POSIXct(substr(time.i_minus1,7,12), format="%H%M%S")))>5){ # if it's the same group and the same ID, but more than 5 s, we assign its visit +1
    net.data.winter$visit[i] <- max(na.omit(subset(net.data.winter$visit, net.data.winter$PIT==ID.i & net.data.winter$group==group.i)))+1
  } else if(group.i==group.i_minus1 & !(ID.i %in% already.present )){ # if it's a new bird, then assign visit 1
    net.data.winter$visit[i] <- 1
  } else if(group.i!=group.i_minus1){
    net.data.winter$visit[i] <- 1
  }
}



# add the species and ages
sp_class_wint <- merge(net.data.winter, species_age_sex, by.x= "PIT")
str(sp_class_wint$species)
sp_class_wint$species <- as.factor(sp_class_wint$species)
sp_class_wint$age_in_2020 <- as.factor(sp_class_wint$age_in_2020)
# reduce to great tits, blue tits, marsh tits and nuthatches
sp_class_wint <- subset(sp_class_wint, sp_class_wint$species %in% c("BLUTI", "GRETI", "MARTI", "NUTHA"))
sp_class_wint$season <- "winter"

# extract the data frame for the visit data
winter.visit.df <- unique(sp_class_wint[,c("PIT", "visit", "species", "group", "season", "visit.duration")])
# save it
#save(winter.visit.df, file="winter.visit.df.RDA")


# for the leader follower data, leave out the "already present". 
sp_class_wint$leader.follower[sp_class_wint$leader.follower == "already.present"] <- 0
sp_class_wint <- subset(sp_class_wint, sp_class_wint$leader.follower!=0)
# remove the visit column (to make sure we do not use this one for the visitation data) 
sp_class_wint <- sp_class_wint[,!(names(sp_class_wint) %in% c("sex", "visit"))]


# 3.4) spring -------------------------------------------------------------


load("data/gmm.spring.RData")
net.data.spring <- read.delim("data/Mill.data.spring.txt", sep=",", row.names = 1)
head(net.data.spring)


gmm.spring$metadata$Location2 <- substr(gmm.spring$metadata$Location, 8, 13)

# for each group, we have the start and end time and a location
# we add a new column called 'group' that will be filled by using a function
net.data.spring[, "group"] <- NA
# we also extract the visit duration
net.data.spring[, "visit.duration"] <- NA
head(net.data.spring)


for(i in 1:length(gmm.spring$metadata$Start)){ # for each start time
  i.loc <- gmm.spring$metadata$Location2[i] # we extract the location
  rows.i <-  rownames(subset(net.data.spring, 
                             net.data.spring$Date.Time>=gmm.spring$metadata$Start[i] &
                               net.data.spring$Date.Time<=gmm.spring$metadata$End[i] &
                               net.data.spring$location == gmm.spring$metadata$Location2[i]))
  net.data.spring[rows.i, "group"] <- i
  net.data.spring[rows.i, "visit.duration"] <- 
    as.numeric(difftime(as.POSIXct(substr(gmm.spring$metadata$End[i],7,12), format="%H%M%S"),as.POSIXct(substr(gmm.spring$metadata$Start[i],7,12), format="%H%M%S")), units="mins")
}

net.data.spring[which(is.na(net.data.spring$group)),]

length(unique(net.data.spring$group))
# 2389 groups remain

# make sure it's in correct order (first ordered by groups, then time)
net.data.spring <- net.data.spring[with(net.data.spring, order(group, Date.Time)), ]

# we create a new column called 'leader_follower' all filled with NA
net.data.spring$leader.follower <- NA
# to the very first visitor, we assign a 'leader'
net.data.spring$leader.follower[1] <- "leader"
# we also create a column 'visit' to count how many times the same bird flew back and forth in a given visit by the entire flock
# if it was absent for at least 5 seconds, we count is as a new visit
net.data.spring$visit <- NA
net.data.spring$visit[1] <- 1

# now we loop through the entire data frame
for(i in 2:length(net.data.spring$group)){
  # we extract the PIT tag, the group number, and visit time of bird i and the bird, group number and visit time that came just before
  group.i <- net.data.spring$group[i]
  ID.i <- net.data.spring$PIT[i]
  time.i <- net.data.spring$Date.Time[i]
  group.i_minus1 <- net.data.spring$group[i-1]
  ID.i_minus1 <- net.data.spring$PIT[i-1]
  time.i_minus1 <- net.data.spring$Date.Time[i-1]
  
  # first we look at the leader/follower assignment
  
  # we also don't want to assign a new leader.follower number, if the bird had already arrived as part of the current group, so we extract the PIT tags that are already part of the current group
  sub <- net.data.spring[1:(i-1),]
  sub <- subset(sub, sub$group==group.i)
  already.present <- unique(sub$PIT)
  
  if(ID.i %in% already.present){ # if the bird hard already arrived as part of the current group
    net.data.spring$leader.follower[i] <- 'already.present' # we assign 'already.present'
  } else if(group.i==group.i_minus1 & ID.i!=ID.i_minus1){
    # if it's the same group but a new bird, we assign 'follower'
    net.data.spring$leader.follower[i] <- 'follower'
  } else if(group.i!=group.i_minus1){
    # if it's a new group, we start over with the leader.follower=1
    net.data.spring$leader.follower[i] <- 'leader'
  }
  
  
  # then we look at the visits:
  # if the current entry i is by a bird already present in the groupthe same bird within the same group and within 5 s of the previous entry, it is considered the same visit. Otherwise a new visit (visit+1)
  if(group.i==group.i_minus1 & ID.i %in% already.present & as.numeric(difftime(as.POSIXct(substr(time.i,7,12), format="%H%M%S"),as.POSIXct(substr(time.i_minus1,7,12), format="%H%M%S")))<=5){
    net.data.spring$visit[i] <- max(na.omit(subset(net.data.spring$visit, net.data.spring$PIT==ID.i & net.data.spring$group==group.i)))
  } else if(group.i==group.i_minus1 & ID.i %in% already.present & as.numeric(difftime(as.POSIXct(substr(time.i,7,12), format="%H%M%S"),as.POSIXct(substr(time.i_minus1,7,12), format="%H%M%S")))>5){ # if it's the same group and the same ID, but more than 5 s, we assign its visit +1
    net.data.spring$visit[i] <- max(na.omit(subset(net.data.spring$visit, net.data.spring$PIT==ID.i & net.data.spring$group==group.i)))+1
  } else if(group.i==group.i_minus1 & !(ID.i %in% already.present )){ # if it's a new bird, then assign visit 1
    net.data.spring$visit[i] <- 1
  } else if(group.i!=group.i_minus1){
    net.data.spring$visit[i] <- 1
  }
}



# add the species and ages
sp_class_spr <- merge(net.data.spring, species_age_sex, by.x= "PIT")
str(sp_class_spr$species)
sp_class_spr$species <- as.factor(sp_class_spr$species)
sp_class_spr$age_in_2020 <- as.factor(sp_class_spr$age_in_2020)
# reduce to great tits, blue tits, marsh tits and nuthatches
sp_class_spr <- subset(sp_class_spr, sp_class_spr$species %in% c("BLUTI", "GRETI", "MARTI", "NUTHA"))
sp_class_spr$season <- "spring"

# extract the data frame for the visit data
spring.visit.df <- unique(sp_class_spr[,c("PIT", "visit", "species", "group", "season", "visit.duration")])
# save it
#save(spring.visit.df, file="spring.visit.df.RDA")


# for the leader follower data, leave out the "already present". 
sp_class_spr$leader.follower[sp_class_spr$leader.follower == "already.present"] <- 0
sp_class_spr <- subset(sp_class_spr, sp_class_spr$leader.follower!=0)
# remove the visit column (to make sure we do not use this one for the visitation data) 
sp_class_spr <- sp_class_spr[,!(names(sp_class_spr) %in% c("sex", "visit"))]


# 3.5) combine follower leader data ----------------------------------------

sp_class_season <- rbind(sp_class_summ, sp_class_aut, sp_class_wint, sp_class_spr)

visits_all_season <- rbind(summer.visit.df, autumn.visit.df, winter.visit.df, spring.visit.df)

# only retain the max number of visits for each bird in each group

# Load the dplyr package
library(dplyr)

# Assuming your data frame is called 'df'
visits_all_season <- visits_all_season %>%
  dplyr::group_by(PIT, group) %>%
  filter(visit == max(visit)) %>%
  ungroup()



write.csv(visits_all_season, file="data/visits_all_season.csv")
read.csv("data/visits_all_season.csv")

# 4) calculate network position -------------------------------------------

# this automatically adds the social network position calculated across the three weeks each season for birds with a minimum of 5 visits
library(igraph)
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

# apply the function to each season
network.pos.summer <- calculate.soc.network.pos(gbi=gmm.summer$gbi, metadata = gmm.summer$metadata, threshold=5, season="summer")
network.pos.autumn <- calculate.soc.network.pos(gbi=gmm.autumn$gbi, metadata = gmm.autumn$metadata, threshold=5, season="autumn")
network.pos.winter <- calculate.soc.network.pos(gbi=gmm.winter$gbi, metadata = gmm.winter$metadata, threshold=5, season="winter")
network.pos.spring <- calculate.soc.network.pos(gbi=gmm.spring$gbi, metadata = gmm.spring$metadata, threshold=5, season="spring")



# 5) Calculate group size -------------------------------------------------

# here is a function that extracts group sizes - we need to control for this in the model
extract.group.size <- function(network.pos.object, net.data){
  for(i in sort(unique(network.pos.object$group))){
    
    network.pos.object[which(network.pos.object$group==i), "group.size"] <-  length(unique(subset(net.data$PIT, net.data$group==i)))
    
    
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

network.pos.all.seasons$group.size <- as.numeric(network.pos.all.seasons$group.size)
network.pos.all.seasons$leader.follower <- as.factor(network.pos.all.seasons$leader.follower)

write.csv(network.pos.all.seasons, file="data/leader_follower_data_all_seasons.csv")

