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

# NOTE: these are computationally intense - we therefore provide the objects. they can be loaded below

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

library(dplyr)
library(lubridate)

<<<<<<< HEAD
# Step 1: Parse Date.Time correctly
net.data.summer <- net.data.summer %>%
  mutate(Date.Time = strptime(Date.Time, format = "%y%m%d%H%M%S", tz = "UTC"))
=======
# now we loop through the entire data frame
for(i in 2:length(net.data.summer$group)){
  # we extract the PIT tag, the group number, and visit time of bird i and the bird's group number and visit time that came just before
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
>>>>>>> 405b6681b634c6e89f05ea73ce96490304ae8e70

# Step 2: Find first Date.Time for each PIT within each group
df_first <- net.data.summer %>%
  group_by(group, PIT) %>%
  summarise(
    Date.Time = min(Date.Time),
    location = first(location),
    week = first(week),
    visit.duration = first(visit.duration),
    .groups = "drop"
  )

# Step 3: Identify the leader in each group
leaders <- df_first %>%
  group_by(group) %>%
  filter(Date.Time == min(Date.Time)) %>%
  mutate(leader.follower = "leader") %>%
  select(group, PIT, leader.follower)

# Step 4: Join leader info back to df_first
df_labeled <- df_first %>%
  left_join(leaders, by = c("group", "PIT")) %>%
  mutate(leader.follower = ifelse(is.na(leader.follower), "follower", leader.follower))

# Step 5: Calculate number of distinct visits (≥10s apart)
df_visits <- net.data.summer %>%
  arrange(PIT, group, Date.Time) %>%
  group_by(PIT, group) %>%
  mutate(
    time_diff = as.numeric(difftime(Date.Time, lag(Date.Time), units = "secs")),
    new_visit = if_else(is.na(time_diff) | time_diff > 10, 1, 0),
    visit_id = cumsum(new_visit)
  ) %>%
  ungroup()

df_visit_counts <- df_visits %>%
  group_by(PIT, group) %>%
  summarise(n_visits = n_distinct(visit_id), .groups = "drop")

# Step 6: Combine leader/follower and visit counts
df_final <- df_labeled %>%
  left_join(df_visit_counts, by = c("PIT", "group")) %>%
  arrange(group, Date.Time)


# add the species and ages
df_final_species <- merge(df_final, species_age_sex, by.x= "PIT")
# reduce to great tits, blue tits, marsh tits and nuthatches
df_final_species <- subset(df_final_species, df_final_species$species %in% c("BLUTI", "GRETI", "MARTI", "NUTHA"))
df_final_species$season <- "summer"

df_summer <- df_final_species


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

net.data.autumn[which(is.na(net.data.autumn$group)),]
# some are NA that were outside of the 48 hour period

length(unique(net.data.autumn$group))
# 1580 groups 

# make sure it's in correct order (first ordered by groups, then time)
net.data.autumn <- net.data.autumn[with(net.data.autumn, order(group, Date.Time)), ]

# Step 1: Parse Date.Time correctly
net.data.autumn <- net.data.autumn %>%
  mutate(Date.Time = strptime(Date.Time, format = "%y%m%d%H%M%S", tz = "UTC"))

# Step 2: Find first Date.Time for each PIT within each group
df_first <- net.data.autumn %>%
  group_by(group, PIT) %>%
  summarise(
    Date.Time = min(Date.Time),
    location = first(location),
    week = first(week),
    visit.duration = first(visit.duration),
    .groups = "drop"
  )

# Step 3: Identify the leader in each group
leaders <- df_first %>%
  group_by(group) %>%
  filter(Date.Time == min(Date.Time)) %>%
  mutate(leader.follower = "leader") %>%
  select(group, PIT, leader.follower)

# Step 4: Join leader info back to df_first
df_labeled <- df_first %>%
  left_join(leaders, by = c("group", "PIT")) %>%
  mutate(leader.follower = ifelse(is.na(leader.follower), "follower", leader.follower))

# Step 5: Calculate number of distinct visits (≥10s apart)
df_visits <- net.data.autumn %>%
  arrange(PIT, group, Date.Time) %>%
  group_by(PIT, group) %>%
  mutate(
    time_diff = as.numeric(difftime(Date.Time, lag(Date.Time), units = "secs")),
    new_visit = if_else(is.na(time_diff) | time_diff > 10, 1, 0),
    visit_id = cumsum(new_visit)
  ) %>%
  ungroup()

df_visit_counts <- df_visits %>%
  group_by(PIT, group) %>%
  summarise(n_visits = n_distinct(visit_id), .groups = "drop")

# Step 6: Combine leader/follower and visit counts
df_final <- df_labeled %>%
  left_join(df_visit_counts, by = c("PIT", "group")) %>%
  arrange(group, Date.Time)


# add the species and ages
df_final_species <- merge(df_final, species_age_sex, by.x= "PIT")
# reduce to great tits, blue tits, marsh tits and nuthatches
df_final_species <- subset(df_final_species, df_final_species$species %in% c("BLUTI", "GRETI", "MARTI", "NUTHA"))
df_final_species$season <- "autumn"

df_autumn <- df_final_species


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

net.data.winter[which(is.na(net.data.winter$group)),]
# some are NA that were outside of the 48 hour period

length(unique(net.data.winter$group))
# 2868 groups 

# make sure it's in correct order (first ordered by groups, then time)
net.data.winter <- net.data.winter[with(net.data.winter, order(group, Date.Time)), ]

# Step 1: Parse Date.Time correctly
net.data.winter <- net.data.winter %>%
  mutate(Date.Time = strptime(Date.Time, format = "%y%m%d%H%M%S", tz = "UTC"))

# Step 2: Find first Date.Time for each PIT within each group
df_first <- net.data.winter %>%
  group_by(group, PIT) %>%
  summarise(
    Date.Time = min(Date.Time),
    location = first(location),
    week = first(week),
    visit.duration = first(visit.duration),
    .groups = "drop"
  )

# Step 3: Identify the leader in each group
leaders <- df_first %>%
  group_by(group) %>%
  filter(Date.Time == min(Date.Time)) %>%
  mutate(leader.follower = "leader") %>%
  select(group, PIT, leader.follower)

# Step 4: Join leader info back to df_first
df_labeled <- df_first %>%
  left_join(leaders, by = c("group", "PIT")) %>%
  mutate(leader.follower = ifelse(is.na(leader.follower), "follower", leader.follower))

# Step 5: Calculate number of distinct visits (≥10s apart)
df_visits <- net.data.winter %>%
  arrange(PIT, group, Date.Time) %>%
  group_by(PIT, group) %>%
  mutate(
    time_diff = as.numeric(difftime(Date.Time, lag(Date.Time), units = "secs")),
    new_visit = if_else(is.na(time_diff) | time_diff > 10, 1, 0),
    visit_id = cumsum(new_visit)
  ) %>%
  ungroup()

df_visit_counts <- df_visits %>%
  group_by(PIT, group) %>%
  summarise(n_visits = n_distinct(visit_id), .groups = "drop")

# Step 6: Combine leader/follower and visit counts
df_final <- df_labeled %>%
  left_join(df_visit_counts, by = c("PIT", "group")) %>%
  arrange(group, Date.Time)


# add the species and ages
df_final_species <- merge(df_final, species_age_sex, by.x= "PIT")
# reduce to great tits, blue tits, marsh tits and nuthatches
df_final_species <- subset(df_final_species, df_final_species$species %in% c("BLUTI", "GRETI", "MARTI", "NUTHA"))
df_final_species$season <- "winter"

df_winter <- df_final_species


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
# 2389 groups 

# make sure it's in correct order (first ordered by groups, then time)
net.data.spring <- net.data.spring[with(net.data.spring, order(group, Date.Time)), ]

# Step 1: Parse Date.Time correctly
net.data.spring <- net.data.spring %>%
  mutate(Date.Time = strptime(Date.Time, format = "%y%m%d%H%M%S", tz = "UTC"))

# Step 2: Find first Date.Time for each PIT within each group
df_first <- net.data.spring %>%
  group_by(group, PIT) %>%
  summarise(
    Date.Time = min(Date.Time),
    location = first(location),
    week = first(week),
    visit.duration = first(visit.duration),
    .groups = "drop"
  )

# Step 3: Identify the leader in each group
leaders <- df_first %>%
  group_by(group) %>%
  filter(Date.Time == min(Date.Time)) %>%
  mutate(leader.follower = "leader") %>%
  select(group, PIT, leader.follower)

# Step 4: Join leader info back to df_first
df_labeled <- df_first %>%
  left_join(leaders, by = c("group", "PIT")) %>%
  mutate(leader.follower = ifelse(is.na(leader.follower), "follower", leader.follower))

# Step 5: Calculate number of distinct visits (≥10s apart)
df_visits <- net.data.spring %>%
  arrange(PIT, group, Date.Time) %>%
  group_by(PIT, group) %>%
  mutate(
    time_diff = as.numeric(difftime(Date.Time, lag(Date.Time), units = "secs")),
    new_visit = if_else(is.na(time_diff) | time_diff > 10, 1, 0),
    visit_id = cumsum(new_visit)
  ) %>%
  ungroup()

df_visit_counts <- df_visits %>%
  group_by(PIT, group) %>%
  summarise(n_visits = n_distinct(visit_id), .groups = "drop")

# Step 6: Combine leader/follower and visit counts
df_final <- df_labeled %>%
  left_join(df_visit_counts, by = c("PIT", "group")) %>%
  arrange(group, Date.Time)


# add the species and ages
df_final_species <- merge(df_final, species_age_sex, by.x= "PIT")
# reduce to great tits, blue tits, marsh tits and nuthatches
df_final_species <- subset(df_final_species, df_final_species$species %in% c("BLUTI", "GRETI", "MARTI", "NUTHA"))
df_final_species$season <- "spring"

df_spring <- df_final_species


# 4) calculate network position -------------------------------------------

# this automatically adds the social network position calculated across the three weeks each season for birds with a minimum of 5 visits
library(igraph)
calculate.soc.network.pos <- function(gbi, metadata, threshold=5, season, df){
  
  # subset to the threshold
  gbi.sub <- gbi[,colSums(gbi)>=threshold]
  
  # calculate the network
  network_season <- get_network(gbi.sub, data_format="GBI",
                                association_index="SRI")
  
  
  dim(network_season)
  
  net <- graph_from_adjacency_matrix(network_season,mode= c("undirected"), diag=FALSE, weighted=TRUE)
  net_deg <- degree(net)
  Tag <- V(net)$name
  
  bet <- betweenness(net)
  
  comb.net <- cbind.data.frame(Tag, net_deg, bet)
  colnames(comb.net) <- c("PIT", "degree", "betweenness")
  rownames(comb.net) <- NULL
  
  df.merge <- merge(df, comb.net, by="PIT")

  return(df.merge)
  
}

# apply the function to each season
network.pos.summer <- calculate.soc.network.pos(gbi=gmm.summer$gbi, metadata = gmm.summer$metadata, threshold=5, season="summer", df=df_summer)
network.pos.autumn <- calculate.soc.network.pos(gbi=gmm.autumn$gbi, metadata = gmm.autumn$metadata, threshold=5, season="autumn", df=df_autumn)
network.pos.winter <- calculate.soc.network.pos(gbi=gmm.winter$gbi, metadata = gmm.winter$metadata, threshold=5, season="winter", df=df_winter)
network.pos.spring <- calculate.soc.network.pos(gbi=gmm.spring$gbi, metadata = gmm.spring$metadata, threshold=5, season="spring", df=df_spring)



# 3.5) combine follower leader data ----------------------------------------

network.pos.all.seasons <- rbind(network.pos.summer, network.pos.autumn, network.pos.winter, network.pos.spring)


network.pos.all.seasons$group <- paste( network.pos.all.seasons$season,network.pos.all.seasons$group, sep="_")

# we need to add one column on the species prevalence in each flock
# Add species counts per flock
species_counts <- network.pos.all.seasons %>%
  group_by(group, species) %>%
  summarise(n_species = n(), .groups = "drop")

# Total birds per flock
flock_sizes <- network.pos.all.seasons %>%
  group_by(group) %>%
  summarise(flock_size = n(), .groups = "drop")

# Join and compute proportion
species_props <- left_join(species_counts, flock_sizes, by = "group") %>%
  mutate(species_prop = n_species / flock_size)

# Add this back to your main data
network.pos.all.seasons <- network.pos.all.seasons %>%
  left_join(species_props, by = c("group", "species"))

network.pos.all.seasons$leader.follower <- as.factor(network.pos.all.seasons$leader.follower)

# remove sex column since it has NAs
network.pos.all.seasons <- network.pos.all.seasons %>% select(-sex)
# change format of Date time back so it does not contain spaces

network.pos.all.seasons <- network.pos.all.seasons %>%
  mutate(Date.Time = format(Date.Time, "%y%m%d%H%M%S"))

write.table(network.pos.all.seasons, file="data/leader_follower_data_all_seasons.txt")

