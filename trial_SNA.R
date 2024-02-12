
#fledgling_data
fd<- read.table("fledgling_data.txt",header = TRUE, fill = T, sep = "\t")
View(fd)
str(fd)

#species_age
sp_a<- read.table("species_age.txt",header = TRUE, fill = T, sep = "\t")
View(sp_a)
str(fd)


#How to get started

# install the package asnipe and load the library
#install.packages("asnipe")
library(asnipe)

net.data.summer <- read.delim("Mill.data.summer.txt", sep=" ", row.names = 1)
head(net.data.summer)
min(net.data.summer$Date.Time)
max(net.data.summer$Date.Time)

# juveniles only started to enter the population in week 4,
###so we subset the data to after week 4
net.data.summer <- subset(net.data.summer, net.data.summer$week>=4)


# this is how you build groups from the data stream 
# your laptop will likely not have enough memory to run this, so I sent you the output (gmm.summer)
#gmm.summer <- gmmevents(
time = net.data.summer$Date.Time,
identity = net.data.summer$PIT,
location = net.data.summer$location,
verbose = TRUE,
splitGroups = TRUE)

# it's stored in an RData object which you can load
load("gmm.summer.RData")
head(gmm.summer)
View(gmm.summer)


# also investigate the object gmm.summer - it has three slots
gmm.summer$gbi
gmm.summer$metadata
gmm.summer$B
?gmmevents() # will be helpful - that is how you bring up the documentation of a function

# as a next step, you can try to figure out how to create a social network from the gmm.object and extract social network positions for each individual.
##You can start off with creating a social network using all the data
###at a later stage we can think of creating weekly networks instead to see how the network positions change over time. 

# install the package igraph and load the library
install.packages("igraph")
library("igraph")
g <- make_graph(edges = c(1,2, 1,5), n=10, directed = FALSE)
g
plot(g)
make_graph(gmm.summer)


#subsets
gbi <- gmm.summer$gbi
metadata <- gmm.summer$metadata
B <- gmm.summer$B

#generate the network
library("asnipe")
network <- get_network(gbi, data_format = "GBI",
                       association_index = "SRI")

#making graph with i_graph 
library("igraph")
net <- graph_from_adjacency_matrix(network,mode= c("undirected"), diag=FALSE, weighted=TRUE)
deg_weighted <- strength(net) #Summing up the edge weights of the adjacent edges for each vertex.
E(net)#There are 4254 edges
V(net)#There are 187 vertices/nods
plot(net, edge.arrow.size= 4, vertex.label= NA)
mean_distance(net, directed=F) #gives the average distance #0.01289476
edge_density(net, loops= F) #The density of a graph is the ratio of the actual number of edges
#                           and the largest possible number of edges in the graph, assuming that no multi-edges are present. #0.2446093

net_deg <- degree(net)#the number of the adjacent edges of each vertex. 
which.max(net_deg)#0700EDAC58 is the ind with the highest degree (108). It is a female, not a chick.
which.min(net_deg)#01101775DD  is the ind with the lowest degree



###plotting with degree centrality
plot(net, edge.color= "black", vertex.label.cex= 0.5)

#I want to make a table where I can see each ind (ring) with its respective centrality degree and fledge order
#So that next I can see whether the highest degrees are the ones that fledged first

Tag <- V(net)$name

# Create a data frame with individual names and degree values
centrality_table <- data.frame(
  Tag = Tag,
  degree = net_deg)

# Print the centrality table
print(centrality_table)

#merge
data_c <- merge (centrality_table, fd, by.x= "Tag")

#plot 
data_c$Fledge.order <- as.factor(data_c$Fledge.order)
str(data_c$Fledge.order)#has 7 levels
str(data_c$degree)

library(ggplot2)
ggplot(data_c, aes(x = degree, y = Fledge.order)) +
  geom_point() +
  labs(title = "Scatter Plot of Degree Centrality and Fledge Order",
       x = "Degree Centrality",
       y = "Fledge Order")
#the NA are the adults I presume. Hence, I can leave them out of the plot interpretation. 
data_c_no_NA <- na.omit(data_c)
ggplot(data_c_no_NA, aes(x = degree, y = Fledge.order)) +
  geom_point() +
  labs(title = "Scatter Plot of Degree Centrality and Fledge Order",
       x = "Degree Centrality",
       y = "Fledge Order")



###plotting with betweenness centrality
btw_net <- betweenness(net,v = V(net),directed = F)

betweenness_table <- data.frame(
  Tag = Tag,
  betweenness = btw_net)

data_cc <- merge (betweenness_table, data_c, by.x= "Tag")
data_cc$Fledge.order <- as.factor(data_cc$Fledge.order)

library(ggplot2)
ggplot(data_cc, aes(x = betweenness, y = Fledge.order)) +
  geom_point() +
  labs(title = "Scatter Plot of Betweenness Centrality and Fledge Order",
       x = "Betweenness Centrality",
       y = "Fledge Order")

data_cc_no_NA <- na.omit(data_cc)
ggplot(data_cc_no_NA, aes(x = betweenness, y = Fledge.order)) +
  geom_point() +
  labs(title = "Scatter Plot of Betweenness Centrality and Fledge Order",
       x = "Betweenness Centrality",
       y = "Fledge Order")
#The highest betweenness is indeed that of an individual that fledged first. 
#High betweenness = may have considerable influence within a network by virtue of their control over information passing between others


###density of the social network
edge_density(net, loops=F)#0.2446093 --> seems quite low



####Networks for each week

#add week column to gbi
#For that, has to put week column into metadata 
#The following code does not work, go forward up until the right code (row233)

head(metadata)
metadata$Date <- as.POSIXct(as.character(metadata$Start), format = "%Y%m%d%H%M%S") #does not work
head(metadata)
str(metadata$Start)
metadata$Start<- as.character(metadata$Star)

metadata$Date2<- as.POSIXct(metadata$Start,
                                    format = "%YY%mm%dd%HH%MM%SS") #does not work
###
metadata$date <- substr(metadata$Start , 1, 6)
head(metadata)
max(metadata$Start)#go until august
max(metadata$date)
min(metadata$Start)#start in May
min(metadata$date)
str(metadata$date)
metadata$date <- as.numeric(metadata$date)



library(dplyr)

Metadata <- metadata %>%
  mutate(week = case_when(
    date == ranges[200505] & date <= ranges[200509] ~  "1"
    date == ranges[200510] & date <= ranges[200516] ~  "2",
    date == ranges[200517] & date <= ranges[200523] ~  "3",
    date == ranges[200524] & date <= ranges[200530] ~  "4",
    date == ranges[200531] & date <= ranges[200601] ~  "5",
    date == ranges[200531] & date <= ranges[200606] ~  "6",
    date == ranges[200607] & date <= ranges[200613] ~  "7",
    date == ranges[200614] & date <= ranges[200620] ~  "8",
    date == ranges[200621] & date <= ranges[200627] ~  "9",
    date == ranges[200628] & date <= ranges[200704] ~  "10",
    date == ranges[200705] & date <= ranges[200711] ~  "11",
    date == ranges[200712] & date <= ranges[200718] ~  "12",
    date == ranges[200719] & date <= ranges[200725] ~  "13",
    date == ranges[200726] & date <= ranges[200801] ~  "14",
    date == ranges[200802] & date <= ranges[200808] ~  "15")) #does not work 

Metadata <- metadata %>%
  mutate(week = case_when(
    date == 200505 & date <= 200509 ~  "week1"
    date == 200510 & date <= 200516 ~  "week2",
    date == 200517 & date <= 200523 ~  "week3",
    date == 200524 & date <= 200530 ~  "week4",
    date == 200531 & date <= 200601 ~  "week5",
    date == 200531 & date <= 200606 ~  "week6",
    date == 200607 & date <= 200613 ~  "week7",
    date == 200614 & date <= 200620 ~  "week8",
    date == 200621 & date <= 200627 ~  "week9",
    date == 200628 & date <= 200704 ~  "week10",
    date == 200705 & date <= 200711 ~  "week11",
    date == 200712 & date <= 200718 ~  "week12",
    date == 200719 & date <= 200725 ~  "week13",
    date == 200726 & date <= 200801 ~  "week14",
    date == 200802 & date <= 200808 ~  "week15", .default="other")) #does not work

##
install.packages("lubridate")
library("lubridate")
metadata$week <- week(parse_date_time(as.character(metadata$data), orders = "ydm")) #does not work



###Finally it works with:
metadata$date2 <- as.Date(metadata$date, format="%y%m%d", origin= "200505")#prob because my year is YY instead of YYYY
#lets change the $date so I have the year 2020 --> add 20000000 to each $date
metadata$date3 <- 20000000 + metadata$date
metadata$date2 <- as.Date(metadata$date3, format="%Y%m%d", origin= "20200505")#the year is still completely wrong, maybe the date has to be a character
metadata$date3 <- as.character(metadata$date3 )
metadata$date2 <- as.Date(metadata$date3, format="%Y%m%d", origin= "20200505")#finally correct

week_boundaries <- seq(min(metadata$date2), max(metadata$date2) + 7, by="7 days")

# Create a new column "week" based on the boundaries
metadata$week <- cut(metadata$date2, breaks = week_boundaries, labels = FALSE)

# Convert the numeric labels to factors
metadata$week <- as.factor(metadata$week)

# Rename the factor levels 
levels(metadata$week) <- paste("Week", levels(metadata$week), sep = "")

#NB: here the first week correspond to the first seven days of data
head(metadata)
max(metadata$week)#14 weeks

####make network with gbi: select the rows corresponding to each week. 14 weeks, hence 14 networks
###week1
which(metadata$week == 1)#from row 1 to 138

gbi1 <- gbi[1:138] #Is this the right way to make a network only for week1?

network_week1 <- get_network(gbi1, data_format="GBI",
                             association_index="SRI")

net1 <- graph_from_adjacency_matrix(network_week1,mode= c("undirected"), diag=FALSE, weighted=TRUE)
net1_deg <- degree(net1)
plot(net1, edge.color= "black", vertex.label.cex= 0.5)

Tag <- V(net1)$name

# Create a data frame with individual names and degree values
centrality_table <- data.frame(
  Tag = Tag,
  degree = net1_deg)

print(centrality_table)

#merge
dc <- merge (centrality_table, fd, by.x= "Tag")

#plot 
dc$Fledge.order <- as.factor(dc$Fledge.order)
str(dc$Fledge.order)#has 7 levels
str(dc$degree)
dc_no_NA <- na.omit(dc)

library(ggplot2)
ggplot(dc_no_NA, aes(x = degree, y = Fledge.order)) +
  geom_point() +
  labs(title = "Scatter Plot of Degree Centrality and Fledge Order of week1",
       x = "Degree Centrality",
       y = "Fledge Order")

#plotting with betweenness centrality
btw_net1 <- betweenness(net1,v = V(net1),directed = F)

betweenness_table <- data.frame(
  Tag = Tag,
  betweenness = btw_net1)

dcc <- merge (betweenness_table, dc, by.x= "Tag")
dcc$Fledge.order <- as.factor(dcc$Fledge.order)

dcc_no_NA <- na.omit(dcc)
ggplot(dcc_no_NA, aes(x = betweenness, y = Fledge.order)) +
  geom_point() +
  labs(title = "Scatter Plot of Betweenness Centrality and Fledge Order of week1",
       x = "Betweenness Centrality",
       y = "Fledge Order")

###week2
which(metadata$week == 2)#from row 139 to 559

gbi2 <- gbi[139:559] ##the matrix has only zeros --> weird
network_week2 <- get_network(gbi2, data_format="GBI",
                             association_index="SRI") ##I cannot extract this network. 



####
which(metadata$week == 3)
gbi3 <- gbi[560:1388]

#etc up until week14. But is this the right way to do it? Not sure, doesn't seem right



