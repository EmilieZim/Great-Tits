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

# gmm.summer <- gmmevents(
#   time = net.data.summer$Date.Time,
#   identity = net.data.summer$PIT,
#   location = net.data.summer$location,
#   verbose = TRUE,
#   splitGroups = TRUE
# )

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
# install.packages("igraph")
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



####NETWORKS FOR EACH WEEK

#add week column to gbi
#For that, has to put week column into metadata 

#transform Start column into a date
head(metadata)
metadata$Date <- as.POSIXct(as.character(metadata$Start), format = "%y%m%d%H%M%S")
head(metadata)

#substracting the 6 first numbers of the Start column
metadata$date <- substr(metadata$Start , 1, 6)
head(metadata)
max(metadata$Start)#go until august
max(metadata$date)
min(metadata$Start)#start in May
min(metadata$date)
str(metadata$date)
metadata$date <- as.numeric(metadata$date)

###Finally it works with:
metadata$date <- substr(metadata$Start , 1, 6)

#lets change the $date so I have the year 2020 --> add 20000000 to each $date
metadata$date2 <- 20000000 + metadata$date
metadata$date2 <- as.character(metadata$date2 )
metadata$date3 <- as.Date(metadata$date2, format="%Y%m%d", origin= "20200505")

week_boundaries <- seq(min(metadata$date3), max(metadata$date3) + 7, by="7 days")

# Create a new column "week" based on the boundaries
metadata$week <- cut(metadata$date3, breaks = week_boundaries, labels = FALSE)

# Convert the numeric labels to factors
metadata$week <- as.factor(metadata$week)

#NB: here the first week correspond to the first seven days of data
head(metadata)
str(metadata$week)#14 weeks


##THE WEEKS
####make network with gbi: select the rows corresponding to each week. 14 weeks, hence 14 networks
###week1
which(metadata$week == 1)#from row 1 to 138

gbi1 <- gbi[1:138,]
# you will need rows 1:138 (for week 1 as you have specified above), but you will need all the columns
# since these are the individuals
# while we're at it - it is good practise to subset the data set to those indivdiuals who have been
# seen at least x times so that we have more robust estimates for their network position. 
# we can for now set that to 5 (but we whould later do some sensitivity analyses to see how sensitive the
# results are to a change in threshold to e.g. 8 or 10)
# we can do this by using column sums - i.e. we only include columns (individuals) with a column sum
# of at least 5

threshold <- 5
gbi1.sub <- gbi1[,colSums(gbi1)>=threshold]
dim(gbi1.sub)
# that gives you 138 rows and 12 columns (=individuals)
#EZ: So 12 individuals that have been seen at least 5 times in the first week. 


network_week1 <- get_network(gbi1.sub, data_format="GBI",
                             association_index="SRI")

# SW this is how far I've looked at the code.

net1 <- graph_from_adjacency_matrix(network_week1,mode= c("undirected"), diag=FALSE, weighted=TRUE)
net1_deg <- degree(net1)
plot(net1, edge.color= "black", vertex.label.cex= 0.5)

Tag <- V(net1)$name

# Create a data frame with individual names and degree values
centrality_table1 <- data.frame(
  Tag = Tag,
  degree = net1_deg)

#merge
dc <- merge (centrality_table1, fd, by.x= "Tag")
head(dc)
#The 12 concerned individuals are all adults. No juveniles, hence cannot test the relationship with fledge order for this week.

###week2
which(metadata$week == 2)#from row 139 to 559

gbi2 <- gbi[139:559,] 
threshold <- 5
gbi2.sub <- gbi2[,colSums(gbi2)>=threshold]
dim(gbi2.sub) #22 individuals seen at least 5 times during the second week
network_week2 <- get_network(gbi2.sub, data_format="GBI",
                             association_index="SRI")  

net2 <- graph_from_adjacency_matrix(network_week2,mode= c("undirected"), diag=FALSE, weighted=TRUE)
net2_deg <- degree(net2)
plot(net2, edge.color= "black", vertex.label.cex= 0.5) 

Tag <- V(net2)$name

# Create a data frame with individual names and degree values
centrality_table2 <- data.frame(
  Tag = Tag,
  degree = net2_deg)

print(centrality_table2)

#merge
dc2 <- merge (centrality_table2, fd, by.x= "Tag")
#Again no chicks, only the adults were seen under these conditions. 

####week3
which(metadata$week == 3)
gbi3 <- gbi[560:1388,]

threshold <- 5
gbi3.sub <- gbi3[,colSums(gbi3)>=threshold]
dim(gbi3.sub)
# 47 individuals seen at least 5 times in week3

network_week3 <- get_network(gbi3.sub, data_format="GBI",
                             association_index="SRI")


net3 <- graph_from_adjacency_matrix(network_week3,mode= c("undirected"), diag=FALSE, weighted=TRUE)
net3_deg <- degree(net3)
plot(net3, edge.color= "black", vertex.label.cex= 0.5)

Tag <- V(net3)$name

# Create a data frame with individual names and degree values
centrality_table3 <- data.frame(
  Tag = Tag,
  degree = net3_deg)

#merge
dc3 <- merge (centrality_table3, fd, by.x= "Tag")
head(dc)
#again, only adults, no chicks

###week4 --> from here, 4 chicks are concerned
which(metadata$week == 4)
gbi4 <- gbi[1389:2309,]

threshold <- 5
gbi4.sub <- gbi4[,colSums(gbi4)>=threshold]
dim(gbi4.sub)
# 52 individuals seen at least 5 times in week4

network_week4 <- get_network(gbi4.sub, data_format="GBI",
                             association_index="SRI")

net4 <- graph_from_adjacency_matrix(network_week4,mode= c("undirected"), diag=FALSE, weighted=TRUE)
net4_deg <- degree(net4)
plot(net4, edge.color= "black", vertex.label.cex= 0.5)

Tag <- V(net4)$name
centrality_table4 <- data.frame(
  Tag = Tag,
  degree = net4_deg)

#merge
dc4 <- merge(centrality_table4, fd, by.x= "Tag")
#only 4 chicks included #not enough to look for degree and betweenness centrality

###week5 
which(metadata$week == 5)
gbi5 <- gbi[2310:3044,]

threshold <- 5
gbi5.sub <- gbi5[,colSums(gbi5)>=threshold]
dim(gbi5.sub)
# 59 individuals seen at least 5 times in week5

network_week5 <- get_network(gbi5.sub, data_format="GBI",
                             association_index="SRI")

net5 <- graph_from_adjacency_matrix(network_week5,mode= c("undirected"), diag=FALSE, weighted=TRUE)
net5_deg <- degree(net5)
plot(net5, edge.color= "black", vertex.label.cex= 0.5)

Tag <- V(net5)$name
centrality_table5 <- data.frame(
  Tag = Tag,
  degree = net5_deg)

#merge
dc5 <- merge(centrality_table5, fd, by.x= "Tag")

dc5_chicks <- subset(dc5, dc5$Who=="Chick")#only 12 chicks


###week6 
#From week 6 a lot of Chicks start to appear --> would be interesting to start the linear model at week6
which(metadata$week == 6)
gbi6 <- gbi[3045:3999,]

threshold <- 5
gbi6.sub <- gbi6[,colSums(gbi6)>=threshold]
dim(gbi6.sub)
# 83 individuals seen at least 5 times in week5

network_week6 <- get_network(gbi6.sub, data_format="GBI",
                             association_index="SRI")

net6 <- graph_from_adjacency_matrix(network_week6,mode= c("undirected"), diag=FALSE, weighted=TRUE)
net6_deg <- degree(net6)
plot(net6, edge.color= "black", vertex.label.cex= 0.5)

Tag <- V(net6)$name
centrality_table6 <- data.frame(
  Tag = Tag,
  degree = net6_deg)

#merge
dc6 <- merge(centrality_table6, fd, by.x= "Tag")

dc6_chicks <- subset(dc6, dc6$Who=="Chick")# 71 chicks --> much more than the previous week

library(ggplot2)
ggplot(dc6_chicks, aes(x = degree, y = Fledge.order)) +
  geom_point() +
  labs(title = "Scatter Plot of Degree Centrality and Fledge Order of week6",
       x = "Degree Centrality",
       y = "Fledge Order")

#plotting with betweenness centrality
btw_net6 <- betweenness(net6,v = V(net6),directed = F)
betweenness_table6 <- data.frame(
  Tag = Tag,
  betweenness = btw_net6)

dcc6 <- merge (betweenness_table6, dc6_chicks, by.x= "Tag")
dcc6$Fledge.order <- as.factor(dcc6$Fledge.order)

ggplot(dcc6, aes(x = betweenness, y = Fledge.order)) +
  geom_point() +
  labs(title = "Scatter Plot of Betweenness Centrality and Fledge Order of week6",
       x = "Betweenness Centrality",
       y = "Fledge Order")

###week7 
which(metadata$week == 7)
gbi7 <- gbi[4000:4690,]

threshold <- 5
gbi7.sub <- gbi7[,colSums(gbi7)>=threshold]
dim(gbi7.sub)
# 66 individuals seen at least 5 times in week7 (less than the previous week)

network_week7 <- get_network(gbi7.sub, data_format="GBI",
                             association_index="SRI")

net7 <- graph_from_adjacency_matrix(network_week7,mode= c("undirected"), diag=FALSE, weighted=TRUE)
net7_deg <- degree(net7)
plot(net7, edge.color= "black", vertex.label.cex= 0.5)

Tag <- V(net7)$name
centrality_table7 <- data.frame(
  Tag = Tag,
  degree = net7_deg)

#merge
dc7 <- merge(centrality_table7, fd, by.x= "Tag")

dc7_chicks <- subset(dc7, dc7$Who=="Chick")# 29 chicks --> much less than the previous week

library(ggplot2)
ggplot(dc7_chicks, aes(x = degree, y = Fledge.order)) +
  geom_point() +
  labs(title = "Scatter Plot of Degree Centrality and Fledge Order of week7",
       x = "Degree Centrality",
       y = "Fledge Order")

#plotting with betweenness centrality
btw_net7 <- betweenness(net7,v = V(net7),directed = F)
betweenness_table7 <- data.frame(
  Tag = Tag,
  betweenness = btw_net7)

dcc7 <- merge (betweenness_table7, dc7_chicks, by.x= "Tag")
dcc7$Fledge.order <- as.factor(dcc7$Fledge.order)
ggplot(dcc7, aes(x = betweenness, y = Fledge.order)) +
  geom_point() +
  labs(title = "Scatter Plot of Betweenness Centrality and Fledge Order of week7",
       x = "Betweenness Centrality",
       y = "Fledge Order")

###week8 
which(metadata$week == 8)
gbi8 <- gbi[4692:5289,]

threshold <- 5
gbi8.sub <- gbi8[,colSums(gbi8)>=threshold]
dim(gbi8.sub)
# 62 individuals seen at least 5 times in week8 

network_week8 <- get_network(gbi8.sub, data_format="GBI",
                             association_index="SRI")

net8 <- graph_from_adjacency_matrix(network_week8,mode= c("undirected"), diag=FALSE, weighted=TRUE)
net8_deg <- degree(net8)
plot(net8, edge.color= "black", vertex.label.cex= 0.5)

Tag <- V(net8)$name
centrality_table8 <- data.frame(
  Tag = Tag,
  degree = net8_deg)

#merge
dc8 <- merge(centrality_table8, fd, by.x= "Tag")

dc8_chicks <- subset(dc8, dc8$Who=="Chick")# 52 chicks --> more than week7 but less than week6

library(ggplot2)
ggplot(dc8_chicks, aes(x = degree, y = Fledge.order)) +
  geom_point() +
  labs(title = "Scatter Plot of Degree Centrality and Fledge Order of week8",
       x = "Degree Centrality",
       y = "Fledge Order")

#plotting with betweenness centrality
btw_net8 <- betweenness(net8,v = V(net8),directed = F)
betweenness_table8 <- data.frame(
  Tag = Tag,
  betweenness = btw_net8)

dcc8 <- merge (betweenness_table8, dc8_chicks, by.x= "Tag")
dcc8$Fledge.order <- as.factor(dcc8$Fledge.order)
ggplot(dcc8, aes(x = betweenness, y = Fledge.order)) +
  geom_point() +
  labs(title = "Scatter Plot of Betweenness Centrality and Fledge Order of week8",
       x = "Betweenness Centrality",
       y = "Fledge Order")

###week9 
which(metadata$week == 9)
gbi9 <- gbi[4290:5933,]

threshold <- 5
gbi9.sub <- gbi9[,colSums(gbi9)>=threshold]
dim(gbi9.sub)
# 87 individuals seen at least 5 times in week9 (the highest value up until now)

network_week9 <- get_network(gbi9.sub, data_format="GBI",
                             association_index="SRI")

net9 <- graph_from_adjacency_matrix(network_week9,mode= c("undirected"), diag=FALSE, weighted=TRUE)
net9_deg <- degree(net9)
plot(net9, edge.color= "black", vertex.label.cex= 0.5)

Tag <- V(net9)$name
centrality_table9 <- data.frame(
  Tag = Tag,
  degree = net9_deg)

#merge
dc9 <- merge(centrality_table9, fd, by.x= "Tag")

dc9_chicks <- subset(dc9, dc9$Who=="Chick")# 41 chicks

library(ggplot2)
ggplot(dc9_chicks, aes(x = degree, y = Fledge.order)) +
  geom_point() +
  labs(title = "Scatter Plot of Degree Centrality and Fledge Order of week9",
       x = "Degree Centrality",
       y = "Fledge Order")

#plotting with betweenness centrality
btw_net9 <- betweenness(net9,v = V(net9),directed = F)
betweenness_table9 <- data.frame(
  Tag = Tag,
  betweenness = btw_net9)

dcc9 <- merge (betweenness_table9, dc9_chicks, by.x= "Tag")
dcc9$Fledge.order <- as.factor(dcc9$Fledge.order)
ggplot(dcc9, aes(x = betweenness, y = Fledge.order)) +
  geom_point() +
  labs(title = "Scatter Plot of Betweenness Centrality and Fledge Order of week9",
       x = "Betweenness Centrality",
       y = "Fledge Order")

###week10
which(metadata$week == 10)
gbi10 <- gbi[5934:6879,]

threshold <- 5
gbi10.sub <- gbi10[,colSums(gbi10)>=threshold]
dim(gbi10.sub)
# 78 individuals seen at least 5 times in week10 

network_week10 <- get_network(gbi10.sub, data_format="GBI",
                             association_index="SRI")

net10 <- graph_from_adjacency_matrix(network_week10,mode= c("undirected"), diag=FALSE, weighted=TRUE)
net10_deg <- degree(net10)
plot(net10, edge.color= "black", vertex.label.cex= 0.5)

Tag <- V(net10)$name
centrality_table10 <- data.frame(
  Tag = Tag,
  degree = net10_deg)

#merge
dc10 <- merge(centrality_table10, fd, by.x= "Tag")
dc10_chicks <- subset(dc10, dc10$Who=="Chick")# 40 chicks

library(ggplot2)
ggplot(dc10_chicks, aes(x = degree, y = Fledge.order)) +
  geom_point() +
  labs(title = "Scatter Plot of Degree Centrality and Fledge Order of week10",
       x = "Degree Centrality",
       y = "Fledge Order")

#plotting with betweenness centrality
btw_net10 <- betweenness(net10,v = V(net10),directed = F)
betweenness_table10 <- data.frame(
  Tag = Tag,
  betweenness = btw_net10)

dcc10 <- merge (betweenness_table10, dc10_chicks, by.x= "Tag")
dcc10$Fledge.order <- as.factor(dcc10$Fledge.order)
ggplot(dcc10, aes(x = betweenness, y = Fledge.order)) +
  geom_point() +
  labs(title = "Scatter Plot of Betweenness Centrality and Fledge Order of week10",
       x = "Betweenness Centrality",
       y = "Fledge Order")


###week11
which(metadata$week == 11)
gbi11 <- gbi[6880:7724,]

threshold <- 5
gbi11.sub <- gbi11[,colSums(gbi11)>=threshold]
dim(gbi11.sub)
# 68 individuals seen at least 5 times in week11 

network_week11 <- get_network(gbi11.sub, data_format="GBI",
                              association_index="SRI")

net11 <- graph_from_adjacency_matrix(network_week11,mode= c("undirected"), diag=FALSE, weighted=TRUE)
net11_deg <- degree(net11)
plot(net11, edge.color= "black", vertex.label.cex= 0.5)

Tag <- V(net11)$name
centrality_table11 <- data.frame(
  Tag = Tag,
  degree = net11_deg)

#merge
dc11 <- merge(centrality_table11, fd, by.x= "Tag")
dc11_chicks <- subset(dc11, dc11$Who=="Chick")# 28 chicks

library(ggplot2)
ggplot(dc11_chicks, aes(x = degree, y = Fledge.order)) +
  geom_point() +
  labs(title = "Scatter Plot of Degree Centrality and Fledge Order of week11",
       x = "Degree Centrality",
       y = "Fledge Order")

#plotting with betweenness centrality
btw_net11 <- betweenness(net11,v = V(net11),directed = F)
betweenness_table11 <- data.frame(
  Tag = Tag,
  betweenness = btw_net11)

dcc11 <- merge (betweenness_table11, dc11_chicks, by.x= "Tag")
dcc11$Fledge.order <- as.factor(dcc11$Fledge.order)
ggplot(dcc11, aes(x = betweenness, y = Fledge.order)) +
  geom_point() +
  labs(title = "Scatter Plot of Betweenness Centrality and Fledge Order of week11",
       x = "Betweenness Centrality",
       y = "Fledge Order")

###week12
which(metadata$week == 12)
gbi12 <- gbi[7725:8428,]

threshold <- 5
gbi12.sub <- gbi12[,colSums(gbi12)>=threshold]
dim(gbi12.sub)
# 81 individuals seen at least 5 times in week12 

network_week12 <- get_network(gbi12.sub, data_format="GBI",
                              association_index="SRI")

net12 <- graph_from_adjacency_matrix(network_week12,mode= c("undirected"), diag=FALSE, weighted=TRUE)
net12_deg <- degree(net12)
plot(net12, edge.color= "black", vertex.label.cex= 0.5)

Tag <- V(net12)$name
centrality_table12 <- data.frame(
  Tag = Tag,
  degree = net12_deg)

#merge
dc12 <- merge(centrality_table12, fd, by.x= "Tag")
dc12_chicks <- subset(dc12, dc12$Who=="Chick")# 41 chicks

library(ggplot2)
ggplot(dc12_chicks, aes(x = degree, y = Fledge.order)) +
  geom_point() +
  labs(title = "Scatter Plot of Degree Centrality and Fledge Order of week12",
       x = "Degree Centrality",
       y = "Fledge Order")

#plotting with betweenness centrality
btw_net12 <- betweenness(net12,v = V(net12),directed = F)
betweenness_table12 <- data.frame(
  Tag = Tag,
  betweenness = btw_net12)

dcc12 <- merge (betweenness_table11, dc12_chicks, by.x= "Tag")
dcc12$Fledge.order <- as.factor(dcc12$Fledge.order)
ggplot(dcc12, aes(x = betweenness, y = Fledge.order)) +
  geom_point() +
  labs(title = "Scatter Plot of Betweenness Centrality and Fledge Order of week12",
       x = "Betweenness Centrality",
       y = "Fledge Order")

###week13
which(metadata$week == 13)
gbi13 <- gbi[8429:9164,]

threshold <- 5
gbi13.sub <- gbi13[,colSums(gbi13)>=threshold]
dim(gbi13.sub)
# 60 individuals seen at least 5 times in week13 

network_week13 <- get_network(gbi13.sub, data_format="GBI",
                              association_index="SRI")

net13 <- graph_from_adjacency_matrix(network_week13,mode= c("undirected"), diag=FALSE, weighted=TRUE)
net13_deg <- degree(net13)
plot(net13, edge.color= "black", vertex.label.cex= 0.5)

Tag <- V(net13)$name
centrality_table13 <- data.frame(
  Tag = Tag,
  degree = net13_deg)

#merge
dc13 <- merge(centrality_table13, fd, by.x= "Tag")
dc13_chicks <- subset(dc13, dc13$Who=="Chick")# 26 chicks

library(ggplot2)
ggplot(dc13_chicks, aes(x = degree, y = Fledge.order)) +
  geom_point() +
  labs(title = "Scatter Plot of Degree Centrality and Fledge Order of week13",
       x = "Degree Centrality",
       y = "Fledge Order")

#plotting with betweenness centrality
btw_net13 <- betweenness(net13,v = V(net13),directed = F)
betweenness_table13 <- data.frame(
  Tag = Tag,
  betweenness = btw_net13)

dcc13 <- merge (betweenness_table13, dc13_chicks, by.x= "Tag")
dcc13$Fledge.order <- as.factor(dcc13$Fledge.order)
ggplot(dcc13, aes(x = betweenness, y = Fledge.order)) +
  geom_point() +
  labs(title = "Scatter Plot of Betweenness Centrality and Fledge Order of week13",
       x = "Betweenness Centrality",
       y = "Fledge Order")

###week14
which(metadata$week == 14)
gbi14 <- gbi[9165:9913,]

threshold <- 5
gbi14.sub <- gbi14[,colSums(gbi14)>=threshold]
dim(gbi14.sub)
# 74 individuals seen at least 5 times in week14 

network_week14 <- get_network(gbi14.sub, data_format="GBI",
                              association_index="SRI")

net14 <- graph_from_adjacency_matrix(network_week14,mode= c("undirected"), diag=FALSE, weighted=TRUE)
net14_deg <- degree(net14)
plot(net14, edge.color= "black", vertex.label.cex= 0.5)

Tag <- V(net14)$name
centrality_table14 <- data.frame(
  Tag = Tag,
  degree = net14_deg)

#merge
dc14 <- merge(centrality_table14, fd, by.x= "Tag")
dc14_chicks <- subset(dc14, dc14$Who=="Chick")# 32 chicks

library(ggplot2)
ggplot(dc14_chicks, aes(x = degree, y = Fledge.order)) +
  geom_point() +
  labs(title = "Scatter Plot of Degree Centrality and Fledge Order of week14",
       x = "Degree Centrality",
       y = "Fledge Order")

#plotting with betweenness centrality
btw_net14 <- betweenness(net14,v = V(net14),directed = F)
betweenness_table14 <- data.frame(
  Tag = Tag,
  betweenness = btw_net14)

dcc14 <- merge (betweenness_table14, dc14_chicks, by.x= "Tag")
dcc14$Fledge.order <- as.factor(dcc14Fledge.order)
ggplot(dcc14, aes(x = betweenness, y = Fledge.order)) +
  geom_point() +
  labs(title = "Scatter Plot of Betweenness Centrality and Fledge Order of week14",
       x = "Betweenness Centrality",
       y = "Fledge Order")


########REGRESSION MODELS
###Regression models
#fledge order = ordinal + has to be centered around zero (use scale function for this)
#control for weight
#First did it for the whole data, without taking into account possible differences/week.
#Would be interesting to make the regression models from week4 as in that week the Chicks started to be seen at least 5 times.

#make Fledge.order ordinal (1>2>3>4>5>6>7)
head(data_cc_no_NA)
str(data_cc_no_NA$Fledge.order)
data_cc_no_NA$Fledge.order <- factor(data_cc_no_NA$Fledge.order, levels=c(1,2,3,4,5,6,7))
levels(data_cc_no_NA$Fledge.order)

#1. density centrality
par(mfrow = c(1,1))
hist(data_cc_no_NA$degree) #--> not very Gaussian
model1 <- lm(degree ~ Chick.weight + Fledge.order + Chick.weight*Fledge.order, data= data_cc_no_NA)
summary(model1)
require("rgl")
library(ggResidpanel)
resid_panel(model1, plots = c("yvp","resid","boxplot","qq"), smoother = TRUE)          
resid_xpanel(model1, smoother = TRUE)
par(mfrow = c(2,2))
plot(model1)

#Remove the interaction as it is not significant
model1_2 <- lm(degree ~ Chick.weight + Fledge.order, data= data_cc_no_NA)
summary(model1_2) #low linear relationship since the adjusted R square is negative
#Chick.weight is not significant, can be left out of the model
par(mfrow = c(2,2))
plot(model1_2)#there seem to be outliers
resid_panel(model1_2, plots = c("yvp","resid","boxplot","qq"), smoother = TRUE)# "yvp" is not horizontal  
shapiro.test(residuals(model1_2))#p-val= 0.4 --> normal
ggplot() + aes(sample = resid(model1_2)) + geom_qq() + geom_qq_line() #normality seems respected
ggplot() + aes(x = predict(model1_2), y = resid(model1_2)) + geom_point() +
  geom_smooth(se = FALSE) + geom_hline(yintercept = 0) #eventually linearity and homoscedasticity seem alright

#When Chick.weight removed
model1_3 <- lm(degree ~ Fledge.order, data= data_cc_no_NA)
summary(model1_3) #low linear relationship since the adjusted R square is negative
par(mfrow = c(2,2))
plot(model1_3)#there seem to be outliers (observation 133 &nd 124)
resid_panel(model1_3, plots = c("yvp","resid","boxplot","qq"), smoother = TRUE)# "yvp" is not horizontal  
shapiro.test(residuals(model1_3))#p-val= 0.39 --> normal
ggplot() + aes(sample = resid(model1_3)) + geom_qq() + geom_qq_line() #normality seems respected
ggplot() + aes(x = predict(model1_3), y = resid(model1_3)) + geom_point() +
  geom_smooth(se = FALSE) + geom_hline(yintercept = 0) 

ggplot(model1_3) + aes(x = Fledge.order, y = degree)+ 
  geom_jitter(width = 0.1, height = 0, alpha = 0.4) + 
  stat_summary(geom = "point", fun = "mean",size = 3, shape = 3)+
  geom_boxplot(width = 0.2, fill = "lightblue", alpha = 0.7)

#if now the Fledge.order is scaled
head(data_cc_no_NA)
data_cc_no_NA$Fledge.order <- as.numeric(data_cc_no_NA$Fledge.order)
data_cc_no_NA$scaled_Fo <- (data_cc_no_NA$Fledge.order - min(data_cc_no_NA$Fledge.order)) / (max(data_cc_no_NA$Fledge.order) - min(data_cc_no_NA$Fledge.order))
model1_4 <- lm(degree ~ scaled_Fo + Chick.weight, data= data_cc_no_NA) #Chick.weight is not significant, can leave it out
summary(model1_4)
model1_5 <- lm(degree ~ scaled_Fo, data= data_cc_no_NA) 
summary(model1_5)
par(mfrow = c(2,2))
plot(model1_5)
resid_panel(model1_5, plots = c("yvp","resid","boxplot","qq"), smoother = TRUE)# prob with normality 
shapiro.test(residuals(model1_5))#p-val= 0.01 --> not normal
ggplot() + aes(sample = resid(model1_5)) + geom_qq() + geom_qq_line() #normality not well respected

#sqrt, log, log10, ^2 and 1/y transformations to assure normality did not work
#Box-Cox transformation
library(MASS)
boxcox_result <- boxcox(lm(degree ~ scaled_Fo, data= data_cc_no_NA))
lambda <- boxcox_result$scaled_Fo[which.max(boxcox_result$degree)]#does not work
lambda <- boxcox_result$x[which.max(boxcox_result$y)]
new_x_exact <- (scaled_Fo ^ lambda - 1) / lambda
hist(new_x_exact)

model1_7 <- lm(degree ~ new_x_exact, data= data_cc_no_NA)
summary(model1_7)
par(mfrow = c(2,2))
plot(model1_7)
resid_panel(model1_7, plots = c("yvp","resid","boxplot","qq"), smoother = TRUE)# prob with normality 
shapiro.test(residuals(model1_7))#p-val= 0.02 --> best but still not normal
ggplot() + aes(sample = resid(model1_7)) + geom_qq() + geom_qq_line() #normality not well respected

#Box cox + log(y)
hist(data_cc_no_NA$degree)
hist(log10(data_cc_no_NA$degree))
td <- log10(data_cc_no_NA$degree)
model1_8 <- lm(td ~ new_x_exact, data= data_cc_no_NA)
summary(model1_8)
par(mfrow = c(2,2))
plot(model1_8)
resid_panel(model1_8, plots = c("yvp","resid","boxplot","qq"), smoother = TRUE)# prob with normality 
shapiro.test(residuals(model1_8))#still not normal
ggplot() + aes(sample = resid(model1_7)) + geom_qq() + geom_qq_line() 


#2. betweenness centrality
#immediately with scaled fledge order
head(data_cc_no_NA)
hist(data_cc_no_NA$betweenness) #--> not very Gaussian 
m1 <- lm(betweenness ~ Chick.weight + scaled_Fo + Chick.weight*scaled_Fo, data= data_cc_no_NA)
summary(m1)
#leave out the interaction
m2 <- lm(betweenness ~ Chick.weight + scaled_Fo, data= data_cc_no_NA)
summary(m2) #the scaled fledge order is significant (p-val= 0.03)
resid_panel(m2, plots = c("yvp","resid","boxplot","qq"), smoother = TRUE)          
par(mfrow = c(2,2))
plot(m2)
shapiro.test(residuals(m2)) #absolutely not normal
#leave chick.weight out because not significant
m2_2 <- lm(betweenness ~  scaled_Fo, data= data_cc_no_NA)
summary(m2_2)
resid_panel(m2_2, plots = c("yvp","resid","boxplot","qq"), smoother = TRUE)          
par(mfrow = c(2,2))
plot(m2_2)
shapiro.test(residuals(m2_2))#need to adjust the normality problem

#no transformation of y resolves the normality issue
#I tried sqrt, ^2, 1/y, 1/y+1, log, log10
lb <- sqrt(data_cc_no_NA$betweenness)
m3 <- lm(lb ~ scaled_Fo, data=data_cc_no_NA)
summary(m3)
resid_panel(m3, plots = c("yvp","resid","boxplot","qq"), smoother = TRUE)          
par(mfrow = c(2,2))
plot(m3)
shapiro.test(residuals(m3))#not normal
ggplot() + aes(sample = resid(m3)) + geom_qq() + geom_qq_line() #not the worst





