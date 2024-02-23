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

#trying another way to plot
ceb <- cluster_edge_betweenness(net)
class(ceb)  
plot(ceb, net)
dendPlot(ceb, mode="hclust")
#not very visual

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

btw_4 <- betweenness(net4,v = V(net4),directed = F)
betweenness_table4 <- data.frame(
  Tag = Tag,
  betweenness = btw_4)

table_week4 <- merge(centrality_table4,betweenness_table4, by.x= "Tag" )
table_week4$Week <- 4 

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
btw_5 <- betweenness(net5,v = V(net5),directed = F)
betweenness_table5 <- data.frame(
  Tag = Tag,
  betweenness = btw_5)

table_week5 <- merge(centrality_table5,betweenness_table5, by.x= "Tag" )
table_week5$Week <- 5 
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

# betweenness centrality merge
btw_6 <- betweenness(net6,v = V(net6),directed = F)
betweenness_table6 <- data.frame(
  Tag = Tag,
  betweenness = btw_6)

table_week6 <- merge(centrality_table6,betweenness_table6, by.x= "Tag" )
table_week6$Week <- 6 

#plot
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

# betweenness centrality
btw_7 <- betweenness(net7,v = V(net7),directed = F)
betweenness_table7 <- data.frame(
  Tag = Tag,
  betweenness = btw_7)

table_week7 <- merge(centrality_table7,betweenness_table7, by.x= "Tag" )
table_week7$Week <- 7 

#plot
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

#betweenness centrality
btw_8 <- betweenness(net8,v = V(net8),directed = F)
betweenness_table8 <- data.frame(
  Tag = Tag,
  betweenness = btw_8)

table_week8 <- merge(centrality_table8,betweenness_table8, by.x= "Tag" )
table_week8$Week <- 8 
#plot
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

#betweenness centrality
btw_9 <- betweenness(net9,v = V(net9),directed = F)
betweenness_table9 <- data.frame(
  Tag = Tag,
  betweenness = btw_9)

table_week9 <- merge(centrality_table9,betweenness_table9, by.x= "Tag" )
table_week9$Week <- 9 

#plot
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


#betweenness centrality
btw_10 <- betweenness(net10,v = V(net10),directed = F)
betweenness_table10 <- data.frame(
  Tag = Tag,
  betweenness = btw_10)

table_week10 <- merge(centrality_table10,betweenness_table10, by.x= "Tag" )
table_week10$Week <- 10 

#plot
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

#betweenness centrality
btw_11 <- betweenness(net11,v = V(net11),directed = F)
betweenness_table11 <- data.frame(
  Tag = Tag,
  betweenness = btw_11)

table_week11 <- merge(centrality_table11,betweenness_table11, by.x= "Tag" )
table_week11$Week <- 11 

#plot
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

#betweenness centrality
btw_12 <- betweenness(net12,v = V(net12),directed = F)
betweenness_table12 <- data.frame(
  Tag = Tag,
  betweenness = btw_12)

table_week12 <- merge(centrality_table12,betweenness_table12, by.x= "Tag" )
table_week12$Week <- 12 

#plot
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

#betweenness centrality
btw_13 <- betweenness(net13,v = V(net13),directed = F)
betweenness_table13 <- data.frame(
  Tag = Tag,
  betweenness = btw_13)

table_week13 <- merge(centrality_table13,betweenness_table13, by.x= "Tag" )
table_week13$Week <- 13 

#plot
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

#betweenness centrality
btw_14 <- betweenness(net14,v = V(net14),directed = F)
betweenness_table14 <- data.frame(
  Tag = Tag,
  betweenness = btw_14)

table_week14 <- merge(centrality_table14,betweenness_table14, by.x= "Tag" )
table_week14$Week <- 14

#plot
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

## SW: 1) I would recommend treating your fledge order variable as a scaled variable in all your models (-> value between -0.5-0.5), since the factors of 1-7 are not biologically meaningful.

#Scale Fledge.order 
install.packages("scales")
library(scales)
library(datawizard)
fd_withoutNA <- na.omit(fd)
fd_withoutNA$Fledge.order <- as.numeric(fd_withoutNA$Fledge.order)
fd_withoutNA$scaled_FledgeOrder <- rescale(fd_withoutNA$Fledge.order, to= c(-0.5, 0.5))
hist(fd_withoutNA$scaled_FledgeOrder)

fd_withoutNA$scaled_FledgeOrder2 <- normalize(fd_withoutNA$Fledge.order, method = "range", range = c(0, 1))
head(fd_withoutNA)
#Scale Fledged --> age at fledgling since the 1st of April
fd_withoutNA$Fledged <- as.numeric(fd_withoutNA$Fledged)
fd_withoutNA$time_since_fledgling <-scale(fd_withoutNA$Fledged)

#make a new dataframe for the regressions
# 2)SW: I would therefore suggest that you try to get your weekly values into analysable format: 
#col 1: week; col 2: Tag; col 3: weekly betweenness; col 4: weekly degree; col 5: scaled fledge order; col 6: fledge weight; col 7: family, 8: time since fledging (the order can of course be different)
# for the last one (time since fledging): we should include this as a measure of age. See if you can calculate this from the data (fd). The column 'fledged' will be useful - the units are the number of days since the 1st of April. 
# Using the weekly network allows you to only include the individuals that were actually present and alive during that week (which makes the data a lot more accurate). 
#And you are correct, the chicks only started to enter the population in week 4. 

#EZ: I start the new dataframe at week4 as the Chicks start to appear at that point in time
#step1 -> merge all the table_weekX dataframes obtained seperatly for each week (see above)
#they all have the same columns --> 
table_week <- rbind(table_week4,table_week5, table_week6, table_week7, table_week8, table_week9, table_week10, table_week11, table_week12, table_week13, table_week14)
class(table_week)
View(table_week)
#step2 choose only some columns of fd_withoutNA
library(dplyr)
fd_new <- fd_withoutNA %>%
  select(Tag, scaled_FledgeOrder2, Chick.weight, time_since_fledgling, Fledged, Family)
class(fd_new)
View(fd_new)
duplicated(fd_new$Tag) #Tag is unique
#step3: merge table_week and fd_new by Tag, both are data.frames
new_data0 <- merge(table_week, fd_new, by="Tag", all = TRUE, sort = FALSE)
View(new_data0)#Contains NA because the file table_week contains the adults too. SHould have subset to Who==Chicks before making the tables. I can still NA omit.
new_data <- na.omit(new_data0)
View(new_data)
#SW:(time since fledging): we should include this as a measure of age. See if you can calculate this from the data (fd). The column 'fledged' will be useful - the units are the number of days since the 1st of April. 



##Statistical models
# 3) SW: Have you checked for multicollinearity of your predictors (fledge order and fledge weight)? It can greatly mess up your model if they are correlated. Tip here is to calculate the 'variance inflation factor'. Should be straight forward to google it. Then only include both measures if their VIF is appropriate.
#For degree centrality/week and Betweennness/week
#NB! From week4 because Chicks appear from week 4. However, only 1 chick at week4, not enough data to make an analysis
#Start the analysis at week6 ( 17chicks), week5 has 6 chicks (not enough for analysis)
#Use new_data
#degree centrality and betweenness centrality are both continuous variables --> choose models with normal distribution
#I have one observation per week. Hence there are repeated measurements for each Tag (due to the week). But if I do a statistical model for each week, there won't be any repeated values anymore. 
### The repeated measurements will be for family, hence Family must be a random effect. 
head(new_data)

str(new_data$Family)
as.numeric(new_data$Family)
library(dplyr)
new_data %>% summarise(count = n_distinct(Family)) #18 families

#The models
library(lme4)
d <- lmer(degree ~scaled_FledgeOrder2*time_since_fledgling+ (1|Tag) + (1|Family:Tag) , data=new_data)
summary(d)#doesn't give p-values
coefs <- data.frame(coef(summary(d)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs

b <- lmer(betweenness ~scaled_FledgeOrder2*time_since_fledgling+ (1|Tag)+ (1|Family:Tag) , data=new_data)
summary(b)
coefs1 <- data.frame(coef(summary(b)))
coefs1$p.z <- 2 * (1 - pnorm(abs(coefs1$t.value)))
coefs1

##multivariate analysis
PCA <- new_data%>%
  group_by(Tag, degree, betweenness) %>%
  tally()
PCA

attach(PCA)
y <- cbind(degree, betweenness)
cor(y)

pc1 <- princomp(y, cor=T)
loadings(pc1)
summary(pc1)
#Comp1 (1.1231863), Comp2 (0.8593326)

biplot(pc1, pc.biplot=T)
library(ggplot2)
library(factoextra)

pca <- prcomp(~degree + betweenness, scale = TRUE, data=PCA)

library(factoextra)
fviz_pca_var(pca, col.var="darkblue", title= "PCA-Aggression")




#SW: 
# 4) The models you would be looking at then are *mixed effects models* that take into account repeated measures of the same individual, and it also allows us to include an effect of family (if for example chicks from Nest X are consistently more central because of a genetic effect). 
# It would be specified something along those lines: centrality ~ rel.fledge order*scale(time.since.fledging) + (1|Tag) + (1|family:Tag)
# And the equivalent for degree
# If you feel brave, you can even read up on multivariate models that allow you to include both outcome variables at the same time: (centrality, degree) ~ ...
# Here some keywords that will help get to the right model: multivariate regression; nested random effects; mixed effects models
# I don't have a suggestion for a package per se - I usually use Bayesian regression for all of my models these days (package brms), since they are a little more versatile, but you can of course use others.


