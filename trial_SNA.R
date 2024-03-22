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

gmm.summer <- gmmevents(
  time = net.data.summer$Date.Time,
  identity = net.data.summer$PIT,
  location = net.data.summer$location,
  verbose = TRUE,
  splitGroups = TRUE
)

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
View(metadata)
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

library(asnipe)
network_week1 <- get_network(gbi1.sub, data_format="GBI",
                             association_index="SRI")


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
#threshold <- 5
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

#threshold <- 5
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

#threshold <- 5
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

#threshold <- 5
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

#threshold <- 5
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

#threshold <- 5
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

#threshold <- 5
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

#threshold <- 5
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

#threshold <- 5
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

#threshold <- 5
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

#threshold <- 5
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

#threshold <- 5
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

#threshold <- 5
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
library(datawizard)

# SW: I'm adding a column where I scale the fledge order within nests, rather than across nests
# i.e. fledge order/max fledge order -0.5
# so out of a nest of three, the first would have 1/3-0.5=-0.16, the second 2/3-0.5=0.16, the third 3/3-0.5=0.5

fd$fledge.order.scaled.within <- fd$Fledge.order

for(i in fd$Box){
  
  tags.box.chicks <- subset(fd, fd$Box==i & fd$Who=="Chick")
  if(length(tags.box.chicks$Box)==1){
    fd[as.numeric(rownames(tags.box.chicks)), "fledge.order.scaled.within"] <- NA
  } else {
    fd[as.numeric(rownames(tags.box.chicks)), "fledge.order.scaled.within"] <-   datawizard::normalize(tags.box.chicks$Fledge.order)
  }
}


hist(fd$fledge.order.scaled.within)


# this is how the two scalings compare: on the x axis scaled across all nests, on the y axis only scaled within nests
library(datawizard)
fd$scaled_FledgeOrder2 <- datawizard::normalize(fd$Fledge.order, method = "range", range = c(0, 1))
View(fd)
fd_withoutNA <- na.omit(fd) #keeps only the observations with fledge.order > 2
View(fd_withoutNA)
plot(fd_withoutNA$scaled_FledgeOrder2, fd_withoutNA_2$fledge.order.scaled.within)

# all first fledgers now have a value of 0, all last fledgers a value of 1 (thx to datawizard::normalize)
# I have excluded those where only one chick fledged with if(length(tags.box.chicks$Box)==1){
#  fd[as.numeric(rownames(tags.box.chicks)), "fledge.order.scaled.within"] <- NA






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
  select(Tag, scaled_FledgeOrder2, fledge.order.scaled.within, Chick.weight, Fledged, Family)
class(fd_new)
View(fd_new)
duplicated(fd_new$Tag) #Tag is unique
#step3: merge table_week and fd_new by Tag, both are data.frames
new_data0 <- merge(table_week, fd_new, by="Tag", all = TRUE, sort = FALSE)
View(new_data0)#Contains NA because the file table_week contains the adults too. Should have subset to Who==Chicks before making the tables. I can still NA omit.
new_data <- na.omit(new_data0)
View(new_data)
#SW:(time since fledging): we should include this as a measure of age. See if you can calculate this from the data (fd). The column 'fledged' will be useful - the units are the number of days since the 1st of April. 

###Calculate new column: Time since fledging for each ind and each week
head(new_data)
library(lubridate)
#I want Fledge into a date, counting since the 01/04/2020
str(new_data$Fledged)#has to be numeric for the following code
new_data$Fledged <- as.numeric
reference_date <- as.Date("2020-04-01")
new_data$Fledge.date <-reference_date + days(new_data$Fledged)

#I want to see which dates correspond to the data collection for each week
# Extract the value from the original data frame and adding them to the data frame (new_data)
metadata$date3[metadata$week == 1]#2020-05-06
metadata$date3[metadata$week == 2]#2020-05-13
metadata$date3[metadata$week == 3]#2020-05-20
metadata$date3[metadata$week == 4]#2020-05-27
metadata$date3[metadata$week == 5]#2020-06-03
metadata$date3[metadata$week == 6]#2020-06-10
metadata$date3[metadata$week == 7]#2020-06-17
metadata$date3[metadata$week == 8]#2020-06-24
metadata$date3[metadata$week == 9]#2020-07-01
metadata$date3[metadata$week == 10]#2020-07-08
metadata$date3[metadata$week == 11]#2020-07-15
metadata$date3[metadata$week == 12]#2020-07-22
metadata$date3[metadata$week == 13]#2020-07-29
metadata$date3[metadata$week == 14]#2020-08-05

library(dplyr)
new_data <- new_data %>% mutate(date.data.collection = case_when(Week == 1 ~ "2020-05-06", 
                               Week == 2 ~ "2020-05-13",
                               Week == 3 ~ "2020-05-20",
                               Week == 4 ~ "2020-05-27",
                               Week == 5 ~ "2020-06-03",
                               Week == 6 ~ "2020-06-10",
                               Week == 7 ~ "2020-06-17",
                               Week == 8 ~ "2020-06-24",
                               Week == 9 ~ "2020-07-01",
                               Week == 10 ~"2020-07-08",
                               Week == 11 ~ "2020-07-15",
                               Week == 12 ~"2020-07-22",
                               Week == 13 ~"2020-07-29",
                               Week == 14 ~"2020-08-05"))


#Now substract: "date.data.collection" - "Fledge.date" --> gives the age of the chicks at each week (since fledging)
View(new_data)
new_data$date.data.collection  <- as.Date(new_data$date.data.collection, format = "%Y-%m-%d")
new_data$age <- (new_data$date.data.collection) - (new_data$Fledge.date) #gives the time difference in days
new_data$age <- as.numeric(new_data$age)


##Statistical models
# 3) SW: Have you checked for multicollinearity of your predictors (fledge order and fledge weight)? It can greatly mess up your model if they are correlated. Tip here is to calculate the 'variance inflation factor'. Should be straight forward to google it. Then only include both measures if their VIF is appropriate.
#For degree centrality/week and Betweennness/week
#NB! From week4 because Chicks appear from week 4. However, only 1 chick at week4, not enough data to make an analysis
#Use new_data
#degree centrality and betweenness centrality are both continuous variables --> choose models with normal distribution
head(new_data)


str(new_data$Family)
new_data$Family <- as.factor(new_data$Family)
str(new_data$scaled_FledgeOrder2)
str(new_data$fledge.order.scaled.within)
str(new_data$Tag)
str(new_data$degree)
str(new_data$age)
#The models
library(lme4)
library(lmerTest)



#d <- lmer(degree ~ scaled_FledgeOrder2*age + Chick.weight*scaled_FledgeOrder2 + Chick.weight*age + (1|Tag) + (1|Family:Tag) , data=new_data)

# SW: this model does not seem to converge, so you cannot really use it for any itnerpretation or to get reliable vifs. I would recommend just using a simple linear regression to test for vifs. The interaction terms are not really an issue: https://statisticalhorizons.com/multicollinearity/
###EZ: I correct the model first, then I look for multi-collinearity
#to correct converge issues --> scale age (added a new column in new_data for this) "scale.age"
## Also, problems if I add more than 1 random effect.
new_data$scale.age <- scale(new_data$age)
View(new_data)

#degree
d <- lmer(degree ~scaled_FledgeOrder2*scale.age + Chick.weight*scaled_FledgeOrder2 + Chick.weight*scale.age + (1|Tag), data= new_data)
qqnorm(residuals(d))
qqline(residuals(d))
hist(residuals(d))


shapiro.test(resid(d))#not normal
hist(new_data$degree)
trans <- log(new_data$degree)#I have tried other transformations: sqrt, ^2, 1/degree...Nothing works
hist(trans)
d <- lmer(trans ~scaled_FledgeOrder2*scale.age + Chick.weight*scaled_FledgeOrder2 + Chick.weight*scale.age + (1|Tag), data= new_data)
qqnorm(residuals(d))
qqline(residuals(d))
shapiro.test(resid(d))#not normal
hist(new_data$degree)

summary(d)
drop1(d,test="F")#the interactions are not significant, I leave them out of the model
d0 <- lmer(degree ~scaled_FledgeOrder2 + Chick.weight + scale.age + (1|Tag), data= new_data)
summary(d0)
par(mfrow = c(1,1))
plot(d0)
plot(residuals(d0) ~ fitted(d0), main = "residuals v.s. Fitted")
qqnorm(residuals(d0))
qqline(residuals(d0))
shapiro.test(resid(d0))#not normal

#Even if the transformation doesn't ameliorate the assumptions, I can based on this article https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/2041-210X.13434 suppose that my mixed model is robust against the normality assumption
#Hence, the model I take into account is: d0

library(car)
vif(d0)
#simplify model by checking for multi-collinearity
require(car)
# scaled_FledgeOrder2          scale(age)        Chick.weight 
# 1.121848            1.000856            1.122754 

# No multi-collinearity problems

#Best to indeed use glmer because then the model is more robust against assumption deviations
library(lme4)
library(lmerTest)
d_glmer0 <- glmer(degree ~scaled_FledgeOrder2*scale.age + Chick.weight*scaled_FledgeOrder2 + Chick.weight*scale.age + (1|Tag) + (1|Tag:Family), data= new_data, family=gaussian)
#model failed to converge -> has to be simplified, so I chose to eave a random factor behind
d_glmer1 <- glmer(degree ~scaled_FledgeOrder2*scale.age + Chick.weight*scaled_FledgeOrder2 + Chick.weight*scale.age + (1|Tag), data= new_data, family=gaussian)
report(d_glmer1)
summary(d_glmer1)#I leave out the interactions as they are not significant
d_glmer2 <- glmer(degree ~scaled_FledgeOrder2 + scale.age + Chick.weight + (1|Tag), data= new_data, family=gaussian)
summary(d_glmer2)
Anova(d_glmer2)#to extract the p-values that are not in the output
vif(d_glmer2)#no multi-collinearity problems
library(report)
report(d_glmer2)#also shows the p-values
#Nothing is significant



#betweenness
hist(new_data$betweenness)#skewed distribution --> normality is not respected, see further steps
b <- lmer(betweenness ~scaled_FledgeOrder2*scale.age + Chick.weight*scaled_FledgeOrder2 + Chick.weight*scale.age + (1|Tag)+ (1|Family:Tag) , data=new_data)
qqnorm(resid(b))
qqline(resid(b))
shapiro.test(resid(b))#not normal
log <- log((new_data$betweenness)+1)#to handel the zero values
hist(log)#seems better
bb <- lmer(log ~scaled_FledgeOrder2*scale.age + Chick.weight*scaled_FledgeOrder2 + Chick.weight*scale.age + (1|Tag)+ (1|Family:Tag) , data=new_data)
#model failed to converge
bb <- lmer(log ~scaled_FledgeOrder2*scale.age + Chick.weight*scaled_FledgeOrder2 + Chick.weight*scale.age + (1|Tag) , data=new_data)
qqnorm(resid(bb))
qqline(resid(bb))
shapiro.test(resid(bb))#still not normal, but already a better match. 

drop1(bb, test="F") #Leave the interactions out
b0 <- lmer(log ~scaled_FledgeOrder2 + Chick.weight + scale.age + (1|Tag)+ (1|Family:Tag) , data=new_data)
#Here the two random effects don't seem to cause  convergence problems...
summary(b0)
qqnorm(resid(b0))
qqline(resid(b0))
shapiro.test(resid(b0)) #normality is not met

#If I suppose my model is robust against non normality (https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/2041-210X.13434)
b01 <- lmer(betweenness ~scaled_FledgeOrder2 + Chick.weight + scale.age + (1|Tag)+ (1|Family:Tag) , data=new_data)
summary(b01)
library(car)
vif(b01)
#scaled_FledgeOrder2        Chick.weight           scale.age 
# 1.177683            1.179279            1.001471
#No multi-collinearity problems

#Best to indeed use glmer because then the model is more robust against assumption deviations
library(lme4)
library(lmerTest)
b_glmer0 <- glmer(betweenness ~scaled_FledgeOrder2*scale.age + Chick.weight*scaled_FledgeOrder2 + Chick.weight*scale.age + (1|Tag) + (1|Tag:Family), data= new_data, family=gaussian)
#model failed to converge -> has to be simplified, so I chose to eave a random factor behind
b_glmer1 <- glmer(betweenness ~scaled_FledgeOrder2*scale.age + Chick.weight*scaled_FledgeOrder2 + Chick.weight*scale.age + (1|Tag), data= new_data, family=gaussian)
summary(b_glmer1)#I leave out the interactions as they are not significant
report(b_glmer1)
b_glmer2 <- glmer(betweenness ~scaled_FledgeOrder2 + scale.age + Chick.weight + (1|Tag), data= new_data, family=gaussian)
summary(b_glmer2)
Anova(b_glmer2)#to extract the p-values that are not in the output
vif(b_glmer2)#no multi-collinearity problems
library(report)
report(b_glmer2)#also shows the p-values
#Nothing is significant


# SW: there is a very useful library that can help with interpretation of most statistical tests
library(report)
report(d0)
report(b01)

#no effect of fledge order on the position in the social network


##Try the models when scaled_FledgeOrder2 is a factor
min(new_data$scaled_FledgeOrder2)#0

# SW: I've changed this to a 0 (0 are first fledglings, higher is others)
library(dplyr)
new_data <- new_data %>%
  mutate(factor.order = ifelse(scaled_FledgeOrder2 == 0, "First", "Other"))
View(new_data)
View(new_data)
new_data$factor.order <- as.factor(new_data$factor.order)
str(new_data$factor.order)

#degree
library(lme4)
library(lmerTest)
hist(new_data$degree)
d <- lmer(degree ~ factor.order*scale.age + Chick.weight*factor.order + Chick.weight*scale.age + (1|Tag) + (1|Family:Tag) , data=new_data)
qqnorm(resid(d))
qqline(resid(d))
shapiro.test(resid(d))#not normal --> mixed models are robust against non normality (https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/2041-210X.13434)
drop1(d, test="F")#leave out the interactions, they are not significant

d0 <- lmer(degree ~ factor.order + Chick.weight + scale.age + (1|Tag)+ (1|Family:Tag) , data=new_data)
summary(d0)
#very low variance family:Tag, could this be left out? If so, it does't influence the results that much, still no significance

#simplify model by checking for multi-collinearity
require(car)
vif(d0)
drop1(d, test="F")
#factor.order Chick.weight    scale.age 
#1.088888     1.089612     1.000852 
#no multi-collinearity problems

#with glmer
df_glmer0 <- glmer(degree ~ factor.order*scale.age + Chick.weight*factor.order + Chick.weight*scale.age + (1|Tag) + (1|Family:Tag) , data=new_data, family=gaussian)
summary(df_glmer0)
Anova(df_glmer0)
report(df_glmer0) #the interactions are not significant, hence I leave them out
df_glmer1 <- glmer(degree ~ factor.order + scale.age + Chick.weight + (1|Tag) + (1|Family:Tag) , data=new_data, family=gaussian)
summary(df_glmer1)
Anova(df_glmer1)
report(df_glmer1)
vif(df_glmer1)#no prob with multi-collinearity


#betweenness
b <- lmer(betweenness ~ factor.order*scale.age + Chick.weight*factor.order + Chick.weight*scale.age  + (1|Tag)+ (1|Family:Tag) , data=new_data)
drop1(b, test="F")#interactions are unsignificant, leave them out

b0 <- lmer(betweenness ~factor.order + Chick.weight + scale.age  + (1|Tag)+ (1|Family:Tag) , data=new_data)
#problem with convergence --> leave out the random factor (1|Family:Tag)

b1 <- lmer(betweenness ~factor.order + Chick.weight + scale.age  + (1|Tag) , data=new_data)
vif(b1)
#factor.order Chick.weight    scale.age 
#1.120728     1.122299     1.001596 
#no problems with multi-collinearity
summary(b1)

#with glmer
bf_glmer0 <- glmer(betweenness ~ factor.order*scale.age + Chick.weight*factor.order + Chick.weight*scale.age  + (1|Tag)+ (1|Family:Tag) , data=new_data, family=gaussian)
summary(bf_glmer0)
Anova(bf_glmer0)
report(bf_glmer0)
#I leave out the interactions
bf_glmer1 <- glmer(betweenness ~ factor.order + scale.age + Chick.weight + (1|Tag)+ (1|Family:Tag) , data=new_data, family=gaussian)
#prob with convergence, so I leave out a random effect
bf_glmer2 <- glmer(betweenness ~ factor.order + scale.age + Chick.weight + (1|Tag) , data=new_data, family=gaussian)
summary(bf_glmer2)
Anova(bf_glmer2)
report(bf_glmer2)
vif(bf_glmer2)#no prob with multi-collinearity
#nothing is significant


##Try when last sibling versus all other instead of the first. 
max(new_data$scaled_FledgeOrder2)#1

# SW: it occurs to me here that these are actually not all last siblings to fledge. Since we have scaled it across the largest clutch size, some last fledgers may have a different value than 1. 


# 1 are first fledglings, higher is others 
# SW: I've changed it to the newly generated fledge order that is scaled within each nest, so values of 1 are last fledgers
library(dplyr)
new_data <- new_data %>%
  mutate(factor.order2 = ifelse(fledge.order.scaled.within == 1, "Last", "Other"))
View(new_data)
length(which(new_data$factor.order2=="Last"))
# these are now 19 individuals

#degree
new_data$scale.age <- scale(new_data$age)
d_within <- glmer(degree ~ factor.order2*scale.age + Chick.weight*factor.order2 + Chick.weight*scale.age  + (1|Tag)+ (1|Family:Tag) , data=new_data, family=gaussian)
summary(d_within)
library(car)
Anova(d_within)
library(report)
report(d_within)
#Leave out the interactions
d_within1 <- glmer(degree ~ factor.order2 + scale.age + Chick.weight  + (1|Tag)+ (1|Family:Tag) , data=new_data, family=gaussian)
#fail to converge --> leave out one random factor
d_within2 <- glmer(degree ~ factor.order2 + scale.age + Chick.weight  + (1|Tag), data=new_data, family=gaussian)
summary(d_within2)
vif(d_within2)#no prob
Anova(d_within2)
report(d_within2)#nothing is significant



#betweenness
b_within0 <- lmer(betweenness ~ factor.order2*scale.age + Chick.weight*factor.order2 + Chick.weight*scale.age  + (1|Tag)+ (1|Family:Tag), data=new_data)
qqnorm(resid(b_within0))
qqline(resid(b_within0))
shapiro.test(resid(b_within0))#not normal --> mixed models are robust against non normality (https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/2041-210X.13434)
drop1(b_within0, test="F")
#not normal --> use rather glmer because at least these models are stable against assumption deviations

b_within <- glmer(betweenness ~ factor.order2*scale.age + Chick.weight*factor.order2 + Chick.weight*scale.age  + (1|Tag)+ (1|Family:Tag), data=new_data, family=gaussian)
summary(b_within)
Anova(b_within)
report(b_within)#factor.order2:scale.age is significant
vif(b_within)#problems of multicollinearity
#Chick.weight*factor.order2 is almost significant (tendency) with p-val =0.08804 
#leave out the unsignificant interactions

b_within1 <- glmer(betweenness ~ factor.order2*scale.age + Chick.weight + (1|Tag)+ (1|Family:Tag) , data=new_data, family=gaussian)
#Model failed to converge, hence I leave out the random factor

b_within2 <- glmer(betweenness ~ factor.order2*scale.age + Chick.weight + (1|Tag) , data=new_data, family=gaussian)
summary(b_within2)
Anova(b_within2)
report(b_within2)#the interaction scale.age:fledge.order2 is no longer significant
vif(b_within2)
#scale.age has a high VIF (9.963451)
#Leave out the interaction as it is no longer significant

b_within3 <- glmer(betweenness ~ factor.order2 + scale.age + Chick.weight + (1|Tag) , data=new_data, family=gaussian)
summary(b_within3)
Anova(b_within3)
report(b_within3)#the interaction scale.age:fledge.order2 is no longer significant
vif(b_within3)#no longer problems with multi-collinearity

##!! What if betweenness is a count?: https://link.springer.com/article/10.1007/s00265-017-2425-y
#Poisson distribution
hist(new_data$betweenness) ##Indeed, could be Poisson
b_within0.0 <- glmer(betweenness ~ factor.order2*scale.age + Chick.weight*factor.order2 + Chick.weight*scale.age  + (1|Tag)+ (1|Family:Tag), data=new_data, family=poisson)
b_within1.1 <- glmer(betweenness ~ factor.order2+ scale.age + Chick.weight +factor.order2:scale.age + Chick.weight:factor.order2 + Chick.weight:scale.age  + (1|Tag), data=new_data, family=poisson)
summary(b_within0.0)
library(car)
Anova(b_within0.0)
#Difficulties with these models

#When using brms for this, R tells me to use only integer response variables. --> If I transform betweenness in integer values I will lose information. Not such a good idea
# Here They normalize betweenness https://conbio.onlinelibrary.wiley.com/doi/full/10.1111/cobi.13383
#Here they use linear mixed models (seems gaussian, not poisson): https://academic.oup.com/jeb/article/33/11/1634/7326429



# SW: you don't have to use brms - this is just the package I am most familiar with. 
# Here is a tutorial if you are interested in using this package: https://ourcodingclub.github.io/tutorials/brms/

#SW: to test degree and betweenness at the same time: use package brms
install.packages("brms")

# SW: I've been trying a couple of things here - the models seem to fit pretty poorly atm. 
library(brms)

m1 <-
  brm(
    mvbind(degree, betweenness) ~ fledge.order.scaled.within*scale.age + Chick.weight*fledge.order.scaled.within + Chick.weight*scale.age + (1|Tag) + (1|Family:Tag) , data=new_data
  )


summary(m1)
pp_check(m1, resp="degree")
pp_check(m1, resp="betweenness")
# these look like pretty poor models

plot(m1)

#####Test for assortment in fledge order 
install.packages("assortnet")#issues when installing -> now resolved
library("assortnet")


#Take first the initial network, that that contains all the weeks
library("asnipe")

network <- get_network(gbi, data_format = "GBI",
                       association_index = "SRI")



# SW: I here change it to the scaled fledge order wihin the nest
fd$fledge.order.scaled.within#Its continous
length(which(fd$Who=="Chick"& fd$Location =="Castle"& !is.na(fd$fledge.order.scaled.within)))#112 individuals
length(rownames(network))#187


# SW: I have compiled a function where you can submit a network and it automatically extracts the fledge order for those we have data for. It then runs the assortment function and returns the assortment p value, r rand, r, and the size of the network
network.in <- network

assortment.function <- function(network.in){
  
  vec <- NULL #the vector is now empty but will be filled with the fledge order
  #for(i) means that I create a location called i that will contain the names of my initial network
  for(i in rownames(network.in)){
    i.fledge.order <- unique(subset(fd$fledge.order.scaled.within, fd$Tag==i)) #i.fledge.order becomes all the individuals that have a fledge.order for which their Tag equals to i
    if(length(i.fledge.order)==0){ #if i.fledge.order has no length, that means that there is no fledge.orde, therefore that means that we identify the adults that are not the parents of the chicks
      i.fledge.order <- NA #These adults received then now a value of NA
    }
    vec[which(rownames(network.in)==i)] <- i.fledge.order #The empty vector will now contains the row of the network (the tags) that are equal to i, meaning the rows that are also found in the fd data. NB: these rownames (the Tags) have to be of same length and in the same order
  }
  
  # we need to subset both the network and the vector with the fledge order to those we have data for:
  network.in <- network.in[-which(is.na(vec)), -which(is.na(vec))]
  
  # remove the NAs from the fledge order vector
  vec <- as.vector(na.omit(vec))
  # vec now contains the fledge order that is scaled WITHIN each nest
  
  # here we run the actual assortment function on the subset network and its corresponding fledge order values
  vec.rand <- NULL #make an empty vector
  assort <- assortment.discrete(graph=network.in, types = vec , weighted = TRUE)
  object <- NULL #make a new empty vector
  for(i in 1:1000){
    # we use node based permutation for i
    # i will take randomly Tags and permute it 1000 times
    rand.phenotype <- sample(vec)
    r.rand <- assortment.discrete(graph = network.in, types = rand.phenotype, weighted = TRUE)$r
    #this gives the r of the assortment, the observed r
    vec.rand[i] <- r.rand #now I have a vector called vec.rand that contains the r values of the assortment that are all obtained randomly by the permutations
  }
  # extract where the real r falls among the computed r (which corresponds to our p value)
  p <- length(which(vec.rand < assort$r))/1000 #compare the real r we got from our data to the one obtained by permutation to see whether the r obtained with our data is significant or not.
  #Because if we do a histogram we see that the distribution of the random r falls very far left from the observed r, we know that we have to do 1-p, hence the symbole <. If not the case, then symbole in the other way.
  object$network <- network.in
  object$p <- p
  object$r <- assort$r
  object$vec.rand <- vec.rand
  return(object) #now when I run it, I ask RStudio to return to me the p and r saved in object. That becomes the output when I run the function
}


set.seed(5)
assort <- assortment.function(network.in=network)

assort$p
assort$r
#SW: p=0.158
#EZ: I have another p-value
# that is entirely possible - since we are running 1000 randomizations,the p value could be quite different. We can overcome this by setting set.seed before we run the function, then it will always take the same set of 'random' values

# this should plot the histogram and add the line
hist(assort$vec.rand)
abline(v=assort$r, col="red")
# p is significant if either below 0.05 or above 0.95

length(rownames(assort$network)) # the network now contains 45 individuals that have data on fledge order

#Assortnet for each week
# SW: in theory you should be able to just run the function on each week separately now:
#EZ: I changed the symbole (< >) for the p value calculation in the function depending on the histogram of vec.rand obtained for each week

# e.g.
assort.week5 <- assortment.function(network.in=network_week5)

dim(assort.week5$network)
# only 6 individuals - probably too few to run any analysis


assort.week6 <- assortment.function(network.in=network_week6)

dim(assort.week6$network)#17
assort.week6$p
assort.week6$r
hist(assort.week6$vec.rand)
abline(v=assort.week6$r, col="red")

assort.week7 <- assortment.function(network.in=network_week7)

dim(assort.week7$network)#13
assort.week7$p #0.075
assort.week7$r
hist(assort.week7$vec.rand)
abline(v=assort.week7$r, col="red")

assort.week8 <- assortment.function(network.in=network_week8)

dim(assort.week8$network)#16
assort.week8$p #0.45
assort.week8$r
hist(assort.week8$vec.rand)
abline(v=assort.week8$r, col="red")

assort.week9 <- assortment.function(network.in=network_week9)

dim(assort.week9$network)#16
assort.week9$p #0.445
assort.week9$r
hist(assort.week9$vec.rand)
abline(v=assort.week9$r, col="red")

assort.week10 <- assortment.function(network.in=network_week10)

dim(assort.week10$network)#18
assort.week10$p #0.191
assort.week10$r
hist(assort.week10$vec.rand)
abline(v=assort.week10$r, col="red")

assort.week11 <- assortment.function(network.in=network_week11)

dim(assort.week11$network)#13
assort.week11$p #0.271
assort.week11$r
hist(assort.week11$vec.rand)
abline(v=assort.week11$r, col="red")

assort.week12 <- assortment.function(network.in=network_week12)

dim(assort.week12$network)#23
assort.week12$p #0.007 --> significant!!!
assort.week12$r
hist(assort.week12$vec.rand)
abline(v=assort.week12$r, col="red")

# SW: I guess the significance could also just be a chance result - if we're doing multiple testing, we are bound to find a significant effect at some point. That's why people usually correct for multiple testing e.g. by Bonferroni corrections or such. Given there is no evidence for assortment in any of the other weeks, this may not be a real effect.


assort.week13 <- assortment.function(network.in=network_week13)

dim(assort.week13$network)#14
assort.week13$p #0.372 --> significant!!!
assort.week13$r
hist(assort.week13$vec.rand)
abline(v=assort.week13$r, col="red")

assort.week14 <- assortment.function(network.in=network_week14)

dim(assort.week14$network)#16
assort.week14$p #0.296 --> significant!!!
assort.week14$r
hist(assort.week14$vec.rand)
abline(v=assort.week14$r, col="red")



# SW: I'm saving the current workspace so I can just load it again next time without having to rerun everything
save.image(file="R.image.RData")
load("R.image.RData")



####Repeatability of the network position
library(rptR)
head(new_data)

#degree
r1 <- rpt(degree ~ (1|Tag), grname = "Tag", data = new_data, datatype = "Gaussian", CI=0.95,
    nboot = 1000, npermut = 0)
#R  = 0.287
#SE = 0.091
#CI = [0.095, 0.449]
#P  = 0.000163 [LRT] --> significant
#NA [Permutation]

# SW: I noticed a warning about singularity in the model - this is probably something we'll have to deal with, although I'm not sure yet how. 

summary(r1)
plot(r1)

#Other method
#Here: https://link.springer.com/article/10.1007/s00265-017-2425-y
#Uses lmer into rptR package. 
#After does 1000 iterations --> don't know yet how to do that (see below)
r2 <-rptGaussian(degree ~ factor.order2 + scale.age + Chick.weight  + (1|Tag), grname= "Tag", data= new_data, CI = 0.95, nboot = 1000, npermut=0)
#Repeatability for Tag
#R  = 0.302
#SE = 0.095
#CI = [0.118, 0.486]
#P  = 0.000231 [LRT]
#NA [Permutation]
summary(r2)
r2$R

d_within2 <- glmer(degree ~ factor.order2 + scale.age + Chick.weight + (1|Tag) , data=new_data, family=gaussian)
summary(d_within2)

#Repeatability= (between-group variance)/(between-group variance) + (Within group variance)
#So, from the glmer output: Variance of Tag/(Variange of Tage + Variance of the residuals)
75.07/ (75.07+173.41) #0.3021169 --> exactly what gives rptGaussian function, seems a rounding of of rpt()


#betweenness
rpt(betweenness ~ (1|Tag), grname = "Tag", data = new_data, datatype = "Gaussian", CI=0.95,
    nboot = 1000, npermut = 0)
#R  = 0.093
#SE = 0.069
#CI = [0, 0.255]
#P  = 0.0699 [LRT] --> significant
#NA [Permutation]
#Singular Fit problems

# SW: does it make sense to include all of these predictors when we found they are not good predictors in our model above? Should we not just include the random effect?

rptGaussian(betweenness ~ factor.order2 + scale.age + Chick.weight  + (1|Tag), grname= "Tag", data= new_data, CI = 0.95, nboot = 1000, npermut=0)
#singular boundary fit problem --> probably overfitting --> random part too complex which I found wierd
#Repeatability for Tag
#R  = 0.098
#SE = 0.074
#CI = [0, 0.271]
#P  = 0.109 [LRT]
#NA [Permutation]

#Other method: by using the ICC (intraclass correlation coefficient)
#https://www.sciencedirect.com/science/article/pii/S0003347209000189?casa_token=soMn6gw8gf0AAAAA:Go4hPYSJPGOJmL8aCkBNgWuTRI0QpEQu6GQI2CAieBtgZm7XKTBT_c2LHqCtoQcAMYwpC64wbcwU
#https://academic.oup.com/beheco/article/28/1/85/2453500
install.packages(irr)
library("irr") #function icc()
#needs a dataframe whith response variable for each week --> merge of all the centrality tables
#one for betweenness, one for degree
## dataframe such as: Tag    betweenness$week1   betweenness$week2    betweenness$week3 etc
#Even so, this can't be done as the lengths of betweenness and degree for each chicks are different. Not the same lengths for each week


#Here propose to do this:https://academic.oup.com/jeb/article/33/11/1634/7326429
#We consider the observed repeatabilities as statistically significant when their 95% credible intervals (hereafter 95CI) did not include the mean of the respective permuted repeatabilities.
#! had to do permutations in order to test for statistical significance
#"1000 iterations of the observed data for every model"
#See below



#Try the method with the 1000 permutations 
#First resample 1000 times (iterations, like for i=1000 in a function maybe) each weekly network
#From these randomized weekly networks, calculate new degree and betweenness
#Then, compare the observed repeatability (R) with the ones from the randomized networks
#This will tell whether it is significant or not
#Following this tutorial: https://dshizuka.github.io/networkanalysis/example_assortment_in_wytham.html
#However, not the most appropriate tutorial for my code

#All weeks together
library("asnipe")
network <- get_network(gbi, data_format = "GBI",
                       association_index = "SRI")

#making graph with i_graph 
library("igraph")
net <- graph_from_adjacency_matrix(network,mode= c("undirected"), diag=FALSE, weighted=TRUE)

#resample for each week and then recalculate degree and betweenness for each week
s <- sample(V(net), length(V(net)), replace=F) 

#For each week seperately:
#starting from week4 as chicks only started to appear then
library(igraph)
library(asnipe)
library(rptR)
network_week4 <- get_network(gbi4.sub, data_format="GBI",
                             association_index="SRI")

# SW: I guess the sampling needs to happen here rather than after calling the igraph function:
new_order <- sample(rownames(network_week4))
rownames(network_week4) <- new_order
colnames(network_week4) <- new_order

net4 <- graph_from_adjacency_matrix(network_week4,mode= c("undirected"), diag=FALSE, weighted=TRUE)
#s4 <- sample(V(net4), replace=F) #when doing so I lose the pairs that were made
s4_deg <- degree(net4)#doens't work

# should work now. 

# SW: I see you have tried different things down below. I think the above approach is actually very promising! I have taken what you have started and put it in a funciton


permute.networks <- function(gbi, week){
  
  network_week <- get_network(gbi, data_format="GBI",
                               association_index="SRI")
  
  new_order <- sample(rownames(network_week))
  rownames(network_week) <- new_order
  colnames(network_week) <- new_order
  
  net <- graph_from_adjacency_matrix(network_week,mode= c("undirected"), diag=FALSE, weighted=TRUE)
  perm_deg <- degree(net)
  perm_betw <- betweenness(net)
  
  perm <- as.data.frame(as.matrix(cbind( week, perm_deg, perm_betw)))
  perm$Tag <- rownames(perm)
  rownames(perm) <- NULL
  return(perm)
}


# as an example, here it runs for just week 4:
perm4 <- permute.networks(gbi=gbi4.sub, week=4)
head(perm4)


# now we want to run this for all weeks and 1000 times - I try this in a loop here:

# this object is where we will save the output (1000 different data frames that contain data on the randomized networks)
# I am only running it on week 4 and 5, but you can add all of the others equivalently
perm_object <- NULL

for(i in 1:1000){

  perm4 <- permute.networks(gbi=gbi4.sub, week=4)
  perm5 <- permute.networks(gbi=gbi5.sub, week=5)
  # complete this list of weekly networks
  
  perm_combined <- rbind(perm4,
                         perm5) # also complete the list here
  
  # we save it in a list
  perm_object[[i]] <- perm_combined
  
}

# our perm_object contains 1000 different data frames
# data frame 1:
perm_object[[1]]
# data frame 2:
perm_object[[2]]
# etc.


# Now you should be able to run the repeatability function on each of those object slots:
# see if you can figure it out and how to store the repeatability output for each of the data frames

for(i in 1:1000){
  
  perm_object[[i]] # here is a hint on how you can get the correct data frame
  
  
  
}


### SW: this is how far I've looked at things properly




np <- network_permutation(gbi4.sub)#???https://cran.r-project.org/web/packages/asnipe/asnipe.pdf  and https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/2041-210X.12121
#this function already makes a 1000 permutations, by default
#Too heavy for my computer :/ 

#attempt1
t=1000
d.permutation4 <- vector(length=t)
for (i in 1:t){
  s4 <- network_permutation(gbi4.sub)
  nets4 <- graph_from_adjacency_matrix(s4,mode= c("undirected"), diag=FALSE, weighted=TRUE)
  d.permutation4[i]=degree(nets4)
}



d.permutation4
#do this for every week
#calculate repeatability of these randomized degrees
#Compare it then with the actual repeatability value

#attempt2
d.permutation <- function (gbiX.sub, new_data) {
  r.rand <- NULL
    for(i in 1:1000){
    s <- network_permutation(gbiX.sub) #this randomizes the nodes of the network
    net.rand <- graph_from_adjacency_matrix(s,mode= c("undirected"), diag=FALSE, weighted=TRUE)
    degree.rand <- degree(net.rand)
    r.rand[i] <-rptGaussian(degree.rand ~ factor.order2 + scale.age + Chick.weight  + (1|Tag), grname= "Tag", data= new_data, CI = 0.95, nboot = 1000, npermut=0)$R
    }
  
}

#for week4
r.week4 <- d.permutation(network_weekX=network_week4, gbiX.sub = gbi4.sub)
r.rand_week4 <- r.week4$r.rand
#etc
#This approach doesn't make sens because it calculates repeatability only week per week,
#  while repeatability should be calculated across all weeks.
#This is not right


#attempt3
d.permutation4 <- function (network_weekX) {
  t <- NULL
  netX <- graph_from_adjacency_matrix(network_weekX,mode= c("undirected"), diag=FALSE, weighted=TRUE)
  degree.obs <- degree(netX)
  for(i in 1:1000){
    s <- network_permutation(gbiX.sub)
    net.rand <- graph_from_adjacency_matrix(s,mode= c("undirected"), diag=FALSE, weighted=TRUE)
    deg.rand <- degree(netX.rand)
    t[i] <- deg.rand}
}

#for week4
deg.week4 <- assortment.function(network_weekX=network_week4, gbiX.sub = gbi4.sub)
deg.rand_week4 <- deg.week4$t

#for week5
#etc

#Make table to combine all these randomized degrees
#Then calculate the new repeatability with the newly randomized degrees
#Then compare the new repeatability with the observed repeatability 
#Seems a bit too elaborate, maybe this can be done more rapidly with a beter function




### attempt 4: Maybe the best but still not so sure
#Following: https://link.springer.com/article/10.1007/s00265-017-2425-y
#Compare the newly created R, obtained from the 1000 permutations for each weekly network, with the observed R
#This should show whether it is repeatable or not.


#1. permutation of each weekly network
s <- network_permutation(gbiX.sub)
#2. extract the degree
net.rand <- graph_from_adjacency_matrix(s,mode= c("undirected"), diag=FALSE, weighted=TRUE)
degree.rand <- degree(net.rand)
#3. Do this for each week
#4. Calculate repeatability of the random degrees of all these weeks combined
#5.Repeat this a thousand times
#6.Then compare it to the observed r.

d.permutation <- function (gbi4.sub, gbi5.sub ,gbi6.sub,gbi7.sub,gbi8.sub, gbi9.sub, gbi10.sub,
                           gbi11.sub, gbi12.sub, gbi13.sub, gbi14.sub, new_data) {
  r.rand <- NULL
  r.obs <-rptGaussian(degree ~ factor.order2 + scale.age + Chick.weight  + (1|Tag), grname= "Tag", data= new_data, CI = 0.95, nboot = 1000, npermut=0)$R
  
  for(i in 1:1000){
    #permutation of each weekly network
    s4 <- network_permutation(gbi4.sub) #this randomizes the nodes of the network
    #calculate the degree of the randomized weekly network
    net.rand4 <- graph_from_adjacency_matrix(s4,mode= c("undirected"), diag=FALSE, weighted=TRUE)
    degree.rand4 <- degree(net.rand4)
    s5 <- network_permutation(gbi5.sub) #this randomizes the nodes of the network
    net.rand5 <- graph_from_adjacency_matrix(s5,mode= c("undirected"), diag=FALSE, weighted=TRUE)
    degree.rand5 <- degree(net.rand5)
    s6 <- network_permutation(gbi6.sub) #this randomizes the nodes of the network
    net.rand6 <- graph_from_adjacency_matrix(s6,mode= c("undirected"), diag=FALSE, weighted=TRUE)
    degree.rand6 <- degree(net.rand6)
    s7 <- network_permutation(gbi7.sub) #this randomizes the nodes of the network
    net.rand7 <- graph_from_adjacency_matrix(s7,mode= c("undirected"), diag=FALSE, weighted=TRUE)
    degree.rand7 <- degree(net.rand7)
    s8 <- network_permutation(gbi8.sub) #this randomizes the nodes of the network
    net.rand8 <- graph_from_adjacency_matrix(s8,mode= c("undirected"), diag=FALSE, weighted=TRUE)
    degree.rand8 <- degree(net.rand8)
    s9 <- network_permutation(gbi9.sub) #this randomizes the nodes of the network
    net.rand9 <- graph_from_adjacency_matrix(s9,mode= c("undirected"), diag=FALSE, weighted=TRUE)
    degree.rand9 <- degree(net.rand9)
    s10 <- network_permutation(gbi10.sub) #this randomizes the nodes of the network
    net.rand10 <- graph_from_adjacency_matrix(s10,mode= c("undirected"), diag=FALSE, weighted=TRUE)
    degree.rand10 <- degree(net.rand10)
    s11 <- network_permutation(gbi11.sub) #this randomizes the nodes of the network
    net.rand11 <- graph_from_adjacency_matrix(s11,mode= c("undirected"), diag=FALSE, weighted=TRUE)
    degree.rand11 <- degree(net.rand11)
    s12 <- network_permutation(gbi12.sub) #this randomizes the nodes of the network
    net.rand12 <- graph_from_adjacency_matrix(s12,mode= c("undirected"), diag=FALSE, weighted=TRUE)
    degree.rand12 <- degree(net.rand12)
    s13 <- network_permutation(gbi13.sub) #this randomizes the nodes of the network
    net.rand13 <- graph_from_adjacency_matrix(s13,mode= c("undirected"), diag=FALSE, weighted=TRUE)
    degree.rand13 <- degree(net.rand13)
    s14 <- network_permutation(gbi14.sub) #this randomizes the nodes of the network
    net.rand14 <- graph_from_adjacency_matrix(s14,mode= c("undirected"), diag=FALSE, weighted=TRUE)
    degree.rand14 <- degree(net.rand14)
    
    #keep the Tags for each week 
    Tag <- V(net.rand4)$name
    degree_table4 <- data.frame(Tag = Tag, degree = degree.rand4)
    Tag <- V(net.rand5)$name
    degree_table5 <- data.frame(Tag = Tag, degree = degree.rand5)
    Tag <- V(net.rand6)$name
    degree_table6 <- data.frame(Tag = Tag, degree = degree.rand6)
    Tag <- V(net.rand7)$name
    degree_table7 <- data.frame(Tag = Tag, degree = degree.rand7)
    Tag <- V(net.rand8)$name
    degree_table8 <- data.frame(Tag = Tag, degree = degree.rand8)
    Tag <- V(net.rand9)$name
    degree_table9 <- data.frame(Tag = Tag, degree = degree.rand9)
    Tag <- V(net.rand10)$name
    degree_table10 <- data.frame(Tag = Tag, degree = degree.rand10)
    Tag <- V(net.rand11)$name
    degree_table11 <- data.frame(Tag = Tag, degree = degree.rand11)
    Tag <- V(net.rand12)$name
    degree_table12 <- data.frame(Tag = Tag, degree = degree.rand12)
    Tag <- V(net.rand13)$name
    degree_table13 <- data.frame(Tag = Tag, degree = degree.rand13)
    Tag <- V(net.rand14)$name
    degree_table14 <- data.frame(Tag = Tag, degree = degree.rand14)
    
  
    degree_list <- list(degree.table4, degree.table5, degree.table6, degree.table7, degree.table8, degree.table9,
                        degree.table0, degree.table11, degree.table12, degree.table13, degree.table14)
    
    #merge all data frames in list
    library(dplyr)
    degree.rand <- degree_list %>% reduce(full_join, by='Tag')
    
    #of the newly created degrees, calculate the repeatability
    r.rand[i] <-rptGaussian(degree.rand ~ factor.order2 + scale.age + Chick.weight  + (1|Tag), grname= "Tag", data= new_data, CI = 0.95, nboot = 1000, npermut=0)$R
  }
  p <- length(which(r.rand < r.obs))/1000
}

d.permutation$p
hist(d.permutation$r.rand)
abline(v=d.permutation$r.obs, col="red")

#Then do the same for betweenness

#Cannot run this as my computer is not powerful enough



##Other method where they rank the individuals first before doing the node-permutations
#https://link.springer.com/article/10.1007/s00265-012-1428-y
#https://academic.oup.com/beheco/article/28/2/429/2724497




