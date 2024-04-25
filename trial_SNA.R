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


#autumn
# it's stored in an RData object
load("gmm.autumn.RData")
head(gmm.autumn)
View(gmm.autumn)





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


#subsets for summer
gbi <- gmm.summer$gbi
metadata <- gmm.summer$metadata
B <- gmm.summer$B

#subsets for autumn
gbi.aut <- gmm.autumn$gbi
metadata.aut <- gmm.autumn$metadata


#generate the network matrix for summer
library("asnipe")
network <- get_network(gbi, data_format = "GBI",
                       association_index = "SRI")

#generate the network for autumn
library(igraph)
network.autumn <- get_network(gbi.aut, data_format = "GBI",
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

#for autumn
net.autumn <- graph_from_adjacency_matrix(network.autumn,mode= c("undirected"), diag=FALSE, weighted=TRUE)


###plotting with degree centrality
plot(net, edge.color= "black", vertex.label.cex= 0.5)
plot(net.autumn,edge.color= "black", vertex.label.cex= 0.5 )

#I want to make a table where I can see each ind (ring) with its respective centrality degree and fledge order
#So that next I can see whether the highest degrees are the ones that fledged first

Tag <- V(net)$name
tag <- V(net.autumn)$name

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



head(metadata.aut)

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



#do it now for autumn
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


###Now for autumn

#week1
which(metadata.aut$week == 1)
gbi.aut.1 <- gbi.aut[1:104,]

threshold <- 5
gbi.aut.1.sub <- gbi.aut.1 [,colSums(gbi.aut.1 )>=threshold]
dim(gbi.aut.1.sub)
# 29 individuals seen at least 5 times in week1 of autumn 

network.aut.week1 <- get_network(gbi.aut.1.sub, data_format="GBI",
                              association_index="SRI")

net.aut.week1 <- graph_from_adjacency_matrix(network.aut.week1,mode= c("undirected"), diag=FALSE, weighted=TRUE)
net.aut.1_deg <- degree(net.aut.week1)
plot(net.aut.week1, edge.color= "black", vertex.label.cex= 0.5)

#degree
Tag <- V(net.aut.week1)$name
degree.aut_table1 <- data.frame(
  Tag = Tag,
  degree = net.aut.1_deg)

#betweenness
btw.aut.1 <- betweenness(net.aut.week1,v = V(net.aut.week1),directed = F)
betweenness.aut_table1 <- data.frame(
  Tag = Tag,
  betweenness = btw.aut.1)

table.aut.week1 <- merge(degree.aut_table1,betweenness.aut_table1, by.x= "Tag" )
table.aut.week1$Week <- 1
head(table.aut.week1)

#week2
which(metadata.aut$week == 2)
gbi.aut.2 <- gbi.aut[105:203,]

threshold <- 5
gbi.aut.2.sub <- gbi.aut.2 [,colSums(gbi.aut.2 )>=threshold]
dim(gbi.aut.2.sub)
# 24 individuals seen at least 5 times in week1 of autumn 

network.aut.week2 <- get_network(gbi.aut.2.sub, data_format="GBI",
                                 association_index="SRI")

net.aut.week2 <- graph_from_adjacency_matrix(network.aut.week2,mode= c("undirected"), diag=FALSE, weighted=TRUE)
net.aut.2_deg <- degree(net.aut.week2)
plot(net.aut.week2, edge.color= "black", vertex.label.cex= 0.5)

#degree
Tag <- V(net.aut.week2)$name
degree.aut_table2 <- data.frame(
  Tag = Tag,
  degree = net.aut.2_deg)

#betweenness
btw.aut.2 <- betweenness(net.aut.week2,v = V(net.aut.week2),directed = F)
betweenness.aut_table2 <- data.frame(
  Tag = Tag,
  betweenness = btw.aut.2)

table.aut.week2 <- merge(degree.aut_table2,betweenness.aut_table2, by.x= "Tag" )
table.aut.week2$Week <- 2
head(table.aut.week2)

#week3
which(metadata.aut$week == 3)
gbi.aut.3 <- gbi.aut[204:371,]
#length(rownames(metadata.aut)), ok I have all the data

threshold <- 5
gbi.aut.3.sub <- gbi.aut.3[,colSums(gbi.aut.3 )>=threshold]
dim(gbi.aut.3.sub)
# 24 individuals seen at least 5 times in week1 of autumn 

network.aut.week3 <- get_network(gbi.aut.3.sub, data_format="GBI",
                                 association_index="SRI")

net.aut.week3 <- graph_from_adjacency_matrix(network.aut.week3,mode= c("undirected"), diag=FALSE, weighted=TRUE)
net.aut.3_deg <- degree(net.aut.week3)
plot(net.aut.week3, edge.color= "black", vertex.label.cex= 0.5)

#degree
Tag <- V(net.aut.week3)$name
degree.aut_table3 <- data.frame(
  Tag = Tag,
  degree = net.aut.3_deg)

#betweenness
btw.aut.3 <- betweenness(net.aut.week3,v = V(net.aut.week3),directed = F)
betweenness.aut_table3 <- data.frame(
  Tag = Tag,
  betweenness = btw.aut.3)

table.aut.week3 <- merge(degree.aut_table3,betweenness.aut_table3, by.x= "Tag" )
table.aut.week3$Week <- 3
head(table.aut.week3)

##merge all the aut_table, in order to have one dataframe so I can measure Repeatability for autumn
library(dplyr)
table.aut.deg.btw <- rbind(table.aut.week1, table.aut.week2, table.aut.week3)



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
r2 <-rptGaussian(degree ~ (1|Tag), grname= "Tag", data= new_data, CI = 0.95, nboot = 1000, npermut=0)
#Repeatability for Tag
#R  = 0.287
#SE = 0.0915
#P  = 0.000163
summary(r2)
r2$R

library(lme4)
library(lmerTest)
d_within2 <- glmer(degree ~ (1|Tag) , data=new_data, family=gaussian)
summary(d_within2)

#Repeatability= (between-group variance)/(between-group variance) + (Within group variance)
#So, from the glmer output: Variance of Tag/(Variange of Tage + Variance of the residuals)
69.41/ (69.41+172.56) #0.2868537 --> exactly what gives rptGaussian function, seems a rounding of of rpt()


#betweenness
r3 <-rpt(betweenness ~ (1|Tag), grname = "Tag", data = new_data, datatype = "Gaussian", CI=0.95,
    nboot = 1000, npermut = 0)
#R  = 0.093
#SE = 0.069
#CI = [0, 0.255]
#P  = 0.0699 [LRT] --> significant
#NA [Permutation]
#Singular Fit problems

# SW: does it make sense to include all of these predictors when we found they are not good predictors in our model above? Should we not just include the random effect?
#EZ: Yes indeed, it is now corrected. Still singularity problems

r4 <- rptGaussian(betweenness ~ (1|Tag), grname= "Tag", data= new_data, CI = 0.95, nboot = 1000, npermut=0)
#singular boundary fit problem --> probably overfitting --> random part too complex which I found wierd
#Repeatability for Tag
#R  = 0.093
#SE = 0.065
#CI = [0, 0.234]
#P  = 0.0699 [LRT]
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
#EZ: sample the rownames instead of the vertices (is exactly the same but sample (V) does not work as well)
new_order <- sample(rownames(network_week4))#resample the nodes
rownames(network_week4) <- new_order#recreate the matrix
colnames(network_week4) <- new_order#recreate the matrix with the resampled nodes

net4 <- graph_from_adjacency_matrix(network_week4,mode= c("undirected"), diag=FALSE, weighted=TRUE)
s4_deg <- degree(net4) #degree of the resampled network


# SW: I see you have tried different things down below. I think the above approach is actually very promising! I have taken what you have started and put it in a function

#Take only the chicks and the ones with a fledge order, so that in the function the lengths are correct
ID.chicks <- as.vector(na.omit(subset(fd$Tag, fd$Who=="Chick" & !is.na(fd$Fledge.order))))
#maybe that it is not necessary to have the chicks with a fledge order only. But then, I don't have the same length with gbi...

permute.networks <- function(gbi, week){
  
  network_week <- get_network(gbi[,which(colnames(gbi) %in% ID.chicks)], data_format="GBI",
                               association_index="SRI"
                              )
  
  new_order <- sample(rownames(network_week))
  rownames(network_week) <- new_order
  colnames(network_week) <- new_order
  
  net <- graph_from_adjacency_matrix(network_week,mode= c("undirected"), diag=FALSE, weighted=TRUE)
  perm_deg <- degree(net)
  perm_betw <- betweenness(net)
  
  perm <- as.data.frame(as.matrix(cbind(week, perm_deg, perm_betw))) #why convert it into a matrix here?
  perm$Tag <- rownames(perm)
  rownames(perm) <- NULL#Why setting the rownames of perm here to NULL?
  return(perm)
}


# as an example, here it runs for just week 4:
perm4 <- permute.networks(gbi=gbi4.sub, week=4) #reuse the function created above but specify it for a specific week
head(perm4)
perm5 <- permute.networks(gbi=gbi5.sub, week=5) 
head(perm5)

# now we want to run this for all weeks and 1000 times - I try this in a loop here:

# this object is where we will save the output (1000 different data frames that contain data on the randomized networks)
# I am only running it on week 4 and 5, but you can add all of the others equivalently
perm_object <- NULL

for(i in 1:10){

  perm5 <- permute.networks(gbi=gbi5.sub, week=5)
  perm6 <- permute.networks(gbi=gbi6.sub, week=6)
  perm7 <- permute.networks(gbi=gbi7.sub, week=7)
  perm8 <- permute.networks(gbi=gbi8.sub, week=8)
  perm9 <- permute.networks(gbi=gbi9.sub, week=9)
  perm10 <- permute.networks(gbi=gbi10.sub, week=10)
  perm11 <- permute.networks(gbi=gbi11.sub, week=11)
  perm12 <- permute.networks(gbi=gbi12.sub, week=12)
  perm13 <- permute.networks(gbi=gbi13.sub, week=13)
  perm14 <- permute.networks(gbi=gbi14.sub, week=14)
  
  
  perm_combined <- rbind(
                         perm5,perm6, perm7, perm8, perm9, perm10, perm11, perm12, perm13, perm14) # also complete the list here
  
  # we save it in a list
  perm_object[[i]] <- perm_combined
  
}

# our perm_object contains 1000 different data frames
# data frame 1:
perm_object[[1]]
# data frame 2:
perm_object[[2]]
# etc.
str(perm_object[[1]])
#different for each Tag

boxplot(new_data$degree~new_data$Tag)

boxplot(perm_object[[1]]$perm_deg~perm_object[[1]]$Tag)

#Does this work?
R_perm_deg1 <- rptGaussian(perm_deg ~ (1|Tag), grname= "Tag", data= perm_object[[1]] , CI = 0.95, nboot = 1000, npermut=0)$R
#Singularity fit problems

# Now you should be able to run the repeatability function on each of those object slots:
# see if you can figure it out and how to store the repeatability output for each of the data frames
library(lmerTest)
library(lme4)
per_deg_mod <- glmer(perm_deg ~ (1|Tag), data= perm_object[[1]], family = gaussian)
summary(per_deg_mod)#I have no variance in Tags, which is weird
#Again, I have singular fit problems


#Maybe I don't need to do a loop here as I already have a thousand dataframes
#I need the R for each of these dataframes (each dataframe containing the 10 weeks of study but resulting from the node-based permutation method)
for(i in 1:1000){
  
  R_perm_deg <- rptGaussian(perm_deg ~ (1|Tag), grname= "Tag", data= perm_object[[i]] , CI = 0.95, nboot = 1000, npermut=0)$R
  R_perm_bet <- rptGaussian(perm_betw ~ (1|Tag), grname= "Tag", data= perm_object[[i]] , CI = 0.95, nboot = 1000, npermut=0)$R
  
}
#boundary singular fit problem
#Also too heavy for my computer to run. Takes more than 4 hours and then R collapses.

#Maybe just go directly to the repeatability
R_perm_deg <- rptGaussian(perm_deg ~ (1|Tag), grname= "Tag", data= perm_object[[i]] , CI = 0.95, nboot = 1000, npermut=0)$R
R_perm_bet <- rptGaussian(perm_betw ~ (1|Tag), grname= "Tag", data= perm_object[[i]] , CI = 0.95, nboot = 1000, npermut=0)$R
#Singular fit problems

#I have singular fit problems because the variance of the random part is close to zero, or equals to zero. 
#!!! that is because the perm_object slots are not different at all
#The function maybe has a problem somewhere. 


#Maybe use brms instead to fix the singular fit problems
library(brms)
library(ggplot2)
library(tidybayes)
library(dbplyr)

R_rand_degree <- NULL
R_rand_betweenness <- NULL

#I applied here the code comming from the paper FID analysis, https://www.sciencedirect.com/science/article/abs/pii/S000334722200001X 
#It is simplified, see below

for(i in 1:10){
  
  #first bf, short for brmsformula. Not necessary here as I want only the sigma for each Tag, not necessarily for other groups, such as per sex, age etc. 
  #brm_perm_deg1 <- bf(perm_deg ~ (1|Tag), family = 'Gaussian', data= data=perm_object[[i]])
  #brm_perm_btw1 <- bf(perm_betw ~ (1|Tag), family = 'Gaussian', data= data=perm_object[[i]])
  
  #set_prior --> don't know what that is
  
  #brms
  brm_perm_deg <-  brm(perm_deg ~ (1|Tag) , data=perm_object[[i]], family = gaussian)
  brm_perm_btw <-  brm(perm_betw ~ (1|Tag) , data=perm_object[[i]], family = gaussian)
  
  #add criterion?
  
  #get_variables(variance_deg_1)[1:30] #shows how to right the variables for the spread_draws()
  #get_variables(variance_btw_1)[1:30] #shows how to right the variables for the spread_draws()
  
  post.data.deg <- brm_perm_deg %>% spread_draws(sd_Tag__Intercept, b_Intercept)
  post.data.btw <- brm_perm_btw %>% spread_draws(sd_Tag__Intercept, b_Intercept)
  
  
  #Among-individual variance for each life_stage
  post.data.deg$Va <- post.data.deg$sd_Tag__Intercept^2
  post.data.btw$Va <- post.data.btw$sd_Tag__Intercept^2
  
  
  #Within-individual variance for each life stage
  #post.data.deg$Vw<- exp(post.data.deg$b_Intercept)^2
  #why not?
  post.data.deg$Vw<- post.data.deg$b_Intercept^2
  post.data.btw$Vw<- post.data.btw$b_Intercept^2
  
  #Repeatability
  head(post.data.deg)
  head(post.data.btw)
  post.data.deg1 <- post.data.deg %>%
    dplyr::mutate(R_rand_deg = Va/(Va + Vw))
  
  post.data.btw1 <- post.data.btw %>%
    dplyr::mutate(R_rand_btw = Va/(Va + Vw))
  
  R_rand_degree <- post.data.deg1$R_rand_deg
  R_rand_betweenness <- post.data.btw1$R_rand_btw
}

hist(R_rand_degree)
hist(R_rand_betweenness)

##Observed Repeatability, simplified
#Only from week5
#Only ind that have been seen at least in 3 different weeks (so we have individuals that have at least 3 "trials")

new_data0 <- new_data %>%
  subset(Week >= 5)

filtered_nd <- new_data0 %>%
  group_by(Tag) %>%
  filter(n() >= 3)

View(filtered_nd)
length(unique(filtered_nd$Tag))#only 24 chicks

brm_obs_deg <-  brm(degree ~ (1|Tag) , data=filtered_nd, family = gaussian)
brm_obs_btw <-  brm(betweenness ~ (1|Tag) , data=filtered_nd, family = gaussian)

get_variables(brm_obs_deg)[1:30] 
get_variables(brm_obs_btw)[1:30] 


post.data.deg.obs <- brm_obs_deg %>% spread_draws(sd_Tag__Intercept, b_Intercept)
post.data.btw.obs <- brm_obs_btw %>% spread_draws(sd_Tag__Intercept, b_Intercept)
summary(brm_obs_deg)

#Among-individual variance for each life_stage
post.data.deg.obs$Va <- post.data.deg.obs$sd_Tag__Intercept^2
post.data.btw.obs$Va <- post.data.btw.obs$sd_Tag__Intercept^2

#Within-individual variance 
#post.data.deg.obs$Vw<- exp(post.data.deg.obs$b_Intercept)^2
#why not? :
post.data.deg.obs$Vw<- post.data.deg.obs$b_Intercept^2
post.data.btw.obs$Vw<- post.data.btw.obs$b_Intercept^2

#Repeatability
head(post.data.deg.obs)
head(post.data.btw.obs)
post.data.deg.obs1 <- post.data.deg.obs %>%
  dplyr::mutate(R_obs_deg = Va/(Va + Vw))



post.data.btw.obs1 <- post.data.btw.obs %>%
  dplyr::mutate(R_obs_btw = Va/(Va + Vw))

hist(post.data.deg.obs1$R_obs_deg)
R_deg <- mean(post.data.deg.obs1$R_obs_deg)#0.04681559 --> this is the observed R for degree
hist(post.data.btw.obs1$R_obs_btw)
R_btw <-mean(post.data.btw.obs1$R_obs_btw)#0.2190036 --> This is the observed R for betweenness

##Simplified code:
#Randomized R

R_rand_degree <- NULL
R_rand_betweenness <- NULL

for(i in 1:10){
  
  brm_perm_deg <-  brm(perm_deg ~ (1|Tag) , data=perm_object[[i]], family = gaussian)
  brm_perm_btw <-  brm(perm_betw ~ (1|Tag) , data=perm_object[[i]], family = gaussian)
  
  post.data.deg.rand <- brm_perm_deg %>% spread_draws(sd_Tag__Intercept, b_Intercept)
  post.data.btw.rand <- brm_perm_btw %>% spread_draws(sd_Tag__Intercept, b_Intercept)
  
  Rdeg_rand <- (post.data.deg.rand$sd_Tag__Intercept^2)/((post.data.deg.obs$sd_Tag__Intercept^2) + (post.data.deg.obs$b_Intercept^2))
  R_rand_degree <- mean(Rdeg_rand)
  Rbtw_rand <- (post.data.btw.rand$sd_Tag__Intercept^2)/((post.data.btw.obs$sd_Tag__Intercept^2) + (post.data.btw.obs$b_Intercept^2))
  R_rand_betweenness <- mean(Rbtw_rand) }

#weird, this is probably not the right approach, see below
p_dg <- length(which(R_rand_degree < R_deg))/10
p_btw <- length(which(R_rand_betweenness < R_btw))/10

#According to: https://www.researchgate.net/publication/344290324_Repeatable_social_network_node-based_metrics_across_populations_and_contexts_in_a_passerine
#R_obs is considered significant when their IC doesn't fall in the mean of R_rand

R_rand_degree # value: 0.0003952475
R_rand_betweenness # value: 0.001956394

#take IC of the R_obs_deg and R_obs_btw
#For this I need package rethinking (version 2.13)
library("rstan")
install.packages(c('devtools','coda','mvtnorm'))
library(devtools)
install_github("rmcelreath/rethinking")
install.packages(c('coda','mvtnorm'))
options(repos=c(getOption('repos'),rethinking='http://xcelab.net/R'))
install.packages('rethinking',type='source')
library(rethinking)


rethinking::HPDI(post.data.deg.obs1$R_obs_deg, prob = 0.95)
# |0.95     0.95| 
# 0.0002732524 0.0926220306  
rethinking::HPDI(post.data.btw.obs1$R_obs_btw, prob = 0.95)
# |0.95        0.95| 
#7.285400e-09 5.262557e-01  

#R_rand of degree doesn't fall into the IC of the observed R, while that of betweenness falls within the R observed IC
#Hence, only the observed R of degree is significant
#However, 0.08 is a very small number. 
#Still weird because the observed R are really different from the R obtained by rptR
#If this is right, we could use the next season of the data, do the same analysis and so see if the repeatability would have changed 
#  --> a way to search for ontogeny of the social network position


#Trying to solve the singularity problem
#Maybe because the individuals don't have the same number of "trials", hence the individuals were not seen an equal number of times
#Maybe there are not enough individuals present in the first weeks as well
#lets start at week6
which(new_data0$Week==5)#I checked this for each week
new_data01 <- new_data %>%
  subset(Week >= 6)

r2 <-rptGaussian(degree ~ (1|Tag), grname= "Tag", data= new_data01, CI = 0.95, nboot = 1000, npermut=0)
r4 <-rptGaussian(betweenness ~ (1|Tag), grname= "Tag", data= new_data01, CI = 0.95, nboot = 1000, npermut=0)
#Actually, betweenness doesn't have a normal distribution -- >I take the log
hist(log(new_data01$betweenness+1))# +1 to solve the log0 prob
new_data01$logb <- log(new_data01$betweenness+1)
r4 <-rptGaussian(logb~ (1|Tag), grname= "Tag", data= new_data01, CI = 0.95, nboot = 1000, npermut=0)
#Still issues
#Maybe it is because there is not a reasonable number of trials per individual to obtain robust estimates of repeatability.
#Insufficient data can lead to unreliable estimates, maybe also singularity problems?

#Lets say that the individuals must have been seen at least in three different weeks (~3 trials)
library(dplyr)
filtered_data <- new_data0 %>%
  group_by(Tag) %>%
  filter(n() >= 3) 
length(unique(filtered_data$Tag))#only 24 chicks --> maybe this is not enough data ?
length(unique(new_data0$Tag))#38 chicks
rd <-rptGaussian(degree ~ (1|Tag), grname= "Tag", data= filtered_data, CI = 0.95, nboot = 1000, npermut=0)
#what if at least seen 5 times?
filtered_data2 <- new_data0 %>%
  group_by(Tag) %>%
  filter(n() >= 5) 
length(unique(filtered_data2$Tag))#16 chicks only 
str(filtered_data2$Family)
rd <-rptGaussian(degree ~ (1|Tag), grname= "Tag", data= filtered_data2, CI = 0.95, nboot = 100, npermut=0)
hist(filtered_data2$degree)
View(filtered_data2)

#There is still an issue, don't know why

#######################  
#Now for the autumn network data
#######################

#dataframe with betweenness and degree values for each week, for each individual: table.aut.deg.btw
#Is it really necessary to use the chicks with a fledge order? 
ID.chicks.aut <- as.vector(na.omit(subset(fd$Tag, fd$Who=="Chick")))
length(colnames(gbi))


permute.networks.aut <- function(gbi, week){
  
  network_week <- get_network(gbi[,which(colnames(gbi) %in% ID.chicks.aut)], data_format="GBI",
                              association_index="SRI"
  )
  
  new_order <- sample(rownames(network_week))
  rownames(network_week) <- new_order
  colnames(network_week) <- new_order
  
  net <- graph_from_adjacency_matrix(network_week,mode= c("undirected"), diag=FALSE, weighted=TRUE)
  perm_deg <- degree(net)
  perm_betw <- betweenness(net)
  
  perm.aut <- as.data.frame(as.matrix(cbind(week, perm_deg, perm_betw))) 
  perm.aut$Tag <- rownames(perm.aut)
  rownames(perm.aut) <- NULL
  return(perm.aut)
}

perm_object.aut <- NULL

for(i in 1:10){
  
  perm.aut1 <- permute.networks.aut(gbi=gbi.aut.1.sub, week=1)
  perm.aut2 <- permute.networks.aut(gbi=gbi.aut.2.sub, week=2)
  perm.aut3 <- permute.networks.aut(gbi=gbi.aut.3.sub, week=3)
  
  
  perm_combined.aut <- rbind(
    perm.aut1, perm.aut2, perm.aut3) # also complete the list here
  
  # we save it in a list
  perm_object.aut[[i]] <- perm_combined.aut
  
}

##Observed Repeatability, simplified
#Only ind that have been seen at least in 3 different weeks (so we have individuals that have at least 3 "trials")
data_autumn <- merge(table.aut.deg.btw, fd, by.x="Tag")
head(data_autumn)

subset_autumn <- na.omit(data_autumn)#34 chicks with fledge order
#actually, not necessary that these individuals have a fledge ordern they just need to be a chick
chicks_autumn <- subset(data_autumn, Who=="Chick")
head(chicks_autumn)

subset_chicks_autumn <- chicks_autumn %>%
  group_by(Tag) %>%
  filter(n() == 3)
#no individual has been seen 3 times, once each week

subset_chicks_autumn <- chicks_autumn %>%
  group_by(Tag) %>%
  filter(n() == 2)

length(colnames(subset_chicks_autumn))#18, hence 9 individuals only --> not enough to test repeatability I suppose

head(subset_chicks_autumn)


library(brms)
library(ggplot2)
library(tidybayes)
library(dbplyr)
brm_obs_deg.aut <-  brm(degree ~ (1|Tag) , data=subset_chicks_autumn, family = gaussian)
brm_obs_btw.aut <-  brm(betweenness ~ (1|Tag) , data=subset_chicks_autumn, family = gaussian)

get_variables(brm_obs_deg.aut)[1:30] 
get_variables(brm_obs_btw.aut)[1:30] 


post.data.deg.obs.aut <- brm_obs_deg.aut %>% spread_draws(sd_Tag__Intercept, b_Intercept)
post.data.btw.obs.aut <- brm_obs_btw.aut %>% spread_draws(sd_Tag__Intercept, b_Intercept)

#Among-individual varianc
post.data.deg.obs.aut$Va <- post.data.deg.obs.aut$sd_Tag__Intercept^2
post.data.btw.obs.aut$Va <- post.data.btw.obs.aut$sd_Tag__Intercept^2

#Within-individual variance 
post.data.deg.obs.aut$Vw<- post.data.deg.obs.aut$b_Intercept^2
post.data.btw.obs.aut$Vw<- post.data.btw.obs.aut$b_Intercept^2

#Repeatability
head(post.data.deg.obs.aut)
head(post.data.btw.obs.aut)
post.data.deg.obs1.aut <- post.data.deg.obs.aut %>%
  dplyr::mutate(R_obs_deg.aut = Va/(Va + Vw))

post.data.btw.obs1.aut <- post.data.btw.obs.aut %>%
  dplyr::mutate(R_obs_btw.aut = Va/(Va + Vw))

hist(post.data.deg.obs1.aut$R_obs_deg.aut)
R_deg.aut <- mean(post.data.deg.obs1.aut$R_obs_deg.aut)#0.01863995 --> this is the observed R for degree
hist(post.data.btw.obs1.aut$R_obs_btw.aut)
R_btw.aut <-mean(post.data.btw.obs1.aut$R_obs_btw.aut)#0.4571816 --> This is the observed R for betweenness which is high

##Simplified code:
#Randomized R

R_rand_degree.aut <- NULL
R_rand_betweenness.aut <- NULL

for(i in 1:10){
  
  brm_perm_deg.aut <-  brm(perm_deg ~ (1|Tag) , data=perm_object.aut[[i]], family = gaussian)
  brm_perm_btw.aut <-  brm(perm_betw ~ (1|Tag) , data=perm_object.aut[[i]], family = gaussian)
  
  post.data.deg.rand.aut <- brm_perm_deg %>% spread_draws(sd_Tag__Intercept, b_Intercept)
  post.data.btw.rand.aut <- brm_perm_btw %>% spread_draws(sd_Tag__Intercept, b_Intercept)
  
  Rdeg_rand.aut <- (post.data.deg.rand.aut$sd_Tag__Intercept^2)/((post.data.deg.obs.aut$sd_Tag__Intercept^2) + (post.data.deg.obs.aut$b_Intercept^2))
  R_rand_degree.aut <- mean(Rdeg_rand.aut)
  Rbtw_rand.aut <- (post.data.btw.rand.aut$sd_Tag__Intercept^2)/((post.data.btw.obs.aut$sd_Tag__Intercept^2) + (post.data.btw.obs.aut$b_Intercept^2))
  R_rand_betweenness.aut <- mean(Rbtw_rand.aut) }


#According to: https://www.researchgate.net/publication/344290324_Repeatable_social_network_node-based_metrics_across_populations_and_contexts_in_a_passerine
#R_obs is considered significant when their IC doesn't fall in the mean of R_rand

R_rand_degree.aut # value: 0.001738892
R_rand_betweenness.aut # value: 2.294327

#take IC of the R_obs_deg and R_obs_btw.aut
#For this I need package rethinking (version 2.13)
library("rstan")
install.packages(c('devtools','coda','mvtnorm'))
library(devtools)
install_github("rmcelreath/rethinking")
install.packages(c('coda','mvtnorm'))
options(repos=c(getOption('repos'),rethinking='http://xcelab.net/R'))
install.packages('rethinking',type='source')
library(rethinking)


rethinking::HPDI(post.data.deg.obs1.aut$R_obs_deg.aut, prob = 0.95)
# |0.95     0.95| 
# 6.842014e-08 7.268689e-02   --> R_rand_degree.aut # value: 0.001738892 falls within the IC
rethinking::HPDI(post.data.btw.obs1.aut$R_obs_btw.aut, prob = 0.95)
# |0.95        0.95| 
#2.820320e-09 9.929506e-01 --> R_rand_betweenness.aut # value: 2.294327 falls without the IC

#Repeatability of betweenness is high. Higher now than in the summer and now repeatable
#What is weird is that now the degree is no longer statistically repeatable. Whereas it is in the summer
#I think these results cannot be trusted as the individuals are only seen twice in the dataset 
#  And those seen twice in the dataset are in poor number.

#################
#Across seasons (including the summer and autumn data)
#################
#make the dataset including both summer and autumn data 
#combine these:
metadata.aut
metadata

#prob: not the same gbi --> I can oly use one gbi for the network.
#Can I merge gmm.autumn with gmm.summer?
gmm.seasons <- merge(gmm.autumn, gmm.summer)
gmm.seasons$gbi
#I don't think I can merge these two... So maybe go back to the rptR method but than it won't be comparable with the analysis above (R within each season)

#merge the dataframes of summer and autumn with the degree and betweenness values
library(dplyr)

data_summer <-  new_data %>%
  select(Tag, degree, betweenness)

hist(data_summer$degree)#2 pics
hist(data_summer$betweenness)# --> not normal
data_summer$modif.btw <- log(data_summer$betweenness+1)
hist(data_summer$modif.btw) #better

hist(log10(data_summer$betweenness)+1)
data_summer$modif.btw2 <-  log10(data_summer$betweenness+1)
data_summer <-  data_summer %>%
  select(Tag, degree, modif.btw2)

library(rptR)
#Observed R
rpt(degree ~ (1|Tag), grname= "Tag", data= data_summer, datatype = "Gaussian", CI = 0.95, nboot = 1000, npermut=0 )
#no singularity problems
rpt(modif.btw2 ~ (1|Tag), grname= "Tag", data= data_summer, datatype = "Gaussian", CI = 0.95, nboot = 1000, npermut=0 )
#singularity issues --> to me it are the values of betweenness that are the issue

#Still the same issues









##########sources
#https://cran.r-project.org/web/packages/asnipe/asnipe.pdf  
#https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/2041-210X.12121
# https://link.springer.com/article/10.1007/s00265-017-2425-y

#https://link.springer.com/article/10.1007/s00265-012-1428-y
#https://academic.oup.com/beheco/article/28/2/429/2724497

#https://discourse.mc-stan.org/t/brms-modeling-slope-effects-to-measure-individual-consistency/15798/3



# 2) Visitation order -----------------------------------------------------

load("data/gmm.summer.RData")
net.data.summer <- read.delim("data/Mill.data.summer.txt", sep=" ", row.names = 1)

gmm.summer$metadata$Location2 <- substr(gmm.summer$metadata$Location, 8, 13)

# for each group, we have the start and end time and a location

library(data.table)

# we add a new column called 'group'
net.data.summer[, "group"] <- NA

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
# 2397 observations are not part of a group. Perhaps these were birds that arrived before I officially started the observations? Or then they were dropped for some reason by the gmm function? I might need to rerun the gmm function to see what's going on there. 

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

# do the same for autumn

load("data/gmm.autumn.RData")
net.data.autumn <- read.delim("data/Mill.data.autumn.txt", sep=",", row.names = 1)

# for each group, we have the start and end time and a location

library(data.table)

# we add a new column called 'group'
net.data.autumn[, "group"] <- NA

for(i in 1:length(gmm.autumn$metadata$Start)){ # for each start time
  i.loc <- gmm.autumn$metadata$Location[i] # we extract the location
  rows.i <-  rownames(subset(net.data.autumn, 
                             net.data.autumn$Date.Time>=gmm.autumn$metadata$Start[i] &
                               net.data.autumn$Date.Time<=gmm.autumn$metadata$End[i] &
                               net.data.autumn$location == gmm.autumn$metadata$Location[i]))
  net.data.autumn[rows.i, "group"] <- i
  
}


net.data.autumn[which(is.na(net.data.autumn$group)),]

length(which(is.na(net.data.autumn$group)))
# all observations are part of a group

length(unique(net.data.autumn$group))
# 371 groups

# make sure it's in correct order (first ordered by groups, then time)
net.data.autumn <- net.data.autumn[with(net.data.autumn, order(group, Date.Time)), ]

# we create a new column called 'order' all filled with NA
net.data.autumn$order <- NA
# to the very first visitor, we assign a 1
net.data.autumn$order[1] <- 1
# now we loop through the entire data frame
for(i in 2:length(net.data.autumn$group)){
  # we extract the PIT tag and the group number of bird i and the bird and group number that came just before
  group.i <- net.data.autumn$group[i]
  ID.i <- net.data.autumn$PIT[i]
  group.i_minus1 <- net.data.autumn$group[i-1]
  ID.i_minus1 <- net.data.autumn$PIT[i-1]
  
  # we also don't want to assign a new order number, if the bird had already arrived as part of the current group, so we extract the PIT tags that are already part of the current group
  sub <- net.data.autumn[1:(i-1),]
  sub <- subset(sub, sub$group==group.i)
  already.present <- unique(sub$PIT)
  
  if(ID.i %in% already.present){ # if the bird hard already arrived as part of the current group
    net.data.autumn$order[i] <- 'already.present' # we assign 'already.present'
  } else if(group.i==group.i_minus1 & ID.i!=ID.i_minus1){
    # if it's the same group but a new bird, we add the order+1
    net.data.autumn$order[i] <- max(as.numeric(na.omit(sub$order[sub$order != "already.present"])))+1
  } else if(group.i!=group.i_minus1){
    # if it's a new group, we start over with the order=1
    net.data.autumn$order[i] <- 1
  }
}


# we remove the rows that contain 'already present' to only have each bird in each group once
net.data.autumn <- subset(net.data.autumn, !(is.na(net.data.autumn$order)) & net.data.autumn$order != "already.present")

head(net.data.autumn, 40)
net.data.autumn$order <- as.numeric(net.data.autumn$order)
max(net.data.autumn$order)
hist(net.data.autumn$order)

# save it as the object with the order
save(net.data.autumn, file="data/net.data.autumn.w.order.RData")
load("data/net.data.autumn.w.order.RData")

## and for winter
load("data/gmm.winter.RData")
net.data.winter <- read.delim("data/Mill.data.winter.txt", sep=",", row.names = 1)

# for each group, we have the start and end time and a location

library(data.table)

# we add a new column called 'group'
net.data.winter[, "group"] <- NA

for(i in 1:length(gmm.winter$metadata$Start)){ # for each start time
  i.loc <- gmm.winter$metadata$Location[i] # we extract the location
  rows.i <-  rownames(subset(net.data.winter, 
                             net.data.winter$Date.Time>=gmm.winter$metadata$Start[i] &
                               net.data.winter$Date.Time<=gmm.winter$metadata$End[i] &
                               net.data.winter$location == gmm.winter$metadata$Location[i]))
  net.data.winter[rows.i, "group"] <- i
  
}


net.data.winter[which(is.na(net.data.winter$group)),]

length(which(is.na(net.data.winter$group)))
# all observations are part of a group

length(unique(net.data.winter$group))
# 421 groups

# make sure it's in correct order (first ordered by groups, then time)
net.data.winter <- net.data.winter[with(net.data.winter, order(group, Date.Time)), ]

# we create a new column called 'order' all filled with NA
net.data.winter$order <- NA
# to the very first visitor, we assign a 1
net.data.winter$order[1] <- 1
# now we loop through the entire data frame
for(i in 2:length(net.data.winter$group)){
  # we extract the PIT tag and the group number of bird i and the bird and group number that came just before
  group.i <- net.data.winter$group[i]
  ID.i <- net.data.winter$PIT[i]
  group.i_minus1 <- net.data.winter$group[i-1]
  ID.i_minus1 <- net.data.winter$PIT[i-1]
  
  # we also don't want to assign a new order number, if the bird had already arrived as part of the current group, so we extract the PIT tags that are already part of the current group
  sub <- net.data.winter[1:(i-1),]
  sub <- subset(sub, sub$group==group.i)
  already.present <- unique(sub$PIT)
  
  if(ID.i %in% already.present){ # if the bird hard already arrived as part of the current group
    net.data.winter$order[i] <- 'already.present' # we assign 'already.present'
  } else if(group.i==group.i_minus1 & ID.i!=ID.i_minus1){
    # if it's the same group but a new bird, we add the order+1
    net.data.winter$order[i] <- max(as.numeric(na.omit(sub$order[sub$order != "already.present"])))+1
  } else if(group.i!=group.i_minus1){
    # if it's a new group, we start over with the order=1
    net.data.winter$order[i] <- 1
  }
}


# we remove the rows that contain 'already present' to only have each bird in each group once
net.data.winter <- subset(net.data.winter, !(is.na(net.data.winter$order)) & net.data.winter$order != "already.present")

head(net.data.winter, 40)
net.data.winter$order <- as.numeric(net.data.winter$order)
max(net.data.winter$order)
hist(net.data.winter$order)

# save it as the object with the order
save(net.data.winter, file="data/net.data.winter.w.order.RData")
load("data/net.data.winter.w.order.RData")

# And the new species_age_sex table with all
# important: some are designated as 'adults' in 2020 even though they weren't born yet (but it doesn't affect the data, since they would not have been seen on the network feeders)

load("data/species_age_sex.RDA")

head(species_age_sex)

# SW: up until here I've made some changes.


View(net.data.summer)
str(net.data.summer$order)
net.data.summer$order <- as.numeric(net.data.summer$order)
net.data.summer$group <- as.numeric(net.data.summer$group)
##order is nested in group





##The research questions
#1. Are individuals consistent in the order within which they arrive at the feeder? 
#2. And could the order of arrival be predicted by age class (first year/adult), 
#3. and for juveniles, could this be predicted by the fledge order?
 
View(new_data)#only the chicks with a fledge order
View(net.data.summer)
net.data.summer$Tag <- net.data.summer$PIT

View(fd)
full.data.summer <- merge(fd, net.data.summer, by.x="Tag")
View(full.data.summer)

##1. Repeatability of order and model (for chicks)
library(rptR)
library(lmerTest)
library(lme4)
hist(net.data.summer$order) #looks like Poisson

chicks.data.summer <- merge(new_data, net.data.summer, by.x="Tag")
View(chicks.data.summer)
#3. and for juveniles, could the order be predicted by the fledge order?

###I believe it should be a Poisson model as order can be considered as a count
glm.order.Chick <- glmer(order ~ fledge.order.scaled.within*scale(Chick.weight) + fledge.order.scaled.within*scale(age) +scale(Chick.weight)*scale(age) + (1|Tag), family=poisson, data=chicks.data.summer)
summary(glm.order.Chick)
glm.order.Chick <- glmer(order ~fledge.order.scaled.within + scale(Chick.weight) +scale(age) + (1|Tag), family=poisson, data=chicks.data.summer)
summary(glm.order.Chick)#no explanatory variable has a significant effect on order
#check for multicollinearity with a matrix
cor_matrix <- cor(chicks.data.summer[, c("fledge.order.scaled.within", "Chick.weight", "age")])
print(cor_matrix)
corrplot::corrplot(cor_matrix, method = "circle")
#the weight and the fledge order seem correlated
glm.order.Chick <- glmer(order ~fledge.order.scaled.within + (1|Tag), family=poisson, data=chicks.data.summer)
summary(glm.order.Chick)
#no effect of fledge order on the order of arrival at the feeders
rpt(order ~ (1|Tag), grname ="Tag", datatype="Poisson", data=chicks.data.summer, CI = 0.95, nboot = 100, npermut = 0)
#link scale
#R  = 0.042 --> low repeatability
#SE = 0.011
#CI = [0.015, 0.057]
#P  = 3.23e-56 [LRT]
#NA [Permutation]
#to heavy for my computer apparently so I could only do the nboot=100, if nboot=1000 should be better



#Calculate score? --> What if I transform the order variable into proportions so I can calculate a score for each individual 
library(dplyr)
library(tidyverse)

#calculate the total of groups as a new column "Tot"
full.data.summer.2 <- full.data.summer %>%
  group_by(group) %>%
  summarise(Tot = max(order))

View(full.data.summer.2)

data.summer <- full.data.summer %>%
  left_join(full.data.summer.2, by = "group")

#calculate now the proportions for each individual, as a score for order
data.summer$prop<- data.summer$order/data.summer$Tot
View(data.summer)

hist(data.summer$prop)
#NB: sometimes not good to transform the initial data to much, not sure this is the right thing to do

#Model
#Because now it are proportions I can use Binomial distribution

#Only Repeatability of Chicks and fledge order 
data.summer.chicks <- subset(data.summer, Who=="Chick")
View(data.summer.chicks)
data.summer.chicks <- na.omit(data.summer.chicks)
data.summer.chicks$prop

rpt(prop ~ fledge.order.scaled.within + (1|Tag),  data=data.summer.chicks,  grname="Tag", 
    nboot=1000, npermut=0, datatype = "Proportion")
#singular fit problems
rpt(prop ~ (1|Tag),  data=data.summer.chicks,  grname="Tag", 
    nboot=1000, npermut=0, datatype = "Proportion")
#singular fit problems

library(brms)
glmchicks <- brms(prop ~ fledge.order.scaled.within + (1|Tag), family=binomial, data=data.summer.chicks, weights=Tot)
summary(glmchicks)#can't check for overdispersion, don't know why
drop1(glmchicks, test="Chisq")

m <-  brm(prop ~ fledge.order.scaled.within+ (1|Tag) , data=data.summer.chicks )
summary(m)
pp_check(m)#no effect
#look like pretty poor model
plot(m)

library(tidybayes)
post.m <- m %>% spread_draws(sd_Tag__Intercept, b_Intercept)
Rdeg_rand.aut <- (post.data.deg.rand.aut$sd_Tag__Intercept^2)/((post.data.deg.obs.aut$sd_Tag__Intercept^2) + (post.data.deg.obs.aut$b_Intercept^2))


R.order.chicks <- (post.m$sd_Tag__Intercept^2)/((post.m$sd_Tag__Intercept^2) + (post.m$b_Intercept^2))
mean(R.order.chicks)
#0.004114971 --> very small repeatability


#2. And could the order of arrival be predicted by age class (first year/adult)? 
#find the info on the age class in the paper
head(full.data.summer)
str(full.data.summer$Who)
full.data.summer$Who <- as.factor(full.data.summer$Who)
glm.Who <- glmer(order ~ Who + (1|Tag), family=poisson, data= full.data.summer)
summary(glm.Who)#there is an effect of class on the order of arrival to the feeders
report(glm.Who)
Anova(glm.Who)
m.class <-  brm(order ~ Who + (1|Tag) , family=poisson, data= full.data.summer)
summary(m.class)
pp_check(m.class)
library(tidybayes)
post.m.class <- m.class %>% spread_draws(sd_Tag__Intercept, b_Intercept)
R_order <- (post.m.class$sd_Tag__Intercept^2)/((post.m.class$sd_Tag__Intercept^2) + (post.m.class$b_Intercept^2))
mean(R_order)#0.06051628
#--> very low repeatability, also corresponds to the Variance of Tag in the glm.Who model 
rpt(order ~ Who + (1|Tag),  data=full.data.summer,  grname="Tag", nboot=1000, npermut=0, datatype = "Poisson")

##graphical illustration
library(ggplot2)
ggplot(full.data.summer) + aes(x = Who, y = order) +
  geom_boxplot(varwidth = TRUE, outlier.alpha = 0) 

###What if we gather males and females together, as one group called adults
full.data.summer <- full.data.summer %>%
  mutate(Class = ifelse(Who %in% c("Male", "Female"), "Adult", "Chick"))

full.data.summer$Class <- as.factor(full.data.summer$Class) 
glm.Class <- glmer(order ~ Class + (1|Tag), family=poisson, data= full.data.summer)
summary(glm.Class)

#graph
library(ggplot2)
ggplot(full.data.summer) + aes(x = Class, y = order) +
  geom_boxplot(varwidth = TRUE, outlier.alpha = 0)

######order of arrival to feeders with more precise age classes and species
#data
net.data.autumn <- read.delim("Mill.data.autumn.txt", sep=",", row.names = 1)
View(net.data.autumn)
net.data.summer <- read.delim("Mill.data.summer.txt", sep=",", row.names = 1)
net.data.winter <- read.delim("Mill.data.winter.txt", sep=",", row.names = 1)
#this works


#species_age_sex
load("species_age_sex.RDA") #load doesn't work
View(species_age_sex)
save(species_age_sex.RDA, file="species_age_sex.RDA") #does nothing
load("data/species_age_sex.RDA")#what if I change working directory? --> doesn't change anything
#finally works when downloading it from GitHub:
head(species_age_sex)

#order data
load("gmm.autumn.RData")
head(gmm.autumn) #works
load("gmm.summer.RData")
head(gmm.summer) #works
load("gmm.winter.RData")
head(gmm.winter) #works
load("net.data.summer.w.order.RData")
head(net.data.summer.w.order) #doesn't work
load("net.data.autumn.w.order.RData")
head(net.data.autumn.w.order) #doesn't work
load("net.data.winter.w.order.RData")
head(net.data.winter.w.order) #doesn't work


#load doesn't work for the net.data."season".w.order
#Tried to download them straight from Github --> doesn't work either
#Tried to click on them to open them from the files window in rstudio --> doesn't work either
#Also tried restarting R but doesn't change anything







