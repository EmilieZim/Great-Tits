

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

###Regression models
#fledge order = ordinal + has to be centered around zero (use scale function for this)
#control for weight

#make Fledge.order ordinal (1<2<3<4<5<6<7)
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



####Networks for each week

#add week column to gbi
#For that, has to put week column into metadata 
#The following code does not work, go forward up until the right code (row233)

head(metadata)
metadata$Date <- as.POSIXct(as.character(metadata$Start), format = "%y%m%d%H%M%S") #does not work
# SW: the "y" needed to be small, not capitalized - should work now
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
metadata$date <- substr(metadata$Start , 1, 6)

# SW: the following line should actually be correct - the origin thing is super confusing. 
# it should be set to 01. Jan 1970 if it gives you troubles
metadata$date2 <- as.Date(metadata$date, format="%y%m%d", origin= "01-01-1970")#prob because my year is YY instead of YYYY
#It actually worked once I shot down R so date2 and date3 are equivalent
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

# Rename the factor levels if necessary( once I ran everything again, this step was no longer needed)
#metadata$week <- fct_recode(metadata$week,
                          "week1" = "1",
                          "week2" = "2",
                          "week3" = "3") #etc up until week14

#NB: here the first week correspond to the first seven days of data
head(metadata)
str(metadata$week)#14 weeks

####make network with gbi: select the rows corresponding to each week. 14 weeks, hence 14 networks
###week1
which(metadata$week == 1)#from row 1 to 138


gbi1 <- gbi[1:138,1:138] #Is this the right way to make a network only for week1?
# SW: almost right!
# It will be this:
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

betweenness_table1 <- data.frame(
  Tag = Tag,
  betweenness = btw_net1)

dcc <- merge (betweenness_table1, dc, by.x= "Tag")
dcc$Fledge.order <- as.factor(dcc$Fledge.order)

dcc_no_NA <- na.omit(dcc)
ggplot(dcc_no_NA, aes(x = betweenness, y = Fledge.order)) +
  geom_point() +
  labs(title = "Scatter Plot of Betweenness Centrality and Fledge Order of week1",
       x = "Betweenness Centrality",
       y = "Fledge Order")

###week2
which(metadata$week == 2)#from row 139 to 559

gbi2 <- gbi[139:559,139:559] ##the matrix has only zeros --> weird
network_week2 <- get_network(gbi2, data_format="GBI",
                             association_index="SRI") ##I cannot extract this network. 

net2 <- graph_from_adjacency_matrix(network_week2,mode= c("undirected"), diag=FALSE, weighted=TRUE)
net2_deg <- degree(net2)
plot(net2, edge.color= "black", vertex.label.cex= 0.5) #it doesn't seem right

Tag <- V(net2)$name

# Create a data frame with individual names and degree values
centrality_table2 <- data.frame(
  Tag = Tag,
  degree = net2_deg)

print(centrality_table2)

#merge
dc2 <- merge (centrality_table2, fd, by.x= "Tag")

#plot 
dc2$Fledge.order <- as.factor(dc2$Fledge.order)
str(dc2$Fledge.order)
str(dc2$degree)
dc2_no_NA <- na.omit(dc2)

library(ggplot2)
ggplot(dc2_no_NA, aes(x = degree, y = Fledge.order)) +
  geom_point() +
  labs(title = "Scatter Plot of Degree Centrality and Fledge Order of week2",
       x = "Degree Centrality",
       y = "Fledge Order")

#plotting with betweenness centrality
btw_net2 <- betweenness(net2,v = V(net2),directed = F)

betweenness_table2 <- data.frame(
  Tag = Tag,
  betweenness = btw_net2)

dcc2 <- merge (betweenness_table2, dc, by.x= "Tag")
dcc2$Fledge.order <- as.factor(dcc2$Fledge.order)

dcc2_no_NA <- na.omit(dcc2)
ggplot(dcc2_no_NA, aes(x = betweenness, y = Fledge.order)) +
  geom_point() +
  labs(title = "Scatter Plot of Betweenness Centrality and Fledge Order of week2",
       x = "Betweenness Centrality",
       y = "Fledge Order")

####
which(metadata$week == 3)
gbi3 <- gbi[560:1388,]

#etc up until week14. But is this the right way to do it? Not sure, doesn't seem right
#Next step: see how ta make a gbi for each week


#######What if changing the net.data.summer? --> takes to much time from my computer. My computer does not have enough power. 
library(dplyr)
library(forcats)
net.data.summer <- subset(net.data.summer, net.data.summer$week>=4)
str(net.data.summer$week)
net.data.summer$week <- as.factor(net.data.summer$week)#11 levels, hence 11 weeks
net.data.summer_bis <- net.data.summer %>%
  mutate(week = fct_recode(week, "1" = "4", "2"= "5", "3" = "6", "4"= "7", "5"= "8", "6"= "9", "7"= "10", "8"= "11", "9"= "12", "10"= "13", "11"= "14"))

#only for week1
net.data.summer_bis1 <- subset(net.data.summer_bis, net.data.summer_bis$week==1)

gmm.summer2 <- gmmevents(
  time = net.data.summer_bis1$Date.Time,
  identity = net.data.summer_bis1$PIT,
  location = net.data.summer_bis1$location,
  verbose = TRUE,
  splitGroups = TRUE)
#Then I could have used the gbi extracted from this, corresponding to only week1.
#Is not feasible with my computer power. 


