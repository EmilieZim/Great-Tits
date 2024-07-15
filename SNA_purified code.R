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


######################2 scenario's for the story telling of the data analysis

###Scenario 1
##
#Aim: We want to see what factors would determine the social network position of great tits in early life stages. 
###For this, we looked at the fledge order and the order of arrival towards the feeders as explanatory factors
##(Or maybe better to say the order of arrival at the feeders whether this is correlated to the social network position)
###In regard to the order of arrival on the feeders, we looked at whether this could be explained by sex, age class (adults/juveniles) and species
###And whether it could be repeatable through time or not


###Scenario 2
##
#Aim: We are interested in the interactions between different species and the interactions between individuals of different age classes and sex within a species 
#(How dominance can play out in a feeding context maybe?)
##For this, we looked at the order of arrival of different bird species on a feeder. Also, more specifically, we looked at Great tits and whether the order could be determined by age class (juveniles/adults) and sex within that species



######################Code for the two scenario's
###Scenario 1#########
######################

##
#Aim: We want to see what factors would determine the social network position of great tits in early life stages. 
###For this, we looked at the fledge order and the order of arrival towards the feeders as explanatory factors
##(Or maybe better to say the order of arrival at the feeders whether this is correlated to the social network position)
###In regard to the order of arrival on the feeders, we looked at whether this could be explained by sex, age class (adults/juveniles) and species
###And whether it could be repeatable through time or not


#fledgling_data
fd<- read.table("fledgling_data.txt",header = TRUE, fill = T, sep = "\t")
View(fd)
str(fd)

#species_age
sp_a<- read.table("species_age.txt",header = TRUE, fill = T, sep = "\t")
View(sp_a)
str(fd)

#gmm for the summer (Great Tits)
load("gmm.summer.RData")
head(gmm.summer)
View(gmm.summer)


####NETWORKS FOR EACH WEEK

#1.
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


#2. (First look at fledge order data)
#Now make the networks for each week and extract the degree and betweenness centrality
##THE WEEKS
####make network with gbi: select the rows corresponding to each week. 14 weeks, hence 14 networks
##Merge the data with the fledge order data

###week1
which(metadata$week == 1)#from row 1 to 138
gbi1 <- gbi[1:138,]
threshold <- 5 #treshold to individuals that have been seen at least 5 times
gbi1.sub <- gbi1[,colSums(gbi1)>=threshold]
dim(gbi1.sub)
# that gives you 138 rows and 12 columns (=individuals)
#So 12 individuals that have been seen at least 5 times in the first week. 

library(asnipe)
network_week1 <- get_network(gbi1.sub, data_format="GBI",
                             association_index="SRI")


net1 <- graph_from_adjacency_matrix(network_week1,mode= c("undirected"), diag=FALSE, weighted=TRUE)
net1_deg <- degree(net1)

Tag <- V(net1)$name

# Create a data frame with individual names and degree values
centrality_table1 <- data.frame(
  Tag = Tag,
  degree = net1_deg)

#merge it with thefledging order data
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

Tag <- V(net6)$name
centrality_table6 <- data.frame(
  Tag = Tag,
  degree = net6_deg)

#merge
dc6 <- merge(centrality_table6, fd, by.x= "Tag")

dc6_chicks <- subset(dc6, dc6$Who=="Chick")# 71 chicks --> much more than the previous week

# betweenness centrality merge
btw_6 <- betweenness(net6,v = V(net6),directed = F)
betweenness_table6 <- data.frame(
  Tag = Tag,
  betweenness = btw_6)

table_week6 <- merge(centrality_table6,betweenness_table6, by.x= "Tag" )
table_week6$Week <- 6 

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

Tag <- V(net7)$name
centrality_table7 <- data.frame(
  Tag = Tag,
  degree = net7_deg)

#merge
dc7 <- merge(centrality_table7, fd, by.x= "Tag")

dc7_chicks <- subset(dc7, dc7$Who=="Chick")# 29 chicks --> much less than the previous week

# betweenness centrality
btw_7 <- betweenness(net7,v = V(net7),directed = F)
betweenness_table7 <- data.frame(
  Tag = Tag,
  betweenness = btw_7)

table_week7 <- merge(centrality_table7,betweenness_table7, by.x= "Tag" )
table_week7$Week <- 7 

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

Tag <- V(net8)$name
centrality_table8 <- data.frame(
  Tag = Tag,
  degree = net8_deg)

#merge
dc8 <- merge(centrality_table8, fd, by.x= "Tag")

dc8_chicks <- subset(dc8, dc8$Who=="Chick")# 52 chicks --> more than week7 but less than week6

#betweenness centrality
btw_8 <- betweenness(net8,v = V(net8),directed = F)
betweenness_table8 <- data.frame(
  Tag = Tag,
  betweenness = btw_8)

table_week8 <- merge(centrality_table8,betweenness_table8, by.x= "Tag" )
table_week8$Week <- 8 
#plot
dcc8 <- merge (betweenness_table8, dc8_chicks, by.x= "Tag")


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

Tag <- V(net9)$name
centrality_table9 <- data.frame(
  Tag = Tag,
  degree = net9_deg)

#merge
dc9 <- merge(centrality_table9, fd, by.x= "Tag")
dc9_chicks <- subset(dc9, dc9$Who=="Chick")# 41 chicks

#betweenness centrality
btw_9 <- betweenness(net9,v = V(net9),directed = F)
betweenness_table9 <- data.frame(
  Tag = Tag,
  betweenness = btw_9)

table_week9 <- merge(centrality_table9,betweenness_table9, by.x= "Tag" )
table_week9$Week <- 9 


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


Tag <- V(net10)$name
centrality_table10 <- data.frame(
  Tag = Tag,
  degree = net10_deg)

#merge
dc10 <- merge(centrality_table10, fd, by.x= "Tag")
dc10_chicks <- subset(dc10, dc10$Who=="Chick")# 40 chicks


#betweenness centrality
btw_10 <- betweenness(net10,v = V(net10),directed = F)
betweenness_table10 <- data.frame(
  Tag = Tag,
  betweenness = btw_10)

table_week10 <- merge(centrality_table10,betweenness_table10, by.x= "Tag" )
table_week10$Week <- 10 

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

Tag <- V(net11)$name
centrality_table11 <- data.frame(
  Tag = Tag,
  degree = net11_deg)

#merge
dc11 <- merge(centrality_table11, fd, by.x= "Tag")
dc11_chicks <- subset(dc11, dc11$Who=="Chick")# 28 chicks

#betweenness centrality
btw_11 <- betweenness(net11,v = V(net11),directed = F)
betweenness_table11 <- data.frame(
  Tag = Tag,
  betweenness = btw_11)

table_week11 <- merge(centrality_table11,betweenness_table11, by.x= "Tag" )
table_week11$Week <- 11 

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

Tag <- V(net12)$name
centrality_table12 <- data.frame(
  Tag = Tag,
  degree = net12_deg)

#merge
dc12 <- merge(centrality_table12, fd, by.x= "Tag")
dc12_chicks <- subset(dc12, dc12$Who=="Chick")# 41 chicks

#betweenness centrality
btw_12 <- betweenness(net12,v = V(net12),directed = F)
betweenness_table12 <- data.frame(
  Tag = Tag,
  betweenness = btw_12)

table_week12 <- merge(centrality_table12,betweenness_table12, by.x= "Tag" )
table_week12$Week <- 12 

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

Tag <- V(net13)$name
centrality_table13 <- data.frame(
  Tag = Tag,
  degree = net13_deg)

#merge
dc13 <- merge(centrality_table13, fd, by.x= "Tag")
dc13_chicks <- subset(dc13, dc13$Who=="Chick")# 26 chicks

#betweenness centrality
btw_13 <- betweenness(net13,v = V(net13),directed = F)
betweenness_table13 <- data.frame(
  Tag = Tag,
  betweenness = btw_13)

table_week13 <- merge(centrality_table13,betweenness_table13, by.x= "Tag" )
table_week13$Week <- 13 

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


Tag <- V(net14)$name
centrality_table14 <- data.frame(
  Tag = Tag,
  degree = net14_deg)

#merge
dc14 <- merge(centrality_table14, fd, by.x= "Tag")
dc14_chicks <- subset(dc14, dc14$Who=="Chick")# 32 chicks


#betweenness centrality
btw_14 <- betweenness(net14,v = V(net14),directed = F)
betweenness_table14 <- data.frame(
  Tag = Tag,
  betweenness = btw_14)

table_week14 <- merge(centrality_table14,betweenness_table14, by.x= "Tag" )
table_week14$Week <- 14




#####REGRESSION MODELS

###Regression models, look at the effect of fledge order on the social network position
#fledge order = ordinal + has to be centered around zero (use scale function for this)
#control for weight
#First did it for the whole data, without taking into account possible differences/week.
#Would be interesting to make the regression models from week4 as in that week the Chicks started to be seen at least 5 times.

#1. rescale fledge order
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


#2. Prepare the data
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


#3. The models
#The models were tried with lm but there were assumption deviations). thise can be find in the old code (trial_SNA.R). I didn't put them here to win some space

#a.Try the models with scaled_FledgeOrder2 where 0 are first fledglings, higher is others
library(dplyr)
new_data <- new_data %>%
  mutate(factor.order = ifelse(scaled_FledgeOrder2 == 0, "First", "Other"))
View(new_data)
View(new_data)
new_data$factor.order <- as.factor(new_data$factor.order)
str(new_data$factor.order)

#with glmer (more robust against assumption deviations)

#degree
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


## b. Try when last sibling versus all other instead of the first. 
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


######REPEATABILITY OF SN POSITION (during the summer, early stages)

#Here propose to do this:https://academic.oup.com/jeb/article/33/11/1634/7326429
#We consider the observed repeatabilities as statistically significant when their 95% credible intervals (hereafter 95CI) did not include the mean of the respective permuted repeatabilities.
#! had to do permutations in order to test for statistical significance
#"1000 iterations of the observed data for every model"

##tutorial: https://dshizuka.github.io/networkanalysis/example_assortment_in_wytham.html



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

#--> This will be used for the Randomized R (see below). But first the Observed Repeatability

#I applied here the code comming from the paper FID analysis, https://www.sciencedirect.com/science/article/abs/pii/S000334722200001X 
#It is simplified, see below

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
#However, for autumn, only three weeks so difficult to test for repeatability
#In the old code, there is the repeatability for autumn and across seasons (summer + autumn)


######ORDER OF ARRIVAL ON FEEDERS  (ALSO PART OF SCENARIO 2)

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


##The research questions
#1. For the juveniles, could the order of arrival be predicted by the fledge order?
#2. Are individuals consistent in the order within which they arrive at the feeder? 
#3. Could the order of arrival be predicted by age class (first year/adult), 
#(#4. is there a correlation between arrival at feeders and social network position)


#1.
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

#2.
rpt(order ~ (1|Tag), grname ="Tag", datatype="Poisson", data=chicks.data.summer, CI = 0.95, nboot = 100, npermut = 0)
#link scale
#R  = 0.042 --> low repeatability
#SE = 0.011
#CI = [0.015, 0.057]
#P  = 3.23e-56 [LRT]
#NA [Permutation]
#to heavy for my computer apparently so I could only do the nboot=100, if nboot=1000 should be better

#3. And could the order of arrival be predicted by age class (first year/adult)? 
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


######Again but Now when the order of arrival to feeders is analysed more precised age classes and species
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
load("data/gmm.autumn.RData")
head(gmm.autumn) 
load("data/gmm.summer.RData")
head(gmm.summer)
load("data/gmm.winter.RData")
head(gmm.winter) 
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
sp_class <- merge(net.data.summer, species_age_sex, by.x= "PIT")
head(sp_class)
dim(sp_class)
str(sp_class$species)
sp_class$species <- as.factor(sp_class$species)
sp_class$age_in_2020 <- as.factor(sp_class$age_in_2020)
#I see that there is "already.present" in order
#   --> this can be transformed in 0 so that my response variable consists of number and so I can use Poisson

# SW: the 'already present' should actually not even be in this data set! I had a line earlier that should have removed those
# cause it's just the same bird in the same group that has already arrived and has already been given an arrival order

sp_class$order[sp_class$order == "already.present"] <- 0

# SW: I kicked it out here:
sp_class <- subset(sp_class, sp_class$order!=0)

head(sp_class)
str(sp_class$order)
sp_class$order <- as.numeric(sp_class$order)
#apparently more than just great tits --> so I can use species

# SW: You can kick out the COATI and GREFI - COATI are so few and GREFI is most likely a mistake.
sp_class <- subset(sp_class, sp_class$species %in% c("BLUTI", "GRETI", "MARTI", "NUTHA"))

# SW: I'm saving the image here again so I can pick up here next time:
save.image(file="R.image2.RData")
load("R.image2.RData")


glm.summer <- glmer(order ~ species * age_in_2020 + (1|PIT), family=poisson, data= sp_class)
summary(glm.summer)
#Fixed effects:
#                                  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)                       0.94194    0.08104  11.623  < 2e-16 ***
#speciesGRETI                     -0.16634    0.08969  -1.855  0.06366 .  
#speciesMARTI                     -0.42810    0.13720  -3.120  0.00181 ** 
#speciesNUTHA                     -0.13196    0.14218  -0.928  0.35334    
#age_in_2020juvenile              -0.51988    0.30993  -1.677  0.09347 .  
#speciesGRETI:age_in_2020juvenile  0.76807    0.31412   2.445  0.01448 *  
#speciesMARTI:age_in_2020juvenile  1.36862    0.55708   2.457  0.01402 * 

drop1(glm.summer, test="Chisq") #species:age_in_2020   (AIC) 2 70696 (LRT) 7.763 (p-val) 0.02062 *
Anova(glm.summer)#differences within each level of the factors species and age, also the interaction between species and age is significant
m.class <-  brms::brm(order ~  species * age_in_2020 + (1|PIT) , family=poisson, data= sp_class)
#to heavy to for my computer to run this
summary(m.class)
pp_check(m.class)
library(tidybayes)
post.m.class <- m.class %>% spread_draws(sd_Tag__Intercept, b_Intercept)
R_order <- (post.m.class$sd_Tag__Intercept^2)/((post.m.class$sd_Tag__Intercept^2) + (post.m.class$b_Intercept^2))
mean(R_order)

rpt(order ~ species * age_in_2020 + (1|PIT),  data=sp_class,  grname="PIT", nboot=100, npermut=0, datatype = "Poisson")
#to heavy for my computer

#taking into account the differences in flock size
#by using weights
head(sp_class)
sp_class <- sp_class %>%
  group_by(group) %>%
  mutate(weight = 1 / n())

glm.summer.flock <- glmer(order ~ species * age_in_2020 + (1|PIT), family=poisson, weights= weight, data= sp_class)
#fixed-effect model matrix is rank deficient so dropping 1 column / coefficient  and boundary(singular) fit
summary(glm.summer.flock)
#Fixed effects:
#                                   Estimate Std. Error z value Pr(>|z|)    
#(Intercept)                       0.55763    0.08444   6.604   4e-11 ***
#speciesGRETI                     -0.07495    0.08961  -0.836   0.4029    
#speciesMARTI                     -0.25030    0.11709  -2.138   0.0325 *  
#speciesNUTHA                     -0.06500    0.12039  -0.540   0.5892    
#age_in_2020juvenile              -0.27845    0.31197  -0.893   0.3721    
#speciesGRETI:age_in_2020juvenile  0.48392    0.31457   1.538   0.1240    
#speciesMARTI:age_in_2020juvenile  1.34231    0.90428   1.484   0.1377  

#Try with another weights and make proportion of order:
head(sp_class)
View(sp_class)
sp_class <- sp_class %>%
  group_by(group) %>%
  mutate(Tot =n())

sp_class <- sp_class %>%
  mutate(OrderProp = order/Tot)

glm.summer.flock <- glmer(OrderProp ~ species * age_in_2020 + (1|PIT), family=poisson, weights= Tot, data= sp_class)
#fixed-effect model matrix is rank deficient so dropping 1 column / coefficient
summary(glm.summer.flock)
##too heavy for my computer

#try without the interaction
glm.summer.flock <- glmer(OrderProp ~ species + age_in_2020 + (1|PIT), family=poisson, weights= Tot, data= sp_class)
#still too heavy



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

glm.aut <- glmer(order ~ species * age_in_2020 + (1|PIT), family=poisson, data= sp_class_aut)#model failed to converge
drop1(glm.aut, test="Chisq")#intercation is non significant
glm.aut <- glmer(order ~ species + age_in_2020 + (1|PIT), family=poisson, data= sp_class_aut)
summary(glm.aut)
# Fixed effects:
#                      Estimate Std. Error z value Pr(>|z|)    
#(Intercept)          1.67874    0.10282  16.326  < 2e-16 ***
#speciesGRETI        -0.04545    0.12328  -0.369 0.712396    
#speciesMARTI        -0.67871    0.16747  -4.053 5.06e-05 ***
#speciesNUTHA        -0.97156    0.26226  -3.705 0.000212 ***
#age_in_2020juvenile  0.03959    0.09806   0.404 0.686402    

m.class_aut <-  brm(order ~  species * age_in_2020 + (1|PIT) , family=poisson, data= sp_class_aut)
#to heavy to for my computer to run this
summary(m.class)
pp_check(m.class)
library(tidybayes)
post.m.class.aut <- m.class.aut %>% spread_draws(sd_Tag__Intercept, b_Intercept)
R_order.aut <- (post.m.class.aut$sd_Tag__Intercept^2)/((post.m.class.aut$sd_Tag__Intercept^2) + (post.m.class.aut$b_Intercept^2))
mean(R_order.aut)

rpt(order ~ species * age_in_2020 + (1|PIT),  data=sp_class_aut,  grname="PIT", nboot=1000, npermut=0, datatype = "Poisson")
#too heavy :/ 


#Here I try ore simple models so that my computer can run it
glm.summer2 <- glmer(order ~ species + age_in_2020 + (1|PIT), family=poisson, data= sp_class)
summary(glm.summer2)
#Marti is the species that has a significant effect on the order of arrival, as well as juveniles. Marti and nutha sow a tendency
rpt(order ~ species + age_in_2020 + (1|PIT),  data=sp_class,  grname="PIT", nboot=100, npermut=0, datatype = "Poisson")
#still to heavy, my computer does not run this


#when taking into account the flock size, by using weights
head(sp_class_aut)
sp_class_aut <- sp_class_aut %>%
  group_by(group) %>%
  mutate(weight = 1 / n())

glm.aut.flock <- glmer(order ~ species * age_in_2020 + (1|PIT), family=poisson, weights= weight, data= sp_class_aut)
#model failed to converge
drop1(glm.aut.flock, test="Chisq")#interaction not significant
glm.aut.flock <- glmer(order ~ species + age_in_2020 + (1|PIT), family=poisson, weights= weight, data= sp_class_aut)
summary(glm.aut.flock)
#Fixed effects:
#                     Estimate   Std. Error z value Pr(>|z|)    
#(Intercept)          1.21753    0.11731  10.379  < 2e-16 ***
#speciesGRETI        -0.02780    0.13758  -0.202 0.839882    
#speciesMARTI        -0.50402    0.15672  -3.216 0.001300 ** 
#speciesNUTHA        -0.71010    0.20755  -3.421 0.000623 ***
#age_in_2020juvenile  0.08903    0.10905   0.816 0.414267    

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
str(sp_calss_wint$species)
sp_class_wint$order <- as.numeric(sp_class_wint$order)

glm.wint <- glmer(order ~ species * age_in_2020 + (1|PIT), family=poisson, data= sp_class_wint)
drop1(glm.wint, test="Chisq") #the interaction is not significant, hence I leave it out of the model
glm.wint <- glmer(order ~ species + age_in_2020 + (1|PIT), family=poisson, data= sp_class_wint)
summary(glm.wint)
#Fixed effects:
#                      Estimate Std. Error z value Pr(>|z|)    
#(Intercept)          2.729888   0.084681  32.237  < 2e-16 ***
#speciesGRETI        -0.009726   0.095426  -0.102  0.91882    
#speciesMARTI        -0.899064   0.178304  -5.042  4.6e-07 ***
#speciesNUTHA        -0.762020   0.246838  -3.087  0.00202 ** 
#age_in_2020juvenile  0.074387   0.085557   0.869  0.38460   

rpt(order ~ species + age_in_2020 + (1|PIT),  data=sp_class_wint,  grname="PIT", nboot=1000, npermut=0, datatype = "Poisson")
#too heavy for my computer

#when taking into account the flock size, by using weights
head(sp_class_wint)
sp_class_wint <- sp_class_wint %>%
  group_by(group) %>%
  mutate(weight = 1 / n())

glm.wint.flock <- glmer(order ~ species * age_in_2020 + (1|PIT), family=poisson, weights= weight, data= sp_class_wint)
drop1(glm.wint.flock, test="Chisq")#interaction not significant
glm.wint.flock <- glmer(order ~ species + age_in_2020 + (1|PIT), family=poisson, weights= weight, data= sp_class_wint)
summary(glm.wint.flock)
#Fixed effects:
#                    Estimate Std. Error z value Pr(>|z|)    
#(Intercept)          2.24241    0.08456  26.518  < 2e-16 ***
#speciesGRETI        -0.08523    0.09520  -0.895 0.370638    
#speciesMARTI        -0.99429    0.15957  -6.231 4.63e-10 ***
#speciesNUTHA        -0.79769    0.22410  -3.559 0.000372 ***
#age_in_2020juvenile  0.09071    0.08416   1.078 0.281138    

rpt(order ~ species + age_in_2020 + (1|PIT),  data=sp_class_wint,  grname="PIT", nboot=1000, npermut=0, datatype = "Poisson")



## Maybe interesting to see whether across seasons, the order is still repeatable.
#For this make one dataset with all seasons together
library(dplyr)
sp_class$season <- "summer"
head(sp_class)
sp_class_aut$season <- "autumn"
sp_class_wint$season <- "winter"
sp_class_season <- rbind(sp_class, sp_class_aut, sp_class_wint)
View(sp_class_season)
sp_class_season$species <- as.factor(sp_class_season$species)
sp_class_season$seasons <- as.factor(sp_class_season$season)
sp_class_season$age_in_2020 <- as.factor(sp_class_season$age_in_2020)
sp_class_season$order <- as.numeric(sp_class_season$order)
sp_class_wint$order[sp_class_wint$order == "already.present"] <- 0
sp_class_season <- subset(sp_class_season, sp_class_season$order!=0)
sp_class_season <- subset(sp_class_season, sp_class_season$species %in% c("BLUTI", "GRETI", "MARTI", "NUTHA"))


glm.season <- glmer(order ~ species + age_in_2020 + season + (1|PIT), family=poisson, data= sp_class_season)#without interaction so that my computer can run it
summary(glm.season)
#Fixed effects:
#                     Estimate Std. Error z value Pr(>|z|)    
#(Intercept)          1.69108    0.05949  28.425  < 2e-16 ***
#speciesGRETI        -0.08536    0.06422  -1.329   0.1838    
#speciesMARTI        -0.68327    0.12384  -5.518 3.44e-08 ***
#speciesNUTHA        -0.59951    0.13798  -4.345 1.39e-05 ***
#age_in_2020juvenile  0.11925    0.05268   2.264   0.0236 *  
#seasonsummer        -0.65313    0.01530 -42.678  < 2e-16 ***
#seasonwinter         1.03922    0.01471  70.656  < 2e-16 ***

rpt(order ~ species + age_in_2020+ season + (1|PIT),  data=sp_class_season,  grname="PIT", nboot=1000, npermut=0, datatype = "Poisson")
#to heavy for my computer

#taking into account the interactions
glm.season2 <- glmer(order ~ species*age_in_2020 + species*season + age_in_2020*season + (1|PIT), family=poisson, data= sp_class_season)#without interaction so that my computer can run it
summary(glm.season2)
#to heavy for my computer

#just with one interaction (seems interesting to know whether the adults/juveniles change their order of arrival depending on the season, especially the juveniles)
glm.season3 <- glmer(order ~ species +  age_in_2020*season + (1|PIT), family=poisson, data= sp_class_season)
drop1(glm.season3, test="Chisq")#interaction is significant
summary(glm.season3)
#Fixed effects:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)                       1.72914    0.06044  28.608  < 2e-16 ***
#speciesGRETI                     -0.08545    0.06420  -1.331  0.18320    
#speciesMARTI                     -0.68379    0.12376  -5.525 3.29e-08 ***
#speciesNUTHA                     -0.59315    0.13805  -4.297 1.73e-05 ***
#age_in_2020juvenile               0.02449    0.05950   0.412  0.68061    
#seasonsummer                     -0.70850    0.01989 -35.618  < 2e-16 ***
#seasonwinter                      1.00759    0.01910  52.750  < 2e-16 ***
#age_in_2020juvenile:seasonsummer  0.13274    0.03115   4.261 2.03e-05 ***
#age_in_2020juvenile:seasonwinter  0.07748    0.02995   2.587  0.00967 ** 


#considering weights

head(sp_class_season)
sp_class_season <- sp_class_season %>%
  group_by(group) %>%
  mutate(weight = 1 / n())

#without the interaction so it would be easier to run
glm.season4 <- glmer(order ~ species +  age_in_2020 + season + (1|PIT), family=poisson, weights= weight, data= sp_class_season)
summary(glm.season4)
#Fixed effects:
#Estimate Std. Error z value Pr(>|z|)    
#(Intercept)          1.45845    0.07313  19.942  < 2e-16 ***
#speciesGRETI        -0.20805    0.05754  -3.616 0.000299 ***
#speciesMARTI        -0.62370    0.08921  -6.992 2.72e-12 ***
#speciesNUTHA        -0.44304    0.10099  -4.387 1.15e-05 ***
#age_in_2020juvenile  0.15811    0.03972   3.981 6.87e-05 ***
#seasonsummer        -0.68999    0.05419 -12.732  < 2e-16 ***
#seasonwinter         0.78461    0.05729  13.694  < 2e-16 ***

#Maybe when running rpt, I should use the proportions of order (as done above) so that I can include the flock size. Cannot use weight in rpt
sp_class_season <- sp_class_season %>%
  group_by(group) %>%
  mutate(Tot =n())

sp_class_season <- sp_class_season %>%
  mutate(OrderProp = order/Tot)
View(sp_class_season)

glm.season4 <- glmer(OrderProp ~ species +  age_in_2020 + season + (1|PIT), family=poisson, weights= Tot, data= sp_class_season)
#too heavy for my computer :/

rpt(OrderProp ~ species + age_in_2020+ season + (1|PIT),  data=sp_class_season,  grname="PIT", nboot=1000, npermut=0, datatype = "Poisson")
#too heavy for my computer


#and for juveniles, could the order be predicted by the fledge order?
#make a dataset with fd and net.data.summer (fd only takes into account summer data)
head(fd)
head(net.data.summer)
fd$PIT <- fd$Tag
order_fd <- merge(net.data.summer, fd, by.x= "PIT")
head(order_fd)
order_fd$order[order_fd$order == "already.present"] <- 0
order_fd <- subset(order_fd, order_fd$order!=0)
order_fd$order <- as.numeric(order_fd$order)

glm.order <- glmer(order ~ fledge.order.scaled.within  + (1|PIT), family=poisson, data=order_fd)
summary(glm.order)#the fledge order has no effect on the order of arrival to the feeders

#considering weights
head(order_fd)
View(order_fd)
order_fd <- order_fd %>%
  group_by(group) %>%
  mutate(weight = 1 / n())

glm.order2 <- glmer(order ~ fledge.order.scaled.within  + (1|PIT), family=poisson, weights= weight, data=order_fd)
summary(glm.order2)#not significant




###CORRELATION BETWEEN FLEDGE ORDER AND SN POSITION (No longer scenario 2, it is the pursuit of scenario1)
#summer

#data: sp_class (order summer) + degree/centrality (new_data)
head(new_data)
head(sp_class)

sp_class$Tag <- sp_class$PIT

SN_order<- merge (sp_class, new_data, by.x= "Tag")
head(SN_order)
SN_order <- na.omit(SN_order)

cor.test(SN_order$order, SN_order$betweenness, method= "pearson")
#p-value = 0.3134
#cor = -0.006410831 
cor.test(SN_order$order, SN_order$degree, method= "pearson")
#p-value = 1.621e-10
#cor = 0.04064434 
X <- c(0.3134,1.621e-10)
library(psych)
p.adjust(X, method = "holm")
# 3.134e-01 3.242e-10


corr <-data.frame(SN_order$order, SN_order$betweenness, SN_order$degree)
colnames(corr) <- c("order of arival to feeders", "betweenness centrality",  "degree centrality")
t <- cor(corr)
library(corrplot)
corrplot(t, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)
testRes <- corrplot::cor.mtest(corr, conf.level = 0.95)
corrplot(t, p.mat = testRes$p, method = 'color', diag = FALSE, type = 'upper',tl.srt = 55,
         sig.level = c(0.001, 0.01, 0.05), pch.cex = 0.9,
         insig = 'label_sig', pch.col = 'grey20', order = 'AOE')

#Poor correlation between the order of arrival to the feeders and the social network positions


