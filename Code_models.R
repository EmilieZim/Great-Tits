# elephant: write the title of the paper and authors


# 1) Data prep ------------------------------------------------------------


# 1.1) Load libraries -----------------------------------------------------

# elephant: please move all the libraries up here that you use throuhgout (I have done it up to part 3) - and best practise to order libraries alphabetically (although that's just aesthetics)

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
library(performance)
library(brms)
library(dplyr)
library(tidyr)
library(ggplot2)
library(emmeans)
library(flextable)
library(dplyr)



# 1.2) Extract order of arrival at feeders --------------------------------

# Note that you can load the input data directly here, but we also provide code to replicate the data extraction from the raw data here (network_data_prep.R)


# 1.3) Load data ----------------------------------------------------------

# this contains the complete data set for analysis the leader follower dynamics
network.pos.all.seasons <- read.csv("data/leader_follower_data_all_seasons.csv", row.names = 1)
head(network.pos.all.seasons)



# elephant: please add some details on what the different columns are
names(network.pos.all.seasons)

# "PIT"
# "degree"
# "Date.Time"
# "Antenna"
# "location"
# "week"
# "group"
# "visit.duration"  
# "leader.follower" 
# "species"         
# "age_in_2020"
# "season"         
# "betweenness"
# "group.size"
# "group.size.log"
# "n_species"
# "flock_size"
# "species_prop" 


# remove group sizes of 1 
network.pos.all.seasons <- subset(network.pos.all.seasons, network.pos.all.seasons$group.size>1)

# how many data points per group size per species?
table(network.pos.all.seasons$species, network.pos.all.seasons$group.size)

# 2) Follower-leader dynamics ---------------------------------------------


# 2.1) Calculate VIFs -----------------------------------------------------

network.pos.all.seasons$leader.follower <- as.factor(network.pos.all.seasons$leader.follower)

# we first run a simple regression (no interactions) to look at collinearity between predictors
model_order_vif <- glmer(leader.follower ~  species + season + age_in_2020 + scale(degree) + scale(betweenness) + species_prop + (1|PIT), family= binomial, data= network.pos.all.seasons)

summary(model_order_vif)

library(car)
vif(model_order_vif)
# GVIF Df GVIF^(1/(2*Df))
# species            1.719961  3        1.094594
# season             2.807551  3        1.187740
# age_in_2020        1.130303  1        1.063157
# scale(degree)      2.754871  1        1.659780
# scale(betweenness) 1.315307  1        1.146868
# species_prop       1.268547  1        1.126298

# all vifs <5, so we can include all of them


# 2.2) Global model -------------------------------------------------------

# we add a small number to the species prop to avoid Inf in the offset
epsilon <- 0.001
network.pos.all.seasons <- network.pos.all.seasons %>%
  mutate(
    species_prop_adj = pmin(pmax(species_prop, epsilon), 1 - epsilon)
  )

# we run a model with leader follower as outcome variable
# include an offset that controls for species prevalence in each flock - which basically models if a species is more or less likely to be a leader or follower than would be expected by chance
glob_model <- brm(
  leader.follower ~  species * season + age_in_2020*season + scale(betweenness) + scale(degree) + offset(qlogis(species_prop_adj))  + (1 | PIT),
  family = bernoulli, 
  data = network.pos.all.seasons,
  chains = 4,
  iter = 4000,
  cores = 4,
  save_pars = save_pars(all = TRUE)
)

# save(glob_model, file="model output/glob_model.RDA")
load("model output/glob_model.RDA")

summary(glob_model)

# Family: bernoulli 
# Links: mu = logit 
# Formula: leader.follower ~ species * season + age_in_2020 * season + scale(betweenness) + scale(degree) + offset(qlogis(species_prop_adj)) + (1 | PIT) 
# Data: network.pos.all.seasons (Number of observations: 24048) 
# Draws: 4 chains, each with iter = 4000; warmup = 2000; thin = 1;
# total post-warmup draws = 8000
# 
# Multilevel Hyperparameters:
#   ~PIT (Number of levels: 234) 
# Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sd(Intercept)     0.39      0.03     0.33     0.45 1.00     2819     4493
# 
# Regression Coefficients:
#   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept                           -0.52      0.17    -0.85    -0.19 1.00     2442     3399
# speciesGRETI                        -1.44      0.19    -1.80    -1.07 1.00     2955     4369
# speciesMARTI                         0.18      0.21    -0.23     0.60 1.00     2564     3942
# speciesNUTHA                         0.92      0.25     0.42     1.41 1.00     2882     4302
# seasonspring                        -0.04      0.16    -0.37     0.28 1.00     2581     3668
# seasonsummer                        -0.42      0.27    -0.96     0.10 1.00     3398     3933
# seasonwinter                        -0.30      0.17    -0.64     0.05 1.00     2702     3771
# age_in_2020juvenile                  0.38      0.16     0.08     0.70 1.00     3535     4463
# scalebetweenness                     0.14      0.04     0.07     0.22 1.00     5310     5521
# scaledegree                         -0.47      0.04    -0.55    -0.39 1.00     5640     5976
# speciesGRETI:seasonspring            0.42      0.20     0.04     0.81 1.00     3131     4447
# speciesMARTI:seasonspring           -0.05      0.21    -0.45     0.37 1.00     3193     4772
# speciesNUTHA:seasonspring           -0.17      0.28    -0.71     0.38 1.00     4348     5807
# speciesGRETI:seasonsummer            0.71      0.30     0.12     1.31 1.00     3654     4465
# speciesMARTI:seasonsummer            0.52      0.30    -0.07     1.13 1.00     3560     4277
# speciesNUTHA:seasonsummer            0.45      0.32    -0.15     1.09 1.00     3573     4155
# speciesGRETI:seasonwinter            0.35      0.19    -0.03     0.72 1.00     3164     4680
# speciesMARTI:seasonwinter            0.31      0.19    -0.07     0.68 1.00     2932     4233
# speciesNUTHA:seasonwinter            0.00      0.23    -0.44     0.45 1.00     3498     5461
# seasonspring:age_in_2020juvenile    -0.29      0.17    -0.64     0.03 1.00     4145     4653
# seasonsummer:age_in_2020juvenile    -0.52      0.19    -0.89    -0.17 1.00     4167     4908
# seasonwinter:age_in_2020juvenile    -0.27      0.16    -0.59     0.03 1.00     4069     5180
# 
# Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
# and Tail_ESS are effective sample size measures, and Rhat is the potential
# scale reduction factor on split chains (at convergence, Rhat = 1).


# 2.3) Model checks -------------------------------------------------------


# check how well it fits
pp_check(glob_model, ndraws = 100)
# looks like a very good fit
# the asymmetry means that birds are more likely to be followers (0) than leaders (1), which makes sense

plot(glob_model)
# stationarity and mixing are good

# have a first look at the plot (conditional effects)
glob_model_conditional_effects <- plot(conditional_effects(glob_model, re_formula = NA))

# a 0 here means to a follower (baseline)
# formula = NA excludes random effects

# extract an R2 for our model
R2 <- performance::r2_bayes(glob_model)
# # Bayesian R2 with Compatibility Interval
# 
# Conditional R2: 0.136 (95% CI [0.132, 0.141])
# Marginal R2: 0.127 (95% CI [0.116, 0.137])

# 2.4) Summary table ------------------------------------------------------


##make a table with the summary
summary_glob_model <- summary(glob_model)
fixed_effects <- as.data.frame(summary_glob_model$fixed)
fixed_effects <- tibble::rownames_to_column(fixed_effects, var = "Term")


library(flextable)

flextable_table <- fixed_effects %>%
  flextable() %>%
  autofit() %>%
  set_caption("Summary of Fixed Effects from the final Model")

library(officer)

doc_glob_model <- read_docx() %>%
  body_add_flextable(flextable_table)

print(doc_glob_model, target = "fixed_effects_final_glob_model.docx")


# 2.5) Plot ---------------------------------------------------------------

# elephant: need to adjust the plots
##plot of the summary output of the glob_model
library(ggpubr)
network.pos.all.seasons$season <- as.factor(network.pos.all.seasons$season)
levels(network.pos.all.seasons$season)
network.pos.all.seasons$species <- as.factor(network.pos.all.seasons$species)
levels(network.pos.all.seasons$species)
network.pos.all.seasons$age_in_2020 <- as.factor(network.pos.all.seasons$age_in_2020)
levels(network.pos.all.seasons$age_in_2020)

my_colors <- c("spring" = "#56ae6c", 
               "summer" = "#8960b3", 
               "winter" = "#b0923b", 
               "autumn" = "#ba495b")
my_colors_sp <- c("BLUTI" = "#b94b75", 
                  "GRETI" = "#72ac5c", 
                  "MARTI" = "#7f64b9", 
                  "NUTHA" = "#bb7438")

my_colors_age <- c("adult" = "#9a5ea1", 
                   "juvenile" = "#98823c")

ggarrange(
  
  glob_model_conditional_effects$`group.size:species` +
    labs(x= "Flock size", y= "Leader status") +
    ylim(c(0.0, 0.9))+
    theme_bw() +
    theme(legend.background=element_blank()) +
    theme(legend.title=element_blank(), legend.position = c(0.78, 0.6), axis.title.x = element_blank())+
    scale_fill_manual(values = my_colors_sp, breaks=c("BLUTI", "GRETI", "MARTI", "NUTHA"), labels=c("Blue tits", "Great tits", "Marsh tits", "Nuthaches"))+
    scale_color_manual(values = my_colors_sp, breaks=c("BLUTI", "GRETI", "MARTI", "NUTHA"), labels=c("Blue tits", "Great tits", "Marsh tits", "Nuthaches")),
  
  
  glob_model_conditional_effects$degree +
    labs(x= "Degree centrality", y= "Leader status") +
    labs(y= "") +
    ylim(c(0.0, 0.9))+
    theme_bw(),
  
  glob_model_conditional_effects$`group.size:age_in_2020` +
    labs(x= "Flock size", y= "Leader status") +
    labs(y= "Leader status") +
    ylim(c(0.0, 0.9))+
    theme_bw() +
    theme(legend.background=element_blank())+
    theme(legend.title=element_blank(), legend.position = c(0.78, 0.6), axis.title.x = element_blank())+
    scale_fill_manual(values = my_colors_age, breaks=c("adult", "juvenile"), labels=c("Adults", "Juveniles"))+
    scale_color_manual(values = my_colors_age, breaks=c("adult", "juvenile"), labels=c("Adults", "Juveniles")),
  
  glob_model_conditional_effects$betweenness +
    labs(x= "Betweenness centrality", y= "Leader status") +
    labs(y= "") +
    ylim(c(0.0, 0.9))+
    theme_bw(),
  
  glob_model_conditional_effects$`group.size:season`+
    labs(x= "Flock size", y= "Leader status") +
    labs(y = "Leader status") +
    ylim(c(0.0, 0.9))+
    theme_bw() +
    theme(legend.background=element_blank())+
    theme(legend.title=element_blank(), legend.position = c(0.78, 0.6))+
    scale_fill_manual(values = my_colors, breaks=c("autumn", "spring", "summer", "winter"), labels=c("Autumn", "Spring", "Summer", "Winter"))+
    scale_color_manual(values = my_colors, breaks=c("autumn", "spring", "summer", "winter"), labels=c("Autumn", "Spring", "Summer", "Winter")),
  
  
  #legend="right",
  common.legend = FALSE,
  labels= c("a", "d", "b", "e", "c"),
  ncol = 2, nrow = 3)


#display.brewer.all()

#This shows me that the model codes 0 as follower and 1 as leader. So an observation that at the y axis (leader.follower) has 0.7 this means that that observation has a 70% chance to be a leader.  


# 2.8) Compute species comparison -----------------------------------------

# the estimates are expressed in their logit scale so here transform them into odds ratio's
exp(fixef(glob_model))
# Estimate Est.Error      Q2.5     Q97.5
# Intercept                        0.5946538  1.182932 0.4276606 0.8286309
# speciesGRETI                     0.2377178  1.205854 0.1655098 0.3438498
# speciesMARTI                     1.2031726  1.235008 0.7963841 1.8253918
# speciesNUTHA                     2.5046924  1.286932 1.5161438 4.1047411
# seasonspring                     0.9590453  1.179187 0.6911142 1.3279270
# seasonsummer                     0.6571342  1.313575 0.3813961 1.1053641
# seasonwinter                     0.7435422  1.190539 0.5257325 1.0484757
# age_in_2020juvenile              1.4695554  1.173047 1.0863561 2.0187820
# scalebetweenness                 1.1550211  1.037712 1.0731676 1.2424225
# scaledegree                      0.6271391  1.042741 0.5770963 0.6799911
# speciesGRETI:seasonspring        1.5259372  1.215573 1.0433722 2.2430693
# speciesMARTI:seasonspring        0.9545878  1.233550 0.6358985 1.4518843
# speciesNUTHA:seasonspring        0.8459474  1.322981 0.4924490 1.4615501
# speciesGRETI:seasonsummer        2.0274757  1.353543 1.1325330 3.7241668
# speciesMARTI:seasonsummer        1.6818329  1.354601 0.9323330 3.0819060
# speciesNUTHA:seasonsummer        1.5719422  1.372641 0.8593745 2.9701258
# speciesGRETI:seasonwinter        1.4128330  1.206861 0.9723192 2.0470286
# speciesMARTI:seasonwinter        1.3605838  1.208800 0.9362071 1.9775309
# speciesNUTHA:seasonwinter        1.0011498  1.256140 0.6430844 1.5710772
# seasonspring:age_in_2020juvenile 0.7448998  1.187528 0.5278199 1.0347261
# seasonsummer:age_in_2020juvenile 0.5917646  1.203720 0.4090548 0.8408721
# seasonwinter:age_in_2020juvenile 0.7644497  1.173633 0.5520269 1.0355749


# in our model, blue tits are the baseline species to which all others are compared. We would like to compute comparisons between all species. 


# Compute marginal means for the species variable
species_emm <- emmeans(glob_model, ~ species)
# Pairwise comparisons between species levels
species_contrasts <- contrast(species_emm, method = "pairwise")
species_contrasts

# contrast      estimate lower.HPD upper.HPD
# BLUTI - GRETI    1.069     0.858    1.2688
# BLUTI - MARTI   -0.378    -0.693   -0.0856
# BLUTI - NUTHA   -0.987    -1.329   -0.6280
# GRETI - MARTI   -1.447    -1.708   -1.1639
# GRETI - NUTHA   -2.056    -2.368   -1.7341
# MARTI - NUTHA   -0.610    -0.992   -0.2302
# 
# Results are averaged over the levels of: season, age_in_2020 
# Point estimate displayed: median 
# Results are given on the log odds ratio (not the response) scale. 
# HPD interval probability: 0.95 

# Marginal means for species by season
species_season_emm <- emmeans(glob_model, ~ species | season)
# Pairwise comparisons within each season
species_season_contrasts <- contrast(species_season_emm, method = "pairwise")
species_season_contrasts

# season = autumn:
#   contrast      estimate lower.HPD upper.HPD
# BLUTI - GRETI    1.437     1.068    1.7987
# BLUTI - MARTI   -0.184    -0.606    0.2221
# BLUTI - NUTHA   -0.918    -1.414   -0.4243
# GRETI - MARTI   -1.623    -2.025   -1.2505
# GRETI - NUTHA   -2.354    -2.801   -1.8865
# MARTI - NUTHA   -0.735    -1.218   -0.2668
# 
# season = spring:
#   contrast      estimate lower.HPD upper.HPD
# BLUTI - GRETI    1.014     0.812    1.2310
# BLUTI - MARTI   -0.138    -0.493    0.1856
# BLUTI - NUTHA   -0.749    -1.202   -0.2727
# GRETI - MARTI   -1.153    -1.495   -0.8224
# GRETI - NUTHA   -1.765    -2.202   -1.3051
# MARTI - NUTHA   -0.614    -1.156   -0.0916
# 
# season = summer:
#   contrast      estimate lower.HPD upper.HPD
# BLUTI - GRETI    0.735     0.217    1.2391
# BLUTI - MARTI   -0.701    -1.276   -0.1387
# BLUTI - NUTHA   -1.365    -1.970   -0.7921
# GRETI - MARTI   -1.436    -1.800   -1.0771
# GRETI - NUTHA   -2.096    -2.501   -1.6996
# MARTI - NUTHA   -0.669    -1.141   -0.1877
# 
# season = winter:
#   contrast      estimate lower.HPD upper.HPD
# BLUTI - GRETI    1.090     0.894    1.2847
# BLUTI - MARTI   -0.492    -0.796   -0.1873
# BLUTI - NUTHA   -0.918    -1.297   -0.5313
# GRETI - MARTI   -1.585    -1.880   -1.3190
# GRETI - NUTHA   -2.010    -2.368   -1.6628
# MARTI - NUTHA   -0.427    -0.818    0.0140
# 
# Results are averaged over the levels of: age_in_2020 
# Point estimate displayed: median 
# Results are given on the log odds ratio (not the response) scale. 
# HPD interval probability: 0.95 


# How to interpret:
# estimate: difference in marginal means
# if the credible interval does not include 0, two groups are considered significantly different
# Plot the marginal means for species
plot(species_emm)
# Plot pairwise comparisons for species across seasons
plot(species_season_emm)
# Extract contrasts as a data frame
species_season_contrasts_df <- as.data.frame(species_season_contrasts)
head(species_season_contrasts_df)
# you get from log odds to odds by expontiating:
exp(species_season_contrasts_df$estimate) #thus odds are the estimates
# an odds ratio of 1.67 means that e.g. BLUTI are 1.67 times more likely to be the leader compared to GRETI
# If one were interested in probabilities for a certain species to be a leader, you calculate as: probability = exp(log odds) / (1 + exp(log odds)) = exp(estimate)/(1+ exp(estimate))
species_season_contrasts_df$odds <- exp(species_season_contrasts_df$estimate)

emm_table <- species_season_contrasts_df %>%
  dplyr::select(contrast, season, estimate, lower.HPD, upper.HPD, odds) #I make a table so I can make the calculations
#the percentage
emm_table$prob_emm <- (exp(emm_table$estimate) / (1 + exp(emm_table$estimate)))*100

#extract the dataframe into a table for Word


pairwise_table <- flextable(emm_table) %>%
  autofit() %>%
  set_caption("Pairwise Contrasts of Species by Season")

# Export to Word (optional)
library(officer)
doc_pariwise_comparisons <- read_docx() %>%
  body_add_flextable(pairwise_table)

print(doc_pariwise_comparisons, target = "species_season_model_summary.docx")


# 2.9) Calculate individual repeatability ---------------------------------

# how repeatable are individuals in their leader/follower roles?


#### repeatability of arrival (leader versus follower)

# here two alternative approaches:
# Extract variance components
var_components <- VarCorr(glob_model)
print(var_components)


# Extract the variance for the random effect PIT (squaring the standard deviation)
pit_var <- as.numeric(var_components$PIT$sd[1])^2

# Residual variance for Bernoulli-logit model
residual_variance <- pi^2 / 3

# Calculate repeatability
repeatability <- pit_var / (pit_var + residual_variance)
repeatability

# [1] 0.04321718

# same result as above!


# Extract the variance components and calculate repeatability
# other method, same value

get_variables(glob_model)#This just tells us what parameters we can pull from the model
#Extract the variance components
post.data = glob_model %>% spread_draws(sd_PIT__Intercept)
#Among-individual variance
post.data$Va_PIT = post.data$sd_PIT__Intercept^2 # we square it because it is currently a standard deviation, and by squaring this value we turn it into a variance

#Within-individual variance
#For Bernouilli: sigma^2 = pi^2/3
sigma_sq = (pi^2)/3

#Repeatability
repeatability <- post.data$Va_PIT / (post.data$Va_PIT + sigma_sq)
repeatability
hist(repeatability)
mean(repeatability) #same as above, with the other methods
# 0.04345461

#Calculate CI
# elephant: please rerun this, my package isn't properly isntalled apparently
rethinking::HPDI(repeatability, prob = 0.95)


#This last part was inspired by the study: https://datadryad.org/stash/dataset/doi:10.25338/B88P8W


# 2.10) Extract flock sizes in each season -------------------------------------------------------------------

#how many flocks there are in each season
# elephant: please only use the groups with sizes 2+ and use the network_pos_all_season isntead of the visitation one
visits_all_season %>%
  dplyr::group_by(season) %>%
  dplyr::summarise(num_groups = n_distinct(group))

# # A tibble: 4 × 2
# season num_groups
# <chr>       <int>
#   1 autumn       1556
# 2 spring       2363
# 3 summer       2098
# 4 winter       2844

#how many individuals within each flock in each season?
visits_all_season %>%
  filter(season == "summer")%>%
  count(group) %>%
  summarise(min_flock_size = min(n), max_flock_size = max(n))

# min_flock_size max_flock_size
# 1              1             15

visits_all_season %>%
  filter(season == "autumn")%>%
  count(group) %>%
  summarise(min_flock_size = min(n), max_flock_size = max(n))

# min_flock_size max_flock_size
# 1              1             11

visits_all_season %>%
  filter(season == "winter")%>%
  count(group) %>%
  summarise(min_flock_size = min(n), max_flock_size = max(n))

# min_flock_size max_flock_size
# 1              1             37

visits_all_season %>%
  filter(season == "spring")%>%
  count(group) %>%
  summarise(min_flock_size = min(n), max_flock_size = max(n))

# min_flock_size max_flock_size
# 1              1             22


# 3) Visitation rates ------------------------------------------------------

# here we look at potential caching behaviour of different species assuming that for caching, they would be flying back and forth more often
visits_all_season <- read.csv("data/visits_all_season.csv", row.names = 1)


colnames(visits_all_season)
# elephant: please list and explain the column names

# 3.1) vistation rates ----------------------------------------------------


# we first model visits ~ species*season + (1|PIT)
# this should tell use something about caching - caching species are expected to fly back and forth more often within one flock visit
head(visits_all_season) #group=flock
#visits are counts thus we consider a Poisson distribution

#with brm:
library(brms)
brm_visits <- brm(
  visit ~ species*season + (1|PIT),
  family = poisson,
  data = visits_all_season,
  chains = 2,
  iter = 4000,
  cores = 4,
  seed=2
)

#save(brm_visits, file="model output/brm_visits.RDA")
load("model output/brm_visits.RDA")

summary(brm_visits)


# Family: poisson 
# Links: mu = log 
# Formula: visit ~ species * season + (1 | PIT) 
# Data: visits_all_season (Number of observations: 26819) 
# Draws: 2 chains, each with iter = 4000; warmup = 2000; thin = 1;
# total post-warmup draws = 4000
# 
# Multilevel Hyperparameters:
#   ~PIT (Number of levels: 254) 
# Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sd(Intercept)     0.11      0.01     0.10     0.13 1.00     1538     2167
# 
# Regression Coefficients:
#   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept                     0.52      0.05     0.43     0.61 1.00      551     1001
# speciesGRETI                 -0.06      0.05    -0.17     0.05 1.00      599     1117
# speciesMARTI                  0.09      0.06    -0.03     0.21 1.00      682     1210
# speciesNUTHA                 -0.09      0.07    -0.23     0.05 1.00      581     1281
# seasonspring                 -0.20      0.05    -0.29    -0.11 1.00      531     1259
# seasonsummer                  0.03      0.07    -0.12     0.18 1.00      812     1636
# seasonwinter                 -0.30      0.05    -0.39    -0.20 1.00      531     1009
# speciesGRETI:seasonspring     0.09      0.06    -0.03     0.19 1.00      567     1359
# speciesMARTI:seasonspring     0.16      0.06     0.05     0.28 1.00      746     1664
# speciesNUTHA:seasonspring     0.24      0.08     0.08     0.40 1.00      945     1840
# speciesGRETI:seasonsummer     0.13      0.08    -0.03     0.29 1.00      845     1698
# speciesMARTI:seasonsummer    -0.01      0.08    -0.17     0.15 1.00      876     1843
# speciesNUTHA:seasonsummer    -0.12      0.09    -0.29     0.05 1.00      869     1853
# speciesGRETI:seasonwinter     0.04      0.06    -0.07     0.15 1.00      585     1219
# speciesMARTI:seasonwinter     0.18      0.05     0.07     0.28 1.00      603     1097
# speciesNUTHA:seasonwinter     0.25      0.07     0.12     0.38 1.00      735     1487
# 
# Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
# and Tail_ESS are effective sample size measures, and Rhat is the potential
# scale reduction factor on split chains (at convergence, Rhat = 1).

# check for over/under dispsersion

mean_visit <- mean(visits_all_season$visit)
var_visit <- var(visits_all_season$visit)
overdispersion_ratio <- var_visit / mean_visit
overdispersion_ratio
# [1] 0.7237296

# seems okay - we can stick with a poisson

pp_check(brm_visits, ndraws = 100)

# plot
visits_conditional_effects <- plot(conditional_effects(brm_visits), re_formula = NULL)


##make a table with the summary
summary_brm_visits <- summary(brm_visits)
fixed_effects <- as.data.frame(summary_brm_visits$fixed)
fixed_effects <- tibble::rownames_to_column(fixed_effects, var = "Term")


library(flextable)

flextable_table <- fixed_effects %>%
  flextable() %>%
  autofit() %>%
  set_caption("Summary of Fixed Effects from the final Model")

library(officer)

doc_brm_visits <- read_docx() %>%
  body_add_flextable(flextable_table)

print(doc_brm_visits, target = "fixed_effects_visitation_rate.docx")



# the estimates are expressed in their logit scale so here we transform them into odds ratio's
exp(fixef(brm_visits))
# Estimate Est.Error      Q2.5     Q97.5
# Intercept                 1.6810983  1.047473 1.5307977 1.8381222
# speciesGRETI              0.9437601  1.055937 0.8470645 1.0515658
# speciesMARTI              1.0956541  1.062371 0.9661981 1.2366192
# speciesNUTHA              0.9117945  1.072400 0.7962509 1.0485899
# seasonspring              0.8183903  1.049575 0.7455680 0.8997160
# seasonsummer              1.0319820  1.076223 0.8882663 1.1918842
# seasonwinter              0.7437556  1.048169 0.6794740 0.8168139
# speciesGRETI:seasonspring 1.0897109  1.059251 0.9734231 1.2151324
# speciesMARTI:seasonspring 1.1787379  1.062215 1.0469839 1.3280472
# speciesNUTHA:seasonspring 1.2711452  1.083867 1.0815065 1.4862291
# speciesGRETI:seasonsummer 1.1351588  1.082347 0.9717953 1.3308017
# speciesMARTI:seasonsummer 0.9854760  1.084638 0.8405785 1.1577483
# speciesNUTHA:seasonsummer 0.8835419  1.090368 0.7470332 1.0520307
# speciesGRETI:seasonwinter 1.0423715  1.056838 0.9343634 1.1611997
# speciesMARTI:seasonwinter 1.1938795  1.055657 1.0752670 1.3250180
# speciesNUTHA:seasonwinter 1.2861171  1.068658 1.1301792 1.4592192


#creating plot
library(ggpubr)
levels(visits_all_season$season)
my_colors <- c("spring" = "#56ae6c", 
               "summer" = "#8960b3", 
               "winter" = "#b0923b", 
               "autumn" = "#ba495b")

ggarrange(
  
  glob_model_conditional_effects$`species:season` +
    aes(shape = species) +
    scale_shape_manual(values = c("BLUTI" = 16, "GRETI" = 17, "MARTI" = 15, "NUTHA" = 18) ,
                       labels = c("Blue tits", "Great tits", "Marsh tits", "Nuthatches")) +
    labs( y= "Leader status") + 
    ylim(c(0.0,0.35))+
    theme_bw() +
    scale_fill_manual(values = my_colors, breaks=c("autumn", "spring", "summer", "winter"), labels=c("Autumn", "Spring", "Summer", "Winter"))+
    scale_color_manual(values= my_colors, breaks=c("autumn", "spring", "summer", "winter"), labels=c("Autumn", "Spring", "Summer", "Winter")) +
    scale_x_discrete(labels=c("BLUTI" = "Blue tits", "GRETI" = "Great tits", "MARTI" = "Marsh tits", "NUTHA" = "Nuthaches")) +
    theme(legend.title = element_blank(), axis.title.x = element_blank()),
  
  feeders_conditional_effects$`species:season` +
    aes(shape = species) +
    scale_shape_manual(values = c("BLUTI" = 16, "GRETI" = 17, "MARTI" = 15, "NUTHA" = 18)) +
    labs(y= "Number of visited feeders") + 
    ylim(c(0.0,4)) +
    theme_bw() +
    scale_fill_manual(values= my_colors, breaks=c("autumn", "spring", "summer", "winter"), labels=c("Autumn", "Spring", "Summer", "Winter"))+
    scale_color_manual(values= my_colors, breaks=c("autumn", "spring", "summer", "winter"), labels=c("Autumn", "Spring", "Summer", "Winter")) +
    scale_x_discrete(labels=c("BLUTI" = "Blue tits", "GRETI" = "Great tits", "MARTI" = "Marsh tits", "NUTHA" = "Nuthaches")) +
    theme(legend.title = element_blank(),axis.title.x = element_blank()),
  
  visits_conditional_effects$`species:season` +
    aes(shape = species) +
    scale_shape_manual(values = c("BLUTI" = 16, "GRETI" = 17, "MARTI" = 15, "NUTHA" = 18)) +
    labs(x= "Species", y= "Number of visits") +
    ylim(c(1.0,2.2)) +
    theme_bw() +
    scale_fill_manual(values= my_colors, breaks=c("autumn", "spring", "summer", "winter"), labels=c("Autumn", "Spring", "Summer", "Winter"))+
    scale_color_manual(values= my_colors, breaks=c("autumn", "spring", "summer", "winter"), labels=c("Autumn", "Spring", "Summer", "Winter")) +
    scale_x_discrete(labels=c("BLUTI" = "Blue tits", "GRETI" = "Great tits", "MARTI" = "Marsh tits", "NUTHA" = "Nuthaches")) +
    theme(legend.title=element_blank()),
  
  
  #legend="right",
  common.legend = TRUE,
  labels= c("a", "b", "c" ),
  label.x = 0.03,   
  label.y = 1.15,   
  ncol = 1, nrow = 3,
  heights = c(1.2, 1.2, 1.2) )



# Compute marginal means for the species variable
visits_species_emm <- emmeans(brm_visits, ~ species)
visits_contrasts <- contrast(visits_species_emm, method = "pairwise")
species_contrasts

visits_species_season_emm <- emmeans(brm_visits, ~ species | season)
visits_species_season_contrasts <- contrast(visits_species_season_emm, method = "pairwise")
visits_species_season_contrasts
# How to interpret:
# estimate: difference in marginal means
# if the credible interval does not include 0, two groups are considered significantly different
# Plot the marginal means for species
plot(visits_species_emm)
# Plot pairwise comparisons for species across seasons
plot(visits_species_season_emm)
# Extract contrasts as a data frame
visits_species_season_contrasts_df <- as.data.frame(visits_species_season_contrasts)
head(visits_species_season_contrasts_df)
# you get from log odds to odds by expontiating:
exp(visits_species_season_contrasts_df$estimate) #thus odds are the estimates

library(dplyr)
visits_emm_table <- visits_species_season_contrasts_df %>%
  select(contrast, season, estimate, lower.HPD, upper.HPD, odds) #I make a table so I can make the calculations
#the percentage
visits_emm_table$prob_emm <- (exp(visits_emm_table$estimate) / (1 + exp(visits_emm_table$estimate)))*100



#extract the dataframe into a table for Word
library(flextable)

pairwise_table_visits <- flextable(visits_emm_table) %>%
  autofit() %>%
  set_caption("Pairwise Contrasts of the number of visitis of Species by Season ")

# Export to Word (optional)
library(officer)
doc_pariwise_comparisons_visits <- read_docx() %>%
  body_add_flextable(pairwise_table_visits)

print(doc_pariwise_comparisons_visits, target = "species_season_visits_summary.docx")




# 3.2) Investigate how many feeders they visited --------------------------

# number of feeders visited ~ species*season + (1|PIT)
# this will tell use about territoriality - the more territorial species should have fewer feeders that they use
# we use this data frame:

network.pos.all.seasons
# the 'location' are the different feeders 

head(network.pos.all.seasons)

library(dplyr)
subset_network.pos.all.seasons <- network.pos.all.seasons %>% select(PIT, season, species, age_in_2020, location)
# we add the column "num.feeders.visited" that contains the total number of feeders each bird visited
subset2_network.pos.all.seasons <- subset_network.pos.all.seasons %>%
  group_by(PIT) %>%
  mutate(num.feeders.visited = n_distinct(location)) %>%
  ungroup()

head(subset2_network.pos.all.seasons) #some rows are duplicates so I decide to use the function distinct() to eliminate this
subset2_network.pos.all.seasons <- subset2_network.pos.all.seasons %>%
  distinct()

hist(subset2_network.pos.all.seasons$num.feeders.visited) #poisson distribution

brm_visited_feeders <- brm(
  num.feeders.visited ~ species*season + (1|PIT),
  family = poisson,
  data =subset2_network.pos.all.seasons,
  chains = 2,
  iter = 4000,
  cores = 4,
  save_pars = save_pars(all = TRUE)
)

# check for over/under dispsersion

mean_feeders_visited <- mean(subset2_network.pos.all.seasons$num.feeders.visited)
var_feeders_visited <- var(subset2_network.pos.all.seasons$num.feeders.visited)
overdispersion_ratio <- var_feeders_visited / mean_feeders_visited
overdispersion_ratio
# [1] 0.5448532

# our model is underdispersed

# we try a binomial model with the maximum number of feeders included as trials (=6)
brm_visited_feeders <- brm(
  num.feeders.visited | trials(6) ~ species * season + (1 | PIT),
  family = binomial(),
  data = subset2_network.pos.all.seasons,
  chains = 2,
  iter = 4000,
  cores = 4,
  save_pars = save_pars(all = TRUE)
)

#save(brm_visited_feeders, file="model output/brm_visited_feeders.RDA")
load("model output/brm_visited_feeders.RDA")


summary(brm_visited_feeders)
# Family: binomial 
# Links: mu = logit 
# Formula: num.feeders.visited | trials(6) ~ species * season + (1 | PIT) 
# Data: subset2_network.pos.all.seasons (Number of observations: 683) 
# Draws: 2 chains, each with iter = 4000; warmup = 2000; thin = 1;
# total post-warmup draws = 4000
# 
# Multilevel Hyperparameters:
#   ~PIT (Number of levels: 234) 
# Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sd(Intercept)     0.64      0.05     0.55     0.75 1.00     2124     2948
# 
# Regression Coefficients:
#   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept                    -1.07      0.27    -1.60    -0.55 1.00      977     1888
# speciesGRETI                  0.34      0.30    -0.23     0.93 1.00     1040     2071
# speciesMARTI                  0.55      0.38    -0.17     1.31 1.00     1085     1867
# speciesNUTHA                  0.01      0.56    -1.11     1.07 1.00     1686     2365
# seasonspring                 -0.01      0.29    -0.54     0.57 1.00     1080     2197
# seasonsummer                  0.16      0.35    -0.50     0.86 1.00     1214     2328
# seasonwinter                  0.08      0.28    -0.45     0.63 1.00     1039     2066
# speciesGRETI:seasonspring    -0.06      0.33    -0.71     0.56 1.00     1132     2135
# speciesMARTI:seasonspring    -0.04      0.42    -0.89     0.77 1.00     1364     2404
# speciesNUTHA:seasonspring    -0.32      0.72    -1.74     1.08 1.00     2360     2765
# speciesGRETI:seasonsummer    -0.24      0.38    -1.00     0.51 1.00     1247     2044
# speciesMARTI:seasonsummer    -0.23      0.50    -1.24     0.72 1.00     1680     2876
# speciesNUTHA:seasonsummer    -0.32      0.64    -1.61     0.92 1.00     1948     2573
# speciesGRETI:seasonwinter    -0.13      0.31    -0.76     0.47 1.00     1056     2116
# speciesMARTI:seasonwinter    -0.07      0.40    -0.86     0.70 1.00     1315     2636
# speciesNUTHA:seasonwinter    -0.28      0.60    -1.44     0.90 1.00     1827     2416
# 
# Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
# and Tail_ESS are effective sample size measures, and Rhat is the potential
# scale reduction factor on split chains (at convergence, Rhat = 1).
feeders_conditional_effects <- plot(conditional_effects(brm_visited_feeders), re_formula = NULL)
pp_check(brm_visits, ndraws = 100)


##make a table with the summary
summary_visited_feeders <- summary(brm_visited_feeders)
fixed_effects <- as.data.frame(summary_visited_feeders$fixed)
fixed_effects <- tibble::rownames_to_column(fixed_effects, var = "Term")


library(flextable)

flextable_table <- fixed_effects %>%
  flextable() %>%
  autofit() %>%
  set_caption("Summary of Fixed Effects from the number of visited feeders")

library(officer)

doc_visited_feeders <- read_docx() %>%
  body_add_flextable(flextable_table)

print(doc_visited_feeders, target = "fixed_effects_visited_feeders.docx")



# estimates are expressed in their logit scale so here I will transform them into odds ratios
exp(fixef(brm_visited_feeders))
#                           Estimate Est.Error      Q2.5     Q97.5
# Estimate Est.Error      Q2.5     Q97.5
# Intercept                 0.3439054  1.305523 0.2024691 0.5757744
# speciesGRETI              1.4020960  1.348015 0.7939488 2.5293007
# speciesMARTI              1.7330983  1.467177 0.8396807 3.7043368
# speciesNUTHA              1.0086329  1.743781 0.3306755 2.9286777
# seasonspring              0.9933766  1.332027 0.5818194 1.7668275
# seasonsummer              1.1768191  1.422466 0.6079952 2.3580015
# seasonwinter              1.0786291  1.321360 0.6353519 1.8733183
# speciesGRETI:seasonspring 0.9444525  1.392729 0.4922419 1.7476908
# speciesMARTI:seasonspring 0.9593154  1.518831 0.4100664 2.1587360
# speciesNUTHA:seasonspring 0.7290469  2.055136 0.1754964 2.9561889
# speciesGRETI:seasonsummer 0.7896398  1.468715 0.3675647 1.6659568
# speciesMARTI:seasonsummer 0.7977774  1.645898 0.2903382 2.0618390
# speciesNUTHA:seasonsummer 0.7283165  1.891571 0.2003768 2.5111731
# speciesGRETI:seasonwinter 0.8780037  1.368116 0.4686558 1.5949964
# speciesMARTI:seasonwinter 0.9344952  1.486293 0.4224911 2.0051520
# speciesNUTHA:seasonwinter 0.7547378  1.822977 0.2371624 2.4633145


# Compute marginal means for the species variable
feeders_species_emm <- emmeans(brm_visited_feeders, ~ species)
feeders_contrasts <- contrast(feeders_species_emm, method = "pairwise")
feeders_contrasts

feeders_species_season_emm <- emmeans(brm_visited_feeders, ~ species | season)
feeders_species_season_contrasts <- contrast(feeders_species_season_emm, method = "pairwise")
feeders_species_season_contrasts
# How to interpret:
# estimate: difference in marginal means
# if the credible interval does not include 0, two groups are considered significantly different
# Plot the marginal means for species
plot(feeders_species_emm)
# Plot pairwise comparisons for species across seasons
plot(feeders_species_season_emm)
# Extract contrasts as a data frame
feeders_species_season_contrasts_df <- as.data.frame(feeders_species_season_contrasts)
head(feeders_species_season_contrasts_df)
# you get from log odds to odds by expontiating:
exp(feeders_species_season_contrasts_df$estimate) #thus odds are the estimates

feeders_species_season_contrasts_df$odds <- exp(feeders_species_season_contrasts_df$estimate)
library(dplyr)
feeders_emm_table <- feeders_species_season_contrasts_df %>%
  select(contrast, season, estimate, lower.HPD, upper.HPD, odds) #I make a table so I can make the calculations
#the percentage
feeders_emm_table$prob_emm <- (exp(feeders_emm_table$estimate) / (1 + exp(feeders_emm_table$estimate)))*100

#extract the dataframe into a table for Word
library(flextable)

pairwise_table_visited_feeders <- flextable(feeders_emm_table) %>%
  autofit() %>%
  set_caption("Pairwise Contrasts of the number of visited feeders of Species by Season ")

# Export to Word (optional)
library(officer)
doc_pariwise_comparisons_feeders <- read_docx() %>%
  body_add_flextable(pairwise_table_visited_feeders)

print(doc_pariwise_comparisons_feeders, target = "species_season_feeders.docx")


# 4) Descriptive stats ----------------------------------------------------
#for the leader-follower status
View(network.pos.all.seasons)
network.pos.all.seasons$species <- as.factor(network.pos.all.seasons$species)
nbr_species <- network.pos.all.seasons %>%
  distinct(PIT, species, age_in_2020) %>% # Keep only unique combinations of ID and species
  group_by(species, age_in_2020) %>%
  summarise(count = n()) 
66+144+15+9


#for visitation data (this includes groups of 1)
View(visits_all_season)
visits_all_season$species <- as.factor(visits_all_seasons$species)
nbr_species_visits <- visits_all_season %>%
  distinct(PIT, species) %>% # Keep only unique combinations of ID and species
  group_by(species) %>%
  summarise(count = n())
#not the same amount of individuals for this part