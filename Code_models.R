# elephant: write the title of the paper and authors



# 1) Data prep ------------------------------------------------------------


# 1.1) Load libraries -----------------------------------------------------


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


# 1.2) Extract order of arrival at feeders --------------------------------

# Note that you can load the input data directly here, but we also provide code to replicate the data extraction from the raw data here (network_data_prep.R)


# 1.3) Load data ----------------------------------------------------------

# this contains the complete data set for analysis the leader follower dynamics
network.pos.all.seasons <- read.csv("data/leader_follower_data_all_seasons.csv", row.names = 1)
network.pos.all.seasons$leader.follower <- as.factor(network.pos.all.seasons$leader.follower)
head(network.pos.all.seasons)

# elephant: please add some details on what the different columns are

# PIT: 	
# degree	
# Date.Time	
# Antenna	
# location	
# week	
# group	
# leader.follower	
# species	
# age_in_2020	
# season	
# betweenness	
# group.size


# 2) Follower-leader dynamics ---------------------------------------------


# 2.1) Calculate VIFs -----------------------------------------------------


library(lme4)
# we first run a simple regression (no interactions) to look at collinearity between predictors
model_order_vif <- glmer(leader.follower ~  species + season + age_in_2020 + scale(degree) + scale(betweenness) + group.size + (1|PIT), family= binomial, data= network.pos.all.seasons)

summary(model_order_vif)

library(car)
vif(model_order_vif)
# GVIF Df GVIF^(1/(2*Df))
# species            1.378070  3        1.054901
# season             2.693207  3        1.179537
# age_in_2020        1.132574  1        1.064225
# scale(degree)      2.834961  1        1.683734
# scale(betweenness) 1.298273  1        1.139418
# group.size         1.150594  1        1.072658

# all vifs <5, so we can include all of them


# 2.2) Global model -------------------------------------------------------


# we run a model with leader follower (1/0) as explanatory variable
library(brms)
glob_model <- brm(
  leader.follower ~  species * season + species * age_in_2020 + scale(betweenness) *
    season + scale(betweenness) *
    species + scale(betweenness) * age_in_2020 + scale(degree) *
    season + scale(degree) *
    species + scale(degree) * age_in_2020 + group.size * species + group.size *
    age_in_2020 + group.size * season + (1 | PIT),
  family = bernoulli, 
  data = network.pos.all.seasons,
  chains = 2,
  iter = 4000,
  cores = 4,
  save_pars = save_pars(all = TRUE)
)

#save(glob_model, file="model output/glob_model.RDA")
load("model output/glob_model.RDA")

summary(glob_model)

# Family: bernoulli 
# Links: mu = logit 
# Formula: leader.follower ~ species * season + species * age_in_2020 + scale(betweenness) * season + scale(betweenness) * species + scale(betweenness) * age_in_2020 + scale(degree) * season + scale(degree) * species + scale(degree) * age_in_2020 + group.size * species + group.size * age_in_2020 + group.size * season + (1 | PIT) 
# Data: network.pos.all.seasons (Number of observations: 27163) 
# Draws: 2 chains, each with iter = 4000; warmup = 2000; thin = 1;
# total post-warmup draws = 4000
# 
# Multilevel Hyperparameters:
#   ~PIT (Number of levels: 234) 
# Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sd(Intercept)     0.29      0.03     0.24     0.34 1.00     2153     3150
# 
# Regression Coefficients:
#   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept                                1.98      0.31     1.36     2.61 1.00     1631     2237
# speciesGRETI                            -0.23      0.22    -0.67     0.21 1.00     2082     2861
# speciesMARTI                             0.37      0.23    -0.09     0.84 1.00     1939     2697
# speciesNUTHA                             1.30      0.34     0.66     1.96 1.00     2356     2979
# seasonspring                            -0.61      0.31    -1.21     0.01 1.00     1746     2494
# seasonsummer                            -0.50      0.37    -1.22     0.23 1.00     2135     3002
# seasonwinter                            -1.91      0.31    -2.50    -1.28 1.00     1661     2510
# age_in_2020juvenile                     -0.12      0.14    -0.39     0.14 1.00     3372     3285
# scalebetweenness                        -0.01      0.17    -0.35     0.32 1.00     1921     2414
# scaledegree                              0.15      0.26    -0.36     0.65 1.00     1818     2262
# group.size                              -0.69      0.04    -0.76    -0.62 1.00     3336     2682
# speciesGRETI:seasonspring                0.11      0.18    -0.25     0.46 1.00     2241     3079
# speciesMARTI:seasonspring               -0.26      0.22    -0.70     0.19 1.00     2102     2829
# speciesNUTHA:seasonspring               -0.76      0.31    -1.34    -0.17 1.00     3156     3049
# speciesGRETI:seasonsummer                0.51      0.26     0.00     1.02 1.00     3038     2865
# speciesMARTI:seasonsummer                0.14      0.28    -0.42     0.69 1.00     3031     3099
# speciesNUTHA:seasonsummer               -0.10      0.33    -0.74     0.55 1.00     3371     3175
# speciesGRETI:seasonwinter                0.08      0.22    -0.35     0.50 1.00     2102     2637
# speciesMARTI:seasonwinter                0.28      0.23    -0.19     0.73 1.00     2329     2915
# speciesNUTHA:seasonwinter               -0.07      0.31    -0.69     0.54 1.00     2323     2815
# speciesGRETI:age_in_2020juvenile        -0.09      0.14    -0.37     0.19 1.00     3465     3251
# speciesMARTI:age_in_2020juvenile        -0.02      0.35    -0.70     0.67 1.00     4269     3100
# speciesNUTHA:age_in_2020juvenile        -0.02      0.31    -0.63     0.59 1.00     3342     3016
# seasonspring:scalebetweenness            0.07      0.14    -0.20     0.34 1.00     2246     2658
# seasonsummer:scalebetweenness            0.36      0.18     0.01     0.71 1.00     3253     2834
# seasonwinter:scalebetweenness            0.23      0.11     0.01     0.46 1.00     2350     3128
# speciesGRETI:scalebetweenness           -0.18      0.15    -0.47     0.10 1.00     3190     3007
# speciesMARTI:scalebetweenness           -0.01      0.13    -0.26     0.24 1.00     2703     2984
# speciesNUTHA:scalebetweenness           -0.23      0.20    -0.62     0.17 1.00     2790     3023
# age_in_2020juvenile:scalebetweenness    -0.12      0.08    -0.27     0.02 1.00     5233     3398
# seasonspring:scaledegree                -0.33      0.25    -0.82     0.18 1.00     1911     2581
# seasonsummer:scaledegree                -0.25      0.27    -0.78     0.29 1.00     1988     2563
# seasonwinter:scaledegree                -0.28      0.25    -0.75     0.23 1.00     1863     2259
# speciesGRETI:scaledegree                 0.13      0.09    -0.06     0.31 1.00     2993     3259
# speciesMARTI:scaledegree                 0.03      0.11    -0.19     0.25 1.00     3599     3344
# speciesNUTHA:scaledegree                 0.25      0.21    -0.14     0.65 1.00     4051     3171
# age_in_2020juvenile:scaledegree         -0.05      0.06    -0.17     0.06 1.00     6169     3638
# speciesGRETI:group.size                  0.02      0.02    -0.01     0.05 1.00     6909     3293
# speciesMARTI:group.size                 -0.11      0.02    -0.15    -0.06 1.00     6302     3240
# speciesNUTHA:group.size                 -0.25      0.04    -0.32    -0.18 1.00     6442     3323
# age_in_2020juvenile:group.size           0.05      0.01     0.02     0.08 1.00     7724     3163
# seasonspring:group.size                  0.19      0.04     0.12     0.26 1.00     3294     2910
# seasonsummer:group.size                  0.05      0.04    -0.03     0.13 1.00     3613     3025
# seasonwinter:group.size                  0.46      0.03     0.40     0.53 1.00     3295     3285
# 
# Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
# and Tail_ESS are effective sample size measures, and Rhat is the potential
# scale reduction factor on split chains (at convergence, Rhat = 1).

# remove non-significant interactions

#age.degree
#species.degree
#season.degree
#age.between
#species.between
#species.age


# 2.3) Reduced model 1 ----------------------------------------------------

red_model <- brm(
  leader.follower ~  species * season + scale(betweenness) *
    season +  scale(degree) + group.size * species + group.size *
    age_in_2020 + group.size * season + (1 | PIT),
  family = bernoulli, 
  data = network.pos.all.seasons,
  chains = 2,
  iter = 4000,
  cores = 4,
  save_pars = save_pars(all = TRUE)
)

#save(red_model, file="model output/red_model.RDA")
load("model output/red_model.RDA")

summary(red_model)

# Family: bernoulli 
# Links: mu = logit 
# Formula: leader.follower ~ species * season + scale(betweenness) * season + scale(degree) + group.size * species + group.size * age_in_2020 + group.size * season + (1 | PIT) 
# Data: network.pos.all.seasons (Number of observations: 27163) 
# Draws: 2 chains, each with iter = 4000; warmup = 2000; thin = 1;
# total post-warmup draws = 4000
# 
# Multilevel Hyperparameters:
#   ~PIT (Number of levels: 234) 
# Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sd(Intercept)     0.28      0.03     0.23     0.33 1.00     1740     2300
# 
# Regression Coefficients:
#   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept                          1.76      0.17     1.42     2.10 1.00      962     1713
# speciesGRETI                      -0.29      0.16    -0.60     0.04 1.00     1358     1981
# speciesMARTI                       0.31      0.19    -0.07     0.67 1.00     1046     1694
# speciesNUTHA                       0.99      0.25     0.50     1.47 1.00     1283     2038
# seasonspring                      -0.32      0.18    -0.68     0.03 1.00      980     2081
# seasonsummer                      -0.32      0.26    -0.84     0.20 1.00     1390     1984
# seasonwinter                      -1.76      0.18    -2.13    -1.40 1.00     1013     1714
# scalebetweenness                  -0.05      0.08    -0.20     0.12 1.00     1457     2444
# scaledegree                       -0.08      0.04    -0.15    -0.01 1.00     2689     2894
# group.size                        -0.69      0.04    -0.76    -0.62 1.00     1901     2592
# age_in_2020juvenile               -0.13      0.08    -0.28     0.03 1.00     2879     2457
# speciesGRETI:seasonspring          0.12      0.17    -0.20     0.45 1.00     1248     1862
# speciesMARTI:seasonspring         -0.16      0.19    -0.53     0.22 1.00     1102     2044
# speciesNUTHA:seasonspring         -0.52      0.28    -1.07     0.03 1.00     1399     2156
# speciesGRETI:seasonsummer          0.54      0.25     0.06     1.03 1.00     1639     2575
# speciesMARTI:seasonsummer          0.24      0.28    -0.30     0.77 1.00     1425     2388
# speciesNUTHA:seasonsummer          0.02      0.29    -0.56     0.59 1.00     1382     2034
# speciesGRETI:seasonwinter          0.22      0.17    -0.11     0.55 1.00     1368     2154
# speciesMARTI:seasonwinter          0.45      0.18     0.09     0.82 1.00     1119     1689
# speciesNUTHA:seasonwinter          0.27      0.24    -0.21     0.74 1.00     1174     2114
# seasonspring:scalebetweenness      0.02      0.11    -0.19     0.23 1.00     1838     2975
# seasonsummer:scalebetweenness      0.24      0.14    -0.02     0.50 1.00     2900     2635
# seasonwinter:scalebetweenness      0.15      0.09    -0.01     0.32 1.00     1428     2250
# speciesGRETI:group.size            0.02      0.02    -0.01     0.06 1.00     3781     2608
# speciesMARTI:group.size           -0.11      0.02    -0.15    -0.06 1.00     3933     3532
# speciesNUTHA:group.size           -0.24      0.03    -0.31    -0.18 1.00     4399     3142
# group.size:age_in_2020juvenile     0.04      0.01     0.02     0.07 1.00     4587     3179
# seasonspring:group.size            0.18      0.04     0.11     0.26 1.00     1992     2816
# seasonsummer:group.size            0.05      0.04    -0.03     0.13 1.00     2362     2934
# seasonwinter:group.size            0.46      0.03     0.40     0.53 1.00     1957     2383
# 
# Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
# and Tail_ESS are effective sample size measures, and Rhat is the potential
# scale reduction factor on split chains (at convergence, Rhat = 1).




# 2.4) Reduced model 2 (final) ----------------------------------------------------


# remove non-significant interaction: season-betweenness (newly non-significant)

red_model2 <- brm(
  leader.follower ~  species * season + scale(betweenness) + scale(degree) + group.size * species + group.size *
    age_in_2020 + group.size * season + (1 | PIT),
  family = bernoulli, 
  data = network.pos.all.seasons,
  chains = 2,
  iter = 4000,
  cores = 4,
  save_pars = save_pars(all = TRUE)
)

#save(red_model2, file="model output/red_model2.RDA")
load("model output/red_model2.RDA")


summary(red_model2)

# Family: bernoulli 
# Links: mu = logit 
# Formula: leader.follower ~ species * season + scale(betweenness) + scale(degree) + group.size * species + group.size * age_in_2020 + group.size * season + (1 | PIT) 
# Data: network.pos.all.seasons (Number of observations: 27163) 
# Draws: 2 chains, each with iter = 4000; warmup = 2000; thin = 1;
# total post-warmup draws = 4000
# 
# Multilevel Hyperparameters:
#   ~PIT (Number of levels: 234) 
# Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sd(Intercept)     0.27      0.02     0.23     0.32 1.00     2089     2949
# 
# Regression Coefficients:
#   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept                          1.84      0.18     1.50     2.20 1.00     1266     1975
# speciesGRETI                      -0.31      0.16    -0.63     0.02 1.00     1445     2190
# speciesMARTI                       0.18      0.19    -0.18     0.56 1.00     1378     2215
# speciesNUTHA                       0.83      0.23     0.38     1.29 1.00     1679     2584
# seasonspring                      -0.40      0.18    -0.76    -0.06 1.00     1275     2311
# seasonsummer                      -0.47      0.26    -0.98     0.03 1.00     1572     2309
# seasonwinter                      -1.87      0.19    -2.23    -1.51 1.00     1345     2232
# scalebetweenness                   0.07      0.03     0.01     0.13 1.00     4517     3740
# scaledegree                       -0.07      0.04    -0.14     0.00 1.00     3702     3068
# group.size                        -0.69      0.04    -0.76    -0.62 1.00     2095     2434
# age_in_2020juvenile               -0.12      0.08    -0.28     0.04 1.00     3225     3371
# speciesGRETI:seasonspring          0.16      0.17    -0.17     0.49 1.00     1467     2151
# speciesMARTI:seasonspring         -0.10      0.18    -0.46     0.26 1.00     1461     2387
# speciesNUTHA:seasonspring         -0.29      0.25    -0.78     0.21 1.00     2138     2690
# speciesGRETI:seasonsummer          0.57      0.25     0.08     1.06 1.00     1825     2791
# speciesMARTI:seasonsummer          0.45      0.26    -0.06     0.97 1.00     1739     2280
# speciesNUTHA:seasonsummer          0.21      0.28    -0.33     0.74 1.00     1866     2827
# speciesGRETI:seasonwinter          0.25      0.17    -0.08     0.59 1.00     1509     2339
# speciesMARTI:seasonwinter          0.63      0.18     0.28     0.96 1.00     1514     2500
# speciesNUTHA:seasonwinter          0.49      0.21     0.08     0.91 1.00     1742     2692
# speciesGRETI:group.size            0.02      0.02    -0.01     0.05 1.00     4614     3518
# speciesMARTI:group.size           -0.10      0.02    -0.15    -0.06 1.00     5004     3439
# speciesNUTHA:group.size           -0.25      0.03    -0.31    -0.18 1.00     5307     3190
# group.size:age_in_2020juvenile     0.04      0.01     0.02     0.07 1.00     5297     2942
# seasonspring:group.size            0.19      0.04     0.11     0.26 1.00     2046     2503
# seasonsummer:group.size            0.05      0.04    -0.02     0.14 1.00     2427     2773
# seasonwinter:group.size            0.47      0.03     0.40     0.53 1.00     2045     2679
# 
# Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
# and Tail_ESS are effective sample size measures, and Rhat is the potential
# scale reduction factor on split chains (at convergence, Rhat = 1).


# 2.5) Model checks -------------------------------------------------------


# check how well it fits
pp_check(red_model2, ndraws = 100)
# looks like a very good fit
# the asymmetry means that birds are more likely to be followers (0) than leaders (1), which makes sense

plot(red_model2)
# stationarity and mixing are good

# have a first look at the plot (conditional effects)
red_model_conditional_effects <- plot(conditional_effects(red_model2, re_formula = NULL))
# a 0 here means to be a leader 

library(performance)
# extract an R2 for our model
R2 <- performance::r2_bayes(red_model2)
# Bayesian R2 with compatibility Interval
# 
# Conditional R2: 0.279 (95% CI [0.271, 0.286])
# Marginal R2: 0.266 (95% CI [0.255, 0.277])

# 2.6) Summary table ------------------------------------------------------


##make a table with the summary
summary_red_model <- summary(red_model2)
fixed_effects <- as.data.frame(summary_red_model$fixed)
fixed_effects <- tibble::rownames_to_column(fixed_effects, var = "Term")


library(flextable)

flextable_table <- fixed_effects %>%
  flextable() %>%
  autofit() %>%
  set_caption("Summary of Fixed Effects from the final Model")

library(officer)

doc_red_model <- read_docx() %>%
  body_add_flextable(flextable_table)

print(doc_red_model, target = "fixed_effects_final_red_model.docx")


# 2.7) Plot ---------------------------------------------------------------

##plot of the summary output of the red_model2
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
  
  red_model_conditional_effects$`group.size:species` +
    labs(x= "Flock size", y= "Leader status") +
    ylim(c(0.0, 0.9))+
    theme_bw() +
    theme(legend.background=element_blank()) +
    theme(legend.title=element_blank(), legend.position = c(0.78, 0.6), axis.title.x = element_blank())+
    scale_fill_manual(values = my_colors_sp, breaks=c("BLUTI", "GRETI", "MARTI", "NUTHA"), labels=c("Blue tits", "Great tits", "Marsh tits", "Nuthaches"))+
    scale_color_manual(values = my_colors_sp, breaks=c("BLUTI", "GRETI", "MARTI", "NUTHA"), labels=c("Blue tits", "Great tits", "Marsh tits", "Nuthaches")),
  
  
  red_model_conditional_effects$degree +
    labs(x= "Degree centrality", y= "Leader status") +
    labs(y= "") +
    ylim(c(0.0, 0.9))+
    theme_bw(),
  
  red_model_conditional_effects$`group.size:age_in_2020` +
    labs(x= "Flock size", y= "Leader status") +
    labs(y= "Leader status") +
    ylim(c(0.0, 0.9))+
    theme_bw() +
    theme(legend.background=element_blank())+
    theme(legend.title=element_blank(), legend.position = c(0.78, 0.6), axis.title.x = element_blank())+
    scale_fill_manual(values = my_colors_age, breaks=c("adult", "juvenile"), labels=c("Adults", "Juveniles"))+
    scale_color_manual(values = my_colors_age, breaks=c("adult", "juvenile"), labels=c("Adults", "Juveniles")),
  
  red_model_conditional_effects$betweenness +
    labs(x= "Betweenness centrality", y= "Leader status") +
    labs(y= "") +
    ylim(c(0.0, 0.9))+
    theme_bw(),
  
  red_model_conditional_effects$`group.size:season`+
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
exp(fixef(red_model2))
# Estimate Est.Error      Q2.5     Q97.5
# Intercept                      6.3239835  1.192878 4.4787364 9.0006652
# speciesGRETI                   0.7338831  1.179382 0.5301639 1.0156655
# speciesMARTI                   1.2026457  1.204426 0.8346520 1.7495710
# speciesNUTHA                   2.2879350  1.260975 1.4579951 3.6375686
# seasonspring                   0.6706234  1.199776 0.4689009 0.9427827
# seasonsummer                   0.6271902  1.298399 0.3751890 1.0345818
# seasonwinter                   0.1545513  1.204639 0.1079162 0.2203257
# scalebetweenness               1.0705637  1.031490 1.0073222 1.1372261
# scaledegree                    0.9337518  1.036721 0.8699556 1.0010344
# group.size                     0.4999070  1.036726 0.4656332 0.5356519
# age_in_2020juvenile            0.8830218  1.085432 0.7534679 1.0363919
# speciesGRETI:seasonspring      1.1739184  1.184909 0.8408276 1.6243354
# speciesMARTI:seasonspring      0.9003300  1.202716 0.6318563 1.2937922
# speciesNUTHA:seasonspring      0.7497414  1.288641 0.4565293 1.2279248
# speciesGRETI:seasonsummer      1.7648495  1.284446 1.0837587 2.8720890
# speciesMARTI:seasonsummer      1.5702663  1.296891 0.9373157 2.6334541
# speciesNUTHA:seasonsummer      1.2278381  1.325672 0.7157301 2.0945956
# speciesGRETI:seasonwinter      1.2807651  1.186986 0.9190505 1.8083172
# speciesMARTI:seasonwinter      1.8700747  1.191873 1.3257187 2.6149260
# speciesNUTHA:seasonwinter      1.6372127  1.238503 1.0807294 2.4892476
# speciesGRETI:group.size        1.0234942  1.015639 0.9929726 1.0554525
# speciesMARTI:group.size        0.9005732  1.021714 0.8645055 0.9393124
# speciesNUTHA:group.size        0.7808748  1.034884 0.7299535 0.8342891
# group.size:age_in_2020juvenile 1.0445691  1.012149 1.0205119 1.0698306
# seasonspring:group.size        1.2059864  1.038425 1.1217046 1.2981832
# seasonsummer:group.size        1.0557725  1.042188 0.9764024 1.1460845
# seasonwinter:group.size        1.5931767  1.034685 1.4921505 1.7046317


# in our model, blue tits are the baseline species to which all others are compared. We would like to compute comparisons between all species. 

library(emmeans)

# Compute marginal means for the species variable
species_emm <- emmeans(red_model, ~ species)
# Pairwise comparisons between species levels
species_contrasts <- contrast(species_emm, method = "pairwise")
species_contrasts
# Marginal means for species by season
species_season_emm <- emmeans(red_model2, ~ species | season)
# Pairwise comparisons within each season
species_season_contrasts <- contrast(species_season_emm, method = "pairwise")
species_season_contrasts

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
library(dplyr)
emm_table <- species_season_contrasts_df %>%
  dplyr::select(contrast, season, estimate, lower.HPD, upper.HPD, odds) #I make a table so I can make the calculations
#the percentage
emm_table$prob_emm <- (exp(emm_table$estimate) / (1 + exp(emm_table$estimate)))*100

#extract the dataframe into a table for Word
library(flextable)

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

# we can use the function icc (intra class correlation coefficient) from package performance
performance::icc(red_model2)

# # Intraclass Correlation Coefficient
# 
# Adjusted ICC: 0.022
# Unadjusted ICC: 0.012 

# looks like there is very low repeatability within individuals

# # here two alternative approaches:
# # Extract variance components
# var_components <- VarCorr(red_model2)
# print(var_components)
# 
# 
# # Extract the variance for the random effect PIT (squaring the standard deviation)
# pit_var <- as.numeric(var_components$PIT$sd[1])^2
# 
# # Residual variance for Bernoulli-logit model
# residual_variance <- pi^2 / 3
# 
# # Calculate repeatability
# repeatability <- pit_var / (pit_var + residual_variance)
# repeatability
# 
# # [1] 0.0219684
# 
# # same result as above!
# 
# 
# Extract the variance components and calculate repeatability
# other method, same value
# library(tidybayes)
# get_variables(red_model2)#This just tells us what parameters we can pull from the model
# #Extract the variance components
# post.data = red_model2 %>% spread_draws(sd_PIT__Intercept) 
# #Among-individual variance
# post.data$Va_PIT = post.data$sd_PIT__Intercept^2 # we square it because it is currently a standard deviation, and by squaring this value we turn it into a variance
# 
# #Within-individual variance 
# #For Bernouilli: sigma^2 = pi^2/3
# sigma_sq = (pi^2)/3
# 
# #Repeatability
# repeatability <- post.data$Va_PIT / (post.data$Va_PIT + sigma_sq)
# repeatability
# hist(repeatability)
# mean(repeatability) #same as above, with the other methods
# #0.02213161
# 
# #Calculate CI
# rethinking::HPDI(repeatability, prob = 0.95)
# # |0.95      0.95| 
# #0.01509294 0.03026423 
# #Our value does fall within the IC
# 
# #This last part was inspired by the study: https://datadryad.org/stash/dataset/doi:10.25338/B88P8W


# 3) Visitation rates ------------------------------------------------------

# here we look at potential caching behaviour of different species assuming that for caching, they would be flying back and forth more often
visits_all_season <- read.csv("data/visits_all_season.csv", row.names = 1)

colnames(visits_all_season)
# elephant: please list and explain the column names


#how many flocks there are in each season?
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
  
  red_model_conditional_effects$`species:season` +
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


#for visitation data
View(visits_all_season)
visits_all_season$species <- as.factor(visits_all_seasons$species)
nbr_species_visits <- visits_all_season %>%
  distinct(PIT, species) %>% # Keep only unique combinations of ID and species
  group_by(species) %>%
  summarise(count = n())
#not the same amount of individuals for this part