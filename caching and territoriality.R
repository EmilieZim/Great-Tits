

# SW: here you could do those extra couple of analyses with the visitation data:

visits_all_season <- read.csv("data/visits_all_season.csv")
# 1) visits ~ species*season + (1|PIT)
# this should tell use something about caching - caching species are expected to fly back and forth more often within one flock visit
head(visits_all_season) #group=flock
#visits are counts thus we consider a Poisson distribution
library(lme4)
glmer_visits <- glmer(visit ~ species*season + (1|PIT), family = poisson, data= visits_all_season) #the birds go eat in flocks, hence their PIT is nested in group
# SW: hmm I see the thought why you want to include group as a random effect, but the group as we use it is essentially a random integer - I think I would run the model without it.


summary(glmer_visits)#model failed to converge


# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: poisson  ( log )
# Formula: visit ~ species * season + (1 | PIT)
# Data: visits_all_season
# 
# AIC      BIC   logLik deviance df.resid 
# 116892.9 117039.6 -58429.4 116858.9    41512 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -1.1920 -0.4867 -0.2340  0.2246  9.6735 
# 
# Random effects:
#   Groups Name        Variance Std.Dev.
# PIT    (Intercept) 0.01886  0.1373  
# Number of obs: 41529, groups:  PIT, 254
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                0.57636    0.03793  15.194  < 2e-16 ***
#   speciesGRETI              -0.12612    0.04478  -2.817 0.004854 ** 
#   speciesMARTI               0.02299    0.05524   0.416 0.677315    
# speciesNUTHA              -0.12235    0.06718  -1.821 0.068586 .  
# seasonspring              -0.24452    0.03798  -6.439 1.20e-10 ***
#   seasonsummer               0.10090    0.05410   1.865 0.062197 .  
# seasonwinter              -0.34759    0.03663  -9.489  < 2e-16 ***
#   speciesGRETI:seasonspring  0.13216    0.04540   2.911 0.003600 ** 
#   speciesMARTI:seasonspring  0.18981    0.04635   4.096 4.21e-05 ***
#   speciesNUTHA:seasonspring  0.24407    0.06412   3.807 0.000141 ***
#   speciesGRETI:seasonsummer  0.10800    0.05876   1.838 0.066050 .  
# speciesMARTI:seasonsummer -0.06689    0.05932  -1.128 0.259482    
# speciesNUTHA:seasonsummer -0.20713    0.06468  -3.203 0.001362 ** 
#   speciesGRETI:seasonwinter  0.10933    0.04326   2.527 0.011503 *  
#   speciesMARTI:seasonwinter  0.26787    0.04124   6.496 8.27e-11 ***
#   speciesNUTHA:seasonwinter  0.32256    0.05149   6.265 3.73e-10 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation matrix not shown by default, as p = 16 > 12.
# Use print(x, correlation=TRUE)  or
# vcov(x)        if you need it
# 
# optimizer (Nelder_Mead) convergence code: 0 (OK)
# Model failed to converge with max|grad| = 0.0116038 (tol = 0.002, component 1)

#with brm:
library(brms)
brm_visits <- brm(
  visit ~ species*season + (1|PIT),
  family = poisson,
  data = visits_all_season,
  chains = 2,
  iter = 4000,
  cores = 4
)

summary(brm_visits)


# Family: poisson 
# Links: mu = log 
# Formula: visit ~ species * season + (1 | PIT) 
# Data: visits_all_season (Number of observations: 41529) 
# Draws: 2 chains, each with iter = 4000; warmup = 2000; thin = 1;
# total post-warmup draws = 4000
# 
# Multilevel Hyperparameters:
#   ~PIT (Number of levels: 254) 
# Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sd(Intercept)     0.14      0.01     0.12     0.16 1.00      777     1636
# 
# Regression Coefficients:
#   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept                     0.57      0.04     0.50     0.65 1.00      508     1168
# speciesGRETI                 -0.12      0.05    -0.22    -0.03 1.00      475     1264
# speciesMARTI                  0.02      0.06    -0.08     0.14 1.00      448      948
# speciesNUTHA                 -0.12      0.07    -0.26     0.02 1.00      653     1420
# seasonspring                 -0.24      0.04    -0.32    -0.17 1.00      551     1001
# seasonsummer                  0.10      0.05    -0.01     0.21 1.00      683     1040
# seasonwinter                 -0.35      0.04    -0.42    -0.27 1.00      517     1262
# speciesGRETI:seasonspring     0.13      0.05     0.04     0.23 1.00      619     1073
# speciesMARTI:seasonspring     0.19      0.05     0.10     0.28 1.00      657     1538
# speciesNUTHA:seasonspring     0.24      0.06     0.11     0.37 1.00     1025     2027
# speciesGRETI:seasonsummer     0.11      0.06    -0.00     0.23 1.00      720     1471
# speciesMARTI:seasonsummer    -0.06      0.06    -0.18     0.05 1.00      739     1475
# speciesNUTHA:seasonsummer    -0.21      0.06    -0.33    -0.07 1.00      826     1684
# speciesGRETI:seasonwinter     0.11      0.04     0.02     0.20 1.00      543     1237
# speciesMARTI:seasonwinter     0.27      0.04     0.18     0.35 1.00      545     1439
# speciesNUTHA:seasonwinter     0.32      0.05     0.22     0.42 1.00      798     1494
# 
# Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
# and Tail_ESS are effective sample size measures, and Rhat is the potential
# scale reduction factor on split chains (at convergence, Rhat = 1).

# check for over/under dispsersion

# Extract Pearson residuals from the model
residuals <- residuals(brm_visits, type = "pearson")

# Calculate the sum of squared Pearson residuals
sum_squared_residuals <- sum(residuals^2)

# Get the number of observations and the number of parameters in the model
n <- nrow(visits_all_season) # Number of data points
p <- length(fixef(brm_visits)) # Number of fixed effects in the model

# Compute the dispersion statistic
dispersion_ratio <- sum_squared_residuals / (n - p)
dispersion_ratio
# 9.97279
# our dispersion ratio indicates that the variance is greater than the mean -> overdispersion


# meaning we can go for a negative binomial model instead
# I had divergent transitions, so we're setting weakly inforative priors and set the adapt delta higher

brm_visits_2 <- brm(
  visit ~ species*season + (1|PIT),
  family = negbinomial(),
  data = visits_all_season,
  chains = 2,
  prior=   c(prior(normal(0, 1), class= Intercept),
             prior(normal(0,  2), class= b)),
  control = list(adapt_delta = .99), # set higher because of divergent transitions
  iter = 4000,
  cores = 4
)

summary(brm_visits_2)


pp_check(brm_visits_2, ndraws = 100)


plot(conditional_effects(brm_visits_2))
pp_check(brm_visits_2, ndraws = 100)


# SW: looks like there are some species/season differences!


# 2) number of feeders visited ~ species*season + (1|PIT)
# this will tell use about territoriality - the more territorial species should have fewer feeders that they use
# you will need to use this data frame:
network.pos.all.seasons
# the 'location' are the different feeders - so you will need to find a way to summarize it with columns:
# PIT, season, species, age, num.feeders.visited

head(network.pos.all.seasons)

library(dplyr)
subset_network.pos.all.seasons <- network.pos.all.seasons %>% select(PIT, season, species, age_in_2020, location)
#I want to add the column "num.feeders.visited" that contains the total number of feeders each bird visited
subset2_network.pos.all.seasons <- subset_network.pos.all.seasons %>%
  group_by(PIT) %>%
  mutate(num.feeders.visited = n_distinct(location)) %>%
  ungroup()

head(subset2_network.pos.all.seasons) #some rows are duplicates so I decide to use the function distinct() to eliminate this
subset2_network.pos.all.seasons <- subset2_network.pos.all.seasons %>%
  distinct()

hist(subset2_network.pos.all.seasons$num.feeders.visited) #poisson distribution
glmer_visited_feeders <- glmer(num.feeders.visited ~ species*season + (1|PIT), family = poisson, data= subset2_network.pos.all.seasons) #I no longer consider that the group is important for this question as the question is more linked to the territoriality of an individual (species) in general. What is the big picture of their territoriality?
summary(glmer_visited_feeders)
#the model failed to converge so I will try with brm

brm_visited_feeders <- brm(
  num.feeders.visited ~ species*season + (1|PIT),
  family = poisson,
  data =subset2_network.pos.all.seasons,
  chains = 2,
  iter = 4000,
  cores = 4,
  save_pars = save_pars(all = TRUE)
)

summary(brm_visited_feeders)


plot(conditional_effects(brm_visited_feeders))
pp_check(brm_visits, ndraws = 100)


# SW: to fix your issues with the pushing, perhaps move the image_final somewhere outside of the github repository folder. I'll do the same on my end. 
save.image(file="image_final.RData")
load(file="image_final.RData")