

# SW: here you could do those extra couple of analyses with the visitation data:

visits_all_season <- read.csv("data/visits_all_season.csv", row.names = 1)


# 1) visits ~ species*season + (1|PIT)
# this should tell use something about caching - caching species are expected to fly back and forth more often within one flock visit
head(visits_all_season) #group=flock
#visits are counts thus we consider a Poisson distribution
library(lme4)
glmer_visits <- glmer(visit ~ species*season + (1|PIT), family = poisson, data= visits_all_season) #the birds go eat in flocks, hence their PIT is nested in group
# SW: hmm I see the thought why you want to include group as a random effect, but the group as we use it is essentially a random integer - I think I would run the model without it.


summary(glmer_visits)#model failed to converge

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
# Data: visits_all_season (Number of observations: 26819) 
# Draws: 2 chains, each with iter = 4000; warmup = 2000; thin = 1;
# total post-warmup draws = 4000
# 
# Multilevel Hyperparameters:
#   ~PIT (Number of levels: 254) 
# Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sd(Intercept)     0.11      0.01     0.10     0.13 1.00     1331     2220
# 
# Regression Coefficients:
#   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept                     0.52      0.05     0.43     0.61 1.01      435      980
# speciesGRETI                 -0.06      0.05    -0.16     0.05 1.01      469      950
# speciesMARTI                  0.09      0.06    -0.02     0.21 1.01      458     1149
# speciesNUTHA                 -0.09      0.07    -0.23     0.05 1.01      572     1325
# seasonspring                 -0.20      0.05    -0.29    -0.11 1.01      460     1069
# seasonsummer                  0.03      0.07    -0.11     0.17 1.00      801     1058
# seasonwinter                 -0.30      0.05    -0.39    -0.21 1.01      470     1157
# speciesGRETI:seasonspring     0.08      0.06    -0.02     0.19 1.01      494     1263
# speciesMARTI:seasonspring     0.16      0.06     0.05     0.28 1.01      590     1591
# speciesNUTHA:seasonspring     0.24      0.08     0.07     0.40 1.00      858     1983
# speciesGRETI:seasonsummer     0.13      0.08    -0.02     0.28 1.00      733     1260
# speciesMARTI:seasonsummer    -0.01      0.08    -0.18     0.15 1.00      870     1601
# speciesNUTHA:seasonsummer    -0.12      0.09    -0.29     0.04 1.00      885     1316
# speciesGRETI:seasonwinter     0.04      0.05    -0.07     0.14 1.01      473     1131
# speciesMARTI:seasonwinter     0.18      0.05     0.07     0.28 1.00      526     1482
# speciesNUTHA:seasonwinter     0.25      0.07     0.12     0.38 1.00      655     1802
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
# 9.689053
# our dispersion ratio indicates that the variance is greater than the mean -> overdispersion


# meaning we can go for a negative binomial model instead
# I had divergent transitions, so we're setting weakly inforative priors and set the adapt delta higher

brm_visits_2 <- brm(
  visit ~ species*season + (1|PIT),
  family = negbinomial(),
  data = visits_all_season,
  chains = 2,
  prior=   c(prior(normal(0, 2), class= Intercept),
             prior(normal(0,  5), class= b)),
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