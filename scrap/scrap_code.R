# # 3) Territoriality  ------------------------------------------------------
network.pos.all.seasons
# the 'location' are the different feeders

head(network.pos.all.seasons)

subset_network.pos.all.seasons <- network.pos.all.seasons %>% select(PIT, season, species, age_in_2020, location)

library(dplyr)
library(tidyr)


# Step 1: Get all combinations of PIT, season, species, and feeder locations
all_combinations <- subset_network.pos.all.seasons %>%
  distinct(PIT, season, species) %>%
  crossing(location = paste0("Mill", 1:6))  # assuming Mill1–Mill6 are all feeders

# Step 2: Count actual visits
actual_visits <- subset_network.pos.all.seasons %>%
  group_by(PIT, season, species, location) %>%
  summarise(n_visits = n(), .groups = "drop")

# Step 3: Combine and fill missing visits with 0
visit_long <- all_combinations %>%
  left_join(actual_visits, by = c("PIT", "season", "species", "location")) %>%
  mutate(n_visits = replace_na(n_visits, 0))

# add total number of visits in a season
visit_long <- visit_long %>%
  group_by(PIT, season) %>%
  mutate(total_visits = sum(n_visits)) %>%
  ungroup()

# add proportion of visits to each feeder
visit_long$prop_feeder <- visit_long$n_visits/visit_long$total_visits

# Define Shannon entropy function
shannon_entropy <- function(p) {
  p <- p[p > 0]  # remove 0s to avoid log(0)
  -sum(p * log(p))
}

# Calculate entropy per PIT-season
entropy_df <- visit_long %>%
  group_by(PIT, season) %>%
  summarise(
    shannon_entropy = shannon_entropy(prop_feeder),
    .groups = "drop"
  )

entropy_df <- entropy_df %>%
  left_join(
    visit_long %>%
      distinct(PIT, species, season, total_visits),
    by = c("PIT", "season")
  )

ggplot(aes(x=season, y=shannon_entropy, color=species), data=entropy_df)+
  geom_boxplot()
# we run a model for territoriality

network.pos.all.seasons.mod <- network.pos.all.seasons

network.pos.all.seasons.mod <- network.pos.all.seasons.mod %>%
  left_join(
    entropy_df %>%
      distinct(PIT, species, season, shannon_entropy, total_visits),
    by = c("PIT", "season")
  )

network.pos.all.seasons.mod$flock_size_log <- log(network.pos.all.seasons.mod$flock_size+1)

glob_model_terr <- brm(
  leader.follower ~  shannon_entropy * season + total_visits*season + age_in_2020*season + scale(betweenness) + scale(degree) + offset(flock_size_log)  + (1 | PIT),
  family = bernoulli, 
  data = network.pos.all.seasons.mod,
  chains = 4,
  iter = 4000,
  prior = c(
    prior(normal(0, 0.5), class = "b", coef = "flock_size_log")  # weak effect
  ),
  cores = 4,
  save_pars = save_pars(all = TRUE)
)

plot(conditional_effects(glob_model_terr))



# we add a column num.visits.per.season with the number of total visits to any feeder in one season

visit_counts <- subset_network.pos.all.seasons %>%
  group_by(PIT, season) %>%
  summarise(
    n_visits = n(),
    n_feeders = n_distinct(location),  # change to your actual location column name
    species = first(species),
    age_in_2020 = first(age_in_2020),
    .groups = "drop"
  )



ggplot(aes(x=season, y=n_visits/n_feeders, col=species), data=visit_counts)+
  geom_boxplot()


# check for over/under dispsersion by dividing variance by mean

mean_visit <- mean(visit_counts$n_visits)
var_visit <- var(visit_counts$n_visits)
overdispersion_ratio <- var_visit / mean_visit
overdispersion_ratio
# [1] 58.51178
# over dispersed, using negative binomial


mean_feeder <- mean(visit_counts$n_feeders)
var_feeder <- var(visit_counts$n_feeders)
overdispersion_ratio <- var_feeder / mean_feeder
overdispersion_ratio
# [1] 0.3308824
# underdispersed so we are using com poisson

# multivariate model
visits_model <- brm(
  mvbind(n_visits, n_feeders) ~ species * season + (1 | PIT),
  data = visit_counts,
  family = list(negbinomial(), poisson()),
  chains = 4,
  iter = 4000,
  cores = 4,
  control = list(adapt_delta = 0.95)
)

# check for over/under dispsersion by dividing variance by mean

mean_visit <- mean(visit_counts$n_visits)
var_visit <- var(visit_counts$n_visits)
overdispersion_ratio <- var_visit / mean_visit
overdispersion_ratio
# [1] 58.51178

mean_visit <- mean(visit_counts$n_feeders)
var_visit <- var(visit_counts$n_visit)
overdispersion_ratio <- var_visit / mean_visit
overdispersion_ratio


summary(visits_model)


pp_check(visits_model, resp = "nvisits")
pp_check(visits_model, resp = "nfeeders")

# SW: can you compute the emmmeans comparison and create a table like you did with the other models?
emmeans(entropy_model, pairwise ~ species | season)  # compare species within season

# SW: can you create a plot similar to Figure 1a with season in the x axis and territoriality on the y axis
plot(conditional_effects(visits_model, effects = "season:species"))


for.pca <- cbind(visit_counts$n_visits, visit_counts$n_feeders)

pc1 <- princomp(for.pca, cor=T)
loadings(pc1)
summary(pc1)

biplot(pc1, pc.biplot=T)


pca <- pca(cbind.data.frame(visit_counts$n_visits, visit_counts$n_feeders), nfactors=1)


pca$loadings

pca$scores


visits_counts_mod <- cbind.data.frame(visit_counts, pca$scores)

ggplot(aes(x=season, y=PC1, col=species), data=visits_counts_mod)+
  geom_boxplot()




# scrap code:

# 3.1) visitation rates ----------------------------------------------------
# here we look at potential caching behaviour of different species assuming that for caching, they would be flying back and forth more often
visits_all_season <- read.delim("data/visits_all_season.txt", sep=" ")


colnames(visits_all_season)
#PIT: 10 digit alphanumeric code unique to each individual
#visit: the number of visits to the feeder of each bird in a given flock
#group: a unique numeric identifer of each group 
#season: the 4 studied seasons 
#visit.duration: the time spent on a feeder in seconds

# we add flock size:

library(dplyr)

n_flock <- visits_all_season %>%
  group_by(group) %>%
  summarise(flock_size = n_distinct(PIT), .groups = "drop")


# subset to those with flock size of min.2

visits_all_season <- merge(n_flock, visits_all_season, by="group")

visits_all_season <- subset(visits_all_season, visits_all_season$flock_size>=2)


# we first model visits ~ species*season + (1|PIT)
# this should tell use something about caching - caching species are expected to fly back and forth more often within one flock visit
head(visits_all_season) #group=flock
#visits are counts thus we consider a Poisson distribution


#with brm:
library(brms)
brm_visits <- brm(
  visit ~ species*season + (1 | group) + (1|PIT), # we include group as a random effect which compares the species within a given group to account for group level baseline differences. the way it is included, birds can be part of different groups. 
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
# Formula: visit ~ species * season + (1 | group) + (1 | PIT) 
# Data: visits_all_season (Number of observations: 24237) 
# Draws: 2 chains, each with iter = 4000; warmup = 2000; thin = 1;
# total post-warmup draws = 4000
# 
# Multilevel Hyperparameters:
#   ~group (Number of levels: 5846) 
# Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sd(Intercept)     0.01      0.01     0.00     0.03 1.00     1471     1457
# 
# ~PIT (Number of levels: 254) 
# Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sd(Intercept)     0.11      0.01     0.09     0.12 1.00     1651     2662
# 
# Regression Coefficients:
#   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept                     0.53      0.05     0.43     0.62 1.02      458      969
# speciesGRETI                 -0.04      0.06    -0.16     0.07 1.01      638     1355
# speciesMARTI                  0.08      0.06    -0.04     0.21 1.02      441     1282
# speciesNUTHA                 -0.07      0.07    -0.22     0.08 1.01      824     1187
# seasonspring                 -0.19      0.05    -0.29    -0.09 1.02      529     1205
# seasonsummer                  0.02      0.08    -0.14     0.18 1.01      861     1638
# seasonwinter                 -0.31      0.05    -0.40    -0.20 1.02      527     1128
# speciesGRETI:seasonspring     0.06      0.06    -0.06     0.17 1.01      702     1418
# speciesMARTI:seasonspring     0.15      0.07     0.02     0.28 1.01      855     1443
# speciesNUTHA:seasonspring     0.21      0.09     0.04     0.39 1.00     1286     2259
# speciesGRETI:seasonsummer     0.10      0.09    -0.07     0.27 1.01      878     1643
# speciesMARTI:seasonsummer     0.01      0.09    -0.17     0.19 1.01      958     1778
# speciesNUTHA:seasonsummer    -0.10      0.10    -0.29     0.09 1.01     1056     1743
# speciesGRETI:seasonwinter     0.03      0.06    -0.08     0.14 1.01      728     1542
# speciesMARTI:seasonwinter     0.19      0.06     0.08     0.30 1.01      591     1096
# speciesNUTHA:seasonwinter     0.20      0.07     0.05     0.34 1.01     1120     1579
# 
# Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
# and Tail_ESS are effective sample size measures, and Rhat is the potential
# scale reduction factor on split chains (at convergence, Rhat = 1).

# check for over/under dispsersion

mean_visit <- mean(visits_all_season$visit)
var_visit <- var(visits_all_season$visit)
overdispersion_ratio <- var_visit / mean_visit
overdispersion_ratio
# [1] 0.6864319

# seems okay - we can stick with a poisson

pp_check(brm_visits, ndraws = 100)

# plot
visits_conditional_effects <- plot(conditional_effects(brm_visits), re_formula = NA)



##make a table with the summary
summary_brm_visits <- summary(brm_visits)
fixed_effects <- as.data.frame(summary_brm_visits$fixed)
fixed_effects <- tibble::rownames_to_column(fixed_effects, var = "Term")
fixed_effects$Term <- c("Intercept", "Great tit", "Marsh tit", "Nuthatch",
                        "Spring","Summer", "Winter",
                        "Great tit x Spring", "Marsh tit x Spring", "Nuthatch x Spring", 
                        "Great tit x Summer", "Marsh tit x Summer", "Nuthatch x Summer", 
                        "Great tit x Winter", "Marsh tit x Winter", "Nuthatch x Winter" )

fixed_effects <- fixed_effects[, 1:(ncol(fixed_effects) - 2)]
fixed_effects <- fixed_effects %>%
  dplyr::mutate(across(where(is.numeric), ~ round(.x, 2)))


library(flextable)

flextable_table <- fixed_effects %>%
  flextable() %>%
  autofit() %>%
  set_caption("Summary of Fixed Effects from the final Model")

library(officer)

doc_brm_visits <- read_docx() %>%
  body_add_flextable(flextable_table)

print(doc_brm_visits, target = "Plots and tables/fixed_effects_visitation_rate.docx")



# the estimates are expressed in their logit scale so here we transform them into odds ratio's
exp(fixef(brm_visits))
# Estimate Est.Error      Q2.5     Q97.5
# Intercept                 1.6913274  1.050232 1.5315356 1.8612095
# speciesGRETI              0.9572254  1.058553 0.8562552 1.0684535
# speciesMARTI              1.0886985  1.064746 0.9616030 1.2325293
# speciesNUTHA              0.9308974  1.077666 0.8027994 1.0803722
# seasonspring              0.8241274  1.053023 0.7445621 0.9117200
# seasonsummer              1.0169610  1.084831 0.8702855 1.1934485
# seasonwinter              0.7366132  1.051579 0.6700303 0.8163244
# speciesGRETI:seasonspring 1.0586200  1.062648 0.9393872 1.1892272
# speciesMARTI:seasonspring 1.1629691  1.067992 1.0188529 1.3239260
# speciesNUTHA:seasonspring 1.2375605  1.095327 1.0365610 1.4771952
# speciesGRETI:seasonsummer 1.1068032  1.090554 0.9350528 1.3066791
# speciesMARTI:seasonsummer 1.0061992  1.096110 0.8437898 1.2048707
# speciesNUTHA:seasonsummer 0.9009870  1.103038 0.7452818 1.0913015
# speciesGRETI:seasonwinter 1.0292038  1.059916 0.9205240 1.1478893
# speciesMARTI:seasonwinter 1.2118456  1.060361 1.0778902 1.3552017
# speciesNUTHA:seasonwinter 1.2188550  1.077861 1.0506308 1.4115469


# Compute marginal means for the species variable
visits_species_emm <- emmeans(brm_visits, ~ species)
visits_contrasts <- contrast(visits_species_emm, method = "pairwise")
species_contrasts
# contrast      estimate lower.HPD upper.HPD
# BLUTI - GRETI    2.003     1.584     2.426
# BLUTI - MARTI    0.297    -0.414     1.045
# BLUTI - NUTHA   -0.279    -1.169     0.594
# GRETI - MARTI   -1.698    -2.408    -1.028
# GRETI - NUTHA   -2.280    -3.159    -1.436
# MARTI - NUTHA   -0.582    -1.621     0.444
# 
# Results are averaged over the levels of: season, age_in_2020 
# Point estimate displayed: median 
# Results are given on the log odds ratio (not the response) scale. 
# HPD interval probability: 0.95 

visits_species_season_emm <- emmeans(brm_visits, ~ species | season)
visits_species_season_contrasts <- contrast(visits_species_season_emm, method = "pairwise")
visits_species_season_contrasts

# season = autumn:
#   contrast      estimate lower.HPD upper.HPD
# BLUTI - GRETI   0.0439  -0.06556   0.15571
# BLUTI - MARTI  -0.0873  -0.20265   0.04486
# BLUTI - NUTHA   0.0726  -0.07197   0.22324
# GRETI - MARTI  -0.1291  -0.22294  -0.03604
# GRETI - NUTHA   0.0286  -0.09691   0.15522
# MARTI - NUTHA   0.1568   0.01446   0.28768
# 
# season = spring:
#   contrast      estimate lower.HPD upper.HPD
# BLUTI - GRETI  -0.0130  -0.07774   0.05093
# BLUTI - MARTI  -0.2357  -0.33302  -0.14334
# BLUTI - NUTHA  -0.1414  -0.28678  -0.00299
# GRETI - MARTI  -0.2234  -0.31333  -0.12871
# GRETI - NUTHA  -0.1291  -0.27348   0.00465
# MARTI - NUTHA   0.0941  -0.05554   0.25375
# 
# season = summer:
#   contrast      estimate lower.HPD upper.HPD
# BLUTI - GRETI  -0.0567  -0.18700   0.08825
# BLUTI - MARTI  -0.0895  -0.25052   0.06656
# BLUTI - NUTHA   0.1738   0.00741   0.34729
# GRETI - MARTI  -0.0342  -0.12486   0.06066
# GRETI - NUTHA   0.2332   0.11844   0.34552
# MARTI - NUTHA   0.2669   0.13040   0.40901
# 
# season = winter:
#   contrast      estimate lower.HPD upper.HPD
# BLUTI - GRETI   0.0150  -0.04128   0.07114
# BLUTI - MARTI  -0.2775  -0.35608  -0.20028
# BLUTI - NUTHA  -0.1267  -0.23145  -0.01487
# GRETI - MARTI  -0.2923  -0.35643  -0.21887
# GRETI - NUTHA  -0.1416  -0.24596  -0.04044
# MARTI - NUTHA   0.1512   0.02459   0.26166
# 
# Point estimate displayed: median 
# Results are given on the log (not the response) scale. 
# HPD interval probability: 0.95


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
visits_species_season_contrasts_df$odds <- exp(visits_species_season_contrasts_df$estimate)

visits_emm_table <- visits_species_season_contrasts_df %>%
  select(contrast, season, estimate, lower.HPD, upper.HPD, odds) #I make a table so I can make the calculations
#the percentage
visits_emm_table$prob_emm <- (exp(visits_emm_table$estimate) / (1 + exp(visits_emm_table$estimate)))*100



#extract the dataframe into a table for Word
library(flextable)
species_names <- c(
  "BLUTI" = "Blue tit",
  "GRETI" = "Great tit",
  "MARTI" = "Marsh tit",
  "NUTHA" = "Nuthatch"
)
library(stringr)
visits_emm_table <- visits_emm_table %>%
  mutate(contrast = str_replace_all(contrast, species_names))
colnames(visits_emm_table) <- c("Contrast", "Season", "Estimate", "Lower_HPD", "Upper_HPD", "Odds", "Probability")
visits_emm_table <- visits_emm_table %>%
  dplyr::mutate(across(where(is.numeric), ~ round(.x, 2)))

pairwise_table_visits <- flextable(visits_emm_table) %>%
  autofit() %>%
  set_caption("Pairwise Contrasts of the number of visitis of Species by Season ")

# Export to Word (optional)
library(officer)
doc_pariwise_comparisons_visits <- read_docx() %>%
  body_add_flextable(pairwise_table_visits)

print(doc_pariwise_comparisons_visits, target = "Plots and tables/pairwise_contrast_visitation_rate.docx")


# 3.2) Feeders visited ----------------------------------------------------




brm_visited_feeders <- brm(
  num.feeders.visited ~ species*season + scale(visits_per_season) + (1|PIT),
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
# [1] 0.3308824

# our model is underdispersed

# we try a neg binomial model 
brm_visited_feeders <- brm(
  num.feeders.visited ~ species * season + scale(visits_per_season) + (1 | PIT),
  family = negbinomial(),
  data = subset2_network.pos.all.seasons,
  chains = 4,
  iter = 4000,
  cores = 4,
  save_pars = save_pars(all = TRUE)
)

#save(brm_visited_feeders, file="model output/brm_visited_feeders.RDA")
load("model output/brm_visited_feeders.RDA")

# Using brms output
bayes_R2(brm_visited_feeders)  # Good to see variance explained

# Estimate  Est.Error       Q2.5     Q97.5
# R2 0.1432613 0.03653711 0.07756966 0.2187103

summary(brm_visited_feeders)
# Family: binomial 
# Links: mu = logit 
# Formula: num.feeders.visited | trials(6) ~ species * season + scale(visits_per_season) + (1 | PIT) 
# Data: subset2_network.pos.all.seasons (Number of observations: 475) 
# Draws: 4 chains, each with iter = 4000; warmup = 2000; thin = 1;
# total post-warmup draws = 8000
# 
# Multilevel Hyperparameters:
#   ~PIT (Number of levels: 234) 
# Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sd(Intercept)     0.06      0.05     0.00     0.17 1.00     5037     4185
# 
# Regression Coefficients:
#   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept                    -1.37      0.28    -1.91    -0.85 1.00     2944     4111
# speciesGRETI                  0.24      0.31    -0.35     0.85 1.00     3073     4228
# speciesMARTI                  0.69      0.40    -0.11     1.47 1.00     3713     5179
# speciesNUTHA                  0.54      0.60    -0.65     1.69 1.00     4960     5348
# seasonspring                  0.01      0.32    -0.61     0.65 1.00     3272     4356
# seasonsummer                  0.12      0.40    -0.68     0.90 1.00     3916     5477
# seasonwinter                  0.26      0.31    -0.33     0.87 1.00     3183     4469
# scalevisits_per_season        0.05      0.06    -0.06     0.16 1.00    17260     6375
# speciesGRETI:seasonspring    -0.19      0.37    -0.92     0.50 1.00     3535     5233
# speciesMARTI:seasonspring    -0.38      0.52    -1.39     0.63 1.00     4664     5825
# speciesNUTHA:seasonspring    -0.87      0.80    -2.44     0.69 1.00     5864     6491
# speciesGRETI:seasonsummer    -0.23      0.44    -1.07     0.63 1.00     3940     5223
# speciesMARTI:seasonsummer    -0.52      0.61    -1.72     0.68 1.00     5336     6125
# speciesNUTHA:seasonsummer    -0.65      0.78    -2.16     0.85 1.00     5663     6238
# speciesGRETI:seasonwinter    -0.19      0.35    -0.89     0.47 1.00     3327     5037
# speciesMARTI:seasonwinter    -0.51      0.49    -1.47     0.45 1.00     4737     6042
# speciesNUTHA:seasonwinter    -0.82      0.72    -2.21     0.60 1.00     5426     5909
# 
# Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
# and Tail_ESS are effective sample size measures, and Rhat is the potential
# scale reduction factor on split chains (at convergence, Rhat = 1).

feeders_conditional_effects <- plot(conditional_effects(brm_visited_feeders), re_formula = NA)
pp_check(brm_visits, ndraws = 100)


##make a table with the summary
summary_visited_feeders <- summary(brm_visited_feeders)
fixed_effects <- as.data.frame(summary_visited_feeders$fixed)
fixed_effects <- tibble::rownames_to_column(fixed_effects, var = "Term")

fixed_effects$Term <- c("Intercept", "Great tit", "Marsh tit", "Nuthatch",
                        "Spring","Summer", "Winter",
                        "Great tit x Spring", "Marsh tit x Spring", "Nuthatch x Spring", 
                        "Great tit x Summer", "Marsh tit x Summer", "Nuthatch x Summer", 
                        "Great tit x Winter", "Marsh tit x Winter", "Nuthatch x Winter" )

fixed_effects <- fixed_effects[, 1:(ncol(fixed_effects) - 2)]
fixed_effects <- fixed_effects %>%
  dplyr::mutate(across(where(is.numeric), ~ round(.x, 2)))
library(flextable)

flextable_table <- fixed_effects %>%
  flextable() %>%
  autofit() %>%
  set_caption("Summary of Fixed Effects from the number of visited feeders")

library(officer)

doc_visited_feeders <- read_docx() %>%
  body_add_flextable(flextable_table)

print(doc_visited_feeders, target = "Plots and tables/fixed_effects_visited_feeders.docx")



# estimates are expressed in their logit scale so here I will transform them into odds ratios
exp(fixef(brm_visited_feeders))
# Estimate Est.Error       Q2.5     Q97.5
# Intercept                 0.2497070  1.318130 0.14124879 0.4244773
# speciesGRETI              1.2639621  1.362760 0.68965854 2.3332046
# speciesMARTI              2.1426229  1.485799 0.97996376 4.6327766
# speciesNUTHA              1.9059056  1.806721 0.58626897 5.8960910
# seasonspring              1.0277350  1.372307 0.55791445 1.9630545
# seasonsummer              1.1112258  1.500886 0.49078350 2.3994869
# seasonwinter              1.3299565  1.361792 0.73236924 2.5199169
# speciesGRETI:seasonspring 0.8200513  1.444351 0.39849401 1.6763972
# speciesMARTI:seasonspring 0.6467368  1.680859 0.23213607 1.7571595
# speciesNUTHA:seasonspring 0.3756343  2.216621 0.07560513 1.7587908
# speciesGRETI:seasonsummer 0.8126164  1.559420 0.34633597 1.9608316
# speciesMARTI:seasonsummer 0.5800391  1.838535 0.17660776 1.9010465
# speciesNUTHA:seasonsummer 0.5150794  2.171526 0.11401700 2.3710936
# speciesGRETI:seasonwinter 0.8312591  1.419645 0.42066953 1.6380849
# speciesMARTI:seasonwinter 0.6547176  1.624082 0.25156574 1.6730311
# speciesNUTHA:seasonwinter 0.4314953  2.081859 0.10286918 1.8139496


# Compute marginal means for the species variable
feeders_species_emm <- emmeans(brm_visited_feeders, ~ species)
feeders_contrasts <- contrast(feeders_species_emm, method = "pairwise")
feeders_contrasts
# contrast      estimate lower.HPD upper.HPD
# BLUTI - GRETI  -0.0866    -0.336   0.16480
# BLUTI - MARTI  -0.4119    -0.755  -0.01361
# BLUTI - NUTHA  -0.0270    -0.513   0.49186
# GRETI - MARTI  -0.3238    -0.641  -0.00927
# GRETI - NUTHA   0.0554    -0.389   0.54829
# MARTI - NUTHA   0.3879    -0.138   0.93196
# 
# Results are averaged over the levels of: season 
# Point estimate displayed: median 
# Results are given on the log odds ratio (not the response) scale. 
# HPD interval probability: 0.95 
feeders_species_season_emm <- emmeans(brm_visited_feeders, ~ species | season)
# feeders_species_season_contrasts <- contrast(feeders_species_season_emm, method = "pairwise")
# feeders_species_season_contrasts
# season = autumn:
#   contrast      estimate lower.HPD upper.HPD
# BLUTI - GRETI  -0.2313    -0.837    0.3771
# BLUTI - MARTI  -0.7564    -1.528    0.0214
# BLUTI - NUTHA  -0.6503    -1.784    0.5255
# GRETI - MARTI  -0.5296    -1.158    0.0784
# GRETI - NUTHA  -0.4278    -1.393    0.7077
# MARTI - NUTHA   0.1109    -0.992    1.3265
# 
# season = spring:
#   contrast      estimate lower.HPD upper.HPD
# BLUTI - GRETI  -0.0307    -0.439    0.3427
# BLUTI - MARTI  -0.3305    -0.974    0.3460
# BLUTI - NUTHA   0.3114    -0.685    1.3778
# GRETI - MARTI  -0.2919    -0.940    0.3051
# GRETI - NUTHA   0.3393    -0.632    1.3864
# MARTI - NUTHA   0.6443    -0.403    1.9204
# 
# season = summer:
#   contrast      estimate lower.HPD upper.HPD
# BLUTI - GRETI  -0.0227    -0.679    0.5663
# BLUTI - MARTI  -0.2153    -1.152    0.6793
# BLUTI - NUTHA   0.0190    -0.988    0.9953
# GRETI - MARTI  -0.1976    -0.939    0.5389
# GRETI - NUTHA   0.0285    -0.833    0.8282
# MARTI - NUTHA   0.2302    -0.824    1.3283
# 
# season = winter:
#   contrast      estimate lower.HPD upper.HPD
# BLUTI - GRETI  -0.0472    -0.370    0.2570
# BLUTI - MARTI  -0.3398    -0.883    0.2207
# BLUTI - NUTHA   0.1819    -0.702    1.0302
# GRETI - MARTI  -0.2936    -0.808    0.2241
# GRETI - NUTHA   0.2330    -0.606    1.0660
# MARTI - NUTHA   0.5223    -0.422    1.4407
# 
# Point estimate displayed: median 
# Results are given on the log odds ratio (not the response) scale. 
# HPD interval probability: 0.95 


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
species_names <- c(
  "BLUTI" = "Blue tit",
  "GRETI" = "Great tit",
  "MARTI" = "Marsh tit",
  "NUTHA" = "Nuthatch"
)
library(stringr)
feeders_emm_table <- feeders_emm_table %>%
  mutate(contrast = str_replace_all(contrast, species_names))
colnames(feeders_emm_table) <- c("Contrast", "Season", "Estimate", "Lower_HPD", "Upper_HPD", "Odds", "Probability")
feeders_emm_table <- feeders_emm_table %>%
  dplyr::mutate(across(where(is.numeric), ~ round(.x, 2)))

pairwise_table_visited_feeders <- flextable(feeders_emm_table) %>%
  autofit() %>%
  set_caption("Pairwise Contrasts of the number of visited feeders of Species by Season ")

# Export to Word (optional)
library(officer)
doc_pariwise_comparisons_feeders <- read_docx() %>%
  body_add_flextable(pairwise_table_visited_feeders)

print(doc_pariwise_comparisons_feeders, target = "Plots and tables/pairwise_contrasts_feeders.docx")


#creating plot
library(ggpubr)
levels(visits_all_season$season)
my_colors <- c("spring" = "#56ae6c", 
               "summer" = "#8960b3", 
               "winter" = "#b0923b", 
               "autumn" = "#ba495b")


# comb.plot <- ggarrange(
#   
# 
#     visits_conditional_effects$`species:season` +
#     aes(shape = species) +
#     scale_shape_manual(
#       values = c("BLUTI" = 16, "GRETI" = 18, "MARTI" = 15, "NUTHA" = 17),
#       name = "Species",
#       labels = c("BLUTI" = "Blue tits", "GRETI" = "Great tits", "MARTI" = "Marsh tits", "NUTHA" = "Nuthaches")
#     ) +
#     labs(x= NULL, y= "Number of visits") +
#     ylim(c(1.0,2.2)) +
#     theme_bw() +
#     scale_fill_manual(
#       values= my_colors, 
#       breaks=c("autumn", "spring", "summer", "winter"), 
#       labels=c("Autumn", "Spring", "Summer", "Winter"),
#       name = "Season" 
#     ) +
#     scale_color_manual(
#       values= my_colors, 
#       breaks=c("autumn", "spring", "summer", "winter"), 
#       labels=c("Autumn", "Spring", "Summer", "Winter"),
#       name = "Season"  # <- And here
#     ) +
#     scale_x_discrete(labels=c("BLUTI" = "Blue tits", "GRETI" = "Great tits", "MARTI" = "Marsh tits", "NUTHA" = "Nuthaches")) +
#     theme(
#       legend.title=element_text(),
#       plot.margin = margin(t = 20, r = 5, b = 5, l = 5)
#     ),
#   
#   feeders_conditional_effects$`species:season` +
#     aes(shape = species) +
#     scale_shape_manual(
#       values = c("BLUTI" = 16, "GRETI" = 18, "MARTI" = 15, "NUTHA" = 17),
#       name = "Species",
#       labels = c("BLUTI" = "Blue tits", "GRETI" = "Great tits", "MARTI" = "Marsh tits", "NUTHA" = "Nuthaches")
#     ) +
#     labs(y= "Number of visited feeders") + 
#     ylim(c(0.0,4)) +
#     theme_bw() +
#     scale_fill_manual(
#       values= my_colors, 
#       breaks=c("autumn", "spring", "summer", "winter"), 
#       labels=c("Autumn", "Spring", "Summer", "Winter"),
#       name = "Season"   
#     ) +
#     scale_color_manual(
#       values= my_colors, 
#       breaks=c("autumn", "spring", "summer", "winter"), 
#       labels=c("Autumn", "Spring", "Summer", "Winter"),
#       name = "Season"  
#     ) +
#     scale_x_discrete(labels=c("BLUTI" = "Blue tits", "GRETI" = "Great tits", "MARTI" = "Marsh tits", "NUTHA" = "Nuthaches")) +
#     theme(
#       legend.title = element_text(),  # show legend titles
#       axis.title.x = element_blank(),
#       plot.margin = margin(t = 5, r = 5, b = 20, l = 5)
#     ),
#   
#   common.legend = TRUE,
#   legend = "right",
#   labels= c("a", "b"),
#   label.x = 0.01,   
#   label.y = 1.02,   
#   ncol = 1, nrow = 2,
#   heights = c(1, 1.2)
# )
# 
# ggsave(file="Plots and tables/plot_territoriality and caching.tiff", width=9, height=6.5)
# 
# 




plot1 <- 
  visits_conditional_effects$`species:season` +
  aes(shape = species) +
  scale_shape_manual(
    values = c("BLUTI" = 16, "GRETI" = 18, "MARTI" = 15, "NUTHA" = 17),
    name = "Species",
    labels = c("BLUTI" = "Blue tits", "GRETI" = "Great tits", "MARTI" = "Marsh tits", "NUTHA" = "Nuthaches")
  ) +
  labs(x= NULL, y= "Number of visits") +
  ylim(c(1.0,2.2)) +
  theme_bw() +
  scale_fill_manual(
    values= my_colors, 
    breaks=c("autumn", "spring", "summer", "winter"), 
    labels=c("Autumn", "Spring", "Summer", "Winter"),
    name = "Season" 
  ) +
  scale_color_manual(
    values= my_colors, 
    breaks=c("autumn", "spring", "summer", "winter"), 
    labels=c("Autumn", "Spring", "Summer", "Winter"),
    name = "Season"
  ) +
  scale_x_discrete(labels=c("BLUTI" = "Blue tits", "GRETI" = "Great tits", "MARTI" = "Marsh tits", "NUTHA" = "Nuthaches")) +
  theme(
    # axis.title.x = element_blank(),
    #  axis.text.x = element_blank(),
    #  axis.ticks.x = element_blank(),
    legend.title = element_text(),
    plot.margin = margin(t = 20, r = 5, b = 5, l = 5)
  )





plot2 <- 
  feeders_conditional_effects$`species:season` +
  aes(shape = species) +
  scale_shape_manual(
    values = c("BLUTI" = 16, "GRETI" = 18, "MARTI" = 15, "NUTHA" = 17),
    name = "Species",
    labels = c("BLUTI" = "Blue tits", "GRETI" = "Great tits", "MARTI" = "Marsh tits", "NUTHA" = "Nuthaches")
  ) +
  labs(x= NULL, y= "Number of visited feeders") +
  ylim(c(0.0,4)) +
  theme_bw() +
  scale_fill_manual(
    values= my_colors, 
    breaks=c("autumn", "spring", "summer", "winter"), 
    labels=c("Autumn", "Spring", "Summer", "Winter"),
    name = "Season"   
  ) +
  scale_color_manual(
    values= my_colors, 
    breaks=c("autumn", "spring", "summer", "winter"), 
    labels=c("Autumn", "Spring", "Summer", "Winter"),
    name = "Season"  
  ) +
  scale_x_discrete(labels=c("BLUTI" = "Blue tits", "GRETI" = "Great tits", "MARTI" = "Marsh tits", "NUTHA" = "Nuthaches")) +
  theme(
    #  axis.title.x = element_blank(),
    #  axis.text.x = element_blank(),
    #  axis.ticks.x = element_blank(),
    legend.title = element_text(),
    plot.margin = margin(t = 20, r = 5, b = 5, l = 5)
  )




# Combine
combined_plot <- ggarrange(
  plot1, plot2,
  common.legend = TRUE,
  legend = "right",
  labels = c("a", "b"),
  label.x = 0.0,
  label.y = 1.0,
  ncol = 1,
  nrow = 2,
  heights = c(1, 1)
)


# Save
ggsave("Plots and tables/plot_territoriality_and_caching.tiff",
       plot = combined_plot,
       width = 7, height = 5,
       bg = "white")


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

# 0.3012372

# different approach:

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
# 0.301091

#Calculate CI
rethinking::HPDI(repeatability, prob = 0.95)
#|0.95             0.95| 
#  0.03059087    0.05734588 

#This last part was inspired by the study: https://datadryad.org/stash/dataset/doi:10.25338/B88P8W


