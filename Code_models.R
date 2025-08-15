# elephant: write the title of the paper and authors


# 1) Data prep ------------------------------------------------------------


# 1.1) Load libraries -----------------------------------------------------
#library(devtools)
#devtools::install_github("rmcelreath/rethinking")
# note that rethinking needs cmdstan (make sure that is installed)

##Library
library(asnipe)
library(brms)
library(broom.mixed)
library(car)
library(corrplot)
library(data.table)
library(datawizard)
library(dplyr)
library(devtools)
library(dplyr)
library(emmeans)
library(flextable)
library(ggplot2)
library(ggpubr)
library(lme4)
library(lmerTest)
library(lubridate)
library(officer)
library(performance)
library(psych)
library(report)
library(rethinking)
library(rptR)
library(rstan)
library(stringr)
library(tibble)
library(tidybayes)
library(tidyr)


# 1.2) Extract order of arrival at feeders --------------------------------

# this contains the complete data set for analysis the leader follower dynamics
network.pos.all.seasons <- read.delim("data/leader_follower_data_all_seasons.txt", sep=" ")
head(network.pos.all.seasons)


#Some details on what the different columns are
names(network.pos.all.seasons)

# "PIT" : 10 digit alphanumeric code unique to each individual
# "degree": the value of degree centrality for each individual
# "Date.Time": yymmddHHMMSS
# "Antenna": A for auxiliary, M for main
# "location":  Mill1-Mill6 (6 feeders around study area)
# "week": experimental week within each season (during each week, we collected data for 48 hours). summer has 14 weeks, all other season 3 weeks
# "group": a unique numeric identifier for eachflock
# "visit.duration": the time spent on the feeder in seconds  
# "leader.follower": the role within the mixed flock (leader or follower) 
# "species": the 4 studied species (GRETI : Great tits, BLUTI: Blue tits, MARTI: Marsh tits, NUTHA: Nuthaches)         
# "age_in_2020": adults or juveniles
# "season": summer, spring, winter, autumn         
# "betweenness": the value of the betweenness centrality for each individual
# "n_species": a bird's species prevalence within each flock (e.g. 7 great tits in flock 450)
# "flock_size": total birds per flock
# "species_prop": species prevalence as a proportion of the total flock size (n_species/flock_size)


# 2) Validate species territoriality --------------------------------------


# 2.1) Extracting territoriality ------------------------------------------

# includes all group sizes (also lone birds)

# how many data points per group size per species?
table(network.pos.all.seasons$species, network.pos.all.seasons$flock_size)

# extract per season per bird:
# - how many feeders did it visit
# - how many times was it registered in a season
# - the number of unique days it was registered per season

library(lubridate)

territoriality_df <- network.pos.all.seasons %>%
  mutate(
    date_time = ymd_hms(Date.Time, tz = "UTC"),        # parse full timestamp
    date = as.Date(date_time)                          # extract date only
  ) %>%
  group_by(PIT, season, species) %>%
  summarise(
    n_feeders = n_distinct(location),
    n_unique_days = n_distinct(date),
    n_obs = n(),
    .groups = "drop"
  )


# calculate the number of visits per feeder as a measure of territorialiy
# more territorial = more visits per feeder
territoriality_df$visits_per_feeder <- territoriality_df$n_obs/territoriality_df$n_feeders

# subset to one value per 
head(territoriality_df)

# we use these two measures as validation for territoriality
boxplot(territoriality_df$n_unique_days~territoriality_df$species)
boxplot(territoriality_df$visits_per_feeder~territoriality_df$species)

# set class of visits per feeder to integer

territoriality_df$visits_per_feeder <- as.integer(territoriality_df$visits_per_feeder)

# 2.2) Run model territoriality -------------------------------------------

# 2.2.1) model territoriality  -------------------------------------------
# we run a multivariate model to test whether the number of visits per feeder and the number of days a bird was observed in a given season is explained by species and season


model_territoriality_num_visits <- brm(
  bf(visits_per_feeder ~ season*species + (1|PIT), family = negbinomial()),
  data = territoriality_df,
  cores = 4,
  chains = 4,
  iter = 4000
)

#save(model_territoriality_num_visits, file="model output/model_territoriality_num_visits.RDA")
load("model output/model_territoriality_num_visits.RDA")
summary(model_territoriality_num_visits)

model_territoriality_days_present <- brm(
    bf(n_unique_days | trials(9) ~ season*species + (1|PIT), family = binomial()),
  data = territoriality_df,
  cores = 4,
  chains = 4,
  iter = 4000
)

#save(model_territoriality, file="model output/model_territoriality_days_present.RDA")
load("model output/model_territoriality_days_present.RDA")

# SW: I had to make two separate models, because emmeans does currently not work for multivariate models. You'll have to extract things separately for each model

summary(model_territoriality_days_present)
summary(model_territoriality_num_visits)

# 2.2.2) model model_territoriality_days_present -------------------------------------------
###Extract the summary output into a table
sum_days <- summary(model_territoriality_days_present)
days_fixed_df <- as.data.frame(sum_days$fixed)
days_fixed_df <- tibble::rownames_to_column(days_fixed_df, var = "Term")
days_fixed_df$Term <- c("Intercept", "Spring","Summer", "Winter",
                        "Great tit", "Marsh tit", "Nuthatch",
                        "Great tit x Spring", "Great tit x Summer", "Great tit x Winter",
                        "Marsh tit x Spring", "Marsh tit x Summer", "Marsh tit x Winter",
                        "Nuthatch x Spring","Nuthatch x Summer","Nuthatch x Winter" )
days_fixed_df <- days_fixed_df[, 1:(ncol(days_fixed_df) - 2)]
colnames(days_fixed_df)


days_fixed_df <- days_fixed_df %>%
  select("Term", "Estimate", "Est.Error", "l-95% CI", "u-95% CI", "Rhat") %>%
  rename(
    Estimate = "Estimate",
    Est_Error = "Est.Error",
    CI_Lower = "l-95% CI",
    CI_Upper = "u-95% CI",
    Rhat = Rhat
  )%>%
  mutate(Odds = exp(Estimate),
         Est_Error_Odds = exp(Est_Error),
         Odds_Lower = exp(CI_Lower),
         Odds_Upper = exp(CI_Upper))

days_fixed_df <- days_fixed_df %>%
  select("Term", "Est_Error_Odds", "Odds_Lower", "Odds_Upper", "Odds", "Rhat") 

days_fixed_df <- days_fixed_df %>%
  dplyr::mutate(across(where(is.numeric), ~ round(.x, 2)))

days_table <- flextable(days_fixed_df) %>%
  autofit()

# 6. Export to Word
doc <- read_docx()
doc <- body_add_flextable(doc, value = days_table)
print(doc, target = "Plots and tables/fixed_effects_days_model.docx")

# 2.2.3) model model_territoriality_num_visits -------------------------------------------
###Extract the summary output into a table
sum_visits <- summary(model_territoriality_num_visits)
sum_visits_df <- as.data.frame(sum_visits$fixed)
sum_visits_df <- tibble::rownames_to_column(sum_visits_df, var = "Term")
sum_visits_df$Term <- c("Intercept", "Spring","Summer", "Winter",
                        "Great tit", "Marsh tit", "Nuthatch",
                        "Great tit x Spring", "Great tit x Summer", "Great tit x Winter",
                        "Marsh tit x Spring", "Marsh tit x Summer", "Marsh tit x Winter",
                        "Nuthatch x Spring","Nuthatch x Summer","Nuthatch x Winter" )
sum_visits_df <- sum_visits_df[, 1:(ncol(sum_visits_df) - 2)]
colnames(sum_visits_df)


sum_visits_df <- sum_visits_df %>%
  select("Term", "Estimate", "Est.Error", "l-95% CI", "u-95% CI", "Rhat") %>%
  rename(
    Estimate = "Estimate",
    Est_Error = "Est.Error",
    CI_Lower = "l-95% CI",
    CI_Upper = "u-95% CI",
    Rhat = Rhat
  )%>%
  mutate(Odds = exp(Estimate),
         Est_Error_Odds = exp(Est_Error),
         Odds_Lower = exp(CI_Lower),
         Odds_Upper = exp(CI_Upper))

sum_visits_df <- sum_visits_df %>%
  select("Term", "Est_Error_Odds", "Odds_Lower", "Odds_Upper", "Odds", "Rhat") 

sum_visits_df <- sum_visits_df %>%
  dplyr::mutate(across(where(is.numeric), ~ round(.x, 2)))

visits_table <- flextable(sum_visits_df) %>%
  autofit()

# 6. Export to Word
doc <- read_docx()
doc <- body_add_flextable(doc, value = visits_table)
print(doc, target = "Plots and tables/fixed_effects_visits_model.docx")

# 2.3) Model checks -------------------------------------------------------
pp_check(model_territoriality_days_present, ndraws = 100)
pp_check(model_territoriality_num_visits, ndraws = 100)


performance::r2_bayes(model_territoriality_days_present)
#Conditional R2: 0.557 (95% CI [0.518, 0.598])
#Marginal R2: 0.228 (95% CI [0.186, 0.271])
performance::r2_bayes(model_territoriality_num_visits)
#Conditional R2: 0.552 (95% CI [0.468, 0.621])
#Marginal R2: 0.336 (95% CI [0.196, 0.481])


# 2.4) Extract effects  ---------------------------------------------------
#a) emmeans for the number of visits per feeder
# Compute marginal means for the species variable
days_species_emm <- emmeans(model_territoriality_days_present, ~ species)
# Pairwise comparisons between species levels
days_species_contrasts <- contrast(days_species_emm, method = "pairwise")
days_species_contrasts

#  contrast      estimate lower.HPD upper.HPD
#BLUTI - GRETI   0.0543    -0.282     0.407
#BLUTI - MARTI  -1.3411    -2.016    -0.722
#BLUTI - NUTHA  -1.1088    -2.014    -0.289
#GRETI - MARTI  -1.3938    -2.034    -0.803
#GRETI - NUTHA  -1.1570    -1.993    -0.346
#MARTI - NUTHA   0.2267    -0.743     1.235
# Results are averaged over the levels of: season, age_in_2020 
# Point estimate displayed: median 
# Results are given on the log odds ratio (not the response) scale. 
# HPD interval probability: 0.95


# Marginal means for species by season
days_species_season_emm <- emmeans(model_territoriality_days_present, ~ species | season)
# Pairwise comparisons within each season
days_species_season_contrasts <- contrast(days_species_season_emm, method = "pairwise")
days_species_season_contrasts

#season = autumn:
#contrast      estimate lower.HPD upper.HPD
#BLUTI - GRETI   0.1480    -0.475    0.7092
#BLUTI - MARTI  -1.5393    -2.455   -0.6074
#BLUTI - NUTHA  -3.0708    -5.157   -1.3265
#GRETI - MARTI  -1.6874    -2.474   -0.8739
#GRETI - NUTHA  -3.2247    -5.200   -1.5133
#MARTI - NUTHA  -1.5474    -3.602    0.3536

#season = spring:
#  contrast      estimate lower.HPD upper.HPD
#BLUTI - GRETI   0.2438    -0.196    0.6671
#BLUTI - MARTI  -1.2184    -2.185   -0.3247
#BLUTI - NUTHA  -0.1974    -1.191    0.8767
#GRETI - MARTI  -1.4576    -2.365   -0.5729
#GRETI - NUTHA  -0.4374    -1.442    0.5904
#MARTI - NUTHA   1.0274    -0.268    2.3120

#season = summer:
#  contrast      estimate lower.HPD upper.HPD
#BLUTI - GRETI  -0.3769    -0.952    0.2037
#BLUTI - MARTI  -1.1689    -2.075   -0.1454
#BLUTI - NUTHA  -0.4011    -1.426    0.6193
#GRETI - MARTI  -0.7912    -1.623    0.0649
#GRETI - NUTHA  -0.0276    -0.941    0.8829
#MARTI - NUTHA   0.7649    -0.382    2.0038

#season = winter:
#  contrast      estimate lower.HPD upper.HPD
#BLUTI - GRETI   0.2079    -0.175    0.6008
#BLUTI - MARTI  -1.4326    -2.262   -0.6447
#BLUTI - NUTHA  -0.7138    -1.747    0.2414
#GRETI - MARTI  -1.6423    -2.438   -0.8920
#GRETI - NUTHA  -0.9268    -1.924    0.0181
#MARTI - NUTHA   0.7224    -0.451    1.9284


#Results are averaged over the levels of: age_in_2020 
#Point estimate displayed: median 
#Results are given on the log (not the response) scale. 
#HPD interval probability: 0.95 


# How to interpret:
# estimate: difference in marginal means
# if the credible interval does not include 0, two groups are considered significantly different
# Plot the marginal means for species
plot(days_species_emm)
# Plot pairwise comparisons for species across seasons
plot(days_species_season_emm)
# Extract contrasts as a data frame
days_species_season_contrasts_df <- as.data.frame(days_species_season_contrasts)
head(days_species_season_contrasts_df)
# you get from log odds to odds by expontiating:
exp(days_species_season_contrasts_df$estimate) 
# an odds ratio of 1.67 means that e.g. BLUTI are 1.67 times more likely to be the leader compared to GRETI
# If one were interested in probabilities for a certain species to be a leader, you calculate as: probability = exp(log odds) / (1 + exp(log odds)) = exp(estimate)/(1+ exp(estimate))
days_species_season_contrasts_df <- days_species_season_contrasts_df %>%
  mutate(
    odds = exp(estimate),
    odds_lower = exp(lower.HPD),
    odds_upper = exp(upper.HPD)
  )
days_emm_table <- days_species_season_contrasts_df %>%
  dplyr::select(contrast, season, odds_lower, odds_upper, odds) 

#extract the dataframe into a table for Word
species_names <- c(
  "BLUTI" = "Blue tit",
  "GRETI" = "Great tit",
  "MARTI" = "Marsh tit",
  "NUTHA" = "Nuthatch"
)
library(stringr)
days_emm_table <- days_emm_table %>%
  mutate(contrast = str_replace_all(contrast, species_names))
colnames(days_emm_table) <- c("Contrast", "Season", "odds_lower", "odds_upper", "Odds")
days_emm_table <- days_emm_table %>%
  dplyr::mutate(across(where(is.numeric), ~ round(.x, 2)))

days_pairwise_table <- flextable(days_emm_table) %>%
  autofit() %>%
  set_caption("Pairwise Contrasts of Species by Season")

# Export to Word (optional)
library(officer)
doc_pariwise_comparisons_days <- read_docx() %>%
  body_add_flextable(days_pairwise_table)

print(doc_pariwise_comparisons_days, target = "Plots and tables/pairwise_contrasts_days_model.docx")

#b) emmeans for the number of visits at any feeder
# Compute marginal means for the species variable
visits_species_emm <- emmeans(model_territoriality_num_visits, ~ species)
# Pairwise comparisons between species levels
visits_species_contrasts <- contrast(visits_species_emm, method = "pairwise")
visits_species_contrasts

#  contrast      estimate lower.HPD upper.HPD
#BLUTI - GRETI   0.0157    -0.229     0.292
#BLUTI - MARTI  -1.0122    -1.456    -0.586
#BLUTI - NUTHA  -1.2411    -1.789    -0.705
#GRETI - MARTI  -1.0335    -1.413    -0.629
#GRETI - NUTHA  -1.2604    -1.783    -0.760
#MARTI - NUTHA  -0.2241    -0.849     0.385
# Results are averaged over the levels of: season, age_in_2020 
# Point estimate displayed: median 
# Results are given on the log odds ratio (not the response) scale. 
# HPD interval probability: 0.95


# Marginal means for species by season
visits_species_season_emm <- emmeans(model_territoriality_num_visits, ~ species | season)
# Pairwise comparisons within each season
visits_species_season_contrasts <- contrast(visits_species_season_emm, method = "pairwise")
visits_species_season_contrasts

#season = autumn:
#contrast      estimate lower.HPD upper.HPD
#BLUTI - GRETI   0.5449    0.0408    1.0751
#BLUTI - MARTI  -1.0339   -1.7801   -0.3312
#BLUTI - NUTHA  -1.5539   -2.5907   -0.4771
#GRETI - MARTI  -1.5824   -2.2304   -0.9636
#GRETI - NUTHA  -2.0891   -3.0948   -1.1077
#MARTI - NUTHA  -0.5118   -1.7054    0.5376

#season = spring:
##  contrast      estimate lower.HPD upper.HPD
#BLUTI - GRETI   0.4495    0.1056    0.7821
#BLUTI - MARTI  -0.0963   -0.7095    0.5039
#BLUTI - NUTHA  -0.1880   -1.0790    0.5840
#GRETI - MARTI  -0.5448   -1.1425    0.0365
#GRETI - NUTHA  -0.6356   -1.4687    0.1531
#MARTI - NUTHA  -0.0984   -1.0148    0.8886

#season = summer:
#  contrast      estimate lower.HPD upper.HPD
#BLUTI - GRETI  -0.9323   -1.4566   -0.3642
#BLUTI - MARTI  -1.6133   -2.4592   -0.8203
#BLUTI - NUTHA  -1.9133   -2.8215   -1.0451
#GRETI - MARTI  -0.6757   -1.3590   -0.0189
#GRETI - NUTHA  -0.9816   -1.7395   -0.2146
#MARTI - NUTHA  -0.2970   -1.3158    0.6404

#season = winter:
##  contrast      estimate lower.HPD upper.HPD
#BLUTI - GRETI   0.0074   -0.2984    0.3124
#BLUTI - MARTI  -1.2899   -1.8310   -0.7507
##BLUTI - NUTHA  -1.2795   -2.0582   -0.5632
#GRETI - MARTI  -1.2985   -1.8133   -0.8046
#GRETI - NUTHA  -1.2837   -2.0297   -0.5685
#MARTI - NUTHA   0.0154   -0.8797    0.8159

#Results are averaged over the levels of: age_in_2020 
#Point estimate displayed: median 
#Results are given on the log (not the response) scale. 
#HPD interval probability: 0.95 


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
exp(visits_species_season_contrasts_df$estimate) 
# an odds ratio of 1.67 means that e.g. BLUTI are 1.67 times more likely to be the leader compared to GRETI
# If one were interested in probabilities for a certain species to be a leader, you calculate as: probability = exp(log odds) / (1 + exp(log odds)) = exp(estimate)/(1+ exp(estimate))
visits_species_season_contrasts_df <- visits_species_season_contrasts_df %>%
  mutate(
    odds = exp(estimate),
    odds_lower = exp(lower.HPD),
    odds_upper = exp(upper.HPD)
  )
visits_emm_table <- visits_species_season_contrasts_df %>%
  dplyr::select(contrast, season, odds_lower, odds_upper, odds) 

#extract the dataframe into a table for Word
species_names <- c(
  "BLUTI" = "Blue tit",
  "GRETI" = "Great tit",
  "MARTI" = "Marsh tit",
  "NUTHA" = "Nuthatch"
)
library(stringr)
visits_emm_table <- visits_emm_table %>%
  mutate(contrast = str_replace_all(contrast, species_names))
colnames(visits_emm_table) <- c("Contrast", "Season", "odds_lower", "odds_upper", "Odds")
visits_emm_table <- visits_emm_table %>%
  dplyr::mutate(across(where(is.numeric), ~ round(.x, 2)))

visits_pairwise_table <- flextable(visits_emm_table) %>%
  autofit() %>%
  set_caption("Pairwise Contrasts of Species by Season")

# Export to Word (optional)
library(officer)
doc_pariwise_comparisons_visits <- read_docx() %>%
  body_add_flextable(visits_pairwise_table)

print(doc_pariwise_comparisons_visits, target = "Plots and tables/pairwise_contrasts_visits_model.docx")



# 2.5) Plot ---------------------------------------------------------------

plot(conditional_effects(model_territoriality))

#extract predicted means and CI from the brm model
# First response variable
pred_visits <- fitted(
  model_territoriality,
  resp = "visitsperfeeder",
  newdata = expand.grid(
    season = unique(territoriality_df$season),
    species = unique(territoriality_df$species),
    PIT = NA   # random effect set to population-level
  ),
  re_formula = NA,     # exclude random effects
  summary = TRUE
) %>%
  as.data.frame() %>%
  bind_cols(expand.grid(
    season = unique(territoriality_df$season),
    species = unique(territoriality_df$species)
  ))

# Second response variable
pred_days <- fitted(
  model_territoriality,
  resp = "nuniquedays",
  newdata = expand.grid(
    season = unique(territoriality_df$season),
    species = unique(territoriality_df$species),
    PIT = NA
  ),
  re_formula = NA,
  summary = TRUE
) %>%
  as.data.frame() %>%
  bind_cols(expand.grid(
    season = unique(territoriality_df$season),
    species = unique(territoriality_df$species)
  ))

#my colors:
my_colors <- c("spring" = "#56ae6c", 
               "summer" = "#8960b3", 
               "winter" = "#b0923b", 
               "autumn" = "#ba495b")

my_colors_sp <- c("BLUTI" = "#b94b75",
                  "GRETI" = "#72ac5c",
                  "MARTI" = "#7f64b9",
                  "NUTHA" = "#bb7438")

#change the order of the levels within the season and change the name
pred_visits$season <- factor(pred_visits$season,
                             levels = c("summer", "spring", "autumn", "winter"),
                             labels = c("Summer", "Spring", "Autumn", "Winter"))
pred_days$season <- factor(pred_days$season,
                             levels = c("summer", "spring", "autumn", "winter"),
                             labels = c("Summer", "Spring", "Autumn", "Winter"))

#the two plots
pred_visits$species <- factor(pred_visits$species,
                              levels = c("BLUTI", "GRETI", "MARTI", "NUTHA"))
p1 <- ggplot(pred_visits, aes(x = season, y = Estimate,
                              color = species, group = species,
                              shape = species)) +
  geom_point(position = position_dodge(width = 0.4), size = 3) +
  geom_errorbar(aes(ymin = Q2.5, ymax = Q97.5),
                position = position_dodge(width = 0.4), width = 0.2) +
  scale_shape_manual(
    values = c("BLUTI" = 16, "GRETI" = 18, "MARTI" = 15, "NUTHA" = 17),
    labels = c("BLUTI" = "Blue tits",
               "GRETI" = "Great tits",
               "MARTI" = "Marsh tits",
               "NUTHA" = "Nuthatches")
  ) +
  scale_color_manual(
    values = my_colors_sp,
    labels = c("BLUTI" = "Blue tits",
               "GRETI" = "Great tits",
               "MARTI" = "Marsh tits",
               "NUTHA" = "Nuthatches"
               )
  ) +
  labs(y = "Estimated visits", x = "") +
  theme_minimal()

# Second plot
p2 <- ggplot(pred_days, aes(x = season, y = Estimate,
                            color = species, group = species,
                            shape = species)) +
  geom_point(position = position_dodge(width = 0.4), size = 3) +
  geom_errorbar(aes(ymin = Q2.5, ymax = Q97.5),
                position = position_dodge(width = 0.4), width = 0.2) +
  scale_shape_manual(
    values = c("BLUTI" = 16, "GRETI" = 18, "MARTI" = 15, "NUTHA" = 17),
    labels = c("BLUTI" = "Blue tits",
               "GRETI" = "Great tits",
               "MARTI" = "Marsh tits",
               "NUTHA" = "Nuthatches")
  ) +
  scale_color_manual(
    values = my_colors_sp,
    labels = c("BLUTI" = "Blue tits",
               "GRETI" = "Great tits",
               "MARTI" = "Marsh tits",
               "NUTHA" = "Nuthatches")
  ) +
  labs(y = "Estimated days", x = "") +
  theme_minimal()

#plot plot1 and2 together
ggarrange(
  p1, p2,
  labels = c("A", "B"),
  ncol = 2, nrow = 1,
  common.legend = TRUE,
  legend = "bottom"
)

# 3) Leadership probability -----------------------------------------------


# 3.1) Data prep ----------------------------------------------------------

# since we are interested in whether leadership probability is predicted by territoriality in mixed-species flocks when some birds are more territorial than others, we restrict the data set to groups with a minimum of 2 birds and those groups that have both territorial (nuthatches, marsh tits) and non-territorial (great tits, blue tits) birds

# remove group sizes of 1 
network.pos.all.seasons <- subset(network.pos.all.seasons, network.pos.all.seasons$flock_size>1)

# filter to only include mixed groups
# Define species categories
territorial <- c("MARTI", "NUTHA")
non_territorial <- c("GRETI", "BLUTI")

# Identify flocks with both territorial and non-territorial species
mixed_groups <- network.pos.all.seasons %>%
  group_by(group) %>%
  summarise(
    has_territorial = any(species %in% territorial),
    has_non_territorial = any(species %in% non_territorial),
    .groups = "drop"
  ) %>%
  filter(has_territorial & has_non_territorial) %>%
  pull(group)

# Filter original data
network.pos.all.seasons.sub <- network.pos.all.seasons %>%
  filter(group %in% mixed_groups)

# how many data points per group size per species?
table(network.pos.all.seasons.sub$species, network.pos.all.seasons.sub$flock_size)

# Summarize data at the PIT × season level
leadership_data <- network.pos.all.seasons.sub %>%
  # Convert Date.Time to Date class first (adjust format if needed)
  mutate(date = ymd_hms(as.character(Date.Time)) %>% as.Date()) %>%
  group_by(PIT, season, species, age_in_2020) %>%
  summarise(
    n_leader = sum(leader.follower == "leader"),
    n_total_log = log(n()),
    n_total = n(),
    expected_leaders = sum(1 / flock_size),
    degree_log = log(degree[1] + 1e-6),# we only have one value per season per bird, so we just the first one. We take the log and add a small constant for values of 0
    betweenness_log = log(betweenness[1] + 1e-6),
    n_unique_days = n_distinct(date),   # this serves as a territoriality measure
    .groups = "drop"
  )

head(leadership_data)

names(leadership_data)
# PIT: 10 digit alphanumeric code unique to each individual
# season: summer, spring, winter, autumn
# species: the 4 studied species (GRETI : Great tits, BLUTI: Blue tits, MARTI: Marsh tits, NUTHA: Nuthaches)
# age_in_2020: adults or juveniles
# n_feeders: the number of feeders (6 in total, in 6 different locations)
# n_leader: the total number of times an individual was the leader within a season
# n_total_log: the log of the total of birds within each season, species and age class. 
# expected leaders: the sum of the probabilities of leading within is flock. For example, bird X was the leader in a flock of group size 2, 4 and 6, thus its probability overall to lead is : 1/2 + 1/4 + 1/6 = 0.92
# degree_log: the log of the value of degree centrality for each individual
# betweenness_log: the log of the value of betweenness centrality for each individual
# n_unique_days: the number of unique days a bird was registered per season

# 3) Follower-leader dynamics ---------------------------------------------


# 3.1) Calculate VIFs -----------------------------------------------------

# we first run a simple regression (no interactions) to look at collinearity between predictors
model_order_vif <- lm(n_leader ~  species + season + age_in_2020 + scale(degree_log) + scale(betweenness_log), data= leadership_data)

vif(model_order_vif)

# GVIF Df GVIF^(1/(2*Df))
# species                1.228948  3        1.034957
# season                 1.850587  3        1.108030
# age_in_2020            1.138519  1        1.067014
# scale(degree_log)      1.840465  1        1.356637
# scale(betweenness_log) 1.333482  1        1.154765


# all vifs <5, so we can include all of them

#extract the VIF into a table (Word doc)
vif_lead_df <- as.data.frame(vif(model_order_vif))

vif_lead_df <- vif_lead_df %>%
  mutate(Variable = rownames(vif_lead_df)) %>%
  select(Variable, everything())

colnames(vif_lead_df) <- c("Variable", "GVIF", "Df", "GVIF^(1/(2*Df))")

vif_lead_df <- vif_lead_df %>%
  mutate(across(where(is.numeric), ~ round(., 2)))

vif_lead_df <- flextable(vif_lead_df) %>%
  autofit()

# Export to Word
doc <- read_docx()
doc <- body_add_flextable(doc, value = vif_lead_df)
print(doc, target = "Plots and tables/VIF_lead_Table.docx")


# 3.2) run the model -------------------------------------------------------

# we run a model with the number of times a bird was the leader in a given season.
# we include individual level predictors, as well as the expected number of times the bird is expected to be a leader by chance based on the flock sizes
# e.g. if a bird was in three flocks of 2,3 and 4 birds, the expected chance of being a leader across the three flocks is 1/2 + 1/3 + 1/4 = 1.08333

detach("package:rethinking", unload=TRUE)
leadership_model <- brm(
  formula = n_leader ~ season*species + scale(degree_log)*season + scale(betweenness_log)*season + season*age_in_2020 +
    offset(log(expected_leaders)) +
    (1 | PIT),
  data = leadership_data,
  family = poisson(),
  control = list(adapt_delta = 0.95, max_treedepth = 12),
  cores = 4, chains = 4, iter = 4000
)

summary(leadership_model)
save(leadership_model, file="model output/leadership_model.RDA")
load("model output/leadership_model.RDA")


# 3.3) Model checks -------------------------------------------------------


# check how well it fits
pp_check(leadership_model, ndraws = 100)
# looks like a good fit

plot(leadership_model)
# stationarity and mixing are good

# have a first look at the plot (conditional effects)
leadership_model_conditional_effects <- plot(conditional_effects(leadership_model, re_formula = NA))

# a 0 here means to a follower (baseline)
# formula = NA excludes random effects

# extract an R2 for our model
R2 <- performance::r2_bayes(leadership_model)
# # Bayesian R2 with Compatibility Interval
# 
# Conditional R2: 0.948 (95% CI [0.935, 0.958])
# Marginal R2: 0.858 (95% CI [0.818, 0.887])

# 3.4) Summary table ------------------------------------------------------


##make a table with the summary
summary_leadership_model <- summary(leadership_model)
fixed_effects <- as.data.frame(summary_leadership_model$fixed)
fixed_effects <- tibble::rownames_to_column(fixed_effects, var = "Term")
fixed_effects$Term <- c("Intercept", "Spring","Summer", "Winter",
                   "Great tit", "Marsh tit", "Nuthatch",
                   "Degree", "Betweenness", "Age (Juvenile)",
                   "Great tit x Spring", "Great tit x Summer", "Great tit x Winter",
                   "Marsh tit x Spring", "Marsh tit x Summer", "Marsh tit x Winter",
                   "Nuthatch x Spring","Nuthatch x Summer","Nuthatch x Winter",
                   "Spring × Degree", "Summer × Degree", "Winter × Degree",
                   "Spring × Betweenness", "Summer × Betweenness", "Winter × Betweenness",
                   "Spring x Age(Juvenile)", "Summer x Age(Juvenile)", "Winter x Age(Juvenile)" )
fixed_effects <- fixed_effects[, 1:(ncol(fixed_effects) - 2)]
colnames(fixed_effects)


fixed_effects <- fixed_effects %>%
  select("Term", "Estimate", "Est.Error", "l-95% CI", "u-95% CI", "Rhat") %>%
  rename(
    Estimate = "Estimate",
    Est_Error = "Est.Error",
    CI_Lower = "l-95% CI",
    CI_Upper = "u-95% CI",
    Rhat = Rhat
  )%>%
  mutate(Odds = exp(Estimate),
         Est_Error_Odds = exp(Est_Error),
         Odds_Lower = exp(CI_Lower),
         Odds_Upper = exp(CI_Upper))

fixed_effects <- fixed_effects %>%
  select("Term", "Est_Error_Odds", "Odds_Lower", "Odds_Upper", "Odds", "Rhat") 
  
fixed_effects <- fixed_effects %>%
  dplyr::mutate(across(where(is.numeric), ~ round(.x, 2)))

library(flextable)

flextable_table <- fixed_effects %>%
  flextable() %>%
  autofit() %>%
  set_caption("Summary of Fixed Effects from the final leadership Model")

library(officer)

doc_leadership_model <- read_docx() %>%
  body_add_flextable(flextable_table)

print(doc_leadership_model, target = "Plots and tables/fixed_effects_final_leadership_model.docx")


# 3.5) Plot ---------------------------------------------------------------

##plot of the summary output of the leadership_model
library(ggpubr)
network.pos.all.seasons$season <- as.factor(network.pos.all.seasons$season)
levels(network.pos.all.seasons$season)
network.pos.all.seasons$species <- as.factor(network.pos.all.seasons$species)
levels(network.pos.all.seasons$species)
network.pos.all.seasons$age_in_2020 <- as.factor(network.pos.all.seasons$age_in_2020)
levels(network.pos.all.seasons$age_in_2020)

my_colors <- c("Spring" = "#56ae6c", 
               "Summer" = "#8960b3", 
               "Winter" = "#b0923b", 
               "Autumn" = "#ba495b")
my_colors_sp <- c("BLUTI" = "#b94b75", 
                  "GRETI" = "#72ac5c", 
                  "MARTI" = "#7f64b9", 
                  "NUTHA" = "#bb7438")

my_colors_age <- c("adult" = "#9a5ea1", 
                   "juvenile" = "#98823c")

names(leadership_model_conditional_effects)

ggarrange(
  
  #First plot: interaction between species and season
  {
    p <- leadership_model_conditional_effects$`season:species`
    for (i in seq_along(p$layers)) {
      if (inherits(p$layers[[i]]$geom, "GeomPoint")) {
        p$layers[[i]]$aes_params$size <- 2.5  # smaller points
      }
    }
    p +
      aes(x = season, shape = species, color = species, fill = species) +
      scale_shape_manual(
        values = c("BLUTI" = 16, "GRETI" = 18, "MARTI" = 15, "NUTHA" = 17),
        labels = c("BLUTI" = "Blue tits", "GRETI" = "Great tits", "MARTI" = "Marsh tits", "NUTHA" = "Nuthatches"),
        name = NULL
      ) +
      scale_color_manual(
        values = my_colors_sp,
        labels = c("BLUTI" = "Blue tits", "GRETI" = "Great tits", "MARTI" = "Marsh tits", "NUTHA" = "Nuthatches"),
        name = NULL
      ) +
      scale_fill_manual(
        values = my_colors_sp,
        labels = c("BLUTI" = "Blue tits", "GRETI" = "Great tits", "MARTI" = "Marsh tits", "NUTHA" = "Nuthatches"),
        name = NULL
      ) +
      scale_x_discrete(
        limits = c("summer", "autumn", "winter", "spring"),
        labels = c("summer" = "Summer", "autumn" = "Autumn", "winter" = "Winter", "spring" = "Spring")
      ) +
      labs(x = "Season", y = "Leadership probability")  +
      coord_cartesian(ylim = c(4.5, 25))+
      theme(
        legend.position = "bottom",
        legend.title = element_text(),
        axis.title.x = element_blank()
      )
  },
  
  
   #Second plot: degree centrality:season
  {
    ce_degree <- leadership_model_conditional_effects$`degree_log:season`$data
    ce_degree$season <- factor(ce_degree$season, 
                               levels = c("summer", "autumn", "winter", "spring"),  
                               labels = c("Summer", "Autumn", "Winter", "Spring"))
    
    ggplot(ce_degree, aes(x = scale(degree_log), y = estimate__, color = season, fill = season)) +
      geom_ribbon(aes(ymin = lower__, ymax = upper__), alpha = 0.2, color = NA) +
      geom_line(size = 1) +
      scale_color_manual(values = my_colors) +
      scale_fill_manual(values = my_colors) +
      labs(x = "Degree centrality", y = "") +
      coord_cartesian(ylim = c(4.5, 25)) +
      theme_bw() +
      theme(
        legend.position = "bottom",
        legend.title = element_blank()
      )
  },
  
  labels = c("a", "b"),
  ncol = 2, nrow = 1,
  common.legend = FALSE
)



ggsave(file="Plots and tables/plot_leadership_model.tiff", width=8.5, height=6.5)


#display.brewer.all()

#This shows me that the model codes 0 as follower and 1 as leader. So an observation that at the y axis (leader.follower) has 0.7 this means that that observation has a 70% chance to be a leader.  


# 3.6) Compute species comparison -----------------------------------------

# the estimates are expressed in their logit scale so here transform them into odds ratio's
exp(fixef(leadership_model))

#                                    Estimate Est.Error      Q2.5     Q97.5
##Intercept                         3.3053261  1.135883 2.5562041 4.2175585
#seasonspring                      0.9091121  1.146286 0.6961262 1.1950550
#seasonsummer                      0.7202703  1.274143 0.4488387 1.1476357
#seasonwinter                      0.4767395  1.153628 0.3613298 0.6322333
#speciesGRETI                      0.7916532  1.166918 0.5837240 1.0750536
#speciesMARTI                      1.1070372  1.156872 0.8322133 1.4817604
#speciesNUTHA                      1.1115076  1.187683 0.7963964 1.5601706
##scaledegree_log                   1.0242657  1.072885 0.8925439 1.1811882
#scalebetweenness_log              0.9973700  1.047823 0.9101231 1.0955589
#age_in_2020juvenile               1.1412961  1.137924 0.8874184 1.4716207
#seasonspring:speciesGRETI         1.1565898  1.182819 0.8356651 1.6088075
#seasonsummer:speciesGRETI         1.6358592  1.303827 0.9784839 2.7512480
#seasonwinter:speciesGRETI         1.3926503  1.179322 1.0061364 1.9309790
#seasonspring:speciesMARTI         0.8970535  1.180603 0.6480150 1.2355219
#seasonsummer:speciesMARTI         1.3819361  1.284749 0.8492360 2.2684873
#seasonwinter:speciesMARTI         1.3716449  1.168108 1.0120368 1.8594987
#seasonspring:speciesNUTHA         0.9130505  1.237113 0.5993952 1.3910933
#seasonsummer:speciesNUTHA         1.4568884  1.297078 0.8824247 2.4506007
#seasonwinter:speciesNUTHA         1.2447061  1.202419 0.8715141 1.7834143
#seasonspring:scaledegree_log      0.8351132  1.081219 0.7142656 0.9742751
#seasonsummer:scaledegree_log      0.9433804  1.100244 0.7845292 1.1342963
#seasonwinter:scaledegree_log      0.8342330  1.082512 0.7132584 0.9727839
#seasonspring:scalebetweenness_log 1.0562147  1.066578 0.9289719 1.1966151
#seasonsummer:scalebetweenness_log 0.9502962  1.075599 0.8263775 1.0993690
#seasonwinter:scalebetweenness_log 1.0686729  1.063275 0.9495384 1.2043441
#seasonspring:age_in_2020juvenile  0.9097835  1.154281 0.6869015 1.2098501
#seasonsummer:age_in_2020juvenile  0.9439511  1.171106 0.6934001 1.2856321
#seasonwinter:age_in_2020juvenile  0.9384690  1.145778 0.7148197 1.2243158


# in our model, blue tits are the baseline species to which all others are compared. We would like to compute comparisons between all species. 


# Compute marginal means for the species variable
species_emm <- emmeans(leadership_model, ~ species)
# Pairwise comparisons between species levels
species_contrasts <- contrast(species_emm, method = "pairwise")
species_contrasts

#  contrast      estimate lower.HPD upper.HPD
#BLUTI - GRETI  0.02642    -0.188   0.23444
#BLUTI - MARTI -0.21430    -0.421   0.00591
#BLUTI - NUTHA -0.21983    -0.458   0.03469
#GRETI - MARTI -0.23994    -0.405  -0.06383
#GRETI - NUTHA -0.24428    -0.450  -0.04947
#MARTI - NUTHA -0.00646    -0.203   0.19749
# Results are averaged over the levels of: season, age_in_2020 
# Point estimate displayed: median 
# Results are given on the log odds ratio (not the response) scale. 
# HPD interval probability: 0.95


# Marginal means for species by season
species_season_emm <- emmeans(leadership_model, ~ species | season)
# Pairwise comparisons within each season
species_season_contrasts <- contrast(species_season_emm, method = "pairwise")
species_season_contrasts

#season = autumn:
#contrast      estimate lower.HPD upper.HPD
#BLUTI - GRETI -0.01390    -0.502    0.5003
#BLUTI - MARTI -0.27714    -0.748    0.2025
#BLUTI - NUTHA -0.32698    -0.872    0.1592
#GRETI - MARTI -0.26302    -0.674    0.1478
#GRETI - NUTHA -0.31631    -0.760    0.1001
#MARTI - NUTHA -0.05427    -0.369    0.2734

#season = spring:
#  contrast      estimate lower.HPD upper.HPD
#BLUTI - GRETI  0.14348    -0.112    0.4113
#BLUTI - MARTI  0.00836    -0.273    0.2842
#BLUTI - NUTHA  0.08230    -0.302    0.4663
#GRETI - MARTI -0.13709    -0.440    0.1430
#GRETI - NUTHA -0.06894    -0.424    0.3144
#MARTI - NUTHA  0.06739    -0.331    0.4785

#season = summer:
##  contrast      estimate lower.HPD upper.HPD
#BLUTI - GRETI -0.02733    -0.605    0.5320
#BLUTI - MARTI -0.25879    -0.813    0.2834
#BLUTI - NUTHA -0.33055    -0.883    0.2469
#GRETI - MARTI -0.22844    -0.541    0.1038
#GRETI - NUTHA -0.30014    -0.658    0.0598
#MARTI - NUTHA -0.06949    -0.415    0.2490

#season = winter:
##  contrast      estimate lower.HPD upper.HPD
#BLUTI - GRETI  0.00358    -0.155    0.1613
#BLUTI - MARTI -0.32149    -0.510   -0.1296
#BLUTI - NUTHA -0.29492    -0.536   -0.0628
#GRETI - MARTI -0.32537    -0.486   -0.1648
#GRETI - NUTHA -0.29824    -0.507   -0.0712
#MARTI - NUTHA  0.02766    -0.195    0.2515


#Results are averaged over the levels of: age_in_2020 
#Point estimate displayed: median 
#Results are given on the log (not the response) scale. 
#HPD interval probability: 0.95 


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
exp(species_season_contrasts_df$estimate) 
# an odds ratio of 1.67 means that e.g. BLUTI are 1.67 times more likely to be the leader compared to GRETI
# If one were interested in probabilities for a certain species to be a leader, you calculate as: probability = exp(log odds) / (1 + exp(log odds)) = exp(estimate)/(1+ exp(estimate))
species_season_contrasts_df <- species_season_contrasts_df %>%
  mutate(
    odds = exp(estimate),
    odds_lower = exp(lower.HPD),
    odds_upper = exp(upper.HPD)
  )
emm_table <- species_season_contrasts_df %>%
  dplyr::select(contrast, season, odds_lower, odds_upper, odds) 

#extract the dataframe into a table for Word
species_names <- c(
  "BLUTI" = "Blue tit",
  "GRETI" = "Great tit",
  "MARTI" = "Marsh tit",
  "NUTHA" = "Nuthatch"
)
library(stringr)
emm_table <- emm_table %>%
  mutate(contrast = str_replace_all(contrast, species_names))
colnames(emm_table) <- c("Contrast", "Season", "odds_lower", "odds_upper", "Odds", "Odds_SE")
emm_table <- emm_table %>%
  dplyr::mutate(across(where(is.numeric), ~ round(.x, 2)))

pairwise_table <- flextable(emm_table) %>%
  autofit() %>%
  set_caption("Pairwise Contrasts of Species by Season")

# Export to Word (optional)
library(officer)
doc_pariwise_comparisons <- read_docx() %>%
  body_add_flextable(pairwise_table)

print(doc_pariwise_comparisons, target = "Plots and tables/pairwise_contrasts_leadership_model.docx")

# for age

# Marginal means for species by season
age_season_emm <- emmeans(leadership_model, ~ age_in_2020 | season)
plot(age_season_emm)
# Pairwise comparisons within each season
age_season_contrasts <- contrast(age_season_emm, method = "pairwise")
age_season_contrasts 
#no differences in age across seasons
#season = autumn:
#contrast         estimate lower.HPD upper.HPD
#adult - juvenile -0.00217    -0.326    0.3268

#season = spring:
# contrast         estimate lower.HPD upper.HPD
#adult - juvenile -0.15353    -0.391    0.0901

#season = summer:
# contrast         estimate lower.HPD upper.HPD
#adult - juvenile -0.20309    -0.523    0.1187

#season = winter:
# contrast         estimate lower.HPD upper.HPD
#adult - juvenile -0.05538    -0.191    0.0622

#Results are averaged over the levels of: species 
#Point estimate displayed: median 
#Results are given on the log (not the response) scale. 
#HPD interval probability: 0.95 


# 5) Extract flock sizes in each season -------------------------------------------------------------------

colnames(network.pos.all.seasons)
network.pos.all.seasons %>%
  dplyr::group_by(season) %>%
  dplyr::filter(flock_size >= 2) %>%
  dplyr::summarise(num_groups = n_distinct(group))
  
# # A tibble: 4 × 2
# season num_groups
# <fct>       <int>
#   1 autumn        860
# 2 spring       1453
# 3 summer       1223
# 4 winter       2268

#how many individuals within each flock in each season when min flock size is 2?
network.pos.all.seasons.sub %>%
  filter(season == "summer", flock_size >= 2) %>%
  distinct(group, flock_size, season) %>%
  summarise(
    min_flock_size = min(flock_size),
    max_flock_size = max(flock_size)
  )

# min_flock_size max_flock_size
# 1              2             15

network.pos.all.seasons %>%
  filter(season == "autumn", flock_size >= 2) %>%
  distinct(group, flock_size) %>%
  summarise(
    min_flock_size = min(flock_size),
    max_flock_size = max(flock_size)
  )

# min_flock_size max_flock_size
# 1              2             10        

network.pos.all.seasons %>%
  filter(season == "winter", flock_size >= 2) %>%
  distinct(group, flock_size) %>%
  summarise(
    min_flock_size = min(flock_size),
    max_flock_size = max(flock_size)
  )

# min_flock_size max_flock_size
# 1              2             36

network.pos.all.seasons %>%
  filter(season == "spring", flock_size >= 2) %>%
  distinct(group, flock_size) %>%
  summarise(
    min_flock_size = min(flock_size),
    max_flock_size = max(flock_size)
  )

# min_flock_size max_flock_size
# 1              2             22


# 6) Extract number of birds in each season ------------------------------------------------------------------

#number of birds, per season, per species in the whole dataset, even for flocks of 1 bird
network.pos.all.seasons %>%
  dplyr::filter(flock_size>=1) %>%
  dplyr::group_by(season) %>%
  dplyr::summarise(num_PIT = n_distinct(PIT))

network.pos.all.seasons %>%
  dplyr::filter(flock_size>=1) %>%
  dplyr::group_by(season, species) %>%
  dplyr::summarise(num_PIT = n_distinct(PIT))
# autumn BLUTI        14
# autumn GRETI        47
# autumn MARTI         9
# autumn NUTHA         3
# spring BLUTI        43
# spring GRETI        64
# spring MARTI        10
# spring NUTHA         5
# summer BLUTI        12
# summer GRETI        80
# summer MARTI         7
# summer NUTHA         6
# winter BLUTI        48
# winter GRETI       108
# winter MARTI        13
# winter NUTHA         6


#number of birds per species, per season, 
#   when flock_size min 2 and both types of species (territorial and non territorial) are present

network.pos.all.seasons.sub %>%
  dplyr::group_by(season) %>%
  dplyr::summarise(num_PIT = n_distinct(PIT))
# autumn      71
# spring     119
# summer     96
# winter     175

#Number of birds per species, per season
network.pos.all.seasons.sub %>%
  dplyr::group_by(species) %>%
  dplyr::summarise(num_PIT = n_distinct(PIT))

#Number juvenimes and adults of birds per species
network.pos.all.seasons.sub %>%
  dplyr::group_by(species, age_in_2020) %>%
  dplyr::summarise(num_PIT = n_distinct(PIT))



#number of flocks per season
network.pos.all.seasons.sub %>%
  dplyr::group_by(season) %>%
  dplyr::summarise(num_group = n_distinct(group))

#number of birds per season
network.pos.all.seasons.sub %>%
  dplyr::group_by(season, species) %>%
  dplyr::summarise(num_PIT = n_distinct(PIT))

#flock size in each season
network.pos.all.seasons.sub %>%
  filter(season == "autumn") %>%
  distinct(group, flock_size) %>%
  summarise(
    min_flock_size = min(flock_size),
    max_flock_size = max(flock_size)
  )

network.pos.all.seasons.sub %>%
  filter(season == "winter") %>%
  distinct(group, flock_size) %>%
  summarise(
    min_flock_size = min(flock_size),
    max_flock_size = max(flock_size)
  )

network.pos.all.seasons.sub %>%
  filter(season == "spring") %>%
  distinct(group, flock_size) %>%
  summarise(
    min_flock_size = min(flock_size),
    max_flock_size = max(flock_size)
  )

network.pos.all.seasons.sub %>%
  filter(season == "summer") %>%
  distinct(group, flock_size) %>%
  summarise(
    min_flock_size = min(flock_size),
    max_flock_size = max(flock_size)
  )
