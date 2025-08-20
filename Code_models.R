# Seasonal variation in leadership in mixed-species flocks of territorial and non-territorial birds
#Emilie Zimmer1 & Sonja Wild2†
#1Department of Biology, Ecology and Evolution, University of Liège, Liège B-4020, Belgium
#2Department of Environmental Science and Policy, University of California, Davis, CA, 95616, USA
#† Corresponding author: swild@ucdavis.edu



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


# extract a few numbers:
length(unique(network.pos.all.seasons$PIT))
# 234 birds

length(unique(network.pos.all.seasons$PIT[network.pos.all.seasons$species=="GRETI"]))
# 144
length(unique(network.pos.all.seasons$PIT[network.pos.all.seasons$species=="BLUTI"]))
# 66
length(unique(network.pos.all.seasons$PIT[network.pos.all.seasons$species=="MARTI"]))
# 15
length(unique(network.pos.all.seasons$PIT[network.pos.all.seasons$species=="NUTHA"]))
# 9

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

# 2.2) do a PCA on number of days present and average number of visits to a feeder -------------------------------------------

pca_res <- prcomp(
  cbind(territoriality_df$n_unique_days/9, territoriality_df$visits_per_feeder),
  scale. = TRUE   # scale variables to unit variance
)

summary(pca_res)

# Importance of components:
#   PC1    PC2
# Standard deviation     1.2707 0.6207
# Proportion of Variance 0.8074 0.1926
# Cumulative Proportion  0.8074 1.0000

# extract the first component
pc1 <- pca_res$x[, 1]  # first principal component

pca_res$rotation[, 1]  # loadings for PC1
# [1] 0.7071068 0.7071068
# they  both load in the same direction

# Add PC1 as a new column to our data
territoriality_df$PC1_territoriality <- pca_res$x[, 1]


# 2.3) Run model territoriality -------------------------------------------

# we run a model with the territoriality pca as the outcome variable to validate species-level and seasonal patterns of territoriality.

model_territoriality_pca <- brm(
  PC1_territoriality ~ season * species + (1 | PIT),
  family = gaussian,
  data = territoriality_df,
  cores = 4,
  chains = 4,
  iter = 4000
)

#save(model_territoriality_pca, file="model output/model_territoriality_pca.RDA")
load("model output/model_territoriality_pca.RDA")


# 2.4) Summary -------------------------------------------
###Extract the summary output into a table (Table S2)
sum_mod <- summary(model_territoriality_pca)
fixed_effects <- as.data.frame(sum_mod$fixed)
fixed_effects <- tibble::rownames_to_column(fixed_effects, var = "Term")
fixed_effects$Term <- c("Intercept", "Spring","Summer", "Winter",
                        "Great tit", "Marsh tit", "Nuthatch",
                        "Great tit x Spring", "Great tit x Summer", "Great tit x Winter",
                        "Marsh tit x Spring", "Marsh tit x Summer","Marsh tit x Winter", 
                        "Nuthatch x Spring","Nuthatch x Summer","Nuthatch x Winter" 
                         )
fixed_effects <- fixed_effects[, 1:(ncol(fixed_effects) - 2)]
colnames(fixed_effects)


terr_table <- fixed_effects %>%
  select("Term", "Estimate", "l-95% CI", "u-95% CI", "Rhat") %>%
  rename(
    Estimate = "Estimate",
    CI_Lower = "l-95% CI",
    CI_Upper = "u-95% CI",
    Rhat = Rhat
  )%>%
  mutate(
    Odds = exp(Estimate),
    CI_Lower_Odds = exp(CI_Lower),
    CI_Upper_Odds = exp(CI_Upper)
  )

terr_table <- terr_table %>%
  dplyr::mutate(across(where(is.numeric), ~ round(.x, 2)))

terr_table <- terr_table %>%
  select("Term", "Odds", "CI_Lower_Odds", "CI_Upper_Odds", "Rhat")

terr_table <- flextable(terr_table) %>%
  autofit()

# 6. Export to Word
doc <- read_docx()
doc <- body_add_flextable(doc, value = terr_table)
print(doc, target = "Plots and tables/model_territoriality_summary.docx")


# 2.3) Model checks -------------------------------------------------------
pp_check(model_territoriality_pca, ndraws = 100)
# the fit is okay - even though there are likely variables that explain territoriality that are not incluced in the model

plot(model_territoriality_pca)
# mixture and stationarity is good

R2 <- performance::r2_bayes(model_territoriality_pca)
R2
# Conditional R2: 0.448 (95% CI [0.367, 0.521])
# Marginal R2: 0.244 (95% CI [0.183, 0.302])



# 2.4) Extract effects  ---------------------------------------------------
#emmeans
territoriality_emm <- emmeans(model_territoriality_pca, ~ species)
territoriality_contrasts <- contrast(territoriality_emm, method = "pairwise")
# contrast      estimate lower.HPD upper.HPD
# BLUTI - GRETI   0.0352    -0.290     0.354
# BLUTI - MARTI  -1.3224    -1.859    -0.798
# BLUTI - NUTHA  -1.3923    -2.052    -0.703
# GRETI - MARTI  -1.3539    -1.855    -0.880
# GRETI - NUTHA  -1.4262    -2.042    -0.776
# MARTI - NUTHA  -0.0735    -0.828     0.698
# 
# Results are averaged over the levels of: season 
# Point estimate displayed: median 
# HPD interval probability: 0.95 

#emmeans within each season (Table S3)
terr_season_emm <- emmeans(model_territoriality_pca, ~ species | season)
terr_species_season_contrasts <- contrast(terr_season_emm, method = "pairwise")
terr_species_season_contrasts

# season = autumn:
#   contrast      estimate lower.HPD upper.HPD
# BLUTI - GRETI   0.2669    -0.386    0.9173
# BLUTI - MARTI  -1.3350    -2.241   -0.3875
# BLUTI - NUTHA  -2.4193    -3.761   -1.0313
# GRETI - MARTI  -1.5990    -2.351   -0.7600
# GRETI - NUTHA  -2.6797    -3.957   -1.4300
# MARTI - NUTHA  -1.0802    -2.490    0.3524
# 
# season = spring:
#   contrast      estimate lower.HPD upper.HPD
# BLUTI - GRETI   0.3094    -0.159    0.7132
# BLUTI - MARTI  -0.5115    -1.314    0.2363
# BLUTI - NUTHA  -0.1939    -1.250    0.8260
# GRETI - MARTI  -0.8252    -1.556   -0.0541
# GRETI - NUTHA  -0.4926    -1.524    0.5553
# MARTI - NUTHA   0.3243    -0.935    1.4838
# 
# season = summer:
#   contrast      estimate lower.HPD upper.HPD
# BLUTI - GRETI  -0.5353    -1.172    0.1063
# BLUTI - MARTI  -1.3397    -2.373   -0.3488
# BLUTI - NUTHA  -1.3553    -2.467   -0.2883
# GRETI - MARTI  -0.8033    -1.680    0.0257
# GRETI - NUTHA  -0.8265    -1.788    0.1014
# MARTI - NUTHA  -0.0209    -1.187    1.2351
# 
# season = winter:
#   contrast      estimate lower.HPD upper.HPD
# BLUTI - GRETI   0.1058    -0.273    0.5067
# BLUTI - MARTI  -2.0852    -2.768   -1.3526
# BLUTI - NUTHA  -1.6047    -2.566   -0.6212
# GRETI - MARTI  -2.1921    -2.889   -1.5592
# GRETI - NUTHA  -1.7080    -2.615   -0.7326
# MARTI - NUTHA   0.4941    -0.666    1.5655
# 
# Point estimate displayed: median 
# HPD interval probability: 0.95 

plot(territoriality_emm)
plot(terr_season_emm)

#extracting the results to make a table
terr_species_season_contrasts_df <- as.data.frame(terr_species_season_contrasts)
head(terr_species_season_contrasts_df)
terr_emm_table <- terr_species_season_contrasts_df %>%
  dplyr::select(contrast, season, estimate, lower.HPD, upper.HPD)
species_names <- c(
  "BLUTI" = "Blue tit",
  "GRETI" = "Great tit",
  "MARTI" = "Marsh tit",
  "NUTHA" = "Nuthatch")
season_names <- c(
  "autumn" = "Autumn",
  "winter" = "Winter",
  "spring" = "Spring",
  "summer" = "Summer")

terr_emm_table <- terr_emm_table %>%
  rename(Contrast = "contrast",
    Season = "season",
    CI_Lower = "lower.HPD",
    CI_Upper = "upper.HPD"
  )%>%
  mutate(
    Odds = exp(estimate),
    CI_Lower_Odds = exp(CI_Lower),
    CI_Upper_Odds = exp(CI_Upper)
  )
terr_emm_table <- terr_emm_table%>%
  select(c(Contrast, Season, Odds, CI_Lower_Odds, CI_Upper_Odds))
terr_emm_table <- terr_emm_table %>%
  mutate(Contrast = str_replace_all(Contrast, species_names))
terr_emm_table <- terr_emm_table %>%
  mutate(Season = str_replace_all(Season, season_names))

terr_emm_table <- terr_emm_table %>%
  dplyr::mutate(across(where(is.numeric), ~ round(.x, 2)))

terr_pairwise_table <- flextable(terr_emm_table) %>%
  autofit() %>%
  set_caption("Territoriality Pairwise Contrasts of Species by Season")


doc_pariwise_comparisons_terr <- read_docx() %>%
  body_add_flextable(terr_pairwise_table)

print(doc_pariwise_comparisons_terr, target = "Plots and tables/pairwise_contrasts_territoriality.docx")


# 2.5) Plot ---------------------------------------------------------------

plot(conditional_effects(model_territoriality_pca))
#my colors:
my_colors <- c("spring" = "#56ae6c", 
               "summer" = "#8960b3", 
               "winter" = "#b0923b", 
               "autumn" = "#ba495b")

my_colors_sp <- c("BLUTI" = "#b94b75",
                  "GRETI" = "#72ac5c",
                  "MARTI" = "#7f64b9",
                  "NUTHA" = "#bb7438")

pred_terr <- fitted(
  model_territoriality_pca,
  newdata = expand.grid(
    season = unique(territoriality_df$season),
    species = unique(territoriality_df$species),
    PIT = NA   # random effect set to population-level
  ),
  re_formula = NA,   # exclude random effects
  summary = TRUE
) %>%
  as.data.frame() %>%
  bind_cols(expand.grid(
    season = unique(territoriality_df$season),
    species = unique(territoriality_df$species)
  ))

#change the order of the levels within the season and change the name
pred_terr$season <- factor(pred_terr$season,
                             levels = c("summer", "autumn", "winter", "spring"),
                             labels = c("Summer", "Autumn", "Winter", "Spring"))

pred_terr$species <- factor(pred_terr$species,
                              levels = c("BLUTI", "GRETI", "MARTI", "NUTHA"))
ggplot(pred_terr, aes(x = season, y = Estimate,
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
  labs(y = "Territoriality [PC1 score]", x = "") +
  theme_minimal() + theme(legend.title = element_blank())

ggsave(file="Plots and tables/plot_territoriality_sp_season_Fig1.tiff", width=4.5, height=3.5, bg = "white")


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

# Table S1:

# GVIF Df GVIF^(1/(2*Df))
# species                1.228948  3        1.034957
# season                 1.850587  3        1.108030
# age_in_2020            1.138519  1        1.067014
# scale(degree_log)      1.840465  1        1.356637
# scale(betweenness_log) 1.333482  1        1.154765


# all vifs <5, so we can include all of them


# 3.2) run the model -------------------------------------------------------

# we run a model with the number of times a bird was the leader in a given season.
# we include individual level predictors, as well as the expected number of times the bird is expected to be a leader by chance based on the flock sizes
# e.g. if a bird was in three flocks of 2,3 and 4 birds, the expected chance of being a leader across the three flocks is 1/2 + 1/3 + 1/4 = 1.08333

leadership_model <- brm(
  formula = n_leader ~ season*species + season*scale(degree_log) + season*scale(betweenness_log) + season*age_in_2020 +
    offset(log(expected_leaders)) +
    (1 | PIT),
  data = leadership_data,
  family = poisson(),
  control = list(adapt_delta = 0.95, max_treedepth = 12),
  cores = 4, chains = 4, iter = 4000
)

summary(leadership_model)
#save(leadership_model, file="model output/leadership_model.RDA")
load("model output/leadership_model.RDA")


# 3.3) Model checks -------------------------------------------------------


# check how well it fits
pp_check(leadership_model, ndraws = 100)
# looks like a good fit

plot(leadership_model)
# stationarity and mixing are good

# have a first look at the plot (conditional effects)
leadership_model_conditional_effects <- plot(conditional_effects(leadership_model, re_formula = NA))
# formula = NA excludes random effects

# extract an R2 for our model
R2 <- performance::r2_bayes(leadership_model)
# # Bayesian R2 with Compatibility Interval
# 
# Conditional R2: 0.947 (95% CI [0.932, 0.959])
# Marginal R2: 0.909 (95% CI [0.877, 0.931])

# 3.4) Summary table ------------------------------------------------------

##make a table with the summary (Table S4)
summary_leadership_model <- summary(leadership_model)
fixed_effects <- as.data.frame(summary_leadership_model$fixed)
fixed_effects <- tibble::rownames_to_column(fixed_effects, var = "Term")
fixed_effects$Term <- c("Intercept", "Spring","Summer", "Winter",
                   "Great tit", "Marsh tit", "Nuthatch",
                   "Degree", "Betweenness", "Age (Juvenile)",  
                   "Great tit x Spring", "Great tit x Summer", "Great tit x Winter",
                   "Marsh tit x Spring", "Marsh tit x Summer", "Marsh tit x Winter",
                   "Nuthatch x Spring", "Nuthatch x Summer","Nuthatch x Winter", 
                   "Degree x Spring", "Degree x Summer", "Degree x Winter",   
                   "Betweenness x Spring", "Betweenness x Summer", "Betweenness x Winter",   
                   "Age(Juvenile) x Spring", "Age(Juvenile) x Summer", "Age(Juvenile) x Winter" )
fixed_effects <- fixed_effects[, 1:(ncol(fixed_effects) - 2)]
colnames(fixed_effects)
lead_table <- fixed_effects %>%
  select("Term", "Estimate", "l-95% CI", "u-95% CI", "Rhat") %>%
  rename(
    CI_Lower = "l-95% CI",
    CI_Upper = "u-95% CI",
    Rhat = Rhat
  )%>%
  mutate(
    Odds = exp(Estimate),
    CI_Lower_Odds = exp(CI_Lower),
    CI_Upper_Odds = exp(CI_Upper)
  )

lead_table <- lead_table %>%
  dplyr::mutate(across(where(is.numeric), ~ round(.x, 2)))

lead_table <- lead_table %>%
  select("Term", "Odds", "CI_Lower_Odds", "CI_Upper_Odds", "Rhat")

lead_table <- flextable(lead_table) %>%
  autofit()

# 6. Export to Word
doc <- read_docx()
doc_leadership_model <- body_add_flextable(doc, value = lead_table)
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

my_colors <- c("spring" = "#56ae6c", 
               "summer" = "#8960b3", 
               "winter" = "#b0923b", 
               "autumn" = "#ba495b")

my_colors_bis <- c("Spring" = "#56ae6c", 
               "Summer" = "#8960b3", 
               "Winter" = "#b0923b", 
               "Autumn" = "#ba495b")


my_colors_sp <- c("BLUTI" = "#b94b75", 
                  "GRETI" = "#72ac5c", 
                  "MARTI" = "#7f64b9", 
                  "NUTHA" = "#bb7438")

my_colors_age <- c("Adult" = "#9a5ea1", 
                   "Juvenile" = "#98823c")


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
      labs(x = "", y = "Leadership events") +
      theme_bw() +
      theme(legend.position = "bottom") +
          coord_cartesian()
      
  },
  
  # Second plot: interaction between season and age
  {
    p <- leadership_model_conditional_effects$`season:age_in_2020`
    for (i in seq_along(p$layers)) {
      if (inherits(p$layers[[i]]$geom, "GeomPoint")) {
        p$layers[[i]]$aes_params$size <- 2.5
      }
    }
    p$data$season <- factor(p$data$season,
                            levels = c("summer", "autumn", "winter", "spring"),
                            labels = c("Summer", "Autumn", "Winter", "Spring"))
    p$data$age_in_2020 <- factor(p$data$age_in_2020,
                                 levels = c("adult", "juvenile"),
                                 labels = c("Adult", "Juvenile"))
    p +  aes(x = season, y = estimate__, color = age_in_2020, fill = age_in_2020) +
      geom_ribbon(aes(ymin = lower__, ymax = upper__), alpha = 0.2, color = NA) +
      geom_line(size = 1) +
      scale_color_manual(values = my_colors_age) +
      scale_fill_manual(values = my_colors_age) +
      labs(x = "", y = "") +
      theme_bw() +
      theme(legend.position = "bottom", legend.title = element_blank()) + 
      coord_cartesian()
    
    
  },
  # Third plot: interaction between season and betweenness (continuous)
  {
    ce_between <- leadership_model_conditional_effects$`betweenness_log:season`$data
    ce_between$season <- factor(ce_between$season, 
                               levels = c("summer", "autumn", "winter", "spring"),
                               labels = c("Summer", "Autumn", "Winter", "Spring"))
    
    ggplot(ce_between, aes(x = betweenness_log, y = estimate__, color = season, fill = season)) +
      geom_ribbon(aes(ymin = lower__, ymax = upper__), alpha = 0.2, color = NA) +
      geom_line(size = 1) +
      scale_color_manual(values = my_colors_bis) +
      scale_fill_manual(values = my_colors_bis) +
      labs(x = "Betweenness centrality [log]", y = "Leadership events") +
      theme_bw() +
      theme(legend.position = "bottom", legend.title = element_blank() ) +
      coord_cartesian() 
      
  },
  
  
  # fourth plot: interaction between season and degree
  {
    ce_degree <- leadership_model_conditional_effects$`degree_log:season`$data 
    ce_degree$season <- factor(ce_degree$season, 
                               levels = c("summer", "autumn", "winter", "spring"),
                               labels = c("Summer", "Autumn", "Winter", "Spring"))
    ggplot(ce_degree, aes(x = degree_log, y = estimate__, color = season, fill = season)) +
      geom_ribbon(aes(ymin = lower__, ymax = upper__), alpha = 0.2, color = NA) +
      geom_line(size = 1) +
      scale_color_manual(values = my_colors_bis) +
      scale_fill_manual(values = my_colors_bis) +
      labs(x = "Degree centrality [log]", y = "") +
      theme_bw() +
      theme(
        legend.position = "bottom",
        legend.title = element_blank() ) +
      coord_cartesian()
  },
  
  labels = c("a", "b", "c", "d"),
  ncol = 2, nrow = 2,
  widths = c(1, 1),   
  heights = c(1, 1),
  common.legend = FALSE
)

ggsave(file="Plots and tables/plot_leadership_model_Fig2.tiff", width=8.5, height=6.5)


#display.brewer.all()

#This shows me that the model codes 0 as follower and 1 as leader. So an observation that at the y axis (leader.follower) has 0.7 this means that that observation has a 70% chance to be a leader.  


# 3.6) Compute species comparison -----------------------------------------
# in our model, blue tits are the baseline species to which all others are compared. We would like to compute comparisons between all species. 

# Compute marginal means for the species variable
species_emm <- emmeans(leadership_model, ~ species)
# Pairwise comparisons between species levels
species_contrasts <- contrast(species_emm, method = "pairwise")
species_contrasts

# contrast      estimate lower.HPD upper.HPD
# BLUTI - GRETI  0.02175    -0.183   0.23799
# BLUTI - MARTI -0.21546    -0.428   0.00582
# BLUTI - NUTHA -0.22012    -0.467   0.01835
# GRETI - MARTI -0.24016    -0.408  -0.06538
# GRETI - NUTHA -0.24009    -0.441  -0.04379
# MARTI - NUTHA -0.00235    -0.208   0.19665
# 
# Results are averaged over the levels of: season, age_in_2020 
# Point estimate displayed: median 
# Results are given on the log (not the response) scale. 
# HPD interval probability: 0.95


# Marginal means for species by season (Table 1)
species_season_emm <- emmeans(leadership_model, ~ species | season)
# Pairwise comparisons within each season
species_season_contrasts <- contrast(species_season_emm, method = "pairwise")
species_season_contrasts

#season = autumn:
#  contrast      estimate lower.HPD upper.HPD
#BLUTI - GRETI -0.02760    -0.584    0.4692
#BLUTI - MARTI -0.28718    -0.804    0.1805
#BLUTI - NUTHA -0.34066    -0.884    0.1616
#GRETI - MARTI -0.26045    -0.670    0.1606
#GRETI - NUTHA -0.31037    -0.756    0.1053
#MARTI - NUTHA -0.05172    -0.380    0.2600

#season = spring:
#  contrast      estimate lower.HPD upper.HPD
#BLUTI - GRETI  0.14791    -0.111    0.4240
#BLUTI - MARTI  0.01100    -0.269    0.2914
#BLUTI - NUTHA  0.08813    -0.269    0.4844
#GRETI - MARTI -0.13594    -0.437    0.1470
#GRETI - NUTHA -0.05708    -0.411    0.3162
#MARTI - NUTHA  0.08007    -0.304    0.4896

#season = summer:
#  contrast      estimate lower.HPD upper.HPD
#BLUTI - GRETI -0.02218    -0.595    0.5525
#BLUTI - MARTI -0.25543    -0.801    0.3250
#BLUTI - NUTHA -0.31477    -0.858    0.2646
#GRETI - MARTI -0.23042    -0.550    0.0924
#GRETI - NUTHA -0.29594    -0.649    0.0811
#MARTI - NUTHA -0.06508    -0.387    0.2792

#season = winter:
#  contrast      estimate lower.HPD upper.HPD
#BLUTI - GRETI  0.00118    -0.156    0.1611
#BLUTI - MARTI -0.32215    -0.516   -0.1379
#BLUTI - NUTHA -0.29473    -0.544   -0.0622
#GRETI - MARTI -0.32612    -0.483   -0.1580
#GRETI - NUTHA -0.29676    -0.512   -0.0776
#MARTI - NUTHA  0.02786    -0.203    0.2526

# Results are averaged over the levels of: age_in_2020 
# Point estimate displayed: median 
# Results are given on the log (not the response) scale. 
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

head(species_season_contrasts_df)
lead_sp_emm_table <- species_season_contrasts_df %>%
  dplyr::select(contrast, season, estimate, lower.HPD, upper.HPD)
species_names <- c(
  "BLUTI" = "Blue tit",
  "GRETI" = "Great tit",
  "MARTI" = "Marsh tit",
  "NUTHA" = "Nuthatch")
season_names <- c(
  "autumn" = "Autumn",
  "winter" = "Winter",
  "spring" = "Spring",
  "summer" = "Summer")

lead_sp_emm_table <- lead_sp_emm_table %>%
  rename(Contrast = "contrast",
         Season = "season",
         CI_Lower = "lower.HPD",
         CI_Upper = "upper.HPD"
  )%>%
  mutate(
    Odds = exp(estimate),
    CI_Lower_Odds = exp(CI_Lower),
    CI_Upper_Odds = exp(CI_Upper)
  )
lead_sp_emm_table <- lead_sp_emm_table%>%
  select(c(Contrast, Season, Odds, CI_Lower_Odds, CI_Upper_Odds))
lead_sp_emm_table <- lead_sp_emm_table %>%
  mutate(Contrast = str_replace_all(Contrast, species_names))
lead_sp_emm_table <- lead_sp_emm_table %>%
  mutate(Season = str_replace_all(Season, season_names))

lead_sp_emm_table <- lead_sp_emm_table %>%
  dplyr::mutate(across(where(is.numeric), ~ round(.x, 2)))

lead_sp_pairwise_table <- flextable(lead_sp_emm_table) %>%
  autofit() %>%
  set_caption("Leaderschip Pairwise Contrasts of Species by Season")


doc_pairwise_comparisons_lead_sp <- read_docx() %>%
  body_add_flextable(lead_sp_pairwise_table)

print(doc_pairwise_comparisons_lead_sp, target = "Plots and tables/pairwise_contrasts_sp_leadership_model.docx")

# for age

age_emm <- emmeans(leadership_model, ~ age_in_2020)
# age_in_2020 emmean lower.HPD upper.HPD
# adult         1.74      1.63      1.84
# juvenile      1.84      1.70      1.96
# 
# Results are averaged over the levels of: season, species 
# Point estimate displayed: median 
# Results are given on the log (not the response) scale. 
# HPD interval probability: 0.95 

# Marginal means for species by season (Table S5)
age_season_emm <- emmeans(leadership_model, ~ age_in_2020 | season)
# Pairwise comparisons within each season
age_season_contrasts <- contrast(age_season_emm, method = "pairwise")
age_season_contrasts

# season = autumn:
#   contrast         estimate lower.HPD upper.HPD
# adult - juvenile  0.00496    -0.329    0.3382
# 
# season = spring:
#   contrast         estimate lower.HPD upper.HPD
# adult - juvenile -0.15469    -0.387    0.0863
# 
# season = summer:
#   contrast         estimate lower.HPD upper.HPD
# adult - juvenile -0.20367    -0.521    0.1133
# 
# season = winter:
#   contrast         estimate lower.HPD upper.HPD
# adult - juvenile -0.05760    -0.188    0.0682
# 
# Results are averaged over the levels of: species 
# Point estimate displayed: median 
# Results are given on the log (not the response) scale. 
# HPD interval probability: 0.95 

#extract the associated table
age_season_contrasts_df <- as.data.frame(age_season_contrasts)

lead_age_emm_table <- age_season_contrasts_df %>%
  dplyr::select(contrast, season, estimate, lower.HPD, upper.HPD)
species_names <- c(
  "BLUTI" = "Blue tit",
  "GRETI" = "Great tit",
  "MARTI" = "Marsh tit",
  "NUTHA" = "Nuthatch")
season_names <- c(
  "autumn" = "Autumn",
  "winter" = "Winter",
  "spring" = "Spring",
  "summer" = "Summer")

lead_age_emm_table <- lead_age_emm_table %>%
  rename(Contrast = "contrast",
         Season = "season",
         CI_Lower = "lower.HPD",
         CI_Upper = "upper.HPD"
  )%>%
  mutate(
    Odds = exp(estimate),
    CI_Lower_Odds = exp(CI_Lower),
    CI_Upper_Odds = exp(CI_Upper)
  )

age_names <- c(
  "adult" = "Adult",
  "juvenile" = "Juvenile")

lead_age_emm_table <- lead_age_emm_table%>%
  select(c(Contrast, Season, Odds, CI_Lower_Odds, CI_Upper_Odds))
lead_age_emm_table <- lead_age_emm_table %>%
  mutate(Contrast = str_replace_all(Contrast, age_names))
lead_age_emm_table <- lead_age_emm_table %>%
  mutate(Season = str_replace_all(Season, season_names))

lead_age_emm_table <- lead_age_emm_table %>%
  dplyr::mutate(across(where(is.numeric), ~ round(.x, 2)))

lead_age_pairwise_table <- flextable(lead_age_emm_table) %>%
  autofit() %>%
  set_caption("Leaderschip Pairwise Contrasts of Age class by Season")


doc_pariwise_comparisons_lead_age <- read_docx() %>%
  body_add_flextable(lead_age_pairwise_table)

print(doc_pariwise_comparisons_lead_age, target = "Plots and tables/pairwise_contrasts_age_leadership_model.docx")


# 3.7) Trends for continuous variable (Table S7)
trends_degree <- emtrends(leadership_model, ~ season, var = "degree_log")

# season degree_log.trend lower.HPD upper.HPD
# autumn           0.1083   -0.0832     0.303
# spring          -0.0153   -0.1350     0.115
# summer           0.0307   -0.1514     0.211
# winter           0.0904   -0.0166     0.189
# 
# Results are averaged over the levels of: species, age_in_2020 
# Point estimate displayed: median 
# HPD interval probability: 0.95

#extract the associated table
trends_degree_df <- as.data.frame(trends_degree)
colnames(trends_degree_df)
trends_degree_table <- trends_degree_df %>%
  dplyr::select(season, degree_log.trend, lower.HPD, upper.HPD)
trends_degree_table <- trends_degree_table %>%
  rename(Season = "season",
         Trend = "degree_log.trend",
         CI_Lower = "lower.HPD",
         CI_Upper = "upper.HPD"
  )


trends_degree_table <- trends_degree_table %>%
  dplyr::mutate(across(where(is.numeric), ~ round(.x, 3)))

trends_degree_table <- flextable(trends_degree_table)
doc_Trend_degree <- read_docx() %>%
  body_add_flextable(trends_degree_table)

print(doc_Trend_degree, target = "Plots and tables/degree_trends.docx")


# Table S6
trends_betweenness <- emtrends(leadership_model, ~ season, var = "betweenness_log")

# season betweenness_log.trend lower.HPD upper.HPD
# autumn              -0.00388   -0.0200    0.0130
# spring               0.00838   -0.0114    0.0263
# summer               0.00265   -0.0162    0.0212
# winter               0.00121   -0.0106    0.0129
# 
# Results are averaged over the levels of: species, age_in_2020 
# Point estimate displayed: median 
# HPD interval probability: 0.95 

#extract the associated table
#extract the associated table
trends_betweenness_df <- as.data.frame(trends_betweenness)
colnames(trends_betweenness_df)
trends_betweenness_table <- trends_betweenness_df %>%
  dplyr::select(season, betweenness_log.trend, lower.HPD, upper.HPD)

trends_betweenness_table <- trends_betweenness_table %>%
  rename(Season = "season",
         Trend = "betweenness_log.trend",
         CI_Lower = "lower.HPD",
         CI_Upper = "upper.HPD"
  )

trends_betweenness_table <- trends_betweenness_table %>%
  dplyr::mutate(across(where(is.numeric), ~ round(.x, 3)))

trends_betweenness_table <- flextable(trends_betweenness_table)
doc_Trend_betweenness <- read_docx() %>%
  body_add_flextable(trends_betweenness_table)

print(doc_Trend_betweenness, target = "Plots and tables/betweenness_trends.docx")


# 5) Extract flock sizes in each season -------------------------------------------------------------------

length(unique(network.pos.all.seasons.sub$PIT))
# 231
length(unique(network.pos.all.seasons.sub$PIT[network.pos.all.seasons.sub$species==
                                            "GRETI"]))
# 142

length(unique(network.pos.all.seasons.sub$PIT[network.pos.all.seasons.sub$species==
                                                "BLUTI"]))


length(unique(network.pos.all.seasons.sub$PIT[network.pos.all.seasons.sub$species==
                                                "MARTI"]))
# 15

length(unique(network.pos.all.seasons.sub$PIT[network.pos.all.seasons.sub$species==
                                                "NUTHA"]))
# 9 


# by season
length(unique(network.pos.all.seasons.sub$PIT[network.pos.all.seasons.sub$season==
                                                "summer"]))
# 96
length(unique(network.pos.all.seasons.sub$PIT[network.pos.all.seasons.sub$season==
                                                "autumn"]))
# 71

length(unique(network.pos.all.seasons.sub$PIT[network.pos.all.seasons.sub$season==
                                                "winter"]))
# 175
length(unique(network.pos.all.seasons.sub$PIT[network.pos.all.seasons.sub$season==
                                                "spring"]))
# 119


#how many flocks there are in each season
#All groups contain at least 2 individuals

network.pos.all.seasons.sub %>%
  dplyr::group_by(season) %>%
  dplyr::summarise(num_groups = n_distinct(group))

# season num_groups
# <chr>       <int>
#   1 autumn        383
# 2 spring        472
# 3 summer        409
# 4 winter       1545

383+472+409+1545
# 2809

#how many individuals within each flock in each season?
network.pos.all.seasons.sub %>%
  filter(season == "summer", flock_size >= 2) %>%
  distinct(group, flock_size, season) %>%
  summarise(
    min_flock_size = min(flock_size),
    max_flock_size = max(flock_size)
  )

# min_flock_size max_flock_size
# 1              2             15

network.pos.all.seasons.sub %>%
  filter(season == "autumn", flock_size >= 2) %>%
  distinct(group, flock_size) %>%
  summarise(
    min_flock_size = min(flock_size),
    max_flock_size = max(flock_size)
  )

# min_flock_size max_flock_size
# 1              2             10        

network.pos.all.seasons.sub %>%
  filter(season == "winter", flock_size >= 2) %>%
  distinct(group, flock_size) %>%
  summarise(
    min_flock_size = min(flock_size),
    max_flock_size = max(flock_size)
  )

# min_flock_size max_flock_size
# 1              2             36

network.pos.all.seasons.sub %>%
  filter(season == "spring", flock_size >= 2) %>%
  distinct(group, flock_size) %>%
  summarise(
    min_flock_size = min(flock_size),
    max_flock_size = max(flock_size)
  )

# min_flock_size max_flock_size
# 1              2             22








