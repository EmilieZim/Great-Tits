# elephant: write the title of the paper and authors


# 1) Data prep ------------------------------------------------------------


# 1.1) Load libraries -----------------------------------------------------

#devtools::install_github("rmcelreath/rethinking")

##Library
library(asnipe)
library(brms)
library(car)
library(corrplot)
library(data.table)
library(datawizard)
library(dbplyr)
library(devtools)
library(dplyr)
library(emmeans)
library(flextable)
library(ggplot2)
library(ggpubr)
library(lme4)
library(lmerTest)
library(officer)
library(performance)
library(psych)
library(report)
library(rethinking)
library(rptR)
library(rstan)
library(stringr)
library(tidybayes)
library(tidyr)


# 1.2) Extract order of arrival at feeders --------------------------------

# Note that you can load the input data directly here, but we also provide code to replicate the data extraction from the raw data here (network_data_prep.R)


# 1.3) Load data ----------------------------------------------------------

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


# remove group sizes of 1 
network.pos.all.seasons <- subset(network.pos.all.seasons, network.pos.all.seasons$flock_size>1)

# how many data points per group size per species?
table(network.pos.all.seasons$species, network.pos.all.seasons$flock_size)


# 3) Follower-leader dynamics ---------------------------------------------


# 3.1) Calculate VIFs -----------------------------------------------------

network.pos.all.seasons$leader.follower <- as.factor(network.pos.all.seasons$leader.follower)

# we first run a simple regression (no interactions) to look at collinearity between predictors
model_order_vif <- glmer(leader.follower ~  species + scale(n_visits) + season + age_in_2020 + scale(degree) + scale(betweenness) + (1|PIT), family= binomial, data= network.pos.all.seasons)

summary(model_order_vif)

vif(model_order_vif)
# GVIF Df GVIF^(1/(2*Df))
# species            1.474976  3        1.066917
# scale(n_visits)    1.017361  1        1.008643
# season             2.742389  3        1.183100
# age_in_2020        1.140250  1        1.067825
# scale(degree)      2.710099  1        1.646238
# scale(betweenness) 1.346542  1        1.160406

# all vifs <5, so we can include all of them


# 3.2) Global model -------------------------------------------------------

# we run a model with leader follower as outcome variable
# include an offset that controls for flock size since being a leader is more probable in smaller flocks. We also control for species prevalence by including the proportion of the bird's species in the flock. 
glob_model <- brms::brm(
  leader.follower ~  season * species + scale(n_visits) + age_in_2020*season + scale(betweenness) + scale(degree) + offset(flock_size) + species_prop + (1 | PIT),
  family = bernoulli, 
  data = network.pos.all.seasons,
  chains = 4,
  iter = 4000,
  cores = 4
#  save_pars = save_pars(all = TRUE)
)

#save(glob_model, file="model output/glob_model.RDA")
load("model output/glob_model.RDA")

# SW: I have updated things until here

summary(glob_model)


# 3.3) Model checks -------------------------------------------------------


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
# Bayesian R2 with Compatibility Interval
# 
# Conditional R2: 0.276 (95% CI [0.273, 0.278])
# Marginal R2: 0.279 (95% CI [0.275, 0.285])

# 3.4) Summary table ------------------------------------------------------


##make a table with the summary
summary_glob_model <- summary(glob_model)
fixed_effects <- as.data.frame(summary_glob_model$fixed)
fixed_effects <- tibble::rownames_to_column(fixed_effects, var = "Term")
fixed_effects$Term <- c("Intercept", "Great tit", "Marsh tit", "Nuthatch",
                   "Spring","Summer", "Winter",
                   "Age (Juvenile)", "Betweenness", "Degree",
                   "Great tit x Spring", "Marsh tit x Spring", "Nuthatch x Spring", 
                   "Great tit x Summer", "Marsh tit x Summer", "Nuthatch x Summer", 
                   "Great tit x Winter", "Marsh tit x Winter", "Nuthatch x Winter", 
                   "Spring x Age(Juvenile)", "Summer x Age(Juvenile)", "Winter x Age(Juvenile)" )
fixed_effects <- fixed_effects[, 1:(ncol(fixed_effects) - 2)]
fixed_effects <- fixed_effects %>%
  dplyr::mutate(across(where(is.numeric), ~ round(.x, 2)))

library(flextable)

flextable_table <- fixed_effects %>%
  flextable() %>%
  autofit() %>%
  set_caption("Summary of Fixed Effects from the final Model")

library(officer)

doc_glob_model <- read_docx() %>%
  body_add_flextable(flextable_table)

print(doc_glob_model, target = "Plots and tables/fixed_effects_final_glob_model.docx")


# 3.5) Plot ---------------------------------------------------------------

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
  
  #First plot: interaction between species and season
  {
    p <- glob_model_conditional_effects$`species:season`
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
      labs(x = "Season", y = "Leadership probability") +
      ylim(c(0.0, 0.9)) +
      theme(
        legend.position = "bottom",
        legend.title = element_text(),
        axis.title.x = element_blank()
      )
  },
  
  # Second plot: interaction between season and age
  {
    p <- glob_model_conditional_effects$`season:age_in_2020`
    for (i in seq_along(p$layers)) {
      if (inherits(p$layers[[i]]$geom, "GeomPoint")) {
        p$layers[[i]]$aes_params$size <- 2.5
      }
    }
    p +
      labs(y = "Leadership probability") +
      ylim(c(0.0, 0.9)) +
      theme(
        legend.background = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.x = element_blank()
      ) +
      scale_fill_manual(
        values = my_colors_age,
        breaks = c("adult", "juvenile"),
        labels = c("Adults", "Juveniles")
      ) +
      scale_color_manual(
        values = my_colors_age,
        breaks = c("adult", "juvenile"),
        labels = c("Adults", "Juveniles")
      ) +
      scale_x_discrete(
        limits = c("summer", "autumn", "winter", "spring"),
        labels = c("summer" = "Summer", "autumn" = "Autumn", "winter" = "Winter", "spring" = "Spring")
      )
  },
  #Third plot : betweenness centrality
  {
    ce <- conditional_effects(glob_model, effects = "betweenness")$betweenness
    
    ggplot(ce, aes(x = betweenness, y = estimate__)) +
      geom_ribbon(aes(ymin = lower__, ymax = upper__), fill = "grey80") +
      geom_line(color = "black", size= 0.5) +
      labs(x = "Betweenness centrality", y = "") +
      ylim(0.0, 0.9) +
      theme_bw()
  },
  
 
  
  #Fourth plot: degree centrality
  {
    ce_degree <- glob_model_conditional_effects$degree$data
    
      ggplot(ce_degree, aes(x = degree, y = estimate__)) +
      geom_ribbon(aes(ymin = lower__, ymax = upper__), fill = "grey80", alpha = 0.5) +  # CI ribbon in grey
      geom_line(color = "black", size = 0.5) +                                          # main curve in black
      labs(x = "Degree centrality", y = "") +
      ylim(0, 0.9) +
      theme_bw()
  },
  
  labels = c("a", "b", "c", "d"),
  ncol = 2, nrow = 2,
  common.legend = FALSE
)



ggsave(file="Plots and tables/plot_glob_model.tiff", width=8.5, height=6.5)


#display.brewer.all()

#This shows me that the model codes 0 as follower and 1 as leader. So an observation that at the y axis (leader.follower) has 0.7 this means that that observation has a 70% chance to be a leader.  


# 3.6) Compute species comparison -----------------------------------------

# the estimates are expressed in their logit scale so here transform them into odds ratio's
exp(fixef(glob_model))
# Estimate Est.Error       Q2.5       Q97.5
# Intercept                        0.21954653  1.296610 0.13148467  0.36442835
# speciesGRETI                     0.25333936  1.342641 0.14214456  0.45192419
# speciesMARTI                     0.21238314  1.524381 0.09410808  0.49328314
# speciesNUTHA                     1.44262604  1.639034 0.55541081  3.90218636
# seasonspring                     0.34856939  1.219697 0.23449389  0.51374076
# seasonsummer                     0.86218210  1.363427 0.46256429  1.58637189
# seasonwinter                     1.15124528  1.235557 0.75726198  1.71986970
# age_in_2020juvenile              1.77587714  1.287824 1.07954133  2.92453095
# scalebetweenness                 1.40192349  1.058989 1.25358433  1.57062800
# scaledegree                      0.71499829  1.068001 0.62850028  0.81316422
# speciesGRETI:seasonspring        2.94239085  1.289040 1.78123748  4.80538230
# speciesMARTI:seasonspring        7.71348460  1.323201 4.39813226 13.27928055
# speciesNUTHA:seasonspring        1.11034565  1.409924 0.56118100  2.14903043
# speciesGRETI:seasonsummer        0.02955643  1.446962 0.01438641  0.06194623
# speciesMARTI:seasonsummer        7.27718471  1.444899 3.55675991 15.08416144
# speciesNUTHA:seasonsummer        0.53579837  1.470618 0.25291195  1.15419533
# speciesGRETI:seasonwinter        0.93588364  1.262840 0.59323247  1.48120936
# speciesMARTI:seasonwinter        2.66748642  1.264051 1.67629173  4.18104360
# speciesNUTHA:seasonwinter        1.21428541  1.295821 0.72590606  2.00411703
# seasonspring:age_in_2020juvenile 1.16854871  1.253146 0.75449731  1.83194899
# seasonsummer:age_in_2020juvenile 0.23856669  1.287525 0.14541129  0.39603645
# seasonwinter:age_in_2020juvenile 0.78809046  1.222375 0.53034699  1.16118090


# in our model, blue tits are the baseline species to which all others are compared. We would like to compute comparisons between all species. 


# Compute marginal means for the species variable
species_emm <- emmeans(glob_model, ~ species)
# Pairwise comparisons between species levels
species_contrasts <- contrast(species_emm, method = "pairwise")
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


# Marginal means for species by season
species_season_emm <- emmeans(glob_model, ~ species | season)
# Pairwise comparisons within each season
species_season_contrasts <- contrast(species_season_emm, method = "pairwise")
species_season_contrasts

# season = autumn:
#   contrast      estimate lower.HPD upper.HPD
# BLUTI - GRETI    1.374     0.784    1.9350
# BLUTI - MARTI    1.551     0.674    2.3256
# BLUTI - NUTHA   -0.362    -1.347    0.5998
# GRETI - MARTI    0.176    -0.603    0.9849
# GRETI - NUTHA   -1.732    -2.667   -0.8060
# MARTI - NUTHA   -1.912    -3.007   -0.8190
# 
# season = spring:
#   contrast      estimate lower.HPD upper.HPD
# BLUTI - GRETI    0.295    -0.137    0.7374
# BLUTI - MARTI   -0.494    -1.233    0.3241
# BLUTI - NUTHA   -0.470    -1.411    0.5373
# GRETI - MARTI   -0.783    -1.564   -0.0608
# GRETI - NUTHA   -0.759    -1.663    0.2427
# MARTI - NUTHA    0.020    -1.138    1.1768
# 
# season = summer:
#   contrast      estimate lower.HPD upper.HPD
# BLUTI - GRETI    4.900     4.220    5.5986
# BLUTI - MARTI   -0.433    -1.377    0.4445
# BLUTI - NUTHA    0.267    -0.787    1.2997
# GRETI - MARTI   -5.332    -6.107   -4.5467
# GRETI - NUTHA   -4.635    -5.547   -3.7011
# MARTI - NUTHA    0.691    -0.437    1.7685
# 
# season = winter:
#   contrast      estimate lower.HPD upper.HPD
# BLUTI - GRETI    1.442     1.030    1.8398
# BLUTI - MARTI    0.567    -0.156    1.3027
# BLUTI - NUTHA   -0.558    -1.461    0.3405
# GRETI - MARTI   -0.870    -1.542   -0.1552
# GRETI - NUTHA   -1.997    -2.907   -1.1452
# MARTI - NUTHA   -1.129    -2.173   -0.0619
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
species_names <- c(
  "BLUTI" = "Blue tit",
  "GRETI" = "Great tit",
  "MARTI" = "Marsh tit",
  "NUTHA" = "Nuthatch"
)
library(stringr)
emm_table <- emm_table %>%
  mutate(contrast = str_replace_all(contrast, species_names))
colnames(emm_table) <- c("Contrast", "Season", "Estimate", "Lower_HPD", "Upper_HPD", "Odds", "Probability")
emm_table <- emm_table %>%
  dplyr::mutate(across(where(is.numeric), ~ round(.x, 2)))

pairwise_table <- flextable(emm_table) %>%
  autofit() %>%
  set_caption("Pairwise Contrasts of Species by Season")

# Export to Word (optional)
library(officer)
doc_pariwise_comparisons <- read_docx() %>%
  body_add_flextable(pairwise_table)

print(doc_pariwise_comparisons, target = "Plots and tables/pairwise_contrasts_glob_model.docx")

# for age

# Marginal means for species by season
age_season_emm <- emmeans(glob_model, ~ age_in_2020 | season)
# Pairwise comparisons within each season
age_season_contrasts <- contrast(age_season_emm, method = "pairwise")
age_season_contrasts

# season = autumn:
#   contrast         estimate lower.HPD upper.HPD
# adult - juvenile   -0.574    -1.085   -0.0930
# 
# season = spring:
#   contrast         estimate lower.HPD upper.HPD
# adult - juvenile   -0.731    -1.138   -0.3141
# 
# season = summer:
#   contrast         estimate lower.HPD upper.HPD
# adult - juvenile    0.857     0.414    1.3176
# 
# season = winter:
#   contrast         estimate lower.HPD upper.HPD
# adult - juvenile   -0.342    -0.687    0.0456
# 
# Results are averaged over the levels of: species 
# Point estimate displayed: median 
# Results are given on the log odds ratio (not the response) scale. 
# HPD interval probability: 0.95 


# 3.7) Compute age comparisons --------------------------------------------

# Can you do the emmeans for age across seasons as well?


# 3.8) Extract flock sizes in each season -------------------------------------------------------------------

#how many flocks there are in each season
#All groups contain at least 2 individuals

network.pos.all.seasons %>%
  dplyr::group_by(season) %>%
  dplyr::summarise(num_groups = n_distinct(group))

# # A tibble: 4 × 2
# season num_groups
# <fct>       <int>
#   1 autumn        860
# 2 spring       1453
# 3 summer       1223
# 4 winter       2268

#how many individuals within each flock in each season?
network.pos.all.seasons %>%
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
^^^^
# min_flock_size max_flock_size
# 1              2             22








