# elephant: write the title of the paper and authors


# 1) Data prep ------------------------------------------------------------


# 1.1) Load libraries -----------------------------------------------------

devtools::install_github("rmcelreath/rethinking")

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

# 2) Follower-leader dynamics ---------------------------------------------


# 2.1) Calculate VIFs -----------------------------------------------------

network.pos.all.seasons$leader.follower <- as.factor(network.pos.all.seasons$leader.follower)

# we first run a simple regression (no interactions) to look at collinearity between predictors
model_order_vif <- glmer(leader.follower ~  species + season + age_in_2020 + scale(degree) + scale(betweenness) + species_prop + (1|PIT), family= binomial, data= network.pos.all.seasons)

summary(model_order_vif)

library(car)
vif(model_order_vif)
# GVIF Df GVIF^(1/(2*Df))
# species            1.453904  3        1.064362
# season             3.061528  3        1.205007
# age_in_2020        1.123639  1        1.060018
# scale(degree)      2.788198  1        1.669790
# scale(betweenness) 1.293165  1        1.137174
# species_prop       1.190900  1        1.091284

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

#save(glob_model, file="model output/glob_model.RDA")
load("model output/glob_model.RDA")

summary(glob_model)

# Family: bernoulli 
# Links: mu = logit 
# Formula: leader.follower ~ species * season + age_in_2020 * season + scale(betweenness) + scale(degree) + offset(qlogis(species_prop_adj)) + (1 | PIT) 
# Data: network.pos.all.seasons (Number of observations: 24000) 
# Draws: 4 chains, each with iter = 4000; warmup = 2000; thin = 1;
# total post-warmup draws = 8000
# 
# Multilevel Hyperparameters:
#   ~PIT (Number of levels: 234) 
# Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sd(Intercept)     1.19      0.07     1.06     1.33 1.00     2403     4481
# 
# Regression Coefficients:
#   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept                           -1.52      0.26    -2.03    -1.01 1.00     1778     3035
# speciesGRETI                        -1.37      0.29    -1.95    -0.79 1.00     1915     3712
# speciesMARTI                        -1.55      0.42    -2.36    -0.71 1.00     1827     3390
# speciesNUTHA                         0.37      0.49    -0.59     1.36 1.00     2153     3615
# seasonspring                        -1.05      0.20    -1.45    -0.67 1.00     3119     4441
# seasonsummer                        -0.15      0.31    -0.77     0.46 1.00     3628     5028
# seasonwinter                         0.14      0.21    -0.28     0.54 1.00     3114     4962
# age_in_2020juvenile                  0.57      0.25     0.08     1.07 1.00     2474     3828
# scalebetweenness                     0.34      0.06     0.23     0.45 1.00     9833     6701
# scaledegree                         -0.34      0.07    -0.46    -0.21 1.00     5532     5981
# speciesGRETI:seasonspring            1.08      0.25     0.58     1.57 1.00     3698     5276
# speciesMARTI:seasonspring            2.04      0.28     1.48     2.59 1.00     4105     5684
# speciesNUTHA:seasonspring            0.10      0.34    -0.58     0.77 1.00     4975     5649
# speciesGRETI:seasonsummer           -3.52      0.37    -4.24    -2.78 1.00     3848     4934
# speciesMARTI:seasonsummer            1.98      0.37     1.27     2.71 1.00     3996     5361
# speciesNUTHA:seasonsummer           -0.62      0.39    -1.37     0.14 1.00     3811     5180
# speciesGRETI:seasonwinter           -0.07      0.23    -0.52     0.39 1.00     3644     5329
# speciesMARTI:seasonwinter            0.98      0.23     0.52     1.43 1.00     4092     4907
# speciesNUTHA:seasonwinter            0.19      0.26    -0.32     0.70 1.00     3877     5537
# seasonspring:age_in_2020juvenile     0.16      0.23    -0.28     0.61 1.00     5376     5818
# seasonsummer:age_in_2020juvenile    -1.43      0.25    -1.93    -0.93 1.00     5628     6097
# seasonwinter:age_in_2020juvenile    -0.24      0.20    -0.63     0.15 1.00     5596     6382
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

# we plot it as season:species
glob_model_conditional_effects_reversed <- plot(conditional_effects(glob_model, effects = "season:species", re_formula = NA))

# a 0 here means to a follower (baseline)
# formula = NA excludes random effects

# extract an R2 for our model
R2 <- performance::r2_bayes(glob_model)
# Bayesian R2 with Compatibility Interval
# 
# Conditional R2: 0.276 (95% CI [0.273, 0.278])
# Marginal R2: 0.279 (95% CI [0.275, 0.285])

# 2.4) Summary table ------------------------------------------------------


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


# 2.5) Plot ---------------------------------------------------------------

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


# 2.8) Compute species comparison -----------------------------------------

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


# 2.10) Extract flock sizes in each season -------------------------------------------------------------------

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

# min_flock_size max_flock_size
# 1              2             22


# 3) Post-hoc: social centrality of species ------------------------------------

ggplot(aes(x=species, y=betweenness), data=network.pos.all.seasons)+
  geom_boxplot()

ggplot(aes(x=species, y=degree), data=network.pos.all.seasons)+
  geom_boxplot()

network.pos.all.seasons



# # 3) Territoriality  ------------------------------------------------------
# 
# 
# network.pos.all.seasons
# # the 'location' are the different feeders 
# 
# head(network.pos.all.seasons)
# 
# subset_network.pos.all.seasons <- network.pos.all.seasons %>% select(PIT, season, species, age_in_2020, location)
# # we add the column "num.feeders.visited" that contains the total number of feeders each bird visited
# subset2_network.pos.all.seasons <- subset_network.pos.all.seasons %>%
#   group_by(PIT, season) %>%
#   mutate(num.feeders.visited = n_distinct(location)) %>%
#   ungroup()
# 
# 
# # and we add a column how many times a bird was seen at a particular feeder
# 
# subset2_network.pos.all.seasons <- subset2_network.pos.all.seasons %>%
#   group_by(PIT, location, season) %>%
#   mutate(pit_location_season_count = n()) %>%
#   ungroup()
# 
# 
# # head(subset2_network.pos.all.seasons) #some rows are duplicates so I decide to use the function distinct() to eliminate this
# # subset2_network.pos.all.seasons <- subset2_network.pos.all.seasons %>%
# #   distinct()
# 
# 
# hist(subset2_network.pos.all.seasons$num.feeders.visited) #poisson distribution
# hist(subset2_network.pos.all.seasons$pit_location_season_count) #poisson distribution
# 
# 
# library(dplyr)
# library(vegan)  # for diversity()
# 
# entropy_df <- subset2_network.pos.all.seasons %>%
#   group_by(PIT, species, season, location) %>%
#   summarise(n_visits = n(), .groups = "drop") %>%
#   group_by(PIT, species, season) %>%
#   summarise(
#     shannon_entropy = diversity(n_visits, index = "shannon"),
#     n_feed = n_distinct(location),
#     total_visits = sum(n_visits),
#     .groups = "drop"
#   ) %>%
#   mutate(
#     entropy_normalized = shannon_entropy / log(n_feed)  # range: 0–1
#   )
# 
# # replace NaNs with 0 in the normalized entropy
# entropy_df <- entropy_df %>%
#   mutate(
#     entropy_normalized = ifelse(n_feed == 1, 0, shannon_entropy / log(n_feed))
#   )
# 
# 
# # the higher the entropy, the less territorial
# 
# entropy_model <- brm(
#   entropy_normalized ~ species * season + (1 | PIT),
#   data = entropy_df,
#   family = gaussian(),
#   chains = 4,
#   iter = 4000,  # you can increase to 4000 later
#   cores = 4,
#   control = list(adapt_delta = 0.95)
# )
# 
# 
# summary(entropy_model)
# 
# pp_check(entropy_model)
# 
# # SW: can you compute the emmmeans comparison and create a table like you did with the other models?
# emmeans(entropy_model, pairwise ~ species | season)  # compare species within season
# 
# # SW: can you create a plot similar to Figure 1a with season in the x axis and territoriality on the y axis
# plot(conditional_effects(entropy_model, effects = "season:species"))
# 
# 


# 4) Descriptive stats ----------------------------------------------------
#for the leader-follower status
View(network.pos.all.seasons)
network.pos.all.seasons$species <- as.factor(network.pos.all.seasons$species)
nbr_species <- network.pos.all.seasons %>%
  distinct(PIT, species, age_in_2020) %>% # Keep only unique combinations of ID and species
  group_by(species, age_in_2020) %>%
  summarise(count = n()) 

# species age_in_2020 count
# <fct>   <chr>       <int>
#   1 BLUTI   adult          41
# 2 BLUTI   juvenile       25
# 3 GRETI   adult          64
# 4 GRETI   juvenile       80
# 5 MARTI   adult          12
# 6 MARTI   juvenile        3
# 7 NUTHA   adult           5
# 8 NUTHA   juvenile        4


# #for visitation data (also subset to groups of a minimum of 2 birds)
# View(visits_all_season)
# 
# visits_all_season$species <- as.factor(visits_all_season$species)
# nbr_species_visits <- visits_all_season %>%
#   distinct(PIT, species) %>% # Keep only unique combinations of ID and species
#   group_by(species) %>%
#   summarise(count = n())
# #not the same amount of individuals for this part









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




