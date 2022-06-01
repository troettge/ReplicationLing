## Title: Bayesian analysis of Kobrock & Roettger (2022)
## Description: Bayesian Model following prereg https://osf.io/a5xd7/.
## Authors: Kobrock & Roettger
## Contact: kkobrock@uni-osnabrueck.de
## Date: 21-03-22

# preprocessing---------------
## load in library
library(tidyverse)
library(brms)
library(rstudioapi)
library(emmeans)

# load in data-----------------------------------------------------------------
## getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path))

mention <-  read.csv("../data/mention.csv")
guidelines  <-  read.csv("../data/guidelines.csv")
replication  <-  read.csv("../data/coded.csv")  


# MENTIONS: wrangle data mention and guideline----------------------------------

# merge dfs
df <- full_join(mention, guidelines)

# delete older journals that have been renamed
df <- df %>% 
  filter(!(journals %in% c("LANGUAGE AND COGNITIVE PROCESSES", "LITERARY AND LINGUISTIC COMPUTING")))

# recode jif
df$jif <- as.numeric(ifelse(df$jif == "not retrievable", NA, df$jif)) 
df$jif_s <- df$jif - mean(df$jif, na.rm = TRUE)

# NAs would be dropped from model. What to do? Either assume 0 or seperate models

# recode open access as binary
df$openaccess_binary <- ifelse(df$openaccess == "DOAJ gold", 1, 0)

### Reviewer 1 remarks that the coding of JIF is off and quotes mean = 38 and median = 38.47,
# I cannot reproduce these numbers. Descriptives seems fine
mean(df$jif, na.rm = TRUE)
median(df$jif, na.rm = TRUE)

# MENTIONS: model--------------------------------------------------------------

## priors
priors <- c( 
  ### prior for the Intercept should be low because we expect very low rates overall
  ### Makel report a 1.6% rate, we will be lenient and assume 10% with a generous spread using cauchy
  set_prior("normal(0, 2.5)", class = "Intercept"),
  ### priors for all fixed effects will be centered on zero
  set_prior("cauchy(0, 2.5)", class = "b")
)

# set open access reference level to "no" 
df$openaccess <- as.factor(df$openaccess)
df$openaccess <- relevel(df$openaccess, ref = "partial")

## contrast-code binary factors
df$openaccess_binary_s <- ifelse(df$openaccess_binary == 0, -0.5, 0.5)
df$binary_policy_s <- ifelse(df$binary_policy == 0, -0.5, 0.5)

## preregistered model
xmdl_mention1 = brm(no_replic | trials(no_exp) ~ jif + openaccess_binary_s + binary_policy_s,
          data = df, 
          prior = priors,
          cores = 4,
          file  = "../data/repl_mention1_mdl.RDS",
          family = binomial(link = "logit"))

## revised model (following reviewer suggestions) with categorical open access factor and scaled jif
xmdl_mention2 = brm(no_replic | trials(no_exp) ~ jif_s + openaccess + binary_policy_s,
                    data = df, 
                    prior = priors,
                    cores = 4,
                    iter = 4000,
                    file  = "../data/repl_mention2_mdl.RDS",
                    family = binomial(link = "logit"))

predict <- xmdl_mention2 %>% 
emmeans(
        #spec = ~ jif_s,
        specs = pairwise ~ jif_s | openaccess,
  at = list(
    jif_s = c(0, 1), # or whatever you want here
    openaccess = c("no", "partial", "DOAJ gold"),     # replace with factor levels
    binary_policy_s = 0 # replace with factor levels
  ), 
  re_formula = NA, # change to NA if you dont want to include grouping variable
  #epred = TRUE,
  allow_new_levels = TRUE
) %>% 
  contrast() %>% 
  tidy() %>%
  mutate(emmean = plogis(estimate),
         lower.HPD = plogis(lower.HPD),
         upper.HPD = plogis(upper.HPD)) 


emmeans(mod,
        specs = pairwise ~ factor_of_interest | other_factor
)

predict_diff <- predict %>% 
  mutate(diff = )

## summary(xmdl_mention1)
## summary(xmdl_mention2)
## pp_check(xmdl_mention1)
## pp_check(xmdl_mention2)

# REPLICATION: wrangle data replication----------------------------------------------------

replication_selected <- replication %>% 
  select(Title, Journal, Publication.Year,
         experimental., Replication., 
         Open.Access..article., author.overlap, 
         years.between.initial.and.replication.study,
         type.of.replication, if.direct..check.for.success, 
         citation.count.until.replication.study.was.published..retrieved..16.08.2021.) %>% 
  rename(title = Title, 
         journal = Journal,
         year = Publication.Year,
         exp = experimental.,
         replication = Replication.,
         oa = Open.Access..article.,
         overlap = author.overlap,
         lag = years.between.initial.and.replication.study,
         type = type.of.replication, 
         succesful = if.direct..check.for.success,
         citation_initial = citation.count.until.replication.study.was.published..retrieved..16.08.2021.) %>% 
  mutate(year_s = year - mean(year, na.rm = TRUE))


## how many really experimental
round(sum(replication_selected$exp) / nrow(replication_selected), 2)
### 0.95 (199 out of 210)

## subset only those
replication_sub <- replication_selected %>% 
  filter(exp == 1)

## of those how many actually replications
round(sum(replication_sub$replication) / nrow(replication_sub), 2)
### 0.58 (115 out of 199)

## subset only those
replication_sub_repli <- replication_sub %>% 
  filter(replication == 1)

## of those what types of replications
round(xtabs(~type, replication_sub_repli) / nrow(replication_sub_repli), 2)
### 0.57 conceptual
### 0.07 direct
### 0.36 partial

## of those author overlap
round(xtabs(~type + overlap, replication_sub_repli) / nrow(replication_sub_repli), 2)
###            no   yes 
### conceptual 0.26 0.30
### direct     0.03 0.04
### partial    0.15 0.21

## overall direct independent replication rate 
round(nrow(replication_sub[replication_sub$replication == 1 &
                replication_sub$type == "direct" &
                replication_sub$overlap == 0,]) / nrow(replication_sub), 3)
### 0.015

# REPLICATION: model------------------------------------------------------------

## preregistered logistic regression
## priors
priors <- c( 
  ### prior for the Intercept should be low because we expect very low rates overall
  ### Makel report a 1.6% rate, we will be lenient and assume 10% with a generous spread using cauchy
  set_prior("normal(0, 2.5)", class = "Intercept"),
  ### priors for all fixed effects will be centered on zero
  set_prior("cauchy(0, 2.5)", class = "b")
)

## relevel oa factor
# replication_sub$oa <- as.factor(replication_sub$oa)
#replication_sub$oa <- relevel(replication_sub$oa, ref = "no")

## contrast-code
replication_sub$oa_s <- ifelse(replication_sub$oa == 0, -0.5, 0.5)

## make direct replication vector
replication_sub$direct <- ifelse(replication_sub$type == "direct", 1, 0)

## model
xmdl_replication = brm(direct ~ year_s + oa_s + lag + citation_initial,
           data = replication_sub, 
           prior = priors,
           cores = 4,
           file  = "../data/direct_replication_mdl.RDS",
           family = bernoulli(link = "logit"))

## summary(xmdl2)
## pp_check(xmdl2)

### model is pretty useless, because replication rates are at floor
