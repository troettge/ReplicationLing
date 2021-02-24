## Title: SimData for Replication Project
## Description: Simulate and plot
## Authors: Timo Roettger
## Date: 21-02-02

### preprocessing---------------
## load in library
library(tidyverse)
library(truncnorm)
library(brms)
        
# package for visualization
library(tidybayes)
library(bayesplot)

### replication rate across 100 journals---------------
## make data frame
# define sample
sample = 100

journals <- 1:100

# journal policies: encourage vs. not encourage
policies <- sample(c("encourage", "not"), sample, 
                   replace = TRUE, prob = c(.1, .9))

# journal impact factor: 0-5
jif <- rexp(n = sample, rate = 2)

# open access or not
oa <- sample(c("open", "closed"), sample, 
             replace = TRUE, prob = c(.2, .8))

# replication rate
overall_hits <- rpois(n = sample, 500)
replication_hits <- rpois(n = sample, 25)

# put together
df_all <- data.frame(journals = journals,
                     policies = policies,
                     jif = jif,
                     oa = oa,
                     overall_hits = overall_hits,
                     replication_hits = replication_hits)

df_all <- df_all %>% 
  mutate(rate = replication_hits/overall_hits)

## plot stuff---------------
# jif x rate
ggplot(df_all, aes(x = jif, y = log(rate))) +
  geom_point(pch = 21) +
  geom_point(pch = 21, alpha = 0.5) +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2) + I(x^3) + I(x^4), se = FALSE,
              size = 2) +
  theme_minimal()

# policies x rate
ggplot(df_all, aes(x = policies, y = log(rate))) +
  geom_point(pch = 21, alpha = 0.5) +
  theme_minimal()

# NOTE: probably too little data points to compare

# oa x rate
ggplot(df_all, aes(x = oa, y = log(rate))) +
  geom_point(pch = 21, alpha = 0.5) +
  theme_minimal()

# NOTE: probably too little data points to compare

# oa x jif x rate
ggplot(df_all, aes(x = jif, y = log(rate), fill = oa, color = oa)) +
  geom_point(pch = 21, alpha = 0.5) +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2) + I(x^3) + I(x^4), se = FALSE,
              size = 2) +
  facet_grid(~ policies) +
  theme_minimal()

# NOTE: likely not a lot of data points for the two low base-rates policies and oa

## models---------------

priors <- c( 
  # prior for the Intercept should be low because we expect very low rates overall
  # Makel report a 1.6% rate, we will be lenient and assume 10% with a generous spread using cauchy
  set_prior("normal(-2.1973, 2.5)", class = "Intercept"),
  # priors for all fixed effects will be centered on zero
  set_prior("cauchy(0, scale = 2.5)", class = "b")
)

xlm = brm(replication_hits | trials(overall_hits) ~ jif + oa + policies,
          data = df_all, 
          #prior = priors,
          family = binomial(link = "logit"))

predicted_values <- xlm %>%
  spread_draws(b_Intercept, b_jif, b_oaopen, b_policiesnot) %>%
  # make a list of relevant value range of logRT
  mutate(jif = list(seq(0, 3, 0.1))) %>% 
  unnest(jif) %>%
  # transform into proportion space using the plogis function
  mutate(pred_closed_encouraged = plogis(b_Intercept + b_jif*jif),
         pred_open_encouraged = plogis(b_Intercept + b_jif*jif + b_oaopen),
         pred_closed_discouraged = plogis(b_Intercept + b_jif*jif + b_policiesnot),
         pred_open_discouraged = plogis(b_Intercept + b_jif*jif + b_oaopen + b_policiesnot),
         ) %>%
  pivot_longer(cols = c(pred_closed_encouraged,
                        pred_open_encouraged,
                        pred_closed_discouraged,
                        pred_open_discouraged),
              names_to = "factors") %>% 
  mutate(oa = ifelse(factors %in% c("pred_closed_encouraged","pred_closed_discouraged"), "closed", "open"),
         policies = ifelse(factors %in% c("pred_closed_encouraged","pred_open_encouraged"), "encourage", "not")) %>% 
  group_by(jif, oa, policies) %>%
  summarise(pred_m = mean(value, na.rm = TRUE),
            pred_low = quantile(value, prob = 0.025),
            pred_high = quantile(value, prob = 0.975)) 

# plot predicted values against data
ggplot(data = predicted_values, aes(x = jif, y = pred_m, fill = oa)) +
  geom_hline(yintercept = c(0,1), lty = "dashed", color = "grey") +
  geom_point(data = df_all, aes(x = jif, y = rate, fill = oa),
    position = position_jitter(height = 0.02), 
             alpha = 0.5, color = "white",
             pch = 21) +
  facet_grid(oa~policies) +
  geom_ribbon(aes(ymin = pred_low, ymax = pred_high), alpha = 0.5,
              fill = "grey") +
  geom_line(color = "#E69F00", size = 1.5) +
  ylab("Predicted rate of replication mention") +
  ylim(-0.02, 0.08) +
  xlim(0, 2.5) +
  theme_minimal() +
    theme(legend.position = "none")


### replication type across 200 articles in 20 journals---------------
## make data frame

sample = 200

journal <- sample(LETTERS[1:20], replace = TRUE, sample,
                  prob = rtruncnorm(20, a = 0, b = 1, mean = 0.05, sd = 0.1))

article <- 1:200

# type of replication: direct, partial, conceptual
type <- sample(c("direct", "partial", "conceptual", "none"), sample, 
                     replace = TRUE, prob = c(0.1, 0.15, 0.6, 0.15))

# citation of original study: 0-500
citations <- rtruncnorm(n = sample, a = 0, b = 500, mean = 20, sd = 30)

# publication year: 1970-2020
year <- round(runif(200, min = 2000, max = 2020), 0)

# years past: 0-10
pastYear <- round(runif(200, min = 1, max = 10), 0)

# open access or not
oa <- sample(c("open", "closed"), sample, 
             replace = TRUE, prob = c(.2, .8))

# put together
df_sub <- data.frame(journals = journals,
                     article = article,
                     type = type,
                     citations = citations,
                     year = year,
                     pastYear = pastYear,
                     oa = oa)

df_sub$direct <- ifelse(df_sub$type == "direct", 1, 0)

## plot stuff---------------
# year x type
df_sub %>% count(type, year) %>% 
  ggplot(aes(x = year, y = n, fill = type, color = type)) +
  geom_point(pch = 21, alpha = 0.5) +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2) + I(x^3) + I(x^4), se = FALSE,
              size = 2) +
  theme_minimal()

# pastYear x type
df_sub %>% count(type, pastYear) %>% 
  ggplot(aes(x = pastYear, y = n, fill = type, color = type)) +
  geom_point(pch = 21, alpha = 0.5) +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2) + I(x^3) + I(x^4), se = FALSE,
              size = 2) +
  theme_minimal()

# citations x type
ggplot(df_sub, aes(x = type, y = citations, fill = type)) +
  geom_point(pch = 21, alpha = 0.5) +
  theme_minimal()

## models--------------
priors <- c( 
  # prior for the Intercept should be low because we expect very low rates overall
  # Makel report a 1.6% rate, we will be lenient and assume 10% with a generous spread
  set_prior("normal(-2.1973, 2.5)", class = "Intercept"),
  # priors for all fixed effects will be centered on zero
  set_prior("cauchy(0, scale = 2.5)", class = "b"),
  set_prior("cauchy(0, scale = 1)", class = "sd")
)

xlm2 = brm(direct ~ citations + year + pastYear + (1 | journals),
          data = df_sub, family = "bernoulli")



