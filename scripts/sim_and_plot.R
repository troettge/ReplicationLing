## Title: SimData for Replication Project
## Description: Simulate and plot
## Authors: Timo Roettger
## Date: 21-02-02

### preprocessing---------------
## load in library
library(tidyverse)
library(truncnorm)
library(brms)

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
rate_base <- rbeta(n = sample, shape1 = 0.1, shape2 = 5)
rate <- rate_base * (jif / 2)

# put together
df_all <- data.frame(journals = journals,
                     policies = policies,
                     jif = jif,
                     oa = oa,
                     rate = rate)

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
xlm = brm(rate ~ jif + oa + policies,
          data = df_all,
          family = "beta")

### replication type across 200 articles in 20 journals---------------
## make data frame

sample = 200

journal <- sample(LETTERS[1:20], replace = TRUE, sample,
                  prob = rtruncnorm(20, a = 0, b = 1, mean = 0.05, sd = 0.1))

article <- 1:200

# type of replication: direct, partial, conceptual
type <- sample(c("direct", "partial", "conceptual"), sample, 
                     replace = TRUE, prob = c(0.1, 0.3, 0.6))

# citation of original study: 0-500
citations <- rtruncnorm(n = sample, a = 0, b = 500, mean = 20, sd = 30)

# publication year: 1970-2020
year <- round(runif(200, min = 2000, max = 2020), 0)

# years past: 0-10
pastYear <- round(runif(200, min = 1970, max = 2020), 0)

# put together
df_sub <- data.frame(journals = journals,
                     article = article,
                     type = type,
                     citations = citations,
                     year = year,
                     pastYear = pastYear)


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
xlm2 = brm(type ~ citations + year + (citations + year | journals),
          data = df_sub, family = "categorical")



