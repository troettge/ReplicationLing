## Title: Visualise & Explore
## Description: Exploring Data
## Authors: Timo Roettger
## Date: 21-03-22

# preprocessing---------------
## load in library
library(tidyverse)
library(truncnorm)
library(brms)
library(rstudioapi)
library(ggdist)
library(distributional)
library(ggstream)

## package for visualization
library(tidybayes)
library(bayesplot)

# set graphical parameters-----------------------------------------------------

## global color scheme / non-optimized
### purple
col_purple = "#7b3294"
### light purple
col_lightPurple = "#c2a5cf"
### green
col_green = "#008837"
### light green
col_lightGreen = "#a6dba0"


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

# NAs would be dropped from model. What to do? Either assume 0 or seperate models

# recode open access as binary
df$openaccess <- ifelse(df$openaccess == "DOAJ gold", 1, 0)


# MENTIONS: model--------------------------------------------------------------

## priors
priors <- c( 
  ### prior for the Intercept should be low because we expect very low rates overall
  ### Makel report a 1.6% rate, we will be lenient and assume 10% with a generous spread using cauchy
  set_prior("normal(0, 2.5)", class = "Intercept"),
  ### priors for all fixed effects will be centered on zero
  set_prior("cauchy(0, 2.5)", class = "b")
)

## contrast-code
df$openaccess_s <- ifelse(df$openaccess == 0, -0.5, 0.5)
df$binary_policy_s <- ifelse(df$binary_policy == 0, -0.5, 0.5)

## model
xmdl = brm(no_replic | trials(no_exp) ~ jif + openaccess_s + binary_policy_s,
          data = df, 
          prior = priors,
          cores = 4,
          file  = "../data/repl_mention_mdl.RDS",
          family = binomial(link = "logit"))

## summary(xmdl)
## pp_check(xmdl)

# MENTIONS: extract posteriors-----------------------------------------------------------

## extract predicted values for JIF
predicted_values <- xmdl %>%
  spread_draws(b_Intercept, b_jif) %>%
  ### make a list of relevant value range of logRT
  mutate(jif = list(seq(0, 4, 0.1))) %>% 
  unnest(jif) %>%
  ### transform into proportion space using the plogis function
  mutate(pred = plogis(b_Intercept + b_jif*jif)) %>%
  pivot_longer(cols = pred,
               names_to = "factors") %>% 
  group_by(jif) %>%
  summarise(pred_m = mean(value, na.rm = TRUE),
            pred_low = quantile(value, prob = 0.025),
            pred_high = quantile(value, prob = 0.975)) 

# calculate mean JIF
jif_mean <- mean(df$jif, na.rm = TRUE)

## extract predicted values for forest plot
predicted_values2 <- xmdl %>%
  spread_draws(b_Intercept, b_jif, b_openaccess_s, b_binary_policy_s) %>%
  select(b_Intercept, b_jif, b_openaccess_s, b_binary_policy_s) %>% 
  ### transform into proportion space using the plogis function
  pivot_longer(cols = c(b_Intercept, b_jif, b_openaccess_s, b_binary_policy_s),
               names_to = "parameters") 

predicted_values3 <- predicted_values2 %>% 
  group_by(parameters) %>% 
  summarise(pred_m = mean(value, na.rm = TRUE),
            pred_low = quantile(value, prob = 0.025),
            pred_high = quantile(value, prob = 0.975))

predicted_values2 <- predicted_values2 %>% full_join(predicted_values3)

# MENTIONS: plot---------------------------------------------------------------

## change df for plt
df_plot <- df 
df_plot$openaccess <- as.factor(df_plot$openaccess)

## rename levels
df_plot$openaccess = ifelse(df_plot$openaccess == 0, "closed", "open")
df_plot$binary_policy = ifelse(df_plot$binary_policy == 0, "not encouraged", "encouraged")

## plot predicted values against data
mention_jif <- 
ggplot(data = predicted_values, 
       aes(x = jif, 
           y = pred_m)) +
  geom_ribbon(aes(ymin = pred_low, 
                  ymax = pred_high), 
              alpha = 0.4,
              fill = "grey") +
  geom_line(color = "black", 
            size = 1.5) +
  geom_point(data = df_plot, 
             aes(x = jif, 
                 y = replic_rate,
                 size = no_exp),
             pch = 21,
             alpha = 0.7, 
             color = "white", 
             fill = col_purple) +
  scale_fill_manual(values = col_purple) +
  ylab("Predicted rate of replication mention") +
  ylim(0, 0.20) +
  xlim(0, 4) +
  labs(title = "Rate of mentioning the term 'replicat*'",
       subtitle = "   ",
       y = "Predicted rate of replication mention\n",
       x = "\nJournal Impact Factor") +
  theme_minimal() +
  theme(legend.position = "none",
              legend.key.height = unit(2,"line"),
              legend.title = element_text(face = "bold", size = 12),
              legend.text = element_text(size = 12),
              strip.background = element_blank(),
              strip.text = element_text(size = 12, face = "bold"),
              panel.spacing = unit(2, "lines"),
              panel.border = element_blank(),
              plot.background = element_rect(fill = "transparent", colour = NA),
              strip.text.y = element_text(size = 12, hjust = 0),
              axis.text = element_text(size = 12),
              axis.line = element_blank(),
              axis.title = element_text(size = 12, face = "bold"),
              plot.title = element_text(size = 14, face = "bold"),
              plot.margin = unit(c(0.6,0.6,0.6,0.6),"cm"))

## plot
ggsave(filename = "../plots/mention_jif.png",
               plot = mention_jif,
               device = "png",
               width = 120, 
               height = 96,
               units = "mm",
               dpi = 300)


## could this effect be driven by the number experimental studies?
#### plot exp-ratio vs. jif
ggplot(data = df_plot, aes(y = exp_ratio, 
                           x = jif,
                           size = no_exp)) +
  geom_point(alpha = 0.7, 
             color = col_green) +
  ylab("Predicted rate of replication mention") +
  xlim(0, 4) +
  ylim(0, 75) +
  labs(title = "Rate of mentioning the term 'replicat*'",
       subtitle = "   ",
       x = "Journal Impact Factor\n",
       y = "\n% of experimental studies") +
  theme_minimal() +
  theme(legend.position = "none")

### yeah, as I thought, highly correlated
df_red <- df[!is.na(df$jif),]
cor(df_red$exp_ratio, df_red$jif)


## forest plot
### reorder levels
predicted_values2$parameters <- factor(predicted_values2$parameters, 
                                    levels = c("b_jif", "b_binary_policy_s", "b_openaccess_s"))

forest_mention <- 
ggplot(predicted_values2 %>% filter(parameters != "b_Intercept"), 
       aes(x = value, 
           y = parameters)) +
  geom_vline(xintercept = 0,
             lty = "dashed") +
  stat_halfeye(aes(fill = stat(cut_cdf_qi(
    cdf, 
    .width = c(.5, .8, .95),
    labels = scales::percent_format()
  )))) +
  scale_fill_brewer(direction = -1, 
                    na.translate = FALSE) +
  labs(title = "Model parameters for replic* mention",
       x = "posterior log odds",
       y = "",
       fill = "quantiles") +
  scale_y_discrete(labels = c('Journal Impact Factor', 'Policy', 'Open Access')) +
  #scale_x_continuous(breaks = (c(-1,-.5,0,.5,1)), limits = c(-1,1.5)) +
  theme_minimal() +
  theme(legend.position = "none",
        legend.key.height = unit(2,"line"),
        legend.title = element_text(face = "bold", size = 12),
        legend.text = element_text(size = 12),
        strip.background = element_blank(),
        strip.text = element_text(size = 12, face = "bold"),
        panel.spacing = unit(2, "lines"),
        panel.border = element_blank(),
        plot.background = element_rect(fill = "transparent", colour = NA),
        strip.text.y = element_text(size = 12, hjust = 0),
        axis.text = element_text(size = 12),
        axis.line = element_blank(),
        axis.title = element_text(size = 12, face = "bold"),
        plot.title = element_text(size = 14, face = "bold"),
        plot.margin = unit(c(0.6,0.6,0.6,0.6),"cm"))


ggsave(filename = "../plots/forest_mention.png",
       plot = forest_mention,
       device = "png",
       width = 160, 
       height = 120,
       units = "mm",
       dpi = 300)


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
         citation_initial = citation.count.until.replication.study.was.published..retrieved..16.08.2021.)


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

## contrast-code
replication_sub$oa_s <- ifelse(replication_sub$oa == 0, -0.5, 0.5)

## make direct replication vector
replication_sub$direct <- ifelse(replication_sub$type == "direct", 1, 0)

## model
xmdl2 = brm(direct ~ year + oa_s + lag + citation_initial,
           data = replication_sub, 
           prior = priors,
           cores = 4,
           file  = "../data/direct_replication_mdl.RDS",
           family = bernoulli(link = "logit"))

## summary(xmdl2)
## pp_check(xmdl)

### model is pretty useless, because replication rates are at floor

### let's plot the distribution in stream plot
replication_sub_agg <- replication_sub %>% 
  mutate(type2 = ifelse(is.na(type), "no", 
                        ifelse(type == "", "no", type))) %>% 
  group_by(type2, year) %>% 
  summarise(n = n())
  
## change level orfer           
replication_sub_agg$type2 <- factor(replication_sub_agg$type2, 
                                    levels = c("no", "conceptual", "partial", "direct"))

## stream plot
stream_plot <- 
ggplot(replication_sub_agg, aes(x = year, y = n, fill = type2)) +
  geom_stream(bw = 0.7)  +
  scale_fill_manual(values = c("#998ec3", "#fee0b6", "#f1a340", "#b35806"),
                    name = "") +
  labs(title = "Number of replication types from 1988-2020",
       subtitle = "   ",
       x = "\nYear",
       y = "number of papers in corpus\n") +
  theme_minimal() +
  theme(legend.key.height = unit(2,"line"),
        legend.title = element_text(face = "bold", size = 12),
        legend.text = element_text(size = 12),
        strip.background = element_blank(),
        strip.text = element_text(size = 12, face = "bold"),
        panel.spacing = unit(2, "lines"),
        panel.border = element_blank(),
        plot.background = element_rect(fill = "transparent", colour = NA),
        strip.text.y = element_text(size = 12, hjust = 0),
        axis.text = element_text(size = 12),
        axis.line = element_blank(),
        axis.title = element_text(size = 12, face = "bold"),
        plot.title = element_text(size = 14, face = "bold"),
        plot.margin = unit(c(0.6,0.6,0.6,0.6),"cm"))

## store
ggsave(filename = "../plots/stream_plot.png",
       plot = stream_plot,
       device = "png",
       width = 160, 
       height = 120,
       units = "mm",
       dpi = 300)


## 