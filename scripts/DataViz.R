## Title: Visualise & Explore
## Description: Exploring Data
## Authors: Timo Roettger
## Date: 21-03-22

### preprocessing---------------
## load in library
library(tidyverse)
library(truncnorm)
library(brms)
library(rstudioapi)

# package for visualization
library(tidybayes)
library(bayesplot)

## set graphical parameters-----------------------------------------------------

# global color scheme / non-optimized
# purple
col_purple = "#7b3294"
#light purple
col_lightPurple = "#c2a5cf"
# green
col_green = "#008837"
# light green
col_lightGreen = "#a6dba0"


## load in data-----------------------------------------------------------------
# getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path))

mention = read.csv("../data/mention.csv")
guidelines = read.csv("../data/guidelines.csv")

## wrangle data-----------------------------------------------------------------

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


## model------------------------------------------------------------------------

# priors
priors <- c( 
  # prior for the Intercept should be low because we expect very low rates overall
  # Makel report a 1.6% rate, we will be lenient and assume 10% with a generous spread using cauchy
  set_prior("normal(0, 2.5)", class = "Intercept"),
  # priors for all fixed effects will be centered on zero
  set_prior("cauchy(0, 2.5)", class = "b")
)

# contrast-code
df$openaccess_s <- ifelse(df$openaccess == 0, -0.5, 0.5)
df$binary_policy_s <- ifelse(df$binary_policy == 0, -0.5, 0.5)

# model
xmdl = brm(no_replic | trials(no_exp) ~ jif + openaccess_s + binary_policy_s,
          data = df, 
          prior = priors,
          cores = 4,
          family = binomial(link = "logit"))

# summary(xmdl)
# pp_check(xmdl)

## extract posteriors-----------------------------------------------------------

# extract predicted values for
predicted_values <- xmdl %>%
  spread_draws(b_Intercept, b_jif) %>%
  # make a list of relevant value range of logRT
  mutate(jif = list(seq(0, 4, 0.1))) %>% 
  unnest(jif) %>%
  # transform into proportion space using the plogis function
  mutate(pred = plogis(b_Intercept + b_jif*jif),
         # pred_closed_encouraged = plogis(b_Intercept + b_jif*jif),
         # pred_open_encouraged = plogis(b_Intercept + b_jif*jif + b_openaccess),
         # pred_closed_discouraged = plogis(b_Intercept + b_jif*jif + b_binary_policy),
         # pred_open_discouraged = plogis(b_Intercept + b_jif*jif + b_openaccess + b_binary_policy)
  ) %>%
  pivot_longer(cols = pred,
                        # c(pred_closed_encouraged,
                        # pred_open_encouraged,
                        # pred_closed_discouraged,
                        # pred_open_discouraged),
               names_to = "factors") %>% 
  #mutate(openaccess = ifelse(factors %in% c("pred_closed_encouraged","pred_closed_discouraged"), "closed", "open"),
  #       binary_policy = ifelse(factors %in% c("pred_closed_encouraged","pred_open_encouraged"), "encouraged", "not encouraged")) %>% 
  group_by(jif) %>%
  summarise(pred_m = mean(value, na.rm = TRUE),
            pred_low = quantile(value, prob = 0.025),
            pred_high = quantile(value, prob = 0.975)) 


## plot-------------------------------------------------------------------------

# change df for plt
df_plot <- df 
df_plot$openaccess <- as.factor(df_plot$openaccess)

# rename levels
df_plot$openaccess = ifelse(df_plot$openaccess == 0, "closed", "open")
df_plot$binary_policy = ifelse(df_plot$binary_policy == 0, "not encouraged", "encouraged")

# plot predicted values against data
mention_jif <- 
ggplot(data = predicted_values, aes(x = jif, y = pred_m)) +
  geom_ribbon(aes(ymin = pred_low, ymax = pred_high), alpha = 0.4,
              fill = "grey") +
  geom_line(color = "black", size = 1.5) +
  geom_point(data = df_plot, aes(x = jif, y = replic_rate, 
                                 fill = openaccess, shape = binary_policy,
                                 size = no_exp),
             alpha = 0.7, color = "white") +
  scale_shape_manual(values = c(22,21), guide = FALSE) +
  scale_fill_manual(values = c(col_green, col_purple)) +
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

# plot
ggsave(filename = "../plots/mention_jif.png",
               plot = mention_jif,
               device = "png",
               width = 150, 
               height = 120,
               units = "mm",
               #bg = "transparent",
               dpi = 300)
