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

mention <-  read.csv("../data/mention.csv")
guidelines  <-  read.csv("../data/guidelines.csv")
replication  <-  read.csv("../data/Coding_Articles.csv")  


## wrangle data mention and guideline--------------------------------------------

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
               width = 120, 
               height = 96,
               units = "mm",
               #bg = "transparent",
               dpi = 300)


## could this effect be driven by the number experimental findings?
# plot exp-ratio vs. jif
ggplot(data = df_plot, aes(x = exp_ratio, y = jif,
                             size = no_exp)) +
  geom_point(alpha = 0.7, color = col_green) +
  ylab("Predicted rate of replication mention") +
  ylim(0, 4) +
  xlim(0, 75) +
  labs(title = "Rate of mentioning the term 'replicat*'",
       subtitle = "   ",
       y = "Journal Impact Factor\n",
       x = "\n% of experimental studies") +
  theme_minimal() +
  theme(legend.position = "none")

# yeah, as I though, highly correlated
df_red <- df[!is.na(df$jif),]
cor(df_red$exp_ratio, df_red$jif)

## wrangle data replication----------------------------------------------------

# how many really experimental
sum(replication$experimental.) / nrow(replication)
# 0.95

#subset only those
replication_sub <- replication %>% 
  filter(experimental. == 1)

# of those how many replications
sum(replication_sub$Replication.) / nrow(replication_sub)
# 0.58

#subset only those
replication_sub_repli <- replication_sub %>% 
  filter(Replication. == 1)

# of those what types of replications
xtabs(~type.of.replication, replication_sub_repli) / nrow(replication_sub_repli)
# 0.57 conceptual
# 0.07 direct
# 0.36 partial

# of those author overlap
xtabs(~type.of.replication + author.overlap, replication_sub_repli) / nrow(replication_sub_repli)

# plot years distance for those categories
ggplot(replication_sub_repli,
       aes(x = type.of.replication, 
           y = years.between.initial.and.replication.study,
           color = as.factor(author.overlap))) +
  geom_point(alpha = 0.5, size = 3,
             position = position_jitter(0.05)) + 
  facet_grid(~as.factor(author.overlap))


# plot publication year
ggplot(replication_sub,
       aes(x = Publication.Year, 
           #y = Replication.,
           fill = type.of.replication)
       ) +
  geom_histogram(alpha = 0.5) + 
  facet_grid(type.of.replication ~ .)


