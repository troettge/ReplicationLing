## Title: Figures from Kobrock & Roettger (2022)
## Description: Generate Publication-ready figures
## Authors: Kobrock & Roettger
## Contact: kkobrock@uni-osnabrueck.de
## Date: 21-03-22

# nifty code using the pacman package
# it checks if the packages specified below are installed, if not, they will be installed, if yes, they will be loaded
if (!require("pacman")) install.packages("pacman")
pacman::p_load(rstudioapi, truncnorm, tidyverse, brms, ggdist, distributional, ggstream, tidybayes, bayesplot, rticles, knitr, ggalluvial)

# set the current working directory to the one where this file is
current_working_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(current_working_dir)

## global color scheme / non-optimized
### purple
col_purple = "#7b3294"
### light purple
col_lightPurple = "#c2a5cf"
### green
col_green = "#008837"
### light green
col_lightGreen = "#a6dba0"

# loading the data
mention <-  read.csv("../data/mention.csv")
guidelines  <-  read.csv("../data/guidelines.csv")
coded_articles  <-  read.csv("../data/coded_updated.csv")  
sample_journals <- read.csv("../Sample_journals.csv")

# load models
mention_model <- readRDS(file = "../data/repl_mention2_mdl.RDS")

# merge dfs
df <- full_join(mention, guidelines)

# delete older journals that have been renamed
df <- df %>% 
  filter(!(journals %in% c("LANGUAGE AND COGNITIVE PROCESSES", "LITERARY AND LINGUISTIC COMPUTING")))

# recode jif
df$jif <- ifelse(df$jif == "not retrievable", NA, as.numeric(as.character(df$jif)))

# NAs would be dropped from model. What to do? Either assume 0 or separate models

# recode open access as binary
df$openaccess <- ifelse(df$openaccess == "DOAJ gold", 1, 0)

# list of shortened journal names
journals_abb <- c("Humor", "Lang Learn Dev", "Morphology", "Transl Interpreting", "J Logic Lang Inf", "J Mem Lang", "J East Asian Ling", "Cogn Linguist", "Int J Speech Lang La", "J Spec Transl", "Stud Second Lang Acq", "Lang Cogn Neurosci", "Glossa", "Linguistic Res", "First Lang", "J Neurolinguistics", "Lang Learn Technol", "Biling-Lang Cogn", "J Psycholinguist Res", "3L Lang Linguist Literat", "Lab Phonol", "Lang Teach Res", "Lang Cogn", "Lang Linguist Compass", "Metaphor Symb", "Lang Learn", "J Child Lang", "J Semant", "Aphasiology", "J Lang Soc Psychol", "Int J Lang Comm Dis", "J Phon", "Appl Psycholinguist", "Lang Speech", "Acta Linguist Hung", "J Cogn Sci", "Brain Lang", "Recall", "Int J Biling", "Phonetica", "Mind Lang", "Linguist Approach Bi", "IRAL-Int Rev Appl Li", "J Speech Lang Hear R", "Interact Stud", "Arab World Eng J", "Am J Speech-Lang Pat", "Ment Lex", "Clin Linguist Phonet", "Lang Acquis", "System", "Second Lang Res", "Nat Lang Eng", "Comput Assist Lang L", "Lingua", "Lect Notes Comput Sci", "Comput Linguist", "Lect Notes Artif Int", "Phonology", "Interpreting", "Eurasian J Appl Linguist", "J Lang Educat", "Linguistics Vanguard", "Proces de Leng Nat", "Appl Linguist Res J", "Nat Lang Semant", "J Quant Linguist", "Corpus Linguist Ling", "Rev Cogn Linguist", "Interpret Transl Tra", "Poz Stud Contemp Lin", "Pragmat Cogn", "Syntax-UK", "J Res Appl Linguist", "Digit Scholarsh Hum", "Probus", "Innov Lang Learn Teach", "Int J Eng Linguist", "Across Lang Cult", "Rev Roum Linguist", "Intercult Pragmat", "Child Lang Teach The", "Lang Aware", "Gesture", "J Int Phon Assoc", "Metaphor Symb Act", "Iberica", "Annu Rev Appl Linguist", "Ling Antverp New Ser", "Terminology", "Annu Rev Linguist", "J Fr Lang Stud", "Lang Linguist", "Nord J Linguist", "Lang Lit", "Babel-Amsterdam", "Int J Corpus Linguist", "Int J Appl Linguist")

# recode jif
df$jif <- as.numeric(ifelse(df$jif == "not retrievable", NA, df$jif)) 
df$jif_s <- df$jif - mean(df$jif, na.rm = TRUE)

# include shortened journal names in dataframe
df$journals_abb <- factor(journals_abb)

# calculate non-zero mentions
zero_mention = nrow(df[df$replic_rate == 0,])
nonzero_mention = nrow(df[df$replic_rate != 0,])

# plot Figure1------------------------------------------------------------------
Figure1 <- 
  df %>% arrange(desc(replic_rate)) %>%
  mutate(exp_ratio = round(exp_ratio,0),
         exp_rate = paste0(exp_ratio,"%"),
         replic_rate = replic_rate*100) %>% 
  select(journals_abb, exp_rate, replic_rate) %>% 
  top_n(nonzero_mention, replic_rate) %>% 
  #mutate(journals = fct_reorder(journals, replic_rate)) %>%
  mutate(journals_abb = fct_reorder(journals_abb, replic_rate)) %>% 
  ggplot(aes(x = replic_rate, 
             y = journals_abb,
             color = replic_rate)) +
  geom_segment(aes(x = 0, 
                   xend = replic_rate,
                   y = journals_abb,
                   yend = journals_abb)) +
  geom_point(size = 3) + 
  scale_color_gradient(low = col_purple,
                       high = col_green) +
  #scale_y_discrete(label = function(x) stringr::str_trunc(x, 20))+
  scale_x_continuous(limits = c(0,15), 
                     breaks = c(0,5,10,15),
                     labels = c("0%","5%","10%","15%")
  ) +
  geom_text(aes(label = exp_rate),
            x = 15,
            color = "#666666",
            cex = 3,
            hjust = 1,
            check_overlap = TRUE) +
  labs(y = " ",
       x = "\nProportion of replicat* mentions in %") + #\nProportion of 'replicat*' mentions in %\n") +
  theme_minimal() +
  theme(legend.position = "none",
        panel.spacing = unit(2, "lines"),
        panel.border = element_blank(),
        strip.text.y = element_text(size = 12, hjust = 0),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 8),
        axis.line = element_blank(),
        axis.title = element_text(size = 12, face = "bold"),
        plot.title = element_text(size = 14, face = "bold"),
        plot.margin = unit(c(0.6,0.6,0.6,0.6),"cm"))

## store plot pdf 
ggsave(filename = "../plots/Figure1.pdf",
       plot = Figure1,
       device = "pdf",
       width = 120, 
       height = 300,
       units = "mm",
       dpi = 300)


# plot Figure 2------------------------------------------------------------------
## extract predicted values for JIF_s from revised model 
predicted_values <- mention_model %>%
  spread_draws(b_Intercept, b_jif_s) %>%
  ### make a list of relevant value range of logRT
  mutate(jif = list(seq(-2, 3, 0.1))) %>% 
  unnest(jif) %>%
  ### transform into proportion space using the plogis function
  mutate(pred = plogis(b_Intercept + b_jif_s*jif)) %>%
  pivot_longer(cols = pred,
               names_to = "factors") %>% 
  group_by(jif) %>%
  summarise(pred_m = mean(value, na.rm = TRUE),
            pred_0025 = quantile(value, prob = 0.025),
            pred_0075 = quantile(value, prob = 0.075),
            pred_025 = quantile(value, prob = 0.25),
            pred_075 = quantile(value, prob = 0.75),
            pred_0925 = quantile(value, prob = 0.925),
            pred_0975 = quantile(value, prob = 0.975)) 


## plot predicted values against data
Figure2 <- 
  ggplot(data = predicted_values, 
         aes(x = jif, 
             y = pred_m)) +
  geom_ribbon(aes(ymin = pred_0025, 
                  ymax = pred_0975), 
              alpha = 0.2,
              fill = "grey") +
  geom_ribbon(aes(ymin = pred_0075, 
                    ymax = pred_0925), 
                alpha = 0.4,
                fill = "grey") +
  geom_ribbon(aes(ymin = pred_025, 
                    ymax = pred_075), 
                alpha = 0.6,
                fill = "grey") +
  geom_line(color = "black", 
            size = 1.5) +
  geom_point(data = df, 
             aes(x = jif_s, 
                 y = replic_rate,
                 size = exp_ratio,
                 fill = replic_rate),
             pch = 21,
             alpha = 0.8, 
             color = "white") +
  scale_fill_gradient(low = col_purple,
                      high = col_green) +
  ylim(0, 0.15) +
  #xlim(-2, 3) +
  scale_x_continuous(limits = c(min(df$jif_s, na.rm=T) - min(df$jif, na.rm=T), 3),
                     breaks = c(min(df$jif_s, na.rm=T) - min(df$jif, na.rm=T),
                                min(df$jif_s, na.rm=T) - min(df$jif, na.rm=T) + 1,
                                min(df$jif_s, na.rm=T) - min(df$jif, na.rm=T) + 2,
                                min(df$jif_s, na.rm=T) - min(df$jif, na.rm=T) + 3,
                                min(df$jif_s, na.rm=T) - min(df$jif, na.rm=T) + 4),
                     labels = c(0,1,2,3,4)) +
  labs(title = " ", 
       subtitle = "   ",
       y = "Empirical / Predicted\n rate of replication mention\n",
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

## store plot pdf 
ggsave(filename = "../plots/Figure2.pdf",
       plot = Figure2,
       device = "pdf",
       width = 120, 
       height = 120,
       units = "mm",
       dpi = 300)


# plot Figure 3------------------------------------------------------------------

# subset data
replication_sub <- coded_articles %>% 
  filter(experimental. == 1)

### let's plot the distribution in stream plot
replication_sub_agg <- replication_sub %>% 
  mutate(type2 = ifelse(is.na(type.of.replication), "no", 
                        ifelse(type.of.replication == "", "no", as.character(type.of.replication)))) %>% 
  group_by(type2, Publication.Year) %>% 
  summarise(n = n())

## change level order           
replication_sub_agg$type2 <- factor(replication_sub_agg$type2, 
                                    levels = c("no", "conceptual", "partial", "direct"))



## original stream plot
stream <- 
  ggplot(replication_sub_agg, aes(x = Publication.Year, y = n, fill = type2)) +
  geom_stream(bw = 0.7)  +
  scale_fill_manual(values = c("#998ec3", "#fee0b6", "#f1a340", "#b35806"),
                    name = "") +
  labs(
    #title = "Number of replication types from 1988-2020",
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
        axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 12),
        plot.title = element_text(size = 14, face = "bold"),
        plot.margin = unit(c(0.6,0.6,0.6,0.6),"cm"))

## store plot pdf 
ggsave(filename = "../plots/stream.pdf",
       plot = stream,
       device = "pdf",
       width = 160, 
       height = 120,
       units = "mm",
       dpi = 300)


## revised stream plot
Figure3 <- 
  ggplot(replication_sub_agg, aes(x = Publication.Year, y = n, fill = type2)) +
  geom_stream(bw = 0.7, type = "ridge")  +
  scale_fill_manual(values = c("#998ec3", "#fee0b6", "#f1a340", "#b35806"),
                    name = "") +
  labs(
    #title = "Number of replication types from 1988-2020",
    subtitle = "   ",
    x = "\npublication year",
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
        axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 12),
        plot.title = element_text(size = 14, face = "bold"),
        plot.margin = unit(c(0.6,0.6,0.6,0.6),"cm"))

## store plot pdf 
ggsave(filename = "../plots/Figure3.pdf",
       plot = Figure3,
       device = "pdf",
       width = 160, 
       height = 120,
       units = "mm",
       dpi = 300)
