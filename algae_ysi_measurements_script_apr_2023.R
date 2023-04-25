setwd("/Users/katieschroeder/Documents/GitHub/general-lab-exp")

library(tidyverse)

alga <- read_csv("mic_ank_exp_april_19_2023_processed2.csv")

alga_long <- alga %>%
  pivot_longer(
    cols = chl_ugL:pc_rfu,
    names_to = c("pigment","reading_unit"),
    names_sep = "_",
    values_to = "reading_value"
  )

#summarize into each tank
alga_sum <- alga_long %>% group_by(tank,pigment,reading_unit) %>%
  summarize(mean_read = mean(reading_value),
            count = n(),
            se_read = sd(reading_value)/sqrt(count),
            exp = unique(exp),
            conc_ank = unique(conc_ank),
            conc_mic = unique(conc_mic)) %>%
  mutate(conc_tot = conc_ank+conc_mic)



#look at chlorophyll for each species first

alga_sum %>% filter(exp != "combo") %>% 
  filter(reading_unit == "rfu") %>%
  ggplot (aes(x = conc_tot, y = mean_read, color = exp, group = exp)) + 
  geom_point() + 
  geom_line(aes(group=exp)) +
  geom_errorbar(aes(ymin = mean_read - se_read, ymax = mean_read + se_read), width = 0.1) + 
  theme_classic() + 
  facet_wrap(.~pigment)

alga_sum %>% filter(exp == "mic_only") %>% 
  filter(reading_unit == "rfu") %>%
  ggplot (aes(x = conc_tot, y = mean_read, group = exp)) + 
  geom_point() + 
  geom_line(aes(group=exp)) +
  geom_errorbar(aes(ymin = mean_read - se_read, ymax = mean_read + se_read), width = 0.1) + 
  theme_classic() + 
  facet_wrap(.~pigment)

alga_sum %>% filter(exp == "ank_only") %>% 
  filter(reading_unit == "rfu") %>%
  ggplot (aes(x = conc_tot, y = mean_read, group = exp)) + 
  geom_point() + 
  geom_line(aes(group=exp)) +
  geom_errorbar(aes(ymin = mean_read - se_read, ymax = mean_read + se_read), width = 0.1) + 
  theme_classic() + 
  facet_wrap(.~pigment)

#really not what we expected. ugL looks exactly the same

#look at combo experiments
alga_sum %>% 
  filter(exp != "mic_only") %>%
  filter(reading_unit == "rfu") %>% 
  ggplot(aes(x=conc_ank,y=mean_read,group = conc_mic, color = as.factor(conc_mic))) + 
  geom_point() + 
  geom_line(aes(group = conc_mic)) +
  theme_bw() +
  facet_wrap(.~pigment)

alga_sum %>% 
  filter(exp != "ank_only") %>%
  filter(reading_unit == "rfu") %>% 
  ggplot(aes(x=conc_mic,y=mean_read,group = conc_ank, color = as.factor(conc_ank))) + 
  geom_point() + 
  geom_line(aes(group = conc_ank)) +
  theme_bw() +
  facet_wrap(.~pigment)

#chlorophyll to pc ratio?
ratio <- alga %>% 
  mutate(ratio_rfu = chl_rfu / pc_rfu,
         ratio_ugL = chl_ugL / pc_ugL) %>%
  group_by(tank) %>%
  summarize(mean_rfu_ratio = mean(ratio_rfu),
            mean_ugL_ratio = mean(ratio_ugL),
            count = n(),
            se_rfu_ratio = sd(ratio_rfu)/sqrt(count),
            se_ugL_raito = sd(ratio_ugL)/sqrt(count),
            conc_ank = unique(conc_ank),
            conc_mic = unique(conc_mic),
            exp = unique(exp)) %>%
  mutate(conc_tot = conc_ank + conc_mic,
         rel_conc_ank = conc_ank / conc_tot,
         rel_conc_mic = conc_mic / conc_tot) 

ratio %>% filter(conc_tot != 0) %>% 
  ggplot(aes(x = rel_conc_ank,
         y = mean_rfu_ratio,
         group = exp,
         color = as.factor(conc_mic),
         label = tank)) +
  geom_errorbar(aes(ymin = mean_rfu_ratio - se_rfu_ratio,ymax = mean_rfu_ratio + se_rfu_ratio))+
  geom_point() +
  theme_bw() +
  geom_text(vjust = 2, hjust = 2)

alga_sum %>% filter(conc_tot != 0) %>% 
  filter(pigment == "pc"&reading_unit=="rfu") %>%
  ggplot(aes(x = conc_tot,
             y = mean_read,
             group = exp,
             color = exp)) +
  geom_errorbar(aes(ymin = mean_read - se_read,ymax = mean_read + se_read))+
  geom_point() +
  theme_bw()
  

ratio %>% ggplot 

ratio %>% filter(conc_tot != 0) %>%
  ggplot(aes(x = rel_conc_mic,
             y=ratio_ugL, 
             group = exp, 
             color = conc_ank)) +
  geom_point() +
  theme_bw() 

  




