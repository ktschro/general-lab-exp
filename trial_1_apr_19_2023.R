library(tidyverse)
alga <- read_csv("mic_ank_exp_april_19_2023_processed.csv")


#visualize each tank
#things in rfus
ggplot(alga,aes(x=tank,y=chl_rfu,group=tank)) + geom_boxplot() + theme_bw()
ggplot(alga,aes(x=tank,y=pc_rfu,group=tank)) + geom_boxplot() + theme_bw()

#things in ug/L
ggplot(alga,aes(x=tank,y=chl_ugL,group=tank)) + geom_boxplot() + theme_bw()
ggplot(alga,aes(x=tank,y=pc_ugL,group=tank)) + geom_boxplot() + theme_bw()

#summarize each trial into single value

alga_sum<-alga %>% 
  group_by(tank) %>%
  summarize(mean_chl = mean(chl_rfu),
            count = n(),
            se_chl = sd(chl_rfu),sqrt(count),
            mean_pc = mean(pc_rfu),
            se_pc = sd(pc_rfu)/sqrt(count),
            mean_chl_u = mean(chl_ugL),
            se_chl_u = sd(chl_ugL),sqrt(count),
            mean_pc_u = mean(pc_ugL),
            se_pc_u = sd(pc_ugL)/sqrt(count),
            exp = unique(exp),
            conc_ank = unique(conc_ank),
            conc_mic = unique(conc_mic))

#pivot longer so we can plot ank and mic on the same axes for both pc and ank



