# dealing with the old zoop data sheet ---- 
zoop <- read_excel("Live counts.xlsx")

library(tidyverse)
library(magrittr)

zoop %<>% mutate(parasite = case_when(
  infect=="U" ~ "uninfected",
  infect=="L" ~ "larssonia",
  infect=="G" ~ "gurleya",
  infect=="B" ~ "brood",
  infect=="C" ~ "chytrid",
  infect=="M" ~ "metsch",
  infect=="P" ~ "pasteuria",
  TRUE ~ "check"
))

zoop %<>% mutate(demography = case_when(
  dem=="J" ~ "juvenile",
  dem=="A" ~ "adult",
  dem=="M" ~ "male",
  dem=="E" ~ "ephippial",
  TRUE ~ "check"
))

write.csv(zoop,"zoop.csv") #this file is now pond_zoop.csv

# start here for edited zoop spreadsheet ----- 
setwd("~/GitHub/general-lab-exp/CB_water_research_poster_Oct_2023")
zoop <- read.csv("pond_zoop.csv")
library(tidyverse)
library(magrittr)
zoop$date<-mdy(zoop$date)

# making the poster ---- 
# community composition - showing what hosts are present in each of the ponds
# starting with just Whitehall

zoop2 <- zoop %>% filter(lake_id!="Herrick"&lake_id!="NN1"&
                           lake_id!="NN2"&lake_id!="Picks"&
                           lake_id!="Parvo"&lake_id!="Bear Creek"
                           )

zoop2 %<>% mutate(lake_id = ifelse(lake_id=="Deans","Dean's",lake_id))

zoop_goodlakes <- zoop %>% filter(lake_id!="Herrick"&lake_id!="NN1"&
                                    lake_id!="NN2"&lake_id!="Picks"&
                                   lake_id!="Parvo"&lake_id!="Bear Creek"&
                                    lake_id!="Memorial"&
                                    lake_id!="Oglethorpe"&
                                    lake_id!="Chapman") %>%
  mutate(lake_id = ifelse(lake_id=="Deans","Dean's",lake_id))

zoop_goodlakes2 <- zoop %>% filter(lake_id!="Herrick"&lake_id!="NN1"&
                                    lake_id!="NN2"&lake_id!="Picks"&
                                    lake_id!="Parvo"&lake_id!="Bear Creek") %>%
  mutate(lake_id = ifelse(lake_id=="Deans","Dean's",lake_id))

# figure out which species are infected
# filter out uninfected individuals
zoop2 %<>% filter(parasite!="uninfected")

table(zoop2$taxon)

count <- zoop2 %>% group_by(lake_id) %>%
  summarize(total_host = sum(count)) %>%
  select(lake_id,total_host)

zoop3 <- merge(zoop2,count,by="lake_id")

fig1 <- zoop3 %>% group_by(lake_id,taxon) %>%
  summarize(proportion_host = unique(sum(count)/total_host))

library(wacolors)

fig1 %<>% 
  mutate(Host = case_when(
    taxon == "bos" ~ "Bosmina",
    taxon == "cer" ~ "Ceriodaphnia",
    taxon == "dam" ~ "D. ambigua",
    taxon == "dia" ~ "Diaphanosoma",
    taxon == "dla" ~ "D. laevis",
    taxon == "dpa" ~ "D. parvula",
    taxon == "sim" ~ "Simocephalus"
  )) 
fig1$Host <- factor(fig1$Host,levels=c("D. ambigua","D. laevis","D. parvula","Ceriodaphnia","Bosmina","Diaphanosoma","Simocephalus"))

fig1$lake_id <-factor(fig1$lake_id,levels=c("Big Sister","Catfish","Dean's","Deer","NN3","VIP","Sister 1","Chapman","Memorial","Oglethorpe"))

fig1 %<>% mutate(facet <- case_when(
  lake_id=="Big Sister" ~ "panel1",
  lake_id=="Catfish" ~ "panel1",
  lake_id=="Dean's" ~ "panel1",
  lake_id=="Big Deer" ~ "panel1",
  lake_id=="NN3" ~ "panel1",
  TRUE ~ "panel2"))

#stacked plot
ggplot(fig1, aes(x=lake_id,y=proportion_host,fill=Host)) +
  geom_bar(stat="identity") +
  xlab("Pond") +
  theme_classic() +
  ylab("Proportion of cladoceran community") +
  scale_fill_wa_d("sea_star") +
  theme(text = element_text(size=20)) 
ggsave("communitycomp.png",dpi=500,height=8,width=16,units="in")

fig1 %>% filter(lake_id=="Big Sister"|lake_id=="Catfish"|lake_id=="Dean's"|lake_id=="Deer"|lake_id=="NN3") %>%
  ggplot(aes(x=lake_id,y=proportion_host,fill=Host)) +
  geom_bar(stat="identity") +
  xlab("Pond") +
  theme_classic() +
  ylab("Proportion of cladoceran community") +
  scale_fill_wa_d("sea_star") +
  theme(text = element_text(size=20)) 
ggsave("communitycomp1.png",dpi=500,height=8,width=12,units="in")
fig1 %>% filter(lake_id!="Big Sister"&lake_id!="Catfish"&lake_id!="Dean's"&lake_id!="Deer"&lake_id!="NN3") %>%
  ggplot(aes(x=lake_id,y=proportion_host,fill=Host)) +
  geom_bar(stat="identity") +
  xlab("Pond") +
  theme_classic() +
  ylab("Proportion of cladoceran community") +
  scale_fill_wa_d("sea_star") +
  theme(text = element_text(size=20)) 
ggsave("communitycomp2.png",dpi=500,height=8,width=12,units="in")

#unstacked plot 
ggplot(fig1, aes(x=lake_id,y=proportion_host,fill=taxon)) +
  geom_bar(stat="identity",
           position=position_dodge2(width=0.7,preserve="single")) +
  theme_classic() +
  scale_y_continuous(expand=c(0,0),limits=c(0,1))

# do the same thing, but by sampling date


# figure/table 2 ----
# proportion of hosts infected with each parasite

#calculate the total number of hosts for each sampling date and host species
z1 <- zoop_goodlakes %>% group_by(lake_id,date,taxon) %>%
  summarize(total_hosts = sum(count)) 

z2 <- zoop_goodlakes %>% select(c(lake_id,date,taxon,infect,count)) %>%
  group_by(lake_id,date,taxon,infect) %>%
  summarize(count = sum(count)) %>% 
  filter(infect !="U")

fig2<-merge(z1,z2,by=c("lake_id","date","taxon"))

fig2 %<>% group_by(lake_id,date,taxon,infect) %>%
  mutate(prev = count/total_hosts*100)

z3 <- zoop_goodlakes2 %>% group_by(lake_id,date,taxon) %>%
  summarize(total_hosts = sum(count)) 

z4 <- zoop_goodlakes2 %>% select(c(lake_id,date,taxon,infect,count)) %>%
  group_by(lake_id,date,taxon,infect) %>%
  summarize(count = sum(count)) %>% 
  filter(infect !="U")

fig2B<-merge(z3,z4,by=c("lake_id","date","taxon"))
fig2B %<>% group_by(lake_id,date,taxon,infect) %>%
  mutate(prev = count/total_hosts)

#calculate max prevalence for each host species and each parasite

max <- fig2B %>% filter(total_hosts>25) %>% group_by(taxon,infect) %>% summarize(maxprev = max(prev))

# graph prevalence over time ---- 
fig2 %>% ggplot(aes(x=date,y=prev,fill=infect,group=infect)) +
  geom_bar(stat="identity",position=position_dodge2(width=0.7,preserve="single"))+
  theme_classic() +
  facet_wrap(~lake_id,nrow=2)

#change factor levels so that chapman, memorial, and oglethorpe are last
fig2B$lake_id <-factor(fig2B$lake_id,levels=c("Big Sister","Catfish","Dean's","Deer","NN3","VIP","Sister 1","Chapman","Memorial","Oglethorpe"))

#prevalence of larssonia microsporidian infections over time
library(wacolors)

fig2B %<>% 
  mutate(Host = case_when(
    taxon == "bos" ~ "Bosmina",
    taxon == "cer" ~ "Ceriodaphnia",
    taxon == "dam" ~ "D. ambigua",
    taxon == "dia" ~ "Diaphanosoma",
    taxon == "dla" ~ "D. laevis",
    taxon == "dpa" ~ "D. parvula",
    taxon == "sim" ~ "Simocephalus"
  )) 
fig2B$Host <- factor(fig2B$Host,levels=c("D. ambigua","D. laevis","D. parvula","Ceriodaphnia","Bosmina","Diaphanosoma","Simocephalus"))

fig2B %>% filter(total_hosts > 25) %>%
  filter(infect=="L") %>%
  ggplot(aes(x=date,y=prev,group=Host,color=Host)) +
  geom_point() +
  geom_line() +
  ylab("Prevalence of Larssonia complex microsporidian parasites")+
  xlab("Sample Date")+
  theme_classic() +
  facet_wrap(~lake_id,nrow=5)+
  theme(text = element_text(size=20), axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5,size=16))+
  scale_x_date(date_breaks = "3 months", date_labels =  "%b %Y") +
  scale_color_wa_d("sea_star")
ggsave("larssonia_over_time3.png",dpi=500,height=12,width=12,units="in")

fig2B %>% filter(total_hosts > 25) %>%
  filter(infect=="L") %>%
  ggplot(aes(x=date,y=prev,group=taxon,color=taxon)) +
  geom_point() +
  geom_line() +
  ylab("Prevalence of Larssonia complex microsporidian parasites")+
  xlab("Sample Date")+
  theme_classic() +
  facet_wrap(~lake_id,nrow=5)+
  theme(text = element_text(size=20), axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5,size=16))+
  scale_x_date(date_breaks = "3 months", date_labels =  "%b %Y") 

ggsave("larssonia_over_time2.png",dpi=500,height=12,width=12,units="in")


#now time for other parasites over time
#first summarize by parasite
fig3<-fig2B %>% filter(total_hosts>25) %>%
  group_by(lake_id,date,infect) %>%
  summarize(total_prev=sum(prev)) %>%
  ungroup() 
fig3B <- fig3 %>% group_by(lake_id,infect) %>%
  summarize(max_prev=max(total_prev))

figure3 <- merge(fig3,fig3B,by=c("lake_id","infect"))

figure3 %<>% filter(infect!="L+G"&infect!="L+B")

figure3$percent_max <- figure3$total_prev/figure3$max_prev

#graph percent of max prev over time
figure3$mp1 <- ifelse(figure3$percent_max>=0.1,1,0)

figure3 %>% filter(infect!="L") %>%
  filter(mp1>0) %>%
  ggplot(aes(x=date,y=infect,color=infect)) +
  geom_point()+
  xlab("Sample Date")+
  theme_classic() +
  facet_wrap(~lake_id,nrow=4)+
  theme(text = element_text(size=20), axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5,size=16))+
  scale_x_date(date_breaks = "3 months", date_labels =  "%b %Y")
ggsave("other_outbreaks.png",dpi=500,height=12,width=12,units="in")

figure3 %>%
  filter(mp1>0) %>%
  ggplot(aes(x=date,y=infect,color=infect)) +
  geom_point()+
  xlab("Sample Date")+
  theme_classic() +
  facet_wrap(~lake_id,nrow=4)+
  theme(text = element_text(size=20), axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5,size=16))+
  scale_x_date(date_breaks = "3 months", date_labels =  "%b %Y")

#extra stuff from last time
zoop_sum <- zoop %>% group_by(lake_id,date,parasite) %>%
  summarize(total = sum(count))

zoop_sum %>% filter(lake_id=="Oglethorpe") %>%
  ggplot(aes(x=date,y=total,color=parasite,group=parasite)) +
  geom_point() +
  geom_line(aes(group=parasite)) +
  theme_classic()

zoop_sum %>% filter(lake_id=="Oglethorpe"&parasite!="uninfected") %>%
  ggplot(aes(x=date,y=total,color=parasite,group=parasite)) +
  geom_point() +
  geom_line(aes(group=parasite)) +
  theme_classic()

zoop_sum %<>% filter(lake_id!="Herrick"&lake_id!="NN1"&lake_id!="NN2"&lake_id!="Picks"&lake_id!="Parvo"&lake_id!="Bear Creek")

zoop_sum %>% 
  ggplot(aes(x=date,y=total,color=parasite,group=parasite)) +
  geom_point() +
  geom_line(aes(group=parasite)) +
  theme_classic() +
  facet_wrap(.~lake_id)

zoop_sum %>% filter(parasite!="uninfected"&lake_id!="Catfish") %>%
  ggplot(aes(x=date,y=total,color=parasite,group=parasite)) +
  geom_point() +
  geom_line(aes(group=parasite)) +
  theme_classic() +
  facet_wrap(.~lake_id)


