checking<-read.csv("Checking1.csv")
credit<-read.csv("CreditCard3.csv")

library(tidyverse)

spending <- bind_rows(checking,credit)

spending <- spending %>% 
  mutate(category = case_when(
    str_detect(vendor,"AMERICAN EXPRESS") ~ "Credit Card Payment",
    str_detect(vendor,"Joiner Managemen") ~ "Rent",
    str_detect(vendor,"Ge EDI PYMNTS") ~ "Income",
    str_detect(vendor,"SPECTRUM") ~ "Internet",
    str_detect(vendor,"APPLE.COM") ~ "Subscriptions",
    str_detect(vendor,"Title 9|GOODWILL") ~ "Clothing",
    str_detect(vendor,"KROGER FUEL|RACETRAC|SHELL") ~ "Gas",
    str_detect(vendor,"GEORGIA POWER") ~ "Electric",
    str_detect(vendor,"KROGER|PUBLIX|TRADER JOE'S") ~ "Groceries",
    
    
))
