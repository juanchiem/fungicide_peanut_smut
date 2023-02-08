library(tidyverse)

# In-vitro
de50 <- readxl::read_excel("data/in vitro.xlsx", sheet = "EC50") %>% 
  mutate_at(vars("ai"), funs(factor)) %>% 
  select(-chem_group) %>% 
  filter(ai!="Epoxiconazole")

# Pot
load("./data/pot_model.rda")
pot = res
pot15 = filter(pot, year==2015) %>% select(ai,prob) %>% 
  filter(ai!="Check")
pot16 = filter(pot, year==2016) %>% select(ai,prob) %>% 
  filter(ai!="Check")

# Field
load("./data/field_model.rda")
field = res
field15 = filter(field, year==2015) %>% select(ai,prob) %>% 
  filter(ai!="Check")
field16 = filter(field, year==2016) %>% select(ai,prob) %>% 
  filter(ai!="Check")

allres <- pot15 %>% left_join(pot16, by="ai") %>% 
  left_join(field15, by="ai") %>% 
  left_join(field16, by="ai") %>% 
  transmute(ai=ai, pot15 = prob.x, pot16=prob.y,  field15=prob.x.x, field16=prob.y.y)  

allres %>% select(-ai) %>% GGally::ggpairs(method = c("pairwise", "spearman"))

