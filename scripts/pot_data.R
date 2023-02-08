library(tidyverse)

dat <- readxl::read_excel("../data/active_ingredients_JEM.xlsx", 
                           sheet = "pots_raw") %>% 
  mutate_at(vars(c("year","ai")),funs(factor)) %>% 
  mutate(ai = factor(stringr::str_to_title(ai)),
         x3.4 = rowSums(select(., matches('x3|x4')))) %>% 
  filter(ai!="Epoxiconazole")
  
save(dat, file="../data/pot_data.RData")

dat %>%
  group_by(year, ai)%>%
  summarise(n=n())%>%
  knitr::kable()