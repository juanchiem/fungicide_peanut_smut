library(tidyverse)
# library(ggthemes)

dat0 <- readxl::read_excel("././data/active_ingredients_JEM.xlsx", sheet = "field_raw") %>% 
  mutate_at(vars(c("year","ai", "sprays", "bk")),funs(factor)) 
  
# levels(dat0$year)
# IS =  Σ (xi  * ni) / N 

# dat %>% mutate(sumVar=select(.,matches('x1|x2|x3|x4'))%>%rowSums()) 
dat <- dat0 %>% 
  filter(sprays!=1) %>% 
  droplevels() %>% 
  mutate(
    ai = factor(stringr::str_to_title(ai)), 
    ai = relevel(ai, ref="Check"),
    # dis_pod = rowSums(select(., matches('x1|x2|x3|x4'))),
    # inc = dis_pod/n_pods,
    # x0_p = rowSums(select(., matches('x0')))/n_pods,
    x3.4 = rowSums(select(., matches('x3|x4')))) %>% 
  # x3.4_p = x3.4/n_pods,
  # x4_p = rowSums(select(., matches('x4')))/n_pods) %>% 
  # s0_4 = (0*x0+1*x1+2*x2+3*x3+4*x4)/n_pods,
  # sev0_1 = (0*x0 + 0.01*x1 +0.1*x2 + 0.7*x3 + 1*x4)/ n_pods) %>% 
  group_by(year, ai, sprays, bk)%>%
  filter(ai!="Epoxiconazole") %>% 
  mutate(sample = row_number()) %>% ungroup 
  
save(dat0, dat, file="~/Dropbox/Papers/1 In progress/carbon_mani/data/field_data.RData")

dat %>%
  group_by(year, ai, sprays, bk)%>%
  summarise(n=n())%>%
  knitr::kable()
