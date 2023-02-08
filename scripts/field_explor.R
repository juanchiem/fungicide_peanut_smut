load("~/Dropbox/Papers/1 In progress/carbon_mani/data/field_data.RData")

# solo checks: ver presion de enfermedad de cada año y si hay heterogeneidad entre bloques 
dat %>% 
  filter(ai=="check") %>% #cyproconazole 
  ggplot(aes(x=bk, y=x3.4/n_pods)) +
  geom_boxplot()+
  geom_jitter(width=0.1)+
  facet_wrap("year")

# Distribucion de grados de severidad
dat %>%
  filter(ai %in% c("check","azoxystrobin")) %>% 
  # , year %in% c(2015,2016)) %>% 
  group_by(year, ai, sprays, bk) %>%
  summarise (tot_pods = sum(n_pods), 
             x0_sum = sum(x0),
             x1_sum = sum(x1),
             x2_sum = sum(x2),
             x3_sum = sum(x3),
             x4_sum = sum(x4)) %>%
  mutate(freq_x0 = x0_sum / tot_pods,
         freq_x1 = x1_sum / tot_pods,
         freq_x2 = x2_sum / tot_pods,
         freq_x3 = x3_sum / tot_pods, 
         freq_x4 = x4_sum / tot_pods) %>% 
  gather(freq_x0:freq_x4, 
         key = class,
         value = count) %>% 
  ggplot(aes(x=class,y=count)) +
  facet_grid(ai~year)+
  geom_boxplot()+
  geom_point()+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) 

# dat %>% 
#   ggplot(aes(x=fct_relevel(ai, "check"), y=x34m)) + 
#   labs(x="Fungicide treatment",
#        y="Damaged pods proportion")+
#   geom_tufteboxplot()+
#   facet_grid(. ~ year)+ 
#   coord_flip()+
#   theme_bw()+
#   ggtitle("2 sprays")

# Que proporcion de vainas totalmente sanas
dat %>% 
  # filter(year==2016) %>%  
  # droplevels() %>% 
  group_by(year, ai, sprays, bk) %>% 
  summarise(px0 = mean(x0/n_pods)) %>% 
  ggplot(aes(ai, px0))+
  geom_tufteboxplot()+
  facet_wrap("year")+
  coord_flip()+
  theme_bw()

p <- dat %>% 
  ggplot(aes((x1+x2+x3+x4)/n_pods, (x3+x4)/n_pods))+
  geom_point(alpha=0.2, size=0.5)+
  geom_smooth(method = "lm")+
  geom_abline(intercept = 0, slope = 1, color="red", 
              linetype="dashed")

ggExtra::ggMarginal(p, type="histogram")

#Progreso anual de eficiencias
res2015 %>% 
  left_join(res2016, by="ai") %>% 
  left_join(res2017, by="ai") %>% 
  select(ai, `2015` = prob.x, `2016` = prob.y, `2017` = prob) %>% 
  gather(`2015`:`2017`, key = 'year', value = prop34) %>% 
  ggplot(aes(year, prop34, color=ai, group = ai))+
  geom_point()+
  geom_line()
