library(tidyverse)
library(emmeans)
library(car)
library(lme4)

# conjunto
m <- glm(x3.4/n_pods ~ ai * year, 
         weights=n_pods, family="binomial", 
         data=dat)

m1 <- glmer(x3.4/n_pods ~ ai + (1|year), 
         weights=n_pods, family="binomial", 
         data=dat)

hnp::hnp(m)
plot(m)

summary(m)
Anova(m)

# ai por año
em <- emmeans (m, ~ai|year)
res <- CLD(em, Letters = letters, alpha = .05, type = "response")
res %>% knitr::kable()
save(m, res, file = "../data/pot_model.rda")
load(file = "../data/pot_model.rda")

em1 <- emmeans (m1, ~ai)
res1 <- CLD(em1, Letters = letters, alpha = .05, type = "response")
res1 %>% knitr::kable()

library(cowplot)
source('../scripts/theme_juan.R')
theme_set(theme_juan(9))

# pot_2015.tiff ####

res %>% 
  filter(year==2015) %>% 
  ggplot(aes(x=fct_reorder(ai, prob, .desc = TRUE), y=prob)) + 
  labs(x=NULL,
       y="Proportion of severely damaged pods")+
  geom_pointrange(aes(ymin = asymp.LCL, ymax = asymp.UCL), size=.3)+
  geom_text(aes(label = str_remove_all(.group, " "), 
                y = c(rep(0.17, 13))),
            size=2.5, position = position_dodge(0.9), vjust =0.5, hjust="inward")+
  scale_y_continuous(breaks=seq(0.2, 0.8, 0.1))+
  coord_flip()+
  theme(plot.margin = unit(c(0.1,0.1,0,0), "cm"))-> p2015

ggdraw(p2015) + draw_text("2014/15", 
                          x = 0.82, y = 0.92, hjust = 0, vjust = 0,
                          size = 10)  # default font size is 14, 

ggsave(w=85, h=70, units="mm", dpi=300, "./plots/pot_2015.tiff")

# pot_2016.tiff ####

res %>% #View 
  filter(year==2016) %>% 
  ggplot(aes(x=fct_reorder(ai, prob, .desc = TRUE), y=prob)) + 
  labs(x=NULL,
       y="Proportion of severely damaged pods")+
  geom_pointrange(aes(ymin = asymp.LCL, ymax = asymp.UCL), size=.3)+
  geom_text(aes(label = str_remove_all(.group, " "), 
                y = c(rep(-0.003, 13))),
            size=2.5, position = position_dodge(0.9), vjust =0.5, hjust="inward")+
  scale_y_continuous(breaks=seq(0, 0.8, 0.1))+
  coord_flip()+
  theme(plot.margin = unit(c(0.1,0.1,0,0), "cm"))-> p2016

ggdraw(p2016) + draw_text("2015/16", 
                          x = 0.82, y = 0.92, hjust = 0, vjust = 0,
                          size = 10)  # default font size is 14, 

ggsave(w=85, h=70, units="mm", dpi=300, "./plots/pot_2016.tiff")

# pot_2016.tiff
res %>% #View 
  filter(year==2016) %>% 
  ggplot(aes(x=fct_reorder(ai, prob, .desc = TRUE), y=prob)) + 
  labs(x="Fungicide treatment",
       y="Proportion of severely damaged pods")+
  geom_pointrange(aes(ymin = asymp.LCL, ymax = asymp.UCL), size=.3)+
  geom_text(aes(label = str_remove_all(.group, " "), 
                y = c(rep(0.23, 14))),
            size=2.5, position = position_dodge(0.9), vjust = 0.5)+
  # facet_wrap("year")+ 
  coord_flip()+
  theme_bw()

#2015
m0 <- glmer(x3.4/n_pods ~ ai + (1|bk/sample),
      weights=n_pods, family="binomial", 
      data=dat, subset = year==2015)

plot(m0)
summary(m0)
Anova(m0)
em2015 <- emmeans (m0, ~ai)
res2015 <- CLD(em2015, Letters = letters, alpha = .05, type = "prob")
res2015

d15 <- dat_ai %>% filter(year==2015)  

d15 %>% 
  ggplot(aes(ai,x3.4/n_pods))+
  # geom_point(alpha=0.5, aes(size=n_pods/10)) +
  geom_violin()+
  coord_flip() -> g15

g15 + geom_point(data=res0, aes(x=ai, y=prob), colour="red")

#2016
m2 <- glmer(x3.4/n_pods ~ ai + (1|bk/sample),
      weights=n_pods, family="binomial", 
      data=dat, subset = year==2016)

plot(m2)
summary(m2)
Anova(m2)
em2016 <- emmeans (m2, ~ai, type)
res2016 <- CLD(em2016, by = NULL, Letters = letters, alpha = .05, type = "prob")
res2016

#2017
m3 <- glmer(x3.4/n_pods ~ ai + (1|bk/sample),
            weights=n_pods, family="binomial", 
            data=dat, subset = year==2017)

plot(m3)
summary(m3)
Anova(m3)
em2017 <- emmeans (m3, ~ai)
res2017 <- CLD(em2017, Letters = letters, alpha = .05, type = "prob")
res2017

