load("~/Dropbox/Papers/1 In progress/carbon_mani/data/field_data.RData")

library(lme4)
library(emmeans)
library(car)

# Considerando la interacción
m <- glmer(x3.4/n_pods ~ ai * year + (1|bk/sample),
           weights=n_pods, family="binomial", 
           data=dat, subset = year!=2017)
plot(m)
summary(m)
Anova(m)

em <- emmeans (m, ~ai|year)
res <- CLD(em, Letters = letters, alpha = .05, type = "response")
knitr::kable(res)


# Año como efecto fijo aditivo 
m1 <- glmer(x3.4/n_pods ~ ai + year + (1|bk),
           weights=n_pods, family="binomial",
           data=dat, subset = year!=2017)

em1 <- emmeans (m1, ~ai)
res1 <- CLD(em1, Letters = letters, alpha = .05, type = "response")
knitr::kable(res1)

save(m,m1, res, res1, file = "./data/field_model.rda")

library(cowplot)
source('./scripts/theme_juan.R')
theme_set(theme_juan(9))

# field_2015.tiff ####
res %>% #View 
  filter(year==2015) %>% 
  ggplot(aes(x=fct_reorder(ai, prob, .desc = TRUE), y=prob)) + 
  geom_pointrange(aes(ymin = asymp.LCL, ymax = asymp.UCL), size=.3)+
  geom_text(aes(label = str_remove_all(.group, " "), 
                y = c(rep(0.17, 13))),
            size=2.5, position = position_dodge(0.9), vjust =0.5, hjust="inward")+
  labs(x=NULL, y="Proportion of severely damaged pods")+
  scale_y_continuous(breaks=seq(0.2, 0.5, 0.05))+
  coord_flip()+
  theme(plot.margin = unit(c(0.1,0.1,0,0), "cm"))-> p2015

ggdraw(p2015) + draw_text("2014/15", 
                          x = 0.82, y = 0.92, hjust = 0, vjust = 0,
                          size = 10)  # default font size is 14, 

ggsave(w=85, h=70, units="mm", dpi=300, "./plots/field_2015.tiff")

# field_2016.tiff ####

res %>% #View 
  filter(year==2016) %>% 
  ggplot(aes(x=fct_reorder(ai, prob, .desc = TRUE), y=prob)) + 
  scale_y_continuous(breaks=seq(0.2, 0.5, 0.05))+
  geom_pointrange(aes(ymin = asymp.LCL, ymax = asymp.UCL), size=.3)+
  geom_text(aes(label = str_remove_all(.group, " "), 
                y = c(rep(0.23, 13))),
            size=2.5, position = position_dodge(0.9), vjust =0.5, hjust="inward")+
  labs(x=NULL, y="Proportion of severely damaged pods")+
  coord_flip()+
  theme(plot.margin = unit(c(0.1,0.1,0,0), "cm")) -> p2016

ggdraw(p2016) + draw_text("2015/16", 
                          x = 0.82, y = 0.92, hjust = 0, vjust = 0,
                          size = 10)  # default font size is 14, 

ggsave(w=85, h=70, units="mm", dpi=300, "./plots/field_2016.tiff")

# BIN ########
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
res2015 <- CLD(em2015, Letters = letters, alpha = .05, type = "response")
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
res2016 <- CLD(em2016, by = NULL, Letters = letters, alpha = .05, type = "response")
res2016

#2017
m3 <- glmer(x3.4/n_pods ~ ai + (1|bk/sample),
            weights=n_pods, family="binomial", 
            data=dat, subset = year==2017)

plot(m3)
summary(m3)
Anova(m3)
em2017 <- emmeans (m3, ~ai)
res2017 <- CLD(em2017, Letters = letters, alpha = .05, type = "response")
res2017

