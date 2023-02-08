library(tidyverse)
# library(forcats)

de50 <- de50CCC %>% 
  mutate_at(vars("loc"), funs(factor)) %>% 
  mutate(ec50 = as.character(round(ec50,3)),
         ec50 = recode(ec50, "100" = ">100"))

dat <- dataCCC%>% 
  
  mutate_at(vars("loc"), funs(factor)) %>% 
  gather(-(loc:plate), key = dose, value = inhib) %>% 
  left_join(de50, by="loc") %>% 
  mutate_at(vars(c("loc", "plate")),funs(factor)) %>% 
  mutate(inhib = replace(inhib, which(inhib<0), 0)) %>% 
  mutate_at(vars("dose"), funs(as.numeric)) 

# levels(dat$chem_group)
# str(dat)

dat %>%
  group_by(loc,plate)%>%
  summarise(n=n())%>%
  knitr::kable()

# dat1 <- dat %>% filter(!(year==2015 & ai == "Azoxystrobin" & pot==3))
#save(dat, de50, file="~/Dropbox/Papers/1 In progress/carbon_mani/data/invitro_data.RData")

# ec50.tiff #####

source('./scripts/theme_juan.R')
theme_set(theme_juan(9))
library(scales) # to access break formatting functions

dat %>% 
  ggplot(aes(dose, inhib, group=loc)) +
  geom_point(alpha=0.2)+
  facet_wrap("loc")+
  stat_summary(fun.y=mean, colour="black", geom="line", aes(group = 1))+
  geom_hline(yintercept=50, linetype="dashed", color = "grey")+
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  labs(y="Micelial growth inhibition (%)", 
       x=expression(paste("Fungicide concentration (",
    mu, g, " a.i./", ml,")", sep="")))+ # "Fungicide dose (mg L-1)")+
  geom_text(data = subset(de50, ai!="Epoxiconazole"),
            aes(label=ec50, x=30, y=55),
            size=2.5, position = position_dodge(0.9), vjust =0.5)+
  theme(panel.border = element_blank())

#ggsave(w=160, h=140, units="mm", dpi=150, "./plots/ec50.tiff")
