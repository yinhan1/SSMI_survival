
### Last Edited: September 5, 2022

library(tidyverse)
library(magrittr)
library(ggsci)

light = '#f6a5c3'
mid = '#c542ac'
dark = '#3f0967'

# female = '#E69F00'
# male = '#56B4E9'

male = "#0072B2" 
female = "#D55E00"


####  --------------------------    F1    --------------------------  ####
set_breaks <- c("Virgin","Mated","Cohabiting")
set_labels <- c("Virgin","Mated","Cohabiting")
read.csv("data/SSMI bootstrap.csv") %>% 
  mutate(int_colour = paste(Mating_status, Sex) %>% tolower(), tag = paste(Treatment, int_colour)) %>% 
  mutate(Sex = recode(Sex, "female" = "Female", "male" = "Male")) %>%
  ggplot(aes(x = Day, y = est, ymin = lower, ymax = upper, linetype = Treatment, colour = Mating_status)) + 
  geom_line(size = 0.7) + geom_point(size = 0.8) + 
  scale_colour_manual(values = c("Virgin"=light,"Mated"=mid,"Cohabiting"=dark), breaks = set_breaks, labels = set_labels) +
  scale_linetype_manual(values = c('Control'='dotted','Inoculated'='solid')) + 
  geom_errorbar(linetype = "solid", alpha = 0.7, width = 0.7) +
  labs(x = "Days after spray", y = "Survival Probability", colour = "", linetype = "") + thm + ylim(0,1) + facet_wrap(~Sex) + theme_classic() + 
  guides(linetype = guide_legend(order = 1), colour = guide_legend(order = 2))




####  --------------------------    FS1    --------------------------  ####
#### upper panel #### 
set_breaks <- c('Female','Male')
set_labels <- c('Female','Male')
read.csv("data/SSMI bootstrap.csv") %>% 
  mutate(int_colour = paste(Mating_status, Sex) %>% tolower(), tag = paste(Treatment, int_colour)) %>% 
  mutate(Sex = recode(Sex, "female" = "Female", "male" = "Male"),Mating_status = factor(Mating_status, levels = c("Cohabiting","Virgin","Mated"))) %>%
  ggplot(aes(x = Day, y = est, ymin = lower, ymax = upper, linetype = Treatment, colour = Sex)) + 
  geom_line(size = 0.6) + geom_point(size = 0.6) +
  scale_colour_manual(values = c('Female'=female,'Male'=male), breaks = set_breaks, labels = set_labels) +
  scale_linetype_manual(values = c('Control'='dotted','Inoculated'='solid')) + 
  geom_errorbar(linetype = 'solid', alpha = 0.5, width = 0.5, size = 0.5) +
  labs(x = "Days after spray", y = "Survival Probability", colour = "", linetype = "") +thm + ylim(0,1) +facet_wrap(~Mating_status) + theme_classic() + guides(linetype = guide_legend(order = 1), colour = guide_legend(order = 2))

#### lower panel #### 
df = read.csv("data/SSMI raw.csv") %>% mutate(Mating_status = recode(Mating_status, "Cohabit" = "Cohabiting"), Treatment = recode(Treatment, "Fungal" = "Inoculated")) %>% filter(Mating_status != "Virgin CO2") %>% droplevels() %>% mutate(sex = ifelse(Sex=="F", "female", "male"),tag = as.factor(paste(Treatment, tolower(Mating_status), sex))) %>% mutate(Mating_status = factor(Mating_status, levels = c("Cohabiting","Virgin","Mated")))
surv_df = df %>% group_by(Mating_status,Sex,tag,Treatment) %>% mutate(total = length(Death)) %>%group_by(Mating_status,Sex,tag,Treatment,Day,total) %>% summarise(Death = sum(Death)) %>%group_by(Mating_status,Sex,tag, Treatment,total) %>% mutate(Death=cumsum(Death)) %>%mutate(surv_per = 100*(1-Death/total))  %>% mutate(Sex = recode(Sex, "F" = "Female", 'M' = "Male"), int_colour = paste(Mating_status, Sex) %>% tolower())
surv_rep = df %>% group_by(Replicate,Mating_status,Sex,tag,Treatment) %>% mutate(total = length(Death)) %>% group_by(Replicate,Mating_status,Sex,tag,Treatment,Day,total) %>% summarise(Death = sum(Death)) %>% group_by(Replicate,Mating_status,Sex,tag, Treatment,total) %>% mutate(Death=cumsum(Death)) %>% mutate(surv_per = 100*(1-Death/total)) %>% mutate(Sex = recode(Sex, "F" = "Female", "M" = "Male"), int_colour = paste(Mating_status, Sex) %>% tolower())
ggplot(data=NULL, aes(x=Day, y= surv_per, linetype=Treatment, color=Sex)) +
  geom_line(data=surv_df, size = 0.7, alpha = 0.9) + geom_point(data=surv_rep, size = 0.6, alpha = 0.8) +
  scale_colour_manual(values = c('Female'=female,'Male'=male), breaks = set_breaks, labels = set_labels) +
  scale_linetype_manual(values = c('Control'='dotted','Inoculated'='solid')) + 
  labs(linetype="", color="",y = "Survival Percent (%)", color="Mating Status",x = "Days after spray") +ylim(0,100) +theme_classic() +facet_grid(~Mating_status) + guides(linetype = guide_legend(order = 1), color = guide_legend(order = 2))
  




### --------------------------    F2    --------------------------  
reps <- read.csv("data/SSMI larva.csv") %>% mutate(Treatment = recode(Treatment, "Fungal" = "Inoculated"))
model <- lm(larvaPsurv ~ Day + Treatment + Mating_status + Treatment:Mating_status + Day:Treatment, reps)
tb_fit <- reps %>% select(Day, Treatment, Mating_status) %>% unique()
tb_fit <- cbind(tb_fit, predict(model, tb_fit, interval = "confidence")) %>% mutate(tag = as.factor(paste(Treatment, tolower(Mating_status))), Mating_status = recode(Mating_status, "Cohabit" = "Cohabiting"))
set_breaks = c("Mated","Cohabiting")
ggplot(data = tb_fit, aes(x = Day, y = fit, ymin = lwr, ymax = upr, linetype = Treatment, color = Mating_status)) + 
  geom_line(size = 0.9) + geom_point(size = 1) + geom_errorbar(size=0.5, width=0.5,linetype = 'dashed') +
  scale_color_manual(values = c("Mated"=mid,"Cohabiting"=dark), breaks = set_breaks) +
  scale_linetype_manual(values = c("Control" = "dotted","Inoculated" = "solid")) +
  theme_classic() + labs(x = "Days after spray", y = "Offspring count per surviving female", linetype='', color = '')




### --------------------------    F3    --------------------------  
df_plot = read.csv('data/GHA bootstrap.csv')
thm = theme(axis.line = element_line(color="gray54", size=0.1, linetype="solid"),panel.background = element_rect(fill = NA),legend.key = element_rect(fill = NA),legend.background = element_rect(fill = NA),plot.title = element_text(size = 15, hjust = 0.5)) 
ggplot(df_plot, aes(x=day, y=est, color=Sex, linetype=Treatment, ymin=lower, ymax=upper)) +
  geom_line(size = 0.8) + geom_point(size = 0.8) + geom_errorbar(size = 0.7, width = 0.7, alpha = 0.5) +
  scale_linetype_manual(values = c("Control" = "dotted","GHA" = "solid")) + scale_color_manual(values = c("Female" = female, "Male" = male)) +
  labs(x="Days", y="Survival Probability",color='',linetype='') + theme_classic()+ ylim(0,1) + guides(linetype = guide_legend(order = 1), color = guide_legend(order = 2))
  # thm 




### --------------------------    F4    --------------------------  
df = read.csv("data/DIET 2 bootstrap.csv") %>% select(-X) %>% mutate(Diet = factor(Diet, levels=c("C/C","C/G","G/G","G/C"))) %>% select(c(Treatment,Sex,Diet,Sex,day,est,lower,upper))
df_initial = df %>% select(-c(day,est,lower,upper)) %>% unique() %>% mutate(day=0, est=1, lower=1, upper=1)
set_labels = labels %>% mutate(Sex = ifelse(Sex=="M", "Male","Female"),Sex = factor(Sex, levels=c("Male","Female")),tag0 = paste0(Treatment," ",Sex),tag = paste0(Treatment," ",Sex, " (n=",Initial,")")) %>% arrange(Treatment,Sex)
bind_rows(df,df_initial) %>% mutate(tag = factor(paste(Treatment, Sex), levels=c("Control Male","Control Female","Fungal Male","Fungal Female")), Treatment = recode(Treatment, "Fungal" = "Inoculated")) %>% 
  ggplot(aes(x=day, y=est, ymin=lower, ymax=upper, color=Sex, linetype=Treatment)) + 
  geom_errorbar(width=0.5,size=0.7,alpha=0.7) +geom_line(size=0.9) +geom_point(size=1) +scale_x_continuous(breaks=seq(0,12,4)) +
  scale_linetype_manual(values = c("Control" = "dotted","Inoculated" = "solid")) + scale_color_manual(values = c("Female" = female, "Male" = male)) +
  facet_grid(~Diet) +labs(x="Days after spray", y="Survival Probability", color="", linetype="") + theme_classic()




### --------------------------    F5    --------------------------  
df = read.csv("./data/DIET 1 bootstrap.csv") %>% select(-X) %>% mutate(Diet = factor(Diet, levels=c("C/C","C/CY","CY/C","CY/CY","G/G")))
bind_rows(df) %>% group_by(Diet) %>% 
    mutate(tag = factor(paste(Treatment, Sex), levels=c("Control Male","Control Female","Fungal Male","Fungal Female")), Treatment = recode(Treatment, "Fungal" = "Inoculated")) %>% 
  ggplot(aes(x=day, y=est, ymin=lower, ymax=upper, color=Sex, linetype=Treatment)) + 
  geom_errorbar(width=0.5,size=0.5,alpha=0.5,linetype='solid') + geom_line(size=0.6, aes(linetype=Treatment)) + geom_point(size=0.6) +scale_x_continuous(breaks=seq(0,12,4)) +
  scale_linetype_manual(values = c("Control" = "dotted","Inoculated" = "solid")) + scale_color_manual(values = c("Female" = female, "Male" = male)) +
  facet_grid(~Diet) +labs(x="Days after spray", y="Survival Probability", color="", linetype="") + theme_classic()
    



### --------------------------    F6    --------------------------  
df = read.csv("./data/DIET 3 bootstrap.csv") %>% select(-X) %>% mutate(Diet = factor(Diet, levels=c("C","CY0.5","CY1.0","CY1.5")),Sex = ifelse(Sex=="M", "Male","Female"))
bind_rows(df) %>% group_by(Diet) %>% mutate(tag = factor(paste(Treatment, Sex), levels=c("Control Male","Control Female","Fungal Male","Fungal Female")), Treatment = recode(Treatment, "Fungal" = "Inoculated")) %>% 
  ggplot(aes(x=day, y=est, ymin=lower, ymax=upper, color=Sex, linetype=Treatment)) +
  geom_errorbar(width=0.5,size=0.5,alpha=0.5,linetype='solid') + geom_line(aes(linetype=Treatment), size=0.7) + geom_point(size=0.7) + scale_x_continuous(breaks=seq(0,12,4)) +
  scale_linetype_manual(values = c("Control" = "dotted","Inoculated" = "solid")) + scale_color_manual(values = c("Female" = female, "Male" = male)) +
  facet_grid(~Diet) +labs(x="Days after spray", y="Survival Probability", color="", linetype="") + theme_classic()

