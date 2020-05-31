
### Last Edited: June 5, 2020

library(tidyverse)
library(magrittr)
library(ggsci)

source("Figure R code/R functions.R")

### --------------------------    F1    --------------------------  

set_label = c("Virgin female (n=425)","Mated female (n=426)","Cohabiting female (n=435)",
              "Virgin male (n=411)","Mated male (n=427)","Cohabiting male (n=453)")
set_order = c("Virgin female","Mated female","Cohabiting female",
              "Virgin male","Mated male","Cohabiting male")

read.csv("data/SSMI bootstrap.csv") %>% 
  mutate(int_colour = paste(Mating_status, Sex)) %>% 
  mutate(Mating_status = recode(Mating_status, 
                                "Virgin"="(B) Virgin",
                                "Cohabiting"="(A) Cohabiting",
                                "Mated"="(C) Mated"),
         Mating_status = factor(Mating_status, 
                                levels=c("(A) Cohabiting",
                                         "(B) Virgin",
                                         "(C) Mated"))) %>% 
  ggplot(aes(x=Day, y=est, ymin=lower, ymax=upper,
             colour = int_colour)) + 
  geom_line(aes(linetype = Treatment), size = 0.9) + 
  geom_point(size = 1) +
  geom_errorbar(size = 0.7, width = 0.7, alpha = 0.5) +
  scale_colour_manual(name="", breaks=set_order, labels=set_label,
                      values=c("#32CD32","#64B39B","#006400",
                               "#FA8072","#EE2C2C","#BA251F")) +
  scale_linetype_manual(name="",values=c("dashed", "solid"),
                        labels=c("Control (n=1273)", "Inoculated (n=1304)")) +
  labs(x="Days after spray", y="Survival Probability", 
       title="") +
  thm + ylim(0,1) + g +
  # facet_wrap(~Mating_status) +
  facet_wrap(~Sex) +
  theme_bw()



### --------------------------    F2    --------------------------  

### --------------------------    F3    --------------------------  

### --------------------------    F4    --------------------------  

df = 
  read.csv("./data/DIET 2 bootstrap.csv") %>% 
  select(-X) %>% 
  mutate(Diet = factor(Diet, levels=c("C/C","C/G","G/G","G/C"))) %>% 
  select(c(Treatment,Sex,Diet,Sex,day,est,lower,upper))

df_initial = 
  df %>% 
  select(-c(day,est,lower,upper)) %>% 
  unique() %>% 
  mutate(day=0, est=1, lower=1, upper=1)

labels = read.csv("./data/DIET 2 raw.csv") %>% 
  mutate(Diet = factor(Diet, levels=c("C/C","C/G","G/G","G/C"))) %>% 
  filter(Day==1) %>% 
  group_by(Diet,Treatment,Sex) %>% 
  summarise(Initial=sum(Initial.density))

plot_fun4 = function(d){
  set_label = 
    labels %>% 
    filter(Diet==d) %>% 
    mutate(Sex = ifelse(Sex=="M", "Male","Female"),
           Sex = factor(Sex, levels=c("Male","Female")),
           tag = paste0(Treatment," ",Sex, " (n=",Initial,")")) %>% 
    arrange(Treatment,Sex)
  
  bind_rows(df,df_initial) %>% 
    filter(Diet == d) %>% 
    mutate(tag = factor(paste(Treatment, Sex), 
                        levels=c("Control Male","Control Female",
                                 "Fungal Male","Fungal Female"))) %>% 
    ggplot(aes(x=day, y=est, ymin=lower, ymax=upper, color=tag, linetype=tag,
               group=interaction(Diet,tag))) +
    geom_errorbar(width=0.5,size=0.5,alpha=0.7) +
    geom_line(size=1) +
    geom_point(size=1) +
    scale_x_continuous(breaks=seq(0,12,4)) +
    scale_linetype_manual(values=c("Control Female"="dotted",
                                   "Control Male"="dotted",
                                   "Fungal Female"="solid",
                                   "Fungal Male"="solid"),
                          labels=set_label$tag) +
    scale_color_manual(values=c("Control Female"="darkgreen",
                                "Control Male"="brown",
                                "Fungal Female"="darkgreen",
                                "Fungal Male"="brown"),
                       labels=set_label$tag) +
    facet_grid(~Diet) +
    labs(x="Days after spray", y="Survival Probability", color="", linetype="") +
    theme_classic()
}

plot_fun4("C/C")
plot_fun4("C/G")
plot_fun4("G/G")
plot_fun4("G/C")


### --------------------------    F5    --------------------------  
df = 
  read.csv("./data/DIET 1 bootstrap.csv") %>% 
  select(-X) %>% 
  mutate(Diet = factor(Diet, levels=c("C/C","C/CY","CY/C","CY/CY","G/G")))

labels = read.csv("./data/DIET 1 raw.csv") %>% 
  mutate(Diet = factor(Diet, levels=c("C/C","C/CY","CY/C","CY/CY","G/G"))) %>% 
  filter(Day==1) %>% 
  group_by(Diet,Treatment,Sex) %>% 
  summarise(Initial=sum(Initial.density))

plot_fun5 = function(d){
  set_label = 
    labels %>% 
    filter(Diet==d) %>% 
    mutate(Sex = ifelse(Sex=="M", "Male","Female"),
           Sex = factor(Sex, levels=c("Male","Female")),
           tag = paste0(Treatment," ",Sex, " (n=",Initial,")")) %>% 
    arrange(Treatment,Sex)
  
  bind_rows(df,df_initial) %>% 
    filter(Diet == d) %>% 
    mutate(tag = factor(paste(Treatment, Sex), 
                        levels=c("Control Male","Control Female",
                                 "Fungal Male","Fungal Female"))) %>% 
    ggplot(aes(x=day, y=est, ymin=lower, ymax=upper, color=tag, linetype=tag,
               group=interaction(Diet,tag))) +
    geom_errorbar(width=0.5,size=0.5,alpha=0.7) +
    geom_line(size=1) +
    geom_point(size=1) +
    scale_x_continuous(breaks=seq(0,12,4)) +
    scale_linetype_manual(values=c("Control Female"="dotted",
                                   "Control Male"="dotted",
                                   "Fungal Female"="solid",
                                   "Fungal Male"="solid"),
                          labels=set_label$tag) +
    scale_color_manual(values=c("Control Female"="darkgreen",
                                "Control Male"="brown",
                                "Fungal Female"="darkgreen",
                                "Fungal Male"="brown"),
                       labels=set_label$tag) +
    facet_grid(~Diet) +
    labs(x="Days after spray", y="Survival Probability", color="", linetype="") +
    theme_classic()
}

plot_fun5("C/C")
plot_fun5("C/CY")
plot_fun5("CY/C")
plot_fun5("CY/CY")
plot_fun5("G/G")


### --------------------------    F6    --------------------------  
df = 
  read.csv("./data/DIET 3 bootstrap.csv") %>% 
  select(-X) %>% 
  mutate(Diet = factor(Diet, levels=c("C","CY0.5","CY1.0","CY1.5")),
         Sex = ifelse(Sex=="M", "Male","Female"))

labels = read.csv("./data/DIET 3 raw.csv") %>% 
  mutate(Diet=recode(Diet,"Cornmeal"="C","C+0.5Y"="CY0.5",
                     "C+1.0Y"="CY1.0","C+1.5Y"="CY1.5"),
         Diet = factor(Diet, levels=c("C","CY0.5","CY1.0","CY1.5"))) %>% 
  filter(Day==1) %>% 
  group_by(Diet,Treatment,Sex) %>% 
  summarise(Initial=sum(Initial.Density))

plot_fun6 = function(d){
  set_label = 
    labels %>% 
    filter(Diet==d) %>% 
    mutate(Sex = ifelse(Sex=="M", "Male","Female"),
           Sex = factor(Sex, levels=c("Male","Female")),
           tag = paste0(Treatment," ",Sex, " (n=",Initial,")")) %>% 
    arrange(Treatment,Sex)
  
  bind_rows(df,df_initial) %>% 
    filter(Diet == d) %>% 
    mutate(tag = factor(paste(Treatment, Sex), 
                        levels=c("Control Male","Control Female",
                                 "Fungal Male","Fungal Female"))) %>% 
    ggplot(aes(x=day, y=est, ymin=lower, ymax=upper, color=tag, 
               group=interaction(Diet,tag))) +
    geom_errorbar(width=0.5,size=0.5,alpha=0.7) +
    geom_line(aes(linetype=tag), size=1) +
    geom_point(size=1) +
    scale_x_continuous(breaks=seq(0,12,4)) +
    scale_linetype_manual(values=c("Control Female"="dotted",
                                   "Control Male"="dotted",
                                   "Fungal Female"="solid",
                                   "Fungal Male"="solid"),
                          labels=set_label$tag) +
    scale_color_manual(values=c("Control Female"="darkgreen",
                                "Control Male"="brown",
                                "Fungal Female"="darkgreen",
                                "Fungal Male"="brown"),
                       labels=set_label$tag) +
    facet_grid(~Diet) +
    labs(x="Days after spray", y="Survival Probability", color="", linetype="") +
    theme_classic()
}

plot_fun6("C")
plot_fun6("CY0.5")
plot_fun6("CY1.0")
plot_fun6("CY1.5")





### --------------------------    FS1    --------------------------  

df = read.csv("./data/SSMI raw.csv") %>% 
  mutate(Mating_status = recode(Mating_status, "Cohabit"="Cohabiting"))
surv_df =
  df %>% 
  filter(Mating_status != "Virgin CO2") %>%
  mutate(sex = ifelse(Sex=="F", "female", "male"),
         tag = paste(Mating_status, sex)) %>%
  group_by(Mating_status,Sex,tag,Treatment) %>% mutate(total = length(Death)) %>%
  group_by(Mating_status,Sex,tag,Treatment,Day,total) %>% summarise(Death = sum(Death)) %>%
  group_by(Mating_status,Sex,tag, Treatment,total) %>% mutate(Death=cumsum(Death)) %>%
  mutate(surv_per = 100*(1-Death/total))

surv_rep = 
  df %>% 
  filter(Mating_status != "Virgin CO2") %>%
  mutate(sex = ifelse(Sex=="F", "female", "male"),
         tag = paste(Mating_status, sex)) %>%
  group_by(Replicate,Mating_status,Sex,tag,Treatment) %>%
  mutate(total = length(Death)) %>%
  group_by(Replicate,Mating_status,Sex,tag,Treatment,Day,total) %>% 
  summarise(Death = sum(Death)) %>%
  group_by(Replicate,Mating_status,Sex,tag, Treatment,total) %>% 
  mutate(Death=cumsum(Death)) %>%
  mutate(surv_per = 100*(1-Death/total))
  
ggplot(data=NULL,
       aes(x=Day, y=surv_per, linetype=Treatment, color=tag)) +
  geom_line(data=surv_df, size = 0.9) +
  geom_point(data=surv_rep, alpha=0.5, size=0.5) +
  scale_color_manual(
    values=c("Virgin female"="#32CD32", "Mated female"="#64B39B",
             "Cohabiting female"="#006400", "Virgin male"="#FA8072",
             "Mated male"="#EE2C2C","Cohabiting male"="#BA251F"),
    breaks=c("Virgin male","Mated male","Cohabiting male",
             "Virgin female","Mated female","Cohabiting female")) +
  scale_linetype_manual(values = c("dashed", "solid"), labels = c("Control", "Fungal")) +
  labs(linetype="", color="",
       y = "Survival Percent (%)", color="Mating Status",
       x = "Days after spray") +
  ylim(0,100) +
  theme_classic() +
  facet_grid(~Mating_status)

### --------------------------    FS2 (done)    --------------------------  


### --------------------------    FS3    --------------------------  
# GHA


### --------------------------    FS4    --------------------------  
df = 
  read.csv("./data/DIET 2 raw.csv") %>% 
  mutate(Diet = factor(Diet, levels=c("C/C","C/G","G/G","G/C")),
         Sex = ifelse(Sex=="M","Male","Female"))

surv_rep = 
  df %>% 
  group_by(Replicate,Diet,Treatment,Sex,Day) %>% 
  summarize(Death=sum(Death),
            Initial=sum(Initial.density)) %>% 
  ungroup() %>% 
  group_by(Replicate,Diet,Treatment,Sex) %>% 
  mutate(Death=cumsum(Death)) %>% 
  ungroup() %>% 
  mutate(survs_per=1-Death/Initial)

surv_df =
  df %>% 
  group_by(Diet,Treatment,Sex,Day) %>% 
  summarize(Death=sum(Death),
            Initial=sum(Initial.density)) %>% 
  ungroup() %>% 
  group_by(Diet,Treatment,Sex) %>% 
  mutate(Death=cumsum(Death)) %>% 
  ungroup() %>% 
  mutate(survs_per=1-Death/Initial)

plot_fun4s = function(d){
  surv_df_i = surv_df %>% filter(Diet==d)
  surv_rep_i = surv_rep %>% filter(Diet==d)
  
  ggplot(data=NULL, aes(x=Day, y=100*survs_per,color=Sex)) + 
    geom_line(data=surv_df_i, aes(linetype=Treatment), size=0.9) +
    geom_point(data=surv_rep_i, size=0.5, alpha=0.6) +
    scale_color_manual(values=c("Male"="brown","Female"="darkgreen")) +
    scale_linetype_manual(values=c("Control"="dotted","Fungal"="solid")) +
    scale_x_continuous(breaks=seq(0,12,4)) +
    facet_grid(~Diet) +
    labs(x="Days after spray", y="Survival Percent (%)",
         color="",linetype="") +
    theme_classic()
}

plot_fun4s("C/C")
plot_fun4s("C/G")
plot_fun4s("G/G")
plot_fun4s("G/C")


  
### --------------------------    FS5    --------------------------  
df =  
  read.csv("./data/DIET 1 raw.csv") %>% 
  select(-X) %>% 
  mutate(Diet = factor(Diet, levels=c("C/C","C/CY","CY/C","CY/CY","G/G")),
         Sex = ifelse(Sex=="M","Male","Female"))

surv_rep = 
  df %>% 
  group_by(Replicate,Diet,Treatment,Sex,Day) %>% 
  summarize(Death=sum(Death),
            Initial=sum(Initial.density)) %>% 
  ungroup() %>% 
  group_by(Replicate,Diet,Treatment,Sex) %>% 
  mutate(Death=cumsum(Death)) %>% 
  ungroup() %>% 
  mutate(survs_per=1-Death/Initial)

surv_df =
  df %>% 
  group_by(Diet,Treatment,Sex,Day) %>% 
  summarize(Death=sum(Death),
            Initial=sum(Initial.density)) %>% 
  ungroup() %>% 
  group_by(Diet,Treatment,Sex) %>% 
  mutate(Death=cumsum(Death)) %>% 
  ungroup() %>% 
  mutate(survs_per=1-Death/Initial)

plot_fun5s = function(d){
  surv_df_i = surv_df %>% filter(Diet==d)
  surv_rep_i = surv_rep %>% filter(Diet==d)
  
  ggplot(data=NULL, aes(x=Day, y=100*survs_per,color=Sex)) + 
    geom_line(data=surv_df_i, aes(linetype=Treatment), size=0.9) +
    geom_point(data=surv_df_i,size=1.1) +
    geom_point(data=surv_rep_i, size=0.5, alpha=0.6) +
    scale_color_manual(values=c("Male"="brown","Female"="darkgreen")) +
    scale_linetype_manual(values=c("Control"="dashed","Fungal"="solid")) +
    scale_x_continuous(breaks=seq(0,12,4)) +
    facet_grid(~Diet) +
    labs(x="Days after spray", y="Survival Percent (%)",
         color="",linetype="") +
    theme_classic()
}

plot_fun5s("C/C")
plot_fun5s("C/CY")
plot_fun5s("CY/C")
plot_fun5s("CY/CY")
plot_fun5s("G/G")



### --------------------------    F6    --------------------------  
df = 
  read.csv("./data/DIET 3 raw.csv") %>% 
  select(-X) %>% 
  mutate(Diet = recode(Diet,
                       "C+0.5Y"="CY0.5",
                       "C+1.0Y"="CY1.0",
                       "C+1.5Y"="CY1.5",
                       "Cornmeal"="C"),
         Diet = factor(Diet, levels=c("C","CY0.5","CY1.0","CY1.5")),
         Sex = ifelse(Sex=="M","Male","Female"))
surv_df =
  df %>% 
  group_by(Diet,Treatment,Sex,Day) %>% 
  summarize(Death=sum(Death),
            Initial=sum(Initial.Density)) %>% 
  ungroup() %>% 
  group_by(Diet,Treatment,Sex) %>% 
  mutate(Death=cumsum(Death)) %>% 
  ungroup() %>% 
  mutate(survs_per=1-Death/Initial)

surv_rep = 
  df %>% 
  group_by(Replicate,Diet,Treatment,Sex,Day) %>% 
  summarize(Death=sum(Death),
            Initial=sum(Initial.Density)) %>% 
  ungroup() %>% 
  group_by(Replicate,Diet,Treatment,Sex) %>% 
  mutate(Death=cumsum(Death)) %>% 
  ungroup() %>% 
  mutate(survs_per=1-Death/Initial)

plot_fun6s = function(d){
  surv_df_i = surv_df %>% filter(Diet==d)
  surv_rep_i = surv_rep %>% filter(Diet==d)
  
  ggplot(data=NULL, aes(x=Day, y=100*survs_per,color=Sex)) + 
    geom_line(data=surv_df_i, aes(linetype=Treatment), size=0.9) +
    geom_point(data=surv_df_i,size=1.1) +
    geom_point(data=surv_rep_i, size=0.5, alpha=0.6) +
    scale_color_manual(values=c("Male"="brown","Female"="darkgreen")) +
    scale_linetype_manual(values=c("Control"="dashed","Fungal"="solid")) +
    scale_x_continuous(breaks=seq(0,12,4)) +
    facet_grid(~Diet) +
    labs(x="Days after spray", y="Survival Percent (%)",
         color="",linetype="") +
    theme_classic()
}

plot_fun6s("C")
plot_fun6s("CY0.5")
plot_fun6s("CY1.0")
plot_fun6s("CY1.5")









