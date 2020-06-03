
### Last Edited: June 5, 2020

library(tidyverse)
library(magrittr)
library(ggsci)

source("Figure R code/R functions.R")

####  --------------------------    F1    --------------------------  #####

set_breaks <- 
  c("Control virgin female","Control mated female","Control cohabiting female",
    "Inocaluated virgin female","Inocaluated mated female","Inocaluated cohabiting female",
    "Control virgin male","Control mated male","Control cohabiting male",
    "Inocaluated virgin male","Inocaluated mated male","Inocaluated cohabiting male")

set_labels <- 
  c("Control virgin female (n=265)",
    "Control mated female (n=270)",
    "Control cohabiting female (n=280)",
    "Inocaluated virgin female (n=280)",
    "Inocaluated mated female (n=276)",
    "Inocaluated cohabiting female (n=271)",
    "Control virgin male (n=257)",
    "Control mated male (n=277)",
    "Control cohabiting male (n=277)",
    "Inocaluated virgin male (n=272)",
    "Inocaluated mated male (n=265)",
    "Inocaluated cohabiting male (n=294)")

read.csv("data/SSMI bootstrap.csv") %>% 
  mutate(int_colour = paste(Mating_status, Sex) %>% tolower(),
         tag = paste(Treatment, int_colour)) %>% 
  mutate(Sex = recode(Sex, "female" = "Female", "male" = "Male")) %>% 
  ggplot(aes(x = Day, y = est, ymin = lower, ymax = upper,
             linetype = tag, 
             colour = tag)) + 
  geom_line(size = 0.9) + 
  geom_point(size = 1) +
  scale_colour_manual(
    values = c(
      "Control virgin female" = "#32CD32",
      "Control mated female" = "#64B39B",
      "Control cohabiting female" = "#006400",
      "Inocaluated virgin female" = "#32CD32",
      "Inocaluated mated female" = "#64B39B",
      "Inocaluated cohabiting female" = "#006400",
      "Control virgin male" = "#FA8072",
      "Control mated male" = "#EE2C2C",
      "Control cohabiting male" = "#BA251F",
      "Inocaluated virgin male" = "#FA8072",
      "Inocaluated mated male" = "#EE2C2C",
      "Inocaluated cohabiting male" = "#BA251F"
      ),
    breaks = set_legend_breaks, labels = set_legend_labels
    ) + 
  scale_linetype_manual(
    values = c(
      "Control virgin female" = "dashed",
      "Control mated female" = "dashed",
      "Control cohabiting female" = "dashed",
      "Inocaluated virgin female" = "solid",
      "Inocaluated mated female" = "solid",
      "Inocaluated cohabiting female" = "solid",
      "Control virgin male" = "dashed",
      "Control mated male" = "dashed",
      "Control cohabiting male" = "dashed",
      "Inocaluated virgin male" = "solid",
      "Inocaluated mated male" = "solid",
      "Inocaluated cohabiting male" = "solid"
    ),
    breaks = set_legend_breaks, labels = set_legend_labels
  ) +
  geom_errorbar(linetype = 'solid') +
  labs(x = "Days after spray", y = "Survival Probability", 
       colour = "", linetype = "") +
  thm + ylim(0,1) +
  facet_wrap(~Sex) +
  theme_classic()


####  --------------------------    FS1    --------------------------  #####

#### upper panel #### 

read.csv("data/SSMI bootstrap.csv") %>% 
  mutate(int_colour = paste(Mating_status, Sex) %>% tolower(),
         tag = paste(Treatment, int_colour)) %>% 
  mutate(Sex = recode(Sex, "female" = "Female", "male" = "Male"),
         Mating_status = factor(Mating_status, 
                                levels = c("Cohabiting","Virgin","Mated"))) %>% 
  ggplot(aes(x = Day, y = est, ymin = lower, ymax = upper,
             linetype = tag, 
             colour = tag)) + 
  geom_line(size = 0.5) + 
  geom_point(size = 0.6) +
  scale_colour_manual(
    values = c(
      "Control virgin female" = "#32CD32",
      "Control mated female" = "#64B39B",
      "Control cohabiting female" = "#006400",
      "Inocaluated virgin female" = "#32CD32",
      "Inocaluated mated female" = "#64B39B",
      "Inocaluated cohabiting female" = "#006400",
      "Control virgin male" = "#FA8072",
      "Control mated male" = "#EE2C2C",
      "Control cohabiting male" = "#BA251F",
      "Inocaluated virgin male" = "#FA8072",
      "Inocaluated mated male" = "#EE2C2C",
      "Inocaluated cohabiting male" = "#BA251F"
    ),
    breaks = set_legend_breaks, labels = set_legend_labels
  ) + 
  scale_linetype_manual(
    values = c(
      "Control virgin female" = "dashed",
      "Control mated female" = "dashed",
      "Control cohabiting female" = "dashed",
      "Inocaluated virgin female" = "solid",
      "Inocaluated mated female" = "solid",
      "Inocaluated cohabiting female" = "solid",
      "Control virgin male" = "dashed",
      "Control mated male" = "dashed",
      "Control cohabiting male" = "dashed",
      "Inocaluated virgin male" = "solid",
      "Inocaluated mated male" = "solid",
      "Inocaluated cohabiting male" = "solid"
    ),
    breaks = set_legend_breaks, labels = set_legend_labels
  ) +
  geom_errorbar(linetype = 'solid', alpha = 0.8, width = 0.6, size = 0.5) +
  labs(x = "Days after spray", y = "Survival Probability", 
       colour = "", linetype = "") +
  thm + ylim(0,1) +
  facet_wrap(~Mating_status) +
  theme_classic()


#### lower panel #### 

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

levels(surv_df$Mating_status) = c("Cohabiting","Virgin","Mated","Virgin CO2") 
levels(surv_rep$Mating_status) = c("Cohabiting","Virgin","Mated", "Virgin CO2") 

ggplot(data=NULL,
       aes(x=Day, y=surv_per, linetype=Treatment, color=tag)) +
  geom_line(data=surv_df, size = 1, alpha = 0.5) +
  geom_point(data=surv_rep, size = 0.6) +
  scale_color_manual(
    values=c("Virgin female"="#32CD32", "Mated female"="#64B39B",
             "Cohabiting female"="#006400", "Virgin male"="#FA8072",
             "Mated male"="#EE2C2C","Cohabiting male"="#BA251F"),
    breaks=c("Virgin male","Mated male","Cohabiting male",
             "Virgin female","Mated female","Cohabiting female")) +
  scale_linetype_manual(values = c("dashed", "solid"), 
                        labels = c("Control", "Fungal")) +
  labs(linetype="", color="",
       y = "Survival Percent (%)", color="Mating Status",
       x = "Days after spray") +
  ylim(0,100) +
  theme_classic() +
  facet_grid(~Mating_status)


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









