
### Last Edited: June 5, 2020

library(tidyverse)
library(magrittr)
library(ggsci)

####  --------------------------    F1    --------------------------  ####

set_breaks <- 
  c("Control virgin female","Control mated female","Control cohabiting female",
    "Inoculated virgin female","Inoculated mated female","Inoculated cohabiting female",
    "Control virgin male","Control mated male","Control cohabiting male",
    "Inoculated virgin male","Inoculated mated male","Inoculated cohabiting male")

set_labels <- 
  c("Control virgin female (n=265)",
    "Control mated female (n=270)",
    "Control cohabiting female (n=280)",
    "Inoculated virgin female (n=280)",
    "Inoculated mated female (n=276)",
    "Inoculated cohabiting female (n=271)",
    "Control virgin male (n=257)",
    "Control mated male (n=277)",
    "Control cohabiting male (n=277)",
    "Inoculated virgin male (n=272)",
    "Inoculated mated male (n=265)",
    "Inoculated cohabiting male (n=294)")

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
      "Control virgin female" = "#FF83FA",
      "Control mated female" = "#AB82FF",
      "Control cohabiting female" = "#5D478B",
      "Inoculated virgin female" = "#FF83FA",
      "Inoculated mated female" = "#AB82FF",
      "Inoculated cohabiting female" = "#5D478B",
      "Control virgin male" = "#FFA500",
      "Control mated male" = "#CD8500",
      "Control cohabiting male" = "#8B5A00",
      "Inoculated virgin male" = "#FFA500",
      "Inoculated mated male" = "#CD8500",
      "Inoculated cohabiting male" = "#8B5A00"
      ),
    breaks = set_breaks, labels = set_labels
    ) + 
  scale_linetype_manual(
    values = c(
      "Control virgin female" = "dashed",
      "Control mated female" = "dashed",
      "Control cohabiting female" = "dashed",
      "Inoculated virgin female" = "solid",
      "Inoculated mated female" = "solid",
      "Inoculated cohabiting female" = "solid",
      "Control virgin male" = "dashed",
      "Control mated male" = "dashed",
      "Control cohabiting male" = "dashed",
      "Inoculated virgin male" = "solid",
      "Inoculated mated male" = "solid",
      "Inoculated cohabiting male" = "solid"
    ),
    breaks = set_breaks, labels = set_labels
  ) +
  # geom_errorbar(linetype = "solid", alpha = 0.5) +
  labs(x = "Days after spray", y = "Survival Probability", 
       colour = "", linetype = "") +
  thm + ylim(0,1) +
  facet_wrap(~Sex) +
  theme_classic()


####  --------------------------    FS1    --------------------------  ####

#### upper panel #### 

set_breaks =
  c("Control cohabiting female","Inoculated cohabiting female",
    "Control cohabiting male","Inoculated cohabiting male",
    "Control virgin female","Inoculated virgin female",
    "Control virgin male","Inoculated virgin male",
    "Control mated female","Inoculated mated female",
    "Control mated male","Inoculated mated male")

set_labels <- 
  c("Control cohabiting female (n=280)",
    "Inoculated cohabiting female (n=271)",
    "Control cohabiting male (n=277)",
    "Inoculated cohabiting male (n=294)",
    
    "Control virgin female (n=265)",
    "Inoculated virgin female (n=280)",
    "Control virgin male (n=257)",
    "Inoculated virgin male (n=272)",
    
    "Control mated female (n=270)",
    "Inoculated mated female (n=276)",
    "Control mated male (n=277)",
    "Inoculated mated male (n=265)"
    )

read.csv("data/SSMI bootstrap.csv") %>% 
  mutate(int_colour = paste(Mating_status, Sex) %>% tolower(),
         tag = paste(Treatment, int_colour)) %>% 
  mutate(Sex = recode(Sex, "female" = "Female", "male" = "Male"),
         Mating_status = factor(Mating_status, 
                                levels = c("Cohabiting","Virgin","Mated"))) %>% 
  ggplot(aes(x = Day, y = est, ymin = lower, ymax = upper,
             linetype = tag, colour = tag, group = tag)) + 
  geom_line(size = 0.5) + 
  geom_point(size = 0.6) +
  scale_colour_manual(
    values = c(
      "Control virgin female" = "#FF83FA",
      "Control mated female" = "#AB82FF",
      "Control cohabiting female" = "#5D478B",
      "Inoculated virgin female" = "#FF83FA",
      "Inoculated mated female" = "#AB82FF",
      "Inoculated cohabiting female" = "#5D478B",
      "Control virgin male" = "#FFA500",
      "Control mated male" = "#CD8500",
      "Control cohabiting male" = "#8B5A00",
      "Inoculated virgin male" = "#FFA500",
      "Inoculated mated male" = "#CD8500",
      "Inoculated cohabiting male" = "#8B5A00"
    ),
    breaks = set_breaks, labels = set_labels
  ) + 
  scale_linetype_manual(
    values = c(
      "Control virgin female" = "dashed",
      "Control mated female" = "dashed",
      "Control cohabiting female" = "dashed",
      "Inoculated virgin female" = "solid",
      "Inoculated mated female" = "solid",
      "Inoculated cohabiting female" = "solid",
      "Control virgin male" = "dashed",
      "Control mated male" = "dashed",
      "Control cohabiting male" = "dashed",
      "Inoculated virgin male" = "solid",
      "Inoculated mated male" = "solid",
      "Inoculated cohabiting male" = "solid"
    ),
    breaks = set_breaks, labels = set_labels
  ) +
  geom_errorbar(linetype = 'solid', alpha = 0.5, width = 0.5, size = 0.5) +
  labs(x = "Days after spray", y = "Survival Probability", 
       colour = "", linetype = "") +
  thm + ylim(0,1) +
  facet_wrap(~Mating_status) +
  theme_classic()


#### lower panel #### 

df = read.csv("data/SSMI raw.csv") %>% 
  mutate(Mating_status = recode(Mating_status, "Cohabit" = "Cohabiting"),
         Treatment = recode(Treatment, "Fungal" = "Inoculated")) %>% 
  filter(Mating_status != "Virgin CO2") %>%
  droplevels() %>% 
  mutate(sex = ifelse(Sex=="F", "female", "male"),
         tag = as.factor(paste(Treatment, tolower(Mating_status), sex)))

surv_df =
  df %>% 
  group_by(Mating_status,Sex,tag,Treatment) %>% 
  mutate(total = length(Death)) %>%
  group_by(Mating_status,Sex,tag,Treatment,Day,total) %>% 
  summarise(Death = sum(Death)) %>%
  group_by(Mating_status,Sex,tag, Treatment,total) %>% 
  mutate(Death=cumsum(Death)) %>%
  mutate(surv_per = 100*(1-Death/total))

surv_rep = 
  df %>% 
  group_by(Replicate,Mating_status,Sex,tag,Treatment) %>%
  mutate(total = length(Death)) %>%
  group_by(Replicate,Mating_status,Sex,tag,Treatment,Day,total) %>% 
  summarise(Death = sum(Death)) %>%
  group_by(Replicate,Mating_status,Sex,tag, Treatment,total) %>% 
  mutate(Death=cumsum(Death)) %>%
  mutate(surv_per = 100*(1-Death/total))

ggplot(data=NULL,
       aes(x=Day, y=surv_per, linetype=tag, color=tag, group=tag)) +
  geom_line(data=surv_df, size = 1, alpha = 0.5) +
  geom_point(data=surv_rep, size = 0.6) +
  scale_color_manual(
  values = c(
    "Control virgin female" = "#FF83FA",
    "Control mated female" = "#AB82FF",
    "Control cohabiting female" = "#5D478B",
    "Inoculated virgin female" = "#FF83FA",
    "Inoculated mated female" = "#AB82FF",
    "Inoculated cohabiting female" = "#5D478B",
    "Control virgin male" = "#FFA500",
    "Control mated male" = "#CD8500",
    "Control cohabiting male" = "#8B5A00",
    "Inoculated virgin male" = "#FFA500",
    "Inoculated mated male" = "#CD8500",
    "Inoculated cohabiting male" = "#8B5A00"
  ),
  breaks = set_breaks, labels = set_labels
  ) + 
  scale_linetype_manual(
    values = c(
      "Control virgin female" = "dashed",
      "Control mated female" = "dashed",
      "Control cohabiting female" = "dashed",
      "Inoculated virgin female" = "solid",
      "Inoculated mated female" = "solid",
      "Inoculated cohabiting female" = "solid",
      "Control virgin male" = "dashed",
      "Control mated male" = "dashed",
      "Control cohabiting male" = "dashed",
      "Inoculated virgin male" = "solid",
      "Inoculated mated male" = "solid",
      "Inoculated cohabiting male" = "solid"
    ),
    breaks = set_breaks, labels = set_labels
  ) +
  labs(linetype="", color="",
       y = "Survival Percent (%)", color="Mating Status",
       x = "Days after spray") +
  ylim(0,100) +
  theme_classic() +
  facet_grid(~Mating_status)
  


### --------------------------    F2    --------------------------  

reps <- read.csv("data/SSMI larva.csv") %>% 
  mutate(Treatment = recode(Treatment, "Fungal" = "Inoculated"))
model <- lm(larvaPsurv ~ Day + Treatment + Mating_status + 
              Treatment:Mating_status + Day:Treatment, reps)

tb_fit <- reps %>% select(Day, Treatment, Mating_status) %>% unique()
tb_fit <- cbind(tb_fit, predict(model, tb_fit, interval = "confidence")) %>% 
  mutate(tag = as.factor(paste(Treatment, tolower(Mating_status))))

set_breaks = c("Control cohabit","Inoculated cohabit",
               "Control mated","Inoculated mated")

ggplot(data = tb_fit, 
       aes(x = Day, y = fit, ymin = lwr, ymax = upr,
           linetype = tag, color = tag, group = tag)) +
  geom_line(size = 0.9) +
  geom_point(size = 1) +
  # geom_errorbar(size=0.5, width=0.5) +
  scale_linetype_manual(
    values = c(
      "Control cohabit" = "dashed",
      "Inoculated cohabit" = "solid",
      "Control mated" = "dashed",
      "Inoculated mated" = "solid"
    ), 
    breaks = set_breaks
  ) +
  scale_color_manual(
    values = c(
      "Control cohabit" = "#5D478B",
      "Inoculated cohabit" = "#5D478B",
      "Control mated" = "#AB82FF",
      "Inoculated mated" = "#AB82FF"
    ),
    breaks = set_breaks
  ) +
  theme_classic() +
  labs(x = "Days after spray", 
       y = "Offspring count per surviving female")



### --------------------------    F3    --------------------------  
set_line_size = 1.8      
set_point_size = 2.1  

set_err_size = 1.5     
set_err_width = 0.5    
set_alpha = 0.7

df_plot = read.csv('data/GHA bootstrap.csv')

thm = theme(axis.line = element_line(color="gray54", size=0.1, linetype="solid"),
            panel.background = element_rect(fill = NA),
            legend.key = element_rect(fill = NA),
            legend.background = element_rect(fill = NA),
            plot.title = element_text(size = 15, hjust = 0.5)) 

ggplot(df_plot, aes(x=day, y=est, color=Sex, linetype=Treatment, ymin=lower, ymax=upper)) +
  geom_line(size = set_line_size) + geom_point(size = set_point_size) + 
  geom_errorbar(size = set_err_size, width = set_err_width, alpha = set_alpha) +
  
  scale_linetype_manual(values = c("Control" = "dotted","GHA" = "solid")) +
  scale_color_manual(values = c("Female" = "#FF83FA", "Male" = "#FFA500")) +
  
  labs(x="Days", y="Survival Probability") +
  thm +
  ylim(0,1) +
  guides(linetype = guide_legend(order = 1), color = guide_legend(order = 2))






### --------------------------    F4    --------------------------  

#### model plot #### 

df = 
  read.csv("data/DIET 2 bootstrap.csv") %>% 
  select(-X) %>% 
  mutate(Diet = factor(Diet, levels=c("C/C","C/G","G/G","G/C"))) %>% 
  select(c(Treatment,Sex,Diet,Sex,day,est,lower,upper))

df_initial = 
  df %>% 
  select(-c(day,est,lower,upper)) %>% 
  unique() %>% 
  mutate(day=0, est=1, lower=1, upper=1)

set_labels = 
  labels %>% 
  # filter(Diet==d) %>% 
  mutate(Sex = ifelse(Sex=="M", "Male","Female"),
         Sex = factor(Sex, levels=c("Male","Female")),
         tag0 = paste0(Treatment," ",Sex),
         tag = paste0(Treatment," ",Sex, " (n=",Initial,")")) %>% 
  arrange(Treatment,Sex)

bind_rows(df,df_initial) %>% 
  mutate(tag = factor(paste(Treatment, Sex), 
                      levels=c("Control Male","Control Female",
                               "Fungal Male","Fungal Female"))) %>% 
  ggplot(aes(x=day, y=est, ymin=lower, ymax=upper, color=tag, linetype=tag,
             group=interaction(Diet,tag))) +
  geom_errorbar(width=0.5,size=0.7,alpha=0.7) +
  geom_line(size=0.9) +
  geom_point(size=1) +
  scale_x_continuous(breaks=seq(0,12,4)) +
  scale_linetype_manual(values=c("Control Female"="dotted",
                                 "Control Male"="dotted",
                                 "Fungal Female"="solid",
                                 "Fungal Male"="solid"),
                        labels = c("Control Female (n=609)", "Control Male (n=609)", "Fungal Female (n=576)", "Fungal Male (n=576)"),
                        breaks = c('Control Female','Control Male','Fungal Female','Fungal Male')) +
  scale_color_manual(values=c("Control Female"="#5D478B",
                              "Control Male"="#8B5A00",
                              "Fungal Female"="#5D478B",
                              "Fungal Male"="#8B5A00"),
                     labels = c("Control Female (n=609)", "Control Male (n=609)", "Fungal Female (n=576)", "Fungal Male (n=576)"),
                     breaks = c('Control Female','Control Male','Fungal Female','Fungal Male')) +
  facet_grid(~Diet) +
  labs(x="Days after spray", y="Survival Probability", color="", linetype="") +
  theme_classic()


#### raw data plot #### 

read.csv("data/DIET 2 raw.csv") %>% 
  mutate(Sex = recode(Sex, "F" = "Female", "M" = "Male"),
         Diet = factor(Diet, 
                       levels = c("C/C","C/G","G/G","G/C"))) %>% 
  group_by(Diet, Treatment, Sex, Day) %>% 
  summarise(Death = sum(Death),
            Initial = sum(Initial.density)) %>% 
  group_by(Diet, Treatment, Sex) %>% 
  mutate(cumDeath = cumsum(Death)) %>% 
  ungroup() %>% 
  ggplot(aes(x = Day, 
             y = 1 - cumDeath/Initial,
             color = paste(Treatment, Sex))) +
  geom_line() +
  facet_wrap(~Diet, nrow = 1) +
  scale_linetype_manual(values=c("Control Female"="dotted",
                                 "Control Male"="dotted",
                                 "Fungal Female"="solid",
                                 "Fungal Male"="solid")) +
  scale_color_manual(values=c("Control Female"="#5D478B",
                              "Control Male"="#8B5A00",
                              "Fungal Female"="#5D478B",
                              "Fungal Male"="#8B5A00"))



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


set_labels = c("Control Female (n=401)", "Control Male (n=422)", "Fungal Female (n=413)", "Fungal Male (n=382)")
bind_rows(df) %>% 
    group_by(Diet) %>% 
    mutate(tag = factor(paste(Treatment, Sex), 
                        levels=c("Control Male","Control Female",
                                 "Fungal Male","Fungal Female"))) %>% 
    ggplot(aes(x=day, y=est, ymin=lower, ymax=upper, color=tag, 
               group=interaction(Diet,tag))) +
    # geom_errorbar(width=0.3,size=0.5,alpha=0.5) +
    geom_line(size=0.6, aes(linetype=tag)) + geom_point(size=0.6) +
    scale_x_continuous(breaks=seq(0,12,4)) +
    scale_linetype_manual(
      values=c("Control Female"="dotted","Control Male"="dotted","Fungal Female"="solid","Fungal Male"="solid"),
      labels = set_labels,
      breaks = c('Control Female','Control Male','Fungal Female','Fungal Male')) +
    scale_color_manual(
      values=c("Control Female"="#5D478B","Control Male"="#8B5A00","Fungal Female"="#5D478B","Fungal Male"="#8B5A00"),
      labels = set_labels,
      breaks = c('Control Female','Control Male','Fungal Female','Fungal Male')) +
    facet_grid(~Diet) +
    labs(x="Days after spray", y="Survival Probability", color="", linetype="") + theme_classic()
    



### --------------------------    F6    --------------------------  
df = 
  read.csv("./data/DIET 3 bootstrap.csv") %>% 
  select(-X) %>% 
  mutate(Diet = factor(Diet, levels=c("C","CY0.5","CY1.0","CY1.5")),
         Sex = ifelse(Sex=="M", "Male","Female"))

set_labels = c("Control Female (n=1268)", "Control Male (n=1268)", "Fungal Female (n=1133)", "Fungal Male (n=1133)")
bind_rows(df) %>% 
  group_by(Diet) %>% 
  mutate(tag = factor(paste(Treatment, Sex), 
                      levels=c("Control Male","Control Female",
                               "Fungal Male","Fungal Female"))) %>% 
  ggplot(aes(x=day, y=est, ymin=lower, ymax=upper, color=tag, 
             group=interaction(Diet,tag))) +
  # geom_errorbar(width=0.3,size=0.5,alpha=0.5) +
  geom_line(aes(linetype=tag), size=0.6) +
  geom_point(size=0.6) +
  scale_x_continuous(breaks=seq(0,12,4)) +
  scale_linetype_manual(
    values=c("Control Female"="dotted","Control Male"="dotted","Fungal Female"="solid","Fungal Male"="solid"),
    labels = set_labels,
    breaks = c('Control Female','Control Male','Fungal Female','Fungal Male')) +
  scale_color_manual(
    values=c("Control Female"="#5D478B","Control Male"="#8B5A00","Fungal Female"="#5D478B","Fungal Male"="#8B5A00"),
    labels = set_labels,
    breaks = c('Control Female','Control Male','Fungal Female','Fungal Male')) +
  facet_grid(~Diet) +
  labs(x="Days after spray", y="Survival Probability", color="", linetype="") +
  theme_classic()



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









