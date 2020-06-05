
# Last Edited: June 6, 2020





### --------------------------    Figure 1    --------------------------  ###


### --------------------------    F4    --------------------------  ###

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