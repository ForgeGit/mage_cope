#### -Plot 1 - Fight Length

plot_1_min <- df_master %>%
  group_by(boss) %>%
  summarise(min=min(duration_s),
            max=max(duration_s),
            median=median(duration_s)) %>%
  ungroup() %>%
  pivot_longer(c(min,max,median),names_to = "stat") %>%
  mutate(
    value=round(value),

    y= case_when(
      stat=="median" ~ 1.5,
      stat=="min" ~ 0.5,
      stat=="max" ~ 2.5
    ),

    stat= case_when(
      stat=="median" ~ "Median:\n",
      stat=="min" ~ "Shortest:\n",
      stat=="max" ~ "Longest:\n"
    )

  )

plot_1_min_2 <- data.frame(x=seq(30,max(df_master$duration_s),60)) %>%
  mutate(lab = paste0((x+30)/60," Min."))


plot_1<-df_master %>%

  ggplot(aes(x=duration_s)) +
  geom_violin(size=0.5,aes(y=2.5))+
  geom_boxplot(width=0.25,aes(y=2.5))+
  geom_jitter(#jitter.color = NA,
    #outlier.colour = NULL,
    stroke = 0,aes(y=1.5,color=cat_ranking,
                   alpha=cat_ranking,
                   size=cat_ranking)) +
  geom_histogram(aes(y = ..ncount..),fill=class_wow_color)+
  scale_x_continuous(breaks =seq(0,max(df_master$duration_s),60),
                     limits=c(0,360),
                     expand = c(0, 0)) +
  scale_y_continuous(limits=c(0,3.3)) +
  facet_wrap(.~boss,nrow=5) +
  vivax_theme()+
  theme(axis.text.x = element_markdown(size= scale_factor * 9,
                                       angle=0,hjust=0.5,vjust=0.6,
                                       margin = margin(t = 0,l=0, unit = "pt"),lineheight = 0),
        plot.caption = element_markdown(face = "italic",
                                        hjust = 0,
                                        vjust=0,
                                        size = scale_factor * 8,
                                        lineheight=0.3,
                                        margin = margin(t = 5,
                                                        l=0,
                                                        unit = "pt"))
  )+
  #  geom_hline(yintercept=100, linewidth=0.5,linetype="dotted", colour="#DC3220") +
  # geom_hline(yintercept=125, linewidth=0.25,linetype="dotted", colour="#005AB5")+
  # geom_hline(yintercept=75, linewidth=0.25,linetype="dotted", colour="#005AB5") +
  geom_vline(xintercept=seq(60,max(df_master$duration_s),60),linetype="twodash") +
  geom_vline(xintercept=seq(90,max(df_master$duration_s),60),linetype="dotted",alpha=0.5)  +
  #geom_vline(xintercept=5.5,linetype="dotted") +
  labs(y="", x="Fight Length (Seconds)",

       title="A) Fight Length",

       caption=c("<p><span style='font-family:forgefooter'>&#xe900;</span> &emsp; Discord: Discfordge &#91;Vivax-Pagle(US)&#93;<br><span style='font-family:forgefooter'>&#xe901;</span> https:&#47;&#47;www&#46;github.com/ForgeGit/class_cope</p>")) +                                                           # Remove axis labels & ticks
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())+
  scale_color_manual(values=c("#E5CC80","#E268A8")) +
  scale_alpha_manual(values=c(1,0.25)) +
  scale_size_manual(values=c(1,0.8))  +
  guides(size="none",alpha="none",fill="none",color="none") +

  geom_text(data=plot_1_min,
            aes(label = paste0(stat,value,"s"),y=y),
            x=5, lineheight = .25, hjust = 0,
            size= scale_factor*3.25, color=class_wow_color, fontface = "bold"
  ) +

  geom_text(data=plot_1_min_2,
            aes(label = paste0(lab),y=3.15,
                x=x), lineheight = .225, hjust = 0.5, alpha=0.5,
            size= scale_factor*3.25, color="black", fontface = "bold"
  )
# annotate('rect', ymin=90, ymax=110, xmin=0, xmax=4, alpha=.2, fill='#DC3220')

ggsave(paste0("./_img/plot_1_",class_wow,".png"),plot_1,
       width = 3.3, height =7,units="in",device = "png",dpi=300)
