#### -Plot 6 & 7

##### *Preprocessing
#
# plot_6_min <- df_master%>%
#
#   mutate(rank_color = case_when(rank_boss == 1 ~ "1",
#                                 rank_boss == 2 ~ "2",
#                                 rank_boss == 3 ~ "3",
#                                 rank_boss == 4 ~ "4",
#                                 rank_boss == 5 ~ "5",
#                                 rank_boss >=6 & rank_boss<=50 ~ "6-50",
#                                 rank_boss >=51 & rank_boss<=100 ~ "51-100",
#                                 rank_boss >=101 & rank_boss<=250 ~ "101-250",
#                                 rank_boss >=251 & rank_boss<=500 ~ "251-500"),
#          rank_color = factor(rank_color,
#                              levels=c("1","2",
#                                       "3", "4",
#                                       "5","6-50",
#                                       "51-100", "101-250",
#                                       "251-500" ))
#   ) %>%
#   group_by(boss) %>%
#   summarise(min=min(dps),
#             max=max(dps),
#             mean=mean(dps)) %>%
#   ungroup() %>%
#   pivot_longer(c(min,max,mean),names_to = "stat") %>%
#   mutate(
#     dps=round(value),
#
#     y= case_when(
#       stat=="mean" ~ 24400,
#       stat=="min" ~ 24400,
#       stat=="max" ~ 24400
#     ),
#
#     stat= case_when(
#       stat=="mean" ~ "Mean",
#       stat=="min" ~ "Min.",
#       stat=="max" ~ "Max."
#     )
#
#   )
# plot_6_min_2 <- df_master%>%
#
#   mutate(rank_color = case_when(rank_boss == 1 ~ "1",
#                                 rank_boss <= 5 & rank_boss>1 ~ "2-5",
#                                 rank_boss >=6 & rank_boss<=500 ~ "6-500"),
#          rank_color = factor(rank_color,
#                              levels=c("1","2-5",
#                                       "6-500" ))
#   ) %>%
#   group_by(boss,rank_color) %>%
#   summarise(min=min(dps),
#             max=max(dps),
#             mean=mean(dps)) %>%
#   ungroup() %>%
#   pivot_longer(c(min,max,mean),names_to = "stat") %>%
#   mutate(
#     dps=round(value),
#
#     y= case_when(
#       stat=="mean" ~ value,
#       stat=="min" ~ value,
#       stat=="max" ~ value
#     ),
#
#     y2 = y+1600,
#
#     stat= case_when(
#       stat=="mean" ~ "Mean",
#       stat=="min" ~ "Min.",
#       stat=="max" ~ "Max."
#     ),
#
#
#     list_id_2 = factor(boss,
#                        labels=c( "The Northrend Beasts","Lord Jaraxxus" ,
#                                  "Faction Champions",
#                                  "Twin Val'kyr", "Anub'arak"))
#
#   )
#



plot_7_summary_tabler6to500<- df_master%>%

  mutate(rank_color = case_when(rank_boss == 1 ~ "1",
                                rank_boss == 2 ~ "2",
                                rank_boss == 3 ~ "3",
                                rank_boss == 4 ~ "4",
                                rank_boss == 5 ~ "5",
                                rank_boss >=6 & rank_boss<=50 ~ "6-50",
                                rank_boss >=51 & rank_boss<=100 ~ "51-100",
                                rank_boss >=101 & rank_boss<=250 ~ "101-250",
                                rank_boss >=251 & rank_boss<=500 ~ "251-500"),
         rank_color = factor(rank_color,
                             levels=c("1","2",
                                      "3", "4",
                                      "5","6-50",
                                      "51-100", "101-250",
                                      "251-500" ))
  ) %>%
  filter(rank_boss>5) %>%
  group_by(boss) %>%
  summarise(min=min(dps),
            max=max(dps),
            mean=mean(dps)) %>%
  ungroup() %>%
  pivot_longer(c(min,max,mean),names_to = "stat") %>%
  mutate(
    dps=round(value),

    y= case_when(
      stat=="mean" ~ value,
      stat=="min" ~ value,
      stat=="max" ~ value
    ),

    y2 = y-1000,

    stat= case_when(
      stat=="mean" ~ "Mean",
      stat=="min" ~ "Min.",
      stat=="max" ~ "Max."
    ),


    list_id_2 = factor(boss,
                       labels=c( "The Northrend Beasts","Lord Jaraxxus" ,
                                 "Faction Champions",
                                 "Twin Val'kyr", "Anub'arak"))

  )


plot_6_min_2 <- df_master%>%

  mutate(rank_color = case_when(rank_boss == 1 ~ "1",
                                rank_boss <= 5 & rank_boss>1 ~ "2-5",
                                rank_boss >=6 & rank_boss<=500 ~ "6-500"),
         rank_color = factor(rank_color,
                             levels=c("1","2-5",
                                      "6-500" ))
  ) %>%
  group_by(boss,rank_color) %>%
  summarise(min=min(dps),
            max=max(dps),
            mean=mean(dps)) %>%
  ungroup() %>%
  pivot_longer(c(min,max,mean),names_to = "stat") %>%
  mutate(
    dps=round(value),

    y= case_when(
      stat=="mean" ~ value,
      stat=="min" ~ value,
      stat=="max" ~ value
    ),

    y2 = y+1000,

    stat= case_when(
      stat=="mean" ~ "Mean",
      stat=="min" ~ "Min.",
      stat=="max" ~ "Max."
    ),


    list_id_2 = factor(boss,
                       labels=c( "The Northrend Beasts","Lord Jaraxxus" ,
                                 "Faction Champions",
                                 "Twin Val'kyr", "Anub'arak"))

  )


#### -Plot 7 - Rankings

plot_7_rank1 <- plot_6_min_2 %>% filter(rank_color==1 & stat=="Max.") %>% select(boss,value,y)



plot_7 <- df_master %>%

  mutate(rank_color = case_when(rank_boss == 1 ~ "1",
                                rank_boss == 2 ~ "2",
                                rank_boss == 3 ~ "3",
                                rank_boss == 4 ~ "4",
                                rank_boss == 5 ~ "5",
                                rank_boss >=6 & rank_boss<=50 ~ "6-50",
                                rank_boss >=51 & rank_boss<=100 ~ "51-100",
                                rank_boss >=101 & rank_boss<=200 ~ "101-200",
                                rank_boss >=201 & rank_boss<=300 ~ "201-300"),
         rank_color = factor(rank_color,
                             labels=c("","1","","2",
                                      "3", "4",
                                      "5","6-50",
                                      "51-100", "101-200",
                                      "201-300" ),
                             levels=c("0","1","1.5","2",
                                      "3", "4",
                                      "5","6-50",
                                      "51-100", "101-200",
                                      "201-300" )),


         list_id_2 = factor(boss,
                            labels=c( "The Northrend Beasts","Lord Jaraxxus" ,
                                      "Faction Champions",
                                      "Twin Val'kyr", "Anub'arak"))
  )%>%
  group_by(list_id_2,rank_color) %>%
  summarise(dps = mean(dps, na.rm = T)) %>%
  ggplot(aes(x=rank_color,y=dps)) +

  geom_segment(aes(x=rank_color, xend=rank_color,
                   y=10000, yend=dps,
                   color=rank_color),linewidth=1.5) +



  geom_point(stroke=NA, size=3,
             aes(color=rank_color)) +
  #geom_smooth(method=lm)+
  scale_y_continuous(labels = unit_format(unit = "k", scale = 1e-3),
                     limits=c(10000,max(df_master$dps)+1600),
                     breaks = seq(10000,max(df_master$dps),2000)
  )+

  # limits=c(0,max(df_cope_2$dps)))+
  #                    expand = c(0, 0)) +
  #  scale_y_log10(breaks=c(10,50,75,100,125,150,1000)) +
  facet_wrap(.~list_id_2,nrow=5)+
  vivax_theme()+
  theme(axis.text.x = element_markdown(size= scale_factor * 8,
                                       angle=25,hjust=1,vjust=1,
                                       margin = margin(t = 0,
                                                       l=0, unit = "pt"),lineheight = 0),

        plot.caption = element_markdown(face = "italic",
                                        hjust = 0,
                                        vjust=0,
                                        size = scale_factor * 8,
                                        lineheight=0.3,
                                        margin = margin(t = 5,
                                                        l=0,
                                                        unit = "pt"))
  )+
  geom_vline(xintercept=1.5,linetype="solid")+
  # geom_vline(xintercept=5.5,linetype="solid")+
  geom_hline(yintercept=seq(10000,24000,4000),linetype="dotted", alpha=0.25)+

  geom_hline(aes(yintercept=ifelse(list_id_2=="The Northrend Beasts",
                                   plot_7_rank1%>%
                                     filter(boss=="The Northrend Beasts") %>%
                                     select(value) %>%
                                     pull(.) ,0)),
             linetype="dashed",color="grey30", size=0.8,alpha=0.75)+
  geom_hline(aes(yintercept=ifelse(list_id_2=="Lord Jaraxxus",
                                   plot_7_rank1%>%
                                     filter(boss=="Lord Jaraxxus") %>%
                                     select(value) %>%
                                     pull(.),0)),
             linetype="dashed",color="grey30", size=0.8) +
  geom_hline(aes(yintercept=ifelse(list_id_2=="Faction Champions",
                                   plot_7_rank1%>%
                                     filter(boss=="Faction Champions") %>%
                                     select(value) %>%
                                     pull(.) ,0)),
             linetype="dashed",color="grey30",size=0.8) +
  geom_hline(aes(yintercept=ifelse(list_id_2=="Twin Val'kyr",
                                   plot_7_rank1%>%
                                     filter(boss=="Twin Val'kyr") %>%
                                     select(value) %>%
                                     pull(.) ,0)),
             linetype="dashed",color="grey30",size=0.8)+
  geom_hline(aes(yintercept=ifelse(list_id_2=="Anub'arak",
                                   plot_7_rank1%>%
                                     filter(boss=="Anub'arak") %>%
                                     select(value) %>%
                                     pull(.) ,0)),
             linetype="dashed",color="grey30",size=0.8)   +
  labs(y="DPS",

       x="DPS Rankings",

       title="B) DPS Ranks",

       caption=c("<p><br></p>"))     +

  scale_color_manual(values=c("#E0C97D",
                              "#E0C97D","#E0C97D","#E0C97D","#E0C97D",
                              "#E0C97D","#E0C97D","#E268A8","#E268A8"))+

  guides(size="none",alpha="none",fill="none",color="none")  +

  geom_text(data=plot_6_min_2 %>% filter(rank_color==1 & stat=="Max."),
            aes(label = paste0("1°: ",scales::comma(round(value),accuaracy=1)," DPS"),y=y2),
            x=9.5, lineheight = .225, hjust = 1,
            size= scale_factor*3.6, color="black", fontface = "bold",alpha=0.75
  ) #+

# geom_text(data=plot_7_summary_tabler6to500 %>% filter(stat=="Mean"),
#           aes(label = paste0("Top 500: ",scales::comma(round(value),accuaracy=1)," DPS"),y=y2),
#           x=9.5, lineheight = .225, hjust = 1,
#           size= scale_factor*3.4, color="#E268A8", fontface = "bold",alpha=0.5
# )

ggsave(paste0("./_img/plot_7_",class_wow,".png"),plot_7,
       width = 2.1, height =7.1,units="in",device = "png",dpi=300)

