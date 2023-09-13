
#### -Plot 5 - Buffs

PI_img<- rasterGrob(readJPEG('./_img/icons/PI.jpg',native=T), interpolate=TRUE)
Berserk_img<- rasterGrob(readJPEG('./_img/icons/Berserk.jpg',native=T), interpolate=TRUE)
IB_img<- rasterGrob(readJPEG('./_img/icons/IB.jpg',native=T), interpolate=TRUE)
evo_img<- rasterGrob(readJPEG('./_img/icons/evo.jpg',native=T), interpolate=TRUE)
bop_img<- rasterGrob(readJPEG('./_img/icons/bop.jpg',native=T), interpolate=TRUE)
tailor_img<- rasterGrob(readJPEG('./_img/icons/tailor.jpg',native=T), interpolate=TRUE)
tentacle_img<- rasterGrob(readJPEG('./_img/icons/tentacle.jpg',native=T), interpolate=TRUE)
vate_img<- rasterGrob(readJPEG('./_img/icons/innervate.jpg',native=T), interpolate=TRUE)
salv_img<- rasterGrob(readJPEG('./_img/icons/salv.jpg',native=T), interpolate=TRUE)
bomb_img<- rasterGrob(readJPEG('./_img/icons/bomb.jpg',native=T), interpolate=TRUE)
sapper_img<- rasterGrob(readJPEG('./_img/icons/sapper.jpg',native=T), interpolate=TRUE)
Tricks_img<- rasterGrob(readJPEG('./_img/icons/Tricks.jpg',native=T), interpolate=TRUE)
FM_img<- rasterGrob(readJPEG('./_img/icons/FM.jpg',native=T), interpolate=TRUE)
tailor2_img<- rasterGrob(readJPEG('./_img/icons/tailor2.jpg',native=T), interpolate=TRUE)

#download.file("https://wow.zamimg.com/images/wow/icons/large/spell_shadow_demoniccircleteleport.jpg",
#              './_img/icons/teleport.jpg', mode = 'wb')
tele_img<- rasterGrob(readJPEG('./_img/icons/teleport.jpg',native=T), interpolate=TRUE)

#download.file("https://wow.zamimg.com/images/wow/icons/large/spell_arcane_arcane01.jpg",
#              './_img/icons/shatt.jpg', mode = 'wb')
shatt_img<- rasterGrob(readJPEG('./_img/icons/shatt.jpg',native=T), interpolate=TRUE)

#download.file("https://wow.zamimg.com/images/wow/icons/large/inv_alchemy_potion_02.jpg",
#             './_img/icons/crit.jpg', mode = 'wb')
crit_img<- rasterGrob(readJPEG('./_img/icons/crit.jpg',native=T), interpolate=TRUE)

horde_img<- rasterGrob(readJPEG('./_img/icons/Berserk.jpg',native=T), interpolate=TRUE)

download.file("https://wow.zamimg.com/images/wow/icons/large/spell_frost_iceshock.jpg",
             './_img/icons/cs.jpg', mode = 'wb')
cs_img <- rasterGrob(readJPEG('./_img/icons/cs.jpg',native=T), interpolate=TRUE)


df_cope_buffs_3 <- df_interim %>%
  select(-c(type)) %>%

  ## Add Tentacle data
  left_join(tentacle_mage_Detection, by=c("logID"="log_id",
                                           "fightID"="fight_id",
                                           "sourceID"="source_id")) %>%
  mutate(Tenti = ifelse(is.na(Tenti),0,Tenti)) %>%

  ## Add Bombs
  left_join(Saronite_mage_Detection, by=c("logID"="log_id",
                                           "fightID"="fight_id",
                                           "sourceID"="source_id")) %>%
  mutate(Bomb = ifelse(is.na(Bomb),0,Bomb)) %>%

  ## Add Sapper
  left_join(Sapper_mage_Detection, by=c("logID"="log_id",
                                         "fightID"="fight_id",
                                         "sourceID"="source_id")) %>%
  mutate(Sapp = ifelse(is.na(Sapp),0,Sapp)) %>%

  ## Add CS
  left_join(CS_mage_Detection, by=c("logID"="log_id",
                                         "fightID"="fight_id",
                                         "sourceID"="source_id")) %>%
  mutate(cs = ifelse(is.na(cs),0,cs)) %>%

  ## Add FM Extra layer
  left_join(FM_Extra_Detection, by=c("logID",
                                     "fightID",
                                     "sourceID")) %>%
  mutate(FM_2 = ifelse(is.na(FM_2),0,FM_2)) %>%
  mutate(FM = ifelse(FM>=1|FM_2>=1,1,0)) %>%

  ## Add Tailor Extra layer
  left_join(Tailor_Extra_Detection, by=c("logID",
                                     "fightID",
                                     "sourceID")) %>%
  mutate(Tailor_2 = ifelse(is.na(Tailor_2),0,Tailor_2))%>%

  ## Add Crit Elixir
  left_join(Crit_Extra_Detection, by=c("logID",
                        "fightID",
                        "sourceID")) %>%
  mutate(Crit2 = ifelse(is.na(Crit2),0,Crit2))%>%

  ## Add Crit Enchant
  left_join(Crit_enchant_Extra_Detection, by=c("logID",
                                               "fightID",
                                               "sourceID")) %>%
  mutate(Crit_Enchant2 = ifelse(is.na(Crit_Enchant2),0,Crit_Enchant2)) %>%

  #mutate(Crit = ifelse(Crit_Enchant2>=1|Crit2>=1 | Crit >=1,1,0)) %>%
  ungroup()

### External
df_cope_buffs_3_download <- df_cope_buffs_3


df_cope_buffs_3 <- df_cope_buffs_3 %>%

  group_by(logID,fightID,sourceID,boss,cat_ranking) %>%
  summarize(
    across(where(is.double),sum)
  ) %>%
  ungroup() %>%

  mutate(Tailor = `Haste Tail.`+ `Mana Tail.` + `Crit Tail.`) %>%

  mutate(Tailor = ifelse(Tailor>=1|Tailor_2>=1,1,0))%>%


  select(-c(`Haste Tail.`, `Mana Tail.`, `Crit Tail.`,FM_2,Tailor_2,Crit_Enchant2,Crit2))



df_cope_buffs_3 <- df_cope_buffs_3 %>%

  mutate_at(c(6:ncol(df_cope_buffs_3)), ~ifelse(. > 0, "Yes", "No")) %>%

  pivot_longer(
    cols = c(6:ncol(df_cope_buffs_3)),
    names_to = "Ability",
    values_to = "n") %>%
  group_by(boss,cat_ranking ,Ability,n) %>%
  summarise(n2=n()) %>%

  pivot_wider(
    names_from = Ability,
    values_from=n2) %>%

  mutate(dummy = na.locf(dummy,fromLast=T))


df_cope_buffs_3 <- df_cope_buffs_3 %>%
  mutate_at(c(4:length(df_cope_buffs_3)), ~replace(., is.na(.), 0)) %>%
  mutate(across(BoP:cs, ~./dummy, .names = "per_{.col}"))

df_cope_buffs_3 <- df_cope_buffs_3 %>%

  pivot_longer(
    cols = c(4:length(df_cope_buffs_3)),
    names_to = "Ability",
    values_to = "n2")

#
# df_cope_buffs_3 %>%
#   group_by(boss,Ability) %>%
#   summarise(total=sum(n2))  %>%
#   arrange(desc(total))


order_magecope_buff <- df_cope_buffs_3%>%

  filter(!Ability%in%c("per_Speed","per_Wild Magic","per_Flm. Cap","per_Hotstreak") & startsWith(Ability, "per_")) %>%
  filter (n=="Yes") %>%
  group_by(Ability) %>%
  summarise(total=sum(n2))  %>%
  arrange(desc(total)) %>%

  select(Ability) %>%

  # filter(Ability != "dummy" & startsWith(Ability, "per_")) %>%

  pull(.)



order_magecope_buff_label <- df_cope_buffs_3 %>%
  filter (n=="Yes") %>%
  group_by(Ability) %>%
  summarise(total=sum(n2))  %>%
  arrange(desc(total)) %>%

  filter(!Ability%in%c("Hotstreak", "Speed","Wild Magic","Flm. Cap") & !startsWith(Ability, "per_")) %>%

  select(Ability) %>%

  pull(.)



order_magecope_buff_alt <- c("per_Horde",
                             "per_FM","per_PI","per_Vate","per_Tricks",
                             "per_IB","per_Evo","per_cs",

                             "per_Salv", "per_BoP",

                              "per_Sapp" ,"per_Bomb","per_Tailor","per_Tenti")


##### *Plot 5 wide

xmax <- 1.8
xmin <-0.2
plot_5 <- df_cope_buffs_3 %>%

ungroup()%>%

  filter(!Ability%in%c("per_Speed","per_Wild Magic","per_Flm. Cap","per_Hotstreak") & startsWith(Ability, "per_")) %>%

  mutate(Ability=factor(Ability,
                        labels= gsub("per_", "", order_magecope_buff_alt),
                        levels=order_magecope_buff_alt)) %>%

  ggplot(aes(y = n2,
             x = Ability,
             fill=n,
             alpha=n)) +

  geom_bar(stat="identity", position="fill",width =0.8) +

  scale_fill_manual(values=c("grey",class_wow_color)) +
  scale_alpha_manual(values=c(0.5,1)) +

  scale_y_continuous(labels = scales::percent,
                     limits = c(0,1.19),
                     breaks = seq(0,1,by=c(0.25)))  +

  labs(x="Buff",y="% of magection Warlocks",

       caption=c("<p>Gift me a coffee: https:&#47;&#47;www&#46;ko-fi.com/forge<br>Last Update: 13/09/2023 &#40;dd/mm/yyyy&#41;</p>"),

       title="<p>C) Buffs and utility</p>")+

  geom_hline(yintercept=0.5, linetype="dashed",size=0.25,alpha=0.75)+
  geom_hline(yintercept=0.25, linetype="dotted",size=0.25,alpha=0.75)+
  geom_hline(yintercept=0.75, linetype="dotted",size=0.25,alpha=0.75)+


  annotation_custom(horde_img,xmin=xmin, xmax=xmax,
                    ymin=1.01, ymax=1.25)+

annotation_custom(FM_img,xmin=xmin+1, xmax=xmax+1,
                  ymin=1.01, ymax=1.25) +
  annotation_custom(PI_img,xmin=xmin+2, xmax=xmax+2,
                    ymin=1.01, ymax=1.25)+
  annotation_custom(vate_img,xmin=xmin+3, xmax=xmax+3,
                    ymin=1.01, ymax=1.25) +
  annotation_custom(Tricks_img ,xmin=xmin+4, xmax=xmax+4,
                    ymin=1.01, ymax=1.25)  +

  annotation_custom(IB_img ,xmin=xmin+5, xmax=xmax+5,
                    ymin=1.01, ymax=1.25)+
  annotation_custom(evo_img,xmin=xmin+6, xmax=xmax+6,
                    ymin=1.01, ymax=1.25)+
  annotation_custom(cs_img,xmin=xmin+7, xmax=xmax+7,
                    ymin=1.01, ymax=1.25) +


  annotation_custom(salv_img,xmin=xmin+8, xmax=xmax+8,
                    ymin=1.01, ymax=1.25)  +
  annotation_custom(bop_img,xmin=xmin+9, xmax=xmax+9,
                    ymin=1.01, ymax=1.25) +

  annotation_custom(sapper_img,xmin=xmin+10, xmax=xmax+10,
                    ymin=1.01, ymax=1.25) +
  annotation_custom(bomb_img,xmin=xmin+11, xmax=xmax+11,
                    ymin=1.01, ymax=1.25)+
  annotation_custom(tailor2_img,xmin=xmin+12, xmax=xmax+12,
                    ymin=1.01, ymax=1.25)+
  annotation_custom(tentacle_img,xmin=xmin+13, xmax=xmax+13,
                    ymin=1.01, ymax=1.25) +


  geom_vline(xintercept=1.5, linetype="solid",size=0.25)+
  geom_vline(xintercept=5.5, linetype="solid",size=0.25)+
  geom_vline(xintercept=8.5, linetype="solid",size=0.25) +
  geom_vline(xintercept=10.5, linetype="solid",size=0.25)+

  facet_wrap(.~boss,nrow=5) +
  vivax_theme() +
  guides(size="none",alpha="none",fill="none",color="none") +
  theme(plot.caption = element_markdown(face = "italic",
                                        hjust = 0,
                                        vjust=1,
                                        size = scale_factor * 8,
                                        lineheight=0.3,
                                        margin = margin(t = 1, unit = "pt")),

        axis.text.x = element_markdown(size= scale_factor * 6,
                                       angle=25,hjust=1,vjust=1,
                                       margin = margin(t = 0,
                                                       l=0, unit = "pt"),lineheight = 0))
#axis.text.x = element_blank(),
#axis.ticks.x = element_blank())


ggsave(paste0("./_img/plot_5_",class_wow,".png"),plot_5,
       width = 3, height =7,units="in",device = "png",dpi=300)
