df_cope_mage_ranking <- read.csv("./raw_data/df_cope_mage_ranking_2023_09_13_h18_m03.csv")

df_cope_mage_logplayers <- read.csv("./raw_data/df_cope_mage_logplayers_2023_09_13_h18_m25.csv")

df_cope_mage_buffs <- read.csv("./raw_data/df_cope_mage_buffs_2023_09_13_h19_m15.csv")

df_cope_mage_dmg <- read.csv("./raw_data/df_cope_mage_dmg_2023_09_13_h19_m42.csv")

df_cope_mage_auras <- read.csv("./raw_data/df_cope_mage_auras_2023_09_13_h18_m50.csv")

df_cope_mage_casts <- read.csv("./raw_data/df_cope_mage_casts2023_09_13_h20_m11.csv")

df_cope_mage_gear <- read.csv("./raw_data/df_cope_mage_gear_2023_09_13_h18_m50.csv")

### Ranking data
df_cope_mage_ranking<- df_cope_mage_ranking %>%

  clean_names() %>%
  rename(character_name = name_1,
         dps = amount,
         logID=code,
         fightID=fight_id,
         logStartTime = start_time_10,
         fightStartTime = start_time_7,
         guildID = id_11,
         serverID = id_14#,
         #server = name_24
  ) %>%

  mutate(duration_s=duration/1000,
         list_id = factor(list_id,levels=unique(df_cope_mage_ranking$List_ID))) %>%
  group_by(list_id) %>%
  mutate(rank_boss = row_number()) %>%
  ungroup()%>%
  mutate(cat_ranking =  ifelse(rank_boss<=100,"Top-100","200-500"),
         cat_ranking = factor(cat_ranking,levels=c("Top-100","200-500"))) %>%

  select(
    logID,fightID,character_name,
    class,   spec,
    dps,
    duration,duration_s,
    fightStartTime,
    logStartTime,
    boss=list_id,rank_boss,cat_ranking
  )


### Ranking + logplayer data
df_cope_mage_master <- df_cope_mage_ranking %>%
  left_join(df_cope_mage_logplayers %>% select(-c(X)),
            by=c("logID",
                 "character_name"="name",
                 # "server",
                 "class"="subType")) %>%
  rename(sourceID = id)

### Buffs filter

mage_Buffs <- c(
  26297, ## Berserking
  10060, ## Power Infusion
  53908, ## Speed
  53909, ## Wild Magic
  28714, ## Flame Cap
  59675, ## Haste Tailor
  59676, ## Crit Tailor
  59674, ## Mana Tailor
  48108,## Hotstreak
  45438,## "Ice Block",
  12051,## "Evocation",
  10278, ## "BoP"
  29166, ## Innervate
  1038, ## Salv
  54646, ## FM
  57933  ## Tricks
)


df_cope_mage_buffs <- df_cope_mage_buffs %>%

  filter(abilityGameID %in%  mage_Buffs)   %>%
  left_join(df_cope_mage_master %>%
              select(logID,fightID,boss,character_name,sourceID,cat_ranking,rank_boss),
            by=c("logID","fightID","sourceID")) %>%

  mutate(abilityGameID = as.character(abilityGameID),
         abilityGameID =  case_when(

           abilityGameID ==  26297 ~ "Horde",
           abilityGameID ==  10060 ~ "PI",
           abilityGameID == 53908 ~  "Speed",
           abilityGameID == 53909 ~  "Wild Magic",
           abilityGameID == 28714 ~  "Flm. Cap",
           abilityGameID == 59675 ~  "Haste Tail.",
           abilityGameID == 59676 ~  "Crit Tail.",
           abilityGameID == 59674 ~ "Mana Tail.",
           abilityGameID == 48108 ~  "Hotstreak",
           abilityGameID == 45438 ~ "IB",
           abilityGameID == 12051 ~ "Evo",
           abilityGameID == 10278 ~ "BoP",
           abilityGameID == 29166 ~ "Vate",
           abilityGameID == 1038 ~ "Salv",
           abilityGameID == 54646 ~ "FM",
           abilityGameID == 57933 ~ "Tricks",
           abilityGameID == 48020 ~ "Tele",
           abilityGameID == 60341 ~ "Crit"
         ),

         dummy=1) %>%

  group_by(logID,fightID,sourceID,abilityGameID,type,boss,cat_ranking,rank_boss) %>%
  summarise(n=n(),
            dummy=sum(dummy)) %>%
  ungroup()

df_cope_mage_buffs <- df_cope_mage_buffs %>%

  pivot_wider(names_from=abilityGameID,values_from = n)

df_cope_mage_buffs_interm <- df_cope_mage_buffs %>%
  mutate_at(c(9:ncol(df_cope_mage_buffs)), ~replace(., is.na(.), 0))


#### Extra detection

tentacle_mage_Detection <- df_cope_mage_dmg %>%
  filter(ability_game_id %in% c(65038, 65033, 65035)) %>%
  select(source_id,fight_id,log_id) %>%
  group_by(source_id,fight_id,log_id) %>%
  summarise(Tenti=1)

Saronite_mage_Detection <- df_cope_mage_dmg %>%
  filter(ability_game_id %in% c(56350)) %>%
  select(source_id,fight_id,log_id) %>%
  group_by(source_id,fight_id,log_id) %>%
  summarise(Bomb=1)

Sapper_mage_Detection <- df_cope_mage_dmg %>%
  filter(ability_game_id %in% c(56488)) %>%
  select(source_id,fight_id,log_id) %>%
  group_by(source_id,fight_id,log_id) %>%
  summarise(Sapp=1)


Shatter_mage_Detection <-df_cope_mage_casts %>%
  filter(ability_game_id %in% c(29858))%>%
  select(source_id,fight_id,log_id)%>%
  group_by(source_id,fight_id,log_id) %>%
  summarise(Shatt=1)

FM_Extra_Detection <- df_cope_mage_auras %>% filter(ability==54646)%>%
  select(logID,fightID,sourceID,ability) %>% ## Fix
  group_by(sourceID,fightID,logID) %>%
  summarise(FM_2=1)

Tailor_Extra_Detection <- df_cope_mage_auras %>% filter(ability%in%c(59674,59675,59676))%>%
  select(logID,fightID,sourceID,ability) %>% ## Fix
  group_by(sourceID,fightID,logID) %>%
  summarise(Tailor_2=1)



Crit_Extra_Detection <- df_cope_mage_auras %>% filter(ability%in%c(60341))%>%
  select(logID,fightID,sourceID,ability) %>% ## Fix
  group_by(sourceID,fightID,logID) %>%
  summarise(Crit2=1)


Crit_enchant_Extra_Detection <- df_cope_mage_gear %>% filter(permanentEnchant %in%c(3788))%>%
  select(logID,fightID,sourceID,permanentEnchant) %>% ## Fix
  group_by(sourceID,fightID,logID) %>%
  summarise(Crit_Enchant2=1)



CS_mage_Detection <-df_cope_mage_casts %>%
  filter(ability_game_id %in% c(2139))%>%
  select(source_id,fight_id,log_id)%>%
  group_by(source_id,fight_id,log_id) %>%
  summarise(cs=1)


