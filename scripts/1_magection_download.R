
### mage rankings request

request_mage <-'{
   worldData{
       zone(id: %i){
           encounters{
           journalID,
               name,
               characterRankings(metric:dps, className:"Mage", page: %i)
               fightRankings(metric:speed, page: %i)
           }
       }
   }
   }'

### Range (3 for Top 300, max 25 for top 2500)
range <- 1:3

### Request rankings
request_logs_code <- sprintf(request_mage,1018, range, range)

output <- lapply(range, function(k) {

  response <- WCL_API2_request(request_logs_code[k]
  )$data$worldData$zone$encounters

  df_list_with_id <- lapply(1:nrow(response), function(i) {
    output <- do.call(bind_rows, response$characterRankings$rankings[[i]]) %>%
      add_id_column(., response,i) %>%
      mutate(List_page = k) %>%
      do.call(bind_cols, .)
  })

  if (k %% 1 == 0) {
    cat("Processed page",k, "out of", length(range), " - Part 1 \n")
  }
  return(do.call(bind_rows,df_list_with_id))
})

df_cope_mage_ranking <- do.call(bind_rows, output)

### Write to csv
write.csv(df_cope_mage_ranking,paste0("./raw_data/df_cope_mage_ranking_",format(Sys.time(), "%Y_%m_%d_h%H_m%M"),".csv"))

### Quickly process the rankings for further download

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
         cat_ranking = factor(cat_ranking,levels=c("Top-100","200-500")))

#### -Download player log ID (magection)

logID_unique_cope <- unique(df_cope_mage_ranking$logID)
request_logs_code <- sprintf(request_player_str,logID_unique_cope)
n_loop <- length(request_logs_code)

output <- lapply(1:n_loop, function(i) {

  response <- WCL_API2_request(request_logs_code[i]
  )$data$reportData$report$masterData$actors

  if (!is.null(response)) {
    response <- response %>%
      mutate(logID = logID_unique_cope[i])
  } else {
    response <- data.frame(gameID = 0,
                           id = 0,
                           name = "NULL",
                           server = "NULL",
                           subType="NULL",
                           logID = logID_unique_cope[i])
  }

  if (i %% 25 == 0) {
    cat("Processed log",i, "out of", n_loop," Player log ID\n")
  }

  return(response)
})

df_cope_mage_logplayers <- do.call(bind_rows, output)

write.csv(df_cope_mage_logplayers,paste0("./raw_data/df_cope_mage_logplayers_",format(Sys.time(), "%Y_%m_%d_h%H_m%M"),".csv"))


df_cope_mage_master <- df_cope_mage_ranking %>%
  left_join(df_cope_mage_logplayers,
            by=c("logID",
                 "character_name"="name",
                 # "server",
                 "class"="subType")) %>%
  rename(sourceID = id)

logID_mageplayer_unique_cope <- df_cope_mage_master  %>%
  group_by(logID,fightID,sourceID) %>%
  summarise(n=n())


#### -magection Initial specs
request_logs_code <- sprintf(request_spec,
                             logID_mageplayer_unique_cope$logID,
                             logID_mageplayer_unique_cope$fightID,
                             logID_mageplayer_unique_cope$sourceID)

n_loop <- length(request_logs_code)

output <- lapply(1:n_loop, function(i) {

  response <- WCL_API2_request(request_logs_code[i]
  )$data$reportData$report$events$data

  if (!is.null(response) & length(response)!=0){
    response <- response %>%
      mutate(logID = logID_mageplayer_unique_cope$logID[i],
             fightID = fight,
             sourceID = logID_mageplayer_unique_cope$sourceID[i])
  } else {
    response <- data.frame(timestamp = -1,
                           type  = "NULL",
                           sourceID = logID_mageplayer_unique_cope$sourceID[i],
                           targetID= 0,
                           logID = logID_mageplayer_unique_cope$logID[i],
                           abilityGameID = 0,
                           fightID = logID_mageplayer_unique_cope$fightID[i],
                           stack=0)
  }

  if (i %% 25 == 0) {
    cat("Processed log",i, "out of", n_loop," Combatant Data\n")
  }

  return(response)
})

df_cope_mage_specs <- do.call(bind_rows, output)


df_cope_mage_specs$id_list <- seq.int(nrow(df_cope_mage_specs))



df_cope_mage_auras <- rbindlist(df_cope_mage_specs$auras, fill = T, idcol = "id_list")
df_cope_mage_auras <- left_join(df_cope_mage_specs %>% select(logID,sourceID,fightID,id_list),
                                 df_cope_mage_auras, by = c("id_list"))

df_cope_mage_gear <- rbindlist(df_cope_mage_specs$gear, fill = T, idcol = "id_list")
df_cope_mage_gear <- left_join(df_cope_mage_specs %>% select(logID,sourceID,fightID,id_list),
                                df_cope_mage_gear, by = c("id_list"))

df_cope_mage_gems <- rbindlist(df_cope_mage_gear$gems, fill = T, idcol = "sub_id_list")
df_cope_mage_gear$sub_id_list <- seq.int(nrow(df_cope_mage_gear))

df_cope_mage_gem_gear <- left_join(df_cope_mage_gear %>% select(logID,sourceID,fightID,id_list,sub_id_list,id,itemLevel),
                                    df_cope_mage_gems, by = c("sub_id_list"))

df_cope_mage_talents <- rbindlist(df_cope_mage_specs$talents, fill = T, idcol = "id_list")
df_cope_mage_talents <- left_join(df_cope_mage_specs %>% select(logID,sourceID,fightID,id_list),
                                   df_cope_mage_talents, by = c("id_list"))


write.csv(df_cope_mage_specs %>%
            select(-c(auras,gear,talents,talentTree,pvpTalents,
                      customPowerSet,secondaryCustomPowerSet,tertiaryCustomPowerSet)),
          paste0("./raw_data/df_cope_mage_specs_",format(Sys.time(), "%Y_%m_%d_h%H_m%M"),".csv"))


write.csv(df_cope_mage_auras,
          paste0("./raw_data/df_cope_mage_auras_",format(Sys.time(), "%Y_%m_%d_h%H_m%M"),".csv"))

write.csv(df_cope_mage_gear %>% select(-c(gems)),
          paste0("./raw_data/df_cope_mage_gear_",format(Sys.time(), "%Y_%m_%d_h%H_m%M"),".csv"))


write.csv(df_cope_mage_talents,
          paste0("./raw_data/df_cope_mage_talents_",format(Sys.time(), "%Y_%m_%d_h%H_m%M"),".csv"))



#### -magection Buffs

request_logs_code <- sprintf(request_buffs_str,
                             logID_mageplayer_unique_cope$logID,
                             logID_mageplayer_unique_cope$fightID,
                             logID_mageplayer_unique_cope$sourceID)

n_loop <- length(request_logs_code)


output <- lapply(1:n_loop, function(i) {

  response <- WCL_API2_request(request_logs_code[i]
  )$data$reportData$report$events$data

  if (!is.null(response)) {
    response <- response %>%
      mutate(logID = logID_mageplayer_unique_cope$logID[i],
             fightID = fight,
             sourceID = logID_mageplayer_unique_cope$sourceID[i])
  } else {
    response <- data.frame(timestamp = -1,
                           type  = "NULL",
                           sourceID = logID_mageplayer_unique_cope$sourceID[i],
                           targetID= 0,
                           logID = logID_mageplayer_unique_cope$logID[i],
                           abilityGameID = 0,
                           fightID = logID_mageplayer_unique_cope$fightID[i],
                           stack=0)
  }

  if (i %% 25 == 0) {
    cat("Processed log",i, "out of", n_loop," Buff Data\n")
  }

  return(response)
})

df_cope_mage_buffs <- do.call(bind_rows, output)

write.csv(df_cope_mage_buffs,paste0("./raw_data/df_cope_mage_buffs_",format(Sys.time(), "%Y_%m_%d_h%H_m%M"),".csv"))

#### -mage damage

request_logs_code <- sprintf(request_dmg_str,
                             logID_mageplayer_unique_cope$logID,
                             logID_mageplayer_unique_cope$fightID,
                             logID_mageplayer_unique_cope$sourceID)

n_loop <- length(request_logs_code)

output <- lapply(1:n_loop, function(i) {

  response <- WCL_API2_request(request_logs_code[i]
  )$data$reportData$report$events$data

  if (!is.null(response)) {
    response <- response %>%
      mutate(logID = logID_mageplayer_unique_cope$logID[i],
             fightID = fight,
             sourceID = logID_mageplayer_unique_cope$sourceID[i])
  } else {
    response <- data.frame(timestamp = -1,
                           type  = "NULL",
                           sourceID = logID_mageplayer_unique_cope$sourceID[i],
                           targetID= 0,
                           logID = logID_mageplayer_unique_cope$logID[i],
                           abilityGameID = 0,
                           fightID = logID_mageplayer_unique_cope$fightID[i],
                           buffs="NULL",
                           hitType=-1,
                           amount=-1,
                           tick=NA)
  }

  if (i %% 25 == 0) {
    cat("Processed log",i, "out of", n_loop," Damage Data\n")
  }

  return(response)
})

df_cope_mage_dmg <- do.call(bind_rows, output)
df_cope_mage_dmg <- df_cope_mage_dmg %>% clean_names() %>% select(-c(class_resources))

write.csv(df_cope_mage_dmg,
          paste0("./raw_data/df_cope_mage_dmg_",format(Sys.time(),
                                            "%Y_%m_%d_h%H_m%M"),".csv"))


#### -mage Casts

request_logs_code <- sprintf(request_casts_str,
                             logID_mageplayer_unique_cope$logID,
                             logID_mageplayer_unique_cope$fightID,
                             logID_mageplayer_unique_cope$sourceID)

n_loop <- length(request_logs_code)

output <- lapply(1:n_loop, function(i) {

  response <- WCL_API2_request(request_logs_code[i]
  )$data$reportData$report$events$data

  if (!is.null(response)) {
    response <- response %>%
      mutate(logID = logID_mageplayer_unique_cope$logID[i],
             fightID = fight,
             sourceID = logID_mageplayer_unique_cope$sourceID[i])
  } else {
    response <- data.frame(timestamp = -1,
                           type  = "NULL",
                           sourceID = logID_mageplayer_unique_cope$sourceID[i],
                           targetID= 0,
                           logID = logID_mageplayer_unique_cope$logID[i],
                           abilityGameID = 0,
                           fightID = logID_mageplayer_unique_cope$fightID[i],
                           buffs="NULL",
                           hitType=-1,
                           amount=-1,
                           tick=NA)
  }

  if (i %% 25 == 0) {
    cat("Processed log",i, "out of", n_loop,"Casts Data\n")
  }

  return(response)
})

df_cope_mage_casts <- do.call(bind_rows, output)
df_cope_mage_casts <- df_cope_mage_casts %>% clean_names() %>% select(-c(class_resources))

write.csv(df_cope_mage_casts,
          paste0("./raw_data/df_cope_mage_casts",format(Sys.time(),
                                              "%Y_%m_%d_h%H_m%M"),".csv"))
