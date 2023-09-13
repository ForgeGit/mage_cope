
add_id_column <- function(df,response, id) {
  df %>% mutate(List_ID = response[id,2])
}


### player data for logs

request_player_str <-     '{
    reportData {
        report(code: "%s") {
            masterData(translate: true) {
                actors(type: "player"){

                gameID
                id
                name
                server
                subType

                }
        }
    }
}}'



### buffs events

request_buffs_str <- '{
    reportData {
        report(code: "%s") {
            events(dataType:Buffs
                  killType:Encounters
                  hostilityType:Friendlies
                  fightIDs:%i
                  sourceID:%i
                  startTime: 0
                  endTime: 999999999999
                  includeResources: true){
              data
              nextPageTimestamp
              }
            }
        }
        }'



### Damage events

request_dmg_str <- '{
    reportData {
        report(code: "%s") {
            events(dataType:DamageDone
                  killType:Encounters
                  hostilityType:Friendlies
                  fightIDs:%i
                  sourceID:%i
                  startTime: 0
                  endTime: 999999999999
                  includeResources: true){
              data
              nextPageTimestamp
              }
            }
        }
        }'


### Casts events

request_casts_str <- '{
    reportData {
        report(code: "%s") {
            events(dataType:Casts
                  killType:Encounters
                  hostilityType:Friendlies
                  fightIDs:%i
                  sourceID:%i
                  startTime: 0
                  endTime: 999999999999
                  includeResources: true){
              data
              nextPageTimestamp
              }
            }
        }
        }'


### Start events


request_spec <-'{
    reportData {
        report(code: "%s") {
            events(
                dataType: CombatantInfo
                startTime: 0
                endTime: 999999999999
                fightIDs: %i
                sourceID: %i
                hostilityType: Friendlies
                includeResources: true

            ) {
                data
                nextPageTimestamp
            }
        }
    }
}'
