###########################################################################################################
### Gather NFL Data
### Historical Betting lines from http://www.footballlocks.com/archived_nfl_odds_lines_point_spreads.shtml
########################################################################################################### 

library(nflscrapR)

#Get NFL Teams
## Create Team Level D Stats at some point
NFLTeamList <- nflscrapR::nflteams

#Get 2016 Rosters and Subset by Offensive Players Only
NFLPlayerList2016 <- season_rosters(2016, TeamInt = NFLTeamList$Abbr[1])

for (j in 2:32) {
  print(NFLTeamList$Abbr[j])
  tmpPlayerList <- season_rosters(2016, TeamInt = NFLTeamList$Abbr[j])
  NFLPlayerList2016 <- rbind(NFLPlayerList2016, tmpPlayerList)
}

NFLPlayerList2016Offense <- subset(NFLPlayerList, Pos == "QB" | Pos == "RB" | Pos == "WR" | Pos == "TE" | Pos == "K")

#Get Game Data
Games <- season_games(2017)

#Get Detailed Game Statistics by Player for the entire 2016 season.

DetailedStats <- player_game(Games$GameID[1])

for(i in 2:nrow(Games)) {
  print(Games$GameID[i])
  tmpPlayerGame <- player_game(Games$GameID[i])
  DetailedStats <- rbind(DetailedStats, tmpPlayerGame)
}