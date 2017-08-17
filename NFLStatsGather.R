###########################################################################################################
### Gather NFL Data
### Historical Betting lines from http://www.footballlocks.com/archived_nfl_odds_lines_point_spreads.shtml
########################################################################################################### 

library(nflscrapR)
library(dplyr) 
#dplyer required for group_by(), summarise()

#Get NFL Teams and correct team abbreviations
NFLTeamList <- nflscrapR::nflteams
NFLTeamList$Abbr <- recode(NFLTeamList$Abbr, JAC = "JAX", STL = "LA")


#Get 2016 Rosters and Subset by Offensive Players Only
NFLPlayerList2016 <- season_rosters(2016, TeamInt = NFLTeamList$Abbr[1])

for (j in 2:32) {
  print(NFLTeamList$Abbr[j])
  tmpPlayerList <- season_rosters(2016, TeamInt = NFLTeamList$Abbr[j])
  NFLPlayerList2016 <- rbind(NFLPlayerList2016, tmpPlayerList)
}

#NFLPlayerList2016Offense <- subset(NFLPlayerList2016, Pos == "QB" | Pos == "RB" | Pos == "WR" | Pos == "TE" | Pos == "K")

#Get Game Data
Games <- season_games(2017)
Games$home <- recode(Games$home, JAC = "JAX")
#Get Detailed Game Statistics by Player for the entire 2016 season.

DetailedStats <- player_game(Games$GameID[1])

for(i in 2:nrow(Games)) {
  print(Games$GameID[i])
  tmpPlayerGame <- player_game(Games$GameID[i])
  DetailedStats <- rbind(DetailedStats, tmpPlayerGame)
}

#Import 2016 Closing Betting Lines
##2016betdataloc <- c(getwd(),"/data/2016bettinglines.csv")
BetLines2016 <- read.csv("data/2016bettinglines.csv", header = TRUE)
BetLines2016$Date <- as.Date(BetLines2016$Date, "%m/%d/%Y")
BetLines2016 <- rename(BetLines2016, date=Date)

#####################
### Merge Data Files
#####################
# Merge NFLPlayerList and Detailed Stats and Remove NAs
MergedDetailedStats <- merge(DetailedStats, NFLPlayerList2016, by=c("Team", "name"))
### Why did 1300 rows disappear after Merge? ###
#subset for offensive players only
#TODO - Integrate 2 deep and starter data
MergedDetailedStats <- subset(MergedDetailedStats, Pos == "QB" | Pos == "RB" | Pos == "WR" | Pos == "TE")

#############################################
### Draftkings Fantasy Football Calculations
#############################################
#Assign Total Yardage Bonuses
MergedDetailedStats$PassYdBonus <- ifelse(MergedDetailedStats$passyds >= 300, 3, 0)
MergedDetailedStats$RushYdBonus <- ifelse(MergedDetailedStats$rushyds >= 100, 3, 0)
MergedDetailedStats$RecYdBonus <- ifelse(MergedDetailedStats$recyds >= 100, 3, 0)

#Calculate Passing Fantasy Points
MergedDetailedStats$DKPassPoints <- (4*MergedDetailedStats$pass.tds) + (MergedDetailedStats$passyds/25) - (MergedDetailedStats$pass.ints) + (MergedDetailedStats$PassYdBonus) + (MergedDetailedStats$pass.twoptm*2)

#Calculate Rushing Fantasy Points
MergedDetailedStats$DKRushPoints <- (MergedDetailedStats$rushyds/10) + (MergedDetailedStats$rushtds*6) + (MergedDetailedStats$RushYdBonus) - (MergedDetailedStats$totalfumbs) + (MergedDetailedStats$rush.twoptm*2)

#Calculate Receiving Fantasy Points
MergedDetailedStats$DKRecPoints <- (MergedDetailedStats$recept) + (MergedDetailedStats$rec.twoptm) + (MergedDetailedStats$recyds/10) + (MergedDetailedStats$rec.tds*6) + (MergedDetailedStats$RecYdBonus) + (MergedDetailedStats$rec.twoptm*2)

#Calculate Return Fanstasy Points
MergedDetailedStats$DKReturnPoints <- (MergedDetailedStats$kickret.tds*6) + (MergedDetailedStats$puntret.tds*6)

#Calculate Total Offensive Fantasy Points
MergedDetailedStats$DKOffFantasyPoints <- MergedDetailedStats$DKPassPoints + MergedDetailedStats$DKRushPoints + MergedDetailedStats$DKRecPoints + MergedDetailedStats$DKReturnPoints

#Calculate Total Defensive Fanstasy Points by player.  Team totals will be caluclated at a later step.  
## TODO Need different feed with defensive touchdowns, blocked kicks, safety and points allowed.
MergedDetailedStats$DKDefFantasyPoints <- MergedDetailedStats$sacks + (MergedDetailedStats$defints*2) + (ifelse(MergedDetailedStats$totalrecfumbs > 0 & MergedDetailedStats$recfumbs==0, MergedDetailedStats$totalrecfumbs, 0)) + MergedDetailedStats$DKReturnPoints 

#Produce list of unique players in MergedDetailedStats
MDSUniquePlayers <- unique(MergedDetailedStats[c(1,2,5,58,59)])

####################  
### Aggregate Logic
####################

# TODO count() and tally() to get total number of games played by player

#Create aggregate fantasy points based on Team, Game and Position historical data
#Aggregates at game level
AggTeamGame <- MergedDetailedStats %>% group_by(Team, game.id) %>% summarise(DKOffSum=sum(DKOffFantasyPoints), DKDefSum=sum(DKDefFantasyPoints))
AggTeamGamePos <- MergedDetailedStats %>% group_by(Team, game.id, Pos) %>% summarise(DKOffSum=sum(DKOffFantasyPoints), DKDefSum=sum(DKDefFantasyPoints))
AggTeamGamePos <- merge(AggTeamGame, AggTeamGamePos, by=c("Team", "game.id"))
AggTeamGamePos$PctPoints <- (AggTeamGamePos$DKOffSum.y / AggTeamGamePos$DKOffSum.x)

#Aggregates across history
AggTeam <- MergedDetailedStats %>% group_by(Team) %>% summarise(DKOffSum=sum(DKOffFantasyPoints), DKDefSum=sum(DKDefFantasyPoints))
AggTeamPos <- MergedDetailedStats %>% group_by(Team, Pos) %>% summarise(DKOffSum=sum(DKOffFantasyPoints), DKDefSum=sum(DKDefFantasyPoints))
AggTeamPos <- merge(AggTeam, AggTeamPos, by="Team")
AggTeamPos$PctPoints <- (AggTeamPos$DKOffSum.y / AggTeamPos$DKOffSum.x)
AggTeamPosPlayer <- MergedDetailedStats %>% group_by(Team, Pos, playerID) %>% summarise(DKOffSum=sum(DKOffFantasyPoints), DKDefSum=sum(DKDefFantasyPoints), DKOffSD=sd(DKOffFantasyPoints), DKOffMean=mean(DKOffFantasyPoints))
AggTeamPosPlayer <- merge(AggTeam, AggTeamPosPlayer, by="Team")
AggTeamPosPlayer <- merge(MDSUniquePlayers, AggTeamPosPlayer, by=c("Team","Pos", "playerID"))
AggTeamPosPlayer$PctPoints <- (AggTeamPosPlayer$DKOffSum.y / AggTeamPosPlayer$DKOffSum.x)
#SDPlayer <- MergedDetailedStats %>% group_by(Team, Pos, Player) %>% summarise(DKOffSD=sd(DKOffFantasyPoints), DKOffMean=mean(DKOffFantasyPoints))

################################################
#Vegas Points to DK Fanstasy Points Correlation
################################################

#Link Bettting Line data to game.id and AggTeamGame
AggTeamGame <-  merge(Games, AggTeamGame, by.x=c("GameID"), by.y=c("game.id")) 
# Merge Betting Lines with AggTeamGame 
AggTeamGame <- merge(BetLines2016, AggTeamGame, by=c("date", "home", "away"), all.y = TRUE)

#Calculate projected points based on vegas spread and over/under
##proj points = (OU / 2) - (Spread / 2) with logic to apply the result to the correct team.
## coerce Favortie and Team to character
AggTeamGame$Team <- as.character(AggTeamGame$Team)
AggTeamGame$Favorite <- as.character(AggTeamGame$Favorite)
AggTeamGame$VegasPoints <- ifelse(AggTeamGame$Team == AggTeamGame$Favorite, ((AggTeamGame$Total / 2) - (AggTeamGame$Spread / 2)), ((AggTeamGame$Total / 2) + (AggTeamGame$Spread / 2)))

#Calculate correlation factor between Vegas projected points and actual fantasy points
## Correllation Factor = DKOffFantasyPoints / Vegas Projected Points
AggTeamGame$CF <- (AggTeamGame$DKOffSum / AggTeamGame$VegasPoints)
CF <- mean(AggTeamGame$CF, na.rm = TRUE)

#CF by team
CFTeam <- AggTeamGame %>% group_by(Team) %>% summarise(CFTeam=mean(CF, na.rm=TRUE))
