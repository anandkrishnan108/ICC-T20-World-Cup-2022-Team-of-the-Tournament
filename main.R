library(tidyverse)
library(dplyr)
#setwd("./Programming/Tutorials/R/T20 World Cup Super Team/t20_csv_files")
player_stats <- read.csv("dim_players.csv", header = T)
batter_stats <- read.csv("fact_batting_summary.csv", header = T)
bowler_stats <- read.csv("fact_bowling_summary.csv", header = T)

# BATSMEN
batter_columns = c("name",
                   "totalRuns",
                   "battingAvg",
                   "battingSR",
                   "innsBatted",
                   "boundaryPer",
                   "battingPos",
                   "avgBallsFaced")
new_batter_stats <- data.frame(matrix(ncol = length(batter_columns), nrow = 0))
batters <- unique(batter_stats[["batsmanName"]])
runs_totals <- aggregate(runs ~ batsmanName, data = batter_stats, FUN = sum)
balls_totals <- aggregate(balls ~ batsmanName, data = batter_stats, FUN = sum)
fours_totals <- aggregate(Fours ~ batsmanName, data = batter_stats, FUN = sum)
sixes_totals <- aggregate(Sixes ~ batsmanName, data = batter_stats, FUN = sum)
batPos_totals <- aggregate(battingPos ~ batsmanName, data = batter_stats, FUN = sum)

for (i in 1:nrow(runs_totals)) {
  batter <- runs_totals$batsmanName[i]
  total_runs <- runs_totals$runs[i]
  total_inns <- sum(batter_stats$batsmanName == batter)
  total_balls <- balls_totals$balls[i]
  boundary_runs <- fours_totals$Fours[i]*4 + sixes_totals$Sixes[i]*6
  num_outs <- sum(batter_stats$batsmanName == batter & batter_stats$out.not_out == "out")
  batting_pos_total <- batPos_totals$battingPos[i]
  if (num_outs == 0) {
    num_outs <- 1
  }
  new_row <- data.frame(
    #name = gsub('[(c)]','',batter),
    name = batter,
    totalRuns = total_runs,
    battingAvg = round(total_runs/num_outs, digits = 2),
    battingSR = round(total_runs/total_balls*100, digits = 0),
    innsBatted = total_inns,
    boundaryPer = round(boundary_runs/total_runs*100, digits = 2),
    battingPos = ceiling(batting_pos_total/total_inns),
    avgBallsFaced = round(total_balls/total_inns, digits = 0)
  )
  new_batter_stats <- rbind(new_batter_stats, new_row)
  #print(paste("Batting Position for", batter, ":", ceiling(batting_pos_total/total_inns)))
}

# BOWLERS
bowler_columns = c("name",
                   "totalWickets",
                   "economy",
                   "bowlingAvg",
                   "bowlingSR",
                   "innsBowled",
                   "runsConceded",
                   "ballsBowled",
                   "dotBallPer")
new_bowler_stats <- data.frame(matrix(ncol = length(bowler_columns), nrow = 0))
bowlers <- unique(bowler_stats[["bowlerName"]])
wickets_totals <- aggregate(wickets ~ bowlerName, data = bowler_stats, FUN = sum)
overs_totals <- aggregate(overs ~ bowlerName, data = bowler_stats, FUN = sum)
runs_conceded_totals <- aggregate(runs ~ bowlerName, data = bowler_stats, FUN = sum)
dots_total = aggregate(dots ~ bowlerName, data = bowler_stats, FUN = sum)
for (i in 1:nrow(overs_totals)) {
  bowler <- overs_totals$bowlerName[i]
  total_wickets <- wickets_totals$wickets[i]
  total_runs_conceded <- runs_conceded_totals$runs[i]
  total_inns <- sum(bowler_stats$bowlerName == bowler)
  total_dots <- dots_total$dots[i]
  total_overs <- as.character(overs_totals$overs[i])
  total_balls <- 0
  if(grepl(".", total_overs, fixed = T)) {
    split = strsplit(total_overs, split = "\\.")
    total_balls <- as.double(split[[1]][1]) * 6 + as.double(split[[1]][2])
  } else {
    total_balls <- as.double(total_overs) * 6
  }
  bowling_sr = round(total_balls/total_wickets, digits=0)
  if (total_wickets == 0) {
    bowling_sr <- NA
  }
  new_row <- data.frame(
    #name = gsub('[(c)]','', bowler),
    name = bowler,
    totalWickets = total_wickets,
    economy = round(total_runs_conceded/(total_balls/6), digits = 2),
    bowlingAvg = round(total_runs_conceded/total_wickets, digits=2),
    bowlingSR = bowling_sr,
    innsBowled = total_inns,
    runsConceded = total_runs_conceded,
    ballsBowled = total_balls,
    dotBallPer = round(total_dots/total_balls*100, digits = 0)
  )
  new_bowler_stats <- rbind(new_bowler_stats, new_row)
  #print(paste("Bowling SR ", bowler, ":", bowling_sr))
}

combined <- left_join(new_batter_stats, new_bowler_stats, by='name')
final <- left_join(combined, player_stats, by='name')
final <- distinct(final)
final$bowlingStyle <- tolower(final$bowlingStyle)

openers <- final %>% 
  filter(battingAvg > 30, battingSR > 140, innsBatted > 3, boundaryPer > 50, battingPos < 3) %>% 
  arrange(desc(battingAvg)) %>% 
  select(name, team, totalRuns, battingAvg, battingSR, innsBatted, boundaryPer, battingPos, avgBallsFaced)

anchors <- final %>% 
  filter(battingAvg > 40, battingSR > 125, innsBatted > 3, avgBallsFaced > 20, battingPos > 2) %>% 
  arrange(desc(battingAvg)) %>% 
  select(name, team, totalRuns, battingAvg, battingSR, innsBatted, boundaryPer, battingPos, avgBallsFaced)

all_rounders <- final %>% 
  filter(battingAvg > 15, battingSR > 140, innsBatted > 2, battingPos > 4, economy < 7, bowlingSR < 20) %>% 
  arrange(desc(battingSR))

fast_bowlers <- final %>% 
  filter(innsBowled > 4, economy < 7, bowlingSR < 16, grepl("fast", bowlingStyle, fixed = T), bowlingAvg < 20, dotBallPer > 40) %>% 
  arrange(bowlingAvg) %>% 
  select(name, team, totalWickets, economy, bowlingAvg, bowlingSR, innsBowled, runsConceded, ballsBowled, dotBallPer)

mystery_spinner <- final %>% 
  filter(innsBowled > 4, economy < 7, bowlingSR < 16, grepl("legbreak", bowlingStyle, fixed = T), bowlingAvg < 20) %>% 
  arrange(bowlingAvg) %>% 
  select(name, team, totalWickets, economy, bowlingAvg, bowlingSR, innsBowled, runsConceded, ballsBowled, dotBallPer)

write.csv(final, "final_player_data.csv")

playing11 <- final %>% 
  filter(name == "Jos Buttler(c)" | name == "Alex Hales" | name == "Virat Kohli" |
           name == "Suryakumar Yadav" | name == "Daryl Mitchell" | name == "Sam Curran" |
           name == "Rashid Khan" | name == "Mitchell Santner" | name == "Wanindu Hasaranga de Silva" | name == "Shaheen Shah Afridi" | name == "Anrich Nortje") %>% 
  
  mutate(battingPos = ifelse(name == "Daryl Mitchell",5,battingPos)) %>% 
  mutate(battingPos = ifelse(name == "Mitchell Santner",8,battingPos)) %>% 
  mutate(battingPos = ifelse(name == "Sam Curran",6,battingPos)) %>% 
  mutate(battingPos = ifelse(name == "Rashid Khan",7,battingPos)) %>% 
  mutate(battingPos = ifelse(name == "Wanindu Hasaranga de Silva",9,battingPos)) %>% 
  mutate(battingPos = ifelse(name == "Shaheen Shah Afridi",10,battingPos)) %>% 
  mutate(battingPos = ifelse(name == "Anrich Nortje",11,battingPos)) %>% 
  mutate(role = "NA") %>% 
  mutate(role = ifelse(name == "Jos Buttler(c)" | name == "Alex Hales","Opener", role)) %>% 
  mutate(role = ifelse(name == "Virat Kohli" | name == "Suryakumar Yadav" | name == "Daryl Mitchell","Anchor", role)) %>% 
  mutate(role = ifelse(name == "Rashid Khan" | name == "Sam Curran" | name == "Mitchell Santner","All Rounder", role)) %>% 
  mutate(role = ifelse(name == "Shaheen Shah Afridi" | name == "Anrich Nortje","Fast Bowler", role)) %>% 
  mutate(role = ifelse(name == "Wanindu Hasaranga de Silva","Legspinner", role)) %>% 
  
  arrange(battingPos) %>% 
  select(image, name, role, team, description)

write.csv(playing11, "playing11.csv")
