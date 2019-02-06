library(dplyr)
match_data = read.csv("file:///F:/Files/E0.csv",stringsAsFactors = FALSE)
#match_data
#############################################################################
matches <- select(match_data,HomeTeam,FTHG,FTAG,AwayTeam,FTR)
matches
matches <- matches[-248,]
matches
#opponents
#homeTeam <- "Leicester"
#awayTeam <- "Man United"
#opp1 <- "Man City"
#opp2 <- "Arsenal"
opp1 <- "Leicester"
opp2 <- "Man United"
filtered_matches <- filter(matches,((HomeTeam == opp1 & AwayTeam == opp2) | (HomeTeam == opp2 & AwayTeam == opp1)))
filtered_matches



#match_filtered <- filter(matches,(HomeTeam != opp1 & AwayTeam != opp2) | (HomeTeam != opp2 & AwayTeam != opp1))
#match_filtered
print(tbl_df(matches),n=nrow(matches))

      
fil_opp1_matches_home <- select(filter(filter(matches,HomeTeam == opp1 | AwayTeam == opp1),HomeTeam == opp1),HomeTeam,FTHG,FTAG,AwayTeam)
fil_opp1_matches_home
      
fil_opp2_matches_home <- select(filter(filter(matches,HomeTeam == opp2 | AwayTeam == opp2),HomeTeam == opp2),HomeTeam,FTHG,FTAG,AwayTeam)
fil_opp2_matches_home

fil_opp1_matches_away <- select(filter(filter(matches,AwayTeam == opp1),AwayTeam == opp1),HomeTeam,FTHG,FTAG,AwayTeam)
fil_opp1_matches_away

fil_opp2_matches_away <- select(filter(filter(matches,AwayTeam == opp2),AwayTeam == opp2),HomeTeam,FTHG,FTAG,AwayTeam)
fil_opp2_matches_away

total_goal_opp1_scored_at_home <- sum(fil_opp1_matches_home$FTHG,na.rm = TRUE)
total_goal_opp1_scored_at_home
total_goal_opp1_conceaded_at_home <- sum(fil_opp1_matches_home$FTAG,na.rm = TRUE)
total_goal_opp1_conceaded_at_home

total_goal_opp2_scored_at_home <- sum(fil_opp2_matches_home$FTHG,na.rm = TRUE)
total_goal_opp2_scored_at_home
total_goal_opp2_conceaded_at_home <- sum(fil_opp2_matches_home$FTAG,na.rm = TRUE)
total_goal_opp2_conceaded_at_home

total_goal_opp1_scored_at_away <- sum(fil_opp1_matches_away$FTAG,na.rm = TRUE)
total_goal_opp1_scored_at_away
total_goal_opp1_conceaded_at_away <- sum(fil_opp1_matches_away$FTHG,na.rm = TRUE)
total_goal_opp1_conceaded_at_away


total_goal_opp2_scored_at_away <- sum(fil_opp2_matches_away$FTAG,na.rm = TRUE)
total_goal_opp2_scored_at_away
total_goal_opp2_conceaded_at_away <- sum(fil_opp2_matches_away$FTHG,na.rm = TRUE)
total_goal_opp2_conceaded_at_away


#total goal scored by home team
total_goal_scored_at_home <- sum(match_data$FTHG,na.rm = TRUE)
total_goal_scored_at_home
#total goal scored by away team
total_goal_scored_by_away <- sum(match_data$FTAG,na.rm = TRUE)
total_goal_scored_by_away
# total number of goal scored per season
total_goal_scored_per_season <- total_goal_scored_by_away + total_goal_scored_at_home
#total number of game in season
total_game_game_played <- nrow(match_data) - 1
total_game_game_played
#Average number of goals scored at home
average_no_goals_scored_at_home <- total_goal_scored_at_home / total_game_game_played
average_no_goals_scored_at_home
#Average number of goals scored at home
average_no_goals_scored_by_away <- total_goal_scored_by_away / total_game_game_played
average_no_goals_scored_by_away
#print(tbl_df(match_data), n=380)
sprintf("Average number of goals scored at home %f",average_no_goals_scored_at_home)
sprintf("Average number of goals scored by away team %f",average_no_goals_scored_by_away)


#Average number of goals conceded at home
average_no_goals_conceaded_at_home <- average_no_goals_scored_by_away
#Average number of goals conceded away from home
average_no_goals_conceaded_at_away <- average_no_goals_scored_at_home

#Calculate home team Attack and diffence  Strength 

opp1_attack_strength <- (total_goal_opp1_scored_at_home/nrow(fil_opp1_matches_home)) / average_no_goals_scored_at_home
#opp1_attack_strength

opp2_diffence_strength <- (total_goal_opp2_conceaded_at_away/nrow(fil_opp2_matches_away)) / average_no_goals_conceaded_at_away
opp2_diffence_strength
average_no_of_goal_opp1_may_score <- opp1_attack_strength * opp2_diffence_strength * average_no_goals_scored_at_home
average_no_of_goal_opp1_may_score

opp1_diffence_strength <- (total_goal_opp1_conceaded_at_away/nrow(fil_opp1_matches_away)) / average_no_goals_conceaded_at_away
opp1_diffence_strength

opp2_attack_strength <- (total_goal_opp2_scored_at_home/nrow(fil_opp2_matches_home)) / average_no_goals_scored_at_home
opp2_attack_strength

average_no_of_goal_opp2_may_score <- opp2_attack_strength * opp1_diffence_strength * average_no_goals_scored_at_home
average_no_of_goal_opp2_may_score


lambda_one <- average_no_of_goal_opp1_may_score
lambda_two <- average_no_of_goal_opp2_may_score
HOME_TEAM <- c(dpois(0:6, lambda_one))
AWAY_TEAM <- c(dpois(0:6, lambda_two))

#HOME_TEAM %*% AWAY_TEAM

dpoison1 <- list(HOME_TEAM)
dpoison <- unlist(dpoison1,use.names=FALSE)
dpoison
dpoison_home <- matrix(dpoison,nrow = 1,ncol = 7)
#dpoison_home
dpoison_away <- matrix(AWAY_TEAM)
#dpoison_away
pred <- dpoison_away %*% dpoison_home
pred1 <- as.data.frame(pred)
pred1




