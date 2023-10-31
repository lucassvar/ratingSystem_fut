library(worldfootballR)
library(dplyr)

# Shooting Logs - Players ------------------------------------------------------

# Convert to numeric and add two new metrics
sh_logs <- sh_logs %>%
  mutate(xG = as.numeric(xG), PSxG = as.numeric(PSxG),
         Gls_minus_xG = case_when(Outcome == "Goal" ~ 1-xG,
                                  Outcome != "Goal" ~ 0-xG),
         PSxG_minus_xG = case_when(is.na(PSxG) ~ 0-xG,
                                   !is.na(PSxG) ~ PSxG-xG))


# Function to extract the stats per shot for a Player
extract_ply_sh_logs <- function(player_selected = "Erling Haaland", teams_selected = "Manchester City"){
  player_selected <- c(player_selected, paste(player_selected, "(pen)"))
  
  for (ply in player_selected) {
    for (team in teams_selected) {
      df <- sh_logs %>%
        filter(Player == ply, Squad %in% team) %>%
        summarise(
          Player = ply,
          Squad = team,
          xG_Total = sum(xG, na.rm = T),
          PSxG_Total = sum(PSxG, na.rm = T),
          Gls_minus_xG_Total = sum(Gls_minus_xG),
          PSxG_minus_xG_Total = sum(PSxG_minus_xG),
          xG_perSh = round(mean(xG), 3),
          PSxG_perSh = round(mean(PSxG, na.rm = T), 3),
          Gls_minus_xG_perSh = round(mean(Gls_minus_xG), 3),
          PSxG_minus_xG_perSh = round(mean(PSxG_minus_xG), 3),
          Total = n(),
          Off_Target = sum(Outcome == "Off Target"),
          Woodwork = sum(Outcome == "Woodwork"),
          Blocked = sum(Outcome == "Blocked"),
          Saved = sum(Outcome == "Saved"),
          Goal = sum(Outcome == "Goal"),
          Saved_off_Target = sum(Outcome == "Saved off Target"),
          Off_Target_xG = round(mean(xG[Outcome == "Off Target"]), 3),
          Woodwork_xG = round(mean(xG[Outcome == "Woodwork"]), 3),
          Blocked_xG = round(mean(xG[Outcome == "Blocked"]), 3),
          Saved_xG = round(mean(xG[Outcome == "Saved"]), 3),
          Goal_xG = round(mean(xG[Outcome == "Goal"]), 3),
          Saved_off_Target_xG = round(mean(xG[Outcome == "Saved_off_Target"]), 3)
        )
      if (grep(team, teams_selected)) {
        player_sh_logs <- df
      } else {
        player_sh_logs <- rbind(player_sh_logs, df[1, ])
      }
    }
  }
  
  return(player_sh_logs)
}


glimpse(extract_ply_sh_logs("Raheem Sterling", teams_selected = c("Manchester City", "Chelsea")))


npxG_total = round(mean(xG[Outcome == "Goal"]), 3)




sh_logs %>% filter(Date > "2023-07-11") %>%
  select(Squad, xG, PSxG, Gls_minus_xG, PSxG_minus_xG) %>%
  group_by(Squad) %>%
  summarise(
    xG_Total = sum(xG, na.rm = TRUE),
    PSxG_Total = sum(PSxG, na.rm = TRUE),
    Gls_minus_xG_Total = sum(Gls_minus_xG),
    PSxG_minus_xG_Total = sum(PSxG_minus_xG),
    xG_perSh = round(mean(xG, na.rm = TRUE), 3),
    PSxG_perSh = round(mean(PSxG, na.rm = TRUE), 3),
    Gls_minus_xG_perSh = round(mean(Gls_minus_xG), 3),
    PSxG_minus_xG_perSh = round(mean(PSxG_minus_xG), 3),
    Total = n()
  )


sh_logs %>% filter(Date > "2023-07-11") %>%
  select(Player, Squad, xG, PSxG, Gls_minus_xG, PSxG_minus_xG) %>%
  group_by(Player) %>%
  summarise(
    xG_Total = sum(xG, na.rm = TRUE),
    PSxG_Total = sum(PSxG, na.rm = TRUE),
    Gls_minus_xG_Total = sum(Gls_minus_xG),
    PSxG_minus_xG_Total = sum(PSxG_minus_xG),
    xG_perSh = round(mean(xG, na.rm = TRUE), 3),
    PSxG_perSh = round(mean(PSxG, na.rm = TRUE), 3),
    Gls_minus_xG_perSh = round(mean(Gls_minus_xG), 3),
    PSxG_minus_xG_perSh = round(mean(PSxG_minus_xG), 3),
    Total = n()
  )


# Match Logs ---------

ply_ML_extraction <- function(ply_selected, ply_date_range, ply_positions, ply_leagues, comp_pool_sex, compPool_date_range, compPool_ply_leagues){
  # Load data
  load("rda/playersMatchLogs.rda")
  
  # Divide position (Pos) column by 4 (primary position to quaternary position)
  playersMatchLogs[c("Pos_1", "Pos_2", "Pos_3", "Pos_4")] <- t(sapply(strsplit(playersMatchLogs$Pos, ","), function(x) c(x, rep(NA, 4 - length(x)))))
  
  # Selected stats for "per min" and for mean calculation
  perMin_stats <- {c("Cmp_Total", "Att_Total", "TotDist_Total", "PrgDist_Total", "Cmp_Short", "Att_Short",
                     "Cmp_Medium", "Att_Medium", "Cmp_Long", "Att_Long", "Ast", "xAG", "xA", "KP", "Final_Third", "PPA", "CrsPA", "PrgP",
                     "Att", "Live_Pass_Types", "Dead_Pass_Types", "FK_Pass_Types", "TB_Pass_Types", "Sw_Pass_Types", "Crs_Pass_Types",
                     "TI_Pass_Types", "CK_Pass_Types", "In_Corner_Kicks", "Out_Corner_Kicks", "Str_Corner_Kicks", "Cmp_Outcomes", "Off_Outcomes",
                     "Blocks_Outcomes", "Tkl_Tackles", "TklW_Tackles", "Def 3rd_Tackles", "Mid 3rd_Tackles", "Att 3rd_Tackles", "Tkl_Challenges",
                     "Att_Challenges", "Lost_Challenges", "Blocks_Blocks", "Sh_Blocks", "Pass_Blocks", "Int.x", "Tkl+Int", "Clr", "Err",
                     "Touches_Touches", "Def Pen_Touches", "Def 3rd_Touches", "Mid 3rd_Touches", "Att 3rd_Touches", "Att Pen_Touches",
                     "Live_Touches", "Att_Take_Ons", "Succ_Take_Ons", "Tkld_Take_Ons", "Carries_Carries", "TotDist_Carries", "PrgDist_Carries", "PrgC_Carries",
                     "Final_Third_Carries", "CPA_Carries", "Mis_Carries", "Dis_Carries", "Rec_Receiving", "PrgR_Receiving", "CrdY", "CrdR",
                     "2CrdY", "Fls", "Fld", "Off", "Crs", "PKwon", "PKcon", "OG", "Recov", "Won_Aerial_Duels", "Lost_Aerial_Duels")}
  mean_stats <- {c(perMin_stats,
                   "Cmp_percent_Total", "Cmp_percent_Short", "Cmp_percent_Medium", "Cmp_percent_Long",
                   "Tkl_percent_Challenges", "Succ_percent_Take_Ons", "Tkld_percent_Take_Ons",
                   "Won_percent_Aerial_Duels")}
  
  # Convert columns to numeric
  playersMatchLogs[c(mean_stats, "Min")] <- lapply(playersMatchLogs[c(mean_stats, "Min")], as.numeric)
  
  # Add attempted aerials as a column
  playersMatchLogs <- playersMatchLogs %>% mutate(Att_Aerials = Won_Aerial_Duels + Lost_Aerial_Duels)
  
  # Filter PLAYER match logs based on arguments
  playerML <- playersMatchLogs %>% filter(
    Player == ply_selected,
    Pos_1 %in% ply_positions,
    Match_Date >= ply_date_range[1] & Match_Date <= ply_date_range[2],
    League %in% ply_leagues
  )
  playerML <- playerML %>% select(-League)
  
  # Filter COMP. POOL match logs based on arguments
  compPoolML <- playersMatchLogs %>% filter(
    Player != ply_selected,
    Pos_1 %in% ply_positions,
    Match_Date >= compPool_date_range[1] & Match_Date <= compPool_date_range[2],
    League %in% compPool_ply_leagues,
    Sex %in% comp_pool_sex
  )
  
  # Group by and calculate p90s stats
  playerML <- playerML %>%
    mutate_at(.vars = perMin_stats, .funs = list(~./Min)) %>%
    mutate_at(.vars = perMin_stats, .funs = list(~ . * 90))
  compPoolML <- compPoolML %>%
    mutate_at(.vars = perMin_stats, .funs = list(~./Min)) %>%
    mutate_at(.vars = perMin_stats, .funs = list(~ . * 90))
  
  # Group by position and calculate the mean for each stat
  playerML <- as.data.frame({playerML %>%
      group_by(Player, Team, Pos_1) %>%
      summarise(across(where(is.numeric), mean, na.rm = TRUE))
  })
  
  # For loop to calculate z-scores for each position the player has played (compared to the same in the comp. pool)
  player_zscores <- data.frame()
  for (ply_pos in playerML$Pos_1) {
    filtered_compPool <- compPoolML %>% filter(Pos_1 == ply_pos)
    filtered_player <- playerML %>% filter(Pos_1 == ply_pos)
    
    pos_df <- data.frame(
      Player = filtered_player$Player,
      Team = filtered_player$Team,
      Pos = filtered_player$Pos_1,
      ProgPassDist = (filtered_player$PrgDist_Total - mean(filtered_compPool$PrgDist_Total, na.rm = T))/sd(filtered_compPool$PrgDist_Total, na.rm = T),
      ShortPasses = ((filtered_player$Att_Short - mean(filtered_compPool$Att_Short, na.rm = T))/sd(filtered_compPool$Att_Short, na.rm = T)) * 0.3 +
        ((filtered_player$Cmp_percent_Short - mean(filtered_compPool$Cmp_percent_Short, na.rm = T))/sd(filtered_compPool$Cmp_percent_Short, na.rm = T)) * 0.7,
      MediumPasses = ((filtered_player$Att_Medium - mean(filtered_compPool$Att_Medium, na.rm = T))/sd(filtered_compPool$Att_Medium, na.rm = T)) * 0.3 +
        ((filtered_player$Cmp_percent_Medium - mean(filtered_compPool$Cmp_percent_Medium, na.rm = T))/sd(filtered_compPool$Cmp_percent_Medium, na.rm = T)) * 0.7,
      LongPasses = ((filtered_player$Att_Long - mean(filtered_compPool$Att_Long, na.rm = T))/sd(filtered_compPool$Att_Long, na.rm = T)) * 0.3 +
        ((filtered_player$Cmp_percent_Long - mean(filtered_compPool$Cmp_percent_Long, na.rm = T))/sd(filtered_compPool$Cmp_percent_Long, na.rm = T)) * 0.7,
      ProgCarriesDist = (filtered_player$PrgDist_Carries - mean(filtered_compPool$PrgDist_Carries, na.rm = T))/sd(filtered_compPool$PrgDist_Carries, na.rm = T),
      xAG = (filtered_player$xAG - mean(filtered_compPool$xAG, na.rm = T))/sd(filtered_compPool$xAG, na.rm = T),
      PPA = (filtered_player$PPA - mean(filtered_compPool$PPA, na.rm = T))/sd(filtered_compPool$PPA, na.rm = T),
      Tackles = (filtered_player$Tkl_Tackles - mean(filtered_compPool$Tkl_Tackles, na.rm = T))/sd(filtered_compPool$Tkl_Tackles, na.rm = T),
      Challenges_Tkld = (filtered_player$Tkl_Challenges - mean(filtered_compPool$Tkl_Challenges, na.rm = T))/sd(filtered_compPool$Tkl_Challenges, na.rm = T),
      Blocks = (filtered_player$Blocks_Blocks - mean(filtered_compPool$Blocks_Blocks, na.rm = T))/sd(filtered_compPool$Blocks_Blocks, na.rm = T),
      Int = (filtered_player$Int.x - mean(filtered_compPool$Int.x, na.rm = T))/sd(filtered_compPool$Int.x, na.rm = T),
      TakeOns = ((filtered_player$Att_Take_Ons - mean(filtered_compPool$Att_Take_Ons, na.rm = T))/sd(filtered_compPool$Att_Take_Ons, na.rm = T)) * 0.7 +
        ((filtered_player$Succ_percent_Take_Ons - mean(filtered_compPool$Succ_percent_Take_Ons, na.rm = T))/sd(filtered_compPool$Succ_percent_Take_Ons, na.rm = T)) * 0.3,
      CPA = (filtered_player$CPA_Carries - mean(filtered_compPool$CPA_Carries, na.rm = T))/sd(filtered_compPool$CPA_Carries, na.rm = T),
      Received = (filtered_player$Rec_Receiving - mean(filtered_compPool$Rec_Receiving, na.rm = T))/sd(filtered_compPool$Rec_Receiving, na.rm = T),
      ProgReceived = (filtered_player$PrgR_Receiving - mean(filtered_compPool$PrgR_Receiving, na.rm = T))/sd(filtered_compPool$PrgR_Receiving, na.rm = T),
      LostPossession = ((mean(filtered_compPool$Mis_Carries, na.rm = T) - filtered_player$Mis_Carries)/sd(filtered_compPool$Mis_Carries, na.rm = T)) * 0.6 +
        ((mean(filtered_compPool$Dis_Carries, na.rm = T) - filtered_player$Dis_Carries)/sd(filtered_compPool$Dis_Carries, na.rm = T)) * 0.4,
      AerialDuels = ((filtered_player$Att_Aerials - mean(filtered_compPool$Att_Aerials, na.rm = T))/sd(filtered_compPool$Att_Aerials, na.rm = T)) * 0.3 +
        ((filtered_player$Won_percent_Aerial_Duels - mean(filtered_compPool$Won_percent_Aerial_Duels, na.rm = T))/sd(filtered_compPool$Won_percent_Aerial_Duels, na.rm = T)) * 0.7
    )
    
    player_zscores <- bind_rows(player_zscores, as.data.frame(pos_df) %>%
                                  mutate_all(~ifelse(is.numeric(.), round(., 2), .))
    )
  }
  
  # Extract goalkeepering data for the player in case he is a goalkeeper
  if (ply_pos == "GK") {
    # Load data
    load("rda/ply_keeper.rda")
    
    # Column for the rival xG
    ply_keeper <- ply_keeper %>% mutate(RivalxG = case_when(Home_Away == "Home" ~ Away_xG,
                                                            Home_Away == "Away" ~ Home_xG),
                                        PSxG_minus_xG = PSxG_Shot_Stopping - RivalxG,
                                        Gls_minus_PSxG = GA_Shot_Stopping - PSxG_Shot_Stopping)
    
    # Selected stats for "per min" and for mean calculation
    perMin_stats_keeper <- {c("SoTA_Shot_Stopping", "GA_Shot_Stopping", "Saves_Shot_Stopping",
                              "PSxG_Shot_Stopping", "Cmp_Launched", "Att_Launched", "Att_Passes", "Thr_Passes",
                              "AvgLen_Passes", "Att_Goal_Kicks", "AvgLen_Goal_Kicks", "Opp_Crosses", "Stp_Crosses",
                              "Player_NumOPA_Sweeper", "AvgDist_Sweeper", "RivalxG", "PSxG_minus_xG", "Gls_minus_PSxG")}
    mean_stats_keeper <- {c(perMin_stats_keeper,
                            "Save_percent_Shot_Stopping", "Cmp_percent_Launched", "Launch_percent_Passes", "Launch_percent_Goal_Kicks",
                            "Stp_percent_Crosses")}
    
    # Create ML df for PLAYER and COMP. POOL
    keeperML <- ply_keeper %>% filter(
      Player == ply_selected,
      Match_Date >= ply_date_range[1] & Match_Date <= ply_date_range[2],
      League %in% ply_leagues
    )
    compPool_keeperML <- ply_keeper %>% filter(
      Player != ply_selected,
      Match_Date >= compPool_date_range[1] & Match_Date <= compPool_date_range[2],
      League %in% compPool_ply_leagues,
      Sex %in% comp_pool_sex
    )
    
    # Group by and calculate p90s stats for both PLAYER and COMP. POOL
    keeperML <- keeperML %>%
      mutate_at(.vars = perMin_stats_keeper, .funs = list(~./Min)) %>%
      mutate_at(.vars = perMin_stats_keeper, .funs = list(~ . * 90))
    compPool_keeperML <- compPool_keeperML %>%
      mutate_at(.vars = perMin_stats_keeper, .funs = list(~./Min)) %>%
      mutate_at(.vars = perMin_stats_keeper, .funs = list(~ . * 90))
    
    # Group by Player and Team and calculate the mean for each stat
    keeperML <- as.data.frame({keeperML %>%
        group_by(Player, Team) %>%
        summarise(across(where(is.numeric), mean, na.rm = TRUE))
    })
    
    # Create z-scores data frame for the goalkeeper stats, then bind
    goalkeeper_zscores <- keeperML %>% summarise(
      Player = Player,
      Team = Team,
      Pos = "GK",
      xG_PSxG_Stopped = ((PSxG_minus_xG - mean(compPool_keeperML$PSxG_minus_xG, na.rm = T))/sd(compPool_keeperML$PSxG_minus_xG, na.rm = T)) * 0.5 +
        ((Gls_minus_PSxG - mean(compPool_keeperML$Gls_minus_PSxG, na.rm = T))/sd(compPool_keeperML$Gls_minus_PSxG, na.rm = T)) * 0.5,
      Saves = ((Save_percent_Shot_Stopping - mean(compPool_keeperML$Save_percent_Shot_Stopping, na.rm = T))/sd(compPool_keeperML$Save_percent_Shot_Stopping, na.rm = T)) * 0.7+
        ((SoTA_Shot_Stopping - mean(compPool_keeperML$SoTA_Shot_Stopping, na.rm = T))/sd(compPool_keeperML$SoTA_Shot_Stopping, na.rm = T)) * 0.3,
      Crosses_Stp = ((Stp_percent_Crosses - mean(compPool_keeperML$Stp_percent_Crosses,na.rm = T))/sd(compPool_keeperML$Stp_percent_Crosses,na.rm = T)) * 0.7+
        ((Opp_Crosses - mean(compPool_keeperML$Opp_Crosses, na.rm = T))/sd(compPool_keeperML$Opp_Crosses, na.rm = T)) * 0.3
    )
    
    player_zscores <- merge(player_zscores, as.data.frame(goalkeeper_zscores) %>%
                              mutate_all(~ifelse(is.numeric(.), round(., 2), .),
                                         by = c("Player", "Team", "Pos"))
    )
  }
  
  return(player_zscores)
}

test <- ply_ML_extraction("Ederson",
                          c("2023-08-01", "2023-11-01"),
                          c("GK"),
                          c("Premier League"),
                          c("M"),
                          c("2023-08-01", "2023-11-01"),
                          c("Premier League", "La Liga", "Fußball-Bundesliga", "Serie A", "Ligue 1"))


  
  
  
  
  