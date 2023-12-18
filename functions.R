library(worldfootballR)
library(dplyr)
library(stringr)
library(ggplot2)
library(forcats)

# Function to get the match URLs from all the leagues of the requested season (default is current season)
get_all_match_urls <- function(year = NA){
  # If no variables are given for either of the variables then it asumes it asks for current year
  if (is.na(year)) {
    year <- as.numeric(format(Sys.Date(), "%Y")) + (as.numeric(format(Sys.Date(), "%m")) > 7)
  }
  
  # Countries to extract data
  first_tier_countries <- c("ENG","ESP","GER","ITA","FRA","NED","POR","BEL", "ARG","BRA","MEX","USA")
  second_tier_countries <- c("ENG","ESP","GER","ITA","FRA")
  women_countries <- c("ENG", "ESP", "GER", "ITA", "FRA", "AUS", "USA")
  
  # Masculine Football
  match_urls_MASC <- c(
    fb_match_urls(first_tier_countries, "M", year, "1st"),
    fb_match_urls(second_tier_countries, "M", year, "2nd"),
    fb_match_urls("", "M", year, non_dom_league_url = "https://fbref.com/en/comps/14/history/Copa-Libertadores-Seasons"),
    fb_match_urls("", "M", year, non_dom_league_url = "https://fbref.com/en/comps/8/history/Champions-League-Seasons"),
    fb_match_urls("", "M", year, non_dom_league_url = "https://fbref.com/en/comps/19/history/Europa-League-Seasons"),
    fb_match_urls("", "M", year, non_dom_league_url = "https://fbref.com/en/comps/882/history/Europa-Conference-League-Seasons")
  )
  
  # Feminine Football
  match_urls_FEM <- c(
    fb_match_urls(women_countries, "F", year, "1st"),
    fb_match_urls("", "F", year, non_dom_league_url = "https://fbref.com/en/comps/181/history/Champions-League-Seasons")
  )
  
  # Save both groups of links in same variable
  all_match_urls <- list(match_urls_MASC, match_urls_FEM)
  
  return(all_match_urls)
}

# Function to extract the data from a specific year
fut_data_extraction <- function(year_sel = NA, links_sel = NA, links_examined = 1:20){
  start_time <- Sys.time()
  
  # Check if at least one of the arguments was stablished when calling
  if (missing(year_sel) && missing(links_sel)) {stop("At least one of the arguments 'year_sel' or 'links_sel' should be given.")}
  
  # Load links already used if there are any
  if (file.exists("rda/used_links.rda")) {load("rda/used_links.rda")} else {used_links <- c()}
  
  # Remove NA values from links variable
  all_match_URLs[1] <- na.omit(all_match_URLs[1])
  all_match_URLs[2] <- na.omit(all_match_URLs[2])
  
  # If there's no input of links then extract the ones from the year
  if (is.null(links_sel)) {links_sel <- get_all_match_urls(year = year_sel)}
  
  # Select links that haven't been used (add a stop to the limit of links examined if it's over the amount of links available)
  if (length(links_examined) > length(links_sel[[1]])) {
    masc_links <- setdiff(links_sel[[1]], used_links)
  } else {
    masc_links <- setdiff(links_sel[[1]], used_links)[links_examined]
  } # Male links
  if (length(links_examined) > length(links_sel[[2]])) {
    fem_links <- setdiff(links_sel[[2]], used_links)
  } else {
    fem_links <- setdiff(links_sel[[2]], used_links)[links_examined]
  } # Fem links
  
  # Stat types used to extract the match logs
  stat_types <- c("passing", "passing_types", "defense", "possession", "misc")
  
  # Empty data frames to save the data
  playersMatchLogs <- teamsMatchLogs <- c()
  
  
  # Extract shooting logs ----------
  print("Shooting Logs")
  fem_sh_logs <- masc_sh_logs <- data.frame()
  tryCatch({
    fem_sh_logs <- fb_match_shooting(fem_links) %>% mutate(Sex = "W")
    masc_sh_logs <- fb_match_shooting(masc_links) %>% mutate(Sex = "M")
  })
  
  # Check that neither of the data frames are empty before binding
  if (nrow(fem_sh_logs) == 0 && nrow(masc_sh_logs) == 0) {
    sh_logs <- NULL
  } else if (nrow(fem_sh_logs) == 0) {
    sh_logs <- masc_sh_logs
  } else if (nrow(masc_sh_logs) == 0) {
    sh_logs <- fem_sh_logs
  } else {
    sh_logs <- bind_rows(fem_sh_logs, masc_sh_logs)
  }
  
  
  # Convert columns to numeric and create new analysis columns (if sh_logs exists)
  if (exists("sh_logs")) {
    # Function to change the Minute column to numeric (handles "90+2", converts to 92)
    mins_to_numeric <- function(x) {
      if (grepl("\\+", x)) {
        parts <- strsplit(x, "\\+")
        return(as.numeric(parts[[1]][1]) + as.numeric(parts[[1]][2]))
      } else {
        return(as.numeric(x))
      }
    }
    sh_logs$Minute <- sapply(sh_logs$Minute, mins_to_numeric)
    sh_logs <- {sh_logs %>% mutate(
      xG = as.numeric(xG),
      PSxG = as.numeric(PSxG),
      Gls_over_xG = case_when(Outcome == "Goal" ~ 1 - xG,
                              Outcome != "Goal" ~ 0 - xG),
      PSxG_minus_xG = case_when(!is.na(PSxG) ~ PSxG - xG,
                                is.na(PSxG) ~ 0 - xG),
      Head_xG = case_when(`Body Part` == "Head" ~ xG,
                          TRUE ~ NA_real_),
      Head_PSxG = case_when(`Body Part` == "Head" ~ PSxG,
                            TRUE ~ NA_real_),
      Head_PSxG_minus_xG = case_when(`Body Part` == "Head" & !is.na(Head_PSxG) & !is.na(Head_xG) ~ Head_PSxG - Head_xG,
                                     `Body Part` == "Head" & is.na(Head_PSxG) & !is.na(Head_xG) ~ 0 - Head_xG,
                                     TRUE ~ NA_real_),
      Head_Gls_minus_xG = case_when(`Body Part` == "Head" & Outcome == "Goal" ~ 1-xG,
                                    `Body Part` == "Head" & Outcome != "Goal" ~ 0-xG,
                                    TRUE ~ NA_real_),
      
      RightF_xG = case_when(`Body Part` == "Right Foot" ~ xG,
                            TRUE ~ NA_real_),
      RightF_PSxG = case_when(`Body Part` == "Right Foot" ~ PSxG,
                              TRUE ~ NA_real_),
      RightF_PSxG_minus_xG = case_when(`Body Part` == "Right Foot" & !is.na(RightF_PSxG) & !is.na(RightF_xG) ~ RightF_PSxG - RightF_xG,
                                       `Body Part` == "Right Foot" & is.na(RightF_PSxG) & !is.na(RightF_xG) & Outcome == "Blocked" ~ 0 - RightF_xG,
                                       TRUE ~ NA_real_),
      RightF_Gls_minus_xG = case_when(`Body Part` == "Right Foot" & Outcome == "Goal" ~ 1-xG,
                                      `Body Part` == "Right Foot" & Outcome != "Goal" ~ 0-xG,
                                      TRUE ~ NA_real_),
      
      LeftF_xG = case_when(`Body Part` == "Left Foot" ~ xG,
                           TRUE ~ NA_real_),
      LeftF_PSxG = case_when(`Body Part` == "Left Foot" ~ PSxG,
                             TRUE ~ NA_real_),
      LeftF_PSxG_minus_xG = case_when(`Body Part` == "Left Foot" & !is.na(LeftF_PSxG) & !is.na(LeftF_xG) ~ LeftF_PSxG - LeftF_xG,
                                      `Body Part` == "Left Foot" & is.na(LeftF_PSxG) & !is.na(LeftF_xG) & Outcome == "Blocked" ~ 0 - LeftF_xG,
                                      TRUE ~ NA_real_),
      LeftF_Gls_minus_xG = case_when(`Body Part` == "Left Foot" & Outcome == "Goal" ~ 1-xG,
                                     `Body Part` == "Left Foot" & Outcome != "Goal" ~ 0-xG,
                                     TRUE ~ NA_real_),
      
      Foot_xG = case_when(`Body Part` == "Left Foot" | `Body Part` == "Right Foot" ~ xG,
                          TRUE ~ NA_real_),
      Foot_PSxG = case_when(`Body Part` == "Left Foot" | `Body Part` == "Right Foot" ~ PSxG,
                            TRUE ~ NA_real_),
      Foot_PSxG_minus_xG = case_when(`Body Part` == "Left Foot" & !is.na(LeftF_PSxG) & !is.na(LeftF_xG) ~ LeftF_PSxG - LeftF_xG,
                                     `Body Part` == "Left Foot" & is.na(LeftF_PSxG) & !is.na(LeftF_xG) & Outcome == "Blocked" ~ 0 - LeftF_xG,
                                     `Body Part` == "Right Foot" & !is.na(RightF_PSxG) & !is.na(RightF_xG) ~ RightF_PSxG - RightF_xG,
                                     `Body Part` == "Right Foot" & is.na(RightF_PSxG) & !is.na(RightF_xG) & Outcome == "Blocked" ~ 0 - RightF_xG,
                                     TRUE ~ NA_real_),
      Foot_Gls_minus_xG = case_when(`Body Part` == "Left Foot" & Outcome == "Goal" ~ 1-xG,
                                    `Body Part` == "Left Foot" & Outcome != "Goal" ~ 0-xG,
                                    `Body Part` == "Right Foot" & Outcome == "Goal" ~ 1-xG,
                                    `Body Part` == "Right Foot" & Outcome != "Goal" ~ 0-xG,
                                    TRUE ~ NA_real_)
    )}
    
    # Adapts teams names to match the match logs data frame teams names
    sh_logs <- sh_logs %>%
      mutate(Squad = gsub("Nott'ham Forest", "Nottingham Forest", Squad),
             Squad = gsub("\\bUtd\\b|\\bUtd\\.\\b", "United", Squad, ignore.case = TRUE))
  }
  
  # Extract player goalkeeping data ----------
  print("Player - Keeper")
  fem_ply_keeper <- masc_ply_keeper <- data.frame()
  tryCatch({
    fem_ply_keeper <- fb_advanced_match_stats(match_url = fem_links, stat_type = "keeper", team_or_player = "player") %>% mutate(Sex = "W")
    masc_ply_keeper <- fb_advanced_match_stats(match_url = masc_links, stat_type = "keeper", team_or_player = "player") %>% mutate(Sex = "M")
  })
  
  # Check that neither of the data frames are empty before binding
  if (nrow(fem_ply_keeper) == 0 && nrow(masc_ply_keeper) == 0) {
    ply_keeper <- NULL
  } else if (nrow(fem_ply_keeper) == 0) {
    ply_keeper <- masc_ply_keeper
  } else if (nrow(masc_ply_keeper) == 0) {
    ply_keeper <- fem_ply_keeper
  } else {
    ply_keeper <- bind_rows(fem_ply_keeper, masc_ply_keeper)
  }
  
  
  # Extract team goalkeeping data ----------
  print("Team - Keeper")
  fem_team_keeper <- masc_team_keeper <- data.frame()
  tryCatch({
    fem_team_keeper <- fb_advanced_match_stats(match_url = fem_links, stat_type = "keeper", team_or_player = "team") %>% mutate(Sex = "W")
    masc_team_keeper <- fb_advanced_match_stats(match_url = masc_links, stat_type = "keeper", team_or_player = "team") %>% mutate(Sex = "M")
  })
  
  # Check that neither of the data frames are empty before binding
  if (nrow(fem_team_keeper) == 0 && nrow(masc_team_keeper) == 0) {
    team_keeper <- NULL
  } else if (nrow(fem_team_keeper) == 0) {
    team_keeper <- masc_team_keeper
  } else if (nrow(masc_team_keeper) == 0) {
    team_keeper <- fem_team_keeper
  } else {
    team_keeper <- bind_rows(fem_team_keeper, masc_team_keeper)
  }
  
  
  # Extract player and team match logs -----------
  # For loop to extract the match logs from each stat type
  for (stat in stat_types) {
    # Player stats
    print(paste("Player -", stat))
    fem_playerML <- masc_playerML <- data.frame()
    tryCatch({
      fem_playerML <- fb_advanced_match_stats(match_url = fem_links,
                                              stat_type = stat,
                                              team_or_player = "player") %>% mutate(Sex = "W")
      masc_playerML <- fb_advanced_match_stats(match_url = masc_links,
                                               stat_type = stat,
                                               team_or_player = "player") %>% mutate(Sex = "M")
    })
    
    # Check that neither of the data frames are empty before binding
    if (nrow(fem_playerML) == 0 && nrow(masc_playerML) == 0) {
      playerML <- NULL
    } else if (nrow(fem_playerML) == 0) {
      playerML <- masc_playerML
    } else if (nrow(masc_playerML) == 0) {
      playerML <- fem_playerML
    } else {
      playerML <- bind_rows(fem_playerML, masc_playerML)
    }
    
    
    # Team stats
    print(paste("Team -", stat))
    fem_teamML <- masc_teamML <- data.frame()
    tryCatch({
      fem_teamML <- fb_advanced_match_stats(match_url = fem_links,
                                            stat_type = stat,
                                            team_or_player = "team") %>% mutate(Sex = "W")
      masc_teamML <- fb_advanced_match_stats(match_url = masc_links,
                                             stat_type = stat,
                                             team_or_player = "team") %>% mutate(Sex = "M")
    })
    
    # Check that neither of the data frames are empty before binding
    if (nrow(fem_teamML) == 0 && nrow(masc_teamML) == 0) {
      teamML <- NULL
    } else if (nrow(fem_teamML) == 0) {
      teamML <- masc_teamML
    } else if (nrow(masc_teamML) == 0) {
      teamML <- fem_teamML
    } else {
      teamML <- bind_rows(fem_teamML, masc_teamML)
    }
    
    
    # Merge the player data of the current stat with the previous ones (if exists)...
    if (!is.null(playerML)) {
      if (is.null(playersMatchLogs)) {
        playersMatchLogs <- playerML
      } else {
        playersMatchLogs <- merge(playersMatchLogs, playerML,
                                  by = c("League", "Match_Date", "Matchweek", "Home_Team", "Home_Formation",
                                         "Home_Score", "Home_xG", "Home_Goals", "Home_Yellow_Cards", "Home_Red_Cards",
                                         "Away_Team", "Away_Formation", "Away_Score", "Away_xG", "Away_Goals",
                                         "Away_Yellow_Cards", "Away_Red_Cards", "Game_URL", "Team", "Home_Away",
                                         "Player", "Nation"),
                                  all.x = T, all.y = T)
      }
    }
    
    # ...Now the same for team data
    if (!is.null(teamML)) {
      if (is.null(teamsMatchLogs)) {
        teamsMatchLogs <- teamML
      } else {
        teamsMatchLogs <- merge(teamsMatchLogs, teamML,
                                by = c("League", "Match_Date", "Matchweek", "Home_Team", "Home_Formation",
                                       "Home_Score", "Home_xG", "Home_Goals", "Home_Yellow_Cards", "Home_Red_Cards",
                                       "Away_Team", "Away_Formation", "Away_Score", "Away_xG", "Away_Goals",
                                       "Away_Yellow_Cards", "Away_Red_Cards", "Game_URL", "Team", "Home_Away"),
                                all.x = T, all.y = T)
      }
    }
  }
  
  
  # Now save the data frames in the rda folder (skip if the data frames were empty and bind if files exists)
  if (!is.null(sh_logs)) {
    if(file.exists("rda/sh_logs.rda")){
      new_sh_logs <- sh_logs
      load("rda/sh_logs.rda")
      sh_logs <- bind_rows(sh_logs, new_sh_logs)
    }
    save(sh_logs, file = "rda/sh_logs.rda")
  }
  if (!is.null(ply_keeper)) {
    if(file.exists("rda/ply_keeper.rda")){
      new_ply_keeper <- ply_keeper
      load("rda/ply_keeper.rda")
      ply_keeper <- bind_rows(ply_keeper, new_ply_keeper)
    }
    save(ply_keeper, file = "rda/ply_keeper.rda")
  }
  if (!is.null(team_keeper)) {
    if(file.exists("rda/team_keeper.rda")){
      new_team_keeper <- team_keeper
      load("rda/team_keeper.rda")
      team_keeper <- bind_rows(team_keeper, new_team_keeper)
    }
    save(team_keeper, file = "rda/team_keeper.rda")
  }
  if (!is.null(playersMatchLogs)) {
    if(file.exists("rda/playersMatchLogs.rda")){
      new_playersMatchLogs <- playersMatchLogs
      load("rda/playersMatchLogs.rda")
      playersMatchLogs <- bind_rows(playersMatchLogs, new_playersMatchLogs)
    }
    save(playersMatchLogs, file = "rda/playersMatchLogs.rda")
  }
  if (!is.null(teamsMatchLogs)) {
    if(file.exists("rda/teamsMatchLogs.rda")){
      new_teamsMatchLogs <- teamsMatchLogs
      load("rda/teamsMatchLogs.rda")
      teamsMatchLogs <- bind_rows(teamsMatchLogs, new_teamsMatchLogs)
    }
    save(teamsMatchLogs, file = "rda/teamsMatchLogs.rda")
  }
  
  # Add the links extracted to the used data frames and save it
  used_links <- unique(c(used_links, masc_links, fem_links))
  save(used_links, file = "rda/used_links.rda")
  
  
  # Print total running time
  Sys.time() - start_time
}

# Get new links
update_links <- function(new_year){
  start_time <- Sys.time()
  load("rda/all_match_URLs.rda")
  new_links <- get_all_match_urls(year = new_year)
  all_match_URLs[[1]] <- unique(c(all_match_URLs[[1]], new_links[[1]]))
  all_match_URLs[[2]] <- unique(c(all_match_URLs[[2]], new_links[[2]]))
  save(all_match_URLs, file = "rda/all_match_URLs.rda")
  Sys.time() - start_time
}

# Extract Match Logs Z-Scores for selected player
plyML_zscores <- function(ply_selected, ply_team, ply_exclude_mins = 15, ply_date_range, ply_positions, ply_leagues, comp_pool_exclude_mins = 15, comp_pool_sex, compPool_date_range, compPool_ply_leagues, predicted_or_total = "total", pos_or_role = "role"){
  # Load data
  load("rda/playersMatchLogs.rda")
  load("rda/sh_logs.rda")
  
  # Save the original dplyr.summarise.inform and deactivate it
  original_inform <- getOption("dplyr.summarise.inform")
  options(dplyr.summarise.inform = FALSE)
  
  # Divide position (Pos) column by 4 (primary position to quaternary position)
  playersMatchLogs[c("Pos_1", "Pos_2", "Pos_3", "Pos_4")] <- t(sapply(strsplit(playersMatchLogs$Pos, ","), function(x) c(x, rep(NA, 4 - length(x)))))
  
  # Create "Role" column and assign it to the Pos_1 column in the playersMatchLogs data frame
  if (pos_or_role == "role") {
    playersMatchLogs <- playersMatchLogs %>%
      filter(!is.na(Pos_1)) %>%
      mutate(
        Role = case_when(
          Pos_1 == "LW" | Pos_1 == "RW" | Pos_1 == "LM" | Pos_1 == "RM" ~ "W",
          Pos_1 == "AM" | Pos_1 == "CM" ~ "CM",
          Pos_1 == "DM" ~ "DM",
          Pos_1 == "CB" ~ "CB",
          Pos_1 == "FW" ~ "FW",
          Pos_1 == "WB" | Pos_1 == "LB" | Pos_1 == "RB" ~ "FB",
          Pos_1 == "GK" ~ "GK",
          TRUE ~ NA_character_
        )
      )
    playersMatchLogs$Pos_1 <- playersMatchLogs$Role
  }
  
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
  
  # Create ID columns to identify and filter matches (specially in Comp Pool)
  playersMatchLogs <- playersMatchLogs %>%
    mutate(ID = paste(Team, Match_Date, Player, sep = "_"))
  sh_logs <- sh_logs %>%
    mutate(ID = paste(Squad, Date, Player, sep = "_"))
  
  # Filter PLAYER match logs based on arguments
  playerML <- playersMatchLogs %>% filter(
    Player == ply_selected,
    Pos_1 %in% ply_positions,
    Team %in% ply_team,
    Match_Date >= ply_date_range[1] & Match_Date <= ply_date_range[2],
    League %in% ply_leagues,
    Min >= ply_exclude_mins
  )
  playerML <- playerML %>% select(-League)
  
  # Filter COMP. POOL match logs based on arguments
  compPoolML <- playersMatchLogs %>% filter(
    Player != ply_selected,
    Pos_1 %in% ply_positions,
    Match_Date >= compPool_date_range[1] & Match_Date <= compPool_date_range[2],
    League %in% compPool_ply_leagues,
    Sex %in% comp_pool_sex,
    Min >= comp_pool_exclude_mins
  )

  # Create the sh_logs data frames for both player and Comp Pool
  ply_shoot <- sh_logs %>% filter(ID %in% playerML$ID)
  compPool_shoot <- sh_logs %>% filter(ID %in% compPoolML$ID)
  
  # Execute the z-score calculation for shooting stats only if there are shots available
  if (nrow(ply_shoot) > 0 & nrow(compPool_shoot) > 0) {
    # Add a Pos_1 column based on ID column between ML and SL data frames
    ply_shoot <- merge(ply_shoot, playerML[, c("ID", "Pos_1")], by = "ID", all.x = TRUE)
    compPool_shoot <- merge(compPool_shoot, compPoolML[, c("ID", "Pos_1")], by = "ID", all.x = TRUE)
    
    # Shots per 90s for PLAYER
    ply_sh_per90 <- {as.data.frame(merge(sh_logs %>% filter(ID %in% playerML$ID), playerML[, c("ID", "Pos_1", "Min")], by = "ID", all.x = TRUE) %>%
                                     filter(Min >= 15) %>%
                                     group_by(Player, Squad, Date, Pos_1, `Body Part`) %>%
                                     summarise(
                                       Min = mean(Min, na.rm = T),
                                       Shots = n()
                                     ) %>%
                                     mutate(Shots_per_90 = Shots / Min) %>% 
                                     group_by(Player, Pos_1, `Body Part`) %>% 
                                     summarize(Shots_per_90_mean = mean(Shots_per_90, na.rm = TRUE) * 90))}
    
    # Shots per 90s for COMP.POOL
    compPool_sh_per90 <- {as.data.frame(merge(sh_logs %>% filter(ID %in% compPoolML$ID), compPoolML[, c("ID", "Pos_1", "Min")], by = "ID", all.x = TRUE) %>%
                                          filter(Min >= 15) %>%
                                          group_by(Player, Squad, Date, Pos_1, `Body Part`) %>%
                                          summarise(
                                            Min = mean(Min, na.rm = T),
                                            Shots = n()
                                          ) %>%
                                          mutate(Shots_per_90 = Shots / Min) %>% 
                                          group_by(Player, Pos_1, `Body Part`) %>% 
                                          summarize(Shots_per_90_mean = mean(Shots_per_90, na.rm = TRUE) * 90))}
    
    # Create Mean columns for each player 
    ply_shoot <- {as.data.frame(ply_shoot %>%
                                  select(Squad, Player, Pos_1, xG, PSxG, Gls_over_xG, PSxG_minus_xG, Head_xG, Head_PSxG, Head_PSxG_minus_xG, Head_Gls_minus_xG,
                                         RightF_xG, RightF_PSxG, RightF_PSxG_minus_xG, RightF_Gls_minus_xG, LeftF_xG, LeftF_PSxG,
                                         LeftF_PSxG_minus_xG, LeftF_Gls_minus_xG, Foot_xG, Foot_PSxG, Foot_PSxG_minus_xG, Foot_Gls_minus_xG) %>%
                                  group_by(Squad, Player, Pos_1) %>%
                                  summarize(across(.fns = list(mean = ~mean(., na.rm = TRUE)),
                                                   .names = "{.col}_mean_{.fn}")))}
    compPool_shoot <- {as.data.frame(compPool_shoot %>%
                                       select(Squad, Player, Pos_1, xG, PSxG, Gls_over_xG, PSxG_minus_xG, Head_xG, Head_PSxG, Head_PSxG_minus_xG, Head_Gls_minus_xG,
                                              RightF_xG, RightF_PSxG, RightF_PSxG_minus_xG, RightF_Gls_minus_xG, LeftF_xG, LeftF_PSxG,
                                              LeftF_PSxG_minus_xG, LeftF_Gls_minus_xG, Foot_xG, Foot_PSxG, Foot_PSxG_minus_xG, Foot_Gls_minus_xG) %>%
                                       group_by(Squad, Player, Pos_1) %>%
                                       summarize(across(.fns = list(mean = ~mean(., na.rm = TRUE)),
                                                        .names = "{.col}_mean_{.fn}")))}
    
    # For loop to calculate the z-scores
    shooting_zscores <- data.frame()
    for (ply_pos_sh in ply_shoot$Pos_1) {
      player_shoot <- ply_shoot %>% filter(Pos_1 == ply_pos_sh)
      cp_shoot <- compPool_shoot %>% filter(Pos_1 == ply_pos_sh)
      per_90_player <- ply_sh_per90 %>% filter(Pos_1 == ply_pos_sh)
      per_90_compPool <- compPool_sh_per90 %>% filter(Pos_1 == ply_pos_sh)
      
      # Create shooting z-scores df
      plySH_z <- {
        data.frame(
          Player = player_shoot$Player,
          Team = player_shoot$Squad,
          Pos = player_shoot$Pos_1,
          Shooting = ((player_shoot$PSxG_minus_xG_mean_mean - mean(cp_shoot$PSxG_minus_xG_mean_mean, na.rm = T))/sd(cp_shoot$PSxG_minus_xG_mean_mean, na.rm = T)) * 0.45 +
            ((player_shoot$Gls_over_xG_mean_mean - mean(cp_shoot$Gls_over_xG_mean_mean, na.rm = T))/sd(cp_shoot$Gls_over_xG_mean_mean, na.rm = T)) * 0.45 +
            (mean(per_90_player$Shots_per_90_mean) - mean(per_90_compPool$Shots_per_90_mean, na.rm = T)/sd(per_90_compPool$Shots_per_90_mean, na.rm = T)) * 0.1,
          Heading = (player_shoot$Head_PSxG_minus_xG_mean_mean - mean(cp_shoot$Head_PSxG_minus_xG_mean_mean, na.rm = T))/sd(cp_shoot$Head_PSxG_minus_xG_mean_mean, na.rm = T) * 0.45 +
            (player_shoot$Head_Gls_minus_xG_mean_mean - mean(cp_shoot$Head_Gls_minus_xG_mean_mean, na.rm = T))/sd(cp_shoot$Head_Gls_minus_xG_mean_mean, na.rm = T) * 0.45 +
            (ifelse(length((per_90_player %>% filter(`Body Part` == "Head"))$Shots_per_90_mean) == 0, NaN, mean((per_90_player %>% filter(`Body Part` == "Head"))$Shots_per_90_mean)) - mean((per_90_compPool %>% filter(`Body Part` == "Head"))$Shots_per_90_mean, na.rm = T)/sd((per_90_compPool %>% filter(`Body Part` == "Head"))$Shots_per_90_mean, na.rm = T)) * 0.1,
          Foot_Shots = ((player_shoot$Foot_PSxG_minus_xG_mean_mean - mean(cp_shoot$Foot_PSxG_minus_xG_mean_mean, na.rm = T))/sd(cp_shoot$Foot_PSxG_minus_xG_mean_mean, na.rm = T)) * 0.45 +
            ((player_shoot$Foot_Gls_minus_xG_mean_mean - mean(cp_shoot$Foot_Gls_minus_xG_mean_mean, na.rm = T))/sd(cp_shoot$Foot_Gls_minus_xG_mean_mean, na.rm = T)) * 0.45 +
            (ifelse(length((per_90_player %>% filter(`Body Part` == "Left Foot" | `Body Part` == "Right Foot"))$Shots_per_90_mean) == 0, NaN, mean((per_90_player %>% filter(`Body Part` == "Left Foot" | `Body Part` == "Right Foot"))$Shots_per_90_mean)) - mean((per_90_compPool %>% filter(`Body Part` == "Left Foot" | `Body Part` == "Right Foot"))$Shots_per_90_mean, na.rm = T)/sd((per_90_compPool %>% filter(`Body Part` == "Left Foot" | `Body Part` == "Right Foot"))$Shots_per_90_mean, na.rm = T)) * 0.1,
          RightFoot_Shots = ((player_shoot$RightF_PSxG_minus_xG_mean_mean - mean(cp_shoot$RightF_PSxG_minus_xG_mean_mean, na.rm = T))/sd(cp_shoot$RightF_PSxG_minus_xG_mean_mean, na.rm = T)) * 0.45 +
            ((player_shoot$RightF_Gls_minus_xG_mean_mean - mean(cp_shoot$RightF_Gls_minus_xG_mean_mean, na.rm = T))/sd(cp_shoot$RightF_Gls_minus_xG_mean_mean, na.rm = T)) * 0.45 +
            (ifelse(length((per_90_player %>% filter(`Body Part` == "Right Foot"))$Shots_per_90_mean) == 0, NaN, mean((per_90_player %>% filter(`Body Part` == "Right Foot"))$Shots_per_90_mean)) - mean((per_90_compPool %>% filter(`Body Part` == "Right Foot"))$Shots_per_90_mean, na.rm = T)/sd((per_90_compPool %>% filter(`Body Part` == "Right Foot"))$Shots_per_90_mean, na.rm = T)) * 0.1,
          LeftFoot_Shots = ((player_shoot$LeftF_PSxG_minus_xG_mean_mean - mean(cp_shoot$LeftF_PSxG_minus_xG_mean_mean, na.rm = T))/sd(cp_shoot$LeftF_PSxG_minus_xG_mean_mean, na.rm = T)) * 0.45 +
            ((player_shoot$LeftF_Gls_minus_xG_mean_mean - mean(cp_shoot$LeftF_Gls_minus_xG_mean_mean, na.rm = T))/sd(cp_shoot$LeftF_Gls_minus_xG_mean_mean, na.rm = T)) * 0.45 +
            (ifelse(length((per_90_player %>% filter(`Body Part` == "Left Foot"))$Shots_per_90_mean) == 0, NaN, mean((per_90_player %>% filter(`Body Part` == "Left Foot"))$Shots_per_90_mean)) - mean((per_90_compPool %>% filter(`Body Part` == "Left Foot"))$Shots_per_90_mean, na.rm = T)/sd((per_90_compPool %>% filter(`Body Part` == "Left Foot"))$Shots_per_90_mean, na.rm = T)) * 0.1
        )
      }
      
      # Bind new shooting z-scores with the previous ones
      shooting_zscores <- bind_rows(shooting_zscores, plySH_z %>%
                                      mutate_all(~ifelse(is.numeric(.), round(., 2), .))
      )
    }
  }
  
  # Calculate the p90 stats by predicted or total
  if (predicted_or_total == "predicted") {
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
  } else if (predicted_or_total == "total") {
    playerML <- playerML %>%
      group_by(Player, Team, Pos_1) %>%
      summarize(across(where(is.numeric), sum, na.rm = TRUE), .groups = "keep") %>%
      filter(Min >= 90) %>%
      mutate_if(is.numeric, ~ . / (Min / 90))
    compPoolML <- compPoolML %>%
      group_by(Player, Team, Pos_1) %>%
      summarize(across(where(is.numeric), sum, na.rm = TRUE), .groups = "keep") %>%
      filter(Min >= 90)  %>%
      mutate_if(is.numeric, ~ . / (Min / 90))
  }
  
  # For loop to calculate z-scores for each position the player has played (compared to the same in the comp. pool)
  player_zscores <- data.frame()
  for (ply_pos in playerML$Pos_1) {
    filtered_compPool <- compPoolML %>% filter(Pos_1 == ply_pos)
    filtered_player <- playerML %>% filter(Pos_1 == ply_pos)
    
    pos_df <- data.frame(
      Player = filtered_player$Player,
      Team = filtered_player$Team,
      Pos = filtered_player$Pos_1,
      ProgPassDist = ((filtered_player$PrgDist_Total - mean(filtered_compPool$PrgDist_Total, na.rm = T))/sd(filtered_compPool$PrgDist_Total, na.rm = T)) * 0.4 +
        ((filtered_player$PrgP - mean(filtered_compPool$PrgP, na.rm = T))/sd(filtered_compPool$PrgP, na.rm = T)) * 0.6,
      ShortPasses = ((filtered_player$Att_Short - mean(filtered_compPool$Att_Short, na.rm = T))/sd(filtered_compPool$Att_Short, na.rm = T)) * 0.3 +
        ((filtered_player$Cmp_percent_Short - mean(filtered_compPool$Cmp_percent_Short, na.rm = T))/sd(filtered_compPool$Cmp_percent_Short, na.rm = T)) * 0.7,
      MediumPasses = ((filtered_player$Att_Medium - mean(filtered_compPool$Att_Medium, na.rm = T))/sd(filtered_compPool$Att_Medium, na.rm = T)) * 0.3 +
        ((filtered_player$Cmp_percent_Medium - mean(filtered_compPool$Cmp_percent_Medium, na.rm = T))/sd(filtered_compPool$Cmp_percent_Medium, na.rm = T)) * 0.7,
      LongPasses = ((filtered_player$Att_Long - mean(filtered_compPool$Att_Long, na.rm = T))/sd(filtered_compPool$Att_Long, na.rm = T)) * 0.3 +
        ((filtered_player$Cmp_percent_Long - mean(filtered_compPool$Cmp_percent_Long, na.rm = T))/sd(filtered_compPool$Cmp_percent_Long, na.rm = T)) * 0.7,
      ProgCarriesDist = ((filtered_player$PrgDist_Carries - mean(filtered_compPool$PrgDist_Carries, na.rm = T))/sd(filtered_compPool$PrgDist_Carries, na.rm = T)) * 0.4 +
        ((filtered_player$PrgC_Carries - mean(filtered_compPool$PrgC_Carries, na.rm = T))/sd(filtered_compPool$PrgC_Carries, na.rm = T)) * 0.6,
      xAG = (filtered_player$xAG - mean(filtered_compPool$xAG, na.rm = T))/sd(filtered_compPool$xAG, na.rm = T),
      PPA = (filtered_player$PPA - mean(filtered_compPool$PPA, na.rm = T))/sd(filtered_compPool$PPA, na.rm = T),
      Tackles = (filtered_player$Tkl_Tackles - mean(filtered_compPool$Tkl_Tackles, na.rm = T))/sd(filtered_compPool$Tkl_Tackles, na.rm = T),
      Challenges_Tkld = (filtered_player$Tkl_percent_Challenges - mean(filtered_compPool$Tkl_percent_Challenges, na.rm = T))/sd(filtered_compPool$Tkl_percent_Challenges, na.rm = T) * 0.7 +
        (filtered_player$Att_Challenges - mean(filtered_compPool$Att_Challenges, na.rm = T))/sd(filtered_compPool$Att_Challenges, na.rm = T) * 0.3,
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
  
  # Extract goalkeeping data for the player in case he is a goalkeeper
  if ("GK" %in% playerML$Pos_1) {
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
  
  # Merge the data frames (ML and shooting z-scores)
  if (nrow(ply_shoot) > 0 & nrow(compPool_shoot) > 0) {
    player_zscores <- merge(player_zscores, shooting_zscores, by = c("Player", "Team", "Pos"))
  }
  
  # Restore the original dplyr.summarise.inform value
  options(dplyr.summarise.inform = original_inform)
  
  return(player_zscores)
}

# Get all the teams percentiles
teams_perc <- function(leagues_sel = "All", sex_sel = "M") {
  # Load data
  load("rda/playersMatchLogs.rda")
  
  # Filter the data frame based on arguments
  if (!"All" %in% leagues_sel) {
    playersMatchLogs <- playersMatchLogs %>% select(League %in% leagues_sel)
  }
  
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
  
  # Divide position (Pos) column by 4 (primary position to quaternary position)
  playersMatchLogs[c("Pos_1", "Pos_2", "Pos_3", "Pos_4")] <- t(sapply(strsplit(playersMatchLogs$Pos, ","), function(x) c(x, rep(NA, 4 - length(x)))))
  
  # Convert columns to numeric
  playersMatchLogs[c(mean_stats, "Min")] <- lapply(playersMatchLogs[c(mean_stats, "Min")], as.numeric)
  
  # Add attempted aerials as a column
  playersMatchLogs <- playersMatchLogs %>% mutate(Att_Aerials = Won_Aerial_Duels + Lost_Aerial_Duels)
  
  # Calculate stats per 90
  stats_per90 <- playersMatchLogs %>% 
    filter(Sex %in% sex_sel) %>% 
    select(Team, League, Pos_1, Min, PrgDist_Total, Att_Short, Att_Medium, Att_Long,
           PrgDist_Carries, xAG, PPA, Tkl_Tackles, Att_Challenges,
           Blocks_Blocks, Int.x, Att_Take_Ons, CPA_Carries, Rec_Receiving, PrgR_Receiving, Att_Aerials) %>%
    group_by(Team, League, Pos_1) %>%
    filter(Min >= 15) %>%
    summarize(across(where(is.numeric), sum, na.rm = TRUE), .groups = "keep") %>%
    filter(Min >= 90) %>%
    mutate_if(is.numeric, ~ . / (Min / 90))
  
  # For loop to iterate through stats per 90 based on position
  teams_percentiles <- data.frame()
  for (ply_pos in unique(stats_per90$Pos_1)) {
    filtered_p90 <- stats_per90 %>% filter(Pos_1 == ply_pos)
    perc_df <- data.frame(
      Team = filtered_p90$Team,
      League = filtered_p90$League,
      Pos = filtered_p90$Pos_1,
      ProgPassDist = percent_rank(coalesce(filtered_p90$PrgDist_Total, -Inf)),
      ShortPasses = percent_rank(coalesce(filtered_p90$Att_Short, -Inf)),
      MediumPasses = percent_rank(coalesce(filtered_p90$Att_Medium, -Inf)),
      LongPasses = percent_rank(coalesce(filtered_p90$Att_Long, -Inf)),
      ProgCarriesDist = percent_rank(coalesce(filtered_p90$PrgDist_Carries, -Inf)),
      xAG = percent_rank(coalesce(filtered_p90$xAG, -Inf)),
      PPA = percent_rank(coalesce(filtered_p90$PPA, -Inf)),
      Tackles = percent_rank(coalesce(filtered_p90$Tkl_Tackles, -Inf)),
      Challenges_Tkld = percent_rank(coalesce(filtered_p90$Att_Challenges, -Inf)),
      Blocks = percent_rank(coalesce(filtered_p90$Blocks_Blocks, -Inf)),
      Int = percent_rank(coalesce(filtered_p90$Int.x, -Inf)),
      TakeOns = percent_rank(coalesce(filtered_p90$Att_Take_Ons, -Inf)),
      CPA = percent_rank(coalesce(filtered_p90$CPA_Carries, -Inf)),
      Received = percent_rank(coalesce(filtered_p90$Rec_Receiving, -Inf)),
      ProgReceived = percent_rank(coalesce(filtered_p90$PrgR_Receiving, -Inf)),
      AeerialDuels = percent_rank(coalesce(filtered_p90$Att_Aerials, -Inf))
    )
    teams_percentiles <- rbind(teams_percentiles, perc_df)
  }
  
  return(teams_percentiles)
}

# Get the weighted z-scores based on similar teams' percentiles
percentile_weights <- function(ply_team = NULL, z_scores_df = NULL, leagues_sel = "All", sex_selected = "M", pos_or_role = "role") {
  # Load data
  load("rda/playersMatchLogs.rda")
  load("rda/similar_teams.rda")
  
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
  
  # Divide position (Pos) column by 4 (primary position to quaternary position)
  playersMatchLogs[c("Pos_1", "Pos_2", "Pos_3", "Pos_4")] <- t(sapply(strsplit(playersMatchLogs$Pos, ","), function(x) c(x, rep(NA, 4 - length(x)))))
  
  # Create "Role" column and assign it to the Pos_1 column in the playersMatchLogs data frame
  if (pos_or_role == "role") {
    playersMatchLogs <- playersMatchLogs %>%
      filter(!is.na(Pos_1)) %>%
      mutate(
        Role = case_when(
          Pos_1 == "LW" | Pos_1 == "RW" | Pos_1 == "LM" | Pos_1 == "RM" ~ "W",
          Pos_1 == "AM" | Pos_1 == "CM" ~ "CM",
          Pos_1 == "DM" ~ "DM",
          Pos_1 == "CB" ~ "CB",
          Pos_1 == "FW" ~ "FW",
          Pos_1 == "WB" | Pos_1 == "LB" | Pos_1 == "RB" ~ "FB",
          Pos_1 == "GK" ~ "GK",
          TRUE ~ NA_character_
        )
      )
    playersMatchLogs$Pos_1 <- playersMatchLogs$Role
  }
  
  # Convert columns to numeric
  playersMatchLogs[c(mean_stats, "Min")] <- lapply(playersMatchLogs[c(mean_stats, "Min")], as.numeric)
  
  # Add attempted aerials as a column
  playersMatchLogs <- playersMatchLogs %>% mutate(Att_Aerials = Won_Aerial_Duels + Lost_Aerial_Duels)
  
  # Filter the arguments
  if (!"All" %in% leagues_sel) {
    playersMatchLogs <- playersMatchLogs %>% filter(League %in% leagues_sel)
  }
  
  # Calculate stats per 90
  stats_per90 <- playersMatchLogs %>% 
    filter(Sex %in% sex_selected) %>%
    select(Team, League, Pos_1, Min, PrgP, Att_Short, Att_Medium, Att_Long,
           PrgC_Carries, xAG, PPA, Tkl_Tackles, Att_Challenges,
           Blocks_Blocks, Int.x, Att_Take_Ons, CPA_Carries, Rec_Receiving, PrgR_Receiving, Att_Aerials) %>%
    group_by(Team, League, Pos_1) %>%
    filter(Min >= 15) %>%
    summarize(across(where(is.numeric), sum, na.rm = TRUE), .groups = "keep") %>%
    filter(Min >= 90) %>%
    mutate_if(is.numeric, ~ . / (Min / 90))
  
  # For loop to iterate through stats per 90 based on position and calculate teams percentiles
  similarity_weights <- data.frame()
  for (ply_pos in unique(stats_per90$Pos_1)) {
    filtered_p90 <- stats_per90 %>% filter(Pos_1 == ply_pos)
    teams_percentiles <- data.frame(
      Team = filtered_p90$Team,
      League = filtered_p90$League,
      Position = filtered_p90$Pos_1,
      ProgPassDist = percent_rank(coalesce(filtered_p90$PrgP, -Inf)),
      ShortPasses = percent_rank(coalesce(filtered_p90$Att_Short, -Inf)),
      MediumPasses = percent_rank(coalesce(filtered_p90$Att_Medium, -Inf)),
      LongPasses = percent_rank(coalesce(filtered_p90$Att_Long, -Inf)),
      ProgCarriesDist = percent_rank(coalesce(filtered_p90$PrgC_Carries, -Inf)),
      PPA = percent_rank(coalesce(filtered_p90$PPA, -Inf)),
      Tackles = percent_rank(coalesce(filtered_p90$Tkl_Tackles, -Inf)),
      Challenges_Tkld = percent_rank(coalesce(filtered_p90$Att_Challenges, -Inf)),
      Blocks = percent_rank(coalesce(filtered_p90$Blocks_Blocks, -Inf)),
      Int = percent_rank(coalesce(filtered_p90$Int.x, -Inf)),
      TakeOns = percent_rank(coalesce(filtered_p90$Att_Take_Ons, -Inf)),
      CPA = percent_rank(coalesce(filtered_p90$CPA_Carries, -Inf)),
      Received = percent_rank(coalesce(filtered_p90$Rec_Receiving, -Inf)),
      ProgReceived = percent_rank(coalesce(filtered_p90$PrgR_Receiving, -Inf)),
      AerialDuels = percent_rank(coalesce(filtered_p90$Att_Aerials, -Inf))
    )
    
    # Get similar teams
    sim_teams <- (similar_teams %>% filter(Team == ply_team))$Similar_To
    
    # Calculate weights
    similar_teams_percs <- teams_percentiles %>% filter(Team %in% sim_teams)
    own_team_percs <- teams_percentiles %>% filter(Team %in% ply_team)
    new_weights <- data.frame(
      Position = ply_pos,
      ProgPassDist = mean(similar_teams_percs$ProgPassDist) * 0.4 + ifelse(nrow(own_team_percs) > 0, mean(own_team_percs$ProgPassDist) * 0.6, 0),
      ShortPasses = mean(similar_teams_percs$ShortPasses) * 0.4 + ifelse(nrow(own_team_percs) > 0, mean(own_team_percs$ShortPasses) * 0.6, 0),
      MediumPasses = mean(similar_teams_percs$MediumPasses) * 0.4 + ifelse(nrow(own_team_percs) > 0, mean(own_team_percs$MediumPasses) * 0.6, 0),
      LongPasses = mean(similar_teams_percs$LongPasses) * 0.4 + ifelse(nrow(own_team_percs) > 0, mean(own_team_percs$LongPasses) * 0.6, 0),
      ProgCarriesDist = mean(similar_teams_percs$ProgCarriesDist) * 0.4 + ifelse(nrow(own_team_percs) > 0, mean(own_team_percs$ProgCarriesDist) * 0.6, 0),
      PPA = mean(similar_teams_percs$PPA) * 0.4 + ifelse(nrow(own_team_percs) > 0, mean(own_team_percs$PPA) * 0.6, 0),
      Tackles = mean(similar_teams_percs$Tackles) * 0.4 + ifelse(nrow(own_team_percs) > 0, mean(own_team_percs$Tackles) * 0.6, 0),
      Challenges_Tkld = mean(similar_teams_percs$Challenges_Tkld) * 0.4 + ifelse(nrow(own_team_percs) > 0, mean(own_team_percs$Challenges_Tkld) * 0.6, 0),
      Blocks = mean(similar_teams_percs$Blocks) * 0.4 + ifelse(nrow(own_team_percs) > 0, mean(own_team_percs$Blocks) * 0.6, 0),
      Int = mean(similar_teams_percs$Int) * 0.4 + ifelse(nrow(own_team_percs) > 0, mean(own_team_percs$Int) * 0.6, 0),
      TakeOns = mean(similar_teams_percs$TakeOns) * 0.4 + ifelse(nrow(own_team_percs) > 0, mean(own_team_percs$TakeOns) * 0.6, 0),
      CPA = mean(similar_teams_percs$CPA) * 0.4 + ifelse(nrow(own_team_percs) > 0, mean(own_team_percs$CPA) * 0.6, 0),
      Received = mean(similar_teams_percs$Received) * 0.4 + ifelse(nrow(own_team_percs) > 0, mean(own_team_percs$Received) * 0.6, 0),
      ProgReceived = mean(similar_teams_percs$ProgReceived) * 0.4 + ifelse(nrow(own_team_percs) > 0, mean(own_team_percs$ProgReceived) * 0.6, 0),
      AerialDuels = mean(similar_teams_percs$AerialDuels) * 0.4 + ifelse(nrow(own_team_percs) > 0, mean(own_team_percs$AerialDuels) * 0.6, 0)
    )
    
    similarity_weights <- rbind(similarity_weights, new_weights)
  }
  
  # Apply weights to the corresponding columns
  common_cols <- intersect(names(z_scores_df), names(similarity_weights))
  new_zscores <- z_scores_df
  for (pos in new_zscores$Pos) {
    df1_filtered <- new_zscores[new_zscores$Pos == pos, common_cols]
    df2_filtered <- similarity_weights[similarity_weights$Position == pos, common_cols]
    new_zscores[new_zscores$Pos == pos, common_cols] <- df1_filtered * df2_filtered
  }
  z_scores_df <- new_zscores
  
  return(z_scores_df)
}

# Create plot based on z-score df
z_scores_plot <- function(test_zsc = NULL, player_position = NULL) {
  # Subset the data for the current player position, excluding 'Team', 'Player', and 'Pos' columns
  if (player_position == "All") {
    df <- as.data.frame({
      test_zsc %>%
        group_by(Player, Team) %>%
        select(-Pos) %>%
        summarise_all(~mean(., na.rm = TRUE))
    }) %>% select(-Team, -Player)
  } else {
    df <- test_zsc %>% filter(Pos == player_position) %>% select(-Team, -Player, -Pos)
  }
  ts_df <- data.frame(stat = names(df),
                      position = rep(player_position, length(df)),
                      z_score = as.numeric(df[1, ]))
  
  
  # Filter by position and create HEX code column
  df <- {(ts_df %>% mutate(z_score = case_when(is.nan(z_score) ~ 0, !is.nan(z_score) ~ z_score))) %>%
      mutate(color_codes = case_when(z_score <= -1.28 ~ "#a62c2b",
                                     z_score > -1.28 & z_score <= -0.84 ~ "#c44841",
                                     z_score > -0.84 & z_score <= -0.52 ~ "#e36359",
                                     z_score > -0.52 & z_score <= -0.25 ~ "#ff7e72",
                                     z_score > -0.25 & z_score < 0 ~ "#ff998b",
                                     z_score == 0 ~ "#ffffff",
                                     z_score > 0 & z_score < 0.25 ~ "#6daa7f",
                                     z_score >= 0.25 & z_score < 0.52 ~ "#548f66",
                                     z_score >= 0.52 & z_score < 0.84 ~ "#3a754e",
                                     z_score >= 0.84 & z_score < 1.28 ~ "#205c37",
                                     z_score >= 1.28 ~ "#014421"))}
  
  # Change stats names and add stat type to use as sorting parameter
  df$stat <- {c("Pass Prog.", "Short Pass", "Medium Pass", "Long Pass", "Carrying Prog.", "xAG",
                "Passes into Box", "Tackles", "Dribbling Tackles", "Blocks", "Interceptions", "Take-Ons",
                "Carries into Box", "Receiving", "Prog. Receiving", "Ball Control", "Aerial Duels",
                "Shooting", "Shots (Head)", "Shots (Foot)", "Right Foot Sh.", "Left Foot Sh")}
  df$stat_type <- {c("Passing", "Passing", "Passing",
                     "Passing", "Possession", "Passing",
                     "Passing", "Defense", "Defense",
                     "Defense", "Defense", "Possession",
                     "Possession", "Possession", "Possession",
                     "Possession", "Defense", "Shooting",
                     "Shooting", "Shooting", "Shooting",
                     "Shooting")}
  
  # Determine the full name of the player's position
  subt <- {case_when(
    player_position == "CM" ~ "Central Midfielder",
    player_position == "FW" ~ "Forward",
    player_position == "WB" ~ "Wing Back",
    player_position == "AM" ~ "Attacking Midfielder",
    player_position == "CB" ~ "Centreback",
    player_position == "GK" ~ "Goalkeeper",
    player_position == "RB" ~ "Right Back",
    player_position == "LM" ~ "Left Midfielder",
    player_position == "RW" ~ "Right Winger",
    player_position == "RM" ~ "Right Midfielder",
    player_position == "LB" ~ "Left Back",
    player_position == "LW" ~ "Left Winger",
    player_position == "DM" ~ "Defensive Midfielder",
    player_position == "DF" ~ "Defender",
    player_position == "MF" ~ "Midfielder",
    player_position == "FB" ~ "Fullback",
    player_position == "W" ~ "Winger",
    player_position == "All" ~ "All Positions",
    TRUE ~ "N"
  )}
  
  # Generate plot and save it with hq
  plot <- df %>% 
    mutate(stat = fct_reorder(stat, stat_type, .desc = TRUE)) %>%
    ggplot(aes(x = z_score, y = stat, fill = color_codes)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = round(z_score, 2),
                  hjust = ifelse(z_score > 0, -0.7, ifelse(z_score < 0, 1.3, 0.5))),
              position = position_dodge(0.9),
              vjust = 0.5,
              color = "white",
              size = 4.5) +
    scale_fill_identity() +
    labs(title = test_zsc$Player[1]) +
    labs(subtitle = subt) +
    theme(panel.background = element_rect(fill = "black"),
          panel.grid.major.y = element_line(color = "#1E1E1E"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.text.y = element_text(size = 12, color = "white"),
          axis.text.x = element_text(color = "white"),
          axis.title.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y = element_blank(),
          plot.title = element_text(hjust = 0.5, color = "white", size = 18),
          plot.subtitle = element_text(hjust = 0.5, color = "white", size = 12),
          plot.background = element_rect(fill = "black"))
  
  ggsave("plot.png", plot, width = 16, height = 9, dpi = 300, units = "in")
}
