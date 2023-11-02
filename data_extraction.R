library(worldfootballR)
library(dplyr)

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
  fem_sh_logs <- fb_match_shooting(fem_links) %>% mutate(Sex = "W")
  masc_sh_logs <- fb_match_shooting(masc_links) %>% mutate(Sex = "M")
  
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
  if (!is.null(sh_logs)) {
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
  }
  
  # Extract player goalkeeping data ----------
  print("Player - Keeper")
  fem_ply_keeper <- fb_advanced_match_stats(match_url = fem_links, stat_type = "keeper", team_or_player = "player") %>% mutate(Sex = "W")
  masc_ply_keeper <- fb_advanced_match_stats(match_url = masc_links, stat_type = "keeper", team_or_player = "player") %>% mutate(Sex = "M")
  
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
  fem_team_keeper <- fb_advanced_match_stats(match_url = fem_links, stat_type = "keeper", team_or_player = "team") %>% mutate(Sex = "W")
  masc_team_keeper <- fb_advanced_match_stats(match_url = masc_links, stat_type = "keeper", team_or_player = "team") %>% mutate(Sex = "M")
  
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
  
  
  # For loop to extract the match logs from each stat type
  for (stat in stat_types) {
    # Player stats
    print(paste("Player -", stat))
    fem_playerML <- fb_advanced_match_stats(match_url = fem_links,
                                            stat_type = stat,
                                            team_or_player = "player") %>% mutate(Sex = "W")
    masc_playerML <- fb_advanced_match_stats(match_url = masc_links,
                                             stat_type = stat,
                                             team_or_player = "player") %>% mutate(Sex = "M")
    
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
    fem_teamML <- fb_advanced_match_stats(match_url = fem_links,
                                          stat_type = stat,
                                          team_or_player = "team") %>% mutate(Sex = "W")
    masc_teamML <- fb_advanced_match_stats(match_url = masc_links,
                                           stat_type = stat,
                                           team_or_player = "team") %>% mutate(Sex = "M")
    
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
  
  
  # Add the links extracted to the used data frames and save it
  used_links <- c(used_links, unique(playersMatchLogs$Game_URL))
  save(used_links, file = "rda/used_links.rda")
  
  
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
  
  
  # Print total running time
  Sys.time() - start_time
}

# Get new links
update_links <- function(new_year){
  load("all_match_URLs.rda")
  new_links <- get_all_match_urls(year = new_year)
  all_match_URLs[[1]] <- unique(c(all_match_URLs[[1]], new_links[[1]]))
  all_match_URLs[[2]] <- unique(c(all_match_URLs[[2]], new_links[[2]]))
  save(all_match_URLs, file = "all_match_URLs.rda")
}

# Execution -------------------------------------------------------------------

# Load the saved match URLs
load("all_match_URLs.rda")

# Extract the data
fut_data_extraction(links_sel = new_all_match_URLs, links_examined = 1:100)


