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
football_data <- function(end_year = NA, links_sel = NULL, links_examined = 1:20){
  start_time <- Sys.time()
  
  # Load links already used (if there are any)
  if (file.exists("rda/used_links.rda")) {
    load("rda/used_links.rda")
  } else {
    used_links <- c()
  }
  
  # If there's no input of links then load by year and only use the links that were not previously used
  if (is.null(links_sel)){
    links_sel <- get_all_match_urls(year = end_year)
  }
  masc_links <- setdiff(links_sel[[1]], used_links)[links_examined]
  fem_links <- setdiff(links_sel[[2]], used_links)[links_examined]
  
  # Stat types used
  stat_types <- c("passing", "passing_types", "defense", "possession", "misc")
  
  # Empty data frames to save the data
  playersMatchLogs <- teamsMatchLogs <- c()
  
  # Extract shooting logs
  print("Shooting Logs")
  fem_sh_logs <- fb_match_shooting(fem_links)
  masc_sh_logs <- fb_match_shooting(masc_links)
  fem_sh_logs$Sex <- rep("W", nrow(fem_sh_logs))
  masc_sh_logs$Sex <- rep("M", nrow(masc_sh_logs))
  sh_logs <- rbind(fem_sh_logs, masc_sh_logs)
  
  # Extract keeper data
  print("Player - Keeper")
  fem_ply_keeper <- fb_advanced_match_stats(match_url = fem_links, stat_type = "keeper", team_or_player = "player")
  masc_ply_keeper <- fb_advanced_match_stats(match_url = masc_links, stat_type = "keeper", team_or_player = "player")
  fem_ply_keeper$Sex <- rep("W", nrow(fem_ply_keeper))
  masc_ply_keeper$Sex <- rep("M", nrow(masc_ply_keeper))
  ply_keeper <- rbind(fem_ply_keeper, masc_ply_keeper)
  
  print("Team - Keeper")
  fem_team_keeper <- fb_advanced_match_stats(match_url = fem_links, stat_type = "keeper", team_or_player = "team")
  masc_team_keeper <- fb_advanced_match_stats(match_url = masc_links, stat_type = "keeper", team_or_player = "team")
  fem_team_keeper$Sex <- rep("W", nrow(fem_team_keeper))
  masc_team_keeper$Sex <- rep("M", nrow(masc_team_keeper))
  team_keeper <- rbind(fem_team_keeper, masc_team_keeper)
  
  
  # For loop to extract the match logs from each 
  for (stat in stat_types) {
    print(paste("Player -", stat))
    fem_playerML <- fb_advanced_match_stats(match_url = fem_links,
                                        stat_type = stat,
                                        team_or_player = "player")
    masc_playerML <- fb_advanced_match_stats(match_url = masc_links,
                                            stat_type = stat,
                                            team_or_player = "player")
    fem_playerML$Sex <- rep("W", nrow(fem_playerML))
    masc_playerML$Sex <- rep("M", nrow(masc_playerML))
    playerML <- rbind(fem_playerML, masc_playerML)
    
    
    print(paste("Team -", stat))
    fem_teamML <- fb_advanced_match_stats(match_url = fem_links,
                                            stat_type = stat,
                                            team_or_player = "team")
    masc_teamML <- fb_advanced_match_stats(match_url = masc_links,
                                             stat_type = stat,
                                             team_or_player = "team")
    fem_teamML$Sex <- rep("W", nrow(fem_teamML))
    masc_teamML$Sex <- rep("M", nrow(masc_teamML))
    teamML <- rbind(fem_teamML, masc_teamML)
    
    # Merge the PLAYER data of the new stat type with the previous ones (if exists)
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
    
    # Merge the TEAM data of the new stat type with the previous ones (if exists)
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
  
  
  # Add the links extracted to the used data frames and save it
  used_links <- c(used_links, unique(playersMatchLogs$Game_URL))
  save(used_links, file = "rda/used_links.rda")
  
  
  # Save the data frames in the rda folder
  if(file.exists("rda/sh_logs.rda")){
    new_sh_logs <- sh_logs
    load("rda/sh_logs.rda")
    sh_logs <- bind_rows(sh_logs, new_sh_logs)
  }
  save(sh_logs, file = "rda/sh_logs.rda")
  
  if(file.exists("rda/playersMatchLogs.rda")){
    new_playersMatchLogs <- playersMatchLogs
    load("rda/playersMatchLogs.rda")
    playersMatchLogs <- bind_rows(playersMatchLogs, new_playersMatchLogs)
  }
  save(playersMatchLogs, file = "rda/playersMatchLogs.rda")
  
  if(file.exists("rda/teamsMatchLogs.rda")){
    new_teamsMatchLogs <- teamsMatchLogs
    load("rda/teamsMatchLogs.rda")
    teamsMatchLogs <- bind_rows(teamsMatchLogs, new_teamsMatchLogs)
  }
  save(teamsMatchLogs, file = "rda/teamsMatchLogs.rda")
  
  if(file.exists("rda/ply_keeper.rda")){
    new_ply_keeper <- ply_keeper
    load("rda/ply_keeper.rda")
    ply_keeper <- bind_rows(ply_keeper, new_ply_keeper)
  }
  save(ply_keeper, file = "rda/ply_keeper.rda")
  
  if(file.exists("rda/team_keeper.rda")){
    new_team_keeper <- team_keeper
    load("rda/team_keeper.rda")
    team_keeper <- bind_rows(team_keeper, new_team_keeper)
  }
  save(team_keeper, file = "rda/team_keeper.rda")
  
  
  Sys.time() - start_time
}


all_match_URLs <- get_all_match_urls(year = c(2024))
football_data(links_sel = all_match_URLs, links_examined = 1:50)
