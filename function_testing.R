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

# Load the PLAYER match logs data frame
load("rda/playersMatchLogs.rda")

# Divide position (Pos) column by 4 (primary position to quaternary position)
playersMatchLogs[c("Pos_1", "Pos_2", "Pos_3", "Pos_4")] <- t(sapply(strsplit(playersMatchLogs$Pos, ","), function(x) c(x, rep(NA, 4 - length(x)))))


# Selected stats for "per min" and for mean calculation
sel_stats <- {c("Cmp_Total", "Att_Total", "TotDist_Total", "PrgDist_Total", "Cmp_Short", "Att_Short",
               "Cmp_Medium", "Att_Medium", "Cmp_Long", "Att_Long", "Ast", "xAG", "xA", "KP", "Final_Third", "PPA", "CrsPA", "PrgP",
               "Att", "Live_Pass_Types", "Dead_Pass_Types", "FK_Pass_Types", "TB_Pass_Types", "Sw_Pass_Types", "Crs_Pass_Types",
               "TI_Pass_Types", "CK_Pass_Types", "In_Corner_Kicks", "Out_Corner_Kicks", "Str_Corner_Kicks", "Cmp_Outcomes", "Off_Outcomes",
               "Blocks_Outcomes", "Tkl_Tackles", "TklW_Tackles", "Def 3rd_Tackles", "Mid 3rd_Tackles", "Att 3rd_Tackles", "Tkl_Challenges",
               "Att_Challenges", "Lost_Challenges", "Blocks_Blocks", "Sh_Blocks", "Pass_Blocks", "Int.x", "Tkl+Int", "Clr", "Err",
               "Touches_Touches", "Def Pen_Touches", "Def 3rd_Touches", "Mid 3rd_Touches", "Att 3rd_Touches", "Att Pen_Touches",
               "Live_Touches", "Att_Take_Ons", "Succ_Take_Ons", "Tkld_Take_Ons", "Carries_Carries", "TotDist_Carries", "PrgDist_Carries", "PrgC_Carries",
               "Final_Third_Carries", "CPA_Carries", "Mis_Carries", "Dis_Carries", "Rec_Receiving", "PrgR_Receiving", "CrdY", "CrdR",
               "2CrdY", "Fls", "Fld", "Off", "Crs", "PKwon", "PKcon", "OG", "Recov", "Won_Aerial_Duels", "Lost_Aerial_Duels")}
mean_stats <- {c(sel_stats, "Min", paste0(sel_stats, "_per_min"),
                 "Cmp_percent_Total", "Cmp_percent_Short", "Cmp_percent_Medium", "Cmp_percent_Long",
                 "Tkl_percent_Challenges", "Succ_percent_Take_Ons", "Tkld_percent_Take_Ons",
                 "Won_percent_Aerial_Duels")}

# Convert columns to numeric
playersMatchLogs[sel_stats] <- lapply(playersMatchLogs[sel_stats], as.numeric)
playersMatchLogs$Min <- as.numeric(playersMatchLogs$Min)

# Create "per Min" columns for selected stats
for (col in sel_stats) {
  new_col_name <- paste0(col, "_per_min")
  playersMatchLogs[new_col_name] <- playersMatchLogs[, col] / playersMatchLogs$Min
}


# Arguments
selected_player <- "Julián Álvarez"
date_range <- c("2023-08-01", "2023-10-27")
first_position <- c("FW", "AM", "CM", "RW")
cp_date_range <- c("2023-08-01", "2023-10-27")

# Filter the data frame based on given player and arguments
player_ML <- playersMatchLogs %>% filter(Player == selected_player,
                                         Pos_1 %in% first_position,
                                         Match_Date >= min(date_range) & Match_Date <= max(date_range))

# Create comp. pool data frame based on arguments
compPool_ML <- playersMatchLogs %>% filter(Player != selected_player,
                                           Pos_1 %in% first_position,
                                           Match_Date >= min(cp_date_range) & Match_Date <= max(cp_date_range))


# Group by and calculate means and Totals for both Player and Comp. Pool
player_ML <- player_ML %>%
  group_by(League, Player, Team, Pos_1) %>%
  summarise(across(all_of(mean_stats), list(mean = ~mean(., na.rm = TRUE), total = ~sum(., na.rm = TRUE))))
            
compPool_ML <- compPool_ML %>%
  group_by(League, Player, Team, Pos_1) %>%
  summarise(across(all_of(mean_stats), list(mean = ~mean(., na.rm = TRUE), total = ~sum(., na.rm = TRUE))))


# Select columns used in the z-scores
columns_zscores <- {c(
  "League", "Team", "Player", "Pos_1", "Min_total", "Min_mean",
  "Cmp_percent_Short_mean", "Att_Short_per_min_mean",
  "Cmp_percent_Medium_mean", "Att_Medium_per_min_mean",
  "Cmp_percent_Long_mean", "Att_Long_per_min_mean",
  "PrgDist_Total_per_min_mean", "PrgDist_Carries_per_min_mean",
  "xAG_per_min_mean", "PPA_per_min_mean", "Tkl_Tackles_per_min_mean",
  "Tkl_percent_Challenges_mean", "Att_Challenges_per_min_mean",
  "Blocks_Blocks_per_min_mean", "Int.x_per_min_mean",
  "Succ_percent_Take_Ons_mean", "Att_Take_Ons_per_min_mean",
  "CPA_Carries_per_min_mean", "Rec_Receiving_per_min_mean",
  "PrgR_Receiving_per_min_mean", "Mis_Carries_per_min_mean",
  "Dis_Carries_per_min_mean", "Won_percent_Aerial_Duels_mean",
  "Won_Aerial_Duels_per_min_mean", "Lost_Aerial_Duels_per_min_mean"
)}
player_ML <- as.data.frame(player_ML %>% select(all_of(columns_zscores)))
compPool_ML <- as.data.frame(compPool_ML %>% select(all_of(columns_zscores)))


# Calculate the stats per 90 minutes
player_ML <- player_ML %>%
  mutate(across(-c(League, Team, Player, Pos_1, Min_mean, Min_total,
                   Cmp_percent_Short_mean, Cmp_percent_Medium_mean, Cmp_percent_Long_mean,
                   Tkl_percent_Challenges_mean, Succ_percent_Take_Ons_mean, Won_percent_Aerial_Duels_mean),
                ~round(. * 90, 2)))
compPool_ML <- compPool_ML %>%
  mutate(across(-c(League, Team, Player, Pos_1, Min_mean, Min_total,
                   Cmp_percent_Short_mean, Cmp_percent_Medium_mean, Cmp_percent_Long_mean,
                   Tkl_percent_Challenges_mean, Succ_percent_Take_Ons_mean, Won_percent_Aerial_Duels_mean),
                ~round(. * 90, 2)))



# For loop to calculate z-scores for each position the player has played (comparing to same position in the comp. pool)
player_zscores <- NULL
for (ply_pos in unique(player_ML$Pos_1)) {
  filtered_CP <- compPool_ML %>% filter(Pos_1 == ply_pos) %>% mutate(Att_Aerials = Won_Aerial_Duels_per_min_mean + Lost_Aerial_Duels_per_min_mean)
  ply_data <- player_ML %>% filter(Pos_1 == ply_pos) %>% mutate(Att_Aerials = Won_Aerial_Duels_per_min_mean + Lost_Aerial_Duels_per_min_mean)
  pos_df <- ply_data %>% summarise(
    Player = Player,
    League = League,
    Team = Team,
    Pos = Pos_1,
    Prog_Pass_Dist = (PrgDist_Total_per_min_mean - mean(filtered_CP$PrgDist_Total_per_min_mean, na.rm = T)) / sd(filtered_CP$PrgDist_Total_per_min_mean, na.rm = T),
    Short_Passes = ((Cmp_percent_Short_mean - mean(filtered_CP$Cmp_percent_Short_mean, na.rm = T)) / sd(filtered_CP$Cmp_percent_Short_mean, na.rm = T)) +
      ((Att_Short_per_min_mean - mean(filtered_CP$Att_Short_per_min_mean, na.rm = T)) / sd(filtered_CP$Att_Short_per_min_mean, na.rm = T)),
    Medium_Passes = ((Cmp_percent_Medium_mean - mean(filtered_CP$Cmp_percent_Medium_mean, na.rm = T)) / sd(filtered_CP$Cmp_percent_Medium_mean, na.rm = T)) +
      ((Att_Medium_per_min_mean - mean(filtered_CP$Att_Medium_per_min_mean, na.rm = T)) / sd(filtered_CP$Att_Medium_per_min_mean, na.rm = T)),
    Long_Passes = ((Cmp_percent_Long_mean - mean(filtered_CP$Cmp_percent_Long_mean, na.rm = T)) / sd(filtered_CP$Cmp_percent_Long_mean, na.rm = T)) +
      ((Att_Long_per_min_mean - mean(filtered_CP$Att_Long_per_min_mean, na.rm = T)) / sd(filtered_CP$Att_Long_per_min_mean, na.rm = T)),
    Prog_Carries_Dist = (PrgDist_Carries_per_min_mean - mean(filtered_CP$PrgDist_Carries_per_min_mean, na.rm = T)) / sd(filtered_CP$PrgDist_Carries_per_min_mean, na.rm = T),
    xAG = (xAG_per_min_mean - mean(filtered_CP$xAG_per_min_mean, na.rm = T)) / sd(filtered_CP$xAG_per_min_mean, na.rm = T),
    PPA = (PPA_per_min_mean - mean(filtered_CP$PPA_per_min_mean, na.rm = T)) / sd(filtered_CP$PPA_per_min_mean, na.rm = T),
    Tackles = (Tkl_Tackles_per_min_mean - mean(filtered_CP$Tkl_Tackles_per_min_mean, na.rm = T)) / sd(filtered_CP$Tkl_Tackles_per_min_mean, na.rm = T),
    Challenges_Tkld = ((Att_Challenges_per_min_mean - mean(filtered_CP$Att_Challenges_per_min_mean, na.rm = T)) / sd(filtered_CP$Att_Challenges_per_min_mean, na.rm = T)) +
      ((Tkl_percent_Challenges_mean - mean(filtered_CP$Tkl_percent_Challenges_mean, na.rm = T)) / sd(filtered_CP$Tkl_percent_Challenges_mean, na.rm = T)),
    Blocks = (Blocks_Blocks_per_min_mean - mean(filtered_CP$Blocks_Blocks_per_min_mean, na.rm = T)) / sd(filtered_CP$Blocks_Blocks_per_min_mean, na.rm = T),
    Int = (Int.x_per_min_mean - mean(filtered_CP$Int.x_per_min_mean, na.rm = T)) / sd(filtered_CP$Int.x_per_min_mean, na.rm = T),
    Take_Ons = ((Att_Take_Ons_per_min_mean - mean(filtered_CP$Att_Take_Ons_per_min_mean, na.rm = T)) / sd(filtered_CP$Att_Take_Ons_per_min_mean, na.rm = T)) +
      ((Succ_percent_Take_Ons_mean - mean(filtered_CP$Succ_percent_Take_Ons_mean, na.rm = T)) / sd(filtered_CP$Succ_percent_Take_Ons_mean, na.rm = T)),
    CPA = (CPA_Carries_per_min_mean - mean(filtered_CP$CPA_Carries_per_min_mean, na.rm = T)) / sd(filtered_CP$CPA_Carries_per_min_mean, na.rm = T),
    Received = (Rec_Receiving_per_min_mean - mean(filtered_CP$Rec_Receiving_per_min_mean, na.rm = T)) / sd(filtered_CP$Rec_Receiving_per_min_mean, na.rm = T),
    Prog_Received = (PrgR_Receiving_per_min_mean - mean(filtered_CP$PrgR_Receiving_per_min_mean, na.rm = T)) / sd(filtered_CP$PrgR_Receiving_per_min_mean, na.rm = T),
    Lost_Possession = ((mean(filtered_CP$Mis_Carries_per_min_mean, na.rm = T) - Mis_Carries_per_min_mean) / sd(filtered_CP$Mis_Carries_per_min_mean, na.rm = T)) +
      ((mean(filtered_CP$Dis_Carries_per_min_mean, na.rm = T) - Dis_Carries_per_min_mean) / sd(filtered_CP$Dis_Carries_per_min_mean, na.rm = T)),
    Aerial_Duels = ((Won_percent_Aerial_Duels_mean - mean(filtered_CP$Won_percent_Aerial_Duels_mean, na.rm = T)) / sd(filtered_CP$Won_percent_Aerial_Duels_mean, na.rm = T)) +
      ((Att_Aerials - mean(filtered_CP$Att_Aerials, na.rm = T)) / sd(filtered_CP$Att_Aerials, na.rm = T))
  )
  
  if (is.null(player_zscores)) {
    player_zscores <- as.data.frame(pos_df) %>%
      mutate_all(~ifelse(is.numeric(.), round(., 2), .))
  } else {
    player_zscores <- bind_rows(player_zscores, as.data.frame(pos_df) %>%
                                  mutate_all(~ifelse(is.numeric(.), round(., 2), .)))
  }
}






