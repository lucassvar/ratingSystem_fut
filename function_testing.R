# Shooting Logs - Players
sh_logs <- sh_logs %>% mutate(xG = as.numeric(xG), PSxG = as.numeric(PSxG))


{sh_logs %>%
  filter(Player_SCA_1 == "Julián Álvarez", Squad %in% c("Manchester City")) %>%
  summarise(
    xG = mean(xG),
    PSxG = mean(PSxG, na.rm = T),
    Total = n(),
    PassLive = sum(Event_SCA_1 == "Pass (Live)"),
    Fouled = sum(Event_SCA_1 == "Fouled"),
    PassDead = sum(Event_SCA_1 == "Pass (Dead)"),
    Shots = sum(Event_SCA_1 == "Shot"),
    Take_Ons = sum(Event_SCA_1 == "Take-On"),
    Tackles = sum(Event_SCA_1 == "Tackle"),
    notIdentified = sum(Event_SCA_1 == ""),
    Interception = sum(Event_SCA_1 == "Interception")
  )}




test <- function(player_selected = "Erling Haaland", teams_selected = "Manchester City"){
  count <- 0
  for (team in teams_selected) {
    df <- sh_logs %>%
      mutate(Gls_minus_xG = case_when(Outcome == "Goal" ~ 1-xG,
                                      Outcome != "Goal" ~ 0-xG),
             PSxG_minus_xG = case_when(is.na(PSxG) ~ 0-xG,
                                       !is.na(PSxG) ~ PSxG-xG)) %>%
      filter(Player == player_selected, Squad %in% team) %>%
      summarise(
        Player = player_selected,
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
    if (count == 0) {
      player_sh_logs <- df
    } else {
      player_sh_logs <- rbind(player_sh_logs, df[1, ])
    }
    
    count <- count + 1
  }
  
  return(player_sh_logs)
}


glimpse(test("Raheem Sterling", teams_selected = c("Manchester City", "Chelsea")))













