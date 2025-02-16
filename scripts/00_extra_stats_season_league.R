
#devtools::install_github("JaseZiv/worldfootballR")

#if (!require("pacman")) install.packages("pacman")

library(worldfootballR)
library(dplyr)
library(readxl)
library(readr)


#eng_match_results <- load_match_results(country = "ENG", gender = c("M"), season_end_year = c(2023), tier = "1st")


#countries <- c("ESP", "ITA","FRA","GER","ENG")

#
# filename <- "output/data_otherstats.RData"
#
# if (!file.exists(filename)) {
#
#
#
#   countries <- c("ENG")
#
#   years <- c((2023-5):2023)
#
#   eng_stats_standard<- fb_season_team_stats(country = countries,
#                                             gender = "M",
#                                             season_end_year = years,
#                                             tier = "1st",
#                                             stat_type = "standard")
#
#   eng_stats_keeper<- fb_season_team_stats(country = countries,
#                                           gender = "M",
#                                           season_end_year = years,
#                                           tier = "1st",
#                                           stat_type = "keeper")
#
#   eng_stats_keeper_keeper_adv <- fb_season_team_stats(country = countries,
#                                                       gender = "M",
#                                                       season_end_year = years,
#                                                       tier = "1st",
#                                                       stat_type = "keeper_adv")
#
#   eng_stats_shooting<- fb_season_team_stats(country = countries,
#                                             gender = "M",
#                                             season_end_year = years,
#                                             tier = "1st",
#                                             stat_type = "shooting")
#
#   eng_stats_passing<- fb_season_team_stats(country = countries,
#                                            gender = "M",
#                                            season_end_year = years,
#                                            tier = "1st",
#                                            stat_type = "passing")
#
#   eng_stats_passing_types<- fb_season_team_stats(country = countries,
#                                                  gender = "M",
#                                                  season_end_year = years,
#                                                  tier = "1st",
#                                                  stat_type = "passing_types")
#
#   eng_stats_goal_shot_creation <- fb_season_team_stats(country = countries,
#                                                        gender = "M",
#                                                        season_end_year = years,
#                                                        tier = "1st",
#                                                        stat_type = "goal_shot_creation")
#
#   eng_stats_defense<- fb_season_team_stats(country = countries,
#                                            gender = "M",
#                                            season_end_year = years,
#                                            tier = "1st",
#                                            stat_type = "defense")
#
#   eng_stats_possession<- fb_season_team_stats(country = countries,
#                                               gender = "M",
#                                               season_end_year = years,
#                                               tier = "1st",
#                                               stat_type = "possession")
#
#   eng_stats_playing_time<- fb_season_team_stats(country = countries,
#                                                 gender = "M",
#                                                 season_end_year = years,
#                                                 tier = "1st",
#                                                 stat_type = "playing_time")
#
#   eng_stats_misc<- fb_season_team_stats(country = countries,
#                                         gender = "M",
#                                         season_end_year = years,
#                                         tier = "1st",
#                                         stat_type = "misc")
#
#
#
#   save(eng_stats_defense,
#        eng_stats_goal_shot_creation,
#        eng_stats_keeper,
#        eng_stats_keeper_keeper_adv,
#        eng_stats_misc,
#        eng_stats_passing,
#        eng_stats_passing_types,
#        eng_stats_playing_time,
#        eng_stats_possession,
#        eng_stats_shooting,
#        eng_stats_standard, file = filename)
#
# } else {
#
#   load(filename)
# }


# other idea


# or multiple teams:
urls <- c("https://fbref.com/en/squads/b8fd03ef/2021-2022/Manchester-City-Stats",
          "https://fbref.com/en/squads/18bb7c10/2021-2022/Arsenal-Stats",
          "https://fbref.com/en/squads/19538871/2021-2022/Manchester-United-Stats",
          "https://fbref.com/en/squads/b2b47a98/2021-2022/Newcastle-United-Stats",
          "https://fbref.com/en/squads/822bd0ba/2021-2022/Liverpool-Stats",
          "https://fbref.com/en/squads/d07537b9/2021-2022/Brighton-and-Hove-Albion-Stats",
          "https://fbref.com/en/squads/8602292d/2021-2022/Aston-Villa-Stats",
          "https://fbref.com/en/squads/361ca564/2021-2022/Tottenham-Hotspur-Stats",
          "https://fbref.com/en/squads/cd051869/2021-2022/Brentford-Stats",
          #"https://fbref.com/en/squads/fd962109/2021-2022/Fulham-Stats",
          "https://fbref.com/en/squads/1c781004/2021-2022/Norwich-City-Stats",
          "https://fbref.com/en/squads/47c64c55/2021-2022/Crystal-Palace-Stats",
          "https://fbref.com/en/squads/cff3d9bb/2021-2022/Chelsea-Stats",
          "https://fbref.com/en/squads/8cec06e1/2021-2022/Wolverhampton-Wanderers-Stats",
          "https://fbref.com/en/squads/7c21e445/2021-2022/West-Ham-United-Stats",
          #"https://fbref.com/en/squads/4ba7cbea/2021-2022/Bournemouth-Stats",
          "https://fbref.com/en/squads/2abfe087/2021-2022/Watford-Stats",
          #"https://fbref.com/en/squads/e4a775cb/2021-2022/Nottingham-Forest-Stats",
          "https://fbref.com/en/squads/943e8050/2021-2022/Burnley-Stats",
          "https://fbref.com/en/squads/d3fd31cc/2021-2022/Everton-Stats",
          "https://fbref.com/en/squads/a2d435b3/2021-2022/Leicester-City-Stats",
          "https://fbref.com/en/squads/5bfb9659/2021-2022/Leeds-United-Stats",
          "https://fbref.com/en/squads/33c895d4/2021-2022/Southampton-Stats"
          )


#BURNLEY https://fbref.com/en/squads/943e8050/2021-2022/Burnley-Stats
#NORWICH https://fbref.com/en/squads/1c781004/2021-2022/Norwich-City-Stats
#WATFORD https://fbref.com/en/squads/2abfe087/2021-2022/Watford-Stats


filename <- "data/raw/data_logstats.RData"

if (!file.exists(filename)) {



shooting_logs <- fb_team_match_log_stats(team_urls = urls, stat_type = "shooting")

keeper_logs <- fb_team_match_log_stats(team_urls = urls, stat_type = "keeper")

passing_logs <- fb_team_match_log_stats(team_urls = urls, stat_type = "passing")

passing_types_logs <- fb_team_match_log_stats(team_urls = urls, stat_type = "passing_types")

gca_logs <- fb_team_match_log_stats(team_urls = urls, stat_type = "gca")

defense_logs <- fb_team_match_log_stats(team_urls = urls, stat_type = "defense")

possession_logs <- fb_team_match_log_stats(team_urls = urls, stat_type = "possession")

misc_logs <- fb_team_match_log_stats(team_urls = urls, stat_type = "misc")


save(shooting_logs,
     keeper_logs,
     passing_logs,
     passing_types_logs,
     gca_logs,
     defense_logs,
     possession_logs,
     misc_logs, file = filename)

} else {

  load(filename)
}


# Data Wrangling

eng_shoots <- shooting_logs %>%
  filter(ForAgainst == "For",Comp%in%c("Premier League","Championship")) %>%
  group_by(Team) %>%
  summarise(Gls_Standard = sum(Gls_Standard, na.rm = T),
            Sh_Standard = sum(Sh_Standard, na.rm = T),
            SoT_Standard= sum(SoT_Standard, na.rm = T),
            SoT_percent_Standard= mean(SoT_percent_Standard, na.rm = T),
            G_per_Sh_Standard = mean(G_per_Sh_Standard, na.rm = T),
            G_per_SoT_Standard = mean(G_per_SoT_Standard, na.rm = T),
            Dist_Standard = mean(Dist_Standard, na.rm = T),
            FK_Standard = sum(FK_Standard, na.rm = T),
            PK_Standard = sum(PK_Standard, na.rm = T),
            PKatt_Standard = sum(PKatt_Standard, na.rm = T),
            xG_Expected  = mean(xG_Expected , na.rm = T),
            npxG_Expected  = mean(npxG_Expected , na.rm = T),
            npxG_per_Sh_Expected  = mean(npxG_per_Sh_Expected , na.rm = T),
            G_minus_xG_Expected  = sum(G_minus_xG_Expected , na.rm = T),
            `np:G_minus_xG_Expected`  = sum(`np:G_minus_xG_Expected` , na.rm = T))


eng_keeper <- keeper_logs %>%
  filter(ForAgainst == "For",Comp%in%c("Premier League","Championship")) %>%
  group_by(Team) %>%
  summarise(SoTA_Performance = sum(SoTA_Performance, na.rm = T),
            GA_Performance = sum(GA_Performance, na.rm = T),
            Saves_Performance= sum(Saves_Performance, na.rm = T),
            Save_percent_Performance= mean(Save_percent_Performance, na.rm = T),
            CS_Performance = sum(CS_Performance, na.rm = T),
            PSxG_Performance = mean(PSxG_Performance, na.rm = T),
            PSxGPlus_Minus_Performance = mean(PSxGPlus_Minus_Performance, na.rm = T),
            PKatt_Kicks = sum(PKatt_Kicks, na.rm = T),
            PKA_Kicks = sum(PKA_Kicks, na.rm = T),
            PKsv_Kicks  = sum(PKsv_Kicks , na.rm = T),
            PKm_Kicks  = sum(PKm_Kicks , na.rm = T),
            Cmp_Launched  = sum(Cmp_Launched , na.rm = T),
            Att_Launched  = sum(Att_Launched , na.rm = T),
            Cmp_percent_Launched  = mean(Cmp_percent_Launched , na.rm = T),
            `Att (GK)_Passes`  = sum(`Att (GK)_Passes` , na.rm = T),
            Thr_Passes  = sum(Thr_Passes , na.rm = T),
            Launch_percent_Passes  = mean(Launch_percent_Passes , na.rm = T),
            AvgLen_Passes  = mean(AvgLen_Passes , na.rm = T),
            Att_Goal_Kicks  = sum(Att_Goal_Kicks , na.rm = T),
            Launch_percent_Goal_Kicks  = mean(Launch_percent_Goal_Kicks , na.rm = T),
            AvgLen_Goal_Kicks  = mean(AvgLen_Goal_Kicks , na.rm = T),
            Opp_Crosses  = sum(Opp_Crosses , na.rm = T),
            Stp_Crosses  = sum(Stp_Crosses , na.rm = T),
            Stp_percent_Crosses  = mean(Stp_percent_Crosses , na.rm = T),
            Num_OPA_Sweeper  = sum(Num_OPA_Sweeper , na.rm = T),
            AvgDist_Sweeper  = mean(AvgDist_Sweeper , na.rm = T))


eng_passing <- passing_logs %>%
  filter(ForAgainst == "For",Comp%in%c("Premier League","Championship")) %>%
  group_by(Team) %>%
  summarise(Cmp_Total = sum(Cmp_Total, na.rm = T),
            Att_Total = sum(Att_Total, na.rm = T),
            Cmp_percent_Total= mean(Cmp_percent_Total, na.rm = T),
            TotDist_Total= mean(TotDist_Total, na.rm = T),
            PrgDist_Total = mean(PrgDist_Total, na.rm = T),
            Cmp_Short = sum(Cmp_Short, na.rm = T),
            Att_Short = sum(Att_Short, na.rm = T),
            Cmp_percent_Short= mean(Cmp_percent_Short, na.rm = T),
            Cmp_Medium = sum(Cmp_Medium, na.rm = T),
            Att_Medium = sum(Att_Medium, na.rm = T),
            Cmp_percent_Medium= mean(Cmp_percent_Medium, na.rm = T),
            Cmp_Long = sum(Cmp_Long, na.rm = T),
            Att_Long = sum(Att_Long, na.rm = T),
            Cmp_percent_Long= mean(Cmp_percent_Long, na.rm = T),
            Ast= sum(Ast, na.rm = T),
            xAG= mean(xAG, na.rm = T),
            xA= mean(xA, na.rm = T),
            KP= sum(KP, na.rm = T),
            Final_Third= sum(Final_Third, na.rm = T),
            PPA= sum(PPA, na.rm = T),
            CrsPA= sum(CrsPA, na.rm = T),
            PrgP= sum(PrgP, na.rm = T))


eng_passing_types <- passing_types_logs %>%
  filter(ForAgainst == "For",Comp%in%c("Premier League","Championship")) %>%
  group_by(Team) %>%
  summarise(Att = sum(Att, na.rm = T),
            Live_Pass_Types = sum(Live_Pass_Types, na.rm = T),
            Dead_Pass_Types= sum(Dead_Pass_Types, na.rm = T),
            FK_Pass_Types = sum(FK_Pass_Types, na.rm = T),
            TB_Pass_Types= sum(TB_Pass_Types, na.rm = T),
            Sw_Pass_Types = sum(Sw_Pass_Types, na.rm = T),
            Crs_Pass_Types= sum(Crs_Pass_Types, na.rm = T),
            TI_Pass_Types = sum(TI_Pass_Types, na.rm = T),
            CK_Pass_Types= sum(CK_Pass_Types, na.rm = T),
            In_Corner_Kicks = sum(In_Corner_Kicks, na.rm = T),
            Out_Corner_Kicks= sum(Out_Corner_Kicks, na.rm = T),
            Str_Corner_Kicks = sum(Str_Corner_Kicks, na.rm = T),
            Cmp_Outcomes= sum(Cmp_Outcomes, na.rm = T),
            Off_Outcomes = sum(Off_Outcomes, na.rm = T),
            Blocks_Outcomes= sum(Blocks_Outcomes, na.rm = T))


eng_gca <- gca_logs %>%
  filter(ForAgainst == "For",Comp%in%c("Premier League","Championship")) %>%
  group_by(Team) %>%
  summarise(SCA_SCA_Types = sum(SCA_SCA_Types, na.rm = T),
            PassLive_SCA_Types = sum(PassLive_SCA_Types, na.rm = T),
            PassDead_SCA_Types= sum(PassDead_SCA_Types, na.rm = T),
            TO_SCA_Types = sum(TO_SCA_Types, na.rm = T),
            Sh_SCA_Types= sum(Sh_SCA_Types, na.rm = T),
            Fld_SCA_Types = sum(Fld_SCA_Types, na.rm = T),
            Def_SCA_Types= sum(Def_SCA_Types, na.rm = T),
            GCA_GCA_Types = sum(GCA_GCA_Types, na.rm = T),
            PassLive_GCA_Types= sum(PassLive_GCA_Types, na.rm = T),
            PassDead_GCA_Types = sum(PassDead_GCA_Types, na.rm = T),
            TO_GCA_Types= sum(TO_GCA_Types, na.rm = T),
            Sh_GCA_Types = sum(Sh_GCA_Types, na.rm = T),
            Fld_GCA_Types= sum(Fld_GCA_Types, na.rm = T),
            Def_GCA_Types = sum(Def_GCA_Types, na.rm = T))

eng_defense <- defense_logs %>%
  filter(ForAgainst == "For",Comp%in%c("Premier League","Championship")) %>%
  group_by(Team) %>%
  summarise(Tkl_Tackles = sum(Tkl_Tackles, na.rm = T),
            TklW_Tackles = sum(TklW_Tackles, na.rm = T),
            `Def 3rd_Tackles`= sum(`Def 3rd_Tackles`, na.rm = T),
            `Mid 3rd_Tackles` = sum(`Mid 3rd_Tackles`, na.rm = T),
            `Att 3rd_Tackles`= sum(`Att 3rd_Tackles`, na.rm = T),
            Tkl_Challenges = sum(Tkl_Challenges, na.rm = T),
            Att_Challenges= sum(Att_Challenges, na.rm = T),
            Tkl_percent_Challenges = mean(Tkl_percent_Challenges, na.rm = T),
            Lost_Challenges= sum(Lost_Challenges, na.rm = T),
            Blocks_Blocks = sum(Blocks_Blocks, na.rm = T),
            Sh_Blocks= sum(Sh_Blocks, na.rm = T),
            Pass_Blocks = sum(Pass_Blocks, na.rm = T),
            Int= sum(Int, na.rm = T),
            `Tkl+Int` = sum(`Tkl+Int`, na.rm = T),
            Clr = sum(Clr, na.rm = T),
            Err = sum(Err, na.rm = T))


eng_possession <- possession_logs %>%
  filter(ForAgainst == "For",Comp%in%c("Premier League","Championship")) %>%
  group_by(Team) %>%
  summarise(Poss = mean(Poss, na.rm = T),
            Touches_Touches = sum(Touches_Touches, na.rm = T),
            `Def Pen_Touches`= sum(`Def Pen_Touches`, na.rm = T),
            `Def 3rd_Touches` = sum(`Def 3rd_Touches`, na.rm = T),
            `Mid 3rd_Touches`= sum(`Mid 3rd_Touches`, na.rm = T),
            `Att 3rd_Touches`= sum(`Att 3rd_Touches`, na.rm = T),
            `Att Pen_Touches`= sum(`Att Pen_Touches`, na.rm = T),
            Live_Touches = sum(Live_Touches, na.rm = T),
            Att_Take_Ons= sum(Att_Take_Ons, na.rm = T),
            Succ_Take_Ons = sum(Succ_Take_Ons, na.rm = T),
            Succ_percent_Take_Ons= mean(Succ_percent_Take_Ons, na.rm = T),
            Tkld_Take_Ons = sum(Tkld_Take_Ons, na.rm = T),
            Tkld_percent_Take_Ons= mean(Tkld_percent_Take_Ons, na.rm = T),
            Carries_Carries = sum(Carries_Carries, na.rm = T),
            TotDist_Carries= mean(TotDist_Carries, na.rm = T),
            PrgDist_Carries= mean(PrgDist_Carries, na.rm = T),
            PrgC_Carries = sum(PrgC_Carries, na.rm = T),
            Final_Third_Carries = sum(Final_Third_Carries, na.rm = T),
            CPA_Carries = sum(CPA_Carries, na.rm = T),
            Mis_Carries = sum(Mis_Carries, na.rm = T),
            Dis_Carries = sum(Dis_Carries, na.rm = T),
            Rec_Receiving = sum(Rec_Receiving, na.rm = T),
            PrgR_Receiving = sum(PrgR_Receiving, na.rm = T))



eng_misc <- misc_logs %>%
  filter(ForAgainst == "For",Comp%in%c("Premier League","Championship")) %>%
  group_by(Team) %>%
  summarise(CrdY_Performance = sum(CrdY_Performance, na.rm = T),
            CrdR_Performance = sum(CrdR_Performance, na.rm = T),
             `2CrdY_Performance` = sum(`2CrdY_Performance`, na.rm = T),
            Fls_Performance= sum(Fls_Performance, na.rm = T),
            Fld_Performance = sum(Fld_Performance, na.rm = T),
            Off_Performance= sum(Off_Performance, na.rm = T),
            Crs_Performance = sum(Crs_Performance, na.rm = T),
            Int_Performance= sum(Int_Performance, na.rm = T),
            TklW_Performance = sum(TklW_Performance, na.rm = T),
            PKwon_Performance= sum(PKwon_Performance, na.rm = T),
            PKcon_Performance= sum(PKcon_Performance, na.rm = T),
            OG_Performance = sum(OG_Performance, na.rm = T),
            Recov_Performance = sum(Recov_Performance, na.rm = T),
            Won_Aerial_Duels = sum(Won_Aerial_Duels, na.rm = T),
            Lost_Aerial_Duels = sum(Lost_Aerial_Duels, na.rm = T),
            Won_percent_Aerial_Duels = mean(Won_percent_Aerial_Duels, na.rm = T))



eng_otherstats <- eng_shoots %>%
  left_join(eng_keeper, by = "Team") %>%
  left_join(eng_passing, by = "Team") %>%
  left_join(eng_passing_types, by = "Team") %>%
  left_join(eng_gca, by = "Team") %>%
  left_join(eng_defense, by = "Team") %>%
  left_join(eng_possession, by = "Team") %>%
  left_join(eng_misc, by = "Team")

write.csv(eng_otherstats, file = "data/processed/eng_otherstats.csv")
# sum(is.na(eng_shoots))
#
# sapply(eng_shoots, function(x) sum(is.na(x)))
# last_365 <- fb_player_scouting_report(player_url = "https://fbref.com/en/players/b25e597e/Rodrigo-Holgado",
#                                       pos_versus = "primary",
#                                       league_comp_name = "Primera A")




rm(list = ls())



