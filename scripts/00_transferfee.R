library(worldfootballR)
library(dplyr)

paises <- c("England","Spain","Germany","France","Italy")


transfer_list <- vector(mode = "list",length = length(paises))


i = 1

#for (i in seq_along(paises)) {

  team_urls <- tm_league_team_urls(country_name = paises[i], start_year = 2021)

  transfer_list[[i]] <- tm_team_transfers(team_url = team_urls, transfer_window = "all")

#}


England <- transfer_list[[i]]


England_transferfee <- England %>%
  filter(!is.na(transfer_fee) & transfer_fee != 0 & transfer_type=="Arrivals")  %>%
  group_by(team_name) %>%
  summarise(transfer_fee = sum(transfer_fee, na.rm = T))


save(England_transferfee, file = "output/transferfee.RData")

rm(list = ls())


