
library(worldfootballR)
library(dplyr)
library(rvest)
library(tidyr)


paises <- c("England","Spain","Germany","France","Italy")


meta_df <- read.csv(url("https://raw.githubusercontent.com/JaseZiv/worldfootballR_data/master/raw-data/transfermarkt_leagues/main_comp_seasons.csv"),
                    stringsAsFactors = F)




league_urls <- meta_df %>%
  filter(country %in% paises, season_start_year == 2021)

years_stats <- vector(mode = "list", length = length(paises))

for (i in seq_along(paises)) {

  xdf <- league_urls %>%
    filter(country == paises[i])

  url <- xdf$season_urls


  season_page <- xml2::read_html(url)

  xd <- season_page %>%
    rvest::html_nodes("#yw1 table") %>%
    html_table() %>%
    as.data.frame()


  xd2 <- xd[2:7]

  names(xd2) <- names(xd)[3:length(names(xd))]


  years_stats[[i]] <- xd2[-1,]




}


names(years_stats)<- paises


list2env(years_stats,globalenv())


England_age_marketvalue <- England %>%
  mutate(Foreigners = as.integer(Foreigners))%>%
  mutate(Total.market.value = gsub("€","",x = Total.market.value))%>%
  separate(Total.market.value,
           into = c("Total.market.value","Total.market.value.units"),
           sep = "(?<=[0-9])(?=[A-Za-z])"
  ) %>%
  mutate(Total.market.value = as.numeric(Total.market.value),
         Total.market.value = case_when(
           Total.market.value.units == "bn" ~ Total.market.value*1000,
           TRUE ~ Total.market.value
         ))%>%
  mutate(ø.market.value = gsub("€","",x = ø.market.value))%>%
  separate(ø.market.value,
           into = c("ø.market.value","ø.market.value.units"),
           sep = "(?<=[0-9])(?=[A-Za-z])"
  ) %>%
  mutate(ø.market.value = as.numeric(ø.market.value),
         ø.market.value = case_when(
           ø.market.value == "bn" ~ ø.market.value*1000,
           TRUE ~ ø.market.value
         )) %>%
  select(-ends_with("units"))

save(England_age_marketvalue, file = "output/age_marketvalue.RData")

rm(list = ls())
