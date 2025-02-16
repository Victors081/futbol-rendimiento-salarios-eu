
library(tidyverse)
library(ggthemes)
library(scales)
library(BradleyTerry2)
library(readr)

# BTL Properties

# - Win expectations all come down to the difference in abilities
#
# - An equal difference, $\delta_A - \delta_B$, implies the same win expectation no matter the individual abilities of $A$ and $B$
#
#   - Expectations are even when $\delta_A -\delta_B = 0$
#
#   - Roughly +10% for every added difference in ability of 0.4

#
# BT <- function(delta){
#   exp(delta) / (1 + exp(delta))
# }
#
# data_2 <- data.frame(
#   ability = seq(-2, 2, by = 0.1),
#   p = BT(seq(-2, 2, by = 0.1))
# )
#
# data_2 %>%
#   ggplot(aes(y = p * 100, x = ability)) +
#   geom_point(size = 2, col = "#1792d0") +
#   geom_line(size = 2, col = "#1792d0") +
#   theme_hc() +
#   geom_hline(yintercept = 50, col = "red") +
#   scale_y_continuous("BTL Win Expectation", breaks = scales::pretty_breaks(n = 10)) +
#   scale_x_continuous("Ability Difference", breaks = scales::pretty_breaks(n = 10))
#

#


data <- read_csv(file = "input/EPL_2122.csv")



data <- data %>%
  dplyr::mutate(
    outcome = as.numeric(FTHG > FTAG),
    HomeTeam = factor(HomeTeam, levels = unique(c(HomeTeam, AwayTeam))),
    AwayTeam = factor(AwayTeam, levels = levels(HomeTeam))
  )


fit <- BTm(outcome, HomeTeam, AwayTeam,
           data = data)


summary(fit)

# Abilities

abilities <- BTabilities(fit) # Matrix of ability and StdErr

abilities <- data.frame(
  Team = rownames(abilities),
  Ability = abilities[,1],
  SE = abilities[,2]
) %>%
  remove_rownames()



save(abilities, file = "output/abilities.RData")


abilities$Team <- factor(abilities$Team,
                         levels = abilities$Team[order(abilities$Ability)],
                         order = T)

abilities %>%
  ggplot(aes(y = Ability, ymin = Ability - 2 * SE, ymax = Ability + 2 * SE, x = Team)) +
  geom_pointrange(col = "#e5952c") +
  coord_flip() +
  theme_hc() + theme(text = element_text(size = 16)) +
  scale_y_continuous("Abilities") +
  scale_x_discrete("") +
  geom_hline(yintercept = 0, col = "red") +
  ggtitle("EPL Team Abilities - 2021/2022")



# Home Advantage

# It is generally thought that a team gets a boost in their win expectations when playing at home.
#
# To introduce a home advantage covariate, we need to restructure out dataset and add a covariate for each team factor.


data$Home <- data.frame(
  Team = data$HomeTeam,
  at.home = 1
)

data$Away <- data.frame(
  Team = data$AwayTeam,
  at.home = 0
)


fit <- BTm(outcome, Home, Away,
           data = data,
           id = "Team",
           formula = ~ Team + at.home
)

summary(fit)

# Problem

# 1. What does the `at.home` effect suggest about the home advantage in the 2022-2023 EPL?
#
# 2. How do the abilities change compared to our previous fit without this advantage?


summary(fit)$coef["at.home",]


# Solution: Change In Abilities

abilities2 <- BTabilities(fit) # Matrix of ability and StdErr

abilities2 <- data.frame(
  Team = rownames(abilities2),
  Ability = abilities2[,1],
  SE = abilities2[,2]
)

abilities$Type <- "Without Home Adv"
abilities2$Type <- "With Home Adv"

order <- levels(abilities$Team)

abilities <- rbind(abilities, abilities2)

abilities$Team <- factor(abilities$Team,
                         levels = order,
                         order = T)

abilities %>%
  ggplot(aes(y = Ability, ymin = Ability - 2 * SE, ymax = Ability + 2 * SE, x = Team)) +
  geom_pointrange(aes(col = Type, group = Type), position = position_dodge(0.1)) +
  coord_flip() +
  scale_colour_tableau(name = "") +
  theme_hc() + theme(text = element_text(size = 16)) +
  scale_y_continuous("Abilities") +
  scale_x_discrete("") +
  geom_hline(yintercept = 0, col = "red") +
  ggtitle("EPL Team Abilities - 2022/2023")


# Reverse Home Advantage?

# - Our results suggest that playing at home is actually a _disadvantage_.
#
# - This goes against all conventional thinking!
#
# - Could we have made a mistake somewhere?

# Problem: Ties

# We have treated ties (or draws) as losses for the home team. Have a look at the frequency of ties in the EPL data. Could our assumption be a problem?

  # Solution: Frequency of Ties

data$ties <- data$FTHG == data$FTAG

data %>%
  ggplot(aes(x = ties)) +
  geom_bar(fill = "#e5952c") +
  theme_hc()



# Problem: Effect of Ties

# - Fit a BTM + Home advantage excluding matches with ties
#
# - Determine the estimate of the home advantage
#
# - What do you conclude about the importance of handling ties for our model?


  # Solution: Effect of Ties

  data$Home <- data$Away <- NULL

data <- data %>% filter(!ties)

data$Home <- data.frame(
  Team = data$HomeTeam,
  at.home = 1
)

data$Away <- data.frame(
  Team = data$AwayTeam,
  at.home = 0
)

fit <- BTm(outcome, Home, Away,
           data = data,
           id = "Team",
           formula = ~ Team + at.home
)

summary(fit)

summary(fit)$coef["at.home",]


abilities3 <- BTabilities(fit) # Matrix of ability and StdErr

abilities3 <- data.frame(
  Team = rownames(abilities3),
  Ability = abilities3[,1],
  SE = abilities3[,2]
)

abilities3$Team <- factor(abilities3$Team,
                         levels = abilities3$Team[order(abilities3$Ability)],
                         order = T)

abilities3 %>%
  ggplot(aes(y = Ability, ymin = Ability - 2 * SE, ymax = Ability + 2 * SE, x = Team)) +
  geom_pointrange(col = "#e5952c") +
  coord_flip() +
  theme_hc() + theme(text = element_text(size = 16)) +
  scale_y_continuous("Abilities") +
  scale_x_discrete("") +
  geom_hline(yintercept = 0, col = "red") +
  ggtitle("EPL Team Abilities - 2021/2022")
