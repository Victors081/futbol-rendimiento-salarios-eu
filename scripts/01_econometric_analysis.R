# se cargan los paquetes

library(tidyverse)
library(readxl)
library(writexl)

# Se cargan los datos


names_eq_eng <- read_excel(path = "output/names_eq_england.xlsx",sheet = "T2122")

load("output/abilities.RData")

load("output/eng_otherstats.RData")

load(file = "output/results_league.RData")

load(file = "output/Ranking_wages_fifa.RData")

load(file ="output/transferfee.RData")

load(file =  "output/age_marketvalue.RData")

# Se ajustan los nombres de los equipos

abilities <- abilities %>%
  arrange(Team) %>%
  left_join(names_eq_eng %>%
              select(`Match-Match`,fbref_names), by = c("Team" = "Match-Match")) %>%
  mutate(NAME = fbref_names) %>%
  select(-Team,-fbref_names) %>%
  select(NAME, everything())


eng_otherstats <- eng_otherstats %>%
  arrange(Team) %>%
  left_join(names_eq_eng%>%
              select(fbref_names), by = c("Team" = "fbref_names")) %>%
  mutate(NAME = Team) %>%
  select(-Team) %>%
  select(NAME, everything())


eng_seasontable <- eng_seasontable %>%
  arrange(Squad) %>%
  left_join(names_eq_eng%>%
              select(transfermk_names, fbref_names), by = c("Squad" = "transfermk_names")) %>%
  mutate(NAME = fbref_names) %>%
  select(-fbref_names,-Squad) %>%
  select(NAME, everything()) %>%
  mutate(NAME = names_eq_eng$fbref_names)


Ranking_wages_fifa <- Ranking_wages_fifa %>%
  arrange(Equipo) %>%
  left_join(names_eq_eng%>%
              select(sfifa, fbref_names), by = c("Equipo" = "sfifa")) %>%
  mutate(NAME = fbref_names) %>%
  select(-fbref_names,-Equipo, -ID) %>%
  select(NAME, everything())

England_transferfee <- England_transferfee %>%
  arrange(team_name) %>%
  left_join(names_eq_eng%>%
              select(transfermk_names2, fbref_names), by = c("team_name" = "transfermk_names2")) %>%
  mutate(NAME = fbref_names) %>%
  select(-fbref_names,-team_name) %>%
  select(NAME, everything())

England_age_marketvalue <- England_age_marketvalue %>%
  arrange(name) %>%
  left_join(names_eq_eng%>%
              select(transfermk_names2, fbref_names), by = c("name" = "transfermk_names2")) %>%
  mutate(NAME = fbref_names) %>%
  select(-fbref_names,-name) %>%
  select(NAME, everything())

# Se unen los datos


data_eng <- left_join(abilities, eng_otherstats, by = "NAME") %>%
  left_join(eng_seasontable, by = "NAME") %>%
  left_join(Ranking_wages_fifa, by = "NAME")%>%
  left_join(England_transferfee, by = "NAME")%>%
  left_join(England_age_marketvalue, by = "NAME")


# Se analizan los datos



options(scipen = 999)

# Se cargan las funciones importantes a usar

source("setup/functions.R")

# Se cargan los paquetes

library(stargazer)
library(corrplot)
library(Hmisc)
library(lmtest)
library(psych)
library(car)
library(leaps)
library(tseries)
library(olsrr)
library(GGally)

data_reg <- data_eng %>%
  mutate(dependent=Ability) %>%
  select(-W_mp, -WL_ratio,
         -Pts.MP,-Pts,
         -Total.stats, -NAME, -Ability, -SE)%>%
  select(dependent,
         #SoT_percent_Standard,
         #Gls_Standard,
         Sh_Standard,
         #PK_Standard,
         GD,
         #G_per_Sh_Standard,
         Cmp_Total,
         TklW_Performance,
         #Dist_Standard,
         #Ast,
         transfer_fee,
         `ø.age`,
         #Total.market.value,
         Gini_overall,
         Gini_wages,
         #Theil_overall,
         #Theil_wages,
         #Total_wages_month,
         #Overall.rating_mean
         )

ggpairs(data_reg)

# Se crea el modelo


xs <- data_reg %>%
  select(-dependent) %>%
  as.matrix()

y <- data_reg %>%
  select(dependent) %>%
  as.matrix()

name_y <- "dependent"

max.model <- lm(dependent ~ ., data = data_reg)

summary(max.model)

vif(max.model)

remueve.VIF.grande(max.model,u=10)

# Selección de los mejores predictores


# Forward y adjr2 ----


modelo1 <- regsubsets(x = xs, y = y , nvmax = 1000, method = "forward")

plot(modelo1, scale = "adjr2", main = "Rˆ2 ajustado")

## Reordering variables and trying again:

m1 <- summary(modelo1)

index <- 1:length(modelo1$xnames)

n.index <- index[m1$which[which.max(m1$adjr2), ] == 1]



#n.index <- index[m1$which[6, ] == 1]

vars.modelo1 <- modelo1$xnames[index[m1$which[which.max(m1$adjr2),] == 1]]

#vars.modelo1 <- modelo1$xnames[index[m1$which[6,] == 1]]


vars.modelo1 <- vars.modelo1[-1]


formula.modelo1 <- as.formula(paste(name_y," ~ ", paste(vars.modelo1,
                                                        collapse = " + "), sep = ""))

modelo1 <- lm(formula.modelo1, data = data_reg)


summary(modelo1)

# Potenciales problemas econometricos

res.modelo1 <- residuals(modelo1)

## Pruebas de normalidad de los errores

### test de shapiro-wilk

shapiro.test(res.modelo1)

### test de jarque bera

jarque.bera.test(res.modelo1)

### test Kolmogorov-Smirnov
ks.test(res.modelo1, "pnorm")

## Prueba de heterocedasticidad de los errores

# prueba de Breusch-Pagan studentizada
bptest(modelo1, studentize = TRUE)

# Prueba VIF

vif(modelo1)

# Correcion multicolinealidad

remueve.VIF.grande(modelo = modelo1,u = 5)

# Se remueven las variables no significativas

modelo1.a <- remueve.no.sinifica(modelo1, 0.05)

summary(modelo1.a)

# Potenciales problemas econometricos

res.modelo1.a <- residuals(modelo1.a)

## Pruebas de normalidad de los errores

### test de shapiro-wilk

shapiro.test(res.modelo1.a)

### test de jarque bera

jarque.bera.test(res.modelo1.a)

### test Kolmogorov-Smirnov
ks.test(res.modelo1.a, "pnorm")

## Prueba de heterocedasticidad de los errores

# prueba de Breusch-Pagan studentizada
bptest(modelo1.a, studentize = TRUE)

# Prueba VIF

vif(modelo1.a)


which.max(res.modelo1.a)
res.modelo1.a[which.max(res.modelo1.a)]

res.modelo1.a[which.min(res.modelo1.a)]


# Forward y AIC ----

modelO2 <- ols_step_forward_aic(max.model)

# se extraen las variables del mejor modelo según el
# algoritmo

vars.modelO2 <- modelO2$predictors

formula.modelo2 <- as.formula(paste(name_y," ~ ", paste(vars.modelO2,
                                                        collapse = " + "), sep = ""))
# se estima el modelo con la fórmula construida

modelo2 <- lm(formula.modelo2, data = data_reg)

summary(modelo2)

# Potenciales problemas econometricos

res.modelo2 <- residuals(modelo2)

## Pruebas de normalidad de los errores

### test de shapiro-wilk

shapiro.test(res.modelo2)

### test de jarque bera

jarque.bera.test(res.modelo2)

### test Kolmogorov-Smirnov
ks.test(res.modelo2, "pnorm")

## Prueba de heterocedasticidad de los errores

# prueba de Breusch-Pagan studentizada
bptest(modelo2, studentize = TRUE)

# Prueba VIF

vif(modelo2)


modelo2.a <- remueve.no.sinifica(modelo2, 0.05)

summary(modelo2.a)

#
# fwd.model.2 <- ols_step_forward_p(max.model)
#
# fwd.model.2
#


# Backward y adjr2 ----


modelo3 <- regsubsets(x = xs, y = y , nvmax = 1000, method = "backward")

plot(modelo3, scale = "adjr2", main = "Rˆ2 ajustado")

## Reordering variables and trying again:

m3 <- summary(modelo3)

index <- 1:length(modelo3$xnames)

n.index <- index[m3$which[which.max(m3$adjr2), ] == 1]

vars.modelo3 <- modelo3$xnames[index[m3$which[which.max(m3$adjr2),] == 1]]

vars.modelo3 <- vars.modelo3[-1]


formula.modelo3 <- as.formula(paste(name_y," ~ ", paste(vars.modelo3,
                                                        collapse = " + "), sep = ""))

modelo3 <- lm(formula.modelo3, data = data_reg)


summary(modelo3)

# Potenciales problemas econometricos

res.modelo3 <- residuals(modelo3)

## Pruebas de normalidad de los errores

### test de shapiro-wilk

shapiro.test(res.modelo3)

### test de jarque bera

jarque.bera.test(res.modelo3)

### test Kolmogorov-Smirnov
ks.test(res.modelo3, "pnorm")

## Prueba de heterocedasticidad de los errores

# prueba de Breusch-Pagan studentizada
bptest(modelo3, studentize = TRUE)

# Prueba VIF

vif(modelo3)


# Se remueven las variables no significativas

modelo3.a <- remueve.no.sinifica(modelo3, 0.05)

summary(modelo3.a)

# Potenciales problemas econometricos

res.modelo3.a <- residuals(modelo3.a)

## Pruebas de normalidad de los errores

### test de shapiro-wilk

shapiro.test(res.modelo3.a)

### test de jarque bera

jarque.bera.test(res.modelo3.a)

### test Kolmogorov-Smirnov
ks.test(res.modelo3.a, "pnorm")

## Prueba de heterocedasticidad de los errores

# prueba de Breusch-Pagan studentizada
bptest(modelo3.a, studentize = TRUE)

# Prueba VIF

vif(modelo3.a)



# Backward y AIC ----

modelO4 <- ols_step_backward_aic(max.model)

# se extraen las variables del mejor modelo según el
# algoritmo

vars.modelO4 <- modelO4$predictors

formula.modelo4 <- as.formula(paste(name_y," ~ ", paste(vars.modelO4,
                                                        collapse = " + "), sep = ""))
# se estima el modelo con la fórmula construida

modelo4 <- lm(formula.modelo4, data = data_reg)

summary(modelo4)

# Potenciales problemas econometricos

res.modelo4 <- residuals(modelo4)

## Pruebas de normalidad de los errores

### test de shapiro-wilk

shapiro.test(res.modelo4)

### test de jarque bera

jarque.bera.test(res.modelo4)

### test Kolmogorov-Smirnov
ks.test(res.modelo4, "pnorm")

## Prueba de heterocedasticidad de los errores

# prueba de Breusch-Pagan studentizada
bptest(modelo4, studentize = TRUE)

# Prueba VIF

vif(modelo4)


modelo4.a <- remueve.no.sinifica(modelo4, 0.05)

summary(modelo4.a)


# Potenciales problemas econometricos

res.modelo4.a <- residuals(modelo4.a)

## Pruebas de normalidad de los errores

### test de shapiro-wilk

shapiro.test(res.modelo4.a)

### test de jarque bera

jarque.bera.test(res.modelo4.a)

### test Kolmogorov-Smirnov
ks.test(res.modelo4.a, "pnorm")

## Prueba de heterocedasticidad de los errores

# prueba de Breusch-Pagan studentizada
bptest(modelo4.a, studentize = TRUE)

# Prueba VIF

vif(modelo4.a)


#
# fwd.model.2 <- ols_step_forward_p(max.model)
#
# fwd.model.2
#

# Both y adjr2 ----

modelo5 <- regsubsets(x = xs, y = y , nvmax = 1000, method = "seqrep")

plot(modelo5, scale = "adjr2", main = "Rˆ2 ajustado")

## Reordering variables and trying again:

m5 <- summary(modelo5)

index <- 1:length(modelo5$xnames)

n.index <- index[m5$which[which.max(m5$adjr2), ] == 1]

vars.modelo5 <- modelo5$xnames[index[m5$which[which.max(m5$adjr2),] == 1]]

vars.modelo5 <- vars.modelo5[-1]


formula.modelo5 <- as.formula(paste(name_y," ~ ", paste(vars.modelo5,
                                                        collapse = " + "), sep = ""))

modelo5 <- lm(formula.modelo5, data = data_reg)


summary(modelo5)

# Potenciales problemas econometricos

res.modelo5 <- residuals(modelo5)

## Pruebas de normalidad de los errores

### test de shapiro-wilk

shapiro.test(res.modelo5)

### test de jarque bera

jarque.bera.test(res.modelo5)

### test Kolmogorov-Smirnov
ks.test(res.modelo5, "pnorm")

## Prueba de heterocedasticidad de los errores

# prueba de Breusch-Pagan studentizada
bptest(modelo5, studentize = TRUE)

# Prueba VIF

vif(modelo5)


# Se remueven las variables no significativas

modelo5.a <- remueve.no.sinifica(modelo5, 0.05)

summary(modelo5.a)

# Potenciales problemas econometricos

res.modelo5.a <- residuals(modelo5.a)

## Pruebas de normalidad de los errores

### test de shapiro-wilk

shapiro.test(res.modelo5.a)

### test de jarque bera

jarque.bera.test(res.modelo5.a)

### test Kolmogorov-Smirnov
ks.test(res.modelo5.a, "pnorm")

## Prueba de heterocedasticidad de los errores

# prueba de Breusch-Pagan studentizada
bptest(modelo5.a, studentize = TRUE)

# Prueba VIF

vif(modelo5.a)



# Both y AIC ----

modelO6 <- ols_step_both_aic(max.model)

# se extraen las variables del mejor modelo según el
# algoritmo

vars.modelO6 <- modelO6$predictors

formula.modelo6 <- as.formula(paste(name_y," ~ ", paste(vars.modelO6,
                                                        collapse = " + "), sep = ""))
# se estima el modelo con la fórmula construida

modelo6 <- lm(formula.modelo6, data = data_reg)

summary(modelo6)

# Potenciales problemas econometricos

res.modelo6 <- residuals(modelo6)

## Pruebas de normalidad de los errores

### test de shapiro-wilk

shapiro.test(res.modelo6)

### test de jarque bera

jarque.bera.test(res.modelo6)

### test Kolmogorov-Smirnov
ks.test(res.modelo6, "pnorm")

## Prueba de heterocedasticidad de los errores

# prueba de Breusch-Pagan studentizada
bptest(modelo6, studentize = TRUE)

# Prueba VIF

vif(modelo6)


modelo6.a <- remueve.no.sinifica(modelo6, 0.05)

summary(modelo6.a)


# Potenciales problemas econometricos

res.modelo6.a <- residuals(modelo6.a)

## Pruebas de normalidad de los errores

### test de shapiro-wilk

shapiro.test(res.modelo6.a)

### test de jarque bera

jarque.bera.test(res.modelo6.a)

### test Kolmogorov-Smirnov
ks.test(res.modelo6.a, "pnorm")

## Prueba de heterocedasticidad de los errores

# prueba de Breusch-Pagan studentizada
bptest(modelo6.a, studentize = TRUE)

# Prueba VIF

vif(modelo6.a)



