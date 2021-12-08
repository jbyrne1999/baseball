# read in statcast csvs
library(dplyr)
library(knitr)
library(tidyverse)
setwd("C:/Users/Jack/Desktop/School Stuff/Sports Analytics/Final Project/2021 Season Data")
files = list.files(pattern = "*.csv")
library(data.table)
statcast = rbindlist(lapply(files, fread))
# remove repeat columns (pitcher and catcher ids)
statcast <- statcast[,-c(60,61)]

#remove all pitches that did not result in a ball in play
statcast$events <- na_if(statcast$events, "null")
statcast <- statcast[!(is.na(statcast$events) | statcast$events==""), ]

# get batting order
tto <- statcast %>%
  group_by(game_pk, inning_topbot) %>%
  arrange(at_bat_number) %>%
  mutate(PAnumTeam = row_number(), BattingOrder = PAnumTeam %% 9)
tto <- mutate(tto, RealBattingOrder = ifelse(BattingOrder == 0, 9, BattingOrder))

# get time through order
tto <- tto %>%
  group_by(game_pk,pitcher,RealBattingOrder) %>%
  arrange(at_bat_number) %>%
  #count the amount of times each batter pitcher combo happens for every game 
  mutate(time_through_order = row_number(batter), total_through_order = n())

# get most number of times each batter saw a pitcher that game (ex 4 ab- 2 vs starter, 1 vs rp, 1 vs closer:
#  then faced most would = 2)
tto <- tto %>%
  group_by(game_pk, RealBattingOrder) %>%
  mutate(facedMost = max(total_through_order))

# test
fletch <- filter(tto, game_date == "2021-10-02", batter == 664058)

# get everyones first abs
tableau <- filter(tto, PAnumTeam == RealBattingOrder)
# calculate avg number of times each spot in the order faced the starter
tableau <- tableau %>%
  group_by(RealBattingOrder) %>%
  summarise(avgFaced = mean(facedMost))

# filter to third time thru
threePlus <- filter(tto, time_through_order == 3)
# calculate woba for each time thru order (for starter)
timesThru <- tto %>%
  group_by(time_through_order) %>%
  summarise(wOBA = sum(na.omit(woba_value))/sum(na.omit(woba_denom)))

# calculate third time thru woba for each spot in batting order and PAs facing starter 3rd time
ott <-  threePlus %>%
  group_by(time_through_order, RealBattingOrder) %>%
  summarise(wOBA = sum(na.omit(woba_value))/sum(na.omit(woba_denom)),
            N = n())
# ott <- filter(OrderTimesThru, time_through_order == 3)
# calculate weight of each spot
ott <- mutate(ott, mult = wOBA*N)
# confirm that this number equals the overall 3rd time through woba
weightedAVG = sum(ott$mult)/sum(ott$N)

# write.csv(ott, "C:/Users/Jack/Desktop/School Stuff/Sports Analytics/Final Project/ThirdTimeThruOpener.csv")

thisgame <- filter(tto, game_pk == 632169)

tto <- tto %>%
  group_by(game_pk, inning_topbot) %>%
  mutate(firstAB = min(at_bat_number))

# mark if pitcher started the game
tto <- tto %>%
  group_by(game_pk, inning_topbot, pitcher) %>%
  mutate(firstABpitcher = min(at_bat_number),
         started = ifelse(firstABpitcher == firstAB, T, F))

# count batters faced
tto <- tto %>%
  group_by(game_pk, pitcher) %>%
  mutate(bf = n())

# mark for opener and bulk guy/starter
tto <- tto %>%
  group_by(game_pk, inning_topbot) %>%
  mutate(open = ifelse((started == T) & (bf < 7), 1, 2),
         opener = min(open),
         bulk = ifelse(bf == max(bf), T, F))

# add in pitcher ID for bulk/starter
tto <- mutate(tto, bulkPitcher = ifelse(bulk == T, pitcher, -69))

# format data for model
mod <- tto %>%
  group_by(game_pk, inning_topbot) %>%
  summarise(GameRunsAllowed = ifelse(inning_topbot == "Top", max(away_score), max(home_score)),
            opener = min(opener), bulk = max(bulkPitcher), home_team, away_team)
# create only one row for each game
mod <- mod[!duplicated(mod),]
# make it so that runs allowed works for home and away team
mod <- mutate(mod, battingTeam = ifelse(inning_topbot == "Top", away_team, home_team),
              pitchingTeam = ifelse(inning_topbot == "Top", home_team, away_team))
# set up correctly
mod <- dplyr::select(mod, game_pk, inning_topbot, GameRunsAllowed, opener, battingTeam, pitchingTeam, bulk)

checker <- filter(mod, opener == 1)
teams <- checker %>%
  group_by(pitchingTeam) %>%
  summarise(timesUsed = n())

# format data
mod$opener <- as.factor(mod$opener)
mod$bulk <- as.factor(mod$bulk)

library(lme4)
library(nlme)
library(caret)
library(MASS)

# model
fit <- lm(GameRunsAllowed ~ opener + pitchingTeam+battingTeam+bulk, data = mod)
summary(fit)

# create data for predicting if every game used an opener
predictData <- mod
predictData$opener = 1
predictData$opener <- as.factor(predictData$opener)
predictData$bulk <- as.factor(predictData$bulk)

mod$pred <- predict(fit, newdata = predictData, type = "response")
mod$better <- mod$GameRunsAllowed - mod$pred
# difference in runs allowed for using an opener vs not (avg game)
# positive means less runs per game (.5 = lowers runs allowed per game by .5)
mean(mod$better)

# 
# # calculate estimates for starting vector in nlme model
# est <- nls(GameRunsAllowed ~ SSlogis(opener, Asym, xmid, scal), data = mod)
# summary(est)
# 
# # create starting vector
# startvec <- c(coef(est))
# # run model
# fit <- nlme(GameRunsAllowed ~ SSlogis(opener, Asym, xmid, scal),
#             data = df,
#             fixed = Asym + xmid + scal ~ 1,
#             random = Asym ~ 1,
#             groups = ~ pitchingTeam + battingTeam,
#             start = startvec)
# summary(fit)

openers <- filter(tto, opener == 1)

check <- filter(tto, game_pk == 633337)
