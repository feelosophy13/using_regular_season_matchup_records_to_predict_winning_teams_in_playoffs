## load libraries
library(tidyverse)
library(nbastatR)
library(knitr)


## get all regular season game logs
rs_games <- get_game_logs(seasons=1980:2017, 
                         result_type='team',
                         season_types='Regular Season')

## get all playoffs season game logs
ps_games <- get_game_logs(seasons=1980:2017,
                          result_type='team',
                          season_types='Playoffs')


## regular season games aggregation
rs_agg <- rs_games %>%
  group_by(slugSeason, slugTeam, slugOpponent) %>%
  summarize(
    rs_w = sum(outcomeGame=='W'),
    rs_l = sum(outcomeGame=='L')
  ) %>%
  mutate(
    winExpected = case_when(
      rs_w > rs_l ~ TRUE,
      rs_w < rs_l ~ FALSE,
      TRUE ~ NA
    )
  ) %>%
  as.data.frame()

## playoffs season games aggregation
ps_agg <- ps_games %>%
  group_by(slugSeason, slugTeam, slugOpponent) %>%
  summarize(
    ps_w = sum(outcomeGame=='W'),
    ps_l = sum(outcomeGame=='L')
  ) %>%
  mutate(
    won = ps_w > ps_l
  ) %>%
  as.data.frame()


## merge agg dfs
agg <- left_join(ps_agg, rs_agg, by=c('slugSeason', 'slugTeam', 'slugOpponent'))
head(agg, 20)


## confusion matrix
cnf_mtx <- table(agg$won, agg$winExpected)
cnf_mtx


## calculate prediction accuracy
sum(diag(cnf_mtx)) / sum(cnf_mtx)


## accuracy over the years
acc_yoy <- agg %>% 
  group_by(slugSeason) %>%
  summarize(
    n_correct = sum(won == winExpected, na.rm=TRUE),
    n_incorrect = sum(won != winExpected, na.rm=TRUE),
    n_skipped = sum(is.na(winExpected))
  ) %>% 
  mutate(
    acc = round(n_correct / (n_correct + n_incorrect), 2)
  ) %>%
  as.data.frame()

## view accuracy over the years
acc_yoy

## export to html
kable(acc_yoy, 'html')


## seasons with the 100% accuracy
subset(acc_yoy, acc==1)
