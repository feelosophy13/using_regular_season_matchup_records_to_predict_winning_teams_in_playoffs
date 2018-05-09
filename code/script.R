## load libraries
library(tidyverse)
library(nbastatR)
library(knitr)
library(glue)


## get all regular season game logs
rs_games <- get_game_logs(seasons=1980:2017, 
                         result_type='team',
                         season_types='Regular Season')


## get all playoffs season game logs
ps_games <- get_game_logs(seasons=1980:2017,
                          result_type='team',
                          season_types='Playoffs')


## win prediction based on regular-season matchup record
rs_agg <- rs_games %>%
  group_by(slugSeason, slugTeam, slugOpponent) %>%
  summarize(
    rs_w = sum(outcomeGame=='W'),
    rs_l = sum(outcomeGame=='L')
  ) %>%
  mutate(
    winExpected_RSMR = case_when(
      rs_w > rs_l ~ TRUE,
      rs_w < rs_l ~ FALSE,
      TRUE ~ NA
    )
  ) %>%
  select(slugSeason, slugTeam, slugOpponent, winExpected_RSMR) %>%
  as.data.frame()


## win prediction based on seeding 
## (i.e. whichever team that hosts the first playoffs matchup game at home)
seed_agg <- ps_games %>%
  group_by(slugSeason, slugTeam, slugOpponent) %>%
  arrange(slugSeason, slugTeam, slugOpponent, dateGame) %>%
  slice(1) %>%
  mutate(
    winExpected_seed = case_when(
      locationGame=='H' ~ TRUE,
      locationGame=='A' ~ FALSE
    )
  ) %>%
  select(slugSeason, slugTeam, slugOpponent, winExpected_seed) %>%
  as.data.frame()


## playoffs matchup result
ps_agg <- ps_games %>%
  group_by(slugSeason, slugTeam, slugOpponent) %>%
  summarize(
    ps_w = sum(outcomeGame=='W'),
    ps_l = sum(outcomeGame=='L')
  ) %>%
  mutate(
    won = ps_w > ps_l
  ) %>%
  select(slugSeason, slugTeam, slugOpponent, won) %>%
  as.data.frame()


## merge agg dfs
agg <- ps_agg %>%
  left_join(rs_agg, by=c('slugSeason', 'slugTeam', 'slugOpponent')) %>%
  left_join(seed_agg, by=c('slugSeason', 'slugTeam', 'slugOpponent')) %>%
  mutate(
    winExpected_both = case_when(
      winExpected_RSMR==TRUE & winExpected_seed==TRUE ~ TRUE,  # TRUE if both expects TRUE
      winExpected_RSMR==FALSE & winExpected_seed==FALSE ~ FALSE,  # FALSE if both expects FALSE
      TRUE ~ NA  # everything else as NA (in case of conflicting expectations or when one of the values is NA)
    )
  )
head(agg, 20)


## confusion matrix
cnf_mtx_RSMR <- table(agg$won, agg$winExpected_RSMR)
cnf_mtx_seed <- table(agg$won, agg$winExpected_seed)
cnf_mtx_both <- table(agg$won, agg$winExpected_both)

cnf_mtx_RSMR
cnf_mtx_seed
cnf_mtx_both


## calculate prediction accuracy
sum(diag(cnf_mtx_RSMR)) / sum(cnf_mtx_RSMR)
sum(diag(cnf_mtx_seed)) / sum(cnf_mtx_seed)
sum(diag(cnf_mtx_both)) / sum(cnf_mtx_both)


## accuracy over the years
acc_yoy <- agg %>% 
  group_by(slugSeason) %>%
  summarize(
    n_correct_RSMR = sum(won == winExpected_RSMR, na.rm=TRUE),
    n_incorrect_RSMR = sum(won != winExpected_RSMR, na.rm=TRUE),
    n_skipped_RSMR = sum(is.na(winExpected_RSMR)),
    n_correct_seed = sum(won == winExpected_seed, na.rm=TRUE),
    n_incorrect_seed = sum(won != winExpected_seed, na.rm=TRUE),
    n_skipped_seed = sum(is.na(winExpected_seed)),
    n_correct_both = sum(won == winExpected_both, na.rm=TRUE),
    n_incorrect_both = sum(won != winExpected_both, na.rm=TRUE),
    n_skipped_both = sum(is.na(winExpected_both))    
  ) %>% 
  mutate(
    acc_RSMR = round(n_correct_RSMR / (n_correct_RSMR + n_incorrect_RSMR), 2),
    acc_seed = round(n_correct_seed / (n_correct_seed + n_incorrect_seed), 2),
    acc_both = round(n_correct_both / (n_correct_both + n_incorrect_both), 2)
  ) %>%
  as.data.frame()


## view accuracy over the years

acc_yoy_html <- acc_yoy %>%
  mutate(
    acc_RSMR = glue("{acc_RSMR*100}% ({n_correct_RSMR}-{n_incorrect_RSMR}-{n_skipped_RSMR})"),
    acc_seed = glue("{acc_seed*100}% ({n_correct_seed}-{n_incorrect_seed}-{n_skipped_seed})"),
    acc_both = glue("{acc_both*100}% ({n_correct_both}-{n_incorrect_both}-{n_skipped_both})")
  ) %>%
  select(slugSeason, acc_RSMR, acc_seed, acc_both)
acc_yoy_html


## export to html
kable(acc_yoy_html, 'html')


