
# Import initial race predictions
pred.race <- read_csv('data/output/final-npi-race.csv') %>%
  mutate(zocdoc_race = case_when(
              zocdoc_race == "middle eastern" ~ "other",
              zocdoc_race == "latino hispanic" ~ "hispanic",
              zocdoc_race == "indian" ~ "asian",
              TRUE ~ zocdoc_race))


# Import and clean 'true' race data ----------------------------------------
source('data-code/clean-FL-voter.R')
source('data-code/clean-TX-license.R')

## Florida voter data
fl.data <- read_csv('data/output/fl_voter_file.csv') %>%
  mutate(
    other = other + native_am + multi,
    fl_voter_race = case_when(
      white == 1 ~ "white",
      black == 1 ~ "black", 
      hispanic == 1 ~ "hispanic", 
      asian == 1 ~ "asian", 
      other == 1 ~ "other", 
      TRUE ~ "unknown"
    )) %>% 
  select(!c(native_am, multi, unknown)) %>%
  mutate(firstname=tolower(first), lastname=tolower(surname), first_initial=substr(firstname,1,1)) %>%
  filter(fl_voter_race!="unknown", firstname!="none", firstname!="(none)", lastname!="none", lastname!="(none)")


fl.final.full <- fl.data %>% 
  filter(str_length(firstname) > 1) %>%
  inner_join(
    pred.race %>% 
      filter(state=="fl") %>% 
      select(npi, firstname, lastname), 
    by=c("firstname", "lastname")
  ) %>%
  group_by(firstname, lastname) %>%
  filter(n_distinct(fl_voter_race) == 1 | n() == 1) %>%
  ungroup() %>%
  select(npi, fl_voter_race) 

fl.final.initial <- fl.data %>% 
  filter(str_length(firstname)==1) %>%
  inner_join(
    pred.race %>% 
      filter(state=="fl") %>% 
      select(npi, firstname, lastname) %>%
      mutate(first_initial=substr(firstname,1,1)),
    by=c("first_initial", "lastname")
  ) %>%
  group_by(first_initial, lastname) %>%
  filter(n_distinct(fl_voter_race) == 1 | n() == 1) %>%
  ungroup() %>%
  select(npi, fl_voter_race) 

fl.final <- bind_rows(fl.final.full, fl.final.initial) %>% 
  group_by(npi) %>%
  filter(n_distinct(fl_voter_race) == 1) %>%
  ungroup() %>%
  distinct(npi, fl_voter_race)


## Texas licensing data
tx.data <- read_csv('data/output/tx-license-clean.csv') %>%
  mutate(
    white_tx = if_else(race == "white", 1, 0),
    black_tx = if_else(race == "black", 1, 0),
    hisp_tx  = if_else(race == "hisp", 1, 0),
    other_tx = if_else(race == "other", 1, 0),
    asian_tx = if_else(race == "asian", 1, 0)
  ) %>%
  rename(tx_true_race=race) %>%
  mutate(tx_true_race = ifelse(tx_true_race == "hisp", "hispanic", tx_true_race)) %>%
  filter(!is.na(npi))

tx.final <- tx.data %>%
  select(npi, tx_true_race) %>%
  group_by(npi) %>%
  filter(n_distinct(tx_true_race)==1) %>%
  ungroup() %>%
  distinct(npi, tx_true_race)


## Combine for 'true race'
merged.data <- pred.race %>%
  left_join(fl.final, by="npi") %>%
  left_join(tx.final, by="npi") %>%
  mutate(true_race=
          case_when(
            !is.na(tx_true_race) ~ tx_true_race,
            !is.na(fl_voter_race) ~ fl_voter_race,
            !is.na(zocdoc_race) ~ zocdoc_race,
            TRUE ~ NA_character_
          ),
          true_race=as.factor(true_race),
          np_mdppas=as.factor(np_mdppas),
          np_nppes=as.factor(np_nppes),
          wru_mdppas=as.factor(wru_mdppas),
          wru_nppes=as.factor(wru_nppes),
          zocdoc=as.factor(zocdoc))


# Form aggregate prediction ------------------------------------

predictor.list <- list(
  c("np_mdppas", "wru_mdppas", "np_nppes", "wru_nppes", "zocdoc"),
  c("np_mdppas", "wru_mdppas", "np_nppes", "wru_nppes"),
  c("np_mdppas", "wru_mdppas"),
  c("np_nppes", "wru_nppes")
)

for (i in seq_along(predictor.list)) {
  predictor.columns <- predictor.list[[i]]

  sample.data <- merged.data %>% filter(if_all(all_of(predictor.columns), ~ !is.na(.)))
  ranger.data <- sample.data %>% select(true_race, all_of(predictor.columns)) %>% filter(!is.na(true_race))

  rf.spec <- rand_forest(trees = 200, min_n = 5) %>%
    set_mode("classification") %>%
    set_engine("ranger")

  rf.data <- rf.spec %>% fit(true_race ~ ., data = ranger.data)
  rf.pred <- predict(rf.data, new_data = sample.data)
  rf.prob <- predict(rf.data, new_data = sample.data, type = "prob")

  pred <- sample.data %>%
    mutate(rf_pred_race = rf.pred$.pred_class,
           rf_pred_model=i) %>%
    bind_cols(
      rf.prob %>% rename_with(~ str_replace(.x, "^\\.pred_", "rf_pred_"))
    )

  assign(paste0("pred", i), pred)
}

## combine predictions
final.data <- bind_rows(pred1, pred2, pred3, pred4) %>%
  arrange(rf_pred_model) %>%
  distinct(npi, .keep_all=TRUE)
write_csv(final.data, 'data/output/final-npi-combined.csv')

