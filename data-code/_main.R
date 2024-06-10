
# Meta --------------------------------------------------------------------

## Title:         Physician Race Concordance and Referrals
## Author:        Shirley Cai & Ian McCarthy
## Date Created:  9/9/2021
## Date Edited:   6/3/2024
## Description:   This file calls all analysis scripts in the relevant order


# Preliminaries -----------------------------------------------------------
if (!require("pacman")) renv::install('pacman')
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stargazer, knitr, kableExtra,
               fixest, modelsummary, broom, tidymodels, data.table, httr, jsonlite, 
               janitor, wru, readxl)
source('data-code/api_keys.R')

# Import initial data -----------------------------------------------------

## NPPES data
nppes.data <- read_csv('data/input/npidata_pfile_20050523-20200809.csv') %>% clean_names()
nppes.names <- nppes.data %>% select(lastname=provider_last_name_legal_name, firstname=provider_first_name) %>%
  filter(lastname!="" & !is.na(lastname)) %>%
  distinct(lastname, firstname)

write_tsv(nppes.names, 'data/output/unique-nppes-names.csv')

## MDPPAS data 
filelist <- c('data/input/MDPPAS/PhysicianData_2013.csv', 
              'data/input/MDPPAS/PhysicianData_2014.csv',
              'data/input/MDPPAS/PhysicianData_2015.csv',
              'data/input/MDPPAS/PhysicianData_2016.csv', 
              'data/input/MDPPAS/PhysicianData_2017.csv', 
              'data/input/MDPPAS/PhysicianData_2018.csv', 
              'data/input/MDPPAS/PhysicianData_2019.csv', 
              'data/input/MDPPAS/PhysicianData_2020.csv')
raw <- lapply(filelist, function(x) read_csv(x))
mdppas <- do.call(rbind.data.frame, raw) %>% 
  clean_names()
mdppas.names <- mdppas %>% select(lastname=name_last, firstname=name_first) %>%
  filter(lastname!="" & !is.na(lastname)) %>%
  distinct(lastname, firstname)

write_tsv(mdppas.names, 'data/output/unique-mdppas-names.csv')

## zip-county crosswalk (HUD)
zc_crosswalk <- read_excel("data/input/ZIP_COUNTY_122010.xlsx")

## state fips crosswalk (BLS)
stfips_crosswalk <- read_csv("data/input/stfips_crosswalk.csv")


# NamePrism race prediction -----------------------------------------------
# source('data-code/nameprism-nppes.R')
# source('data-code/nameprism-mdppas.R')


# WRU race prediction -----------------------------------------------------
# source('data-code/wru-nppes.R')
# source('data-code/wru-mdppas.R')


# Clean data with race variables -----------------------------------------------
source('data-code/clean-FL-voter.R')
source('data-code/clean-TX-license.R')

# Assess accuracy of predictions 
source('data-code/accuracy-FL.R')
source('data-code/accuracy-TX.R')

# Zocdoc predictions -----------------------------------------------------------

# Image predictions are performed using a Python script
# TODO: Source Python file using reticulate 

# Clean race data ---------------------------------------------------------

## from NPPES
npi.data <- nppes.data %>% 
  select(npi, lastname=provider_last_name_legal_name, firstname=provider_first_name)

race.dat <- read_csv(file="data/output/nameprism-nppes.csv",
                      col_names=c("nameprism","firstname","lastname")) %>%
  separate(nameprism, sep="\n", into=c("two_race","hispanic","api","black","asian","white")) %>%
  separate(two_race, sep=",", into=c("cat1","two_race")) %>%
  separate(hispanic, sep=",", into=c("cat2","hispanic")) %>%
  separate(api, sep=",", into=c("cat3","api")) %>%
  separate(black, sep=",", into=c("cat4","black")) %>%
  separate(asian, sep=",", into=c("cat5","asian")) %>%
  separate(white, sep=",", into=c("cat6","white")) %>%
  select(lastname, firstname, two_race, hispanic, api, black, asian, white) %>%
  mutate(across(c("two_race","hispanic","api","black","asian","white"), as.numeric)) %>%
  group_by(lastname, firstname) %>% 
  mutate(case_obs=n()) %>%
  filter(case_obs==1) %>%
  select(-case_obs) %>%
  ungroup()

npi.race <- npi.data %>% filter(lastname!="") %>%
  left_join(race.dat, by=c("lastname","firstname"))

write_tsv(npi.race, 'data/output/final-nppes-nameprism.csv')


## From MDPPAS
npi.data <- mdppas %>% 
  select(npi, lastname=name_last, firstname=name_first)

race.dat <- read_csv(file="data/output/nameprism-mdppas.csv",
                      col_names=c("nameprism","firstname","lastname")) %>%
  separate(nameprism, sep="\n", into=c("two_race","hispanic","api","black","asian","white")) %>%
  separate(two_race, sep=",", into=c("cat1","two_race")) %>%
  separate(hispanic, sep=",", into=c("cat2","hispanic")) %>%
  separate(api, sep=",", into=c("cat3","api")) %>%
  separate(black, sep=",", into=c("cat4","black")) %>%
  separate(asian, sep=",", into=c("cat5","asian")) %>%
  separate(white, sep=",", into=c("cat6","white")) %>%
  select(lastname, firstname, two_race, hispanic, api, black, asian, white) %>%
  filter(!is.na(white)) %>%
  mutate(across(c("two_race","hispanic","api","black","asian","white"), as.numeric)) %>%
  group_by(lastname, firstname) %>% 
  mutate(case_obs=n()) %>%
  filter(case_obs==1) %>%
  select(-case_obs) %>%
  ungroup()

npi.race <- npi.data %>% filter(lastname!="") %>%
  left_join(race.dat, by=c("lastname","firstname"))

write_tsv(npi.race, 'data/output/final-mdppas-nameprism.csv')


# Summary -----------------------------------------------------------------

np.nppes <- read_tsv('data/output/final-nppes-nameprism.csv')
np.mdppas <- read_tsv('data/output/final-mdppas-nameprism.csv')

final.dat <- npi.race %>%
  mutate(
    black_5=case_when(
      black>.05 ~ 1,
      TRUE ~ 0),
    black_10=case_when(
      black>=0.1 ~ 1,
      TRUE ~ 0),
    black_25=case_when(
      black>=0.25 ~ 1,
      TRUE ~ 0))


sumtable(final.dat, vars=c("hispanic","api","black","indian","white"),
         summ=c('notNA(x)','countNA(x)','mean(x)','sd(x)','pctile(x)[10]',
                           'pctile(x)[20]','pctile(x)[30]','pctile(x)[40]','pctile(x)[50]',
                           'pctile(x)[60]','pctile(x)[70]','pctile(x)[80]','pctile(x)[90]'))

sumtable(final.dat, vars=c("black","black_5","black_10","black_25"),
         summ=c('notNA(x)','countNA(x)','mean(x)'))

ggplot(data=final.dat, aes(x=black)) +
  geom_histogram(aes(y=..density..), colour="black", fill="grey45") +
  geom_density(col="blue", size=.5) + theme_bw()

ggplot(data=final.dat) +
  geom_density(aes(x=black), col="blue", size=.5) + 
  geom_density(aes(x=black_10), col="red", size=.5) + theme_bw()
