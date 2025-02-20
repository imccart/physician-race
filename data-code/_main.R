
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
               janitor, wru, readxl, stringr)
source('data-code/api_keys.R')

# Import initial data -----------------------------------------------------

## NPPES data
nppes.data <- read_csv('data/input/npidata_pfile_20050523-20200809.csv') %>% clean_names()
nppes.names <- nppes.data %>% select(lastname=provider_last_name_legal_name, firstname=provider_first_name) %>%
  filter(lastname!="" & !is.na(lastname)) %>%
  distinct(lastname, firstname)

write_csv(nppes.names, 'data/output/unique-nppes-names.csv')

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

write_csv(mdppas.names, 'data/output/unique-mdppas-names.csv')

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

profile.dat <- read_xlsx(path="data/input/zocdoc/from-iu/full-data/zocdoc_profiles_202408_full.xlsx") %>%
  mutate(zocdoc_id=str_extract(zocdoc_link, "[^-]+$")) %>%
  select(name, address, npi, gender, ethnicity, state, city, zip5, zocdoc_id)
zocdoc.race <- read_csv(file="data/output/zocdoc_extract_iu.csv")

zocdoc.data <- zocdoc.race %>% select(-name) %>%
  left_join(profile.dat %>% mutate(zocdoc_id=as.numeric(zocdoc_id)), by="zocdoc_id") %>%
  select(zocdoc_id, npi, name, degree, gender, ethnicity, 
         dominant_race, asian, indian, black, white, middle_eastern, latino_hispanic,
         address, state, city, zip5)

write_csv(zocdoc.data, 'data/output/final-zocdoc.csv')


profile.links <- read_xlsx(path="data/input/zocdoc/from-iu/full-data/zocdoc_profiles_202408_full.xlsx") %>%
  mutate(zocdoc_id=str_extract(zocdoc_link, "[^-]+$")) %>%
  select(name, npi, zocdoc_id, zocdoc_link)

zocdoc.errors <- read_csv(file="data/output/unreadable_images_iu.csv") %>%
  left_join(profile.links %>% mutate(zocdoc_id=as.numeric(zocdoc_id)) %>% select(-name), by="zocdoc_id")

write_csv(zocdoc.errors, 'data/output/zocdoc-errors.csv')

# Clean race data ---------------------------------------------------------

## from NPPES and NAMEPRISM
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

write_csv(npi.race, 'data/output/final-nppes-nameprism.csv')


## from NPPES and WRU
npi.data <- nppes.data %>% 
  select(npi, lastname=provider_last_name_legal_name, firstname=provider_first_name)

race.dat <- read_csv(file="data/output/wru-nppes.csv") %>%
  select(lastname=surname, firstname=first, zip, county, state, white=white_name, 
         black=black_name, hispanic=hisp_name, asian=asian_name, other=other_name) %>%
  group_by(lastname, firstname) %>% 
  mutate(case_obs=n()) %>%
  filter(case_obs==1) %>%
  select(-case_obs) %>%
  ungroup()

npi.race <- npi.data %>% filter(lastname!="") %>%
  left_join(race.dat, by=c("lastname","firstname"))

write_csv(npi.race, 'data/output/final-nppes-wru.csv')

## from NPPES and WRU with GEO
npi.data <- nppes.data %>% 
  select(npi, lastname=provider_last_name_legal_name, firstname=provider_first_name,
         state=provider_business_practice_location_address_state_name,
         zip=provider_business_practice_location_address_postal_code)

race.dat <- read_csv(file="data/output/wru-nppes.csv") %>%
  select(lastname=surname, firstname=first, zip, county, state, white=white_namegeo, 
         black=black_namegeo, hispanic=hisp_namegeo, asian=asian_namegeo, other=other_namegeo) %>%
  group_by(lastname, firstname, zip, state) %>% 
  mutate(case_obs=n()) %>%
  filter(case_obs==1) %>%
  select(-case_obs) %>%
  ungroup()

npi.race <- npi.data %>% filter(lastname!="") %>%
  left_join(race.dat, by=c("lastname","firstname","zip","state"))

write_csv(npi.race, 'data/output/final-nppes-wrugeo.csv')


## From MDPPAS and NAMEPRISM
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

write_csv(npi.race, 'data/output/final-mdppas-nameprism.csv')


## From MDPPAS and WRU
npi.data <- mdppas %>% 
  select(npi, lastname=name_last, firstname=name_first)

race.dat <- read_csv(file="data/output/wru-mdppas.csv") %>%
  select(lastname=surname, firstname=first, zip, county, state, white=white_name, 
         black=black_name, hispanic=hisp_name, asian=asian_name, other=other_name) %>%
  group_by(lastname, firstname) %>% 
  mutate(case_obs=n()) %>%
  filter(case_obs==1) %>%
  select(-case_obs) %>%
  ungroup()

npi.race <- npi.data %>% filter(lastname!="") %>%
  left_join(race.dat, by=c("lastname","firstname"))

write_csv(npi.race, 'data/output/final-mdppas-wru.csv')


## From MDPPAS and WRU with GEO
npi.data <- mdppas %>% 
  select(npi, lastname=name_last, firstname=name_first, zip=phy_zip_pos1)

race.dat <- read_csv(file="data/output/wru-mdppas.csv") %>%
  select(lastname=surname, firstname=first, zip, county, state, white=white_namegeo, 
         black=black_namegeo, hispanic=hisp_namegeo, asian=asian_namegeo, other=other_namegeo) %>%
  mutate(zip=as.numeric(zip)) %>%
  group_by(lastname, firstname, zip) %>% 
  mutate(case_obs=n()) %>%
  filter(case_obs==1) %>%
  select(-case_obs) %>%
  ungroup()

npi.race <- npi.data %>% filter(lastname!="") %>%
  left_join(race.dat, by=c("lastname","firstname","zip"))

write_csv(npi.race, 'data/output/final-mdppas-wrugeo.csv')


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
