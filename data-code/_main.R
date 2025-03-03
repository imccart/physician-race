
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


# Image (DeepFace) predictions --------------------------------------------
## Run python scripts seperately

# Clean predictions -------------------------------------------------------
# source('data-code/1_clean.R')