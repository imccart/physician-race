# Extract key variables --------------------------------------------------------

clean.nameloc <- mdppas %>% 
  select(surname=name_last, 
         first=name_first, 
         zip=phy_zip_pos1) %>%
  filter(surname!="" & !is.na(surname)) %>%
  distinct(surname, first, zip) 

# Map zip to one county in crosswalk -------------------------------------------

set.seed(123)
zc <- zc_crosswalk %>% 
  group_by(ZIP) %>%
  slice(which.max(rank(TOT_RATIO, ties.method = "random"))) %>% 
  select(c(ZIP, COUNTY)) %>% 
  rename(county=COUNTY) 

clean.nameloc$zip <- sprintf("%05d", clean.nameloc$zip)

# Merge into clean.nameloc 
clean.nameloc <- clean.nameloc %>% 
  left_join(zc, by=c('zip'='ZIP')) %>% 
  mutate(
    countyfips = substr(county, 3, 5), 
    stfips = as.numeric(substr(county, 1, 2))
  ) %>% 
  left_join(stfips_crosswalk, by=c('stfips' = 'stfips')) %>% 
  select(!c(county, stfips, state)) %>% 
  rename(county = countyfips, state = stabbr) %>% 
  filter(!is.na(state))

clean.nameloc <- clean.nameloc %>% 
  filter(state!="PR", state!="VI", state!="AK", state !="HI") 

# wru race prediction ----------------------------------------------------------

census.df <- get_census_data(key=census.key,
                             states=unique(clean.nameloc$state), 
                             census.geo='county')

# Drop observations from counties not in census data 
for(i in 1:length(census.df)){
  this_state <- census.df[[i]]$state
  this_county <- census.df[[i]]$county$county
  
  clean.nameloc <- clean.nameloc %>% 
    filter(!(state == this_state & !(county %in% this_county)))
}

# Name only prediction 
wru.mdppas.surname <- predict_race(clean.nameloc, 
                             surname.only=TRUE, 
                             names.to.use='surname, first') %>%
                as_tibble()

# Name + county prediction 
wru.mdppas.geo <- predict_race(clean.nameloc, 
                         census.geo='county', 
                         census.data=census.df, 
                         names.to.use='surname, first') %>%
            as_tibble()


# Combining predictions
wru.mdppas <- wru.mdppas.surname %>% 
    rename(white_name=pred.whi, black_name=pred.bla,
           hisp_name=pred.his, asian_name=pred.asi,
           other_name=pred.oth) %>%
    left_join(wru.mdppas.geo, by=c('surname', 'first', 'zip', 'county', 'state')) %>%
    rename(white_namegeo=pred.whi, black_namegeo=pred.bla,
           hisp_namegeo=pred.his, asian_namegeo=pred.asi,
           other_namegeo=pred.oth)

write_csv(wru.mdppas,file="data/output/wru-mdppas.csv")
