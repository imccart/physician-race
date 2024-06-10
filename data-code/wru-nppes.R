# Extract key variables --------------------------------------------------------

clean.nameloc <- nppes.data %>% 
  select(surname=provider_last_name_legal_name, 
         first=provider_first_name, 
         state=provider_business_practice_location_address_state_name,
         zip=provider_business_practice_location_address_postal_code) %>%
  filter(surname!="" & !is.na(surname)) %>%
  distinct(surname, first, state, zip)

# Map zip to one county in crosswalk -------------------------------------------

# For zips mapped to more than 1 county, pick mapping with highest prop 
set.seed(123)
zc <- zc_crosswalk %>% 
  group_by(ZIP) %>%
  slice(which.max(rank(TOT_RATIO, ties.method = "random"))) %>% 
  select(c(ZIP, COUNTY)) %>% 
  rename(county=COUNTY) 

clean.nameloc <- clean.nameloc %>% 
  mutate(
    zip_short = substr(zip, 1, 5)
  )

# Merge into clean.nameloc 
clean.nameloc <- clean.nameloc %>% select(!state) %>%
  left_join(zc, by=c('zip_short'='ZIP')) %>% 
  mutate(countyfips = substr(county, 3, 5), 
         stfips = as.numeric(substr(county, 1, 2))) %>%
  left_join(stfips_crosswalk, by=c('stfips' = 'stfips')) %>% 
  select(!c(county, stfips, state)) %>% 
  rename(county = countyfips, state = stabbr) %>% 
  filter(state!="PR", state!="VI", state!="AK", state !="HI", county !="NA", !is.na(state),
         !(state=="VA" & county=="560")) 

# wru race prediction ----------------------------------------------------------

census.df <- get_census_data(key=census.key,
                             states=unique(clean.nameloc$state), 
                             census.geo='county')

# Name only prediction 
wru.nppes.surname <- predict_race(clean.nameloc, 
                             surname.only=TRUE, 
                             names.to.use='surname, first') %>%
                     as_tibble()

# Name + county prediction 
wru.nppes.geo <- predict_race(clean.nameloc, 
                         census.geo='county', 
                         census.data=census.df, 
                         names.to.use='surname, first') %>%
                 as_tibble()

# Combining predictions
wru.nppes <- wru.nppes.surname %>% 
    rename(white_name=pred.whi, black_name=pred.bla,
           hisp_name=pred.his, asian_name=pred.asi,
           other_name=pred.oth) %>%
    left_join(wru.nppes.geo, by=c('surname', 'first', 'zip', 'zip_short', 'county', 'state')) %>%
    rename(white_namegeo=pred.whi, black_namegeo=pred.bla,
           hisp_namegeo=pred.his, asian_namegeo=pred.asi,
           other_namegeo=pred.oth) %>%
    select(!zip_short)

write_csv(wru.nppes,file="data/output/wru-nppes.csv")