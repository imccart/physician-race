# Zocdoc predictions -----------------------------------------------------------

profile.dat <- read_xlsx(path="data/input/zocdoc/from-iu/full-data/zocdoc_profiles_202408_full.xlsx") %>%
  mutate(zocdoc_id=str_extract(zocdoc_link, "[^-]+$")) %>%
  select(name, address, npi, gender, ethnicity, state, city, zip5, zocdoc_id)

zocdoc.race1 <- read_csv(file="data/output/zocdoc_extract_iu.csv")
zocdoc.race2 <- read_csv(file="data/output/zocdoc_extract_iu_supplement.csv")
zocdoc.race <- rbind(zocdoc.race1 %>% filter(dominant_race!="picture does not contain a face"), zocdoc.race2) 

zocdoc.data <- zocdoc.race %>% select(-name) %>%
  left_join(profile.dat %>% mutate(zocdoc_id=as.numeric(zocdoc_id)), by="zocdoc_id") %>%
  mutate(other=indian+middle_eastern) %>%
  select(zocdoc_id, npi, name, degree, gender, ethnicity, 
         dominant_race, other, asian, black, white, hispanic=latino_hispanic,
         address, state, city, zip5) %>%
  mutate(
    max_race = case_when(
      other == pmax(other, asian, black, white, hispanic) ~ "other",
      asian == pmax(other, asian, black, white, hispanic) ~ "asian",
      black == pmax(other, asian, black, white, hispanic) ~ "black",
      white == pmax(other, asian, black, white, hispanic) ~ "white",
      hispanic == pmax(other, asian, black, white, hispanic) ~ "hispanic",
      TRUE ~ NA_real_
    ),
    pred_race = case_when(
      max_race == "other"    ~ other,
      max_race == "asian"    ~ asian,
      max_race == "black"    ~ black,
      max_race == "white"    ~ white,
      max_race == "hispanic" ~ hispanic,
      TRUE ~ NA_real_  # Default to NA if no match
    )
  )  

write_csv(zocdoc.data, 'data/output/final-zocdoc.csv')


profile.links <- read_xlsx(path="data/input/zocdoc/from-iu/full-data/zocdoc_profiles_202408_full.xlsx") %>%
  mutate(zocdoc_id=str_extract(zocdoc_link, "[^-]+$")) %>%
  select(name, npi, zocdoc_id, zocdoc_link)

zocdoc.errors <- read_csv(file="data/output/unreadable_images_iu.csv") %>%
  left_join(profile.links %>% mutate(zocdoc_id=as.numeric(zocdoc_id)) %>% select(-name), by="zocdoc_id")

write_csv(zocdoc.errors, 'data/output/zocdoc-errors.csv')

# NPPES and NAMEPRISM ---------------------------------------------------------

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
  left_join(race.dat, by=c("lastname","firstname")) %>%
  mutate(other=api+two_race) %>%
  select(npi, lastname, firstname, hispanic, black, asian, white, other) %>%
  mutate(
    max_race = case_when(
      other == pmax(other, asian, black, white, hispanic) ~ "other",
      asian == pmax(other, asian, black, white, hispanic) ~ "asian",
      black == pmax(other, asian, black, white, hispanic) ~ "black",
      white == pmax(other, asian, black, white, hispanic) ~ "white",
      hispanic == pmax(other, asian, black, white, hispanic) ~ "hispanic",
      TRUE ~ NA_real_      
    ),
    pred_race = case_when(
      max_race == "other"    ~ other,
      max_race == "asian"    ~ asian,
      max_race == "black"    ~ black,
      max_race == "white"    ~ white,
      max_race == "hispanic" ~ hispanic,
      TRUE ~ NA_real_  # Default to NA if no match
    )
  )  

write_csv(npi.race, 'data/output/final-nppes-nameprism.csv')


# NPPES and WRU --------------------------------------------------------------
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
  left_join(race.dat, by=c("lastname","firstname")) %>%
  mutate(
    max_race = case_when(
      other == pmax(other, asian, black, white, hispanic) ~ "other",
      asian == pmax(other, asian, black, white, hispanic) ~ "asian",
      black == pmax(other, asian, black, white, hispanic) ~ "black",
      white == pmax(other, asian, black, white, hispanic) ~ "white",
      hispanic == pmax(other, asian, black, white, hispanic) ~ "hispanic",
      TRUE ~ NA_real_
    ),
    pred_race = case_when(
      max_race == "other"    ~ other,
      max_race == "asian"    ~ asian,
      max_race == "black"    ~ black,
      max_race == "white"    ~ white,
      max_race == "hispanic" ~ hispanic,
      TRUE ~ NA_real_  # Default to NA if no match
    )
  )  

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
  left_join(race.dat, by=c("lastname","firstname","zip","state")) %>%
  mutate(
    max_race = case_when(
      other == pmax(other, asian, black, white, hispanic) ~ "other",
      asian == pmax(other, asian, black, white, hispanic) ~ "asian",
      black == pmax(other, asian, black, white, hispanic) ~ "black",
      white == pmax(other, asian, black, white, hispanic) ~ "white",
      hispanic == pmax(other, asian, black, white, hispanic) ~ "hispanic",
      TRUE ~ NA_real_      
    ),
    pred_race = case_when(
      max_race == "other"    ~ other,
      max_race == "asian"    ~ asian,
      max_race == "black"    ~ black,
      max_race == "white"    ~ white,
      max_race == "hispanic" ~ hispanic,
      TRUE ~ NA_real_  # Default to NA if no match
    )
  )  

write_csv(npi.race, 'data/output/final-nppes-wrugeo.csv')


# MDPPAS and NAMEPRISM --------------------------------------------------------
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
  left_join(race.dat, by=c("lastname","firstname")) %>%
  mutate(other=api+two_race) %>%
  select(npi, lastname, firstname, hispanic, black, asian, white, other) %>%
  mutate(
    max_race = case_when(
      other == pmax(other, asian, black, white, hispanic) ~ "other",
      asian == pmax(other, asian, black, white, hispanic) ~ "asian",
      black == pmax(other, asian, black, white, hispanic) ~ "black",
      white == pmax(other, asian, black, white, hispanic) ~ "white",
      hispanic == pmax(other, asian, black, white, hispanic) ~ "hispanic",
      TRUE ~ NA_real_      
    ),
    pred_race = case_when(
      max_race == "other"    ~ other,
      max_race == "asian"    ~ asian,
      max_race == "black"    ~ black,
      max_race == "white"    ~ white,
      max_race == "hispanic" ~ hispanic,
      TRUE ~ NA_real_  # Default to NA if no match
    )
  )  

write_csv(npi.race, 'data/output/final-mdppas-nameprism.csv')


# MDPPAS and WRU -----------------------------------------------------------
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
  left_join(race.dat, by=c("lastname","firstname")) %>%
  mutate(
    max_race = case_when(
      other == pmax(other, asian, black, white, hispanic) ~ "other",
      asian == pmax(other, asian, black, white, hispanic) ~ "asian",
      black == pmax(other, asian, black, white, hispanic) ~ "black",
      white == pmax(other, asian, black, white, hispanic) ~ "white",
      hispanic == pmax(other, asian, black, white, hispanic) ~ "hispanic",
      TRUE ~ NA_real_      
    ),
    pred_race = case_when(
      max_race == "other"    ~ other,
      max_race == "asian"    ~ asian,
      max_race == "black"    ~ black,
      max_race == "white"    ~ white,
      max_race == "hispanic" ~ hispanic,
      TRUE ~ NA_real_  # Default to NA if no match
    )
  )  

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
  left_join(race.dat, by=c("lastname","firstname","zip")) %>%
  mutate(
    max_race = case_when(
      other == pmax(other, asian, black, white, hispanic) ~ "other",
      asian == pmax(other, asian, black, white, hispanic) ~ "asian",
      black == pmax(other, asian, black, white, hispanic) ~ "black",
      white == pmax(other, asian, black, white, hispanic) ~ "white",
      hispanic == pmax(other, asian, black, white, hispanic) ~ "hispanic",
      TRUE ~ NA_real_      
    ),
    pred_race = case_when(
      max_race == "other"    ~ other,
      max_race == "asian"    ~ asian,
      max_race == "black"    ~ black,
      max_race == "white"    ~ white,
      max_race == "hispanic" ~ hispanic,
      TRUE ~ NA_real_  # Default to NA if no match
    )
  )  


write_csv(npi.race, 'data/output/final-mdppas-wrugeo.csv')


# Combine All Predictions ------------------------------------------------------

np.mdppas <- read_csv('data/output/final-mdppas-nameprism.csv') %>% distinct(npi, .keep_all=TRUE)
np.nppes <- read_csv('data/output/final-nppes-nameprism.csv') %>% distinct(npi, .keep_all=TRUE)
wru.mdppas <- read_csv('data/output/final-mdppas-wru.csv') %>% distinct(npi, .keep_all=TRUE)
wru.nppes <- read_csv('data/output/final-nppes-wru.csv') %>% distinct(npi, .keep_all=TRUE)
wrugeo.mdppas <- read_csv('data/output/final-mdppas-wrugeo.csv') %>% distinct(npi, .keep_all=TRUE)
wrugeo.nppes <- read_csv('data/output/final-nppes-wrugeo.csv') %>% distinct(npi, .keep_all=TRUE)
zocdoc <- read_csv('data/output/final-zocdoc.csv') %>% distinct(npi, .keep_all=TRUE) %>% mutate(npi=as.numeric(npi))

datasets <- list(np_mdppas=np.mdppas, np_nppes=np.nppes, wru_mdppas=wru.mdppas, wru_nppes=wru.nppes,
                 wrugeo_mdppas=wrugeo.mdppas, wrugeo_nppes=wrugeo.nppes, zocdoc=zocdoc)

# Race variables to rename (excluding key merging variable, npi in this case)
race.rename <- c("other", "asian", "black", "white", "hispanic", "max_race", "pred_race") 

# Function to rename variables by appending a suffix
add.suffix <- function(df, suffix) {
  df %>%
    select(npi, all_of(race.rename)) %>%
    rename_with(~ paste0(., "_", suffix), all_of(race.rename))
}

# Apply the function to all datasets
datasets.renamed <- imap(datasets, ~ add.suffix(.x, .y))

# Merge all datasets by npi
merged.race <- reduce(datasets.renamed, full_join, by = "npi")

# Full data of all unique npi/lastname/firstname
unique.docs <- datasets %>%
  map_df(~ select(.x, npi) %>% distinct()) %>%  # Get unique values from each dataset
  distinct(npi) 

final.race <- unique.docs %>% 
              left_join(merged.race, by=c("npi")) %>%
              left_join(zocdoc %>% select(npi, zocdoc_race=ethnicity) %>% filter(!is.na(npi)), by=c("npi"))

max.race <- paste0("max_race_", c("np_mdppas", "np_nppes","wru_mdppas","wru_nppes","wrugeo_mdppas",
                                       "wrugeo_nppes","zocdoc"))

# Compute predicted_race and race_agreement_share
final.race <- final.race %>%
  mutate(
    count_other   = rowSums(select(., all_of(max.race)) == "other", na.rm = TRUE),
    count_asian   = rowSums(select(., all_of(max.race)) == "asian", na.rm = TRUE),
    count_black   = rowSums(select(., all_of(max.race)) == "black", na.rm = TRUE),
    count_white   = rowSums(select(., all_of(max.race)) == "white", na.rm = TRUE),
    count_hispanic = rowSums(select(., all_of(max.race)) == "hispanic", na.rm = TRUE),
    count_any = count_other+count_asian+count_black+count_white+count_hispanic,
    common_pred = case_when(
      count_other/count_any > 0.5 & count_any > 0 ~ "Other",
      count_asian/count_any > 0.5 & count_any > 0  ~ "Asian",
      count_black/count_any > 0.5 & count_any > 0  ~ "Black",
      count_white/count_any > 0.5 & count_any > 0  ~ "White",
      count_hispanic/count_any > 0.5 & count_any > 0  ~ "Hispanic",
      TRUE ~ "None"
    ),
    share_pred = case_when(
      common_pred == "Other"    ~ count_other / count_any,
      common_pred == "Asian"    ~ count_asian / count_any,
      common_pred == "Black"    ~ count_black / count_any,
      common_pred == "White"    ~ count_white / count_any,
      common_pred == "Hispanic" ~ count_hispanic / count_any,
      common_pred == "None"     ~ 0, 
      TRUE ~ NA_real_  # Default to 0 if common_pred is missing
    )
  ) %>%
  select(npi, starts_with("max_race_"), starts_with("pred_race_"), common_pred, share_pred, zocdoc_race) %>%
  rename_with(~ str_remove(., "^max_race_"), starts_with("max_race_")) %>%
  rename_with(~ str_replace(., "^pred_race_","pred_"), starts_with("pred_race_"))

write_csv(final.race, 'data/output/final-npi-race.csv')