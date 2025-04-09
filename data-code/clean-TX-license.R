# About ------------------------------------------------------------------------

# Clean TX licensing data 
# Author:         Shirley Cai 
# Date created:   02/19/2024 
# Last edited:    05/09/2024 
#                 Changed path names

# Setup ------------------------------------------------------------------------

if (!require("pacman")) renv::install('pacman')
pacman::p_load(tidyverse, haven)

# Import data ------------------------------------------------------------------

df_2021 <- read_delim("data/input/texas/IU-project-folder/4Data/Rawdata/202108PHYACTD.txt",
                      trim_ws = TRUE, col_types = cols(.default = col_character()))
df_2023 <- read_delim("data/input/texas/IU-project-folder/4Data/Rawdata/202308PHYALLD.txt", 
                      col_names = FALSE, trim_ws = TRUE, col_types = cols(.default = col_character()))
statelic <- read_dta("data/input/texas/IU-project-folder/4Data/Rawdata/statelic.dta")

colnames(df_2021) <- tolower(colnames(df_2021))
colnames(df_2023) <- colnames(df_2021)

# Manually fix parsing issues --------------------------------------------------

problems_2023 <- problems(df_2023)
n_col <- ncol(df_2023)

# Row 250 
df_2023[250, 9:13] <- df_2023[250, 10:14]
df_2023[250, 14:(n_col - 2)] <- df_2023[250, 16:n_col]
df_2023[250, 34:36] <- t(unlist(str_split(unlist(df_2023[250, 34]), "\\|")))

# Row 6234
df_2023[6234, 13] <- "9250 PINE CROFT DRV"
df_2023[6234, 14:(n_col - 1)] <- df_2023[6234, 15:n_col]
df_2023[6234, 35:36] <- t(unlist(str_split(unlist(df_2023[6234, 35]), "\\|")))

# Row 22040 
df_2023[22040, 9:(n_col - 1)] <- df_2023[22040, 10:n_col]
df_2023[22040, 35:36] <- t(unlist(str_split(unlist(df_2023[22040, 35]), "\\|")))

# Row 22999
df_2023[22999, 12:(n_col - 1)] <- df_2023[22999, 13:n_col]
df_2023[22999, 35:36] <- t(unlist(str_split(unlist(df_2023[22999, 35]), "\\|")))

# Row 27731 
df_2023[27731, 14:(n_col - 1)] <- df_2023[27731, 15:n_col]
df_2023[27731, 35:36] <- t(unlist(str_split(unlist(df_2023[27731, 35]), "\\|")))

# Row 28007 
df_2023[28007, 12:(n_col - 1)] <- df_2023[28007, 13:n_col]
df_2023[28007, 35:36] <- t(unlist(str_split(unlist(df_2023[28007, 35]), "\\|")))

# Row 50906
df_2023[50906, 9:(n_col - 1)] <- df_2023[50906, 10:n_col]
df_2023[50906, 35:36] <- t(unlist(str_split(unlist(df_2023[50906, 35]), "\\|")))

# Row 57501
df_2023[57501, 9:(n_col - 1)] <- df_2023[57501, 10:n_col]
df_2023[57501, 14:(n_col - 1)] <- df_2023[57501, 15:n_col]
df_2023[57501, 34:36] <- t(unlist(str_split(unlist(df_2023[57501, 34]), "\\|")))

# Row 94574
df_2023[94574, 12:(n_col - 1)] <- df_2023[94574, 13:n_col]
df_2023[94574, 35:36] <- t(unlist(str_split(unlist(df_2023[94574, 35]), "\\|")))

# Row 116379 
df_2023[116379, 14:(n_col - 1)] <- df_2023[116379, 15:n_col]
df_2023[116379, 35:36] <- t(unlist(str_split(unlist(df_2023[116379, 35]), "\\|")))

# Row 125220
df_2023[125220, 13] <- paste0(unlist(df_2023[125220, 13:14]), collapse = ", ")
df_2023[125220, 14:(n_col - 1)] <- df_2023[125220, 15:n_col]
df_2023[125220, 35:36] <- t(unlist(str_split(unlist(df_2023[125220, 35]), "\\|")))

# Row 158590
df_2023[158590, 14:(n_col - 1)] <- df_2023[158590, 15:n_col]
df_2023[158590, 35:36] <- t(unlist(str_split(unlist(df_2023[158590, 35]), "\\|")))

# Merge 2021 and 2023 ----------------------------------------------------------

df <- bind_rows(list(df_2021, df_2023)) %>% 
  distinct(id, lic, ln, fn, rac, .keep_all = TRUE) %>% 
  filter(!duplicated(id, fromLast = TRUE)) 

# Keep physicians with race information practicing in TX 
df <- df %>% 
  mutate(
    ps = toupper(ps), 
    ms = toupper(ms),
    id = as.numeric(id)) %>% 
  filter(!is.na(rac), ps == "TX" | ms == "TX") 

# Merge in NPI -----------------------------------------------------------------

df <- df %>% 
  left_join(statelic, by = c("lic" = "plicnum", "ps" = "plicstate")) %>% 
  distinct(id, npi, .keep_all = TRUE)

print(paste0("Total unique physicians: ", length(unique(df$id))))
print(paste0("Physicians unmatched to NPI: ", sum(is.na(df$npi))))
print(paste0("Proportion physicians unmatched: ", sum(is.na(df$npi)/length(unique(df$id)))))

# Race vs. Hispanic 
table(df$rac, df$his)

# Recode variables -------------------------------------------------------------

df <- df %>% 
  mutate(
    race = case_when(
      his == "Y" ~ "hisp", 
      rac == "ASN" ~ "asian", 
      rac == "HAW" ~ "asian", 
      rac == "BLK" ~ "black", 
      rac == "WHT" ~ "white", 
      rac == "OTH" ~ "other", 
      rac == "IND" ~ "other", 
      TRUE ~ NA
    )
  )

# Export dataframe -------------------------------------------------------------

write_csv(df, "data/output/tx-license-clean.csv")
rm(list = ls())
