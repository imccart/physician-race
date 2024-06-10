# About ------------------------------------------------------------------------

# Clean FL voter files 
# Author:         Shirley Cai 
# Date created:   01/23/2024 
# Last edited:    05/09/2024 
#                 Changed path names

# Setup ------------------------------------------------------------------------

if (!require("pacman")) renv::install('pacman')
pacman::p_load(tidyverse)

filelist <- list.files(path = "data/input/florida-voter-detail")
filelist <- unlist(lapply(filelist, function(x) paste0("data/input/florida-voter-detail/", x)))

# Import data ------------------------------------------------------------------

# Function to read in and extract data from raw voter files 
read_clean_raw <- function(file){
  raw <- read_delim(file, col_names = FALSE) %>% 
    select(c(X3, X4, X5, X6, X7, X20, X21))
}

# Bind county-level data together
merged <- lapply(filelist, read_clean_raw)
merged <- bind_rows(merged)
colnames(merged) <- c("surname", "suffix", "first", "middle", "exempt", "gender", "race")

# Drop individuals exempted from public records 
print(paste0("Number of exempt individuals: ", sum(merged$exempt == "Y")))
merged <- merged %>% filter(exempt == "N")

# Recode gender and race variables to dummies
merged <- merged %>% 
  mutate(
    female = as.numeric(gender == "F"),
    native_am = as.numeric(race == 1), 
    asian = as.numeric(race == 2), 
    black = as.numeric(race == 3), 
    hispanic = as.numeric(race == 4), 
    white = as.numeric(race == 5), 
    other = as.numeric(race == 6), 
    multi = as.numeric(race == 7), 
    unknown = as.numeric(race == 9)
  ) %>% 
  select(!c(exempt, gender, race))

# Export -----------------------------------------------------------------------

write_csv(merged, "data/output/fl_voter_file.csv")
