# About ------------------------------------------------------------------------

# Compare wru predictions with TX license data 
# Author:         Shirley Cai 
# Date created:   03/25/2024 
# Last edited:    05/09/2024 

# Import and clean data --------------------------------------------------------

set.seed(1234)

tx <- read_csv("data/output/tx-license-clean.csv") 

tx_lic <- tx %>% 
  filter(!is.na(npi)) %>% 
  mutate(pzip = as.character(pzip)) %>% 
  distinct(npi, race, .keep_all=TRUE)

# Limit MDPPAS extract to only physicians practicing in TX 
mdppas_npi_xw <- read.csv("./data/output/mdppas-npi-xw.csv") %>% 
  filter(state == "TX", spec_broad <= 6) %>% 
  mutate(zip = sprintf("%05d", zip)) 

name_only_col <- c("white_name", "black_name", "hisp_name", 
                   "asian_name", "other_name")
name_geo_col <- c("white_namegeo", "black_namegeo", "hisp_namegeo", 
                  "asian_namegeo", "other_namegeo")

mdppas <- read_csv("data/output/wru-mdppas.csv") %>% 
  filter(state == "TX") %>% 
  rowwise() %>% 
  mutate(
    pred_race = sub("_.*", "", name_only_col[which.max(c_across(all_of(name_only_col)))]), 
    pred_racegeo = sub("_.*", "", name_geo_col[which.max(c_across(all_of(name_geo_col)))])
  )

mdppas_npi <- mdppas_npi_xw %>% 
  left_join(mdppas, by = c('surname', 'first', 'zip')) %>% 
  select(!c(county.x, county.y, zip, state.y)) %>% 
  rename(state = state.x)

mdppas_npi <- mdppas_npi %>% 
  group_by(npi) %>% 
  summarise(
    white_name = mean(white_name), 
    black_name = mean(black_name), 
    hisp_name = mean(hisp_name), 
    asian_name = mean(asian_name), 
    other_name = mean(other_name),
    white_namegeo = mean(white_namegeo), 
    black_namegeo = mean(black_namegeo), 
    hisp_namegeo = mean(hisp_namegeo), 
    asian_namegeo = mean(asian_namegeo), 
    other_namegeo = mean(other_namegeo)
  ) %>% 
  rowwise() %>% 
  mutate(
    pred_race = sub("_.*", "", name_only_col[which.max(c_across(all_of(name_only_col)))]), 
    pred_racegeo = sub("_.*", "", name_geo_col[which.max(c_across(all_of(name_geo_col)))])
  ) 

mdppas_npi_xw_trim <- mdppas_npi_xw %>% 
  group_by(npi) %>% 
  arrange(year) %>% 
  filter(row_number() == 1)

merged_tx <- tx_lic %>% 
  left_join(mdppas_npi, by = c('npi' = 'npi'))
merged_mdppas <- mdppas_npi %>% 
  left_join(mdppas_npi_xw_trim, by = c('npi' = 'npi')) %>% 
  left_join(tx_lic, by = c('npi' = 'npi'))

# Evaluate match quality -------------------------------------------------------

print("TX unmatched to MDPPAS ----------")
print(paste0("Number of observations in license data unmatched to MDPPAS: ", sum(is.na(merged_tx$pred_race))))
print(paste0("Proportion of observations in license data unmatched to MDPPAS: ", sum(is.na(merged_tx$pred_race)) / nrow(merged_tx)))

temp.df <- merged_tx %>% filter(!is.na(pred_race))
n_tx_phys <- length(unique(merged_tx$lic))
n_merged_phys <- length(unique(temp.df$lic))
print(paste0("Number of physicians in license data unmatched to MDPPAS: ", n_tx_phys - n_merged_phys))
print(paste0("Proportion of physicians in license data unmatched to MDPPAS: ", (n_tx_phys - n_merged_phys) / n_tx_phys))


print("MDPPAS unmatched to TX ----------")
print(paste0("Number of physicians in MDPPAS unmatched to license data: ", sum(is.na(merged_mdppas$race))))
print(paste0("Proportion of physicians in in MDPPAS unmatched to license data: ", sum(is.na(merged_mdppas$race)) / nrow(mdppas_npi)))

# Are physicians of certain races more likely to be unmatched to NPI? 
(tab <- table(is.na(tx$npi), tx$race))
print("Proportion of physicians in license data unmatched to NPI")
tab[2, ] / (tab[1, ] + tab[2, ]) 

# Are more recent graduates more likely to be unmatched to NPI?
(tab <- table(is.na(tx$npi), tx$gyr))
tab[2, ] / (tab[1, ] + tab[2, ]) 

# Are more recent graduates more likely to be unmatched to MDPPAS?
(tab <- table(is.na(merged_tx$pred_race), merged_tx$gyr))
tab[2, ] / (tab[1, ] + tab[2, ]) 

# Comparison -------------------------------------------------------------------

merged <- merged_tx %>% 
  mutate(
    Dblack = ifelse(race == "black", 1, 0), 
    Dpred_black = ifelse(pred_race == "black", 1, 0),
    Dpredgeo_black = ifelse(pred_racegeo == "black", 1, 0),
    Dname_correct = ifelse(pred_race == race, 1, 0),
    Dnamegeo_correct = ifelse(pred_racegeo == race, 1, 0), 
    max_p_name = pmax(white_name, black_name, hisp_name, asian_name, other_name), 
    max_p_namegeo = pmax(white_namegeo, black_namegeo, hisp_namegeo, asian_namegeo, other_namegeo)
  )

write_csv(merged, "data/output/tx-pred-license.csv")

# Confusion matrices from caret
confusionMatrix(as.factor(merged$pred_race), as.factor(merged$race))
confusionMatrix(as.factor(merged$pred_racegeo), as.factor(merged$race))

table(merged$Dname_correct, merged$race)

# How accurate are name predictions by predicted race? 
print("Accuracy of name-only predictions by predicted race")
(tab <- table(merged$Dname_correct, merged$pred_race))
tab[2, ] / (tab[1, ] + tab[2, ]) 

print("Accuracy of name-geo predictions by predicted race")
(tab <- table(merged$Dnamegeo_correct, merged$pred_racegeo))
tab[2, ] / (tab[1, ] + tab[2, ]) 

## Table of accuracy -----------------------------------------------------------

merged <- merged %>% filter(!is.na(pred_race))

# Group by actual race
summ_actual <- merged %>% 
  filter(!is.na(Dname_correct)) %>% 
  group_by(race) %>% 
  summarise(n_actual = n()) 

# Group by name-only race predictions
summ_name <- merged %>% 
  filter(!is.na(Dname_correct)) %>% 
  group_by(pred_race) %>% 
  summarise(
    n_pred = n(), 
    accuracy = sum(Dname_correct) / n_pred, 
    p_mean = mean(max_p_name),
    p_std_dev = sd(max_p_name),
    p_min = min(max_p_name),
    p_q25 = quantile(max_p_name, probs = 0.25, na.rm = TRUE), 
    p_q50 = quantile(max_p_name, probs = 0.50, na.rm = TRUE), 
    p_q75 = quantile(max_p_name, probs = 0.75, na.rm = TRUE), 
    p_max = max(max_p_name)
  ) %>% 
  rename(race = pred_race)

# Group by name + location race predictions 
summ_namegeo <- merged %>% 
  filter(!is.na(Dname_correct)) %>% 
  group_by(pred_racegeo) %>% 
  summarise(
    n_pred = n(), 
    accuracy = sum(Dnamegeo_correct) / n_pred, 
    p_mean = mean(max_p_namegeo),
    p_std_dev = sd(max_p_namegeo),
    p_min = min(max_p_namegeo),
    p_q25 = quantile(max_p_namegeo, probs = 0.25, na.rm = TRUE), 
    p_q50 = quantile(max_p_namegeo, probs = 0.50, na.rm = TRUE), 
    p_q75 = quantile(max_p_namegeo, probs = 0.75, na.rm = TRUE), 
    p_max = max(max_p_namegeo)
  ) %>% 
  rename(race = pred_racegeo)

# Join tables
tab_name <- summ_actual %>% 
  left_join(summ_name, by = "race", keep = FALSE) 
tab_namegeo <- summ_actual %>% 
  left_join(summ_namegeo, by = "race", keep = FALSE)  

# TODO: Format and export tables 

## Threshold comparison graphs -------------------------------------------------

threshold_grid <- seq(0, 1, 0.01)
n_name_pred <- rep(0, length(threshold_grid))
n_namegeo_pred <- rep(0, length(threshold_grid))
acc_name <- rep(0, length(threshold_grid))
acc_namegeo <- rep(0, length(threshold_grid))

for(i in 1:length(threshold_grid)){
  # Obs. is black if predicted black prob >= threshold
  thres <- threshold_grid[i]
  pred_name_black <- ifelse(merged$black_name >= thres, 1, 0)
  pred_namegeo_black <- ifelse(merged$black_namegeo >= thres, 1, 0)
  
  # Number of obs. predicted to be black
  n_name_pred[i] <- sum(pred_name_black)
  n_namegeo_pred[i] <- sum(pred_namegeo_black)
  
  # Accuracy of prediction within those predicted to be black
  acc_name[i] <- sum(pred_name_black + merged$Dblack == 2) / n_name_pred[i]
  acc_namegeo[i] <- sum(pred_namegeo_black + merged$Dblack == 2) / n_namegeo_pred[i]
}

ggplot() + 
  geom_line(aes(threshold_grid, acc_name, color="Accuracy"), linewidth = 0.75) + 
  geom_line(aes(threshold_grid, n_name_pred / nrow(merged), color="Number predicted black"), linewidth = 0.75) + 
  geom_hline(yintercept = sum(merged$Dblack) / nrow(merged)) + 
  scale_y_continuous(
    name = "Accuracy", 
    sec.axis = sec_axis(~.*nrow(merged), name = "Num. predicted to be black")
  ) +
  labs(
    title = "Accuracy using name-only predictions", 
    x = "Threshold", 
    caption = paste0("Horizonal black line indicates the number of black physicians in sample: ", sum(merged$Dblack),".")
  ) +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom"
  )
ggsave("results/TX-accuracy-name-only.jpg")

ggplot() + 
  geom_line(aes(threshold_grid, acc_namegeo, color="Accuracy"), linewidth = 0.75) + 
  geom_line(aes(threshold_grid, n_namegeo_pred / nrow(merged), color="Number predicted black"), linewidth = 0.75) + 
  geom_hline(yintercept = sum(merged$Dblack) / nrow(merged)) + 
  scale_y_continuous(
    name = "Accuracy", 
    sec.axis = sec_axis(~.*nrow(merged), name = "Num. predicted to be black")
  ) +
  labs(
    title = "Accuracy using name + location predictions", 
    x = "Threshold", 
    caption = paste0("Horizonal black line indicates the number of black physicians in sample: ", sum(merged$Dblack),".")
  ) +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom"
  )
ggsave("results/TX-accuracy-name-geo.jpg")
