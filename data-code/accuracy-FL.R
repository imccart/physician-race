# About ------------------------------------------------------------------------

# Compare wru predictions with FL voter data
# Author:         Shirley Cai 
# Date created:   02/05/2024 
# Last edited:    05/09/2024 

# Helper functions -------------------------------------------------------------

set.seed(1234)

# Function that takes in physician race prediction df and FL voter df and returns a merged df
join_datasets <- function(physician, voter){
  voter <- voter %>% 
    select(surname, first, voter_race) %>% 
    mutate(
      surname = toupper(surname),
      first = toupper(first)
    )  %>%
    distinct(surname, first, voter_race)
  
  merged <- physician %>% 
    mutate(
      surname = toupper(surname), 
      first = toupper(first)
    ) %>% 
    left_join(voter, 
              by = c('surname', 'first'), 
              multiple = 'any') 
  
  # Matching statistics
  message(paste0("Total physicians: ", nrow(merged)))
  message(paste0("Number unmatched: ", sum(is.na(merged$voter_race))))
  message(paste0("Proportion unmatched: ", sum(is.na(merged$voter_race))/nrow(merged)))
  
  merged <- merged %>% 
    filter(!is.na(voter_race))
  return(merged)
}

# Import and clean data --------------------------------------------------------

voter <- read_csv("data/output/fl_voter_file.csv") %>% 
  mutate(
    other = other + native_am + multi,
    voter_race = case_when(
      white == 1 ~ "white",
      black == 1 ~ "black", 
      hispanic == 1 ~ "hisp", 
      asian == 1 ~ "asian", 
      other == 1 ~ "other", 
      TRUE ~ "unknown"
    )) %>% 
  filter(unknown == 0) %>% 
  select(!c(native_am, multi, unknown))

name_only_col <- c("white_name", "black_name", "hisp_name", 
                   "asian_name", "other_name")
name_geo_col <- c("white_namegeo", "black_namegeo", "hisp_namegeo", 
                  "asian_namegeo", "other_namegeo")

mdppas <- read_csv("data/output/wru-mdppas.csv") %>% 
  filter(state == "FL") %>%
  rowwise() %>% 
  mutate(
    pred_race = sub("_.*", "", name_only_col[which.max(c_across(all_of(name_only_col)))]), 
    pred_racegeo = sub("_.*", "", name_geo_col[which.max(c_across(all_of(name_geo_col)))])
  )

merged <- join_datasets(mdppas, voter)
rm(list=setdiff(ls(), "merged"))

merged <- merged %>% 
  mutate(
    Dblack = ifelse(voter_race == "black", 1, 0), 
    Dpred_black = ifelse(pred_race == "black", 1, 0),
    Dpredgeo_black = ifelse(pred_racegeo == "black", 1, 0),
    Dname_correct = ifelse(pred_race == voter_race, 1, 0),
    Dnamegeo_correct = ifelse(pred_racegeo == voter_race, 1, 0), 
    max_p_name = pmax(white_name, black_name, hisp_name, asian_name, other_name), 
    max_p_namegeo = pmax(white_namegeo, black_namegeo, hisp_namegeo, asian_namegeo, other_namegeo)
  )

write_csv(merged, "data/output/fl-pred-voter.csv")

# Comparison -------------------------------------------------------------------

name_acc <- sum(merged$Dname_correct[merged$Dpred_black==1]) / sum(merged$Dpred_black)
namegeo_acc <- sum(merged$Dnamegeo_correct[merged$Dpredgeo_black==1]) / sum(merged$Dpredgeo_black)

print(paste0("Total observations: ", nrow(merged)))
print(paste0("Obs. black: ", sum(merged$Dblack)))
print(paste0("Name-only accuracy: ", name_acc))
print(paste0("Name + location accuracy: ", namegeo_acc))

# Confusion matrices from caret
confusionMatrix(as.factor(merged$pred_race), as.factor(merged$voter_race))
confusionMatrix(as.factor(merged$pred_racegeo), as.factor(merged$voter_race))

## Table of accuracy -----------------------------------------------------------

# Group by actual race
summ_actual <- merged %>% 
  group_by(voter_race) %>% 
  summarise(n_actual = n()) %>% 
  rename(race = voter_race)

# Group by name-only race predictions
summ_name <- merged %>% 
  group_by(pred_race) %>% 
  summarise(
    n_pred = n(), 
    accuracy = sum(Dname_correct) / n_pred, 
    p_mean = mean(max_p_name),
    p_std_dev = sd(max_p_name),
    p_min = min(max_p_name),
    p_q25 = quantile(max_p_name, probs = 0.25), 
    p_q50 = quantile(max_p_name, probs = 0.50), 
    p_q75 = quantile(max_p_name, probs = 0.75), 
    p_max = max(max_p_name)
  ) %>% 
  rename(race = pred_race)

# Group by name + location race predictions 
summ_namegeo <- merged %>% 
  group_by(pred_racegeo) %>% 
  summarise(
    n_pred = n(), 
    accuracy = sum(Dnamegeo_correct) / n_pred, 
    p_mean = mean(max_p_namegeo),
    p_std_dev = sd(max_p_namegeo),
    p_min = min(max_p_namegeo),
    p_q25 = quantile(max_p_namegeo, probs = 0.25), 
    p_q50 = quantile(max_p_namegeo, probs = 0.50), 
    p_q75 = quantile(max_p_namegeo, probs = 0.75), 
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
    caption = paste0("Horizonal black line indicates the number of black physicians in sample: ", sum(Dblack),".")
  ) +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom"
  )
ggsave("results/wru-mdppas-accuracy-name-only.jpg")

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
    caption = paste0("Horizonal black line indicates the number of black physicians in sample: ", sum(Dblack),".")
  ) +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom"
  )
ggsave("results/wru-mdppas-accuracy-name-geo.jpg")
