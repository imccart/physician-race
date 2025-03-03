## Assess accuracy of predicted race

# Clean data with race variables -----------------------------------------------
source('data-code/clean-FL-voter.R')
source('data-code/clean-TX-license.R')

# Assess accuracy of predictions 
source('data-code/accuracy-FL.R')
source('data-code/accuracy-TX.R')


# Summary -----------------------------------------------------------------

np.nppes <- read_csv('data/output/final-nppes-nameprism.csv')
np.mdppas <- read_csv('data/output/final-mdppas-nameprism.csv')

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
