# Physician Race

This repo contains the code for different race predictions for physicians, organized into data (input and output) and data-code. The raw data, code, and output are described in more detail below:

## Raw Data

We identify physicians from two sources:
- The [National Provider Identifier (NPI) dataset](https://www.cms.gov/Research-Statistics-Data-and-Systems/Downloadable-Public-Use-Files/Provider-Of-Services/index.html) from the Centers for Medicare and Medicaid Services (CMS). The data is from 2018 and contains information on over 4 million physicians and other healthcare providers. The data is available for download [here](https://www.cms.gov/Research-Statistics-Data-and-Systems/Downloadable-Public-Use-Files/Provider-of-Services/Provider_of_Services.zip). 
- We also use the [MDPPAS](https://resdac.org/cms-data/files/md-ppas) dataset from CMS, which is restricted access.
- `ZIP_COUNTY_122010.xlsx` is the zip code county crosswalk from HUD. We use the crosswalk for 2010 since the Census data used to predict race in the wru package (see below) is from 2010. 
- `stfips_crosswalk.csv` is a state name to FIPS crosswalk sourced from the Bureau of Labor Statistics, available [here](https://www.bls.gov/respondents/mwr/electronic-data-interchange/appendix-d-usps-state-abbreviations-and-fips-codes.htm)


## Code

The code is organized as follows:
- `_main.R` imports the raw data, calls all relevant scripts, and exports the final race predictions
- `nameprism.R` predicts race using [NamePrism](https://www.name-prism.com/), which is a publicly-available tool to predict race and ethnicity from first and last names. This tool is generously made available by Junting Ye at Stony Brook University, Steven Skiena also at Stony Brook, and Yifan Hu now at Amazon.
- `wru-nppes.R` predicts race based on name and geolocation using the NPPES data
- `wru-mdppas.R` predicts race based on name and geolocation using the MDPPAS data
- `clean-FL-voter.R` and `clean-TX-license.R` clean the FL voter files and TX license data, respectively. These are the "validation sets" that contain actual race. The accuracy of the name predictions are assesed by `FL-accuracy.R` and `TX-accuracy.R`, respectively. 

-----

`zocdoc-prediction.py` iterates through all files in a folder and uses the package deepface to predict race. These files are assumed to be images and have file names of the form `hyphenated-name-degree-id.extension`. deepface predicts the probability of being asian, indian, black, white, middle eastern, and hispanic. 

NOTE: Current script is not able to identify when there are more than one degrees listed or when there are no degrees listed. It assumes that there is one and only one degree. Script has not been adapted to zipped folders that the Zocdoc sample is provided in and has only been tested on a small test set. 

## Output

The `_main.R` code file outputs three final data files:

1. `final-nppes-race.csv` 