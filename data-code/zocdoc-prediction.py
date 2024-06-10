import numpy as np
import pandas as pd
from deepface import DeepFace
import cv2
import os 
from os import listdir
import string

wd = "C:/Users/shirl/OneDrive - Emory University/race-concordance/"
folder_dir = wd + "data/zocdoc/subset/"
df = pd.DataFrame(columns = ["zocdoc_id", "degree", "name", "dominant_race", "asian", 
                             "indian", "black", "white", "middle_eastern", "latino_hispanic"])

# Interate through all files in folder 
for filename in os.listdir(folder_dir):

    # Splice file name 
    root, ext = os.path.splitext(filename)
    left, z_id = root.rsplit("-", 1)
    name, degree = left.rsplit("-", 1)
    name = name.replace('-', ' ')
    name = string.capwords(name)

    # Analyze race using deepface
    img = cv2.imread(folder_dir + filename)
    result = DeepFace.analyze(img, actions=['race'])
    dominant_race = result[0]['dominant_race']

    # Convert percentage points to probability 
    asian = result[0]['race']['asian'] / 100
    indian = result[0]['race']['indian'] / 100
    black = result[0]['race']['black'] / 100
    white = result[0]['race']['white'] / 100
    middle_eastern = result[0]['race']['middle eastern'] / 100
    latino_hispanic = result[0]['race']['latino hispanic'] / 100

    # Append to df
    row = pd.Series({'zocdoc_id':z_id, 'degree':degree, 'name':name, 'dominant_race':dominant_race,
                     'asian':asian, 'indian':indian, 'black':black, 'white':white,
                     'middle_eastern':middle_eastern, 'latino_hispanic':latino_hispanic})
    df = pd.concat([df, row.to_frame().T], ignore_index=True)

df.to_csv(wd + "data/output/zocdoc_extract.csv", index=False, header=True)
print(df.head())
