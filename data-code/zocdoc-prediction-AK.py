import numpy as np
import pandas as pd
from deepface import DeepFace
import cv2
import os 
from os import listdir
import string

folder_dir = "./data/input/zocdoc/from-aaron-kaye/full photos/"
df = pd.DataFrame(columns = ["zocdoc_id", "degree", "name", "dominant_race", "asian", 
                             "indian", "black", "white", "middle_eastern", "latino_hispanic"])

# save list of file names in directory
files = os.listdir(folder_dir)
# remove any file named .DS_Store
files = [f for f in files if not f.startswith('.DS_Store')]

# Interate through all files in folder 
for filename in files:

    # Splice file name
    root, ext = os.path.splitext(filename)

    # remove the word copy from the file name
    if "copy" in root:
        root = root.replace("copy", "")

    # Check if the file name contains a dash before splitting
    if "-" in root:
        left, z_id = root.rsplit("-", 1)
    else:
    # Handle cases where no dash exists
        left = root
        z_id = "unknown"

    # add a dash to the end of left
    left = left + "-"

    # make a list of possible degrees that occur with a dash before and after
    possible_degrees = ["-phd", "-do-", "-dds-", "-dmd-", "-md-", "-fnp-", "-crnp-", "-np-", "-pa-", "-rn-", "-msn-", "-ms-", 
                        "-ma-", "-mba-", "-mpa-", "-mph-", "-mha-", "-msw-", "-mdiv-",
                         "-mhs-", "-apn-", "-facp-", "-facc-", "-aprn-", "-fagd-", "-bs-", "-msd-", "-anp-",
                         "-arnp-", "-nd-", "-fccp-", "-faap-", "-magd-", "-dnp-", "-bds-", "-cnm-",
                         "-cac-", "-lac-", "-nd-", "-fasn-", "-dc-", "-pa-c-"]

    # create a list of all degrees found in the file name
    degree = [d for d in possible_degrees if d in left]
    degree = [d.replace("-", "") for d in degree]

    # remove degrees from the filename
    for d in degree:
        left = left.replace(d, "")

    # remove trailing dashes
    left = left.rstrip("-")

    # proper case for names
    name = left.replace('-', ' ')
    name = string.capwords(name)

    # Analyze race using deepface
    img = cv2.imread(folder_dir + filename)
    try:
        result = DeepFace.analyze(folder_dir + filename, actions=['race'])
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
    except:
        dominant_race = "picture does not contain a face"
        row = pd.Series({'zocdoc_id':z_id, 'degree':degree, 'name':name, 'dominant_race':dominant_race,
                     'asian':None, 'indian':None, 'black':None, 'white':None,
                     'middle_eastern':None, 'latino_hispanic':None})
        df = pd.concat([df, row.to_frame().T], ignore_index=True)


df.to_csv("./data/output/zocdoc_extract_ak.csv", index=False, header=True)
print(df.head())



