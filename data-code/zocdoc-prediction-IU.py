import numpy as np
import pandas as pd
from deepface import DeepFace
import cv2
import os 
from os import listdir
import string

folder_dir = "./data/input/zocdoc/from-iu/full-data/Profile Pictures/"
output_csv = "./data/output/zocdoc_extract_iu.csv"

# Define DataFrame columns
columns = ["zocdoc_id", "degree", "name", "dominant_race", "asian", 
           "indian", "black", "white", "middle_eastern", "latino_hispanic"]

# Prepare the DataFrame
df = pd.DataFrame(columns=columns)

# Save list of all folders in directory, removing zip files and .DS_Store
files = os.listdir(folder_dir)
files = [f for f in files if not f.startswith('.DS_Store') and not f.endswith('.zip')]

# Save list of all possible degrees
possible_degrees = ["-phd", "-do-", "-dds-", "-dmd-", "-md-", "-fnp-", "-crnp-", "-np-", "-cpa-", "-rn-", "-msn-", "-ms-", 
                    "-ma-", "-mba-", "-mpa-", "-mph-", "-mha-", "-msw-", "-mdiv-", "-mhs-", "-apn-", "-facp-", "-facc-", "-aprn-", 
                    "-fagd-", "-bs-", "-msd-", "-anp-", "-arnp-", "-nd-", "-fccp-", "-faap-", "-magd-", "-dnp-", "-bds-", "-cnm-", 
                    "-lpcc-", "-pmhnp-", "-cac-", "-lac-", "-nd-", "-fasn-", "-dc-", "-dpm-", "-dpt-", "-lmhc-", "-lcsw-", "-lmft-", 
                    "-lpc-", "-lmsw-", "-od-", "-ap-", "-mbbs-", "-ncc-", "-med-", "-pt-", "-aud-", "-psyd-", "-rdn-", "-cdn-", "-rd-", 
                    "-pmh-", "-bc-", "-lisw-", "-lcmhc-", "-facog-", "-dmsc-", "-cnp-", "-facs-", "-facfas-", "-ap-", "-cctp-", "-pa-", 
                    "-abpp-", "-agnp-", "-mn-", "-fscai-", "-amft-", "-lsw-", "-lmfta-", "-lcpc-", "-mn-", "-amft-", "-lsw", "-lmfta-", 
                    "-lcpc-", "-mspa-", "-cnp-", "-lcswa-", "-lamft-", "-whnp-", "-mpt-", "-lcat-", "-msed-",  "-licsw-", "-ldn-", "-acnpc-", 
                    "-dsw-", "-agpcnp-", "-mps-", "-lcmhca-", "-apc-", "-mftc-", "-lgpc-", "-cmhc-", "-nmd-", "-asw-", "-lpca-", "-lcp-", 
                    "-lmt-", "-apcc-", "-Lp-", "-cns-", "-otr-", "-lcmhcs-", "-mspt-", "-bsn-", "-dacm-", "-ccn-", "-eds-", "-mssa-", 
                    "-daom-", "-msom-", "-lpt-", "-ba-", "-ocs-", "-csw-", "-almft-", "-mft-", "-edm-", "-otd-", "-pa-c-"]

# sort possible degrees longest length to shortest
possible_degrees.sort(key=len, reverse=True)

# Process files in chunks of 100
chunk_size = 100
for i in range(0, len(files), chunk_size):
    chunk = files[i:i + chunk_size]

    for filename in chunk:
        root, ext = os.path.splitext(filename)

        if "-" in root:
            left, z_id = root.rsplit("-", 1)
        else:
            left = root
            z_id = "unknown"

        left = left + "-"

        degree = [d for d in possible_degrees if d in left]
        degree = [d.replace("-", "") for d in degree]

        for d in degree:
            left = left.replace(d, "")

        left = left.rstrip("-")
        name = left.replace('-', ' ')
        name = string.capwords(name)

        img = cv2.imread(folder_dir + filename)
        try:
            result = DeepFace.analyze(folder_dir + filename, actions=['race'], enforce_detection=False)
            try:
                dominant_race = result[0]['dominant_race']
                asian = result[0]['race']['asian'] / 100
                indian = result[0]['race']['indian'] / 100
                black = result[0]['race']['black'] / 100
                white = result[0]['race']['white'] / 100
                middle_eastern = result[0]['race']['middle eastern'] / 100
                latino_hispanic = result[0]['race']['latino hispanic'] / 100
            except KeyError as k:
                print(f"KeyError for file {filename}: {k}")
                dominant_race, asian, indian, black, white, middle_eastern, latino_hispanic = None, None, None, None, None, None, None

            row = pd.Series({'zocdoc_id':z_id, 'degree':degree, 'name':name, 'dominant_race':dominant_race,
                             'asian':asian, 'indian':indian, 'black':black, 'white':white,
                             'middle_eastern':middle_eastern, 'latino_hispanic':latino_hispanic})
            df = pd.concat([df, row.to_frame().T], ignore_index=True)
        except Exception as e:
            print(f"DeepFace error for file {filename}: {e}")
            dominant_race = "picture does not contain a face"
            row = pd.Series({'zocdoc_id':z_id, 'degree':degree, 'name':name, 'dominant_race':dominant_race,
                             'asian':None, 'indian':None, 'black':None, 'white':None,
                             'middle_eastern':None, 'latino_hispanic':None})
            df = pd.concat([df, row.to_frame().T], ignore_index=True)

    # Append results to the CSV file
    if not os.path.exists(output_csv):
        df.to_csv(output_csv, index=False, header=True)
    else:
        df.to_csv(output_csv, mode='a', index=False, header=False)

    # Clear the DataFrame for the next chunk
    df = pd.DataFrame(columns=columns)

print("Processing completed.")


