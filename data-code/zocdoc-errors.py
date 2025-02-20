import numpy as np
import pandas as pd
from deepface import DeepFace
import cv2
import os 
from os import listdir
import string

folder_dir = "./data/input/zocdoc/from-iu/full-data/Profile Pictures/"
error_csv = "./data/output/unreadable_images_iu.csv"

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

# Define new DataFrame for errors
error_columns = ["zocdoc_id", "degree", "name", "error_message"]
error_df = pd.DataFrame(columns=error_columns)


# Re-process files to detect unreadable images
print("Processing unreadable images...")
batch_size = 100
for i in range(0, len(files), batch_size):
    batch = files[i:i + batch_size]
    
    batch_errors = []  # Store errors in a temporary list to minimize DataFrame operations

    for filename in batch:
        try:
            # Extract file information
            root, ext = os.path.splitext(filename)

            if "-" in root:
                left, z_id = root.rsplit("-", 1)
            else:
                left = root
                z_id = "unknown"

            left = left + "-"

            # Extract degrees from filename
            degree_list = [d for d in possible_degrees if d in left]
            clean_degree = [d.replace("-", "") for d in degree_list]

            for d in degree_list:
                left = left.replace(d, "")

            left = left.rstrip("-")
            name = left.replace('-', ' ')
            name = string.capwords(name)

            # Read image
            img = cv2.imread(os.path.join(folder_dir, filename))
            if img is None:
                raise ValueError("Image unreadable or corrupted")

            # Run DeepFace analysis
            _ = DeepFace.analyze(os.path.join(folder_dir, filename), actions=['race'], enforce_detection=False)

        except Exception as e:
            # Store error row in list
            batch_errors.append({'zocdoc_id': z_id, 'degree': ", ".join(clean_degree), 'name': name, 'error_message': str(e)})

    # Convert batch errors to DataFrame
    if batch_errors:
        batch_df = pd.DataFrame(batch_errors, columns=error_columns)

        # Append to CSV file
        if not os.path.exists(error_csv):
            batch_df.to_csv(error_csv, index=False, header=True)  # Write with header if file doesn't exist
        else:
            batch_df.to_csv(error_csv, mode='a', index=False, header=False)  # Append without header

    print(f"Processed {i + len(batch)} / {len(files)} files...")

print("\nError processing completed.")