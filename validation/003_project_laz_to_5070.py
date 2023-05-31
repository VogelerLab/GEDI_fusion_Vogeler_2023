# -*- coding: utf-8 -*-
"""
Name:    003_project_laz_to_5070.py
Purpose: project to EPSG:5070
Author:  PA Fekety, Colorado State University, patrick.fekety@colostate.edu
Date:    2023-03-03

"""

"""
Notes:
  There are some sleep calls. These were added during the workflow 
    to aid in debugging. I do not believe they are still needed.
  
  Run with LAZs and No FUSION Indexing
"""

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# Import packages
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
import os
import shutil
import pdal
import json
import time
from joblib import Parallel, delayed
import subprocess


# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# Assign Variables
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
start = time.time()

# Assign aoi name
aois = [
    "Canyon_Creek_2018-06-15",
    "CO_Central_Western_2016",
    "CO_DRCOG_3_2020",
    "CO_SanLuisJuanMiguel_5_2020",
    "CO_Southwest_NRCS_B1_2018",
    "IPNF2019I",
    "Olympics_2017",
    "OR_NRCSUSGS_2_2019",
    "OR_RogueRiverSiskiyouNF_B1_2019",
    "payette2017QL1",
    "QuartzUpperJoe2016",
    "Siskiyou_2017",
    "tieton_2018",
    "USGS_ID_Franklin_Bear_2017",
    "USGS_ID_NorthForkPayette_2020",
    "WA_Klickitat_3DEP_2019",
    "WA_Olympics_South_Opsw_2019",
    "WY_Southwest_1_2020"
]

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# Assign Spatial Reference Systems
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

# PDAL can read SRS directly from the LAS file. Some of the older LAS files
# do not have embeded SRS info. If you want to use the SRS in the lidar file,
# then do not include the key:pair (studyArea:epgsCode) in the dictionary.
# If the SRS is missing from the lidar file, or you want to be explicity,
# include the SRS in the dictionary

# Vertical EPSG Code: 6360 (US survey Foot) GEOID 12B
# Vertical EPSG Code: 8228 (US International Foot)
# Vertical EPSG Code: 5703 (meters) Geoid Model GEOID 18

# Spatial Reference Systems of different aoi areas
dict_srs = {
    "Canyon_Creek_2018-06-15": "4269+5703", #noaa  latlon
    "CO_Central_Western_2016": "6428+6360", 
    "CO_DRCOG_3_2020": "6342+5703",
    "CO_SanLuisJuanMiguel_5_2020": "6341+5703",
    "CO_Southwest_NRCS_B1_2018": "6350+5703",
    "IPNF2019I": "26911+5703",
    "Olympics_2017": "2927+6360",
    "OR_NRCSUSGS_2_2019": "6557+8228",
    "OR_RogueRiverSiskiyouNF_B1_2019": "6339+5703",
    "payette2017QL1": "6340+5703",
    "QuartzUpperJoe2016": "26911+5703",
    "Siskiyou_2017": "4269+5703",# noaa  latlon
    "tieton_2018": "2927+6360",
    "USGS_ID_Franklin_Bear_2017": "6341+5703",
    "USGS_ID_NorthForkPayette_2020": "6340+5703",
    "WA_Klickitat_3DEP_2019": "2927+6360",
    "WA_Olympics_South_Opsw_2019": "2927+6360",
    "WY_Southwest_1_2020": "6341+5703",
}

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# Define Functions
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

# "parallel" functions are used with joblib
def parallel_project_func(lidarFile, dirLidarCopy, dirLAZ5070, srsIn):
    # Function used to project the laz files to EPSG 5070

    # File name of projected LAZ file
    lasfile5070 = os.path.join(dirLAZ5070, lidarFile[:-4] + ".laz")
    
    if not os.path.exists(lasfile5070):

        # pipepline depending if the CRS is defined
        if srsIn == None:
            # The SRS is in the lidar file
            reprojectPipeline = [
                {"filename": os.path.join(dirLidarCopy, lidarFile), "type": "readers.las",},
                {"type": "filters.reprojection", "out_srs": "EPSG:5070+5703",},
                {
                    "type": "writers.las",
                    "scale_x": "0.01",
                    "scale_y": "0.01",
                    "scale_z": "0.01",
                    "offset_x": "auto",
                    "offset_y": "auto",
                    "offset_z": "auto",
                    "compression": "laszip",
                    "filename": lasfile5070,
                },
            ]
        else:
            # Explicitly define SRS
            reprojectPipeline = [
                {
                    "filename": os.path.join(dirLidarCopy, lidarFile),
                    "type": "readers.las",
                    "spatialreference": "EPSG:" + str(srsIn),
                },
                {
                    "type": "filters.reprojection",
                    "in_srs": "EPSG:" + str(srsIn),
                    "out_srs": "EPSG:5070+5703",
                },
                {
                    "type": "writers.las",
                    "scale_x": "0.01",
                    "scale_y": "0.01",
                    "scale_z": "0.01",
                    "offset_x": "auto",
                    "offset_y": "auto",
                    "offset_z": "auto",
                    "compression": "laszip",
                    "filename": lasfile5070,
                },
            ]
        pipeline = pdal.Pipeline(json.dumps(reprojectPipeline))
        try:
            pipeline.execute()
        except Exception as err:
            # Write and error file
            fpErrorLog = os.path.join(dirLAZ5070, "_Error.log")
            cmdError1 = "echo " + "PDAL Reprojection Error " + " >> " + fpErrorLog
            cmdError2 = "echo " + "Check " + lidarFile + " >> " + fpErrorLog
            cmdError3 = "echo " + str(err) + " >> " + fpErrorLog
            subprocess.run(cmdError1, shell=True)
            subprocess.run(cmdError2, shell=True)
            subprocess.run(cmdError3, shell=True)

    time.sleep(0.01)


def calc_n_cores(x, n_cores_max):
    # Calculates the number of cores that should be dedicated to parallel processes
    # x (list) object whose length will be compared
    # n_cores_max (int) - maximum number of processing cores available
    if n_cores_max > len(x):
        n_cores = len(x)
    else:
        n_cores = n_cores_max
    if n_cores < 1:
        n_cores = 1
    return int(n_cores)


# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# Processing
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

for aoi in aois:
    print(aoi)
    
    # Directory of lidar data needing to be processed (e.g., external HHD)
    dir_laz_original = os.path.join(r"O:\gedi_validation\data\als_data", aoi)
    
    
    # Maximum number of processing cores
    n_cores_max = 5
    
    # main output directory
    dir_als_5070 = r"O:\gedi_validation\data\als_data_5070"
    if not os.path.exists(dir_als_5070):
        os.mkdir(dir_als_5070)
    
    # directory for the specific lidar aoi; HOME_FOLDER in FUSION scripts
    dir_als_5070_aoi = os.path.join(dir_als_5070, aoi)
    if not os.path.exists(dir_als_5070_aoi):
        os.mkdir(dir_als_5070_aoi)
    
    
    # Lidar files to be processed
    lidar_files_original = os.listdir(dir_laz_original)
    lidar_files_original.sort()
    
    
    # ----------------------------------------------------------------------------
    # Process Point Data
    # ----------------------------------------------------------------------------
    
    # project to EPSG 5070
    print("\tProjecting Lidar Files")
    srs_needs_defining = aoi in dict_srs
    if srs_needs_defining:
        srs_in = dict_srs[aoi]
    else:
        srs_in = None
    
    n_cores = calc_n_cores(lidar_files_original, n_cores_max)
    print("\treprojecting " + str(len(lidar_files_original)) + " laz files")
    output = Parallel(n_jobs=n_cores)(
        delayed(parallel_project_func)(lidarFile, dir_laz_original, dir_als_5070_aoi, srs_in)
        for lidarFile in lidar_files_original
    )
    del n_cores
    del lidar_files_original
    
    
    stop = time.time()
    
    str(int((stop-start) / 60)) + " minutes"

