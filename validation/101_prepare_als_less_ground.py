# -*- coding: utf-8 -*-
"""
Name:    01_prepare_als.py
Purpose: Copy lidar files; project to EPSG:5070; save as las
Author:  PA Fekety, Colorado State University, patrick.fekety@colostate.edu
Date:    2022-08-17

"""

"""
Notes:

"""

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# Import packages
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
from joblib import Parallel, delayed
import subprocess
import shutil
import json
import pdal
import time
import sys
import os


import gedi_sim_parameter_file as param

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# Assign Variables
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

cmd_call = sys.argv


# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# Define Functions
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

def create_param_dict(cmd_call):
    param_dict = {}
    for i in range(1,len(cmd_call)):
    
        if cmd_call[i].startswith("-"):
            key = cmd_call[i].strip("-")
            param_dict[key] = None
        else:
            #add to previous
            val = cmd_call[i]
            param_dict[key] = val

    return(param_dict)


# "parallel" functions are used with joblib
def parallel_project_func(lidar_file, dir_lidar_in, dir_lidar_out, srs_in):
    # Function used to project the laz files to EPSG 5070

    # File name of projected LAZ file
    las_file_5070 = os.path.join(dir_lidar_out, lidar_file[:-4] + ".las")

    # pipepline depending if the CRS is defined
    if srs_in == None:
        # The SRS is in the lidar file
        reproject_pipeline = [
            {
                "filename": os.path.join(dir_lidar_in, lidar_file), 
                "type": "readers.las",
            },
            {"type": "filters.reprojection", "out_srs": "EPSG:5070+5703",},
            {"type":"filters.hag_nn"},
            {"type":"filters.assign", "value":"Classification = 2 WHERE HeightAboveGround <= 0.6"},
            {
                "type":"filters.range",
                "limits":"Classification[0:5]"
            },
            {
                "type": "writers.las",
                "scale_x": "0.01",
                "scale_y": "0.01",
                "scale_z": "0.01",
                "offset_x": "auto",
                "offset_y": "auto",
                "offset_z": "auto",
                "filename": las_file_5070,
            },
        ]
    else:
        # Explicitly define SRS
        reproject_pipeline = [
            {
                "filename": os.path.join(dir_lidar_in, lidar_file),
                "type": "readers.las",
                "spatialreference": str(srs_in),
            },
            {
                "type": "filters.reprojection",
                "in_srs": str(srs_in),
                "out_srs": "EPSG:5070+5703",
            },
            {"type":"filters.hag_nn"},
            {"type":"filters.assign", "value":"Classification = 2 WHERE HeightAboveGround <= 0.6"},
            {
                "type":"filters.range",
                "limits":"Classification[0:5]"
            },
            {
                "type": "writers.las",
                "scale_x": "0.01",
                "scale_y": "0.01",
                "scale_z": "0.01",
                "offset_x": "auto",
                "offset_y": "auto",
                "offset_z": "auto",
                "filename": las_file_5070,
            },
        ]
    pipeline = pdal.Pipeline(json.dumps(reproject_pipeline))
    try:
        pipeline.execute()
    except Exception as err:
        # Write and error file
        fp_error_log = os.path.join(dir_lidar_out, "_Error.log")
        cmd_error_1 = "echo " + "PDAL Reprojection Error " + " >> " + fp_error_log
        cmd_error_2 = "echo " + "Check " + lidar_file + " >> " + fp_error_log
        cmd_error_3 = "echo " + str(err) + " >> " + fp_error_log
        subprocess.run(cmd_error_1, shell=True)
        subprocess.run(cmd_error_2, shell=True)
        subprocess.run(cmd_error_3, shell=True)

    time.sleep(0.01)


def calc_n_cores(x, n_cores_max):
    # Calculates the number of cores that should be dedicated to parallel processes
    # x (list) object whose length will be compared
    # nCoresMax (int) - maximum number of processing cores available
    if n_cores_max > len(x):
        n_cores = len(x)
    else:
        n_cores = n_cores_max
    if n_cores < 1:
        n_cores = 1
    return int(n_cores)



def main(param_dict):

    start = time.time()
   
    dir_home_folder = param_dict["dir_out"]
    dir_lidar_original = param_dict["dir_las"]
    dir_points = param_dict["dir_points"]
    dir_lidar_copy = param_dict["dir_lidar_copy"]
    dir_als_5070 = param_dict["dir_als_5070"]
    n_cores_max = param_dict["n_cores_max"]

    # ----------------------------------------------------------------------------
    # Copy lidar point files to local machine
    # ----------------------------------------------------------------------------
    
    # Copy Lidar Files
    if "local_copy" in param_dict:
        print("\tCopying Lidar Files")
    
    
    # Lidar files to be processed
    lidar_files_original = os.listdir(dir_lidar_original)
    lidar_files_original.sort()
    
    # if not os.path.exists(dir_main_output):
    #     os.mkdir(dir_main_output)
    
    if not os.path.exists(dir_home_folder):
        os.mkdir(dir_home_folder)
    
    if not os.path.exists(dir_points):
        os.mkdir(dir_points)
    
    
    # Create a local copy of the lidar if needed:
    if "local_copy" in param_dict:
        if not os.path.exists(dir_lidar_copy):
            os.mkdir(dir_lidar_copy)
        
        for lidar_file in lidar_files_original:
            shutil.copy(
                src=os.path.join(dir_lidar_original, lidar_file),
                dst=os.path.join(dir_lidar_copy, lidar_file),
            )
        del lidar_file
        del lidar_files_original
        
    else:
        dir_lidar_copy = dir_lidar_original
        
        
    # ----------------------------------------------------------------------------
    # Project to EPSG 5070
    # ----------------------------------------------------------------------------
    
    print("\tProjecting Lidar Files")
    
    # Checks if SRS is needed
    if "-srs_in" in param_dict:
        srs_in = param_dict["-srs_in"]
    else:
        srs_in = None
    
    dir_als_5070 = os.path.join(dir_points, "las_5070")
    if not os.path.exists(dir_als_5070):
        os.mkdir(dir_als_5070)
    
    lidar_files_to_process = os.listdir(dir_lidar_copy)
    lidar_files_to_process.sort()
    print("\tnumber of ALS files = " + str(len(lidar_files_to_process)))

    
    n_cores = calc_n_cores(lidar_files_to_process, n_cores_max)
    Parallel(n_jobs=n_cores)(
        delayed(parallel_project_func)(lidar_file, dir_lidar_copy, dir_als_5070, srs_in)
        for lidar_file in lidar_files_to_process
    )
    del n_cores
    del lidar_files_to_process
    
    # remove the copy of Lidar files
    if "local_copy" in param_dict:
        shutil.rmtree(dir_lidar_copy)
    
    # Move the Error log
    if os.path.exists(os.path.join(dir_als_5070, "_Error.log")):
        # Move the error log
        shutil.move(
            os.path.join(dir_als_5070, "_Error.log"),
            os.path.join(dir_home_folder, "_Error.log"),
        )
    
    
    # ----------------------------------------------------------------------------
    # Error Checking
    # ----------------------------------------------------------------------------
    # parallelProjectFunc() creates a log file if it failed reprojecting a file
    # Notify the user of the error and copy the error log
    
    if os.path.exists(os.path.join(dir_home_folder, "_Error.log")):
        print("\n")
        print("Errors Exist")
    
        # Print contents to console
        with open(os.path.join(dir_home_folder, "_Error.log")) as f:
            print(f.read())
    
    
    stop = time.time()
    print(str(round(stop - start) / 60) + " minutes to complete.")

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# Processing
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

# Parameters passed from command line
param_dict = create_param_dict(cmd_call)

# From paramter file
if not "n_cores_max" in param_dict:
    param_dict["n_cores_max"] = param.n_cores_max
param_dict["n_cores_max"] = int(param_dict["n_cores_max"])
param_dict["block_size"] = param.block_size
param_dict["cell_size"] = param.cell_size
param_dict["origin"] = param.origin


# Add these parameters to param_dict
# relating to ALS processing
param_dict["dir_list"] = os.path.join(param_dict["dir_out"], "list_of_las_files")
param_dict["fp_las_list"] = os.path.join(param_dict["dir_list"], "las_files.txt")
param_dict["dir_points"] = os.path.join(param_dict["dir_out"], "points")
param_dict["dir_lidar_copy"] = os.path.join(param_dict["dir_points"], "lidar_files_copy")
param_dict["dir_als_5070"] = os.path.join(param_dict["dir_points"], "las_5070")

# Directory that holds a text file of paths to lidar that needs to be processed
param_dict["dir_bash_scripts"] = os.path.join(param_dict["dir_out"], "bash_scripts")
param_dict["fp_bash_script_gediRat"] = os.path.join(param_dict["dir_bash_scripts"], "gediRat.sh")
param_dict["fp_bash_script_gridMetric"] = os.path.join(param_dict["dir_bash_scripts"], "gridMetric.sh")

# Relating to waveform processing1
param_dict["dir_L1B_waveforms"] = os.path.join(param_dict["dir_out"], "L1B_waveforms")
param_dict["dir_L2_products"] = os.path.join(param_dict["dir_out"], "L2_products")

# output rasters
param_dict["dir_grids"] = os.path.join(param_dict["dir_out"], "gridded_products")

   
if __name__ == "__main__":
    main(param_dict)

