# -*- coding: utf-8 -*-
"""
Name:    03_gridMetric.py
Purpose: runs gediRat tool
Author:  PA Fekety, Colorado State University, patrick.fekety@colostate.edu
Date:    2022-08-17

"""


# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# Import packages
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
from joblib import Parallel, delayed
import subprocess
import time
import sys
import os

import gedi_sim_parameter_file as param


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

def parallel_run_bash(cmd, dir_bash_scripts):
    path = os.path.join(dir_bash_scripts, cmd)
    subprocess.run(["bash",path])
       
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# Processing
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------



# -----------------------------------------------------------------------------
# list of LAS files
# -----------------------------------------------------------------------------
def main(param_dict):

    start = time.time()
    
    dir_L1B_waveforms = param_dict["dir_L1B_waveforms"]
    dir_bash_scripts = param_dict["dir_bash_scripts"]
    fp_bash_script_gridMetric = param_dict["fp_bash_script_gridMetric"]
    dir_L2_products = param_dict["dir_L2_products"]
    dir_this_bash_scripts = os.path.join(dir_bash_scripts, "gridMetric")
    n_cores_max = param_dict["n_cores_max"]

    list_L1B = os.listdir(dir_L1B_waveforms)
    
    for l1b in list_L1B:
            
        if not os.path.exists(dir_L2_products):
            os.mkdir(dir_L2_products)
       
        #root file name of the output H5 file
        out_root = l1b.replace(".h5", "")
        
        cmd = (
            #"singularity exec --bind /mnt ~/singularity-ce-3.10.0/gediSingularity " + #Gates
            "singularity exec --bind /mnt ~/gediSingularity " + #Denali
            "gediMetric " + 
            "-input " + os.path.join(dir_L1B_waveforms, l1b) + " " +
            "-outRoot " + os.path.join(dir_L2_products, out_root) + " " +
            "-readHDFgedi " + 
            "-ground " + 
            "-rhRes 1 " +
			"-laiRes 5 " +  
			"-laiH 50 "
        )
    
        
        # write to bash script
        if not os.path.exists(dir_bash_scripts):
            os.mkdir(dir_bash_scripts)
    
        if not os.path.exists(dir_this_bash_scripts):
            os.mkdir(dir_this_bash_scripts)
            
        if l1b == list_L1B[0]:
            with open(fp_bash_script_gridMetric, 'w') as f:
                f.write(cmd)
                f.write('\n')
        else:
            with open(fp_bash_script_gridMetric, 'a') as f:
                f.write(cmd)
                f.write('\n')
        
        fp_bash_script_gediRat = os.path.join(
                dir_this_bash_scripts, "gediRat_"+str(l1b) + ".sh"
                )
        with open(fp_bash_script_gediRat, 'w') as f:
            f.write(cmd)
            f.write('\n')
    
    
    # Run scripts
    cmd_bash =  fp_bash_script_gridMetric
    print(cmd_bash)
    # subprocess.run(["bash", cmd_bash])
    
    bash_scripts = os.listdir(dir_this_bash_scripts)
    n_cores = calc_n_cores(bash_scripts, n_cores_max)
    Parallel(n_jobs=n_cores)(
        delayed(parallel_run_bash)(cmd, dir_this_bash_scripts)
        for cmd in bash_scripts
    )
    del n_cores

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


