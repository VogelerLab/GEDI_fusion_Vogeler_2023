# -*- coding: utf-8 -*-
"""
Name:    02_gediRat.py
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
from shapely.geometry import Polygon
import geopandas as gpd
import subprocess
import json
import pdal
import time
import sys 
import os

import gedi_sim_parameter_file as param
import lasheader_class as LH


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


def write_list(l, fp_l):
    # PURPOSE:
    #  convert a list to a text file
    # INPUTS:
    #  l (list) - a list
    #  fp_l (str) - file path of the output text file
    # OUTPUTS:
    #  a text file of the list contents
    with open(fp_l, "w") as output:
        for e in l:
            output.write(e)
            output.write("\n")

# def calculate_extent(file):
#     # PURPOSE:
#     #  calculates the extent of the lidar file from the header info
#     # INPUTS:
#     #  file - file path to the lidar file
#     # OUTPUTS
#     #  returns tuple of x_min, x_max, y_min, y_max
#     info_pipeline = [
#             {
#                 "filename": file, 
#                 "type": "readers.las",
#             }            
#         ]
        
#     info_pipeline = pdal.Pipeline(json.dumps(info_pipeline))
#     info_pipeline.execute()
    
#     #metadata = json.loads(info_pipeline.metadata)["metadata"]
#     metadata = info_pipeline.metadata["metadata"]
#     x_min = metadata["readers.las"]["minx"]
#     x_max = metadata["readers.las"]["maxx"]
#     y_min = metadata["readers.las"]["miny"]
#     y_max = metadata["readers.las"]["maxy"]
    
#     return x_min, x_max, y_min, y_max, file

def calculate_extent(file):
    # PURPOSE:
    #  calculates the extent of the lidar file from the header info
    # INPUTS:
    #  file - file path to the lidar file
    # OUTPUTS
    #  returns tuple of x_min, x_max, y_min, y_max   
    lh = LH.Las_Header(file)
    x_min = lh.xmin
    x_max = lh.xmax
    y_min = lh.ymin
    y_max = lh.ymax
    
    return x_min, x_max, y_min, y_max, file    

def find_min(ans, index):
    for i, e in enumerate(ans):
        if i == 0:
            val = e[index]
        else:
            val = min(val, e[index])
    return(val)

def find_max(ans, index):
    for i, e in enumerate(ans):
        if i == 0:
            val = e[index]
        else:
            val = max(val, e[index])
    return(val)


def calculate_aoi_extent(x_min, x_max, y_min, y_max, cell_size, origin):
    # PURPOSE:
    #  calculates the grid extent of the lidar unit
    # INPUTS:
    #  x_min, x_max, y_min, y_max - (numeric) of the lidar returns
    #  cell_size (tuple) - (x_cell_resolution, y_cell_resolution)
    #  origin (tuple) - (x_cell_origin, y_cell_origin)
    # OUTPUTS
    #  returns tuple of x_min, x_max, y_min, y_max of the aoi 
    aoi_x_min = x_min - x_min % cell_size[0]
    aoi_y_min = y_min - y_min % cell_size[1]
    
    aoi_x_max = x_max - x_max % cell_size[0] + cell_size[0]
    aoi_y_max = y_max - y_max % cell_size[1] + cell_size[1]
    
    return aoi_x_min, aoi_x_max, aoi_y_min, aoi_y_max

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

def main(param_dict):

    start = time.time()

    #dir_home_folder = param_dict["dir_out"]
    dir_als_5070 = param_dict["dir_als_5070"]

    # Directory that holds a text file of paths to lidar that needs to be processed
    dir_list = param_dict["dir_list"]
    fp_las_list = param_dict["fp_las_list"]
    dir_L1B_waveforms = param_dict["dir_L1B_waveforms"]
    dir_bash_scripts = param_dict["dir_bash_scripts"]
    fp_bash_script_gediRat = param_dict["fp_bash_script_gediRat"]
    n_cores_max = param_dict["n_cores_max"]
    
    # Processing Block Size
    block_size = param_dict["block_size"]
    
    # specifics about the pixels
    cell_size = param_dict["cell_size"]
    origin = param_dict["origin"]

    dir_this_bash_scripts = os.path.join(dir_bash_scripts, "gediRat")

    
    # -----------------------------------------------------------------------------
    # list of LAS files
    # -----------------------------------------------------------------------------
    
    if not os.path.exists(dir_list):
        os.mkdir(dir_list)
    
    
    als_list = os.listdir(dir_als_5070)
    
    als_list = [os.path.join(dir_als_5070, e) for e in als_list]
    
    #create a list of lidar las files
    write_list(l=als_list, fp_l=fp_las_list)
    
    # -----------------------------------------------------------------------------
    # Find extent of lidar unit
    # -----------------------------------------------------------------------------
    
    print("\tfinding lidar extent")
    
    n_cores = calc_n_cores(als_list, n_cores_max)
    ans=Parallel(n_jobs=n_cores)(
        delayed(calculate_extent)(file)
        for file in als_list
    )
    del n_cores 
    
    x_min = find_min(ans, 0)
    x_max = find_max(ans, 1)
    y_min = find_min(ans, 2)
    y_max = find_max(ans, 3)
    
    # extent of the aoi  
    aoi_x_min, aoi_x_max, aoi_y_min, aoi_y_max = calculate_aoi_extent(x_min, x_max, y_min, y_max, cell_size, origin)  
    
    # -----------------------------------------------------------------------------
    # Generate processing blocks
    # -----------------------------------------------------------------------------

    if not os.path.exists(dir_L1B_waveforms):
        os.mkdir(dir_L1B_waveforms)
    
    if not os.path.exists(dir_bash_scripts):
        os.mkdir(dir_bash_scripts)
    
    if not os.path.exists(dir_this_bash_scripts):
        os.mkdir(dir_this_bash_scripts)
        
    # calculates the extent of the processing blocks
    processing_x_min, processing_x_max, processing_y_min, processing_y_max = calculate_aoi_extent(aoi_x_min, aoi_x_max, aoi_y_min, aoi_y_max, (block_size,block_size), (0,0))
    
    #convert to integers
    processing_x_min = int(processing_x_min)
    processing_x_max = int(processing_x_max)
    processing_y_min = int(processing_y_min)
    processing_y_max = int(processing_y_max)
    
    # -----------------------------------------------------------------------------
    # Polygon of ALS tiles
    # -----------------------------------------------------------------------------
    
    gdf = gpd.GeoDataFrame()
    
    for i in range(len(ans)):
        x_min = find_min([ans[i]], 0)
        x_max = find_max([ans[i]], 1)
        y_min = find_min([ans[i]], 2)
        y_max = find_max([ans[i]], 3)
        tile_path = ans[i][4]
        
        x_point_list = [x_min, x_max, x_max, x_min, x_min]
        y_point_list = [y_max, y_max, y_min, y_min, y_max]
        
        polygon_geom = Polygon(zip(x_point_list, y_point_list))
        crs = 'epsg:5070'
        this_polygon = gpd.GeoDataFrame(index=[i], crs=crs, geometry=[polygon_geom])
        this_polygon["fp"] = tile_path
        #gdf = gpd.GeoDataFrame(pd.concat([gdf, this_polygon], ignore_index=True))
        gdf = gdf.append(this_polygon)

    # -----------------------------------------------------------------------------
    # Create bash commands
    # -----------------------------------------------------------------------------

    cmds = []
    for col in range(processing_x_min, processing_x_max, block_size):
        for row in range(processing_y_min, processing_y_max, block_size):
            
            #find las in processing block
            x_point_list = [col, col+block_size, col+block_size, col, col]
            y_point_list = [row+block_size, row+block_size, row, row, row+block_size]
            
            polygon_geom = Polygon(zip(x_point_list, y_point_list))
            crs = 'epsg:5070'
            block_polygon = gpd.GeoDataFrame(index=[0], crs=crs, geometry=[polygon_geom])
            
            # fps = gpd.sjoin(gdf, block_polygon)["fp"].to_list()
            # print(fps)
            
            # fps = []
            # for i in range(len(ans)):
            #     x_min = find_min([ans[i]], 0)
            #     x_max = find_max([ans[i]], 1)
            #     y_min = find_min([ans[i]], 2)
            #     y_max = find_max([ans[i]], 3)
            #     tile_path = ans[i][4]        
            #     if x_min > (col) and x_min < (col + block_size) and y_min > (row) and y_min < (row + block_size):
            #         fps.append(tile_path)
            # print(fps)
            
            fps = gpd.sjoin(block_polygon, gdf)["fp"].to_list()
            #print(fps)
                        
            if len(fps) == 0:
                continue

            this_fp_las_list = os.path.join(dir_list ,str(row) + "_" + str(col) + ".txt")
            #create a list of lidar las files
            write_list(l=fps, fp_l=this_fp_las_list)
            
    
            # construct command
            cmd = (
                #"singularity exec --bind /mnt ~/singularity-ce-3.10.0/gediSingularity " + #Gates
                "singularity exec --bind /mnt ~/gediSingularity " + #Denali
                "gediRat " +
                "-inList " + this_fp_las_list + " " +
                "-hdf " +
                "-gridStep 30 " +
                "-gridBound " + str(col) +" "+ str(col+block_size) +" "+ str(row) +" "+ str(row+block_size)  + " "+
                #"-checkCover " +
                "-pBuff 8 " +
                "-output " + os.path.join(dir_L1B_waveforms, "grid_"+str(col)+"_"+str(row)+".h5") + " " + 
                "-ground" + 
                " "
                )
    
            cmds.append(cmd)
            if col == processing_x_min and row == processing_y_min:
                with open(fp_bash_script_gediRat, 'w') as f:
                    f.write(cmd)
                    f.write('\n')
            else:
                with open(fp_bash_script_gediRat, 'a') as f:
                    f.write(cmd)
                    f.write('\n')
            
            this_fp_bash_script_gediRat = os.path.join(
                dir_this_bash_scripts, "gediRat_"+str(col) + "_" + str(row) + ".sh"
                )
            with open(this_fp_bash_script_gediRat, 'w') as f:
                f.write(cmd)
                f.write('\n')
           

    # Run scripts
    cmd_bash =  fp_bash_script_gediRat
    #print(cmd_bash)
    #subprocess.run(["bash", cmd_bash])

    
    print("\tcreating L1B waveforms")
    
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


