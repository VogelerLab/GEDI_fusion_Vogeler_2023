# -*- coding: utf-8 -*-
"""
Name:    04_rasterize_products.py
Purpose: runs gediRat tool
Author:  PA Fekety, Colorado State University, patrick.fekety@colostate.edu
Date:    2022-08-17

"""


# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# Import packages
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
from osgeo import gdal             # package for handling geotiff data
from osgeo import osr              # package for handling projection information
import numpy as np
import pandas as pd
import shutil
import os
import sys

import gedi_sim_parameter_file as param


cmd_call = sys.argv
#print(cmd_call)


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


def writeTiff(data,x,y,res,filename="lvis_image.tif",epsg=4326):
  # sourced from https://github.com/edinburgh-university-OOSA/OOSA-code-public/blob/main/week4/geotiff/tiffExample.py
  '''
  Make a geotiff from an array of points
  '''

  # determine bounds
  minX=np.min(x)
  maxX=np.max(x)
  minY=np.min(y)
  maxY=np.max(y)

  # determine image size
  nX=int((maxX-minX)/res+1)
  nY=int((maxY-minY)/res+1)

  # pack in to array
  imageArr=np.full((nY,nX),-999.0)        # make an array of missing data flags

  # calculate the raster pixel index in x and y
  xInds=np.floor((x-np.min(x))/res)
  yInds=np.floor((np.max(y)-y)/res)
  # floor rounds down. y is from top to bottom

  xInds = xInds.astype('int32')
  yInds = yInds.astype('int32')
  
  # this is a simple pack which will assign a single footprint to each pixel
  imageArr[yInds,xInds]=data

  # set geolocation information (note geotiffs count down from top edge in Y)
  geotransform = (minX-res/2, res, 0, maxY+res/2, 0, -res)

  # load data in to geotiff object
  dst_ds = gdal.GetDriverByName('GTiff').Create(filename, nX, nY, 1, gdal.GDT_Float32)

  dst_ds.SetGeoTransform(geotransform)    # specify coords
  srs = osr.SpatialReference()            # establish encoding
  srs.ImportFromEPSG(epsg)                # WGS84 lat/long
  dst_ds.SetProjection(srs.ExportToWkt()) # export coords to file
  dst_ds.GetRasterBand(1).WriteArray(imageArr)  # write image to the raster
  dst_ds.GetRasterBand(1).SetNoDataValue(-999)  # set no data value
  dst_ds.FlushCache()                     # write to disk
  dst_ds = None

  #print("Image written to",filename)
  return


# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# Processing
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

def main(param_dict):

    dir_L2_products = param_dict["dir_L2_products"]
    dir_grids = param_dict["dir_grids"]
    cell_size = param_dict["cell_size"]
    out_ras = param_dict["out_ras"]

    # -----------------------------------------------------------------------------
    # list of LAS files
    # -----------------------------------------------------------------------------
    
    list_L2 = os.listdir(dir_L2_products)
    
    for l2 in list_L2:
        if not os.path.exists(dir_grids):
            os.mkdir(dir_grids)
        dir_temp = os.path.join(dir_grids, "temp")
        if not os.path.exists(dir_temp):
            os.mkdir(dir_temp)
            
        #L2 product that is a text file
        fp_l2_product = os.path.join(dir_L2_products, l2)
        
        #get the column names
        col_names = pd.read_table(fp_l2_product, sep = ",", nrows= 1).columns
    
        col_names
        col_names = col_names.to_list()
        col_names.pop()
        col_names
        
        #now, read the data
        data = pd.read_table(
            fp_l2_product, sep = " ", skiprows = 1, header=None, names=col_names
        )
        
        #find the easting and northing columns
        northing_name = [e for e in col_names if "lat" in e]
        easting_name = [e for e in col_names if "lon" in e]
    
        if out_ras == "ALS cover":
            layer_name = [e for e in col_names if out_ras in e]
        else:
            layer_name = [e for e in col_names if out_ras in e and "ALS" not in e]
            
        if out_ras == "FHD":
           layer_name = [e for e in layer_name if e.endswith("FHD")]
    
        # values that will populate cells
        if layer_name == []:
            print ("ERROR!: " + out_ras + " field is not found in L2 text file")
            sys.exit()
        values = data[layer_name[0]].to_list()
    
    
        basename = l2.replace(".metric.txt", ".tif")
        fp_out_ras = os.path.join(dir_temp, basename)
           
        
        writeTiff(
            data=values, 
            x=data[easting_name[0]],
            y=data[northing_name[0]],
            res = cell_size[0],
            filename=fp_out_ras,
            epsg=5070
        )
    
    
    list_of_rasters = os.listdir(dir_temp)
    fp_tiles =[os.path.join(dir_temp, e) for e in list_of_rasters] 
    
    fp_vrt = os.path.join(dir_temp, "merged.vrt")
    vrt = gdal.BuildVRT(fp_vrt, fp_tiles)
    
    fp_merged = os.path.join(dir_grids, out_ras+".tif").replace(" ", "_")
    
    gdal.Translate(fp_merged, vrt, xRes=cell_size[0], yRes=-cell_size[1])
    vrt = None
    
    shutil.rmtree(dir_temp)
    
    
   
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


