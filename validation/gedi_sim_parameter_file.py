"""
Name:    parameter_file.py
Purpose: Assign parameters common to gedi simulator runs
Author:  PA Fekety, Colorado State University, patrick.fekety@colostate.edu
Date:    2022-08-17

"""

# -------------------------------------------------------------------------------
# -------------------------------------------------------------------------------
# Assign variables
# -------------------------------------------------------------------------------
# -------------------------------------------------------------------------------

# Maximum number of processing cores
n_cores_max = 8


# Processing Block Size (must be a multiple of cell_size)
block_size = 1500

# specifics about the pixels
cell_size = (30,30)
origin = (15,15)

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# Assign Spatial Reference Systems
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
#"EPSG:2231+8228"
