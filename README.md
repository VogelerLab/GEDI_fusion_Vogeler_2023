# GEDI_fusion_Vogeler_2023
Processing code related to Vogeler et al. 2023 - Evaluating GEDI data fusions for continuous characterizations of forest wildlife habitat

Vogeler, J. C., P. A. Fekety, L. Elliott, N. C. Swayze, S. K. Filippelli, B. Barry, J. D. Holbrook, and K. T. Vierling. 2023. Evaluating GEDI data fusions for continuous characterizations of forest wildlife habitat. Frontiers In Remote Sensing. 4:1196554. https://doi.org/10.3389/frsen.2023.1196554

The contents of this repository are separated into directories, which roughly describe the processing steps they accomplish. All software used are free and open source. It is up to the end user to install and maintain their local versions.
Users may notice exploratory steps and code not discussed in the paper.

The file paths used in these scripts reference storage locations on our work machines and users would need to update file paths to run on their machines.  

## gedi_shots
Scripts used to download and select gedi shots used in these analyses.  

## sentinel_1
Google Earth Engine Colabs notebook to prepare and download Sentinel-1 data used in these analyses.  

## validation
Scripts used to prepare ALS data and process using the gediSimulator (Hancock 2022; [https://bitbucket.org/StevenHancock/gedisimulator/src/master/](https://bitbucket.org/StevenHancock/gedisimulator/src/master/))  

