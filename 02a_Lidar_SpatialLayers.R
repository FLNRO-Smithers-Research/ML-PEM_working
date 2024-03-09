# Lidar post processing script 
# written by gen perkins 2022-08-24
# create neighbour smoothing for canopy metrics generetd by Lidar 


# read in canopy metrics and generate neighbouhood metrics: 
# https://grass.osgeo.org/grass78/manuals/r.neighbors.html

# median, stdev, variance, diversity, interspersion, quart1, quart2, perc90, 


# how do you want to deal with NA values (omit from calcs?, currenlt as default (all cells))



# options for 

library(terra)
library(dplyr)


AOI <- "Deception"


AOI_dir <- file.path(paste0(AOI, "_AOI"))
cov_dir <- file.path(AOI_dir, "1_map_inputs", "covariates")
res_folder <- "5m"

# set list of functions


testrast <- terra::rast(file.path(cov_dir, res_folder,"template.tif"))
cm_list <- list.files(file.path(cov_dir , res_folder), pattern = "_rproj", full.names = TRUE)
tomatch <- c("vc", "cov_gap", "p90")
cm_list <- grep(paste(tomatch, collapse = "|"), cm_list, value = TRUE)
cm_list <- grep("_mosaic_rproj.tif", cm_list, value = TRUE)


# roll through the list: 
for (i in 1:length(cm_list)){
    #i = 2
    rr <- cm_list[i]
    cm <- terra::rast(rr)
    print(rr)

    # focal_mean
    rnew <- gsub("_rproj.tif", "_rproj_ave.tif", basename(rr)) 
    neighmean <- terra::focal(cm, w = 5, na.policy = "omit",na.rm=TRUE, fun = "mean")
    writeRaster(neighmean, file.path(cov_dir, res_folder, "focal", rnew ))
    
    # focal st dev
    neighsd <- terra::focal(cm, w = 5, na.policy = "omit",na.rm=TRUE, fun = "sd")
    rnew <- gsub("_rproj.tif", "_rproj_std.tif", basename(rr)) 
    writeRaster(neighsd, file.path(cov_dir, res_folder, "focal", rnew ))
    
    # focal median 
    neighmed <- terra::focal(cm, w = 5, na.policy = "omit", na.rm=TRUE, fun = "median")
    rnew <- gsub("_rproj.tif", "_rproj_median.tif", basename(rr)) 
    writeRaster(neighmed, file.path(cov_dir, res_folder, "focal", rnew ))
} 
    
  
    