### Kiri Daust
### Find areas on map with similar bins to unsample-able points

library(sf)
library(data.table)
library(raster)
library(velox)
library(fasterize)

 
# badPointNames <- c("ICH mc 1_2.1_6_cLHS",
#                    "ICH mc 1_2.4_9_cLHS",
#                    "ICH mc 1_3.3_13_cLHS",
#                    "ICH mc 1_4.3_18_cLHS",
#                    "ICH mc 1_4.4_19_cLHS",
#                    "ICH mc 1_5.5_25_cLHS")
# 
# 
# badPointNames <- c('ICH mc 2_1.4_4_cLHS',
#   'ICH mc 2_2.2_7_cLHS',
#   'ICH mc 2_2.3_8_cLHS',
#   'ICH mc 2_2.5_10_cLHS',
#   'ICH mc 2_3.1_11_cLHS',
#   'ICH mc 2_3.2_12_cLHS',
#   'ICH mc 2_3.3_13_cLHS',
#   'ICH mc 2_3.4_14_cLHS',
#   'ICH mc 2_4.1_16_cLHS',
#   'ICH mc 2_4.4_19_cLHS',
#   'ICH mc 2_5.2_22_cLHS',
#   'ICH mc 2_5.3_23_cLHS',
#   'ICH mc 2_5.4_24_cLHS',
#   'ICH mc 2_5.5_25_cLHS')
# 
# badPointNames <- c('ESSFwv_2.4_9_cLHS',
#                   'ESSFwv_3.1_11_cLHS',
#                    'ESSFwv_2.5_10_cLHS',
#                    'ESSFwv_3.5_15_cLHS')
#                    

badPointNames <- 'ICH mc 1_1.3_3_cLHS'


# 
# # Wills version 
# st_layers("./Test_Files/s1_sampling_V2.gpkg")
# points <- st_read("./Test_Files/s1_sampling_V2.gpkg",layer = "ESSFwv_points_all")
# 
# ##rasterise bgc
# template <- raster("./Test_Files/dah_3class.tif")
# bgc <- st_read("D:/CommonTables/BGC_maps/BGCv12_Feb25.gpkg")
# bgc$fID <- as.integer(as.factor(bgc$BGC))
# bgc <- st_cast(bgc,"MULTIPOLYGON")
# bgcRast <- fasterize(bgc,template,"fID")
# ####
# 
# covariates <- list.files("./Test_Files/",full.names = T)
# covariates <- covariates[grep("tif",covariates)]
# covariates <- covariates[-4]
# ancDat <- raster::stack(covariates)
# ancDat <- stack(ancDat,bgcRast)


# Gen version 

aoi <- "Wetzinkwa"
transect_folder <- file.path(paste0(aoi, "_AOI"), "2_sample_design", "stage1_StudyDesign",
                             "transect_layout")
raster_folder <- file.path(paste0(aoi, "_AOI"), "1_map_inputs", "covariates",
                           "25m")

st_layers(file.path(transect_folder, "s1_sampling.gpkg"))
points <- st_read(file.path(transect_folder, "s1_sampling.gpkg"), "ICH mc 1_points_all")

covariates <- list.files(raster_folder,full.names = T)
covariates <- covariates[grep("tif",covariates)]
covariates <- covariates[-6]
covariates <- covariates[-3]
ancDat <- raster::stack(covariates)


#bgc <- raster(list.files(raster_folder,full.names = T)[1])
# template <- list.files(raster_folder,full.names = T)[2]
# bgc <- st_read("Wetzinkwa_AOI/0_raw_inputs/base_layers/bec.gpkg")
# bgc$fID <- as.integer(as.factor(bgc$BGC_LABEL))
# bgc <- st_cast(bgc,"MULTIPOLYGON")
# bgcRast <- fasterize(bgc,template,"fID")
# ####
# 
# covariates <- list.files("./Test_Files/",full.names = T)
# covariates <- covariates[grep("tif",covariates)]
# covariates <- covariates[-4]
# ancDat <- raster::stack(covariates)
# ancDat <- stack(ancDat,bgcRast)


badPoints <- points[points$id %in% badPointNames, c("id","geom")]

new_sample_space <- function(badPoints,ancDat,basename){##
  for(i in 1:nrow(badPoints)){
    cat("Processing point",i)
    currPt <- badPoints[i,]
    covarVals <- extract(ancDat,currPt)
    sArea <- calc(ancDat, fun = function(x){if(x[1] == covarVals[1,1] 
                                               & x[2] == covarVals[1,2] 
                                               & x[3] == covarVals[1,3] 
                                               & x[4] == covarVals[1,4]) 1 else 0})
    writeRaster(sArea, file.path(transect_folder, paste0("NewSampleArea_",currPt$id,".tif")),format = "GTiff", overwrite = TRUE)
  }
  return(TRUE)
}

new_sample_space(badPoints,ancDat,"Testing_NewSpace_ICHmc1")
