###PEM map despeckle
###Kiri Daust 2019
library(raster)
library(foreach)
library(tidyverse)
library(stars)
library(Rcpp)

setwd("C:/Users/Kiri Daust/Desktop/Deception_SamplingPlan/Stage1Analysis")
sourceCpp("RasterDespeckle.cpp") ###source c++ function

rast <- raster("DespeckleOriginal.tif") ###read in map
plot(rast)

###this script calls a C++ function which replaces each cell with the most common value based
### on a square around it (side length = cellsAround)
### This process is repeated 3 times, applying the function on the previously created map

###version 1##############
cellsAround <- 3 ###how many cells around should we look at - must be odd number
outMat <- pem_focal(as.matrix(rast[[1]]),cellsAround,cellsAround) ###cpp function - needs numeric matrix
outMat[outMat < 0] <- NA ###clean up (side effect of c++ script)
nbRast <- rast
values(nbRast) <- outMat ###replace with new values
plot(nbRast)
writeRaster(nbRast, "Despeckle_v1.tif", format = "GTiff")
####version 2##############
cellsAround <- 3
outMat <- pem_focal(outMat,cellsAround,cellsAround) ###input matrix now the previous output matrix
outMat[outMat < 0] <- NA
nbRast <- rast
values(nbRast) <- outMat
plot(nbRast)
writeRaster(nbRast, "Despeckle_v2.tif", format = "GTiff")
####version 3##############
cellsAround <- 5 ##increase size
outMat <- pem_focal(outMat,cellsAround,cellsAround)
outMat[outMat < 0] <- NA
nbRast <- rast
values(nbRast) <- outMat
plot(nbRast)
writeRaster(nbRast, "Despeckle_v3.tif", format = "GTiff")
#######################################################################

###The below script applies a slightly different algorithm, but is rediculously slow - like reallllly bad

r2 <- layerize(rast) ###convert each unit to a separate layer

require(doParallel)
coreNum <- as.numeric(detectCores()-2)
coreNo <- makeCluster(coreNum)
registerDoParallel(coreNo, cores = coreNum)

###where are there areas < 5 cells big?
smallID <- foreach(lay = names(r2), .combine = c, .packages = c("raster", "foreach")) %dopar% {
  rClump <- clump(r2[[lay]], directions = 8)
  clumps <- unique(values(rClump))
  clumpSize <- foreach(num = clumps, .combine = rbind) %do% {
    size <- length(rClump[rClump == num])
    data.frame(clumpID = num, Size = size)
  }
  small <- clumpSize$clumpID[clumpSize$Size < 5]
  small <- small[!is.na(small)]
  cellNums <- which(values(rClump) %in% small)
  cellNums
}



mat <- matrix(c(1,1,1,
                1,0,1,
                1,1,1), nrow = 3, ncol = 3, byrow = T)

focalFun <- function(x, na.rm){
  if(all(is.na(x))){return(NA)}
  else{
    temp <- table(x[!is.na(x)])
    return((names(temp)[temp == max(temp)])[1])
  }
}

nbRast <- focal(rast, mat, focalFun, na.rm = T) ###similar to c++ function
temp <- rast
temp[smallID] <- nbRast[smallID] ###replace small chunks with values from nbRast
writeRaster(temp, "TestCleaned.tif", overwrite = T, format = "GTiff")
