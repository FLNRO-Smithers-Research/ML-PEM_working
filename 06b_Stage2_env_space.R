library (landmap)
library(rgdal)
library(maxlike)
library(spatstat)
library(sf)
library(plotKML)

# assess sample probability
#  https://rdrr.io/cran/GSIF/man/spsample.prob.html


# set work dir
setwd("../../../../../../../Boundary_PEM")

# point to dataset 
trans.dir <- "TransectData/All Layers"
trans <- list.files(trans.dir, pattern = ".shp", full.names = F, recursive = T)

pnts <- trans[2]

# read in the transect data and convert to spatial points 
t1 <- st_read(dsn = file.path(trans.dir,pnts)) %>% 
  st_transform(3005) %>%
  st_zm() %>%
  dplyr::select(X2MapUnit1) %>%
  as(.,"Spatial")

# convert to spatial points 
bo.pnts <- SpatialPoints(t1, proj4string = (CRS(
   "+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 +y_0=0
   +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs" )))

 
 
# read in spatial data frame 
layers <- list.files(file.path("layers", "25m"), pattern = ".tif", full.names = T, recursive = T)
layers.1 <- layers[c(1:4)]

bo.spc <- stack(layers)
rTemp <- raster(file.path("Layers", "Boundary_25m_DEM.tif")) # get a tempate raster in 3005 proj 

# project to 3005 CRS
bo.spc <- projectRaster(bo.spc,rTemp, crs = "+init=epsg:3005", method="bilinear")
bo.spc <- as(bo.spc, 'SpatialPixelsDataFrame')

bo.spc

# run the sample probability 
bo.prob.pnts = landmap::spsample.prob(bo.pnts, bo.spc)

# can turn off the distance measures 

# running but throwing an warning messgae 

#Deriving kernel density map using sigma 24.4 ...
#Creating a 15.1-megapixel window mask
#Deriving inclusion probabilities using MaxLike analysis...
#Warning messages:
#  1: In .local(observations, covariates, ...) :
#  'Sigma' set at 24.4. Consider increasing the value.
#2: Creating a 15.1-megapixel window mask 
#3: In doTryCatch(return(expr), name, parentenv, handler) :
#  restarting interrupted promise evaluation
#4: In sqrt(diag(vc)) : NaNs produced


plot(raster(bo.prob.pnts [[1]]), zlim=c(0,1), col=SAGA_pal[[1]])
points(bo.prob.pnts [["observations"]])


plot(bo.prob.pnts)

# working! 
crs(bo.pnts)
crs(bo.spc)

#But be careful - the maxlike function can be quite computational for large stack of rasters! Also, you need to install the spatstat and maxlike packages before you can run it.



# Option 2:  --------------------------------------------------------------

# Using dismo 

library(dismo)

# set work dir
setwd("../../../../../../../Boundary_PEM")

# point to dataset 
trans.dir <- "TransectData/All Layers"
trans <- list.files(trans.dir, pattern = ".shp", full.names = F, recursive = T)

pnts <- trans[2]

newcrs <- "+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

# read in the transect data 
t1 <- st_read(dsn = file.path(trans.dir,pnts)) %>% 
  st_transform(newcrs) %>%
  st_zm() %>%
  dplyr::select(X2MapUnit1) %>%
  st_coordinates()

# read in spatial data frame 
layers <- list.files(file.path("layers", "25m"), pattern = ".tif", full.names = T, recursive = T)
#layers.1 <- layers[c(1:4)]

bo.spc <- stack(layers)
rTemp <- raster(file.path("Layers", "Boundary_25m_DEM.tif")) # get a tempate raster in 3005 proj 

bo.spc <- stack(layers)
names(bo.spc) <- c('dem1', 'mrvbf')


bo.pnts <- extract(bo.spc, t1)

# run the sample probability 

ms <- mess(bo.spc, bo.pnts, full=TRUE)
plot(ms)

head(mess)

saveRDS(ms, file = "tmp\\bo.mess.R")

head(mess)

saveRDS(ms, file = "tmp\\bo.mess.R")

# 
# 
#   > Pamela has also sent me a link to an interesting paper
# > (https://www.nature.com/articles/s41598-019-50376-w) in which they use so the called MESS:
#   > 
#   > https://rdrr.io/cran/dismo/man/mess.html
# > 
#   > which gives an abstract feature distance measure (negative values indicate potential extrapolation). For more details about the method see Appendix S3 in:
#   > https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/j.2041-21
# > 0X.2010.00036.x
# > 
#   > If you produce results of both spsample.prob and mess, you should get a clear idea of the extrapolation problems. My spsample.prob is probably somewhat to complex as it combines both geographical and feature space inclusion probabilities. Mess on the other hand (what a unlucky name!) produces abstract values, although the authors claim that it is straight forward to use.
# > 
#   > HTH,
