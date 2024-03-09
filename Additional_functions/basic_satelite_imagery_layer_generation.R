

devtools::install_github("16EAGLE/getSpatialData")
library(getSpatialData)
library(raster)
library(sf)
library(sp)
library(mapview)
library(velox) 
library(tidyverse)
library(RStoolbox)
#library(mapedit)
library(rasterVis)
library(purrr)
#library(geosphere)
library(RColorBrewer)

######################################################################

# 1: downsample course scales for finer resolution

AOI <- "Deception"
target_res <- 10 
downscale_res = 25

AOI_dir <- file.path(".", paste0(AOI,"_AOI"))
cov_dir <- file.path(AOI_dir, "1_map_inputs", "covariates")


large_res <- list.files(file.path(cov_dir, paste0(downscale_res,"m")), full.names = TRUE )
target_raster <- list.files(file.path(cov_dir, paste0(target_res,"m")), full.names = TRUE, pattern="\\.tif$")
out_file <- file.path(cov_dir, paste0(target_res,"m"), "downscale") 

ifelse(!dir.exists(file.path(out_file )),              #if tmpOut Does not Exists
       dir.create(file.path(out_file ), recursive = TRUE), "Directory Already Exisits")        #create tmpOut
  
# load target raster
template_raster <- raster::raster(target_raster[1])
#stest <- raster::stack(target_raster) 
#large_res <- large_res[94:123]

for(i in 1:length(large_res)){
      ### testing parms
      i <- large_res[i]
      targetRes <- 10
      output = out_file
      print(paste("Processing:", i))
      r  <- raster::raster(i)
      px <- raster::res(r)[1]
      r  <- raster::disaggregate(r, px/targetRes)  ## This will through an error if not an integer
    
      r_proj <- raster::projectRaster(from = r, to= template_raster, method="bilinear")
      xx <- raster::stack(template_raster, r_proj)
      raster::writeRaster(r_proj, file.path(out_file , gsub(".tif", "_d25.tif", basename(i))), overwrite = TRUE)
      
      #xx <- raster::stack(template_raster, r)
      #xx <- raster::stack(list.files(out_file, full.names = TRUE))
      #xxx <- r_proj - template_raster
      #plot(xxx)
      }
 

########################################################################################

# 2. Derive layers with Sentinel single band data

#######################################################################################


AOI <- "Deception"
res <- "2.5m" 

AOI_dir <- file.path(".", paste0(AOI,"_AOI"))
raw_dir <- file.path(AOI_dir, "0_raw_inputs", "satellite_layers")
cov_dir <- file.path(AOI_dir, "1_map_inputs", "covariates")



all <- list.files(file.path(cov_dir, res), full.names = T) 
#all <- all[14:48]

allst <- stack(all)

rtemp <- raster(file.path(cov_dir, res,"TPI.tif"))###example for use as a template in 

rtemp.wgs <- projectRaster(rtemp,
                           crs = "+proj=utm +zone=9 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0") 

# Point to saved sentinel data location. 

sen.dir = file.path(raw_dir, "\\S2B_MSIL1C_20180714T193859_N0206_R042_T09UXA_20180714T225733.SAFE\\GRANULE\\L1C_T09UXA_A007075_20180714T194841\\IMG_DATA")

# Make a list of the files in a directory of interest
jp2 <- list.files(sen.dir, pattern = "\\.tif$", full.names = T) 
jp2_10 <- stack(jp2[c(2,3,4,8)])
jp2_20 <- stack(jp2[c(5,6,7,11,12,13)])
jp2_60 <- stack(jp2[c(1,9,10)])
# # create an RGB to test 
# aoi <- mapview::viewRGB(x = jp2_10,
#                         r = "T09UXA_20180714T193859_B04",
#                         g = "T09UXA_20180714T193859_B03",
#                         b = "T09UXA_20180714T193859_B02",
#                         maxpixels = 1e+05) # %>% mapedit::editMap()
#
# function to prepare sentinel layers to match template 
senprep <- function(senstack, rtemp, crs.out = "+init=epsg:3005"){
  print("please be patient - this will take a moment")
  rtemp.crs <- projectRaster(rtemp, 
                             crs = "+proj=utm +zone=9 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
  print("cropping raster stack to raster template")
  senst <- crop(senstack, rtemp.crs)
  print("reprojecting raster stack")
  senst <- projectRaster(senst , crs = crs.out)
  print("downscaling as required")
  senst <- resample(senst, rtemp, method = "bilinear")
} 
# down scale the stacks
sout <- senprep(jp2_10, rtemp)
sout20 <- senprep(jp2_20, rtemp)
sout60 <- senprep(jp2_60, rtemp)

# stack and rename 
allsen <- stack(sout, sout20, sout60)

# fix the bank names
band_names <- tribble(
  ~band, ~names,
  "blue", "B02",
  "green","B03",
  "red", "B04",
  "nir", "B08",
  "rededge1", "B05",
  "rededge2", "B06",
  "rededge3", "B07",
  "swir1", "B11",
  "swir2", "B12",
  "vegred", "B8A",
  "ultablue", "B01",
  "nir2", "B09",
  "cirrus", "B10"
)


# write out individual bands 
short_names <- as.data.frame(names(allsen)) %>%
  rename("names" = "names(allsen)") %>%
  mutate(names = gsub("T09UXA_20180714T193859_","", names)) %>%
  pull(., names)

names(allsen) <- tolower(short_names)

writeRaster(allsen, file.path(cov_dir, res, ".tif"), bylayer = TRUE, suffix = names(allsen))

# check layers 

all <- list.files(file.path(cov_dir, res), full.names = T) 
#all <- all[14:48]

allst <- stack(all)

# generate SI and write out 
# stack and rename 
allsen <- stack(sout, sout20, sout60)

long_names <- as.data.frame(names(allsen)) %>%
  rename("names" = "names(allsen)") %>%
  mutate(names = gsub("T09UXA_20180714T193859_","", names)) %>%
  left_join(band_names) %>%
  pull(., band)

# Assign new names
names(allsen) <- long_names

# assign names of bands to basic names 

band_names <- gsub("T09UXA_20180714T193859_","", names(allsen))
  
names(allsen) <- band_names

# writ eout basic bands
#writeRaster(allsen, file.path(cov_dir, res, ".tif"), bylayer = TRUE, suffix = names(allsen))


##  Plot multi-band RGB raster stack, three different ways to check the outputs are correct 

# plotRGB(x = allsen, r = "red", g = "green", b = "blue", maxpixels = 1e5, stretch = "lin")
# # mapview::viewRGB
# viewRGB(x = allsen, r = "nir", g = "red", b = "green", maxpixels = 1e5)
# # RStoolbox::ggRGB
# ggRGB(img = allsen, r = "swir1", g = "nir", b = "red", maxpixels = 1e5, stretch = "lin")
# 

# 
# 
# 
# # generate ndvi 1: Rouse1974 	(nir - red)/(nir + red
# ndvi <- spectralIndices(allsen, red = "red", nir = "nir", indices = "NDVI")
# ggR(ndvi)
# 
# # generate normalised differential water index  : McFeeters1996	(green - nir)/(green + nir)
# ndwi <- spectralIndices(allsen, green = "green", nir = "nir", indices = "NDWI")
# ggR(ndwi)
# 
# # nnormalised differential water index : Gao1996	nir, swir2	(nir - swir2)/(nir + swir2)
# ndwi2 <- spectralIndices(allsen, swir2 = "swir2", nir = "nir", indices = "NDWI2")
# ggR(ndwi2) 


# Generate all possible  derivatives at once:   

SI <- spectralIndices(allsen, red = "red", nir = "nir", swir2 = "swir2", green = "green")
names(SI) <- tolower(names(SI))


writeRaster(SI, file.path(cov_dir, res, "sen.tif"), bylayer = TRUE, suffix = names(SI), overwrite = TRUE)


# -------------------------------------------------------------------------------------

# downscale lidar data 

#AOI <- "Deception"
AOI <- "BoundaryTSA"
res <- "5m" 

AOI_dir <- file.path(".", paste0(AOI,"_AOI"))
raw_dir <- file.path(AOI_dir, "0_raw_inputs", "dem", "lidar")
cov_dir <- file.path(AOI_dir, "1_map_inputs", "covariates")

# get template raster
rtemp <- raster(file.path(cov_dir, res,"TPI.tif"))###example for use as a template in 

foi <- list.files(raw_dir, pattern="\\.tif$", full.names = T) 

foi <- list.files(file.path(cov_dir, res))

foi_stack <- stack(foi)

fst <- crop(foi_stack, rtemp)

#if(!crs(fst)== 3005){ fst <- projectRaster(fst, crs = 3005) }

fst <- resample(fst, rtemp, method = "bilinear")

writeRaster(fst, file.path(cov_dir, "lid.tif"), bylayer = TRUE, suffix = names(fst))



list.files(file.path(cov.dir, "10m"), full.names = TRUE)
stack(list.files(file.path(cov.dir, "10m")))
      


##################################################################

# 3. fix tomislav's layer to align to base rasters 

##################################################################

cov_dir <- file.path(AOI_dir, "1_map_inputs", "covariates", "25m")
foi <- list.files(cov_dir, pattern = "^25m", full.names = T) 

foi_stack <- stack(foi)
foi_stack

fst <- crop(foi_stack, rtemp)
fst <- resample(fst, rtemp, method = "bilinear")

writeRaster(fst, file.path(cov_dir, ".tif"), bylayer = TRUE, suffix = names(fst))


# ----------------------------------------------------------

# check layers for Boundary 

AOI <- "BoundaryTSA"
res <- "5m" 

AOI_dir <- file.path(".", paste0(AOI,"_AOI"))
cov_dir <- file.path(AOI_dir, "1_map_inputs", "covariates")

all <- list.files(file.path(cov_dir, res,"final"), full.names = T, pattern = ".tif$") 

allst <- stack(all)

out_dir <- file.path(cov_dir, res, "final")

# generate the bgc layer

for(i in 3:length(all)){
  
  rtemp = raster(all[2])
  rtemp <- stars::st_as_stars(rtemp) 
  st_crs(rtemp) = "+proj=aea +lat_0=45 +lon_0=-126 +lat_1=50 +lat_2=58.5 +x_0=1000000 +y_0=0 +datum=NAD83 +units=m +no_defs"
  
  stars::write_stars(rtemp, file.path(out_dir, "cnetwork.tif"))
  
}

################################################################

# 4. create a bgc layer -------------------------------------

#################################################################

AOI <- "BoundaryTSA"
res <- "5m" 

AOI_dir <- file.path(".", paste0(AOI,"_AOI"))
cov_dir <- file.path(AOI_dir, "1_map_inputs", "covariates")

shapes_dir <- file.path(AOI_dir, "0_raw_inputs", "base_layers")
all <- list.files(file.path(cov_dir, res), full.names = T, pattern = ".tif$") 

# set up the bgc to filter for tiles with particular variant
bgc.sf = st_read(file.path(shapes_dir, "bgc.gpkg")) %>%
  st_transform(3005) %>%
  #mutate(MAP_LABEL = gsub(" ", "", BGC_LABEL)) %>%
  mutate(MAP_LABEL = seq(1:length(unique(MAP_LABEL))))
         
rtemp <- raster(all[20]) ###example for use as a template in 

rtemplate <- stars::st_as_stars(rtemp)
#grd = stars::st_as_stars(st_bbox(rtemplate,  dx = 5, dy = 5, values = NA_real_))

out <- stars::st_rasterize(bgc.sf["MAP_LABEL"],
                           template = rtemp)

library(raster)

polygonsToR

#st_crs(out) = "+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

mapview::mapview(out)

stars::write_stars(out,
                   file.path(cov_dir, res, "bgc.tif")) #tile name



