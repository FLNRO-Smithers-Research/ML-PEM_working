require(terra)
require(data.table)
require(tidyverse)
r <- list.files(file.path(cov_dir, res_folder), pattern = ".tif$", 
           full.names = TRUE) %>% as.list
r2 <- list.files(file.path(cov_dir, res_folder), pattern = ".tif$", 
                full.names = FALSE) %>% as.list
base.raster <- rast(file.path(cov_dir, res_folder, "dem.tif"))
ext <- ext(base.raster)
                    
for(i in length(r)){
  filename <- r[i] %>% as.character
  covname <- r2[i] %>% as.character
x = rast(filename)
x.new <- terra::align(x, ext)
writeRaster(x.new, file.path(cov_dir, res_folder, "new", covname))
}


compareGeom(base.raster, list(r), lyrs=FALSE, crs=TRUE, warncrs=FALSE, ext=TRUE,
            rowcol=TRUE, res=FALSE, stopOnError=TRUE, messages=FALSE)
