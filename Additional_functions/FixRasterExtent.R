require(raster)
require(data.table)

template = raster("./Deception_AOI/1_map_inputs/covariates/5m/aspect.tif")
r1 = raster("./Deception_AOI/1_map_inputs/covariates/5m/SAVI - Copy.tif")
r.new=resample(r1, template,  "bilinear")
ex = extent(template)
r.new = crop(r.new, ex)
writeRaster(r.new , "./Deception_AOI/1_map_inputs/covariates/5m/SAVI.tif", overwrite = TRUE)
