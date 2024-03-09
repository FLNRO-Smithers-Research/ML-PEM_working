library(stars)
library(rgee)
library(tmap)
library(rgeeExtra)
#remotes::install_github("r-earthengine/rgeeExtra")

#ee_install()
ee_Initialize(drive = TRUE)


# Define study area folder
AOI <- "Wetzinkwa"
AOI_dir <- file.path(".", paste0(AOI, "_AOI"))
AOI_epsg <- 3005
cov_dir <- file.path(AOI_dir, "1_map_inputs", "covariates")

template <- file.path(AOI_dir, "1_map_inputs", "covariates","25m")

template_raster <- file.path(template, "template.tif")

#dem <- file.path(AOI_dir, "1_map_inputs", "covariates", "dem.tif")
aoi <- file.path(AOI_dir, "0_raw_inputs", "base_layers", "aoi_snapped.gpkg" )

aoi_ee <- AOI_bbox <- st_read(aoi, quiet = TRUE) %>%
  st_bbox() %>%
  st_as_sfc() %>%
  st_buffer(dist = 250, joinStyle = "MITRE") %>%
  st_transform(4326)


# Define path for file processing (doesn't have to exist)
raw_dir <- file.path(AOI_dir, "0_raw_inputs", "satellite_layers")



#Next, set up a function that will create the Earth Engine tasks to process cloud free Sentinel-2 bands. This took a while to figure out! The function requires 3 inputs:
  
#1) An AOI polygon or multipolygon. This is provided as an sf object, converted to a bounding box, and then the bounding box shape is used as the Earth Engine polygon area of interest
#2) A data.frame of date ranges used to limit the Earth Engine processing. This data.frame has 3 columns: an ID column, a start date column, and an end date column. I'm still thinking of ways to make that better, but for now that's how it is
#3) A dsn (data source name) for where to place the downloaded/processed data.

#The Earth Engine function works in the following manner:
#1) Filters the sentinel-2 images taken between the specified start and end dates
#2) Removes clouds from each of the images for each of the bands (bands are identified as beginning with the letter "B")
#3) Takes the median value at each pixel for the band "stack"

#Additionally, the true color bands are downloaded using their raw values (0-255). After processing, Google Earth Engine requires that the bands are saved to a Google Drive folder (or Google Cloud Services (GCS) repository, but that costs money). The bands for the specified date ranges are downloaded locally and then processed to produce the satellite indices. Satellite indices are saved within the specified DSN. Only indices that have enough data are kept (usually this means tossing out the EVI2 file).

#This function keeps all of the bands at their native resolutions throughout and results with each satellite index at a raw resolution of 10m^2.


# Function requires that rgee is working
get_satellite_indices <- function(aoi, date_ranges, dsn = NULL) {
  
  #testing lines
  aoi = AOI_bbox
  date_ranges = seasons_df
  dsn = raw_dir

  
  # Evaluate aoi input
  if(!inherits(aoi, c("sf", "sfc")))
    stop("'aoi' must be of class 'sf' or 'sfc'")
  
  if(!st_geometry_type(aoi, by_geometry = FALSE) %in% c("POLYGON", "MULTIPOLYGON"))
    stop("'aoi' must be an sf or sfc POLYGON or MULTIPOLYGON")
  
  # Evaluate date_ranges input
  if(ncol(date_ranges) != 3)
    stop(
      "'date_ranges' should be a data.frame object with 3 character class columns.
    \rColumn 1 should be a character value indicating the subfolder for the specified date range.
    \rColumn 2 should be a character value indicating the starting point of the date range.
    \rColumn 3 should be a character value indicating the ending point of the date range.")
  
  if(!all(apply(date_ranges, 2, is.character)))
    stop(
      "'date_ranges' should be a data.frame object with 3 character class columns.
    \rColumn 1 should be a character value indicating the subfolder for the specified date range.
    \rColumn 2 should be a character value indicating the starting point of the date range.
    \rColumn 3 should be a character value indicating the ending point of the date range.")
  
  # Evaluate dsn input
  if(is.null(dsn)) dsn <- getwd()
  if(!is.character(dsn))
    stop("'dsn' requires a valid file path as a character input")
  
  # Create directories for saving and reprojecting
  dl_dir <- file.path(dsn, "1_download")
  ind_dir <- file.path(dsn, "2_indices")
  
  dir.create(dl_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(ind_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Set geometry for use in GEE (should make checks for polygon/multipolygon type)
  # Should force conversion to sf bounding box!
  # Projection doesn't matter, it is set properly in the download function
  ee_geom <- st_geometry(aoi) %>% 
    st_bbox() %>% 
    st_as_sfc() %>% 
    sf_as_ee()
  
  # Determine some important metadata RE sentinel 2 satellites
  sen <- ee$ImageCollection("COPERNICUS/S2_SR")
  bands <- grep("^B.*|^TCI.*", sen$first()$bandNames()$getInfo(), value = TRUE)
  scales <- sapply(bands, function(x) 
    sen$first()$select(x)$projection()$nominalScale()$getInfo())
  
  # Create a cloud masking function which will run in GEE
  cloud_mask <- function(image) {
    qa <- image$select("QA60")
    cloudBitMask <- bitwShiftL(1, 10)
    cirrusBitMask <- bitwShiftL(1, 11)
    mask <- qa$bitwiseAnd(cloudBitMask)$eq(0)$
      And(qa$bitwiseAnd(cirrusBitMask)$eq(0))
    image <- image$select("B.*")$updateMask(mask)$divide(10000L)
  }
  
  # Function selects the TCI images as well (easier viewing in GIS programs)
  add_tci <- function(image) {
    image$select("TCI.*")
  }
  
  # Perform filtering operations in GEE
  gee_run <- lapply(1:nrow(date_ranges), function(i) {
    i = 1
    dl_subdir <- file.path(dl_dir, date_ranges[i, 1])
    ind_subdir <- file.path(ind_dir, date_ranges[i, 1])
    
    dir.create(dl_subdir, showWarnings = FALSE)
    dir.create(ind_subdir, showWarnings = FALSE)
    
    dataset <- sen$
      filterDate(date_ranges[i, 2], date_ranges[i, 3])$
      filter(ee$Filter$lt("CLOUDY_PIXEL_PERCENTAGE", 20))
    
    rgb <- dataset$map(add_tci)$median()
    composite <- dataset$map(cloud_mask)$median()$addBands(rgb)
    
    message("Downloading Sentinel-2 Level-2A bands via GEE for dates between ", 
            date_ranges[i, 2], " and ", date_ranges[i, 3])
    # Composite image is made up of each sentinel 2 band. Need to extract the
    # bands and run the function to save the files to the download directory
    gee_dl <- lapply(bands, function(x) {
      ee_as_raster(
        composite$select(x), 
        region = ee_geom, 
        dsn = file.path(dl_subdir, paste0(x, ".tif")), 
        scale = scales[x], 
        lazy = TRUE, quiet = TRUE)})
    
    rast_bands <- ee_utils_future_value(gee_dl) %>% setNames(lapply(., names))
    message("Downloading finished! Producing satellite indices")
    
    # At this point, should perform calculations for satellite indices. Extract
    # the 10 and 20m raster bands only
    tci <- raster::stack(rast_bands[startsWith(names(rast_bands), "TCI")])
    bands_10 <- names(scales)[!startsWith(names(scales), "TCI") & scales == 10]
    bands_20 <- names(scales)[!startsWith(names(scales), "TCI") & scales == 20]
    bands_60 <- names(scales)[!startsWith(names(scales), "TCI") & scales == 60]
    res_10 <- raster::stack(rast_bands[bands_10]) %>% setMinMax()
    res_20 <- raster::stack(rast_bands[bands_20]) %>% setMinMax()
    
    # Perform pan sharpening on the 20m bands
    pan <- panSharpen(res_20, mean(res_10, na.rm = TRUE), method = "pca", norm = FALSE)
    
    # Stack the 10m bands and pan sharpened bands that are now 10m
    satellite_stack <- raster::stack(res_10, pan) %>% 
      setNames(gsub("_pan$", "", names(.)))
    
    # Create satellite indices in a single function
    satellite_indices <- spectralIndices(
      img = satellite_stack, 
      blue = "B2", green = "B3", red = "B4", nir = "B8", 
      redEdge1 = "B5", redEdge2 = "B6", redEdge3 = "B7", 
      swir2 = "B11", swir3 = "B12", 
      coefs = list(swir2ccc = max(0, minValue(satellite_stack$B11)), 
                   swir2coc = min(1, maxValue(satellite_stack$B11))))
    
    # Workaround for proper name setting
    ind_names <- names(satellite_indices)
    tci_names <- names(tci)
    satellite_indices <- rast(satellite_indices) %>% setNames(ind_names)
    tci <- rast(tci) %>% setNames(tci_names)
    
    # Remove layers that have missing data
    layer_check <- data.frame(freq(satellite_indices, value = NA)) %>% 
      dplyr::mutate(layer = names(satellite_indices[layer]), 
                    count = ncell(satellite_indices) - count) %>% 
      dplyr::filter(count > 0.95 * median(count))
    
    indices_flt <- subset(satellite_indices, layer_check$layer)
    f_names <- file.path(ind_subdir, paste0(names(indices_flt), ".tif"))
    
    # Write out files
    message("Cleaning Google Drive container and writing outputs")
    ee_clean_container(quiet = TRUE)
    tci1 <- writeRaster(tci, file.path(ind_subdir, "true_color.tif"), overwrite = TRUE)
    out <- writeRaster(indices_flt, f_names, overwrite = TRUE)
    raster::removeTmpFiles(h = 0)
    return(out)
    
  }) %>% setNames(date_ranges[, 1])
  
  return(gee_run)
}



#Finally, we just need to run the function. The candidate AOI is buffered by 250m, the above function is run, and then each folder of satellite indices gets reprojected to match the DEM at each resolution. The result is masked to match the extent and shape of the DEM, and then each file is saved separately.


# Create a buffered bounding box of the AOI
AOI_bbox <- st_buffer(aoi_ee, 250)

# Provide a data frame of dates to collect data between. The longer the data
# frame, the longer it will take to process and download your dataset.
seasons_df <- data.frame(
  season = c( "summer19"),
  start = c("2019-06-21"),
  end = c("2019-09-22"),
  stringsAsFactors = FALSE
)

# Run get_satellite_indices function to download and process sentinel indices
sentinel_indices <- get_satellite_indices(
  aoi = AOI_bbox,
  date_ranges = seasons_df,
  dsn = raw_dir)





# The function above outputs a list format. Create a single SpatRaster of all
# of the satellite indices
sentinel_dirs <- list.dirs(file.path(raw_dir, "2_indices"), recursive = FALSE)
sentinel_indices <- rast(lapply(sentinel_dirs, function(x) {
  rast(grep("true_color.tif", 
            list.files(x, pattern = ".tif$", full.names = TRUE), 
            invert = TRUE, value = TRUE)) %>% 
    setNames(tolower(paste0("sentinel2_", names(.), "_", basename(x))))}))

# Remove progress bars for below reprojection
def_ops <- terra:::spatOptions()$progress
terraOptions(progress = 0)
# Using the raw satellite indices, reproject them, mask, and save them out.
# Note: This takes quite a while for 4m resolution, maybe better to do it by
# layer? Not sure, not tested.
sentinel_reproject <- lapply(dem_list, function(x) {
  out_dir <- dirname(x)
  cat("\nReprojecting satellite indices to", basename(out_dir))
  dem <- rast(x)
  out <- terra::project(sentinel_indices, dem) %>% 
    mask(dem) %>% 
    writeRaster(filename = file.path(out_dir, paste0(names(.), ".tif")),
                overwrite = TRUE)
  suppressWarnings(tmpFiles(remove = TRUE))
  return(out)
}) %>% setNames(basename(dirname(dem_list)))
# Reset progress bars
terraOptions(progress = def_ops)






################################
# old version

ee_geom <- as.numeric(paste(t(st_coordinates(aoi_ee)[, c("X", "Y")])))
ee_geom <- ee$Geometry$Polygon(coords = ee_geom)

aoi <- ee$Geometry$Polygon(ee_geom)
  
# Create a cloud masking function which will run in GEE
cloud_mask <- function(image) {
  qa <- image$select("QA60")
  cloudBitMask <- bitwShiftL(1, 10)
  cirrusBitMask <- bitwShiftL(1, 11)
  mask <- qa$bitwiseAnd(cloudBitMask)$eq(0)$
    And(qa$bitwiseAnd(cirrusBitMask)$eq(0))
  image <- image$updateMask(mask)$divide(10000L)$
    select("B.*")$
    copyProperties(image, list("system:time_start"))
}

# Create the parent folder for the seasonal exports in your Google Drive folder
try(drive_mkdir(drive_folder, path = "~", overwrite = FALSE), silent = TRUE)

# Create a dataframe of Sentinel 2 band names and their associated resolutions
bands <- data.frame(
  band = c("B1", "B2", "B3", "B4", "B5", "B6", 
           "B7", "B8", "B9", "B11", "B12", "B8A"), 
  new_name = c("b01", "b02", "b03", "b04", "b05", "b06", 
               "b07", "b08", "b09", "b11", "b12", "b8A"),
  res = c(60, 10, 10, 10, 20, 20, 20, 10, 60, 20, 20, 20), 
  stringsAsFactors = FALSE)

s2 <- ee$ImageCollection("COPERNICUS/S2_SR")

sen <- s2$
  filterBounds(ee_geom)$
  filter(ee$Filter$lte("CLOUDY_PIXEL_PERCENTAGE", 20))$
  filter(ee$Filter$date('2019-06-01', '2019-09-22'))$ 
  map(cloud_mask)


nimages <- sen$size()$getInfo()
ic_date <- ee_get_date_ic()
