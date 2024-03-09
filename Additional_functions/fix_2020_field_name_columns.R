
library(tidyverse)
library(raster)
library(fasterize)
library(sf)
library(clhs)
library(tools)
library(stringr)
library(lwgeom)
library(dplyr)
library(ranger)
library(foreach)
library(scales)
library(stringr)


## 1. Set up folder structure

AOI <- "Deception"

res <- 2.5

AOI_dir <- file.path(".", paste0(AOI,"_AOI"))
cov_dir <- file.path(AOI_dir, "1_map_inputs", "covariates")
trans_layout <- file.path(AOI_dir, "2_sample_design", "stage1_StudyDesign", "transect_layout")
trans_data <- file.path(AOI_dir, "2_sample_design", "stage1_StudyDesign", "transect_data") 
output_pnts_dir <- file.path(AOI_dir, "2_sample_design", "stage1_StudyDesign", "training_pts")
output_cleaned_dir <- file.path(AOI_dir, "1_map_inputs", "trainingData", "clean")
map.key  <- read.csv(file.path(AOI_dir, "_MapUnitLegend", 
                               paste0(AOI, "_MapUnitLegend.csv")), 
                     stringsAsFactor = FALSE)

res_folder <- paste0(res, "m")

if(!dir.exists(output_pnts_dir)) dir.create(output_pnts_dir, recursive = TRUE)


source(here::here('_functions', 'extend_lines.R'))
source(here::here('_functions', 'make_lines.R'))
source(here::here('_functions', 'transect_sample.R'))
source(here::here('_functions', 'multiline_to_line.R'))



## import base raster of resolution required 
raster_template <- raster(list.files(file.path(cov_dir, res_folder), pattern = ".tif", full.names = TRUE)[1])

#trans <- list.files(trans_layout, pattern = ".gpkg$|.shp$", full.names = TRUE, recursive = TRUE)  
trans <- list.files(trans_layout, pattern = ".gpkg$", full.names = TRUE, recursive = TRUE)

transect_layout <- foreach(x = trans, .combine = rbind) %do% {
  clhs_layers <- st_layers(x)
  lines <- which(clhs_layers[["geomtype"]] %in% c("Line String", "Multi Line String"))
  if(length(lines)) {
    transects <- foreach(y = clhs_layers$name[lines], .combine = rbind) %do% {
      transect <- st_read(x, y, quiet = TRUE) %>% 
        rename_all(recode, geom = "geometry") %>% 
        rename_all(.funs = tolower) %>%
        dplyr::select(id) %>% 
        mutate(id = as.character(id)) %>% 
        st_transform(3005)
    }
  } 
} #%>% dplyr::rename(ID = id)

transect_layout_buf <- st_buffer(transect_layout, 10)
  

# Look at the raw data files and find which are points and which are lines
all_layers <- st_layers(trans_data)
tracks_index <- which(all_layers$geomtype %in% c("Line String", "3D Line String"))
points_index <- which(all_layers$geomtype %in% c("Point", "3D Point"))

tracks <- list.files(trans_data, pattern = ".shp$", full.names = TRUE)[tracks_index]
points <- list.files(trans_data, pattern = ".gpkg$", full.names = TRUE)[points_index]


# consolidate points into single file and match to transect
points = points[1]

# split off the 2020 files and edit for description 

points_read <- st_read(points[1], quiet = TRUE) %>%
  st_transform(3005) %>% 
  st_zm() %in%
  filter(TimeStamp 

points_read_2020 <- points_read[str_detect(points_read$TimeStamp, "2020"),]

sort(unique(points_read_2020$name))

st_write(points_read_2020, file.path(output_pnts_dir, "essf_2020_temp.gpkg"), delete_layer = TRUE)





all_points <- foreach(x = points, .combine = rbind) %do% {
  # x <- points[1]
  points_read <- st_read(x, quiet = TRUE) %>%
    st_transform(3005) %>% 
    st_zm() %>% 
    # points_read <- points_read %>% filter(TimeStamp == "2019-08-06") 
    #drop_na(!!sym(mapunit1_column)) #%>%
    mutate(mapunit1 = X2MapUnit, 
           mapunit2 = X4MapUnit2,
           point_type = desc, 
           file = x) %>%
    st_join(., transect_layout_buf, join = st_intersects) %>%
    rename_all(.funs = tolower) %>%
    dplyr::select(name, mapunit1, mapunit2, point_type, id, file)
} %>% 
  distinct(., .keep_all = TRUE) 

#all_points <- points_read

# export non_standard points: 
all_points_extras <- all_points %>%
  filter(is.na(id))

st_write(all_points_extras, file.path(output_pnts_dir, "s1_extra_points_raw.gpkg"), delete_layer = TRUE)


# export standard transect
all_points <- all_points %>% filter(!is.na(id))

st_write(all_points, file.path(output_pnts_dir, "s1_transect_points_raw.gpkg"), delete_layer = TRUE)


        