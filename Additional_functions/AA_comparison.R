# Copyright 2020 Province of British Columbia
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
# http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.


# script to compare AA and QA transects for Deception, Aleza, and EagleHills 


#written by Gen Perkins
#edits by Ekaterina Daviel and Lizzy Hoffman (Eclipse Geomatics Ltd.)

# Import AA data, convert point data to vector, then raster. 
# Generate AA statistics for QA and AA data 
# Generate AA statistics comparing Model output
# Generate AA spatial Statistics (QA/AA and Model) - to be completed 

library(sf)
library(stringr)
library(tidyr)
library(fasterize)
library(raster)
library(purrr)
library(foreach)
library(dplyr)

#============================================================================
#Set up folder structure and define inputs
#============================================================================

#sync <- "C:sync_data/Sync/PEM_AA"

#AOI <- "Aleza"
#AOI <- "EagleHills"
AOI <- "Deception"

AOI_dir <- file.path(".", paste0(AOI,"_AOI"))
AA_dir <- file.path(AOI_dir, "2_sample_design","stage1_AA_2019")
cov_dir <- file.path(AOI_dir, "1_map_inputs", "covariates")

mapkey_dir <- file.path(AOI_dir, "_MapUnitLegend", paste0(AOI, "_MapUnitLegend.csv"))
output <- file.path(AOI_dir, "3_maps_analysis", "external_AA")

if_else(!dir.exists(output), dir.create(output), FALSE)

#Set up filenames and columns of interest for AOI
#Notes: 
  #transect_file refers to the transect layouts
  #external_file refers to external (contractor) data
  #qa_file refers to QA (internal) data
  #poc = "point of call", i.e. column indicating order of data point collection
  #column_prime = primary call
  #column_alt = secondary call
  #key_file = table relating BEC codes with number
#Issue: false transect results may be produced if transects in the transect layout intersect 

if(AOI == "Deception") {
  base_raster <- "aspect.tif"
  transect_file_name <- "AA_transect_layout_deception.gpkg"
  transect_file_layer <- "AA_transect_layout_deception_line"
  external_file_name <- "Deception_compiled_aa.gpkg"
  external_file_layer <- "points"
  poc_column_ex <- "Name"
  column_prime_ex <-  "X3MapUnit1" 
  column_alt_ex <-  "X5Mapunit2"
  qa_file_name <- "AA_QC_deception.gpkg"
  qa_layer_name <- "SBS"
  poc_column_qa <- "name"
  column_prime_qa <- "X2MapUnit"
  column_alt_qa <- "X4MapUnit2"
  key_file_name <- "Deception_MapUnitLegend.csv"
  model = FALSE
  model_file <- FALSE
}

if(AOI == "Aleza") {
  base_raster <- "models/99_PEMv1/SiteSeries_caret.tif"
  transect_file_name <- "AA_transect_layout_Aleza.gpkg"
  transect_file_layer <- "AA_transect_layout_Aleza"
  external_file_name <- "aleza_compiled_aa.gpkg"
  external_file_layer <- "points"
  poc_column_ex <- "Name"
  column_prime_ex <-  "X3MapUnit1" 
  column_alt_ex <-  "X5Mapunit2"
  qa_file_name <- "AA_QA_transects_Aleza.gpkg"
  qa_layer_name <- "AA_QA_transects_Aleza"
  poc_column_qa <- "desc"
  column_prime_qa <- "SiteSeries"
  column_alt_qa <- "Eco2"
  key_file_name <- "Aleza_MapUnitLegend.csv"
  model = TRUE
  model_file <- base_raster
}

if(AOI == "EagleHills") {
  base_raster <- "aspect.tif"
  transect_file_name <- "AA_transect_layout_Eaglehills.gpkg"
  transect_file_layer <- "AA_transect_layout_Eaglehills_lines"
  external_file_name <- "EagleHills_AA.gpkg"
  poc_column_ex <- "name"
  column_prime_ex <-  "x3mapunit1" 
  column_alt_ex <-  "x5mapunit2"
  qa_file_name <- "AA_QC_EagleHills.gpkg"
  poc_column_qa <- "name"
  column_prime_qa <- "x2mapunit1"
  column_alt_qa <- "x4mapunit2"
  key_file_name <- "EagleHills_MapUnitLegend.csv"
  model = FALSE
  model_file <- FALSE

  x <- file.path(AOI_dir, paste0(external_file_name))
  clhs_layers <- st_layers(x)
  points <- which(clhs_layers[["geomtype"]] %in% c("Point", "3D Point"))
  transects <- foreach(y = clhs_layers$name[points], .combine = rbind) %do% {
    transect <- st_read(x, y, quiet = TRUE) %>% 
      st_zm() %>%
      rename_all(recode, geom = "geom") %>% 
      rename_all(.funs = tolower) %>%
      #dplyr::select(id) %>% 
      #mutate(id = as.character(id)) %>% 
      st_transform(3005) %>%
      st_cast(., "POINT")}
  
# GP : error for eagle hills hre 
# Error in { : z error - expecting three coordinates
#    Backtrace:
#      x
#    1. \-`%do%`(...)
#    2.   \-e$fun(obj, substitute(ex), parent.frame(), e$data)
#    3.     \-base::tryCatch(...)
#    4.       \-base:::tryCatchList(expr, classes, parentenv, handlers)
#    5.         \-base:::tryCatchOne(expr, names, parentenv, handlers[[1L]])
#    6.           \-value[[3L]](cond)

# ED: I did not get your error on my machine, but I added the st_zm() above so hopefully that fixes the error
# LH: I didn't get this error either
  
  external_layer_name <- transects
  remove(transects, transect)
  
  x <- file.path(AOI_dir, paste0(qa_file_name))
  clhs_layers <- st_layers(x)
  points <- which(clhs_layers[["geomtype"]] %in% c("Point", "3D Point"))
  transects <- foreach(y = clhs_layers$name[points], .combine = rbind) %do% {
    transect <- st_read(x, y, quiet = TRUE) %>% 
      st_zm() %>%
      rename_all(recode, geom = "geom") %>% 
      rename_all(.funs = tolower) %>%
      #dplyr::select(id) %>% 
      #mutate(id = as.character(id)) %>% 
      st_transform(3005) %>%
      st_cast(., "POINT")
    }  
  
  qa_layer_name <- transects
  
}



#============================================================================
#Read in input data 
#============================================================================

# read in a base raster of resolution required (2.5) and reproject to CRS 3005 if required

srD <- "+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
srE <- "+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
sr0 <- "+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs "

rTemp <- raster(file.path(cov_dir, "2.5m", paste0(base_raster)))

#test projection
t <- as.character(projection(rTemp)[[1]])
pr <- if_else(t == srD | t == srE | t == sr0, TRUE, FALSE)
if(pr == "TRUE") {print("matching crs")}

#reproject if necessary
if(pr == "FALSE") {rTemp <- projectRaster(rTemp, crs = sr0)}

#read in model raster
if(model == TRUE){
  model_rast <- raster(file.path(AOI_dir, paste0(model_file))) 
}


#import planned transect layout 
transects_layout <- st_read(dsn = file.path(AA_dir, paste0(transect_file_name)), 
                          layer =  paste0(transect_file_layer)) %>%
  st_transform(3005) %>%
  st_buffer(dist = 10)


# import external transects

if(AOI == "EagleHills") {
  
  etrans <-  external_layer_name %>%
    st_zm() %>%
    rename(., Field_prime = paste0(column_prime_ex),
           Field_alt = column_alt_ex,
           name = paste0(poc_column_ex)) %>%
    mutate(Type = "External" ) %>%
    st_join(., transects_layout, join = st_intersects) %>%
    filter(!is.na(ID)) %>%
    filter(., !ID %in% c("11_NE")) # Manual removal of overlapping transects 
} 

#etrans_key <- etrans %>%
  #as.data.frame() %>%
  #distinct(., Field_prime)

if(AOI == "Aleza") {
etrans <-  st_read(dsn = file.path(AOI_dir, paste0(external_file_name)), 
                   layer =  paste0(external_file_layer)) %>%
  st_transform(3005) %>%
  rename(., Field_p1 = paste0(column_prime_ex),
         Field_a1 = column_alt_ex,
         name = paste0(poc_column_ex)) %>%
  mutate(Field_p2 = gsub("/", "_", Field_p1)) %>%
  mutate(Field_prime = gsub("O", "0", Field_p2)) %>%   
  mutate(Field_a2 = gsub("/", "_", Field_a1)) %>%
  mutate(Field_alt = gsub("O", "0", Field_a2)) %>%
  mutate(Type = "External" ) %>%
  st_join(., transects_layout, join = st_intersects) %>%
  filter(!is.na(ID)) %>%
  filter(., !ID %in% c("11_NW", "8_E", "8_NE", "4_NW", "8_SE")) # Manual removal of overlapping transects
}

if(AOI == "Deception") {
  etrans <-  st_read(dsn = file.path(AA_dir, paste0(external_file_name)), 
                     layer =  paste0(external_file_layer)) %>%
    st_transform(3005) %>%
    rename(., Field_prime = paste0(column_prime_ex),
           Field_alt = column_alt_ex,
           name = paste0(poc_column_ex)) %>%
    mutate(Type = "External" ) %>%
    st_join(., transects_layout, join = st_intersects) %>%
    filter(!is.na(ID))
  
 # unique(etrans$Field_prime)
 #  unique(etrans$Field_alt)  
  
  etrans <- etrans %>%
  mutate(Field_prime =  gsub("[[:space:]]", "", Field_prime),
        Field_prime =  gsub("SBC", "SBS", Field_prime),
         Field_prime = gsub("SBS/", "SBSmc2/", Field_prime),
         Field_prime = gsub("SBSMc", "SBSmc", Field_prime),
         Field_prime = gsub("Sc2", "Smc2", Field_prime),
         Field_prime = gsub("SBS1", "SBSmc2/1", Field_prime),
         Field_prime = gsub("SBS09", "SBSmc2/09", Field_prime),
         Field_prime = gsub("/1c", "/01c", Field_prime))
  
  etrans <- etrans %>%
    mutate(Field_alt = gsub("[[:space:]]", "", Field_alt),
           Field_alt = gsub("SNSmc", "SBSmc", Field_alt),
           Field_alt = gsub("SBSmc/", "SBSmc2/", Field_alt),
           Field_alt = gsub("SBSc2/", "SBSmc2/", Field_alt),
           Field_alt = gsub("SBS/", "SBSmc2/", Field_alt),
           Field_alt = gsub("SBC", "SBS", Field_alt),
           Field_alt = gsub("SBSm2/", "SBSmc2/", Field_alt))
  
unique(etrans$Field_alt)
  }

# prep QA transects

if(AOI == "EagleHills") {
  
  qatrans <-  qa_layer_name %>%
    st_zm() %>%
    rename(., Field_prime = paste0(column_prime_qa),
           Field_alt = column_alt_qa,
           name = paste0(poc_column_qa)) %>%
    mutate(Type = "QA" ) %>%
    st_join(., transects_layout, join = st_intersects) %>%
    filter(!is.na(ID)) 
}

if(AOI == "Deception") {
  
qatrans <-  st_read(dsn = file.path(AA_dir, paste0(qa_file_name)), 
                    layer = paste0(qa_layer_name)) %>%
  st_transform(3005)  %>%
  st_zm() %>%
  rename(., Field_prime = paste0(column_prime_qa),
         Field_alt = column_alt_qa,
         name = paste0(poc_column_qa)) %>%
  mutate(Type = "QA" ) %>%
  st_join(., transects_layout, join = st_intersects) %>%
  filter(!is.na(ID))
}

if(AOI == "Aleza") {
  
  qatrans <-  st_read(dsn = file.path(AOI_dir, paste0(qa_file_name)), 
                      layer = paste0(qa_layer_name)) %>%
    st_transform(3005)  %>%
    st_zm() %>%
    rename(., name1 = name) %>% ##Avoid use of original name as call
    rename(., Field_pr = paste0(column_prime_qa),
           Field_a = column_alt_qa,
           name = paste0(poc_column_qa)) %>%
    mutate(Field_prime = gsub("/", "_", Field_pr)) %>%
    mutate(Field_alt = gsub("/", "_", Field_a)) %>%
    mutate(Type = "QA" ) %>%
    st_join(., transects_layout, join = st_intersects) %>%
    filter(!is.na(ID))
}


# read in base data requried. 
map.key  <- read.csv(file.path(AOI_dir, "_MapUnitLegend", paste0(key_file_name)), header = TRUE)


#create table for dropped results due to field data tables missing matches on map.key

e_prime <- etrans %>%
  as.data.frame() %>%
  select(., Field_prime) %>%
  rename(., "FieldCall" = Field_prime)
e_alt <- etrans %>%
  as.data.frame() %>%
  select(., Field_alt) %>%
  rename(., "FieldCall" = Field_alt)
qa_prime <- qatrans %>%
  as.data.frame() %>%
  select(., Field_prime) %>%
  rename(., "FieldCall" = Field_prime)
qa_alt <- qatrans %>%
  as.data.frame() %>%
  select(., Field_alt) %>%
  rename(., "FieldCall" = Field_alt)
map_error <- rbind(e_prime, e_alt, qa_prime, qa_alt) %>%
  mutate(Fieldss = if_else(str_detect(FieldCall,"W"), 
                           gsub("(.*[[:alpha:]]).*","\\1", FieldCall),
                           gsub("(.*[[:digit:]]).*","\\1", FieldCall))) %>%
  left_join(., map.key, by = c("Fieldss" = "FieldCall"), all = T) %>%
  filter(., is.na(MapUnit)) %>%
  distinct(., Fieldss) %>%
  arrange(., Fieldss)
if_else(nrow(map_error) > 1, "map key error - see output table", "no map key errors") 


if(nrow(map_error) > 1) {write.csv(map_error, file.path(output, paste0(AOI, "map_key_missing_values.csv")))}
  


#============================================================================
#Define functions
#============================================================================

# Define function for tabular outputs : summarise raster as table 
rasterFreq <- function(rast, name){
  freq_df <- as.data.frame(freq(rast)) %>%
    group_by(value) %>% #added
    summarise(sum(count)) %>% #added
    rename(., count = "sum(count)") %>% #added
    filter(!is.na(value)) %>%
    left_join(., map.key, by = c("value" = "Field_ID"), all = T) %>% 
    dplyr::select(FieldCall, MapUnit, value, count) %>% 
    #select(-value) %>%
    mutate(prop = count / sum(count)) %>%
    group_by(MapUnit) %>% #added
    summarise(., sum(count), sum(prop)) %>%
    rename(., count = "sum(count)") %>%
    rename(., prop = "sum(prop)") %>%
    mutate(name = paste(name))
}


#Define function for production of raster and results summary (allt = prepped transect points, field_type = Field_prime/Field_alt)
rasterFunct <- function(allt, field_type) {

  allt <- etrans
  field_type =  "Field_prime"
  
  rastAll <<- foreach(trs = unique(allt$ID), .combine = function(x,y){raster::merge(x,y)}) %do% {
  
    trs <- allt$ID[1]
   t1 <- allt %>%
    filter(allt$ID == trs) %>%
    mutate(Order = gsub("\\D+","", name)) %>%
    distinct(.,.keep_all = T) %>%
    mutate(Order = as.numeric(Order)) %>%
    arrange(Order)

   
  calls <- t1 %>%
    dplyr::select(c(name, Order, Field_prime, Field_alt, ID)) %>%
    st_drop_geometry()
  
  lines <- cbind(t1, st_coordinates(t1)) %>% as_tibble() %>% ###connect points
    mutate(Xend = lead(X),
           Yend = lead(Y)) %>%  # collect the coordinates of the next point
    filter(!is.na(Yend)) %>%  # drops the last row (start point with no end)
    unite(Start, X, Y, sep = " ") %>%
    unite(End, Xend, Yend, sep = " ") %>%
    gather(key = "Start_End", value = coords, Start, End) %>% # converts to long table format
    dplyr::select(-geom) %>%  # old point geometry is dropped
    separate(coords, into = c("X", "Y"), sep = " ") %>%       # coordinate pairs are seperated
    st_as_sf(coords = c("X", "Y"))  %>% # geometry is created for each point (start and end)
    group_by(name) %>% 
    dplyr::summarise() %>% 
    st_cast("LINESTRING") %>%
    st_set_crs(st_crs(t1)) %>%
    mutate(length = st_length(.))
  
  
  lines <- merge(lines, calls, by = "name", all.x = TRUE)

  #st_write(lines, dsn = "TestTransect",layer = "lines", driver = "ESRI Shapefile")
 

# ifelse(field_type == "Field_prime",  
  lBuff <- lines %>%
    group_by(Field_prime) %>%
    summarise() %>%
    st_buffer(., dist = 2.5, endCapStyle = "FLAT", joinStyle = "MITRE") %>%
    st_cast(.,"MULTIPOLYGON") %>%
    mutate(ID = trs) %>%
    mutate(Fieldss = if_else(str_detect(Field_prime,"W"), 
                            gsub("(.*[[:alpha:]]).*","\\1", Field_prime),
                            gsub("(.*[[:digit:]]).*","\\1", Field_prime))) %>%
    mutate(FieldCall = as.factor(Field_prime)) %>%
    left_join(., map.key, by = c("Fieldss" = "FieldCall"), all = T),
    #select(., -Fieldss, -FieldCall) %>%
    #group_by(MapUnit) %>%
    #summarise(., mean(Map_ID)) %>%
    #mutate_all(~replace(., is.na(.), 1000)) %>%
    #mutate(x = replace(as.character(MapUnit), (is.na(MapUnit)), "Unk")) %>%
    #select(., -MapUnit) %>%
    #rename(., MapUnit =  x) %>%
    #rename(., Map_ID =  "mean(Map_ID)") %>% 
  
  lBuff <- lines %>%
    group_by(Field_alt) %>%
    summarise() %>%
    st_buffer(., dist = 2.5, endCapStyle = "FLAT", joinStyle = "MITRE") %>%
    st_cast(.,"MULTIPOLYGON") %>%
    mutate(ID = trs) %>%
    mutate(Fieldss = if_else(str_detect(Field_alt,"W"), 
                             gsub("(.*[[:alpha:]]).*","\\1", Field_alt),
                             gsub("(.*[[:digit:]]).*","\\1", Field_alt))) %>%
    mutate(FieldCall = as.factor(Field_alt)) %>%
    left_join(., map.key, by = c("Fieldss" = "FieldCall"), all = T))
    #group_by(MapUnit) %>%
    #summarise(., mean(Map_ID)) %>%
    #mutate_all(~replace(., is.na(.), 1000)) %>%
    #mutate(x = replace(as.character(MapUnit), (is.na(MapUnit)), "Unk")) %>%
    #select(., -MapUnit) %>%
    #rename(., Map_ID =  x) %>%
    #rename(., Map_ID =  "mean(Map_ID)") %>%
    
  
  #st_write(lBuff, paste0("out/lBuff_", trs, ".shp"))
  
 
 # GP: This course an error with Aleza "i..Field_ID?
   
#create output raster 
  rast <<- fasterize(lBuff, rTemp, field = "Field_ID") ###rasterize. Translation of the field call to MapUnit in next script
  rast <<- crop(rast, lBuff)

#create output table
  sub_rast <- rasterFreq(rast, paste0(trs)) %>%
    as.data.frame()   
  
  result_table <<- rbind(result_table, sub_rast)
  
  rast <<- crop(rast, lBuff) #ensure rasterAll remains a raster
    
}}


#Define function for production of merged buffer (allt = prepped transect points, rast = model raster)
bufferFunct <- function(allt, rast) {
  buffAll <<- foreach(trs = unique(allt$ID), .combine = function(x, y){rbind(x, y)}) %do% {
    #trs <- allt$ID[1]
    t1 <- allt %>%
      dplyr::filter(allt$ID == trs) %>%
      mutate(Order = gsub("\\D+","", name)) %>%
      distinct(.,.keep_all = T) %>%
      mutate(Order = as.numeric(Order)) %>%
      arrange(Order)
    
    calls <- t1 %>%
      dplyr::select(c(name, Order, Field_prime, Field_alt, ID)) %>%
      st_drop_geometry()
    
    lines <- cbind(t1, st_coordinates(t1)) %>% as_tibble() %>% ###connect points
      mutate(Xend = lead(X),
             Yend = lead(Y)) %>%  # collect the coordinates of the next point
      filter(!is.na(Yend)) %>%  # drops the last row (start point with no end)
      unite(Start, X, Y, sep = " ") %>%
      unite(End, Xend, Yend, sep = " ") %>%
      gather(key = "Start_End", value = coords, Start, End) %>% # converts to long table format
      dplyr::select(-geom) %>%  # old point geometry is dropped
      separate(coords, into = c("X", "Y"), sep = " ") %>%       # coordinate pairs are seperated
      st_as_sf(coords = c("X", "Y"))  %>% # geometry is created for each point (start and end)
      group_by(name) %>% 
      dplyr::summarise() %>% 
      st_cast("LINESTRING") %>%
      st_set_crs(st_crs(t1)) %>%
      mutate(length = st_length(.))
    
    lines <- merge(lines, calls, by = "name", all.x = TRUE)
    
    #st_write(lines, dsn = "TestTransect",layer = "lines", driver = "ESRI Shapefile")
    
    
    lBuff <- lines %>%
      st_buffer(., dist = 2.5, endCapStyle = "FLAT", joinStyle = "MITRE") %>%
      st_cast(.,"MULTIPOLYGON")
    
    #st_write(lBuff, paste0("out/lBuff_", trs, ".shp"))
   
  rast <- model_rast  
    
   rast_crop <<- crop(rast, lBuff) %>%
     mask(., lBuff)
    
    sub_rast <- rasterFreq(rast_crop, paste0(trs)) %>%
      as.data.frame()   
    
    result_table <<- rbind(result_table, sub_rast)
    
    lBuff <<- lBuff #ensure buffAll remains a buffer
    
    
  }}




#============================================================================
#Run functions for production of rasters and results table input
#============================================================================

# GP: error in Aleza - see above 

#create summary table
result_table <- data.frame(MapUnit=character(0), count=integer(0), name=character(0), prop=numeric(0))

##Run rasterFunct on QA input datasets 
rasterFunct(qatrans, "Field_prime")                 
rastAll <- merge(rastAll, rast)
rastAll_qa_prime <- rastAll
results_qa_prime <- result_table

#Reset result table
result_table <- data.frame(MapUnit=character(0), count=integer(0), name=character(0), prop=numeric(0))

rasterFunct(qatrans, "Field_alt")
rastAll <- merge(rastAll, rast)
rastAll_qa_alt <- rastAll
results_qa_alt <- result_table

#Reset result table
result_table <- data.frame(FieldCall=character(0), count=integer(0), name=character(0), prop=numeric(0))

##Run rasterFunct on external input datasets 
rasterFunct(etrans, "Field_prime")
rastAll <- merge(rastAll, rast)
rastAll_e_prime <- rastAll
results_e_prime <- result_table

#Reset result table
result_table <- data.frame(FieldCall=character(0), value=integer(0), count=integer(0), name=character(0), prop=numeric(0))

rasterFunct(etrans, "Field_alt")
rastAll <- merge(rastAll, rast)
rastAll_e_alt <- rastAll
results_e_alt <- result_table

##Write rasters
#qa transects: 
writeRaster(rastAll_qa_prime, file.path(output, "AA_QA_trans_prime.tif"), format = "GTiff", overwrite = TRUE) ### save forward to analysis script folders.  Field Units will be converted to Map Units in the analysis scripts
writeRaster(rastAll_qa_alt, file.path(output, "AA_QA_trans_alt.tif"), format = "GTiff", overwrite = TRUE) ### save forward to analysis script folders.  Field Units will be converted to Map Units in the analysis scripts
#
#external transects 
writeRaster(rastAll_e_prime, file.path(output, "AA_Ext_trans_prime.tif"), format = "GTiff", overwrite = TRUE) ### save forward to analysis script folders.  Field Units will be converted to Map Units in the analysis scripts
writeRaster(rastAll_e_alt, file.path(output, "AA_Ext_trans_alt.tif"), format = "GTiff", overwrite = TRUE) ### save forward to analysis script folders.  Field Units will be converted to Map Units in the analysis scripts


#writeRaster(rastAll, file.path(AOI_dir, "out", "AA_trans.tif"), format = "GTiff", overwrite = TRUE) ### save forward to analysis script folders.  Field Units will be converted to Map Units in the analysis scripts

#============================================================================
#Format results tables
#============================================================================

#calculate QA total count
count_transect_qa <- results_qa_prime %>%
  group_by(name) %>%
  summarize(sum(count)) %>%
  rename("QA_COUNT" = "sum(count)") %>%
  mutate(QA_AREA_m2 = QA_COUNT*res(rTemp)^2)

#summarize QA results
summary_table_qa <- results_qa_prime %>%  
  full_join(., results_qa_alt, by = c("MapUnit" = "MapUnit", "name" = "name"), all = T, suffix = c(".p", ".a")) %>%
  full_join(., count_transect_qa, by = c("name" = "name")) %>%
  mutate(qa = ((count.a / QA_COUNT)*100)) %>%
  mutate(QA_ASS = round(qa, 3)) %>%
  mutate(qa_per = (prop.p)*100) %>%
  mutate(QA_PERCENT = round(qa_per, 3)) %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  dplyr::select(name, MapUnit, QA_AREA_m2, QA_PERCENT, QA_ASS)

#calculate external total count
count_transect_ext <- results_e_prime %>%
  group_by(name) %>%
  summarize(sum(count)) %>%
  rename("EXT_COUNT" = "sum(count)") %>%
  mutate(EXT_AREA_m2 = EXT_COUNT*res(rTemp)^2)

#Summarize external results
summary_table_ext <- results_e_prime %>%
  full_join(., results_e_alt, by = c("MapUnit" = "MapUnit", "name" = "name"), all = T, suffix = c(".p", ".a")) %>%
  full_join(., count_transect_ext, by = c("name" = "name")) %>%
  mutate(ext_a = ((count.a / EXT_COUNT)*100)) %>%
  mutate(EXT_ASS = round(ext_a, 3)) %>%
  mutate(ext_per = (prop.p)*100) %>%
  mutate(EXT_PERCENT = round(ext_per, 3)) %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  dplyr::select(name, MapUnit, EXT_AREA_m2, EXT_PERCENT, EXT_ASS)



##Combine and summarize QA and external results
summary_table_all <- summary_table_ext %>%
  full_join(., summary_table_qa, by = c("MapUnit" = "MapUnit", "name" = "name"), all = T) %>%
  dplyr::select(., -EXT_AREA_m2, -QA_AREA_m2) %>%
  full_join(., count_transect_ext, by = c("name" = "name")) %>%
  full_join(., count_transect_qa, by = c("name" = "name")) %>%
  dplyr::select(., name, MapUnit, EXT_AREA_m2, EXT_PERCENT, EXT_ASS, QA_AREA_m2, QA_PERCENT, QA_ASS) %>%
  arrange(., name, MapUnit) %>%
  rename(., TRANSECT = name) %>%
  rename(., "AM-BGC_SS" = MapUnit) %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  mutate(PRIME_OVERLAP = pmin(EXT_PERCENT, QA_PERCENT, na.rm = FALSE)) %>%
  #Alt overlap where external alt overlap compared with QA
  mutate(EXT_ALT_OVERLAP = if_else(EXT_ASS == 0, PRIME_OVERLAP,
                                   if_else((EXT_ASS + EXT_PERCENT) <= QA_PERCENT, (EXT_ASS + EXT_PERCENT), QA_PERCENT))) %>% 
  #Alt overlap where QA alt overlap compared with external
  mutate(QA_ALT_OVERLAP = if_else(QA_ASS == 0, PRIME_OVERLAP,
                                  if_else((QA_ASS + QA_PERCENT) <= EXT_PERCENT, (QA_ASS + QA_PERCENT), EXT_PERCENT)))
  


##Calculate overlap scores
PO <- summary_table_all %>%
  group_by(TRANSECT) %>%
  summarize(sum(PRIME_OVERLAP)) %>%
  rename(., PRIME_OVERLAP_SCORE = "sum(PRIME_OVERLAP)")

AO1 <- summary_table_all %>%
  group_by(TRANSECT) %>%
  summarize(sum(EXT_ALT_OVERLAP)) %>%
  rename(., EXT_ALT_OVERLAP_SCORE = "sum(EXT_ALT_OVERLAP)")

AO2 <- summary_table_all %>%
  group_by(TRANSECT) %>%
  summarize(sum(QA_ALT_OVERLAP)) %>%
  rename(., QA_ALT_OVERLAP_SCORE = "sum(QA_ALT_OVERLAP)")

EXT_QA_result_score <- PO %>%
  right_join(., AO1, by = c("TRANSECT" = "TRANSECT")) %>%
  right_join(., AO2, by = c("TRANSECT" = "TRANSECT"))


#Export results

write.csv(summary_table_all, file.path(output, paste0(AOI, "_EXT_QA_results_summary_table.csv")))
write.csv(EXT_QA_result_score, file.path(output, paste0(AOI, "_EXT_QA_overlap_scores.csv")))
 
#============================================================================
#Import model results and prepare results tables
#============================================================================

if(model == TRUE) {
 
  #Run bufferFunct with model raster 
  result_table <- data.frame(MapUnit=character(0), count=integer(0), name=character(0), prop=numeric(0))
  crs_model <- crs(model_rast, asText = FALSE)
  etrans2 <- st_transform(etrans, crs_model)
  merged_buffer <- bufferFunct(etrans2, model_rast) %>%
    st_cast(.,"MULTIPOLYGON") 
  
  # GP: Error with Aleza 
  #Error in { : 
  #    task 1 failed - "`by` can't contain join column `Field_ID` which is missing from RHS"
  #  Backtrace:
  #    x
  #  1. +-[ bufferFunct(qatrans2, model_rast) %>% st_cast(., "MULTIPOLYGON") ] with 2 more calls
  #  4. \-global::bufferFunct(qatrans2, model_rast)
  #  5.   \-`%do%`(...)
  #  6.     \-e$fun(obj, substitute(ex), parent.frame(), e$data)
  #  > 
  
  
  #Create model raster cropped to transect extent
  model_subset <- crop(model_rast, merged_buffer)  %>% #Model raster subset for transects
   mask(., merged_buffer)

  #Export raster results
  writeRaster(model_subset, file.path(output, "Model_trans.tif"), format = "GTiff", overwrite = TRUE) 
  
  #Create model results output table
  model_results <- result_table
  
  #Summarize model results
  model_result_summary <- model_results %>%
    mutate(model_per = prop*100) %>%
    mutate(MODEL_PERCENT = round(model_per, 3)) %>%
    full_join(., summary_table_ext, by = c("MapUnit" = "MapUnit", "name" = "name"), all = T) %>%
    full_join(., count_transect_ext, by = c("name" = "name")) %>%
    select(name, MapUnit, EXT_AREA_m2.y, MODEL_PERCENT, EXT_PERCENT, EXT_ASS) %>%
    arrange(., name, MapUnit) %>%
    rename(., TRANSECT = name, 
           "AM-BGC_SS" = MapUnit,
           TRANSECT_AREA_m2 = EXT_AREA_m2.y) %>%
    mutate_all(~replace(., is.na(.), 0)) %>%
    mutate(PRIME_OVERLAP = pmin(MODEL_PERCENT, EXT_PERCENT, na.rm = FALSE)) %>%
    mutate(ALT_OVERLAP = if_else(EXT_ASS == 0, PRIME_OVERLAP,
                                 if_else((EXT_ASS + EXT_PERCENT) <= MODEL_PERCENT, (EXT_ASS + EXT_PERCENT), MODEL_PERCENT)))
 #Calculate overlap scores
  PO <- model_result_summary %>%
    group_by(TRANSECT) %>%
    summarize(sum(PRIME_OVERLAP)) %>%
    rename(., PRIME_OVERLAP_SCORE = "sum(PRIME_OVERLAP)")
    
  AO <- model_result_summary %>%
    group_by(TRANSECT) %>%
    summarize(sum(ALT_OVERLAP)) %>%
    rename(., ALT_OVERLAP_SCORE = "sum(ALT_OVERLAP)")
  
  model_result_score <- full_join(PO, AO, by = c("TRANSECT" = "TRANSECT"))
  
  #Export results
  
  write.csv(model_result_summary, file.path(output, paste0(AOI, "_model_results_summary_table.csv")))
  write.csv(model_result_score, file.path(output, paste0(AOI, "_model_overlap_scores.csv")))
  
}





