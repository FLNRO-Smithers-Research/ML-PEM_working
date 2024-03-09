# script to generate stage 2 samples : stop gap to incorporate toms script

# written July 16th - stop gap to get deception sample points completed 
# runs ensemble model and generates outputs for stage 2 sampling 


#library(corrplot)
library(caret)
library(sf)
library(ranger)
library(tidyverse)
library(fasterize)
library(stringr)
library(dplyr)
library(raster)
library(terra)
library(readxl)
library(stars)
library(stringr)
#library(plyr)

#AOI <- "Deception"

AOI <- "BoundaryTSA"

# set up file structure
AOI_dir <- file.path(".", paste0(AOI,"_AOI"))
cov_dir <- file.path(AOI_dir, "1_map_inputs", "covariates")
shapes_dir <- file.path(AOI_dir, "0_raw_inputs", "base_layers")
input_pnts_dir <- file.path(AOI_dir, "1_map_inputs", "trainingData")
out_dir <- file.path(AOI_dir, "3_maps_analysis","models")

# read in map and model keys
map.key  <- read.csv(file.path(AOI_dir, "_MapUnitLegend", 
                               paste0(AOI, "_MapUnitLegend.csv")), 
                     stringsAsFactor = FALSE)

model_param <- file.path(AOI_dir, "_MapUnitLegend", "models.xlsx")

# read in temp functions
source(here::here('_functions', 'model_gen.R'))
source(here::here('_functions', 'point_subsample.R'))
#source(here::here('_functions', 'predict_landscape.R'))
source(here::here('_functions', 'predict_map.R'))
#source(here::here('_functions', 'model_gen_mlr3.R'))

# set up model parameters:  

# set up model parameters:  
mparam <- read_xlsx(model_param, "models") %>% filter(to_run == 1)
map_res <- mparam$resolution
mname <- paste0(mparam$model_name)
mrep <- mparam$model_rep

# check which catergory of model to be produced
mtype <- case_when(
  str_detect(mname, "for_nf")  ~ "forest_non_forest",
  str_detect(mname, "nf_") ~ "non_forest",
  str_detect(mname, "fore") ~ "forest"
)

# get covariates
mcov <-read_xlsx(model_param, "covariates", skip = 2) %>%
  filter(!!sym(mparam$covariates) == 1) %>%
  dplyr::select(covariate)

# get training point sets
mtpt <- read_xlsx(model_param, "training_pts", skip = 2) %>%
  filter(!!sym(mparam$training_pts) == 1) %>%
  dplyr::select(tp_code)%>%
  pull

# get the map unit level 
mmu <- read_xlsx(model_param, "map_unit", skip = 2) %>%
  filter(!!sym(mparam$map_unit) == 1) %>%
  dplyr::select(legend_column)%>%
  pull

mmu <- case_when(
  mmu == "column_mu" ~ "MapUnit", 
  mmu == "column_ss" ~ "SiteSeries",
  mmu == "column_ass" ~ "Association",
  mmu == "column_cls" ~ "Class",
  mmu == "column_grp" ~ "Group",
  mmu == "column_typ" ~ "Type"
)


# set up outfolder: 

if(!dir.exists(file.path(out_dir, mtype))){dir.create(file.path(out_dir, mtype))} 

out_dir <- file.path(out_dir, mtype, mname, mrep) 

# set up model folder: 
if(!dir.exists(out_dir)){
  dir.create(out_dir)} else { 
    print ("folder already exists for this model name - are you sure you want to overwrite the results?")}


##Create raster stack of spatial layers

res_folder <- paste0(map_res, "m")
rast_list <- list.files(file.path(cov_dir, res_folder), pattern = ".tif$", full.names = TRUE)
rast_list <- rast_list[tolower(gsub(".tif", "", basename(rast_list))) %in% tolower(mcov$covariate)]

bec_shp <- st_read(file.path(shapes_dir, "bec.gpkg"), quiet = TRUE)
#bec_shp <- st_read(file.path(shapes_dir, "bec_edited.gpkg"), quiet = TRUE)



# read in sets of data 

indata <- list.files(input_pnts_dir, paste0(mtpt,"_att.*.gpkg$"), full.names = TRUE)
tpts <- st_read(indata, quiet = TRUE) 
infiles <- basename(indata) 


# match to the key and filter for forest and non_forest points

subzones <- unique(bec_shp$MAP_LABEL)

#xx <- tpts

tpts <- tpts %>%
  dplyr::select(-c(x,y)) 

tpts <- tpts %>%
  cbind(st_coordinates(.)) %>%
  mutate(fnf = ifelse(grepl(paste0(subzones, collapse = "|"), mapunit1), "forest", "non_forest")) %>%
  st_join(st_transform(bec_shp[, "MAP_LABEL"], st_crs(.)), join = st_nearest_feature) %>%
  st_drop_geometry() %>% 
  dplyr::select(fnf, everything()) %>% 
  dplyr::rename(bgc_cat = MAP_LABEL) %>% 
  rename_all(.funs = tolower) %>% 
  droplevels()

# filter for forest or non_forest points as required
#if(mtype %in% c("forest", "non_forest")) {
#   tpts <- tpts %>% filter(fnf == mtype)
#} 


# tpts <- tpts %>%
#   cbind(st_coordinates(.)) %>%
#   mutate(fnf = ifelse(grepl(paste0(subzones, collapse = "|"), mapunit1), "forest", "non_forest")) %>%
#   st_join(st_transform(bec_shp[, "MAP_LABEL"], st_crs(.)), join = st_nearest_feature) %>% 
#   st_drop_geometry() %>% 
#   dplyr::select(fnf, everything()) %>% 
#   dplyr::rename(bgc_cat = MAP_LABEL) %>% 
#   rename_all(.funs = tolower) %>% 
#   droplevels()

# filter for forest or non_forest points as required

#tpts <- tpts %>% filter(fnf == mtype)


# grab the column of interst from map_key
mcols <- gsub(".tif","", tolower(basename(rast_list)))

# select the target column using the mapkey if needed: 

map.key.sub <- map.key %>%
  dplyr::select(!!sym(mmu))

tpts <- tpts %>% mutate(target = as.factor(mapunit1))

# filter columns of interst and remove mapunits with <20 points

mpts <- tpts %>%
  dplyr::select(target, all_of(mcols)) %>%
  droplevels()

MU_count <- mpts  %>% dplyr::count(target) %>% filter(n > 20) 

mpts <- mpts %>% filter(target %in% MU_count$target)  %>%
  drop_na() 



# if the name contains BGC then run models per BGC else run all 

zones <- c(as.character(subzones) )

bgc_pts_subzone <- lapply(zones, function (i){
  #i <- zones[13]
  
  pts_subzone <- mpts %>%
    filter(str_detect(target, as.character(paste0(i, "_")))) %>%
    drop_na() %>%
    droplevels()
  
  if(nrow(pts_subzone) == 0){ pts_subzone = NULL} else {ppts_subzone = pts_subzone }
  
  pts_subzone
  
})

# generate a name for list objects removing NULL values
names <- unlist(lapply(bgc_pts_subzone, is.null))
zone_names <- zones[-(which(ll <- names)) ] 

# remove null or missing subzones data sets 
bgc_pts_subzone =  bgc_pts_subzone[-which(sapply(bgc_pts_subzone, is.null))]
names(bgc_pts_subzone) <- zone_names

# filter classes with less than 20 samples   
MU_count <- lapply(bgc_pts_subzone, function(x) {
  x %>% dplyr::count(target) %>% filter(n > 20)})
#  
bgc_pts_subzone <- lapply(names(bgc_pts_subzone), function(x) {
  bgc_pts_subzone[[x]] %>% 
    dplyr::filter(target %in% MU_count[[x]]$target) 
}) 
names(bgc_pts_subzone) <- zone_names


# run model for SBSmc2

xx <- names(bgc_pts_subzone[4])

inmdata = bgc_pts_subzone[[xx]]
out_name = names(bgc_pts_subzone[xx])

descrCor <- cor(inmdata[,-1])
highlyCorDescr <- findCorrelation(descrCor, cutoff = 0.75, verbose = FALSE, names = TRUE)
col.to.rm <- c(highlyCorDescr)

inmdata = bgc_pts_subzone[[xx]] %>% 
  dplyr::select(names(bgc_pts_subzone[[xx]])[!names(bgc_pts_subzone[[xx]]) %in% col.to.rm])

outDir = file.path(paste(out_dir, out_name, sep = "/"))


#model_gen(inmdata, "target", 
#          outDir = outDir,  
#          indata = indata, 
#          mmu = mmu,
#          mname = mname,
#          rseed = 456)

#model_gen_esb(inmdata, "target", 
#          outDir = outDir,  
#          indata = indata, 
#          mmu = mmu,
#          mname = mname,
#0          rseed = 456)

# install.packages("mlr", dependencies = TRUE)

library(mlr)
library(tidyverse)
install.packages("glmnet")
install.packages("xgboost")
install.packages("deepnet")
deepnet
## Load the data

modDat <- inmdata 
target <- "target"
outDir = outDir
mmu = mmu
mname = mname
infiles = infiles
rseed = 456

`%notin%` <- Negate(`%in%`)
summary(as.factor(modDat$target))
modDat$target <- droplevels(modDat$target)

table(modDat$target)

## Create task
tsk <- makeClassifTask(data = modDat, target = target)

## Define Learner -- here we create an ensemble learner
esbLrns <- c("classif.ranger", "classif.glmnet", "classif.xgboost", "classif.nnTrain")

## Creates a list of learners using producing probability
lrns <- list(mlr::makeLearner(esbLrns[1]),
             mlr::makeLearner(esbLrns[2]),
             mlr::makeLearner(esbLrns[3], verbose=1),
             mlr::makeLearner(esbLrns[4]))
lrns <- lapply(lrns, setPredictType, "prob") ## use prob as the type 


## converts the list of learners to a ensemble 
lrn <- mlr::makeStackedLearner(base.learners = lrns, predict.type = "prob", 
                               method = "stack.cv", 
                               super.learner = "classif.glmnet")


parallelMap::parallelStartSocket(parallel::detectCores()-1)
mod <- train(lrn, tsk)
parallelMap::parallelStop()

#saveRDS(mod, paste(outDir, "model_ens.rds", sep = "/"))
saveRDS(mod,"D:/PEM_DATA/BEC_DevExchange_Work/Deception_AOI/3_maps_analysis/models/forest/fore_mu_bgc_S2/SBSmc2/model_ens.rds")

saveRDS(mod,"D:/PEM_DATA/BEC_DevExchange_Work/BoundaryTSA_AOI/3_maps_analysis/models/forest/fore_mu_bgc/3/IDFdm1/model_ens.rds")
#saveRDS(mod,"model_ens.rds")

saveRDS(mod,(file.path(out_dir,xx,"model_ens.rds")))


# testing
#
#model = "D:/PEM_DATA/BEC_DevExchange_Work/Deception_AOI/3_maps_analysis/models/forest/fore_mu_bgc_S2/SBSmc2/model_ens.rds"
model = file.path(out_dir,xx,"model_ens.rds")
mod <- readRDS(model)

#cov = paste0("D:/Hengle_Boundary_PEM/stacked5m_dec/",  mod$features, ".tif")
cov = paste0(file.path(cov.dir, paste0(map_res,"m"),"/"), mod$features, ".tif")

tilesize = 500
#outDir = "D:/PEM_DATA/BEC_DevExchange_Work/Deception_AOI/3_maps_analysis/models/forest/fore_mu_bgc_S2/SBSmc2/predicted"
outDir = file.path(out_dir,xx,"predicted")

## libraries  -----
library(dplyr)
library(mlr)
library(raster)
#library(sf)

# Adjust names
## This will be used in the loop to rename stars object
n <- basename(cov)
n <- gsub(".tif", "", n)

# ## Error handle -- model vs. cov -----------
# if (length(setdiff(mod$features, n)) != 0) { ## tests if all model features are found in the cov list
#   ## On model vs. cov error ---------------
#   print("Name mis-match between the model features and the names of the rasters.")
#   print("The following raster co-variates are not found in the model features list:")
#   print(setdiff(mod$features, n))
# } else {
#   
#   ## drop rasters if not included in model
#   if (length(setdiff(n, mod$features) > 0 )) {
#     print("The following rasters are removed as they were not included in the model:")
#     print(setdiff(n, mod$features))
#     
#     ## drop layers not used
#     drop_layers <- paste0(setdiff(n, mod$features), ".tif")
#     cov <- subset(cov, !(basename(cov) %in% drop_layers) )
#     n <- basename(cov) ; n <- gsub(".tif", "", n)  ## fix n
#   }
#   
#   
#   ## create output dir -----------
#   ifelse(!dir.exists(outDir), dir.create(outDir, recursive = TRUE), 
#          print("model folder already exists"))
#   
source(here::here('_functions', 'tile_index.R'))

tiles <- tile_index(cov[1], tilesize)

## begin loop through tiles -----

for (i in 3:nrow(tiles)) {    ## testing first 2 tiles       ##nrow(tiles)) {
  #i = 2
  t <- tiles[i,]  ## get tile
  print(paste("working on ", i, "of", nrow(tiles)))
  print("...")
  
  ## * load tile area---------
  print("... loading new data (from rasters)...")
  
  # extract raster for the area of the tile
  t <- as(t, 'Spatial')
  r <- raster::crop(stack(cov), raster::extent(t))
  
  # create a raster template in stars to convert back to stars object 
  rtemplate <- stars::st_as_stars(r[[1]])
  
  # grab point values for the raster stack
  rsf <- rasterToPoints(r)
  
  rsf.xy <- rsf[,c(1,2)] # keep xy to convert back to raster below
  rsf.sf <- st_as_sf(as.data.frame(rsf.xy), coords = c("x","y"), crs = 3005)
  rsf <- rsf[,-c(1:2)] # drop xy values
  colnames(rsf) = tolower(colnames(rsf))
  rsf.df <- as.data.frame(rsf) 
  rsf.df
  
  ## * Test if tile is empty  -------
  if(any(sapply(rsf.df, function(x) all(is.na(x))) == TRUE)){
    
    print("some variables with all NA values, skipping tile")
    
  } else {
    
    ## * predict ---------
    ## When some of the values are NA change them to zero
    rsf_bk <- rsf  ## create a backup of rsf -- this will be used to restore NA values
    rsf.df[is.na(rsf.df)] <- 0 ## convert NA to zero as the predict function cannot handle NA
    
    print("... modelling outcomes (predicting)...")
    pred <- predict(mod, newdata = rsf.df)
    
    # pp <- pred
    
    # extract the predictions from each of the models 
    
    out.c <- as.matrix(as.data.frame(mlr::getStackedBaseLearnerPredictions(mod, newdata=rsf.df)))
    
    #generate a summary layer
    
    # generate a layer with porportion of models that agreed.
    library(data.table)
    dat <- as.data.table(out.c)
    dat[,ID := seq(nrow(dat ))]
    dat <- data.table::melt(dat, id.vars = "ID")
    setnames(dat, c("ID","Model","Response"))
    dat2 <- dat[,.(Num = .N), by = .(ID, Response)]
    dat2 <- dat2[,.(MaxNum = max(Num)), by = ID]
    dat2[,PropSame := MaxNum/4]
    #dat3 <- dat[,.(NumMods = length(unique(Response))), by = ID]
    
    library(dplyr)
    # proportion of model agreement
    out.proportion <- cbind(rsf.sf, data.frame(dat2)) %>%
      dplyr::select(-c(ID, MaxNum))
    
    # generate a layer per model results 
    # convert to numeric first 
    out.c.df <- as.data.frame(out.c) %>% mutate_if(is.character,as.factor)
    out.c.df[,1] <- as.numeric(out.c.df[,1])
    out.c.df[,2] <- as.numeric(out.c.df[,2])
    out.c.df[,3] <- as.numeric(out.c.df[,3])
    out.c.df[,4] <- as.numeric(out.c.df[,4])
    
    out.c.df  <- cbind(rsf.sf, out.c.df)
    out_dat <- out.c.df 
    
    
    ## Restore NA values
    pred_dat <- pred$data ## predicted values extracted then changed
    pred_dat[is.na(rsf_bk[,1]), 1:length(pred_dat)] <- NA ## if originally NA restore NA
    pred$data <- pred_dat ## values restored to pred -- allows for cbind without issue.
    
    ## * geo-link predicted values ---------
    
    r_out <- cbind(rsf.sf, pred$data)
    
    ## layers to keep (i.e. newly predicted layers)
    keep <- names(pred_dat)
    #r_out <- r_out %>% dplyr::select(keep)
    
    ## Save the names of the model response -----
    ## The levels are in the multiclass 'response'
    wkey <- 0
    if (wkey == 0)  {
      respNames <- levels(r_out$response) ## this becomes the dictionary to describe the raster values
      write.csv(respNames, paste(outDir, "response_names.csv",
                                 sep = "/"),
                row.names = TRUE)
      wkey <- 1 ## Change this value so this small is statement does not execute again.
    }
    
    ## change the text values to numeric values.
    #       r_out$response <- as.numeric(r_out$response)
    
    ## Set up subdirectories for rastertile outputs
    # print("... exporting raster tiles...")
     for (k in keep) {
       dir.create(paste(outDir, k, sep = "/"))
     }
    # 
    # 
    # ## * save tile (each pred item saved) ---------
    for (j in 1:length(keep)) {
      #j <- 1  ## testing
      out <- stars::st_rasterize(r_out[j],
                                 template = rtemplate)
      st_crs(out) = "+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
      
      #out + template
      stars::write_stars(out,
                         paste0(outDir,"/",
                                keep[j], "/",             #sub-directoy
                                keep[j], "_", i, ".tif")) #tile name
    }
    
    
    # output the submodel components: 
    
    library(sf)
    out.proportion<- stars::st_rasterize(out.proportion,
                                         template = rtemplate)
    st_crs(out) = "+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 #+y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
    #st_crs(out.proportion) = 3005
    
    #out + template
    stars::write_stars( out.proportion,
                        paste0(outDir,"/",
                               "submodel", "/", "proportion", "/",    #sub-directoy
                               "model_propagree_",i,".tif")) #tile name
    
    # write out subcomponents
    keep_ens <- names(out_dat) 
    
    ## Set up subdirectories for rastertile outputs
    # print("... exporting raster tiles...")
      for (kk in keep_ens) {
        dir.create(paste(outDir, kk, sep = "/"))
      }
    
    
    for (kkk in 1:length(keep_ens)) {
      #kk <- 1  ## testing
      out <- stars::st_rasterize(out_dat[kkk],
                                 template = rtemplate)
      st_crs(out) = "+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 #+y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
      #st_crs(out) = 3005
      
      #out + template
      stars::write_stars(out,
                         paste0(outDir,"/",
                                keep_ens[kkk], "/",             #sub-directoy
                                keep_ens[kkk], "_", i, ".tif")) #tile name
    }
    
    
    
  } ## end if statement -- for when tile is empty
  
} ## END of TILE LOOP -------------

print("All predicted tiles generated")

## Mosaic Tiles ---------------

print("Generating raster mosaics")
for (k in keep) {
  # get list of tiles
  k = "prob.SBSmc2_01" # testing
  r_tiles <- list.files(paste(outDir, k, sep = "/"),
                        pattern = ".tif$",
                        full.names = TRUE)
  
  ## mosaic
  gdalUtils::mosaic_rasters(gdalfile = r_tiles,
                            dst_dataset = paste0(outDir, "/", k, ".tif"),  #output: dir and filename
                            output_Raster = TRUE) ## saves the raster (not just a virtual raster)
  
}

# list folders: 
folders <- list.dirs("D:/PEM_DATA/BEC_DevExchange_Work/Deception_AOI/3_maps_analysis/models/forest/fore_mu_bgc_S2/SBSmc2/predicted")
folders <- folders[c(2:5,7,13)]

for(f in folders) {
  f = folders[1]
  fname <- basename(f)
  r_tiles <- list.files(f,
                        pattern = ".tif$",
                        full.names = TRUE)
  ## mosaic
  output_dir <- "D:/PEM_DATA/BEC_DevExchange_Work/Deception_AOI/3_maps_analysis/models/forest/fore_mu_bgc_S2/SBSmc2/predicted"
  
  gdalUtils::mosaic_rasters(gdalfile = r_tiles,
                            dst_dataset = paste0(  output_dir , "/", fname, ".tif"),  #output: dir and filename
                            output_Raster = TRUE) ## saves the raster (not just a virtual raster)
  
  
} 


for (kkk in keep) {
  # get list of tiles
  #k = "response" # testing
  r_tiles <- list.files(paste(outDir, k, sep = "/"),
                        pattern = ".tif$",
                        full.names = TRUE)
  
  ## mosaic
  gdalUtils::mosaic_rasters(gdalfile = r_tiles,
                            dst_dataset = paste0(outDir, "/", k, ".tif"),  #output: dir and filename
                            output_Raster = TRUE) ## saves the raster (not just a virtual raster)
  
}



} ### end positive if statment ----------

} ### end function

```



    
    
    
    
    


# predict single map 

predict_map(model = paste0(out_dir,"/",mname,"/model.rds"), 
            cov = rast_list, 
            tilesize = 500,
            outDir = paste0(out_dir,"/", mname,"/predicted"))



# predict series of maps: 

bgcs <- list.files(paste0(out_dir))

for (i in bgcs) {
  
  bgc <- bgcs[2]
  model_name <- paste0(out_dir,"/",bgc,"/","_uc","/model.rds")
  
  predict_map(model = model_name, 
              cov = rast_list, 
              tilesize = 500,
              outDir = paste0(out_dir,"/", bgc,"/_uc","/predicted"))
  
}

#  # or use this code - need to adapt
#  # produce a map from each of the models
# subzone_maps <- lapply(names(model_bgc), function(model) {
#    predict_landscape_quick(
#     mod = model_bgc[[model]], 
#     cov = rast_list_s, 
#     tilesize = 500, 
#     outDir = file.path(out_dir, paste0(model, "_25m"), "quick_map"), 
#     bgc = FALSE,
#     rast_aoi = forest_aoi 
#   )
# })



# generate a quick model 

#model <- ranger(target ~ ., data = mpts, #do.trace = 10,
#         num.trees = 501,   importance = "impurity", 
#         num.threads = parallel::detectCores()-1,
#         splitrule = "extratrees", #seed = 12345, 
#         write.forest = TRUE, classification = TRUE) #strata=BGC, sampsize= c(500),

#model_quick_ranger_outputs(model, model_name = mname, modelout_dir = out_dir)

#predict_landscape_quick(mod =  model, 
#                        cov = rast_list,
#                        tilesize = 500,
#                        outDir = file.path(out_dir,mname,"quick_map"),                                                        bgc = FALSE, 
#                         rast_aoi = FALSE) 


```

We are currently testing multiple methods to generate a stage 2 sampling plan 

1) Use entropy layer (as conducted by Colin and Che at Aleza Lake)
2) Generate multiple maps and uncertainty using multiple models (ie XGBoost, ranger, ) and sample in areas of highest uncertainty between maps



## Generate Entropy layer 

```{r generate entropy layer}

entropy <- function(prob_folder)
  
  prob_dir <-list.dirs(out_dir)
prob_files <- prob_dir[grep("\\predicted$",  prob_dir)]


# check if more then one BGC is mapped
bgcs <- gsub(paste0(out_dir,"/"), "", prob_files)
bgcs <- gsub("/.*","", bgcs) 

bgcs <- bgcs[2]

if(length(bgcs == 1)) {
  i = 1
  
  bgc <- bgcs[i]
  
  fname <- paste(out_dir, bgc, paste0(bgc,"_entropy.tif"), sep = "/")
  #fname1 <- paste(out_dir, bgc, paste0(bgc,"_entropy1.tif"), sep = "/")
  #fname2 <- paste(out_dir, bgc, paste0(bgc,"_entropy2.tif"), sep = "/")
  
  probabilities <- list.files(prob_files[i], pattern= "\\.tif$", full.name= TRUE)
  probabilities <- probabilities[-(grep("response.tif$", probabilities))]
  
  # tprob <- probabilities[1:2]
  #  probabilities <- stack(tprob)
  
  
  probabilities <- stack(probabilities)
  n <- nlayers(probabilities)
  f1 <- function(x){x*log(x+0.000001)}
  
  library(snow)
  beginCluster(4)
  probabilities <- clusterR(probabilities, calc, args=list(fun=f1))
  entropy <- calc(probabilities, fun=sum)
  entropy2 <- -1*entropy/log(n)
  endCluster()
  writeRaster(entropy, filename= fname1, format="GTiff", overwrite=TRUE)
  writeRaster(entropy2, filename= fname2, format="GTiff", overwrite=TRUE)
}


## Plot Entropy layer
my_pal <- rev(brewer.pal(100,"Blues"))
# plot(entropy, col = my_pal, main = "Entropy / Uncertainty", legend = TRUE, axes = FALSE, box = FALSE)

tm_shape(entropy) +
  tm_raster(palette = my_pal, 
            title = "", 
            legend.hist = TRUE, 
            breaks = seq(0,1, by = .1)) +
  tm_layout(frame = FALSE, legend.outside = TRUE, legend.hist.width = 1, legend.hist.height = 0.5, main.title = "Entropy")

# PROJ <- as.character(crs(entropy)) ## sets the crs


