---
title: "Spatial Layer Preparation for DEM derived covariates"
Script author: "Colin Chisholm; Gen Perkins" 
date: "20/8/2019"
output:
    pdf_document

---
```{r global_options, include=FALSE }
require(knitr)
#knitr::opts_chunk$set(fig.width=12, fig.height=8, fig.path='Figs/',
#                      echo=FALSE, warning=FALSE, message=FALSE)
#knitr::opts_knit$set(progress=TRUE, root.dir = 'relative_path_to_root_from_Rmd')
```

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
rm(list=ls())
library(RSAGA)
library(raster)
library(sf)
library(rgdal)
library(purrr)
library(dplyr)
```

## Introduction
This document describes the steps required to prepare spatial layers to be used in machine-learning based predictive ecosystem map method. This is a preliminary step to Stage 1 Sample Design (script ref.) and the modeling process (script ref). 

This method is scripted in R, but uses other open source packages, predominently SAGA :  (https://sourceforge.net/projects/saga-gis/). This script enables you to set up a connection from R to Sage and pass commands to the saga.exe. This relies on the package RSAGA[https://cran.r-project.org/web/packages/RSAGA/RSAGA.pdf]. 

The outputs of the scripts in



###Preparation steps. 

### Software
Prior to running this script you will need to download SAGA[https://sourceforge.net/projects/saga-gis/] and know its location. SAGA can be downloaded on an external drive and run independent of pemissions 

### Data 
To run this script you will need a base digital elevation model (digital terrain model) at the spatial resolution of your choice. The base rasters for the PEM research project are 2.5m. 


### 1) Set up link between Saga and R. 
Depending on your location of the saga program you will need to define the location of the saga_cmd.exe file. This can be done manually as shown below, or can be called using the #myenv <- rsaga.env() function which will search in successive locations on your hard drive. This is normally within the Programs folder on your C:/ drive. 

```{r cost, tidy = TRUE, warning=FALSE}

# Set up Saga - r link on your machine -----------------------------
if(Sys.info()['sysname'] == "Windows"){
  #saga_cmd ="D:\\Programs\\SAGA-GIS\\saga_cmd.exe"
  saga_cmd ="C:/software/saga-7.4.0_x64/saga_cmd.exe"
   } else {
  saga_cmd = "saga_cmd"}   

#env = "D:\\Programs\\SAGA-GIS\\saga_cmd.exe"
env = saga_cmd

```

### 2) Set up your input/output data folders. 
In this example we create a folder structure and associated temporary folders to generate and store outputs. 

```{r, include = TRUE, echo = TRUE}
# Set up location and format of base DEM ---------------------------
#saga_files = file.path("D:/01Deception_Spatial_Layers/DEMs/")

saga_files = "D:\\PEM_DATA\\BEC_DevExchange_Work\\Deception_AOI\\1_map_inputs\\covariates\\2.5m"
saga_files = tmpfolder


#saga_files = file.path("./01Deception_Spatial_Layers/DEMs")
ifelse(!dir.exists(file.path(saga_files)),               #if tmpOut Does not Exists
       dir.create(file.path(saga_files)), FALSE)  


```

Save the base raster (dem/dtm) in this folder. 

```{r, echo = TRUE}
 # You need to save base raster (2.5m into this folder)
         # you can do this manually - depending on folder structure 
         
         # or 
         
         # for Gen;s files copy over the 2.5 raster 
         tmpfolder = "D:/PEM_DATA/Data/Layers/Saga_layers"
         tmpname = "DEMf.tif"
         DTM <- raster(file.path (tmpfolder, tmpname))
         writeRaster(DTM, file.path( tmpfolder,"DEM.tif"), overwrite = TRUE)  # save SAGA Version
         
         saga_files = tmpfolder
         
         # or 
         #r <- readGDAL(file.path (DTMpath, DTMname))
         #w <- file.path(saga_files,"DEM.tif")
         #writeGDAL(r, w)
```

Read in the Dem and convert to a saga object. We also create a temp file for interdemiate saga outputs called sagaTmp.

```{r}
# read in base DEM -----------------------------------
DTMname = "DEM.tif"
DTM <- raster(file.path (saga_files, DTMname))
#DTM = raster(file.path(saga_files, "Deception_dem_2p51_2.5m.tif"))

# project raster ? to do....
#PROJ <- crs(paste("+init=epsg:",3005, sep = "")) 
#crs(DTM) <- PROJ 
# set up output folder 
tmpOut <- file.path(saga_files, "sagaTmp")
ifelse(!dir.exists(file.path(tmpOut)),               #if tmpOut Does not Exists
       dir.create(file.path(tmpOut)), FALSE)        #create tmpOut
# export to saga format: 
sDTM <- "DEM_2_5m.sdat"
sDTM <- file.path(tmpOut, sDTM)
writeRaster(DTM, sDTM, driver = "SAGA", overwrite = TRUE)  # save SAGA Version
#w <- file.path(saga_files,"DEM.tif")
#writeGDAL(r, w)
```

### 3) Generate DEM derived layers using SAGA. 

For each generated layer you will need to setup the input parameters to send to SAGA. Documentation for each layer parameters are found here: http://www.saga-gis.org/saga_tool_doc/7.2.0/a2z.html. The parameters below use the default options and will require further investigation and testing to ensure they are capturing the desired landscape features. 


```{r, Generate Pem layers, echo = TRUE, eval=FALSE}
##### >> 1 -- Fill Sinks XXL (Wang and Liu)  -----------------------------
# http://www.saga-gis.org/saga_tool_doc/7.2.0/ta_preprocessor_5.html
sinksFilled <- "Filled_sinks.sgrd"
sinksFilled = file.path(tmpOut, sinksFilled)
sysCMD <- paste(saga_cmd, "ta_preprocessor 5", "-ELEV" , 
                file.path(gsub("sdat","sgrd", sDTM)),        
                "-FILLED", sinksFilled,                               
                "-MINSLOPE ", 0.1                                       
)
system(sysCMD)
##### >> 2 -- Total Catchment Area --------------------------------------
# http://www.saga-gis.org/saga_tool_doc/7.2.0/ta_hydrology_0.html
tCatchment <- "tCatchment.sgrd"
tCatchment = file.path(tmpOut, tCatchment)
sysCMD <- paste(saga_cmd, "ta_hydrology 0", "-ELEVATION", 
                file.path(gsub("sdat","sgrd", sDTM)), 
                "-FLOW", tCatchment,                                    # Output
                "-METHOD", 4                                            # Default Parameters
)
system(sysCMD)
##### >> 3 -- Flow Width and Specific Catchment Area --------------------
# http://www.saga-gis.org/saga_tool_doc/7.2.0/ta_hydrology_19.html
sCatchment <- "Specific_Catchment.sgrd"
sysCMD <- paste(saga_cmd, "ta_hydrology 19", "-DEM", sinksFilled,       # Input from 1
                "-SCA", sCatchment,                                     # Output
                "-TCA", tCatchment,                                     # Input from 2
                "-METHOD", 1                                            # Parameters
)
system(sysCMD)
##### >> 4 -- Channel Network -------------------------------------------
# http://www.saga-gis.org/saga_tool_doc/7.2.0/ta_channels_0.html
# https://sourceforge.net/projects/saga-gis/files/SAGA%20-%20Documentation/SAGA%20Documents/SagaManual.pdf/download
channelsNetwork <- "Channel_network_grid.sgrd"
sysCMD <- paste(saga_cmd, "ta_channels 0", "-ELEVATION", sinksFilled,     # Input from 1
                "-CHNLNTWRK", channelsNetwork,                            # Output
                "-INIT_GRID", tCatchment,                                 # Input from 2
                "-INIT_VALUE", 1000000, "-INIT_METHOD", 2,                # Based on SAGA Manual Documentation, p. 119 
                "-DIV_CELLS", 5.0, "-MINLEN", 10.0                        # Default Parameters
)
system(sysCMD)
##### >> 5 -- Overland Flow Distance to Channel Network -----------------
# http://www.saga-gis.org/saga_tool_doc/7.2.0/ta_channels_4.html
hDistance <- "OverlandFlowDistance.sgrd"
vDistance  <- "VerticalDistance.sgrd"
sysCMD <- paste(saga_cmd, "ta_channels 4", "-ELEVATION", sinksFilled,   # Input from 1
                "-CHANNELS", channelsNetwork,                             # Input from 4
                "-DISTANCE", hDistance, "-DISTVERT", vDistance,           # Outputs
                "-METHOD", 1, "-BOUNDARY", 1                              # Parameters
)
system(sysCMD)
##### >> 6 -- MRVBF -----------------------------------------------------
# http://www.saga-gis.org/saga_tool_doc/7.2.0/ta_morphometry_8.html
MRVBF <- "MRVBF.sgrd"
MRRTF <- "MRRTF.sgrd"
sysCMD <- paste(saga_cmd, "ta_morphometry 8", "-DEM",
                file.path(gsub("sdat","sgrd", sDTM)),
                "-MRVBF", MRVBF, "-MRRTF", MRRTF,                       # Outputs
                "-T_SLOPE", 16, "-T_PCTL_V", 0.4, "-T_PCTL_R", 0.35,    # Default Parameters
                "-P_SLOPE", 4.0, "-P_PCTL", 3.0, "-UPDATE", 0,
                "-CLASSIFY", 0,"-MAX_RES", 100
)
system(sysCMD)
##### >> 7 -- Terrain Ruggedness Index ----------------------------------
# http://www.saga-gis.org/saga_tool_doc/7.2.0/ta_morphometry_16.html
TRI <- "TRI.sgrd"
sysCMD <- paste(saga_cmd, "ta_morphometry 16", "-DEM",
                file.path(gsub("sdat","sgrd", sDTM)), 
                "-TRI", TRI,                                            # Output
                "-MODE", 1, "-RADIUS", 3.0, "-DW_WEIGHTING", 0          # Parameters
)
system(sysCMD)
##### >> 8 -- Convergence Index -----------------------------------------
# http://www.saga-gis.org/saga_tool_doc/7.2.0/ta_morphometry_1.html
convergence <- "Convergence.sgrd"
sysCMD <- paste(saga_cmd, "ta_morphometry 1", "-ELEVATION ",
                file.path(gsub("sdat","sgrd", sDTM)),      # Input DTM
                "-RESULT", convergence,                                 # Output
                "-METHOD", 1, "-NEIGHBOURS", 1                          # Parameters
)
system(sysCMD)
##### >> 9 -- Openness --------------------------------------------------
# http://www.saga-gis.org/saga_tool_doc/7.2.0/ta_lighting_5.html
POS <- "OpennessPositive.sgrd"
NEG <- "OpennessNegative.sgrd"
sysCMD <- paste(saga_cmd, "ta_lighting 5", "-DEM", 
                file.path(gsub("sdat","sgrd", sDTM)),
                "-POS", POS, "-NEG", NEG,                               # Outputs
                "-RADIUS", 1000, "-METHOD", 1,                          # Default Parameters
                "-DLEVEL",  3, "-NDIRS", 8
)
system(sysCMD)
##### >> 10 -- Diuranal Anisotropic Heating -----------------------------
# http://www.saga-gis.org/saga_tool_doc/7.2.0/ta_morphometry_12.html
dAH <- "dAH.sgrd"
sysCMD <- paste(saga_cmd, "ta_morphometry 12", "-DEM",
                file.path(gsub("sdat","sgrd", sDTM)), # Input DTM
                "-DAH", dAH,                                            # Output
                "-ALPHA_MAX", 202.5                                     # Default Parameters
)
system(sysCMD)
##### >> 11 -- Slope Aspect and Curvature -------------------------------
# http://www.saga-gis.org/saga_tool_doc/7.2.0/ta_morphometry_0.html
Slope <- "Slope.sgrd"
Slope = file.path(tmpOut, Slope)
Aspect <- "Aspect.sgrd"
Aspect = file.path(tmpOut, Aspect)
Curvature <- "gCurvature.sgrd"
Curvature = file.path(tmpOut, Curvature)
tCurve <- "tCurve.sgrd"
tCurve = file.path(tmpOut, tCurve)
ProfCurve = "ProfCurve.sgrd"
ProfCurve = file.path(tmpOut, ProfCurve)
PlanCurve = "PlanCurve.sgrd"
PlanCurve = file.path(tmpOut, PlanCurve)
TangCurve = "TangCurve.sgrd"
TangCurve = file.path(tmpOut, TangCurve)
LongCurve = "LongCurve.sgrd"
LongCurve = file.path(tmpOut, LongCurve)
CrossCurve = "CrossCurve.sgrd"
CrossCurve = file.path(tmpOut, CrossCurve)
MinCurve = "MinCurve.sgrd"
MinCurve = file.path(tmpOut, MinCurve)
MaxCurve = "MaxCurve.sgrd"
MaxCurve = file.path(tmpOut, MaxCurve)
FlowCurve = "FlowCurve.sgrd"
FlowCurve = file.path(tmpOut, FlowCurve)
sysCMD <- paste(saga_cmd, "ta_morphometry 0", "-ELEVATION", 
                file.path(gsub("sdat","sgrd", sDTM)),     # Input DTM
                "-SLOPE", Slope, "-ASPECT", Aspect,                     # Outputs
                "-C_GENE", Curvature, "-C_TOTA", tCurve,                # Outputs
                "-C_PROF", ProfCurve,
                "-C_PLAN", PlanCurve,
                "-C_TANG", TangCurve,
                "-C_LONG", LongCurve,
                "-C_CROS", CrossCurve,
                "-C_MINI", MinCurve,
                "-C_MAXI", MaxCurve,
                "-C_ROTO", FlowCurve,
                "-METHOD", 6, "-UNIT_SLOPE", 0, "-UNIT_ASPECT", 0       # Default Parameters
)
system(sysCMD)
##### >> 12 -- Topographic Position Index --------------------------------
# http://www.saga-gis.org/saga_tool_doc/7.2.0/ta_morphometry_18.html
TPI <- "TPI.sgrd"
sysCMD <- paste(saga_cmd, "ta_morphometry 18", "-DEM", 
                file.path(gsub("sdat","sgrd", sDTM)),# Input DTM
                "-TPI", TPI,                                            # Output
                "-STANDARD", 0, "-RADIUS_MIN", 0, "-RADIUS_MAX", 100,   # Default Parameters
                "-DW_WEIGHTING", 0, "-DW_IDW_POWER", 1, 
                "-DW_IDW_OFFSET", 1, "-DW_BANDWIDTH", 75
)
system(sysCMD)
##### >> 13 -- Topographic Wetness Index --------------------------------
# http://www.saga-gis.org/saga_tool_doc/7.2.0/ta_hydrology_20.html
TWI <- "TWI.sgrd"
sysCMD <- paste(saga_cmd, "ta_hydrology 20", "-SLOPE", Slope,           # Input from 11
                "-AREA", sCatchment,                                    # Input from 3
                "-TWI", TWI,                                            # Output
                "-CONV",0,  "-METHOD", 0                                # Default Parameters
)
system(sysCMD)
##### >> 14 -- Potential Incoming Solar Radiation -----------------------
# http://www.saga-gis.org/saga_tool_doc/7.2.0/ta_lighting_2.html
DirInsol <- "dirinsol.sgrd"
DifInsol <- "difinsol.sgrd"
sysCMD <- paste(saga_cmd, "ta_lighting 2", "-GRD_DEM", 
                file.path(gsub("sdat","sgrd", sDTM)),# Input DTM
                "-GRD_DIRECT", DirInsol, "-GRD_DIFFUS", DifInsol,       # Outputs
                "-SOLARCONST", 1367, "-LOCALSVF", 1, "-SHADOW", 0,      # Parameters
                "-LOCATION", 1, "-PERIOD", 2, "-DAY", "2018-02-15", 
                "-DAY_STOP", "2019-02-15", "-DAYS_STEP", 30, 
                "-HOUR_RANGE_MIN", 0, "-HOUR_RANGE_MAX", 24, 
                "-HOUR_STEP", 0.5, "-METHOD", 2, "-LUMPED", 70
)
system(sysCMD)
########################################################################
#Newly added SAGA tools
#### >> 15 -- Valley Depth ------------------------------------- ## works
# http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_channels_7.html
ValleyDepth = "valleydepth.sgrd"
ValleyDepth = file.path(tmpOut, ValleyDepth)
RidgeLevel = "ridgelevel.sgrd"
RidgeLevel = file.path(tmpOut, RidgeLevel)
sysCMD = paste(saga_cmd, "ta_channels 7", 
               "-ELEVATION", sinksFilled,            # input DEM
               "-VALLEY_DEPTH", ValleyDepth,         # output Valley Depth
               "-RIDGE_LEVEL", RidgeLevel,           # output Ridge Level
               "-THRESHOLD", 1,
               "-NOUNDERGROUND", 1,
               "-ORDER", 4
)
system(sysCMD)
#### >> 16 -- Vertical Distance to Channel Network ------------- ## froze - maybe just very slow?
# http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_channels_3.html
VertDistance = "vertdistance.sgrd"
VertDistance = file.path(tmpOut, VertDistance)
sysCMD = paste(saga_cmd, "ta_channels 3", 
              "-ELEVATION", sinksFilled,            # input DEM
              "-CHANNELS", channelsNetwork,         # input Channel Network
              "-DISTANCE", VertDistance,            # output 
              "-THRESHOLD", 1,
              "-NOUNDERGROUND", 1
)
system(sysCMD)
#### >> 17 -- Slope Length --------------------------------------- ## works
# http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_hydrology_7.html
SlopeLength = "SlopeLength.sgrd"
SlopeLength = file.path(tmpOut, SlopeLength)
sysCMD = paste(saga_cmd, "ta_hydrology 7", 
               "-DEM", sinksFilled,             # input DEM
              "-LENGTH", SlopeLength            # output Slope Length
)
system(sysCMD)
#### >> 18 -- SAGA Wetness Index -------------------------------- ## works but VERY slow (~18 hours)
# http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_hydrology_15.html
CatchmentArea = "CatchmentArea.sgrd"
CatchmentArea = file.path(tmpOut, CatchmentArea)
CatchmentSlope = "CatchmentSlope.sgrd"
CatchmentSlope = file.path(tmpOut, CatchmentSlope)
ModCatchmentArea = "ModCatchmentArea.sgrd"
ModCatchmentArea = file.path(tmpOut, ModCatchmentArea)
TopoWetIndex = "TopoWetIndex.sgrd"
TopoWetIndex = file.path(tmpOut, TopoWetIndex)
sysCMD = paste(saga_cmd, "ta_hydrology 15", 
            "-DEM", sinksFilled,                 # input DEM
            "-AREA", CatchmentArea,              # output Catchment Area
            "-SLOPE", CatchmentSlope,            # output Catchment Slope
            "-AREA_MOD", ModCatchmentArea,       # output Modified Catchment Area
            "-TWI", TopoWetIndex,                # output TWI
            "-SUCTION", 10,
            "-AREA_TYPE", 1, 
            "-SLOPE_TYPE", 1,
            "-SLOPE_MIN", 0,
            "-SLOPE_OFF", 0.1,
            "-SLOPE_WEIGHT", 1
)
system(sysCMD)
#### >> 19 -- Melton Ruggedness Number -------------------------- ## works
# http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_hydrology_23.html
MRNCatchment = "MRNCatchment.sgrd"
MRNCatchment = file.path(tmpOut, MRNCatchment)
MRNMaxHeight = "MRNMaxHeight.sgrd"
MRNMaxHeight = file.path(tmpOut, MRNMaxHeight)
MRN = "MRN.sgrd"
MRN = file.path(tmpOut, MRN)
sysCMD = paste(saga_cmd, "ta_hydrology 23", 
         "-DEM", sinksFilled,                 # input DEM
         "-AREA", MRNCatchment,               # output MRN Catchment
         "-ZMAX", MRNMaxHeight,               # output MRN Max Height
         "-MRN", MRN                          # output MRN
)
system(sysCMD)
#### >> 20 -- Flow Accumulation (Flow Tracing)
# http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_hydrology_2.html
FlowAccumFT = "FlowAccumFT.sgrd"
FlowAccumFT = file.path(tmpOut, FlowAccumFT)
MeanOvCatch = "MeanOvCatch.sgrd"
MeanOvCatch = file.path(tmpOut, MeanOvCatch)
AccumMaterial = "AccumMaterial.sgrd"
AccumMaterial = file.path(tmpOut, AccumMaterial)
sysCMD = paste(saga_cmd, "ta_hydrology 2",
              "-ELEVATION", sinksFilled,            # input DEM
              "-FLOW", FlowAccumFT,                 # output Flow Accumulation
              "-VAL_MEAN", MeanOvCatch,             # output Mean over Catchment
              "-ACCU_TOTAL", AccumMaterial,         # output Accumulated Material
              "-FLOW_UNIT", 1,
              "-METHOD", 1,
              "-MINDQV", 0
)
system(sysCMD)
#### >> 21 -- Flow Accumulation (Parallelizable) ------------------- ## this tool doesn't seem to exist - SAGA version issue?
# http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_hydrology_29.html
FlowAccumP = "FlowAccumP.sgrd"
FlowAccumP = file.path(tmpOut, FlowAccumP)
sysCMD = paste(saga_cmd, "ta_hydrology 29",
             "-DEM", sinksFilled,                  # input DEM
             "-FLOW", FlowAccumP,                  # output Flow Accumulation
             "-METHOD", 2,
             "-CONVERGENCE", 1.1
)
system(sysCMD)
#### >> 22 -- Flow Accumulation (Recursive) ---------------------
# http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_hydrology_1.html
FlowAccumR = "FlowAccumR.sgrd"
FlowAccumR = file.path(tmpOut, FlowAccumR)
MeanOvCatchR = "MeanOvCatchR.sgrd"
MeanOvCatchR = file.path(tmpOut, MeanOvCatchR)
AccumMaterialR = "AccumMaterialR.sgrd"
AccumMaterialR = file.path(tmpOut, AccumMaterialR)
FlowPathLen = "FlowPathLen.sgrd"
FlowPathLen = file.path(tmpOut, FlowPathLen)
sysCMD = paste(saga_cmd, "ta_hydrology 1",
             "-ELEVATION", sinksFilled,         # input DEM
             "-FLOW", FlowAccumR,               # output Flow Accumulation
             "-VAL_MEAN", MeanOvCatchR,         # output Mean over Catchment
             "-FLOW_LENGTH", FlowPathLen,       # output Flow Path Length
             "-FLOW_UNIT", 1,
             "-METHOD", 3,
             "-CONVERGENCE", 1.1
)
system(sysCMD)
#### >> 23 -- Flow Accumulation (Top-Down) ---------------------
# http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_hydrology_0.html
FlowAccumTD = "FlowAccumTD.sgrd"
FlowAccumTD = file.path(tmpOut, FlowAccumTD)
MeanOvCatchTD = "MeanOvCatchTD.sgrd"
MeanOvCatchTD = file.path(tmpOut, MeanOvCatchTD)
AccumMaterialTD = "AccumMaterialTD.sgrd"
AccumMaterialTD = file.path(tmpOut, AccumMaterialTD)
FlowPathLenTD = "FlowPathLenTD.sgrd"
FlowPathLenTD = file.path(tmpOut, FlowPathLenTD)
sysCMD = paste(saga_cmd, "ta_hydrology 0", 
             "-ELEVATION", sinksFilled,                 # input DEM
             "-FLOW", FlowAccumTD,                      # output Flow Accumulation
             "-VAL_MEAN", MeanOvCatchTD,                # output Mean over Catchment
             "-ACCU_TOTAL", AccumMaterialTD,            # output Accumulated Material
             "-FLOW_LENGTH", FlowPathLenTD,             # output Flow Path Length
             "-FLOW_UNIT", 1,
             "-METHOD", 4,
             "-LINEAR_DO", 1,
             "-LINEAR_MIN", 500,
             "-CONVERGENCE", 1.1
)
system(sysCMD)
#### >> 24 -- Stream Power Index ----------------------------------
# http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_hydrology_21.html
StreamPower = "StreamPower.sgrd"
StreamPower = file.path(tmpOut, StreamPower)
sysCMD = paste(saga_cmd, "ta_hydrology 21", 
                "-SLOPE", Slope                    # input Slope
                "-AREA", tCatchment                # input Catchment Area
                "-SPI", StreamPower,               # output Stream Power Index
                "-CONV", 0
)
system(sysCMD)
#### >> 25 -- Maximum Flow Path Length --------------------------- ## works
# http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_hydrology_27.html
FlowPathLength = "FlowPathLength.sgrd"
FlowPathLength = file.path(tmpOut, FlowPathLength)
sysCMD = paste(saga_cmd, "ta_hydrology 27", 
               "-ELEVATION", sinksFilled,            # input DEM
               "-DISTANCE", FlowPathLength,          # output Max Flow Path Length
               "-DIRECTION", 0
)
system(sysCMD)
#### >> 26 -- Slope Limited Flow Accumulation ------------------- ## works 
# http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_hydrology_26.html
FlowAccum = "FlowAccum.sgrd"
FlowAccum = file.path(tmpOut, FlowAccum)
sysCMD = paste(saga_cmd, "ta_hydrology 26", 
               "-DEM", sinksFilled,               # input DEM
               "-FLOW", FlowAccum,                # output Flow Accumulation
               "-SLOPE_MIN", 0,
               "-SLOPE_MAX", 5,
               "-B_FLOW", 0
)
system(sysCMD)
#### >> 27 -- LS Factor -----------------------------------------
# http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_hydrology_22.html
LSFactor = "LSFactor.sgrd"
LSFactor = file.path(tmpOut, LSFactor)
sysCMD = paste(saga_cmd, "ta_hydrology 22", 
               "-SLOPE", Slope,                # input Slope
               "-AREA", tCatchment,            # input Catchment Area
               "-LS", LSFactor,                # output LS Factor
               "-CONV", 0,
               "-METHOD", 0,
               "-EROSIVITY", 1,
               "-STABILITY", 0
)
system(sysCMD)
#### >> 28 -- TCI Low -------------------------------------------
# http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_hydrology_24.html
TCILow = "TCILow.sgrd"
TCILow = file.path(tmpOut, TCILow)
sysCMD = paste(saga_cmd, "ta_hydrology 24", 
              "-DISTANCE", VertDistance,            # input Vertical Distance to Channel Network
              "-TWI", TWI,                          # input TWI
              "-TCILOW", TCILow                     # output TCI Low
)
system(sysCMD)
#### >> 29 -- Wind Exposition Index ------------------------------ ## works but VERY slow
# http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_morphometry_27.html
WindExp = "WindExp.sgrd"
WindExp = file.path(tmpOut, WindExp)
sysCMD = paste(saga_cmd, "ta_morphometry 27", 
             "-DEM", sinksFilled,                     # input DEM
             "-EXPOSITION", WindExp,                  # output Wind Exposition Index
             "-MAXDIST", 300,
             "-STEP", 15,
             "-ACCEL", 1.5
)
system(sysCMD)
#### >> 30 -- Terrain Surface Convexity ---------------------------
# http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_morphometry_21.html
Convexity = "Convexity.sgrd"
Convexity = file.path(tmpOut, Convexity)
sysCMD = paste(saga_cmd, "ta_morphometry 21", 
             "-DEM", sinksFilled,                   # input DEM
             "-CONVEXITY", Convexity,               # output Convexity
             "-KERNEL", 0,
             "-TYPE", 0,
             "-EPSILON", 0,
             "-SCALE", 10,
             "-METHOD", 1,
             "-DW_WEIGHTING", 0
             "-DW_IDW_POWER", 2, 
             "-DW_BANDWIDTH", 1
)
system(sysCMD)
#### >> 31 -- Terrain Surface Texture -----------------------------
# http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_morphometry_20.html
Texture = "Texture.sgrd"
Texture = file.path(tmpOut, Texture)
sysCMD = paste(saga_cmd, "ta_morphometry 20", 
            "-DEM", sinksFilled,                      # input DEM
            "-TEXTURE", Texture,                      # output Terrain Surface Texture
            "-EPSILON", 1,
            "-SCALE", 10,
            "-METHOD", 1,
            "-DW_WEIGHTING", 0,
            "-DW_IDW_POWER", 2,
            "-DW_BANDWIDTH", 1
)
system(sysCMD)
#### >> 32 -- Morphometric Protection Index ----------------------
# http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_morphometry_7.html
Protection = "Protection.sgrd"
Protection = file.path(tmpOut, Protection)
sysCMD = paste(saga_cmd, "ta_morphometry 7",
           "-DEM", sinksFilled,                        # input DEM
           "-PROTECTION", Protection,                  # output Morphometric Protection Index
           "-RADIUS", 2000
)
system(sysCMD)
#### >> 33 -- Vector Ruggedness Measure ---------------------------
# http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_morphometry_17.html
VRM = "VRM.sgrd"
VRM = file.path(tmpOut, VRM)
sysCMD = paste(saga_cmd, "ta_morphometry 17", 
           "-DEM", sinksFilled,                      # input DEM
           "-VRM", VRM,                              # output Vector Ruggedness Measure
           "-MODE", 1,
           "-DW_WEIGHTING", 0, 
           "-DW_IDW_POWER", 2,
           "-DW_BANDWIDTH", 1
)
system(sysCMD)
#### >> 34 -- Mass Balance Index ----------------------------------
# http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_morphometry_10.html
MBI = "MBI.sgrd"
MBI = file.path(tmpOut, MBI)
sysCMD = paste(saga_cmd, "ta_morphometry 10",
           "-DEM", sinksFilled,                 # input DEM
           "-HREL", VertDistance,               # input Vertical Distance to Channel Network
           "-MBI", MBI,                         # output Mass Balance Index
           "-TSLOPE", 15,
           "-TCURVE", 0.01,
           "-THREL", 15
)
system(sysCMD)
#### >> 35 -- Multi-Scale Topographic Position Index --------------
# http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_morphometry_28.html
TPI = "TPI.sgrd"
TPI = file.path(tmpOut, TPI)
sysCMD = paste(saga_cmd, "ta_morphometry 28", 
           "-DEM", sinksFilled,                # input DEM
           "-TPI", TPI,                        # output TPI
           "SCALE_MIN", 1,
           "SCALE_MAX", 8,
           "SCALE_NUM", 3
)
system(sysCMD)
#### >> 36 -- Relative Heights and Slope Positions ----------------
# http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_morphometry_14.html
SlopeHeight = "SlopeHeight.sgrd"
SlopeHeight = file.path(tmpOut, SlopeHeight)
ValleyDepth = "ValleyDepth.sgrd" #don't need this as created above?
ValleyDepth = file.path(tmpOut, ValleyDepth)
NormHeight = "NormHeight.sgrd"
NormHeight = file.path(tmpOut, NormHeight)
StandHeight = "StandHeight.sgrd"
StandHeight = file.path(tmpOut, StandHeight)
MSPosition = "MSPosition.sgrd"
MSPosition = file.path(tmpOut, MSPosition)
sysCMD = paste(saga_cmd, "ta_morphometry 14",  
           "-DEM", sinksFilled,                 # input DEM
           "-HO", SlopeHeight,                  # output Slope Height
           "-HU", ValleyDepth,                  # output Valley Depth
           "-NH", NormHeight,                   # output Normalized Height
           "-SH", StandHeight,                  # output Standardized Height
           "-MS", MSPosition,                   # output Mid-Slope Position
           "-W", 0.5,
           "-T", 10,
           "-E", 2
)
system(sysCMD)
#### >> 37 -- Valley and Ridge Detection (Top Hat Approach) -------
# http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_morphometry_24.html
HillHeight = "HillHeight.sgrd"
HillHeight = file.path(tmpOut, HillHeight)
ValleyIndex = "ValleyIndex.sgrd"
ValleyIndex = file.path(tmpOut, ValleyIndex)
HillIndex = "HillIndex.sgrd"
HillIndex = file.path(tmpOut, HillIndex)
HillslopeIndex = "HillslopeIndex.sgrd"
HillslopeIndex = file.path(tmpOut, HillslopeIndex)
sysCMD = paste(saga_cmd, "ta_morphometry 24",
          "-DEM", sinksFilled,                 # input DEM
          "-HILL", HillHeight,                 # output Hill Height
          "-VALLEY_IDX", ValleyIndex,          # output Valley Index
          "-HILL_IDX", HillIndex,              # output Hill Index
          "-SLOPE_IDX", HillslopeIndex,        # output Hillslope Index
          "-RADIUS_VALLEY", 1000,
          "-RADIUS_HILL", 1000,
          "-THRESHOLD", 100,
          "-METHOD", 0
)
system(sysCMD)
#### >> 38 -- Upslope and Downslope Curvature ---------------------
# http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_morphometry_26.html
LocalCurve = "LocalCurve.sgrd"
LocalCurve = file.path(tmpOut, LocalCurve)
UpslopeCurve = "UpslopeCurve.sgrd"
UpslopeCurve = file.path(tmpOut, UpslopeCurve)
LocalUpCurve = "LocalUpCurve.sgrd"
LocalUpCurve = file.path(tmpOut, LocalUpCurve)
DownCurve = "DownCurve.sgrd"
DownCurve = file.path(tmpOut, DownCurve)
LocalDownCurve = "LocalDownCurve.sgrd"
LocalDownCurve = file.path(tmpOut, LocalDownCurve)
sysCMD = paste(saga_cmd, "ta_morphometry 26",
         "-DEM", sinksFilled,                         # input DEM
         "-C_LOCAL", LocalCurve,                      # output Local Curvature
         "-C_UP", UpslopeCurve,                       # output Upslope Curvature
         "-C_UP_LOCAL", LocalUpCurve,                 # output Local Upslope Curvature
         "-C_DOWN", DownCurve,                        # output Downslope Curvature
         "-C_DOWN_LOCAL", LocalDownCurve,             # output Local Downslope Curvature
         "-WEIGHTING", 0.5
)
system(sysCMD)
#### >> 39 -- Steepest Slope (Slope Aspect and Curvature) --------
# http://www.saga-gis.org/saga_tool_doc/7.2.0/ta_morphometry_0.html
SteepestSlope <- "SteepestSlope.sgrd"
SteepestSlope = file.path(tmpOut, SteepestSlope)
sysCMD <- paste(saga_cmd, "ta_morphometry 0", 
                "-ELEVATION", sinksFilled,                              # input DEM
                "-SLOPE", SteepestSlope,                                # output Steepest Slope
                "-METHOD", 1,                                           # method 1 - steepest slope
                "-UNIT_SLOPE", 0, 
                "-UNIT_ASPECT", 0      
)
system(sysCMD)
#### >> 40 -- Upslope Area -------------------------------------
# http://www.saga-gis.org/saga_tool_doc/7.6.2/ta_hydrology_4.html
UpslopeArea = "UpslopeArea.sgrd"
UpslopeArea = file.path(tmpOut, UpslopeArea)
sysCMD = paste(saga_cmd, "ta_hydrology 4",
                 "-ELEVATION", sinksFilled,           # input DEM
                 "-AREA", UpslopeArea,                # output Upslope Area
                 "-METHOD", 2,
                 "-CONVERGENCE", 1.1
)
system(sysCMD)
```


```{r, }
# Save Processed Grids to tif format -------------------------
ifelse(!dir.exists(file.path(saga_files, "outputs")),              
       dir.create(file.path(saga_files, "outputs")), FALSE)
rasterfiles <- list.files(file.path(saga_files,"sagaTmp"),pattern = ".sdat")
 
sdat_to_tif <- function(sagafile){
 
  for(i in 5:length(rasterfiles)){
   
    # error in #3 
    
    sagafile <- rasterfiles[3] # 5
   outfile <- gsub("sdat", "tif", sagafile)
   #w <- file.path(saga_files,"outputs", outfile)
   #r <- raster(sagafile)
   #writeRaster(r, w, overwrite = TRUE)
   #faster version using rgdal  
   r <- readGDAL(file.path(saga_files,"sagaTmp",sagafile))
   w <- file.path(saga_files, "outputs", outfile) #, sep = "")
   writeGDAL(r, w)

   }
```