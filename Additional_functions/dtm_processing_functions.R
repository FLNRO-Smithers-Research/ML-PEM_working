
dtm_overlap <- function(id, tiles, dtm.tif="/data/ISDA/100m/height_terrain_mean_2019_100m.tif", extend.w=3e4, out.dir="/data/ISDA/tt/", sel=c("TWI","VBF"), cpus=8, srcnodata=-32768){
  out.sgrd = paste0(out.dir, "T", id, "/dtm_", id, ".sgrd")
  out.tif = paste0(out.dir, "T", id, "/dtm_", id, "_twi.tif")
  if(!file.exists(out.tif)){
    te0 = tiles@data[tiles$id == id,c("xl", "yl", "xu", "yu")]
    projwin = paste(te0[c(1,4,3,2)], collapse = " ")
    te0[c(1,2)] = te0[c(1,2)] - extend.w
    te0[c(3,4)] = te0[c(3,4)] + extend.w
    te = paste(te0, collapse = " ")
    system(paste0('gdalwarp ', dtm.tif, ' ', gsub(".sgrd", ".rst", out.sgrd), ' -of \"RST\" -ot \"Int16\" -overwrite -srcnodata \"', srcnodata, '\" -r \"near\" -te ', te))
    system(paste0('/data/git/whitebox-tools/target/release/whitebox_tools --run=FlowAccumulationFullWorkflow --dem="', gsub(".sgrd", ".rst", out.sgrd), '" --out_type="Specific Contributing Area" --log="False" --clip="False" --esri_pntr="False" --out_dem="', gsub(".sgrd", "_out.rst", out.sgrd), '" --out_pntr="',  gsub(".sgrd", "_pntr.rst", out.sgrd), '" --out_accum="', gsub(".sgrd", "_accum.rst", out.sgrd), '"'))
    system(paste0('gdal_translate ', gsub(".sgrd", "_out.rst", out.sgrd), ' ', gsub(".sgrd", ".sdat", out.sgrd), ' -of \"SAGA\" -a_nodata \"', srcnodata, '\" -ot \"Int16\" -a_srs \"', tiles@proj4string, '\"'))
    system(paste0('gdal_translate ', gsub(".sgrd", "_out.rst", out.sgrd), ' ', gsub(".sgrd", "_out.tif", out.sgrd), ' -ot \"Int16\" -a_nodata \"', srcnodata, '\" -b 1 -r \"near\" -co \"BIGTIFF=YES\" -projwin ', projwin, ' -a_srs \"', tiles@proj4string, '\"'))
    x = saga_DEM_derivatives(INPUT=out.sgrd, sel=sel, cpus=cpus)
    unlink(gsub(".sgrd", ".sdat", out.sgrd))
    sdat.lst = list.files(paste0(out.dir, "T", id), pattern = glob2rx("*.sdat$"), full.names = TRUE)
    x = lapply(sdat.lst, function(i){ sdat2geotif(i, projwin=projwin) })
    unlink(list.files(paste0(out.dir, "T", id), pattern = glob2rx("*.rst$"), full.names = TRUE))
    unlink(list.files(paste0(out.dir, "T", id), pattern = glob2rx("*.rdc$"), full.names = TRUE))
    unlink(list.files(paste0(out.dir, "T", id), pattern = glob2rx("*.ref$"), full.names = TRUE))
    unlink(list.files(paste0(out.dir, "T", id), pattern = glob2rx("*.sdat$"), full.names = TRUE))
    unlink(list.files(paste0(out.dir, "T", id), pattern = glob2rx("*.sgrd$"), full.names = TRUE))
    unlink(list.files(paste0(out.dir, "T", id), pattern = glob2rx("*.prj$"), full.names = TRUE))
    unlink(list.files(paste0(out.dir, "T", id), pattern = glob2rx("*.mgrd$"), full.names = TRUE))
    unlink(list.files(paste0(out.dir, "T", id), pattern = glob2rx("*.aux.xml$"), full.names = TRUE))
  }
}

sdat2geotif <- function(x, imin=NULL, imax=NULL, omin=NULL, omax=NULL, ot="Int16", mv=-32768, projwin=NULL){
  if(!file.exists(gsub(".sdat", ".tif", x))){
    if(length(grep(pattern = "cprof", x))>0){
      imin = -0.01; imax = 0.01; omin = -10000; omax = 10000
    }
    if(length(grep(pattern = "devmean", x))>0){
      imin = -15; imax = 15; omin = -15000; omax = 15000
    }
    if(length(grep(pattern = "down", x))>0){
      imin = -50; imax = 50; omin = -5000; omax = 5000
    }
    if(length(grep(pattern = "uplocal", x))>0){
      imin = -50; imax = 50; omin = -5000; omax = 5000
    }
    if(length(grep(pattern = "vbf", x))>0){
      imin = 0; imax = 10; omin = 0; omax = 1000
    }
    if(length(grep(pattern = "mrn", x))>0){
      imin = 0; imax = 50; omin = 0; omax = 500
    }
    if(length(grep(pattern = "open", x))>0){
      imin = 0; imax = 4; omin = 0; omax = 1000
    }
    if(length(grep(pattern = "slope", x))>0){
      imin = 0; imax = 1; omin = 0; omax = 100
    }
    if(length(grep(pattern = "tpi", x))>0){
      imin = -100; imax = 100; omin = -10000; omax = 10000
    }
    if(length(grep(pattern = "twi", x))>0){
      imin = 0; imax = 500; omin = 0; omax = 5000
    }
    if(length(grep(pattern = "catchm", x))>0){
      ot = "Int32"
    }
    if(is.null(omin)){
      if(is.null(projwin)){
        system(paste0('gdal_translate ', x, ' ', gsub(".sdat", ".tif", x), ' -co \"COMPRESS=DEFLATE\" -ot \"', ot, '\"'))
      } else {
        system(paste0('gdal_translate ', x, ' ', gsub(".sdat", ".tif", x), ' -co \"COMPRESS=DEFLATE\" -ot \"', ot, '\" -projwin ', projwin))
      }
    } else {
      if(is.null(projwin)){
        system(paste0('gdal_translate ', x, ' ', gsub(".sdat", ".tif", x), ' -co \"BIGTIFF=YES\" -co \"COMPRESS=DEFLATE\" -scale ', imin, ' ', imax, ' ', omin, ' ', omax, ' -ot \"', ot, '\" -a_nodata \"', mv, '\"'))
      } else {
        system(paste0('gdal_translate ', x, ' ', gsub(".sdat", ".tif", x), ' -co \"BIGTIFF=YES\" -co \"COMPRESS=DEFLATE\" -scale ', imin, ' ', imax, ' ', omin, ' ', omax, ' -ot \"', ot, '\" -a_nodata \"', mv, '\" -projwin ', projwin))
      }
    }
  }
}

## Derive some standard DEM variables of interest for soil mapping:
saga_DEM_derivatives <- function(INPUT, MASK=NULL, sel=c("SLP","CPR","TWI","CRV","VBF","VDP","OPN","DVM","MRN","TPI"), RADIUS=c(9,13), cpus=parallel::detectCores(logical = FALSE)){
  if(pkgmaker::file_extension(INPUT)=="tif"){ 
    if(!file.exists(gsub(".tif", ".sdat", INPUT))){
      system(paste0('gdal_translate ', INPUT, ' ', gsub(".tif", ".sdat", INPUT), ' -of \"SAGA\" -ot \"Int16\"'))
    }
    INPUT = gsub(".tif", ".sgrd", INPUT)
  }
  if(!is.null(MASK)){
    ## Fill in missing DEM pixels:
    suppressWarnings( system(paste0('saga_cmd -c=', cpus,' grid_tools 25 -GRID=\"', INPUT, '\" -MASK=\"', MASK, '\" -CLOSED=\"', INPUT, '\"')) )
  }
  ## Uplslope curvature:
  if(any(sel %in% "CRV")){
    if(!file.exists(gsub(".sgrd", "_downlocal.tif", INPUT))){
      try( suppressWarnings( system(paste0('saga_cmd -c=', cpus,' ta_morphometry 26 -DEM=\"', INPUT, '\" -C_DOWN_LOCAL=\"', gsub(".sgrd", "_downlocal.sgrd", INPUT), '\" -C_UP_LOCAL=\"', gsub(".sgrd", "_uplocal.sgrd", INPUT), '\" -C_UP=\"tmp.sgrd\" -C_LOCAL=\"tmp.sgrd\" -C_DOWN=\"', gsub(".sgrd", "_down.sgrd", INPUT), '\"') ) ) )
    }
  }
  ## Slope:
  if(any(sel %in% "SLP")){
    if(!file.exists(gsub(".sgrd", "_slope.tif", INPUT))){
      try( suppressWarnings( system(paste0('saga_cmd -c=', cpus,' ta_morphometry 0 -ELEVATION=\"', INPUT, '\" -SLOPE=\"', gsub(".sgrd", "_slope.sgrd", INPUT), '\"') ) ) )
    }
  }
  ## CProf:
  if(any(sel %in% "CPR")){
    if(!file.exists(gsub(".sgrd", "_cprof.tif", INPUT))){
      try( suppressWarnings( system(paste0('saga_cmd -c=', cpus,' ta_morphometry 0 -ELEVATION=\"', INPUT, '\" -C_PROF=\"', gsub(".sgrd", "_cprof.sgrd", INPUT), '\"') ) ) )
    }
  }
  ## MrVBF:
  if(any(sel %in% "VBF")){
    if(!file.exists(gsub(".sgrd", "_vbf.tif", INPUT))){
      try( suppressWarnings( system(paste0('saga_cmd -c=', cpus,' ta_morphometry 8 -DEM=\"', INPUT, '\" -MRVBF=\"', gsub(".sgrd", "_vbf.sgrd", INPUT), '\" -T_SLOPE=8 -P_SLOPE=2') ) ) )
    }
  }
  ## Valley depth:
  if(any(sel %in% "VDP")){
    if(!file.exists(gsub(".sgrd", "_vdepth.tif", INPUT))){
      try( suppressWarnings( system(paste0('saga_cmd -c=', cpus,' ta_channels 7 -ELEVATION=\"', INPUT, '\" -VALLEY_DEPTH=\"', gsub(".sgrd", "_vdepth.sgrd", INPUT), '\"') ) ) )
    }
  }
  ## Openess:
  if(any(sel %in% "OPN")){
    if(!file.exists(gsub(".sgrd", "_openp.tif", INPUT))){
      try( suppressWarnings( system(paste0('saga_cmd -c=', cpus,' ta_lighting 5 -DEM=\"', INPUT, '\" -POS=\"', gsub(".sgrd", "_openp.sgrd", INPUT), '\" -NEG=\"', gsub(".sgrd", "_openn.sgrd", INPUT), '\" -METHOD=0' ) ) ) )
    }
  }
  ## Deviation from Mean Value:
  if(any(sel %in% "DVM")){
    if(!file.exists(gsub(".sgrd", "_devmean.tif", INPUT))){
      suppressWarnings( system(paste0('saga_cmd -c=', cpus,' statistics_grid 1 -GRID=\"', INPUT, '\" -DEVMEAN=\"', gsub(".sgrd", "_devmean.sgrd", INPUT), '\" -RADIUS=', RADIUS[1] ) ) )
    }
    if(!file.exists(gsub(".sgrd", "_devmean2.tif", INPUT))){
      suppressWarnings( system(paste0('saga_cmd -c=', cpus,' statistics_grid 1 -GRID=\"', INPUT, '\" -DEVMEAN=\"', gsub(".sgrd", "_devmean2.sgrd", INPUT), '\" -RADIUS=', RADIUS[2] ) ) )
    }
  }
  ## TWI:
  if(any(sel %in% "TWI")){
    if(!file.exists(gsub(".sgrd", "_twi.tif", INPUT))){
      try( suppressWarnings( system(paste0('saga_cmd -c=', cpus,' ta_hydrology 15 -DEM=\"', INPUT, '\" -SLOPE_MIN=0.04 -SLOPE_OFF=0.3 -AREA_MOD=\"', gsub(".sgrd", "_catchm.sgrd", INPUT), '\" -SLOPE_TYPE=0 -TWI=\"', gsub(".sgrd", "_twi.sgrd", INPUT), '\"') ) ) )
    }
  }
  ## Melton Ruggedness Number:
  if(any(sel %in% "MRN")){
    if(!file.exists(gsub(".sgrd", "_mrn.tif", INPUT))){
      suppressWarnings( system(paste0('saga_cmd -c=', cpus,' ta_hydrology 23 -DEM=\"', INPUT, '\" -AREA=\"tmp.sgrd\" -MRN=\"', gsub(".sgrd", "_mrn.sgrd", INPUT), '\" -ZMAX=\"tmp.sgrd\"' ) ) )
    }
  }
  ## TPI:
  if(any(sel %in% "TPI")){
    if(!file.exists(gsub(".sgrd", "_tpi.tif", INPUT))){
      suppressWarnings( system(paste0('saga_cmd -c=', cpus,' ta_morphometry 18 -DEM=\"', INPUT, '\" -STANDARD=1 -TPI=\"', gsub(".sgrd", "_tpi.sgrd", INPUT), '\" -RADIUS_MIN=0 -RADIUS_MAX=2000 -DW_WEIGHTING=3 -DW_BANDWIDTH=75' ) ) )
    }
  }
}
