## PEM Transect Build functions 
## Functions developed by C. Chisholm. 
## Development: May/June 2019

## The intent of this script is to simply house the functions 
## which can then be loaded by whatever script needs them through 
## a source call.  

## Create Triangles -------------------------------------------------
## Build Triangle from a ID, x and y coordinate
Tri_build <- function(ID, X, Y){
  # ID <- "Test" ; X <- 0 ; Y <- 0
  
  MoonLine <- tibble(id = ID ,  Seq = 1, x  =  X, y  =  Y) %>%
    add_row(id = ID, Seq = 2, x = X, y = Y + 250 ) %>%
    add_row(id = ID, Seq = 3, x = X + 216.5, y = Y + 125 ) %>%
    # add_row(id = ID, Seq = 4, x = X , y = Y) %>%
    sf::st_as_sf(coords = c("x","y")) %>%
    group_by(id) %>%
    summarize() %>%
    st_cast("POLYGON") %>%
    st_cast("MULTILINESTRING") %>%
    st_set_crs(PROJ)
  
  return(MoonLine)
}


## Rotation of Features ---------------------------------------------
## Math needed to rotate
rot <- function(a) matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)

## Feature rotation
rotFeature <- function(Feature, PivotPt, Bearing) {
  # where Feature is the Shape to be rotated,
  # Bearing is the compass bearing to rotate to
  # PivotPt is the point to rotate around
  
  ## extract the geometry
  Feature_geo <- st_geometry(Feature)
  PivotPoint  <- st_geometry(PivotPt)
  
  ## Convert bearing for use with rot function
  d <- ifelse(Bearing > 180, pi * ((Bearing-360)/ 180) ,  pi * (Bearing / 180))
  
  rFeature <- (Feature_geo - PivotPoint) * rot(d)   + PivotPoint
  rFeature <- st_set_crs(rFeature, PROJ)
  
  Feature$geometry <- st_geometry(rFeature) ## replace the original geometry
  return(Feature)
}

## Create a series of triangles -------------------------------------
## calls the rotation functions above
## Generate Potential lines /'MoonTransect' around a given POC
pMoonTransects <- function(POC, Shape){  ## Where C is a point feature
  ## Where is Shape is the feature to be rotated
  
  allLines <- Shape ## this will be appended to later
  
  allLines$Rot <- 0  ## no rotation for the first transect
  
  ## Loop through a sequence of bearings
  for(d in seq(15,345, by= 15)){
    
    Shape$Rot <- d  ## Adds an attribute field to indicate the bearing it was rotated to
    tmp <- rotFeature(Shape, POC, d)  ## calls rotFeature function above
    
    allLines <- rbind(allLines, tmp)
  }
  return(allLines)
}

## Shannon's Diversity Index ----------------------------------------
shannon_index <- function(a) {
  dd <- table(a)
  sPi <- dd/sum(dd)
  hh <- sPi*log(sPi)
  return(-sum(hh))
}


## Queries a raster -------------------------------------------------
## adds Shannon's Diversity Index to attributes
MoonDivQuery <- function(Raster, Feature) {
  ## create column for the new metrics
  TTs <- Feature %>% mutate(Shannon = as.numeric(NA))
  
  tmpR <- crop(Raster, extent(TTs)) ## this allows for faster processing in the loop
  
  for(i in 1:nrow(TTs)){
    tmp <- TTs[i,] ## select the transect row
    values <- unlist(extract(tmpR, tmp))  ## extracts the values under the line
    TTs$Shannon[i] <- shannon_index(values)
    
  }
  return(TTs)
}



## Intersection Function ---------------------------------------------------------- 
## Initially used to detect if transects crossed roads ... can be adapted to id if any intersection occurs. 
TX <- function(Feature, IntersectsWith){
  TTs <- Feature %>% mutate(X = as.logical(NA)) 
  
  for(i in 1:nrow(TTs)){
    tmp <- TTs[i,] ## select the transect row
    
    crosses <- st_crosses(tmp, IntersectsWith) # produces a list indicating if the features cross.
    TTs$X[i] <- !is_empty(crosses[[1]])
    
  }
  return(TTs)
}

## Paired Point ----------------------------------------------------------------
pairedPoint <- function(POC, Bearing, Dist){ #Where bearing is the bearing recorded in the transect
  # This function is dependent on other PEM functions: rot, rotFeature
  # testing
  # POC <- point2; Dist <- 100 ; Bearing <- 45 ## testing vars
  
  
  
  PROJ <- st_crs(POC)
  
  pt <- as_data_frame(POC)
  pt <- cbind(pt, st_coordinates(POC))
  pt <- pt %>% dplyr::select(X, Y) %>% 
    mutate(Y = Y + Dist) %>% 
    st_as_sf(coords =  c("X", "Y")) %>% 
    st_set_crs(PROJ)
  pt <- rotFeature(pt, POC, Bearing)
  return(pt)
}


