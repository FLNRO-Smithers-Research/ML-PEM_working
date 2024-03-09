## PEM Post Processing workflow

# Currently based on point collection (see previous github versions for alernative linear method) 
# Thanks to Colin for the data read in and conversion section # 

# SUMMARY : Workflow 
# 1) read in gpx file 
# 2) convert to line segments and error check (Can you read geopackages into Avenza?)
# 3) generate training point set to feed back into model 
#   - output = csv with point info to combine with othwer training point data
# 4) use linear to assess model accuracy? : see 06_AccuracyAsses.R


# read in the functions.R scrips 
source("D:/PEM_DATA/RScripts/Github/PEM_Research_BC/01_Functions.R")
source("D:/PEM_DATA/RScripts/Github/PEM_Research_BC/01_Header.R")

## ## set up location of drives to input and output
setwd("D:/PEM_DATA/")#check the home directory  # set up work directory 

#field.data <- "Temp_testing/Field_data_collection_2019" # ponint to folder where the avenza maps are exported to 
GPSdat <- 'Temp_testing/Field_data_collection_2019/201906_Perkins/201906_Perkins/2019-Planning_UNBCTreatments2.gpx'

## Load layers from GPS
GPSpoints   <- st_read(dsn = GPSdat, layer = "waypoints")
GPStracklog <- st_read(dsn = GPSdat, layer = "tracks")
GPSRoute    <- st_read(dsn = GPSdat, layer = "routes")

# May need to be adjusted for different GPS (Test with Deception GPX files )
GPSpoints <- GPSpoints %>%
  mutate(`desc` = str_replace(`desc`, pattern = "\\n", replacement = ":")) %>%  #There are 2 attibutes special characters that seperate the data needed \n and :
  separate(`desc`, into = letters[seq(1,20)], sep = ":")  %>% # data in spread in to seperate columns
  dplyr::select(name, time, f) %>%   ## select the fields needed
  rename(SiteSeries = f)

## Sample data fix  -- THIS DATA ONLY -------------------------
GPSpoints$SiteSeries[9] <- "01"
GPSpoints$name <- as.character(GPSpoints$name)
GPSpoints$name[3] <- "T1"

GPSpoints2 <- GPSpoints %>% arrange(time) %>%
  rowid_to_column("ID") # ID is needed table manipulation below

lines <- cbind(GPSpoints2, st_coordinates(GPSpoints2)) %>% as_tibble() %>%
  mutate(Xend = lead(X),
         Yend = lead(Y)) %>%  # collect the coordinates of the next point
  filter(!is.na(Yend)) %>%  # drops the last row (start point with no end)
  unite(Start, X, Y, sep = " ") %>%
  unite(End, Xend, Yend, sep = " ") %>%
  gather(key = "Start_End", value = coords, Start, End) %>% # converts to long table format
  dplyr::select(-geometry) %>%  # old point geometry is dropped
  separate(coords, into = c("X", "Y"), sep = " ") %>%       # coordinate pairs are seperated
  st_as_sf(coords = c("X", "Y"))  %>% # geometry is created for each point (start and end)
  group_by(ID) %>% # with the point ID created above it is now possible to cast multiple points to a single geometry
  summarise() %>%
  st_cast("LINESTRING")

tmp <- as_tibble(GPSpoints2) %>% dplyr::select(-geometry)
lines <- left_join(lines, tmp, by= "ID")
st_crs(lines)<- 4326

# Make a pretty map 
GPSRoute$N <- as.factor("Planned Route")
GPStracklog$N <- as.factor("Tracklog")
GPSpoints$N <- as.factor("Site Series Changes")

TTmap2 <- tm_shape(lines, unit = "m") + tm_lines(col = "SiteSeries", lwd = 10, palette = "Set1") +
  tm_shape(GPSpoints) +
  tm_dots(col = "N", size = .5, palette = "orange3", title = "" ) +
  tm_text("SiteSeries", auto.placement = TRUE) +
  # tm_scale_bar() +
  tm_layout(main.title = "Point Site-Series calls converted to lines",   legend.outside = TRUE, frame = FALSE, bg.color = "grey90")

TTmap2

################################################
## error check the length vs the tracklog 

track.length <- as.numeric(st_length(GPStracklog))
line.length <- lines %>% 
   mutate(length = st_length(.)) %>%
   summarise(sum.m = sum(length)) %>%
   mutate(sum.m = as.numeric(sum.m))%>%
   pull(sum.m) 

error.margin <- line.length + line.length/10 # 10% error margin 
  
if(track.length < error.margin) { print("Track length greater than transect lines by 10%")} else {
  print("check track")
} 

# Another option is to calculate trigonometric distance between line and point at 90 degree angle cosin?


###################################################
# 3) Generate training points from line segments

# convert to polygon 
lines.buf <- st_transform(lines,3005) %>%
  st_buffer(dist = 0.5,endCapStyle = "FLAT") # might cause problems with overlap of buffered segments

#pp = ggplot() + geom_sf(data = lines.buf)

# select points within the polygons (stratified)
pts2m = st_poly_sample(lines.buf,1,min_dist = 2) # 2 meters apart
pts10m = st_poly_sample(lines.buf,5,min_dist = 10) # 10 mts

# or 

# select points within entire triangle 
pts <- st_sample(lines.buf,50,add = TRUE) # number of random points created 
rpts <- lines %>% st_transform(.,3005) %>%  # also works on linear features
        st_sample(.,50,add = TRUE) 

# plot to check output
p2 = ggplot() + 
  geom_sf(data = lines.buf, fill = NA,colour = "red") + 
  geom_sf(data = pts2m, colour = "blue")+
  geom_sf(data = pts10m, colour = "green") + 
  geom_sf(data = pts, colour = "red") 
p2

# write out files 
# 1) point files: 
out.df<- as.data.frame(cbind(pts2m,st_coordinates(pts2m))) %>%
  dplyr::select(-"geometry")

output = "Data/Field_data/Dec_year2/"

# addd if file exists.... open and add to file. 

write.csv(out.df, paste(output,"RandomPts_Triangles.csv", sep = ""))

# 2) line segments as shapefile ? for accuracy accessment? 
lines<- ToAlbers(lines)

st_write(lines,paste(output,"S2_line.shp",sep = ""))
st_write(lines.buf,paste(output,"S2_poly.shp",sep = ""))
