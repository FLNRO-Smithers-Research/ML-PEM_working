# calculate % BEC map units per BGC and within the THLB 


library(sf)

data.dir <- "D:/PEM_DATA/AOI_2021"

bec <- st_read(file.path(data.dir, "bc_bec.gpkg"))%>%
  st_transform(3005)
bc_boundary <- st_read(file.path(data.dir, "bc_boundary.gpkg"))%>%
  st_transform(3005)

###########################################################
# 1) williams lake 
#############################################################

wl_aoi <- st_read(file.path(data.dir, "boundary_AOI.shp"))%>%
  st_transform(3005) %>%
  st_zm()

wl_thlb <-  st_read(file.path(data.dir, "wl_thlb.shp"))%>%
  st_transform(3005) %>% 
  st_zm()

wl_bec <-  st_read(file.path(data.dir, "wl_bec.shp"))%>%
  st_transform(3005)

tot_area <- st_area(wl_aoi)

# calculate area of bec for AOI 

bec_area <- wl_bec %>%
  select(MAP_LABEL) %>%
  st_union(by_feature = TRUE) %>%
  mutate(area_bec = st_area(.)) %>%
  group_by(MAP_LABEL) %>%
  summarise(area_bc = sum(area_bec)) %>%
  st_drop_geometry() %>%
  as_tibble()%>%
  mutate(AOI = "williams_lake") %>%
  mutate(area_ha = round(area_bc/10000,0),
         area_total = sum(area_ha),
         area_pc = round((area_ha / area_total *100), 1))

# calculate the % bec per BGC   
thlb_area <- sum(st_area(wl_thlb))

thlb_df <- wl_thlb %>%
  filter(Descriptio == "Warning - Do not distribute outside of FAIB.") %>%
  st_union(.)

wl_bec <- wl_bec %>%
  select(MAP_LABEL) %>%
  st_zm

thlb_bec <- st_intersection(wl_bec, thlb_df) %>%
  mutate(area_bec = st_area(.)) %>%
  group_by(MAP_LABEL) %>%
  summarise(thlb_area_bc = sum(area_bec)) %>%
  st_drop_geometry() %>%
  as_tibble()


thlb_bec_pc <- thlb_bec %>%
  mutate(thlb_area_ha = round(thlb_area_bc/10000,0),
         thlb_area_total = sum(thlb_area_ha, na.rm=TRUE),
         thlb_area_pc = round((thlb_area_ha / thlb_area_total *100), 1))

out_wl <- left_join(bec_area, thlb_bec_pc)

write.csv(out_wl, file.path(data.dir,"final","wlake_area_cals.csv"))


# write thlb, bec, aoi to single geopackage
st_write(thlb_df, file.path(data.dir, "final", "wlake_thlb.gpkg"))
st_write(wl_aoi, file.path(data.dir, "final","wlake_aoi.gpkg"))
st_write(wl_bec, file.path(data.dir, "final", "wl_bec.gpkg"))




##########################################
# 2) wetzink'wa
##########################################

wet_aoi <- st_read(file.path(data.dir, "wetzinkwa_CF.shp")) %>%
  st_transform(3005)

wet_thlb <- st_read(file.path(data.dir, "wet_thlb.shp"))

  tot_area <- st_area(aoi)
  
  # calculate area of bec for AOI 
  bec_aoi <- st_intersection(bec, wet_aoi) 
  
  bec_area <- bec_aoi %>%
    select(MAP_LABEL) %>%
    st_union(by_feature = TRUE) %>%
    mutate(area_bec = st_area(.)) %>%
    group_by(MAP_LABEL) %>%
    summarise(area_bc = sum(area_bec)) %>%
    st_drop_geometry() %>%
    as_tibble()%>%
    mutate(AOI = "wetzinkwa")%>%
    mutate(area_ha = round(area_bc/10000,0),
           area_total = sum(area_ha),
           area_pc = round((area_ha / area_total *100), 1))

  # calculate the % bec per BGC   
  thlb_area <- sum(st_area(thlb))
  
  thlb_df <- wet_thlb %>%
    filter(tsa_number == 3) %>%
    st_union(.)

  thlb_bec <- st_intersection(bec_aoi, thlb_df) %>%
    mutate(area_bec = st_area(.)) %>%
    group_by(MAP_LABEL) %>%
    summarise(thlb_area_bc = sum(area_bec)) %>%
    st_drop_geometry() %>%
    as_tibble()%>%
    mutate(thlb_area_ha = round(thlb_area_bc/10000,0),
           thlb_area_total = sum(thlb_area_ha, na.rm=TRUE),
           thlb_area_pc = round((thlb_area_ha / thlb_area_total *100), 1))
    
  # sum(thlb_bec$thlb_area_bc)
  
out_wt <- left_join(bec_area, thlb_bec) %>%
  select(-c(area_bc, thlb_area_bc))

write.csv(out_wt, file.path(data.dir,"final","wet_area_cals.csv"))

# write thlb, bec, aoi to single geopackage
st_write(thlb_df, file.path(data.dir, "final", "wt_thlb.gpkg"))
st_write(wet_aoi, file.path(data.dir, "final","wt_aoi.gpkg"))
st_write( bec_aoi, file.path(data.dir, "final", "wt_bec.gpkg"))


################################################################
# date Creek
###############################################################

date_bgc <- "D:/PEM_DATA/BEC_DevExchange_Work/DateCreek_AOI/0_raw_inputs/base_layers/dc_becV12.shp"
dc_bgc <- st_read(date_bgc)


# re calculate the bgc % with updated becV12


bec_area <- dc_bgc %>%
  dplyr::select(BGC_LABEL) %>%
  st_union(by_feature = TRUE) %>%
  mutate(area_bec = st_area(.)) %>%
  group_by(BGC_LABEL) %>%
  summarise(area_bc = sum(area_bec)) %>%
  st_drop_geometry() %>%
  as_tibble()%>%
  mutate(AOI = "date_creek")%>%
  mutate(area_ha = round(area_bc/10000,0),
         area_total = sum(area_ha),
         area_pc = round((area_ha / area_total *100), 1))





dc_aoi <- st_read(file.path(data.dir, "date_creek.shp")) %>%
  st_transform(3005)

dc_thlb <- st_read(file.path(data.dir, "Data","dc_thlb.shp")) %>%
  st_zm()

st_crs(dc_thlb) <- 3005

tot_area <- st_area(dc_aoi )

# calculate area of bec for AOI 
bec_aoi <- st_intersection(bec, dc_aoi ) 

bec_area <- bec_aoi %>%
  select(MAP_LABEL) %>%
  st_union(by_feature = TRUE) %>%
  mutate(area_bec = st_area(.)) %>%
  group_by(MAP_LABEL) %>%
  summarise(area_bc = sum(area_bec)) %>%
  st_drop_geometry() %>%
  as_tibble()%>%
  mutate(AOI = "date_creek")%>%
  mutate(area_ha = round(area_bc/10000,0),
         area_total = sum(area_ha),
         area_pc = round((area_ha / area_total *100), 1))

# calculate the % bec per BGC   
thlb_area <- sum(st_area(dc_thlb ))

thlb_df <- dc_thlb %>%
  filter(tsa_number == 12) %>%
  st_union(.)

thlb_bec <- st_intersection(bec_aoi, thlb_df) %>%
  mutate(area_bec = st_area(.)) %>%
  group_by(MAP_LABEL) %>%
  summarise(thlb_area_bc = sum(area_bec)) %>%
  st_drop_geometry() %>%
  as_tibble()%>%
  mutate(thlb_area_ha = round(thlb_area_bc/10000,0),
         thlb_area_total = sum(thlb_area_ha, na.rm=TRUE),
         thlb_area_pc = round((thlb_area_ha / thlb_area_total *100), 1))

# sum(thlb_bec$thlb_area_bc)

out_dc <- left_join(bec_area, thlb_bec) %>%
  select(-c(area_bc, thlb_area_bc))

write.csv(out_dc, file.path(data.dir,"final","dc_area_cals.csv"))


# write thlb, bec, aoi to single geopackage
st_write(thlb_df , file.path(data.dir, "final", "dc_thlb.gpkg"))
st_write(dc_aoi, file.path(data.dir, "final","dc_aoi.gpkg"))
st_write(bec_aoi, file.path(data.dir, "final", "dc_bec.gpkg"))


################################################################
# surrounding areas 
################################################################

dcsa_aoi <- st_read(file.path(data.dir,"Data", "TBA_2018_LiDAR.shp"))

tot_area <- st_area(bec_aoi)

# calculate area of bec for AOI 
bec_aoi <- st_intersection(bec, dcsa_aoi ) 

bec_area <- bec_aoi %>%
  select(MAP_LABEL) %>%
  st_union(by_feature = TRUE) %>%
  mutate(area_bec = st_area(.)) %>%
  group_by(MAP_LABEL) %>%
  summarise(area_bc = sum(area_bec)) %>%
  st_drop_geometry() %>%
  as_tibble()%>%
  mutate(AOI = "date_creek_surrounding")%>%
  mutate(area_ha = round(area_bc/10000,0),
         area_total = sum(area_ha),
         area_pc = round((area_ha / area_total *100), 1))

out_dcsa <- bec_area %>% 
  select(-c(area_bc))

write.csv(out_dcsa, file.path(data.dir,"final","dcsa_area_cals.csv"))


# write thlb, bec, aoi to single geopackage
st_write(dcsa_aoi, file.path(data.dir, "final","dcsa_aoi.gpkg"))
st_write(bec_aoi, file.path(data.dir, "final", "dcsa_bec.gpkg"))






##################################################################
# Sunshine Coast 
##################################################################

sc_aoi <- st_read(file.path(data.dir,"2020_lidar_SSC","sunshine_coast_lidar_AOI_clean2.shp"))  

sc_thlb <- st_read(file.path(data.dir, "Data", "se_coast_thlb.shp")) %>%
    st_transform(3005)

#tot_area <- st_area(aoi)

# calculate area of bec for AOI 
bec_aoi <- st_intersection(bec, sc_aoi) 
bec_aoi <- bec_aoi %>%
  st_zm()

plot(st_geometry(bec_aoi))

bec_area <- bec_aoi %>%
  select(MAP_LABEL) %>%
  st_union(by_feature = TRUE) %>%
  mutate(area_bec = st_area(.)) %>%
  group_by(MAP_LABEL) %>%
  summarise(area_bc = sum(area_bec)) %>%
  st_drop_geometry() %>%
  as_tibble()%>%
  mutate(AOI = "southeast") %>%
  mutate(area_ha = round(area_bc/10000,0),
         area_total = sum(area_ha),
         area_pc = round((area_ha / area_total *100), 1))

# calculate the % bec per BGC   
#thlb_area <- sum(st_area(thlb))

# union and crop thlb
thlb_df <- sc_thlb %>%
  filter(Descriptio == "Warning - Do not distribute outside of FAIB.") %>%
  st_union(.)

# clip 

sc_thlb2 <- st_intersection(thlb_df, sc_aoi)
sc_thlb <- sc_thlb2 %>%
  st_zm()


thlb_bec <- st_intersection(bec_aoi, sc_thlb) %>%
  mutate(area_bec = st_area(.)) %>%
  group_by(MAP_LABEL) %>%
  summarise(thlb_area_bc = sum(area_bec)) %>%
  st_drop_geometry() %>%
  as_tibble()

thlb_bec1 <- thlb_bec %>%
  mutate(thlb_area_ha = round(thlb_area_bc/10000,0),
         thlb_area_total = sum(thlb_area_ha, na.rm=TRUE),
         thlb_area_pc = round((thlb_area_ha / thlb_area_total *100), 1))

# sum(thlb_bec$thlb_area_bc)

out_sc <- left_join(bec_area, thlb_bec1)

write.csv(out_sc, file.path(data.dir,"final","sc_area_cals.csv"))

# write thlb, bec, aoi to single geopackage
st_write(dcsa_aoi, file.path(data.dir, "final","sc_aoi.gpkg"))
st_write(bec_aoi, file.path(data.dir, "final", "sc_bec.gpkg"))
st_write(sc_thlb, file.path(data.dir, "final", "sc_thlb.gpkg"))








###########################################################
# outer SC lidar
#############################################################


library(sf)

data.dir <- "D:/PEM_DATA/AOI_2021"

bec <- st_read(file.path(data.dir, "bc_bec.gpkg"))%>%
  st_transform(3005)
aoi <- st_read(file.path(data.dir, "final", "coast_lidar_aoi.gpkg"))%>%
  st_transform(3005)%>%
  st_zm()


# calculate area of bec for AOI 
bec_aoi <- st_intersection(bec, aoi) 
bec_aoi <- bec_aoi %>%
  st_zm()

st_write(becc)




################################################################
#Update Boundry 
#################################################################

# Boundary 

bound_thlb <- st_read(file.path(data.dir, "bound_thlb.shp"))%>%
  st_transform(3005) %>%
  st_zm()

bound.dir <- "D:/PEM_DATA/BEC_DevExchange_Work/BoundaryTSA_AOI/0_raw_inputs/base_layers"

bound_aoi<- st_read(file.path(bound.dir, list.files(bound.dir)[55])) %>%
  st_zm()

b_bec <- st_read(file.path(bound.dir,"Boundary_becV12.shp"))%>%
  st_zm()


bound_thlb <- st_read(file.path(bound.dir, "Boundary_thlb_19_21.shp")) %>%
  st_transform(3005) %>%
  st_zm()


#st_write(b_bec, file.path(data.dir, "final", "bound_becv12.gpkg"))

library(dplyr)

bec_area <- b_bec %>%
  select(BGC_LABEL) %>%
  st_union(by_feature = TRUE) %>%
  mutate(area_bec = st_area(.)) %>%
  group_by(BGC_LABEL) %>%
  summarise(area_bc = sum(area_bec)) %>%
  st_drop_geometry() %>%
  as_tibble()%>%
  mutate(AOI = "Boundary") %>%
  mutate(area_ha = round(area_bc/10000,0),
         area_total = sum(area_ha),
         area_pc = round((area_ha / area_total *100), 1))
# bec = see D drive
# aoi = see D drive 
out_bound <-bec_area
write.csv(out_bound, file.path(data.dir,"final","bound_area_cals.csv"))

# updte with thlb calcs 



# union and crop thlb
thlb_df <- bound_thlb %>%
  filter(Descriptio == "Warning - Do not distribute outside of FAIB.") %>%
  st_union(.)

# clip 

bound_thlb2 <- st_intersection(thlb_df, bound_aoi)
bound_thlb2 <- bound_thlb2 %>%
  st_zm()


thlb_bec <- st_intersection(bound_aoi, bound_thlb2 ) %>%
  mutate(area_bec = st_area(.)) %>%
  group_by(MAP_LABEL) %>%
  summarise(thlb_area_bc = sum(area_bec)) %>%
  st_drop_geometry() %>%
  as_tibble()

thlb_bec1 <- thlb_bec %>%
  mutate(thlb_area_ha = round(thlb_area_bc/10000,0),
         thlb_area_total = sum(thlb_area_ha, na.rm=TRUE),
         thlb_area_pc = round((thlb_area_ha / thlb_area_total *100), 1))

# sum(thlb_bec$thlb_area_bc)

out_sc <- left_join(bec_area, thlb_bec1)




# eagle hills 
eh_aoi <- st_read(file.path(data.dir, "Eagle_hills_AOI.shp")) %>%
  st_transform(3005)



