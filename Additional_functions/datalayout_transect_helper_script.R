library(sf)

trans_layout <- "D:\\PEM_DATA\\BEC_DevExchange_Work\\Deception_AOI\\2_sample_design\\stage1_StudyDesign\\transect_layout\\"

list.files(trans_layout)

layout <- st_read(file.path(trans_layout, "transect_layout_deception.gpkg"), 
                    layer = "ESSF")

layout <- layout %>%
  st_transform(3005)

layout<- st_buffer(layout, 10)


data_files <- "D:\\PEM_DATA\\BEC_DevExchange_Work\\Deception_AOI\\1_map_inputs\\trainingData"
data <- st_read(file.path(data_files, "transect1_30m_pts_att.gpkg"))
                  
samples <- data %>% 
 st_join(layout) %>% 
  dplyr::select(c(id, ID)) %>%
  filter(stringr::str_detect(id, ".cLHS$")) %>% 
  st_drop_geometry() %>%
  distinct(ID) %>%
  arrange(ID)

samples

#ESSFmc_1.1_1_cLHS
#2   ESSFmc_1.2_2_cLHS
#3   ESSFmc_1.3_3_cLHS
#4   ESSFmc_1.4_4_cLHS
#5   ESSFmc_1.5_5_cLHS
#6   ESSFmc_2.3_8_cLHS
#7   ESSFmc_2.4_9_cLHS
#8  ESSFmc_3.1_11_cLHS
#9  ESSFmc_3.2_12_cLHS
#10 ESSFmc_3.4_14_cLHS
#11 ESSFmc_3.5_15_cLHS
#12 ESSFmc_4.1_16_cLHS
#13 ESSFmc_4.2_17_cLHS
#14 ESSFmc_4.3_18_cLHS
#15 ESSFmc_5.1_21_cLHS
#16 ESSFmc_5.2_22_cLHS
#17 ESSFmc_5.3_23_cLHS


trans_pts <- "D:\\PEM_DATA\\BEC_DevExchange_Work\\Deception_AOI\\2_sample_design\\stage1_StudyDesign\\transect_layout\\"

list.files(trans_layout)

clhs <- st_read(file.path(trans_pts, "ESSFmc_allPointPairs_2019-08-18.shp"))

clhs <- clhs %>%
  filter(ID %in% samples$ID)

st_write(clhs, "ESSF_samples.gpkg")

