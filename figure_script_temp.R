# rough script for exporting plots for talks 

library(dplyr)
library(foreach)
library(stringr)
library(tidyverse)

# example data output 

outDir <-  "D:/PEM_DATA/BEC_DevExchange_Work/Deception_AOI/3_maps_analysis/models/forest/fore_mu_bgc/83"

acc_files <- list.files(file.path(outDir), full.name = TRUE, recursive = T, pattern = ".csv")

f1 <- read.csv(acc_files[1]) %>% mutate(file.name = acc_files[1])
f2 <- read.csv(acc_files[2])%>% mutate(file.name = acc_files[2])
f3 <- read.csv(acc_files[3])%>% mutate(file.name = acc_files[3])

out <- bind_rows(f1, f2, f3)


bgcs2 <- "ESSFmc"

bsRes2 <- out %>%  filter(acc_type == "test_estimate") %>% filter(str_detect(target, bgcs2))
bsRes2$slice <- as.factor(bsRes2$slice)#mutate(across(where(is.numeric), ~ replace_na(.,0)))


bsRes2_all <- bsRes2 %>% 
  group_by(slice) %>%
  #mutate(aspat_p = min((targ.ratio/slice_sum),(map.total/trans.total))*100) %>%
  mutate(aspat_p_acc = aspatial_acc,
         aspat_p_meanacc = aspatial_meanacc,
         spat_p_acc = (sum(spat_p)/trans.sum) *100,
         spat_pa_acc = (sum(spat_pa)/trans.sum) *100,
         spat_fp_tot = (sum(spat_fp)/trans.sum) *100,
         spat_fpa_tot = (sum(spat_fpa)/trans.sum) *100) %>%
  dplyr::select(c(slice, aspat_p_meanacc, aspat_p_acc, spat_p_acc, spat_pa_acc, spat_fp_tot, spat_fpa_tot, mcc)) %>% #,recall
  pivot_longer(cols = where(is.numeric), names_to = "accuracy_type", values_to = "value") %>%
  distinct()%>%
  mutate(type = case_when(
    accuracy_type %in% c("aspat_p_meanacc", "aspat_p_acc") ~ "2_aspatial",
    accuracy_type %in% c("spat_p_acc", "mcc") ~ "0_spatial",
    accuracy_type %in% c("spat_pa_acc", "spat_fp_tot", "spat_fpa_tot") ~ "1_fuzzy_spatial")) %>% 
  filter(!is.na(accuracy_type)) %>% filter(!accuracy_type == "slice")
#   )) %>%
# filter(!is.na(type)) %>%
# mutate(accuracy_type = ifelse(accuracy_type == "spat_p_acc", "acc_spatial", accuracy_type))

bsRes2_all$accuracy_type <- as.factor(bsRes2_all$accuracy_type) %>% factor(levels = c("spat_p_acc", "mcc", "spat_pa_acc", "spat_fp_tot", "spat_fpa_tot", "aspat_p_acc", "aspat_p_meanacc"))



bsRes2_all_edit <- bsRes2_all %>%
  mutate(acc_type = case_when(
  accuracy_type %in% c("aspat_p_meanacc") ~ "Overall mean accuracy",
  accuracy_type %in% c("aspat_p_acc") ~ "Mapunit mean accuracy",
  accuracy_type %in% c("spat_p_acc") ~ "Spatial accuracy",
  accuracy_type %in% c("mcc") ~ "Matthews correlation coeff.",
  accuracy_type %in% c("spat_pa_acc") ~ "Spatial prime/alt accuracy",
  accuracy_type %in% c("spat_pa_acc") ~ "Fuzzy prime accuracy", 
  accuracy_type %in% c("spat_fp_tot") ~ "Fuzzy prime accuracy", 
  accuracy_type %in% c("spat_fpa_tot") ~ "Fuzzy prime/alt accuracy")) 
  


overall_acc1 <- ggplot(aes(y = value, x = acc_type, fill = accuracy_type), data = bsRes2_all_edit) + 
  geom_boxplot(show.legend = FALSE) +
  #geom_jitter() +
  #scale_fill_brewer(type = "qual", guide = guide_legend(reverse = FALSE)) +
  facet_grid(~type, labeller = labeller(type = 
                                          c("0_spatial" = "spatial",
                                            "1_fuzzy_spatial" = "spatial fuzzy",
                                            "2_aspatial" = "aspatial")), scales = "free_x")+
  #facet_wrap(~acc_type)+
  #geom_jitter(position=position_jitter(width=.1), colour = "grey", alpha = 0.8) + 
  geom_hline(yintercept = 65,linetype ="dashed", color = "red") + 
  ggtitle(paste0("Overall map accuracy_", bgcs2)) + 
  theme(axis.text.x = element_text(angle = 90)) +
  ylab("Accuracy") + xlab("")+
  ylim(-0.05, 100) 

overall_acc1 

ggsave(paste0("./Deception_AOI/results/", bgcs2, "_AccuracyMetrics_boxplot.jpg"))





# Plot 2 - showing accuracy mean and accuracy overall 


head(bsRes2) 











# PLot 2: Spatial Accuracy plots 

bsRes2_all_spatial <- bsRes2_all %>%
  dplyr::filter(accuracy_type == "spat_p_acc")


overall_acc <- ggplot(aes(y = value, x = slice), data = bsRes2_all_spatial) + 
  geom_point()  + 
  geom_hline(yintercept = 65,linetype ="dashed", color = "red") + 
  ggtitle(paste0("Spatial_acc_", bgcs2, " by iteration")) + 
  theme(axis.text.x = element_text(angle = 90)) +
  xlab("iteration") + ylab("Accuracy") + 
  ylim(-0.05, 100) 
#overall_acc

# Plot 3: Aspatial Accuracy plots 

bsRes2_all_spatial <- bsRes2_all %>%
  dplyr::filter(accuracy_type == "aspat_p_meanacc")

overall_acc2 <- ggplot(aes(y = value, x = slice), data = bsRes2_all_spatial) + 
  geom_point()  + 
  geom_hline(yintercept = 65,linetype ="dashed", color = "red") + 
  ggtitle(paste0("Aspat_p_meanacc_", bgcs2, " by iteration")) + 
  theme(axis.text.x = element_text(angle = 90)) +
  xlab("iteration") + ylab("Accuracy") + 
  ylim(-0.05, 100) 

#overall_acc2
# Plot 4: Aspatial Accuracy plots 

bsRes2_all_spatial <- bsRes2_all %>%
  dplyr::filter(accuracy_type == "aspat_p_acc")

overall_acc3 <- ggplot(aes(y = value, x = slice), data = bsRes2_all_spatial) + 
  geom_point()  + 
  geom_hline(yintercept = 65,linetype ="dashed", color = "red") + 
  ggtitle(paste0("Aspat_p_acc_", bgcs2, " by iteration")) + 
  theme(axis.text.x = element_text(angle = 90)) +
  xlab("Mapunit") + ylab("Accuracy") + 
  ylim(-0.05, 100) 
#overall_acc3
