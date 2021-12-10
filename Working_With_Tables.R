#### Working With Tables ####

#### Packages ####

require(tidyverse)
require(odbc)
require(jsonlite)
require(lubridate)
require(sf)
require(readxl)
require(httr)
require(RSQLite)
require(openxlsx)
require(here)
require(writexl)
require(ggplot2)
require(gridExtra)
require(data.table)
require(cowplot)


#### Load data from spreadsheets ####

# Path
# Will Need to modify path depending on current working directory

path <- "/Users/tannerhoffman/Desktop/Mullen_Fire/MulFire/MulFire_Dat.xlsx"

# Processing Data and Barcode Conversions

Overview <-  read_excel(path, sheet = "Overview")
Soil <- read_excel(path, sheet = "Soil")
Daubenmire <- read_excel(path, sheet = "Daubenmire")
Images <- read_excel(path, sheet = "Images")
Processing_Data <- read_excel(path, sheet = "Processing_Data")
DNA_Conversions <- read_excel(path, sheet = "DNA_Conversions")
IC_Conversions <- read_excel(path, sheet = "IC_Conversions")

BlackENZSPR2021 <- read_excel(path, sheet = "BlackENZSPR2021")
BlackENZSPR2021$SAMPLE_DESCR <- paste("0", BlackENZSPR2021$SAMPLE_DESCR, sep = "") 

ClearENZSPR2021 <- read_excel(path, sheet = "ClearENZSPR2021")
ClearENZSPR2021$SAMPLE_DESCR <- paste("0", ClearENZSPR2021$SAMPLE_DESCR, sep = "")

ICDat2021 <- read_excel(path, sheet = "ICDat_2021") %>% 
  mutate_at("Barcode", str_replace, "MulFire", "")
#Forest Service Floristic Data

PathFlor <- "/Users/tannerhoffman/Desktop/Mullen_Fire/MulFire/2021MullenFireVegetationSampling.xlsx" 

Floristic_Meta <- read_excel(PathFlor, col_names = TRUE, sheet = "_2021_Mullen_Fire_Vegetatio_0")
Floristic_NWVeg <- read_excel(PathFlor, col_names = TRUE, sheet = "tbl_NWVeg_1")
Floristic_SEVeg <- read_excel(PathFlor, col_names = TRUE, sheet = "tbl_SEVeg_2")
Floristic_Inventory <- read_excel(PathFlor, col_names = TRUE, sheet = "tbl_FloristicInventory_3")

#### Get Floristic data for My Sites ####

Floristic_Meta <- Floristic_Meta %>% 
filter(PlotID %in% c("18FLUT", "21FLUT","24FLTR", "11FLUT", "25FLTR", 
                     "7FLTR", "2FLTR", "13FLUT", "29FLUT", "9FLUT", "21FLTR", "4FLTR", "6FLTR")) 

NW_Veg_Descr <- Floristic_Meta %>% 
  left_join(Floristic_NWVeg, by = c("GlobalID" = "ParentGlobalID")) %>% 
  select(Plant, Percent_Cover, GlobalID, PlotID) %>% 
  mutate(Percent_Cover = case_when(Percent_Cover %in% "T" ~ "Trace",
                                   TRUE ~ Percent_Cover))

SE_Veg_Descr <- Floristic_Meta %>%
  left_join(Floristic_SEVeg, by = c("GlobalID" = "ParentGlobalID")) %>% 
  select(Plant, Percent_Cover, GlobalID, PlotID) %>% 
  mutate(Percent_Cover = case_when(Percent_Cover %in% "T" ~ "Trace",
                                 TRUE ~ Percent_Cover))

          



#### Checking availiable nutrient####


#Spring Samples
tmp <- Soil %>% 
  left_join(IC_Conversions, by = c("Barcode" = "SampleID")) %>% 
  left_join(ICDat2021, by = c("K2SO4_Barcode" = "Barcode")) %>% 
  left_join(Overview,  by = c("SiteID", "SampDate")) %>% 
  left_join(Processing_Data, by = c("Barcode" = "ID")) %>%
  filter(str_detect(SampDate, "2021-06-"))

#Fall Samples
tmp_2 <- Soil %>% 
  left_join(IC_Conversions, by = c("Barcode" = "SampleID")) %>% 
  left_join(ICDat2021, by = c("K2SO4_Barcode" = "Barcode")) %>% 
  left_join(Overview,  by = c("SiteID", "SampDate")) %>% 
  left_join(Processing_Data, by = c("Barcode" = "ID")) %>%
  filter(str_detect(SampDate, "2021-09-"))


 #### Enzymes ####


## Hydrolytic Enzymes


# Normalize data...
tmp <- transform(BlackENZSPR2021, ENZ_ACT = (ENZ_ACT - min(ENZ_ACT)) / (max(ENZ_ACT) - min(ENZ_ACT)))

# Graph Enzyme activity by general substrate category
rm(tmp)
tmp <- BlackENZSPR2021 %>% 
  left_join(Soil, by = c("SAMPLE_DESCR" = "Barcode")) %>% 
  left_join(Processing_Data, by = c("SAMPLE_DESCR" = "ID")) %>% 
  mutate(Sub_General = case_when(SUBSTRATE %in% c("AG") ~ "Starch",
                                 SUBSTRATE %in% c("PHOS") ~ "P-cycling",
                                 SUBSTRATE %in% c("SUL") ~ "S-cycling",
                                 SUBSTRATE %in% c("LAP", "NAG") ~ "N-cycling",
                                 SUBSTRATE %in% c("BX") ~ "Hemicellulose",
                                 SUBSTRATE %in% c("BG", "CBH") ~ "Cellulose")) %>% 
  left_join(Overview, by = "SiteID") 



# Create plots for each enzyme by burn severity 


ggplot(tmp, aes(Burn_Severity, ENZ_ACT, color = SUBSTRATE)) +
  geom_boxplot() +
  facet_wrap(~SUBSTRATE, scales = "free")


## Oxidative Enzymes

tmp <- ClearENZSPR2021 %>% 
  left_join(Soil, by = c("SAMPLE_DESCR" = "Barcode")) %>% 
  left_join(Processing_Data, by = c("SAMPLE_DESCR" = "ID")) %>% 
  mutate(Sub_General = case_when(SUBSTRATE %in% c("Phenox", "Perox") ~ "Lignin")) %>% 
  left_join(Overview, by = "SiteID") %>%
  select(-c("ENZ_ACT1")) %>% 
  mutate(ENZ_ACT = ENZ_ACT2)


ggplot(tmp, aes(Burn_Severity, ENZ_ACT, color = SUBSTRATE)) +
  geom_boxplot() +
  facet_wrap(~SUBSTRATE, scales = "free")









             