#### Creating data frames from JSON files ####

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
require(palmerpenguins)


#### Getting my shit and prepping my shit ####
# for those times when the LIMS Collector app fails
#Still need to add 21FLUT and it's images to the alcova folder
myDir <- "//alcova.arcc.uwyo.edu/project/SoilEcologyLab/MulFire/Collector/BackupMullenFire2021"
# for those times when alcova takes a huge shit
myDir <- "/Users/tannerhoffman/Desktop/Mullen_Fire/MulFireJSON/"

# Create object for muFire data
mulFile <- dir(myDir, pattern = ".json$", full.names = TRUE, recursive = TRUE)

# Create blank dataframes 
overDF_1 <- data.frame()
soilDF_1 <- data.frame()
imageDF_1 <- data.frame()
daubDF_1 <- data.frame()

#### Get overview Table ####
for(i in 1:length(mulFile)){
  # read in the JSON
  dat <- fromJSON(mulFile[i])
  dat$`_metadata`
  dat$siteOverview
  # build overview table
  overDF_1 <- rbind(overDF_1, data.frame(SiteID = dat$`_metadata`$siteId,
                                    SampDate = ymd_hms(dat$`_metadata`$startTime),
                                    EndDate = ymd_hms(dat$`_metadata`$endTime),
                                    Latitude = as.numeric(dat$siteOverview$location$latitude),
                                    Longitude = as.numeric(dat$siteOverview$location$longitude),
                                    AirTemp = as.numeric(dat$siteOverview$airTemperature$degrees),
                                    AirTempUnits = dat$siteOverview$airTemperature$units,
                                    Slope = as.numeric(dat$siteOverview$slope$slope),
                                    SlopeUnits = dat$siteOverview$slope$units,
                                    Aspect = ifelse(is.null(dat$siteOverview$aspect),"NA", dat$siteOverview$aspect),
                                    Wind = ifelse(is.null(dat$siteOverview$wind), "NA", dat$siteOverview$wind),
                                    CloudCover = ifelse(is.null(dat$siteOverview$cloudCover), "NA", dat$siteOverview$cloudCover),
                                    VegType = ifelse(is.null(dat$siteOverview$majorityVegetationType), "NA", dat$siteOverview$majorityVegetationType),
                                    OverviewNotes = str_replace(dat$siteOverview$overviewNotes$notes, "''", "''''"),
                                    ObserverID = paste0("[", paste0(dat$`_metadata`$observerIds, collapse=","), "]")))
}

## Clean up the table

#All Slope units, and vegetation type are the same
overDF_1$SlopeUnits <- "Percent"
overDF_1$VegType <- "Sagebrush Steppe"

# Remove time posixct columns.... may be screwing up *joins downstream
overDF_1$SampDate <- as.Date(ymd_hms(overDF_1$SampDate))
overDF_1$EndDate <- as.Date(ymd_hms(overDF_1$EndDate))



# Add lat/lon, and merge slope and aspect from initial samples to secondary samples 

overDF_1 <- overDF_1 %>%
  arrange(SampDate) %>% 
  slice_head(n = 13) %>%
  select(Slope, SiteID, Aspect, Latitude, Longitude) %>%
  mutate(Latitude = c("41.017933", "41.016464", "41.026839", "41.068605",
                      "41.034371", "41.033485", "41.047661", "41.022650",
                      "42.021726", "41.025729", "41.054962", "41.055900",
                      "41.060507"),
         Longitude = c("-106.304257", "-106.310048", "-106.388423", "-106.292984",
                       "-106.346183", "-106.342983", "-106.404309", "-106.332366",
                       "-106.326235", "-106.331602", "-106.397952", "-106.398934",
                       "-106.415366")) %>% 
  left_join(overDF_1, by = "SiteID") %>% 
  select(-c(Slope.y, Aspect.y, Latitude.y, Longitude.y)) %>% 
  rename(Slope = Slope.x, Aspect = Aspect.x, 
                      Latitude = Latitude.x, Longitude = Longitude.x) %>% 
  select(SiteID, SampDate, EndDate, Latitude, Longitude, AirTemp, 
                      AirTempUnits, Slope, SlopeUnits, Aspect, everything())  %>% 
  arrange(SampDate)



#### Get soil table ####

for(i in 1:length(mulFile)){
  # read in the JSON
  dat <- fromJSON(mulFile[i])
  soilDF_1 <- rbind(soilDF_1, data.frame(SiteID = dat$`_metadata`$siteId,
                                SampDate = ymd_hms(dat$`_metadata`$startTime),
                                EndDate = ymd_hms(dat$`_metadata`$endTime),
                                SampleType = "Soil",
                                RepId = dat$soilSamples$plotNumber,
                                Depth = as.integer(dat$soilSamples$depth$depth),
                                DepthUnit = dat$soilSamples$depth$depthUnits,
                                SoilTemp = as.numeric(dat$soilSamples$soilTemp$degrees),
                                SoilTempUnits = dat$soilSamples$soilTemp$units,
                                Barcode = dat$soilSamples$vessels$primaryWhirlPak,
                                SoilNotes = dat$soilSamples$notes,
                                AssociatedImages = dat$soilSamples$images))
}

## Clean up table

#Remove time posixct columns.... may be screwing up joins downstream
soilDF_1$SampDate <- as.Date(ymd_hms(soilDF_1$SampDate))
soilDF_1$EndDate <- as.Date(ymd_hms(soilDF_1$EndDate))

#Fix depth to 10cm.... will have note if otherwise
soilDF_1$Depth <- 10

#Remove extra observation, remove MulFire from barcode string, and arrange values chronilogically
soilDF_1 <- soilDF_1 %>% 
  slice(-69) %>% 
  arrange(by = SampDate) %>% 
  mutate_at("Barcode", str_replace, "MulFire", "")


#### Get Daubenmire table ####

for(i in 1:length(mulFile)){
  # read in the JSON
  dat <- fromJSON(mulFile[i])
  daubDF_1 <- rbind(daubDF_1, data.frame(SiteID = dat$`_metadata`$siteId,
                                  SampDate = ymd_hms(dat$`_metadata`$startTime),
                                  EndDate = ymd_hms(dat$`_metadata`$endTime), 
                                  RepId = dat$soilSamples$plotNumber,
                                  BareEarth = dat$soilSamples$daubenmire$`Bare Earth`,
                                  Litter = dat$soilSamples$daubenmire$Litter,
                                  Lichen_Moss = dat$soilSamples$daubenmire$`Lichen/Moss`,
                                  Graminoid = dat$soilSamples$daubenmire$Graminoid,
                                  Forb = dat$soilSamples$daubenmire$Forb,
                                  Shrub = dat$soilSamples$daubenmire$Shrub,
                                  Cheatgrass = dat$soilSamples$daubenmire$Cheatgrass,
                                  Rock = dat$soilSamples$daubenmire$Rock))
}

## Clean up table

#remove extra observation
daubDF_1 <- daubDF_1 %>% 
  slice(-69)


#### Get Image Table ####

for(i in 1:length(mulFile)){
  # read in the JSON
  dat <- fromJSON(mulFile[i])
  imageDF_1 <- rbind(imageDF_1, data.frame(SiteID = dat$`_metadata`$siteId,
                                  SampDate = ymd_hms(dat$`_metadata`$startTime),
                                  EndDate = ymd_hms(dat$`_metadata`$endTime),
                                  North = dat$siteOverview$cardinalImages$north,
                                  East = dat$siteOverview$cardinalImages$east,
                                  South = dat$siteOverview$cardinalImages$south,
                                  West = dat$siteOverview$cardinalImages$west))
}
  
                    
   
  
#### Import Processing Data &  Barcode Conversions ####
 ProcessDat <- read_excel("/Users/tannerhoffman/Desktop/Mullen_Fire/MulFire/MulFire_LabDat.xlsx", 
                       sheet = "Lab_Data")
 K2SO4_Conv <- read_excel("/Users/tannerhoffman/Desktop/Mullen_Fire/MulFire/MulFire_LabDat.xlsx", 
                        sheet = "K2SO4_Conversions")
 DNA_Conv <- read_excel("/Users/tannerhoffman/Desktop/Mullen_Fire/MulFire/MulFire_LabDat.xlsx", 
                          sheet = "DNA_Conversions")
  
     
#### Import Data From Forest Service ####
  ## Need to see if there is more information I can get from excel file... i.e., species in sites
  
MetaDat <- read_excel("/Users/tannerhoffman/Desktop/Mullen_Fire/MulFire/2021MullenFireVegetationSampling.xlsx", col_names = TRUE)

##Clean data
#Get only sites I'm using, and remove excess variables 
  MetaDat <- MetaDat %>% 
  filter(PlotID %in% c("18FLUT", "21FLUT","24FLTR", "11FLUT", "25FLTR", "7FLTR", "2FLTR", "13FLUT", "29FLUT", "9FLUT", "21FLTR", "4FLTR", "6FLTR")) %>% 
  select(ObjectID, SurveyDate, PlotID:NativePlantPhenologyNotes) 
# Remove one more sneaky variable
  MetaDat <- MetaDat[,-28]  

#### Create one workbook for all tables ####
  
  MulFire_Dat <- createWorkbook()
  
  
  addWorksheet(MulFire_Dat, sheetName = "Overview")
  addWorksheet(MulFire_Dat, sheetName = "Soil")
  addWorksheet(MulFire_Dat, sheetName = "Daubenmire")                               
  addWorksheet(MulFire_Dat, sheetName = "Images") 
  addWorksheet(MulFire_Dat, sheetName = "Processing_Data")
  addWorksheet(MulFire_Dat, sheetName = "DNA_Conversions")
  addWorksheet(MulFire_Dat, sheetName = "IC_Conversions") 
  
  
  writeData(MulFire_Dat, "Overview", overDF_1)
  writeData(MulFire_Dat, "Soil", soilDF_1)                               
  writeData(MulFire_Dat, "Daubenmire", daubDF_1)                                
  writeData(MulFire_Dat, "Images", imageDF_1)  
  writeData(MulFire_Dat, "Processing_Data", ProcessDat)
  writeData(MulFire_Dat, "DNA_Conversions", DNA_Conv)                               
  writeData(MulFire_Dat, "IC_Conversions", K2SO4_Conv) 
  
  saveWorkbook(MulFire_Dat, file = "MulFire_Dat.xlsx", overwrite = TRUE )  
  