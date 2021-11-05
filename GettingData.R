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
myDir <- "//alcova.arcc.uwyo.edu/project/SoilEcologyLab/MulFire/Collector/BackupMullenFire2021"
# Used this directory from home.... issues accesing alcova through r
myDir <- "/Users/tannerhoffman/Desktop/BackupMullenFire2021"

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
# remove data that is not mine.... I fixed file on locally so I won't need to do this on my mac
overDF_1 <- overDF_1 %>% 
  slice(-c(44:51), -c(14:29), -c(1,2)) %>% 
  slice(-c(14:29)) %>% 
  slice(-c(1,2))



#### get soil table ####

for(i in 1:length(mulFile)){
  # read in the JSON
  dat <- fromJSON(mulFile[i])
  soilDF_1 <- rbind(soilDF_1, data.frame(SiteID = dat$`_metadata`$siteId,
                                SampDate = ymd_hms(dat$`_metadata`$startTime),
                                EndDate = ymd_hms(dat$`_metadata`$endTime),
                                SampleType = "Soil",
                                RepId = dat$soilSamples$plotNumber,
                                Depth = as.numeric(dat$soilSamples$depth$depth),
                                DepthUnit = dat$soilSamples$depth$depthUnits,
                                SoilTemp = as.numeric(dat$soilSamples$soilTemp$degrees),
                                SoilTempUnits = dat$soilSamples$soilTemp$units,
                                Barcode = dat$soilSamples$vessels$primaryWhirlPak,
                                SoilNotes = dat$soilSamples$notes,
                                AssociatedImages = dat$soilSamples$images))
}

# Arrange values chronologically 
soilDF_1 <- soilDF_1 %>% 
  arrange(by = SampDate)

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

#### Poop everything into excel for to show linda ####

MulFire_Dat <- createWorkbook()
  
  addWorksheet(MulFire_Dat, sheetName = "Overview")
  addWorksheet(MulFire_Dat, sheetName = "Soil")
  addWorksheet(MulFire_Dat, sheetName = "Daubenmire")                               
  addWorksheet(MulFire_Dat, sheetName = "Images") 
  
  writeData(MulFire_Dat, "Overview", overDF_1)
  writeData(MulFire_Dat, "Soil", soilDF_1)                               
  writeData(MulFire_Dat, "Daubenmire", daubDF_1)                                
  writeData(MulFire_Dat, "Images", imageDF_1)  
  
  saveWorkbook(MulFire_Dat, file = "MulFire_Dat.xlsx", overwrite = TRUE )
  
  #### Ammend Data ####
  # All done in excell... need to do in r to keep track of workflow and changes. 
  
  ## 17th & 18th June 2021 
  # 21FLTR = 21FLUT
  # 18FLTR = 18FLUT
  # 18FLUT = 11FLUT
  
  ## Removed extra row from Septemeber 21FLUT data and switched South and West soil Samples + daubenmire
  # ATTENTION!!!!!  South and west not switched in image dataframe #
  
 #### Bring Cleaned data back to r: Organize #### 
  
  # retrieve data from excel 
  MulFire_Overview <- data.frame(read_excel("/Users/tannerhoffman/Desktop/MulFire_Dat.xlsx", sheet = "Overview"))
  
  MulFire_Soil <- data.frame(read_excel("/Users/tannerhoffman/Desktop/MulFire_Dat.xlsx", sheet = "Soil"))
  
  MulFire_Daubenmire <- data.frame(read_excel("/Users/tannerhoffman/Desktop/MulFire_Dat.xlsx", sheet = "Daubenmire"))        
  
  MulFire_Images <- data.frame(read_excel("/Users/tannerhoffman/Desktop/MulFire_Dat.xlsx", sheet = "Images"))                     
      
   #Order dataframes to all align
  
  MulFire_Overview <- MulFire_Overview %>% 
    arrange(by = SampDate)
  #Slope units should have all been measured as percent... ammend this
  MulFire_Overview$SlopeUnits <- "Percent"
  
  #All Sites should be logged as having sagebrush steppe vegetation
  MulFire_Overview$VegType <- "Sagebrush Steppe"
  
  MulFire_Daubenmire <- MulFire_Daubenmire %>% 
    arrange(by = SampDate)
  
  MulFire_Images <- MulFire_Images %>% 
    arrange(by = SampDate)
  
  #### Create Lookup tables ####
  
  # Create Master Site table
  BurnSev <- c("Severe", "Moderate", "Unburned", "Severe", "Light", "Light", "Severe" ,"Severe" ,"Moderate" ,"Severe" ,"Severe" ,"Severe" ,"Severe")
  
  MulFire_Master <- MulFire_Overview[1:13,c(1:5, 8:10, 13:15)]  %>% 
    mutate(BurnSeverity = BurnSev)
  str(MulFire_Master)
  
  #Create Soil Sample Lookup Table

  #create table with barcodes + burn severity
 tmp <- left_join( x = MulFire_Soil, y = MulFire_Master, by = "SiteID")
  
 # trim the fat
 SoilSamples_LuTable <- tmp %>% 
    select(-c("SampDate.y", "EndDate.y", "SampleType",))
  
 #Make all depths 10.... not sure why it changed this
 SoilSamples_LuTable$Depth <- 10
 
 # Make an Image lookup table 
      tmp <- MulFire_Soil %>% 
        left_join(y = MulFire_Images, by = "SiteID", "SampDate", "EndDate")
      Image_LuTable <- tmp[, c(1:15, 24)]
 tmp <- left_join(MulFire_Soil, MulFire_Images, by = c("SiteID", "SampDate", "EndDate"))
  ## Sending back to excel to add lat/lon
  
  
  
  
  #### Find Missing Site!!!!!! #### 
                  
                                  
                                  