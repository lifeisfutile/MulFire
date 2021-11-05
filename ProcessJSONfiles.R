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
install.packages('odbc')
install.packages('here')
# This script is for parsing out data collected in the field by the Field Collector App. Data are stored within the UWYO LIMS database as JSON. This particular script is focused on MicroPlot Experiments by Gordon Custer.

#### Useful Functions ####
apiToDF<- function(data, cols, datatypes){
  # Coerce the list of lists into an all character data.frame
  df<- data.frame(matrix(sapply(data, FUN = function(x){as.character(x)}), nrow = length(data), byrow = TRUE), stringsAsFactors = FALSE)
  # Apply the column names
  names(df)<- cols
  # Coerce the columns to the expected data type given DB Type
  for(i in 1:length(datatypes)){
    if(datatypes[i] %in% c("int", "bigint", "bit", "smallint", "tinyint")){
      df[, i]<- as.integer(df[, i])
    }
    if(datatypes[i] %in% c("float", "decimal", "money", "numeric", "real", "smallmoney")) {
      df[, i]<- as.numeric(df[, i])
    }
    if(datatypes[i] %in% c("datetime", "datatime2", "date")) {
      df[, i]<- as.POSIXct(df[, i], format = "%Y-%m-%dT%H:%M:%S")
    }
  }
  return(df)
}

#### RemoteDB API Database connections ####
# Get the table schema so we know what columns are available
req <- POST(
  "https://datacorral.uwyo.edu/RemoteDb/tableschema",
  body = list(Token="SWLlwIQsvILtpJWlSd44ByktrvlR1z8taoJPaQ3q",
              Database = "LIM_Staging",
              Table = "LIMtblCompletedDataSheets"),
  encode = "json"
)

# extract the JSON
cols<- content(req, encoding = "json")
# View all metadata if you like
cols$metadata
# Store the column names and types for later use
dbNames<- sapply(cols$sqlSchema$columns, FUN = function(x) x$COLUMN_NAME)
dbType<- sapply(cols$sqlSchema$columns, FUN = function(x) x$DATA_TYPE)

# Get the field collected data using the API, filtering for Datasheet ID = 3 for MicroPlot
fldData<- POST(
  "https://datacorral.uwyo.edu/RemoteDb/querytable",
  body = list(Token = "SWLlwIQsvILtpJWlSd44ByktrvlR1z8taoJPaQ3q",
              Database = "LIM_Staging",
              Table = "LIMtblCompletedDataSheets", 
              Filters = list(list(Field = "ProjectSubName", In = list("2021 Mullen Fire")))),
  encode = "json")

# Extract the response
con<- content(fldData, encoding = "json")

# Make a data.frame with proper data types, use custom function
allData<- apiToDF(data = con$data,
                  cols = sapply(con$columnInfo, FUN = function(x) x[[1]]),
                  datatypes = sapply(con$columnInfo, FUN = function(x) x[[2]]))


# now we have to extract the data.frames from the JSON that exist in the CompletedDataSheet column
overDF <- data.frame()
soilDF <- data.frame()
imageDF <- data.frame()
daubDF <- data.frame()
for(i in 1:nrow(allData)){
  jdat <- fromJSON(allData$CompletedDataSheet[i])
  overDF <- overDF %>% 
    bind_rows(jdat$Overiew)
  soilDF <- soilDF %>% 
    bind_rows(jdat$Soil)
  imageDF <- imageDF %>% 
    bind_rows(jdat$Images)
  daubDF <- daubDF %>% 
    bind_rows(jdat$Daubenmire)
}


# for those times when the LIMS Collector app fails
myDir <- "//alcova.arcc.uwyo.edu/project/SoilEcologyLab/MulFire/Collector/BackupMullenFire2021"
# Used this directory from home.... issues accesing alcova through r
myDir <- "/Users/tannerhoffman/Desktop/BackupMullenFire2021"

mulFile <- dir(myDir, pattern = ".json$", full.names = TRUE, recursive = TRUE)

# loop through each file and read the info
overDF<- data.frame()
soilDF<- data.frame()
imageDF<- data.frame()
daubDF<- data.frame()
for(i in 1:length(mulFile)){
  # read in the JSON
  dat <- fromJSON(mulFile[i])
  dat$`_metadata`
  dat$siteOverview
  # build overview table
  overDF<- rbind(overDF, data.frame(SiteID = dat$`_metadata`$siteId,
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
# remove data that is not mine
overDF <- overDF %>% 
  slice(-c(44:51), -c(14:29), -c(1,2)) %>% 
  slice(-c(14:29)) %>% 
  slice(-c(1,2))

# fix site name mistakes.... can't figure out how to fix in r, so going to send it to xcel, then send back to r..... this seems like a pain in the ass, must be easier way

write_xlsx(tmp, path = "/Users/tannerhoffman/desktop/fix_dat.xlsx")
overDF <- read_excel("/Users/tannerhoffman/desktop/fix_dat.xlsx", na = "NA")



##### soil table ####
  tmp <- data.frame(SiteID = dat$`_metadata`$siteId,
                                    SampDate = ymd_hms(dat$`_metadata`$startTime),
                                    SampleType = "soil",
                                    dat$soilSamples) 
    select(-daubenmire) %>% 
    rename()
                   select(-c(`daubenmire.Bare Earth`, `daubenmire.Litter`, `daubenmire.Lichen/Moss`, `daubenmire.Graminoid`, `daubenmire.Forb`, `daubenmire.Shrub`, `daubenmire.Cheatgrass`, `daubenmire.Rock`))
                   
                   
                
                   for(i in 1:nrow(allData)){
                     jdat <- fromJSON(allData$CompletedDataSheet[i])
                     overDF <- overDF %>% 
                       bind_rows(jdat$Overiew)
                     soilDF <- soilDF %>% 
                       bind_rows(jdat$Soil)
                     imageDF <- imageDF %>% 
                       bind_rows(jdat$Images)
                     daubDF <- daubDF %>% 
                       bind_rows(jdat$Daubenmire)
                   }

