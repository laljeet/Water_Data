library(dataRetrieval)
library(tidyverse)

setwd("C:/Users/lss284/OneDrive - Northern Arizona University/Desktop/Projects/SNOWPACS/data/USGS water use")
dir <- "C:/Users/lss284/OneDrive - Northern Arizona University/Desktop/Projects/SNOWPACS/data/USGS water use"

# Load JSON library
library(jsonlite)


# This url is for follwoing:
# GW levels from land surface
# Json format
# Arizona
# Active sites


# # URL to retrieve data 
# url <- "https://waterservices.usgs.gov/nwis/gwlevels/?format=json&stateCd=az&parameterCd=72019&siteType=GW&siteStatus=active"
# 
# # Download the JSON data from the URL
# json_data <- jsonlite::fromJSON(url)
# 
# # Extract the relevant data from the JSON object
# data <- json_data$value$timeSeries
# 
# # Convert the data to a data frame
# df <- jsonlite::flatten(data)
# 
# 
# desired_data <- ((df[[1]][[1]])[[1]][[1]])[1,1]
# desired_data
# 
# desired_data <- df[,c(2,3)]
# desired_data$depth <- NA
# 
# 
# # Create an empty vector to store the desired values
# desired_values <- vector("numeric", nrow(df))
# desired_row <- vector("numeric", nrow(df))
# 
# # Iterate over the rows of df
# for (i in seq_len(nrow(df))) {
#   desired_values[i] <- df[i, 1][[1]][[1]][[1]][1]
#   desired_row[i] <- subset(df[[7]][[i]], name == "countyCd")
# 
# }
# 
# # Extract only the first value from each element
# desired_values <- sapply(desired_values, `[`, 1)
# # Convert desired_values to a data frame
# desired_values <- data.frame(depth = as.numeric(desired_values), stringsAsFactors = FALSE)
# 
# 
# 
# desired_row <- sapply(desired_row, `[`, 1)
# desired_row <- data.frame(countyCd = as.numeric(desired_row), stringsAsFactors = FALSE)
# 
# 
# desired_data$countyCd <- desired_row$countyCd
# desired_data$depth <- desired_values$depth
# 
# desired_data$lat <- df$sourceInfo.geoLocation.geogLocation.latitude
# 
# desired_data$long <- df$sourceInfo.geoLocation.geogLocation.longitude
# desired_data$Va_nam <- df$variable.variableName
# 
# save(desired_data, file="AZ_groundwaterlevels.Rdata")
###################################################################

# This is a better way 

# URL to retrieve data 
rm(list = ls(all=TRUE))

url <- "https://waterservices.usgs.gov/nwis/gwlevels/?format=rdb&stateCd=co&parameterCd=72019&siteStatus=active"
filename <- "data.rdb"

# Download the file
download.file(url, destfile = filename)

# Read the file into a character vector
file_lines <- readLines(filename)

# Identify the starting line of the data
data_start_line <- grep("^#", file_lines)[-1][1] + 1

# Read the data from the identified starting line
data <- read.table(text = file_lines[data_start_line:length(file_lines)], header = TRUE, sep = "\t")

# Print the data frame
print(data)

data <- data[-c(1),]
##################################################################

# Define the site numbers
siteNumbers <- data$site_no  # Replace with your actual site numbers


stationData <- data.frame(SiteNumber = character(),
                          Latitude = numeric(),
                          Longitude = numeric(),
                          State_cd = character(),
                          county_cd = character(),
                          stringsAsFactors = FALSE)

siteInfo <- list()

for (i in 1:length(siteNumbers)) {
  siteInfo[[i]] <- readNWISsite(siteNumbers[i])
  
  
}

names(siteInfo) <- siteNumbers



# Create an empty data frame to store the results
stationData <- data.frame(SiteNumber = character(),
                          Latitude = numeric(),
                          Longitude = numeric(),
                          State_cd = character(),
                          county_cd = character(),
                          stringsAsFactors = FALSE)

# Iterate over the list of data frames
for (df in siteInfo) {
  # Extract the desired columns from each data frame
  site_no <- df$site_no
  dec_lat_va <- df$dec_lat_va
  dec_long_va <- df$dec_long_va
  state_cd <- df$state_cd
  county_cd <- df$county_cd
  
  # Append the extracted columns to the stationData data frame
  stationData <- rbind(stationData, data.frame(site_no = site_no,
                                               Latitude = dec_lat_va,
                                               Longitude = dec_long_va,
                                               State_cd = state_cd,
                                               county_cd = county_cd,
                                               stringsAsFactors = FALSE))
}


stationData_CO <- left_join(data[,c(2,7)],stationData ,by = "site_no")

save(stationData_CO, file="CO_groundwaterlevels.Rdata")



##########################################################################


# URL to retrieve data 
rm(list = ls(all=TRUE))

url <- "https://waterservices.usgs.gov/nwis/gwlevels/?format=rdb&stateCd=az&parameterCd=72019&siteStatus=active"
filename <- "azdata.rdb"

# Download the file
download.file(url, destfile = filename)

# Read the file into a character vector
file_lines <- readLines(filename)

# Identify the starting line of the data
data_start_line <- grep("^#", file_lines)[-1][1] + 1

# Read the data from the identified starting line
data <- read.table(text = file_lines[data_start_line:length(file_lines)], header = TRUE, sep = "\t")

# Print the data frame
print(data)

data <- data[-c(1),]
##################################################################

# Define the site numbers
siteNumbers <- data$site_no  # Replace with your actual site numbers


stationData <- data.frame(SiteNumber = character(),
                          Latitude = numeric(),
                          Longitude = numeric(),
                          State_cd = character(),
                          county_cd = character(),
                          stringsAsFactors = FALSE)

siteInfo <- list()

for (i in 1:length(siteNumbers)) {
  siteInfo[[i]] <- readNWISsite(siteNumbers[i])
  
  
}

names(siteInfo) <- siteNumbers



# Create an empty data frame to store the results
stationData <- data.frame(SiteNumber = character(),
                          Latitude = numeric(),
                          Longitude = numeric(),
                          State_cd = character(),
                          county_cd = character(),
                          stringsAsFactors = FALSE)

# Iterate over the list of data frames
for (df in siteInfo) {
  # Extract the desired columns from each data frame
  site_no <- df$site_no
  dec_lat_va <- df$dec_lat_va
  dec_long_va <- df$dec_long_va
  state_cd <- df$state_cd
  county_cd <- df$county_cd
  
  # Append the extracted columns to the stationData data frame
  stationData <- rbind(stationData, data.frame(site_no = site_no,
                                               Latitude = dec_lat_va,
                                               Longitude = dec_long_va,
                                               State_cd = state_cd,
                                               county_cd = county_cd,
                                               stringsAsFactors = FALSE))
}


stationData_AZ <- left_join(data[,c(2,7)],stationData ,by = "site_no")

save(stationData_AZ, file="AZ_groundwaterlevels.Rdata")



##########################################################################


# URL to retrieve data 
rm(list = ls())

url <- "https://waterservices.usgs.gov/nwis/gwlevels/?format=rdb&stateCd=nm&parameterCd=72019&siteStatus=active"
filename <- "nmdata.rdb"

# Download the file
download.file(url, destfile = filename)

# Read the file into a character vector
file_lines <- readLines(filename)

# Identify the starting line of the data
data_start_line <- grep("^#", file_lines)[-1][1] + 1

# Read the data from the identified starting line
data <- read.table(text = file_lines[data_start_line:length(file_lines)], header = TRUE, sep = "\t")

# Print the data frame
print(data)

data <- data[-c(1),]
##################################################################

# Define the site numbers
siteNumbers <- data$site_no  # Replace with your actual site numbers

siteInfo <- list()

for (i in 1:length(siteNumbers)) {
  siteInfo[[i]] <- readNWISsite(siteNumbers[i])
  
  
}

names(siteInfo) <- siteNumbers



# Create an empty data frame to store the results
stationData <- data.frame(SiteNumber = character(),
                          Latitude = numeric(),
                          Longitude = numeric(),
                          State_cd = character(),
                          county_cd = character(),
                          stringsAsFactors = FALSE)

# Iterate over the list of data frames
for (df in siteInfo) {
  # Extract the desired columns from each data frame
  site_no <- df$site_no
  dec_lat_va <- df$dec_lat_va
  dec_long_va <- df$dec_long_va
  state_cd <- df$state_cd
  county_cd <- df$county_cd
  
  # Append the extracted columns to the stationData data frame
  stationData <- rbind(stationData, data.frame(site_no = site_no,
                                               Latitude = dec_lat_va,
                                               Longitude = dec_long_va,
                                               State_cd = state_cd,
                                               county_cd = county_cd,
                                               stringsAsFactors = FALSE))
}


stationData_nm <- left_join(data[,c(2,7)],stationData ,by = "site_no")

save(stationData_nm, file="NM_groundwaterlevels.Rdata")


##########################################################################


# URL to retrieve data 
rm(list = ls())

url <- "https://waterservices.usgs.gov/nwis/gwlevels/?format=rdb&stateCd=nv&parameterCd=72019&siteStatus=active"
filename <- "nvdata.rdb"

# Download the file
download.file(url, destfile = filename)

# Read the file into a character vector
file_lines <- readLines(filename)

# Identify the starting line of the data
data_start_line <- grep("^#", file_lines)[-1][1] + 1

# Read the data from the identified starting line
data <- read.table(text = file_lines[data_start_line:length(file_lines)], header = TRUE, sep = "\t")

# Print the data frame
print(data)

data <- data[-c(1),]
##################################################################

# Define the site numbers
siteNumbers <- data$site_no  # Replace with your actual site numbers

siteInfo <- list()

for (i in 1:length(siteNumbers)) {
  siteInfo[[i]] <- readNWISsite(siteNumbers[i])
  
  
}

names(siteInfo) <- siteNumbers



# Create an empty data frame to store the results
stationData <- data.frame(SiteNumber = character(),
                          Latitude = numeric(),
                          Longitude = numeric(),
                          State_cd = character(),
                          county_cd = character(),
                          stringsAsFactors = FALSE)

# Iterate over the list of data frames
for (df in siteInfo) {
  # Extract the desired columns from each data frame
  site_no <- df$site_no
  dec_lat_va <- df$dec_lat_va
  dec_long_va <- df$dec_long_va
  state_cd <- df$state_cd
  county_cd <- df$county_cd
  
  # Append the extracted columns to the stationData data frame
  stationData <- rbind(stationData, data.frame(site_no = site_no,
                                               Latitude = dec_lat_va,
                                               Longitude = dec_long_va,
                                               State_cd = state_cd,
                                               county_cd = county_cd,
                                               stringsAsFactors = FALSE))
}


stationData_nv <- left_join(data[,c(2,7)],stationData ,by = "site_no")

save(stationData_nv, file="NV_groundwaterlevels.Rdata")

#####################################################################################################
# URL to retrieve data 
rm(list = ls())

url <- "https://waterservices.usgs.gov/nwis/gwlevels/?format=rdb&stateCd=ut&parameterCd=72019&siteStatus=active"
filename <- "utdata.rdb"

# Download the file
download.file(url, destfile = filename)

# Read the file into a character vector
file_lines <- readLines(filename)

# Identify the starting line of the data
data_start_line <- grep("^#", file_lines)[-1][1] + 1

# Read the data from the identified starting line
data <- read.table(text = file_lines[data_start_line:length(file_lines)], header = TRUE, sep = "\t")

# Print the data frame
print(data)

data <- data[-c(1),]
##################################################################

# Define the site numbers
siteNumbers <- data$site_no  # Replace with your actual site numbers

siteInfo <- list()

for (i in 1:length(siteNumbers)) {
  siteInfo[[i]] <- readNWISsite(siteNumbers[i])
  
  
}

names(siteInfo) <- siteNumbers



# Create an empty data frame to store the results
stationData <- data.frame(SiteNumber = character(),
                          Latitude = numeric(),
                          Longitude = numeric(),
                          State_cd = character(),
                          county_cd = character(),
                          stringsAsFactors = FALSE)

# Iterate over the list of data frames
for (df in siteInfo) {
  # Extract the desired columns from each data frame
  site_no <- df$site_no
  dec_lat_va <- df$dec_lat_va
  dec_long_va <- df$dec_long_va
  state_cd <- df$state_cd
  county_cd <- df$county_cd
  
  # Append the extracted columns to the stationData data frame
  stationData <- rbind(stationData, data.frame(site_no = site_no,
                                               Latitude = dec_lat_va,
                                               Longitude = dec_long_va,
                                               State_cd = state_cd,
                                               county_cd = county_cd,
                                               stringsAsFactors = FALSE))
}


stationData_ut <- left_join(data[,c(2,7)],stationData ,by = "site_no")

save(stationData_ut, file="UT_groundwaterlevels.Rdata")
#############################################################################################
# URL to retrieve data 
rm(list = ls())

url <- "https://waterservices.usgs.gov/nwis/gwlevels/?format=rdb&stateCd=ca&parameterCd=72019&siteStatus=active"
filename <- "cadata.rdb"

# Download the file
download.file(url, destfile = filename)

# Read the file into a character vector
file_lines <- readLines(filename)

# Identify the starting line of the data
data_start_line <- grep("^#", file_lines)[-1][1] + 1

# Read the data from the identified starting line
data <- read.table(text = file_lines[data_start_line:length(file_lines)], header = TRUE, sep = "\t")

# Print the data frame
print(data)

data <- data[-c(1),]
##################################################################

# Define the site numbers
siteNumbers <- data$site_no  # Replace with your actual site numbers

siteInfo <- list()

for (i in 1:length(siteNumbers)) {
  siteInfo[[i]] <- readNWISsite(siteNumbers[i])
  
  
}

names(siteInfo) <- siteNumbers



# Create an empty data frame to store the results
stationData <- data.frame(SiteNumber = character(),
                          Latitude = numeric(),
                          Longitude = numeric(),
                          State_cd = character(),
                          county_cd = character(),
                          stringsAsFactors = FALSE)

# Iterate over the list of data frames
for (df in siteInfo) {
  # Extract the desired columns from each data frame
  site_no <- df$site_no
  dec_lat_va <- df$dec_lat_va
  dec_long_va <- df$dec_long_va
  state_cd <- df$state_cd
  county_cd <- df$county_cd
  
  # Append the extracted columns to the stationData data frame
  stationData <- rbind(stationData, data.frame(site_no = site_no,
                                               Latitude = dec_lat_va,
                                               Longitude = dec_long_va,
                                               State_cd = state_cd,
                                               county_cd = county_cd,
                                               stringsAsFactors = FALSE))
}


stationData_CA <- left_join(data[,c(2,7)],stationData ,by = "site_no")

save(stationData_CA, file="CA_groundwaterlevels.Rdata")

##################################################################
rm(list = ls())
setwd("C:/Users/imsan/OneDrive - Northern Arizona University/Desktop/Projects/SNOWPACS/data/USGS water use")
library(rgdal)
library(sf)
library(sp)
library(tmap)
library(tidyverse)

load("NV_groundwaterlevels.Rdata")
load("UT_groundwaterlevels.Rdata")
load("CO_groundwaterlevels.Rdata")
load("AZ_groundwaterlevels.Rdata")
load("NM_groundwaterlevels.Rdata")
load("CA_groundwaterlevels.Rdata")

Ground_level_dat <- rbind.data.frame(stationData_ut,stationData_nv,stationData_nm,stationData_AZ,stationData_CO,stationData_CA)
Ground_level_dat$Depth <- as.numeric(Ground_level_dat$lev_va)

Ground_level_dat$Depth <- ifelse(Ground_level_dat$Depth <0,0,Ground_level_dat$Depth)

# Ground_level_dat <- Ground_level_dat %>% 
#   filter(Depth >0)
  
  
states.keep <- c("Nevada" ,              
                 "Utah"  ,
                 "Colorado",     
                 "Arizona",              
                 "New Mexico",
                 "California")

shape <- st_read(dsn = "C:/Users/lss284/OneDrive - Northern Arizona University/Desktop/Projects/SNOWPACS/data/tl_2022_us_county", layer = "tl_2022_us_county")
# shape <- st_read(dsn = "C:/Users/imsan/OneDrive - Northern Arizona University/Desktop/Projects/SNOWPACS/data/tl_2022_us_county", layer = "tl_2022_us_county")
fips <- read.csv("https://raw.githubusercontent.com/laljeet/MY_files/main/FIPS2.csv")
fips$fips<-  str_pad(fips$fips, 5, pad = "0")
shape <- left_join(shape,fips, by = c("GEOID" = "fips"))

states.keep2 <- toupper(states.keep)
subset_shape <- subset(shape, Name %in% states.keep2)


# Convert the Location_data dataframe to an sf object
 Location_data_sf <- st_as_sf(Ground_level_dat, coords = c("Longitude", "Latitude"), crs = 4269)  # NAD83



Location_data_sf$fips <- paste0(Location_data_sf$State_cd,Location_data_sf$county_cd)

library(gstat)
library(sp)
library(ggplot2)




# Plot the shapefile
tmap_mode("plot")
map <- tm_shape(subset_shape) +
  tm_borders()+  # Add borders to the shapefile
  # Plot the Location_data
  tm_shape(Location_data_sf) +
  tm_bubbles(size = "Depth",col = "darkgoldenrod",  border.col = "black", alpha = 0.8 ,  id="site_no") 
map



library(tmap)

# Create the map without a base map
map <- tm_shape(subset_shape) +
  tm_borders() +
  tm_shape(Location_data_sf) +
  tm_bubbles(size = "Depth", col = "darkgoldenrod", border.col = "black", alpha = 0.8, id = "site_no")

# Convert the map to a leaflet object and add a base map
map_leaflet <- tmap_leaflet(map, basemaps = "OpenStreetMap")

# Display the map
map_leaflet


tmap_save(map, "Groundwater_table.png")

###########################################################################

GW_mean = Location_data_sf %>%
  group_by(fips) %>%
  summarize(GW_mean = mean(Depth, na.rm = TRUE))

GW_mean <- left_join(GW_mean,fips[c(2:3)], by = "fips")

GW_mean$geometry <- NULL
GW_mean<- as.data.frame(GW_mean)
######################################################################
# Assuming the pump at 75% efficiency 1.37 kWH is required to lift 1 Acrefeet
GW_mean$Energy_need_KwH_AF <- round(GW_mean$GW_mean * 1.37 ,0)

######################################################################
# Assuming 0.12$ cost for 1kwH (0.12 was obtained by taking average cost of electricity for month of august in the states)
GW_mean$Electric_cost_dollar_AF <- GW_mean$Energy_need_KwH_AF * 0.12

#########################################################################
# If using Fuel pump. It costs 0.10 gallons to fuel to generate 1Kwh. On an average 4$ for fuel per gallon
GW_mean$Fuel_cost_dollar_AF <- GW_mean$Energy_need_KwH_AF * 0.10*4


load("C:/Users/lss284/OneDrive - Northern Arizona University/Desktop/Projects/SNOWPACS/data/Irrigation_data/county_shapefile.Rdata")


states.keep <- c("Nevada" ,              
                 "Utah"  ,
                 "Colorado",     
                 "Arizona",              
                 "New Mexico" )
states.keep2 <- toupper(states.keep)
subset_shape <- subset(shape, Name %in% states.keep2)

subset_shape@data <- left_join(subset_shape@data, GW_mean, by = c("GEOID"= "fips"))

tmap_mode("view")
tmap_mode("plot")
map <- tm_shape(subset_shape) +
  tm_borders() +
  tm_fill("Electric_cost_dollar_AF",  
          n=5, style="jenks",
          textNA = "No Data",
          alpha = 0.8, 
          id = "name") 
map<- map +
    tm_layout( main.title =  "$ per acre-feet of groundwater pumping (Electric pump)")

map
tmap_save(map, "Electric_pump$_AF.png",  width = 10, height = 8, units = 'in')


tmap_mode("plot")
map <- tm_shape(subset_shape) +
  tm_borders() +
  tm_fill("Fuel_cost_dollar_AF",  
          n=5, style="jenks",
          textNA = "No Data",
          alpha = 0.8, 
          id = "name") 
map<- map +
    tm_layout( main.title =  "$ per acre-feet of groundwater pumping (Diesel pump)")

map
tmap_save(map, "Diesel_pump$_AF.png",  width = 10, height = 8, units = 'in')

subset_shape@data$GW_mean <- round(subset_shape@data$GW_mean,0)
tmap_mode("plot")
map <- tm_shape(subset_shape) +
  tm_borders() +
  tm_fill("GW_mean",  
          n=5, style="jenks",
          textNA = "No Data",
          alpha = 0.8, 
          id = "name")+
          tm_text("GW_mean",
            size = 0.75) 
map<- map +
  tm_layout( main.title =  "Average depth to groundwater")

map

tmap_save(map, "GW_mean.png",  width = 10, height = 8, units = 'in')






















