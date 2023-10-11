setwd("C:/Users/lss284/OneDrive - Northern Arizona University/Desktop/Projects/SNOWPACS/data/Wade data/States Data")
dat_loc <- "C:/Users/lss284/OneDrive - Northern Arizona University/Desktop/Projects/SNOWPACS/data/Wade data/States Data/Input_data"


library(tidyverse)
library(sf)
library(terra)
library(tigris)
library(rgdal)
library(maps)
library(tmap)
library(tmaptools)

pattern <- "waterallocations"

files <- list.files(dat_loc, pattern = pattern)

# Create an empty list to store the loaded data
WA_data_list <- list()

# Load the CSV files and store them in the list
for (file in files) {
  file_path <- file.path(dat_loc, file)
  data <- read.csv(file_path)  # Use read.csv2() if your CSV files have a different separator
  WA_data_list[[file]] <- data
}

colnames(WA_data_list[[1]])

keep.cols <- c("AllocationUUID" ,"SiteUUID", "AllocationFlow_CFS","AllocationVolume_AF","BeneficialUseCategory",       
                "PrimaryBeneficialUseCategory")

#### Use the below code to make sure the ignore.case = TRUE.


WA_data_list <- lapply(WA_data_list, function(df) {
  selected_cols <- grep(paste(keep.cols, collapse = "|"), colnames(df), value = TRUE, ignore.case = TRUE)
  df[, selected_cols, drop = FALSE]
})


names(WA_data_list)

#### Replace names of WY
# colnames(WA_data_list[["WY_waterallocations.csv"]]) <- colnames(WA_data_list[["NV_waterallocations.csv"]])


# Function to get unique values in a column of a data frame
get_unique_values <- function(df, column_name) {
  unique(df[[column_name]])
}

# Apply the function to each data frame in the list
unique_values_list <- lapply(WA_data_list, get_unique_values, column_name = "PrimaryBeneficialUseCategory")

for (i in seq_along(unique_values_list)) {
  cat("Unique values in df", i, ":\n")
  print(unique_values_list[[i]])
  cat("\n")
}


categories <- c("Agriculture Irrigation")

# Function to filter a data frame based on categories
filter_data_frame <- function(df, column_name, categories) {
  subset(df, df[[column_name]] %in% categories)
}

# Apply the filter function to each data frame in the list
Fil_WA_data_list <- lapply(WA_data_list, filter_data_frame, column_name = "PrimaryBeneficialUseCategory", categories = categories)


#################################################################################################################################################
#################################################################################################################################################


# Load sites data
pattern <- "sites"

files <- list.files(dat_loc, pattern = pattern)

sites_data_list <- list()

# Load the CSV files and store them in the list
for (file in files) {
  file_path <- file.path(dat_loc, file)
  data <- read.csv(file_path)  # Use read.csv2() if your CSV files have a different separator
  sites_data_list[[file]] <- data
}


keep.cols_sites <- c("SiteUUID" , "County", "Latitude" ,"Longitude" ,  "SiteName"  ,"SiteTypeCV")

sites_data_list <- lapply(sites_data_list, function(df) {
  selected_cols <- grep(paste(keep.cols_sites, collapse = "|"), colnames(df), value = TRUE, ignore.case = TRUE)
  df[, selected_cols, drop = FALSE]
})


names(sites_data_list)

#### Replace names of WY
# colnames(sites_data_list[["WY_sites.csv"]]) <- colnames(sites_data_list[["NV_sites.csv"]])



###########################################################################################################
###########################################################################################################
##### if we want to check all rights we can replace Fil_WA_data_list with WA_data_list

Water_rights_list <- list()

# i = 3
for (i in 1:length(Fil_WA_data_list)) {
  
  tryCatch({
    
    Water_allocation <- WA_data_list[[i]]
    sites <- sites_data_list[[i]]

    Water_allocation <- separate(Water_allocation, SiteUUID, into = c("Siteid1", "siteid2"), sep = ",")
    
    Water_allocation_dat  <- left_join(Water_allocation,sites, by = c("Siteid1" = "SiteUUID"))
    
    
    Water_allocation_dat$nam <- map.where("county", Water_allocation_dat$Longitude, Water_allocation_dat$Latitude)
    
    Water_allocation_dat$nam <- gsub(",", "_", Water_allocation_dat$nam)
    Water_allocation_dat$nam  <- toupper(Water_allocation_dat$nam)
    Water_allocation_dat$nam <- trimws(Water_allocation_dat$nam) 
    
    Water_allocation_dat$AllocationVolume_AF1 <- ifelse(Water_allocation_dat$AllocationVolume_AF == 0, 
                                                       Water_allocation_dat$AllocationFlow_CFS * 1.983, 
                                                       Water_allocation_dat$AllocationVolume_AF)

    
    # Water_allocation_dat$AllocationFlow_CFS <- NULL
    
   Water_rights_list[[i]] <- Water_allocation_dat
   rm(Water_allocation_dat)
  }, error = function(e) {
    print(paste("Error for record", i, ":", e))
  })
}

save(Water_rights_list, file="Water_rights_list2.RDATA")

#########################################################################################################
#########################################################################################################

load("Water_rights_list2.RDATA")

# Create a summary at county level
options(scipen = 999999)
All_dat <- do.call(rbind, Water_rights_list)
All_dat <- as.data.frame(All_dat)

rm(Water_rights_list)

Apache <- All_dat %>% 
  filter(County == "Apache")

County_level_Primary_use <- All_dat %>% 
  group_by(nam,PrimaryBeneficialUseCategory) %>% 
  summarise(AllocationVolume_AF = round(sum(AllocationVolume_AF),0))

prams <- unique(County_level_Primary_use$PrimaryBeneficialUseCategory)

shape <- readOGR(dsn = "C:/Users/lss284/Downloads/tl_2022_us_county", layer = "tl_2022_us_county")


# save(shape, file="county_shapefile.Rdata")
bbox_new <- st_bbox(shape) 
bbox_new[1] <- -124.736342
bbox_new[2] <- 24.7433195 
bbox_new[3] <- -66.945392
bbox_new[4] <- 49.382808


i=2
for (i in 3:length(prams)) {

  shape2 <- shape
  
  states.keep <- c("Nevada" ,              
                   "Utah"  ,
                   "Colorado",     
                   "Arizona",              
                   "New Mexico" )
  
  
  
   states.keep2 <- toupper(states.keep)
  
   

  
  
  FIPS <- read.csv("FIPS2.csv")
  FIPS$fips <- str_pad(FIPS$fips, 5, pad = "0")
  
  FIPS$fips <- as.character(FIPS$fips)
  
  
  shape2@data <- left_join(shape2@data, FIPS[,c(1,4)], by = c("GEOID" = "fips"))
  shape2@data <- shape2@data[,-c(6:13)]
  shape2@data$NAME <- toupper(shape2@data$NAME)
  
  
  shape2 <- subset(shape2, Name %in% states.keep2)
  
  
  shape2@data$State_county <- paste0(shape2@data$Name,"_",shape2@data$NAME)

pram_data <- County_level_Primary_use %>% 
  filter(PrimaryBeneficialUseCategory == prams[i] & AllocationVolume_AF >0)

shape2@data <- left_join(shape2@data, pram_data, by = c("State_county" = "nam"))

tmap_mode(mode = "plot")

tm_map <- tm_shape(shape2)+
  tm_polygons(col = "AllocationVolume_AF",
              title = paste0(prams[i]),
              n=5,style="jenks",
              # breaks = c(0,1.09,2.19,4.38,8.76,21.9,43.81,109.53, Inf),
              textNA = "No Data",
              colorNA="grey",
              id="NAME")
tm_map <- tm_map +
  tm_layout( main.title =  paste0("Water Rights: ", prams[i]))
 

tmap_save(tm_map, paste0("water_rights", prams[i],".png"),  width = 10, height = 6, units = 'in')
}
















































