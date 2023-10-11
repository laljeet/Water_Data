# Load the data.table package
library(data.table)
library(tidyverse)
library(stringr)

####Function to clean census data 

# The original data from NASS has a lot of columns. This functions clears and renames some columns for easier visualization.
# Create function to clear data ####
setwd("C:/Users/lss284/OneDrive - Northern Arizona University/Desktop/Projects/SNOWPACS/data/CDL")

clear_data <- function(dat){
  dat$VALUE<-gsub(",","",as.character(dat$VALUE)) # replace , in the coloumns
  dat$VALUE <- as.numeric(iconv(dat$VALUE, 'utf-8', 'ascii', sub='')) # convert from utf-8 to ascii to get rid of all characters
  # dat <- dat[,c(2:5,9:11,13,15,16,17,21,22,30,38)]
  dat <- dat[,c(5:13,15,16)]
  return(dat)
}


# Specify the file path
file_path <- paste0(getwd(),"/2017_cdqt_data.txt")
# Read the file using fread()
census_dat <- fread(file_path)

states.keep <- c("California","Nevada" ,              
                 "Utah"  ,
                 "Colorado",     
                 "Arizona",              
                 "New Mexico" )
states.keep <- toupper(states.keep)

census_dat_state <- census_dat %>% 
  dplyr::filter(STATE_NAME %in% states.keep)

rm(census_dat)

unique(census_dat_state$STATE_NAME)
unique(census_dat_state$SECTOR_DESC)


#####################################################
# Selecet Sector

Sector_census_dat <- census_dat_state %>% 
  dplyr::filter(SECTOR_DESC == "CROPS" )

Sector_census_dat <- census_dat_state %>% 
  dplyr::filter( AGG_LEVEL_DESC == "COUNTY")

Sector_census_dat$fips  <- paste0(Sector_census_dat$STATE_FIPS_CODE,Sector_census_dat$COUNTY_CODE)
Sector_census_dat$fips <- as.numeric(Sector_census_dat$fips)
Sector_census_dat$fips  <- sprintf("%05d",Sector_census_dat$fips)

Commodity_census_dat <- Sector_census_dat %>% 
  dplyr::filter(COMMODITY_DESC != "CROP TOTALS" )

Acres_harvested <- Commodity_census_dat[grep('- ACRES HARVESTED', Commodity_census_dat$SHORT_DESC), ]
Acres_harvested <- clear_data(Acres_harvested)

Acres_harvested <- Acres_harvested[complete.cases(Acres_harvested$VALUE), ]

Acres_harvested <- Acres_harvested %>%
  mutate(crop = sub(" - .*", "", SHORT_DESC))



Irrigated_acres_harvested <- Acres_harvested[grep(' IRRIGATED -', Acres_harvested$SHORT_DESC), ]


Acres_harvested <- Acres_harvested[!grep(' IRRIGATED -', Acres_harvested$SHORT_DESC), ]


# Remove duplicate rows based on multiple columns (fips, VALUE, COMMODITY_DESC)
Acres_harvested <- Acres_harvested[!duplicated(Acres_harvested, by = c("fips", "VALUE", "COMMODITY_DESC"))]
Irrigated_acres_harvested <-Irrigated_acres_harvested[!duplicated(Irrigated_acres_harvested, by = c("fips", "VALUE", "COMMODITY_DESC"))]


# #############################################################################
# # HANDLE HAY
# 
# HAy <- Acres_harvested[grep('HAY', Acres_harvested$COMMODITY_DESC), ]
# Acres_harvested <- Acres_harvested[!grep('HAY', Acres_harvested$COMMODITY_DESC), ]
# 
# 
# HAy <- HAy %>%
#   group_by(fips) %>%
#   slice_max(order_by = VALUE)
# 
# Acres_harvested <- rbind.data.frame(Acres_harvested,HAy)
# 
# #################################################################################
# # HANDLE COTTON
# 
# COTTON <- Acres_harvested[grep('COTTON', Acres_harvested$COMMODITY_DESC), ]
# Acres_harvested <- Acres_harvested[!grep('COTTON', Acres_harvested$COMMODITY_DESC), ]
# 
# 
# COTTON <- COTTON %>%
#   group_by(fips) %>%
#   slice_max(order_by = VALUE)
# 
# Acres_harvested <- rbind.data.frame(Acres_harvested,COTTON)
# 




Irrigated_acres_harvested <- Irrigated_acres_harvested %>%
  mutate(crop = gsub(", IRRIGATED", "", crop))


left_merged_df <- left_join(Acres_harvested, Irrigated_acres_harvested[,c(12,11,10)], by = c("crop", "fips"))

colnames(left_merged_df)[c(10,13)] <- c("To_acres", "Ir_acres")

left_merged_df$Pct_irr <- round(100*left_merged_df$Ir_acres/left_merged_df$To_acres,2)


PCT_irrigate <- left_merged_df %>% 
  filter(!is.na(Pct_irr))

############################################################################################
# PCT Irrigated data frame based on crop as a whole (example corn grain and corn silage) are both corn at county level
# if a county has 80% irrigated area for corn silage it is assumed that 80% of corn grain is also irrigated if no data for corn silage exists

PCT_NA <- left_merged_df %>% 
  filter(is.na(Pct_irr))

PCT_NA <- left_join(PCT_NA, PCT_irrigate[,c(3,11,14)], by = c("COMMODITY_DESC", "fips"))

PCT_irrigate2 <- PCT_NA %>% 
  filter(!is.na(Pct_irr.y))

PCT_irrigate2$Pct_irr.x <- NULL

colnames(PCT_irrigate2)  <- colnames(PCT_irrigate)



PCT_irrigate3 <- PCT_NA %>% 
  filter(is.na(Pct_irr.y))


###########################################################################################
# State level crop average

Irrigated_pct_df <- rbind.data.frame(PCT_irrigate,PCT_irrigate2)

State_summary_crop <- Irrigated_pct_df %>% 
  group_by(COMMODITY_DESC,STATE_FIPS_CODE) %>% 
  summarise(Avg_Pct_area = round(mean(Pct_irr, na.rm = TRUE),2))

PCT_irrigate3 <- left_join(PCT_irrigate3, State_summary_crop, by = c("COMMODITY_DESC", "STATE_FIPS_CODE"))

PCT_irrigate3 <- PCT_irrigate3 %>% 
  filter(!is.na(Avg_Pct_area))

PCT_irrigate3<- PCT_irrigate3[,-c(14,15)]

colnames(PCT_irrigate3) <- colnames(Irrigated_pct_df)
Final_data <- rbind.data.frame(Irrigated_pct_df,PCT_irrigate3)

Final_data_summary <- Final_data %>% 
  group_by(fips,COMMODITY_DESC) %>% 
  summarise(AVG_irri = mean(Pct_irr))



Final_data <- Final_data[!duplicated(Final_data, by = c("fips", "Pct_irr", "crop"))]
Final_data <- Final_data[!duplicated(Final_data, by = c("fips", "Pct_irr", "COMMODITY_DESC"))]

write.csv(Final_data, "Irrigated_area_pct_Oct.csv")


load("C:/Users/lss284/OneDrive - Northern Arizona University/Desktop/Projects/SNOWPACS/data/Irrigation_data/county_shapefile.Rdata")


states.keep <- c("CALIFORNIA","Nevada" ,              
                 "Utah"  ,
                 "Colorado",     
                 "Arizona",              
                 "New Mexico" )
states.keep2 <- toupper(states.keep)


crop_data_list <- split(Final_data, f = Final_data$COMMODITY_DESC)
pram <- names(crop_data_list)

subset_shape <- subset(shape, Name %in% states.keep2)

subset_shape@data <- left_join(subset_shape@data, Final_data, by = c("GEOID"= "fips"))

i =2 
for (i in 1:length(pram)) {
  subset_shape <- subset(shape, Name %in% states.keep2)
  subset_shape@data <- left_join(subset_shape@data, crop_data_list[[i]], by = c("GEOID"= "fips"))
  
  tmap_mode("view")
  tmap_mode("plot")
  map <- tm_shape(subset_shape) +
    tm_borders() +
    tm_fill("Pct_irr",  
            n=5, style="jenks",
            textNA = "No Data",
            alpha = 0.8, 
            id = "name") 
  
  
  map<- map +
    tm_layout( main.title =  paste0(pram[i]," Irrigated Percentage") )
  
  
  tmap_save(map, paste0("C:/Users/lss284/OneDrive - Northern Arizona University/Desktop/Projects/SNOWPACS/data/CDL/Irrigated_pct/",pram[i],"_Irrigated_pct.png"),  width = 10, height = 8, units = 'in')
  
}







































