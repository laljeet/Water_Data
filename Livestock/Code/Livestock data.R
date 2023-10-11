# Load the data.table package
library(data.table)
library(tidyverse)
options(scipen = 999999)

setwd("C:/Users/lss284/OneDrive - Northern Arizona University/Desktop/Projects/SNOWPACS/data/Quick stats data")

# Specify the file path
# file_path <- "C:/Users/lss284/Downloads/Census_data/qs.animals_products_20230705.txt"  

# Read the file using fread()
# census_dat <- fread(file_path) # the same is located in .Rdata in the next line

load("Livestock_census_data.Rdata")


####


clear_data <- function(dat){
  dat$VALUE<-gsub(",","",as.character(dat$VALUE)) # replace , in the coloumns
  dat$VALUE <- as.numeric(iconv(dat$VALUE, 'utf-8', 'ascii', sub='')) # convert from utf-8 to ascii to get rid of all characters
  dat <- dat[,c(2:5,9:11,13,15,16,17,21,22,30,38)]
  return(dat)
}



states.keep <- c("Nevada" ,              
                 "Utah"  ,
                 "Colorado",     
                 "Arizona",              
                 "New Mexico" )

states.keep <- toupper(states.keep)

census_dat_state <- census_dat %>% 
  dplyr::filter(STATE_NAME %in% states.keep)

rm(census_dat)


census_dat_state <- census_dat_state %>% 
  dplyr::filter(SOURCE_DESC == "CENSUS" &
                  FREQ_DESC == "ANNUAL" &
                  AGG_LEVEL_DESC == "COUNTY" &
                  YEAR == 2017)

census_dat_state <- clear_data(census_dat_state)
#######################################################################################
# STEP1 Get all different commodities

unique(census_dat_state$COMMODITY_DESC)  # 41 commodities
unique(census_dat_state$UNIT_DESC)   # data is reported in 5 units

########################################################################################
# For a start get all data with "HEAD" as a unit

split_dat_unit <- split(census_dat_state, census_dat_state$UNIT_DESC)

# split_dat_unit <- lapply(split_dat_unit, clear_data)  #will remove D values

split_dat_unit <- map(split_dat_unit, ~ .x[complete.cases(.x), ])  # remove NA

########################################################################################
Head_data <- split_dat_unit[["HEAD"]]

Head_data <- Head_data %>% 
  filter(DOMAIN_DESC == "TOTAL")
unique(Head_data$COMMODITY_DESC)

#########################################################################################
LB_data <- split_dat_unit[["LB"]]
unique(LB_data$COMMODITY_DESC)


#########################################################################################

Opearations_data <- split_dat_unit[["OPERATIONS"]]

Opearations_data <- Opearations_data %>% 
  filter(DOMAIN_DESC == "TOTAL")


#######################################################################
# Operations data for different commodities

split_dat_ops <- split(Opearations_data, Opearations_data$COMMODITY_DESC)


##########################################################################################

Sale_data <- split_dat_unit[["$"]]

Sale_data <- Sale_data %>% 
  filter(DOMAIN_DESC == "TOTAL")

split_dat_ops <- split(Opearations_data, Opearations_data$COMMODITY_DESC)

###################################################
test <- Sale_data %>% 
  filter(COUNTY_NAME == "ADAMS" & COMMODITY_DESC != "ANIMAL TOTALS")

test <- Sale_data %>% 
  filter(COUNTY_NAME == "ADAMS")

sum(test$VALUE)

######################################################################################################
# Let's focus on Heads

dat_head <- Head_data

unique(dat_head$COMMODITY_DESC)

dat_summary <- dat_head %>% 
  group_by(COMMODITY_DESC) %>% 
summarise(Count = n(),
          Heads = sum(VALUE))%>%
  mutate(Percentage = round(Heads / sum(Heads) * 100,2)) %>% 
  arrange(desc(Percentage))


## Major Contributors
# Find the subset with cumulative percentage close to 90%
target_percentage <- 90
cumulative_percentage <- 0
selected_rows <- integer(0)

for (i in seq_along(dat_summary$Percentage)) {
  cumulative_percentage <- cumulative_percentage + dat_summary$Percentage[i]
  selected_rows <- c(selected_rows, i)
  if (cumulative_percentage >= target_percentage) {
    break
  }
}

subset_data <- dat_summary[selected_rows, ]

Major_Livestock <- c(subset_data$COMMODITY_DESC)

#######################################################################################
# Let's Subset Heads data for only major livestock

dat_head <- dat_head %>% 
  dplyr::filter(COMMODITY_DESC %in% Major_Livestock)

dat_head <-dat_head[,-c(1,2,7,8,14)]

###################################################################################
Cattle <- dat_head %>% 
  filter(COMMODITY_DESC == "CATTLE")

## Remove duplicates by visual inspection

Cattle <- Cattle %>% 
  filter(CLASS_DESC != "INCL CALVES")


#############################
# From USDA 2020 report

# Live weight of animal

Cattle_weight_live <- 1379 
Calf_weight_live <- 261
ON_feed_weight_live <- 1200

# 63% of live weight is fed meat
Cattle_weight_meat <- Cattle_weight_live*0.63
Calf_weight_meat <- Calf_weight_live*0.63
ON_feed_weight_meat <- ON_feed_weight_live*0.63



Cattle <- Cattle %>%
  mutate(
    meat_production = case_when(
      str_detect(CLASS_DESC, "500 LBS") ~ Cattle_weight_meat * VALUE,
      CLASS_DESC == "CALVES" ~ Calf_weight_meat * VALUE,
      CLASS_DESC == "ALL CLASSES" ~ ON_feed_weight_meat * VALUE
    )
  )

# Assuming 2000 gallons for a pound of meat. Slaughter occurs 30-42 months of age. 36 months a
Cattle$Water_meat <- (Cattle$meat_production*2000)/325900 # COnvert to acre-feet
Cattle$Water_meat <- ((Cattle$meat_production*2000)/325900)/3 # COnvert to acre-feet per year
############################################################################################
###################################################################################
# TURKEYS, PRODUCTION CONTRACT - PRODUCTION, MEASURED IN HEAD # Check this


HOGS <- dat_head %>% 
  filter(COMMODITY_DESC == "HOGS")


unique_hogs <- HOGS[duplicated(HOGS, by = c('STATE_FIPS_CODE', 'COUNTY_CODE')), ]
# This is straightforward

HOG_weight_live <- 285 

Hog_weight_meat <- HOG_weight_live*0.57

HOGS <- HOGS %>%
  mutate(
    meat_production = case_when(
       CLASS_DESC == "ALL CLASSES" ~ Hog_weight_meat * VALUE
    )
  )


# Assuming 720 gallons for a pound of meat
HOGS$Water_Meat <- (HOGS$meat_production*720)/325900 # Convert to acre-feet



############################################################################################
###################################################################################
TURKEYS <- dat_head %>% 
  filter(COMMODITY_DESC == "TURKEYS")


unique_TURKEYS <- TURKEYS[duplicated(TURKEYS, by = c('STATE_FIPS_CODE', 'COUNTY_CODE')), ]


TURKEYS_weight_live <- 30.5  # From Graph

## Store weight of turkey 16-24 or 8-16 depending on male or female. Took the average

TURKEYS_weight_meat <- TURKEYS_weight_live*0.52

TURKEYS <- TURKEYS %>%
  mutate(
    meat_production = case_when(
      CLASS_DESC == "ALL CLASSES" ~ TURKEYS_weight_meat * VALUE
    )
  )


# Assuming 518  gallons for a pound of meat
TURKEYS$Water_Meat <- (TURKEYS$meat_production*518 )/325900 # Convert to acre-feet

############################################################################################
###################################################################################
Chickens <- dat_head %>% 
  filter(COMMODITY_DESC == "CHICKENS")


unique_Chickens <- Chickens[duplicated(Chickens, by = c('STATE_FIPS_CODE', 'COUNTY_CODE')), ]


Chickens_weight_live <- 4  # 3-5 lb

## Store weight of turkey 16-24 or 8-16 depending on male or female. Took the average

Chickens_weight_meat <- Chickens_weight_live*0.82

Chickens <- Chickens %>%
  mutate(
    meat_production = case_when(
      CLASS_DESC == "BROILERS" ~ Chickens_weight_meat * VALUE
    )
  )

Chickens$meat_production <- ifelse(is.na(Chickens$meat_production),0,Chickens$meat_production )

# Assuming 18 pounds of water during life time
Chickens$Water_Meat <- (Chickens$meat_production*520)/325900 # Convert to acre-feet

### For Layer chicken life time is 18-24 months # 21 months average, 630 days
#  Water need is 115-255 grams or 0.00026 gal  

Chicken_water_need <- 630*0.00026

#Saying it meet to keep col names uniform
Chickens$Water_Meat <- ifelse(Chickens$Water_Meat ==0,Chicken_water_need*Chickens$VALUE,Chickens$Water_Meat)


colnames(Cattle)=colnames(HOGS)=colnames(TURKEYS)=colnames(Chickens)

# 
Livestock_water_dat <- rbind.data.frame(Cattle,HOGS,TURKEYS,Chickens) 

colnames(Livestock_water_dat)[10:12] <- c("heads","lb","Acre_feet")

Livestock_water_dat$STATE_FIPS_CODE <- sprintf("%02d", Livestock_water_dat$STATE_FIPS_CODE)
Livestock_water_dat$COUNTY_CODE <- sprintf("%03d", Livestock_water_dat$COUNTY_CODE)

Livestock_water_dat$fips <- paste0(Livestock_water_dat$STATE_FIPS_CODE,Livestock_water_dat$COUNTY_CODE)

Livestock_water_dat_list <- split(Livestock_water_dat,Livestock_water_dat$COMMODITY_DESC )
summarize_dataframe <- function(df) {
  df %>%
    group_by(fips) %>%
    summarise(
      Total_water_use = sum(Acre_feet, na.rm = TRUE),
      heads = sum(heads, na.rm= TRUE),
      lb = sum(lb, na.rm=TRUE),
      across(everything(), ~ first(.))
    )
}

# Apply the summarization function to each data frame in the list
Livestock_water_dat_county <- lapply(Livestock_water_dat_list, summarize_dataframe)

Livestock_water_dat_county<- lapply(Livestock_water_dat_county, function(df) subset(df, select = -c(Acre_feet)))

# Livestock_water_dat_county <- Livestock_water_dat %>%
#   group_by(fips) %>%
#   mutate(
#     total_sum = sum(heads + lb + Acre_feet),
#     across(everything(), ~ first(.))
#   )


load("C:/Users/lss284/OneDrive - Northern Arizona University/Desktop/Projects/SNOWPACS/data/Irrigation_data/county_shapefile.Rdata")

states.keep <- c("Nevada" ,              
                 "Utah"  ,
                 "Colorado",     
                 "Arizona",              
                 "New Mexico" ,"California")
states.keep2 <- toupper(states.keep)


crop_data_list <- Livestock_water_dat_county





pram <- names(crop_data_list)

crop_data_df <- bind_rows(crop_data_list)

write.csv(crop_data_df, "Livestock_dat.csv")
library(tmap)
library(tmaptools)
tmap_list<- list()
tmap_mode("plot")
tmap_mode("view")
i = 1
library(sf)
library(viridis)



# Loop through each crop
for (i in 1:length(crop_data_list)) {
  subset_shape <- subset(shape, Name %in% states.keep2)
  subset_shape@data <- left_join(subset_shape@data, crop_data_list[[i]][,c(1:4,9:13)], by=c( "GEOID" = "fips"))
  
  # max_value <- max(subset_shape@data["Total_water_use"],na.rm=TRUE)
  # tbreaks <- 10^(0:ceiling(log10(max_value)))
  # tbreaks <- c(0,tbreaks)
  
  map <- tm_shape(subset_shape) +
    tm_borders() +
    tm_fill("Total_water_use",  
            title = "Acre feet",
             n=5, style="kmeans",
            # breaks = tbreaks,
            textNA = "No Data",
            # alpha = 0.8, 
            id = "NAME"
    ) +
    # tm_text("Crop", size = 0.5)+
    tm_layout(main.title = paste0("Water Needs: ",names(crop_data_list)[i]))
  
  
  
  tmap_list[[i]] <- map
}

# Arrange the plots using tmap_arrange
combined_plot <- tmap_arrange(tmap_list[[1]],tmap_list[[2]],tmap_list[[3]],tmap_list[[4]],
                              ncol = 2,nrow = 2)

combined_plot
          tmap_save(combined_plot,"combined_plot_water2.png", width = 12, height = 10, units = 'in')


for (i in 1:4) {
  tmap_save(tmap_list[[i]],paste0(names(crop_data_list)[i]),"combined_plot_water.html")
  
}

















































