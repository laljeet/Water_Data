library("tidyverse")
library(tmap)
library(sf)

setwd("C:/Users/lss284/OneDrive - Northern Arizona University/Desktop/Projects/SNOWPACS/data/Irrigation_data")

# # Adapted from FRIS-IWMS-Manuscript-Viz
ctys <- readRDS("county.RDS")
w_ctys <- readRDS("western_ctys.RDS")
state <- readRDS("states.RDS")

load("counties_shp.Rdata")
Irrigation_decision <- readRDS("info-prop-indyrs.RDS")

Irrigation_decision <- Irrigation_decision %>% filter(YEAR == 2018) %>% select(!c("total")) %>% gather(category, proportion, 5:12) %>%   mutate(
  category = case_when(
    category == "K1040" ~ "K1040 - Extension or university",
    category == "K1041" ~ "K1041 - Private consultants",
    category == "K1042" ~ "K1042 - Equipment dealers",
    category == "K1043" ~ "K1043 - Irrigation district",
    category == "K1044" ~ "K1044 - Government",
    category == "K1045" ~ "K1045 - Press",
    category == "K1046" ~ "K1046 - Neighbors",
    category == "K1047" ~ "K1047 - Internet"
  )
)

# write.csv(Irrigation_decision, "Irrigation_decision.csv")

unique(Irrigation_decision$STATE_ALPHA)

keep.states <- c("ARIZONA", "COLORADO", "NEVADA","NEW MEXICO", "UTAH", "CALIFORNIA")

Irrigation_decision <- Irrigation_decision %>% 
  filter(STATE_ALPHA %in% keep.states)

 # 


# rename variables for nicer viz
Irrigation_decision <- Irrigation_decision %>% 
  mutate(
    category = case_when(
      category == "K1040 - Extension or university" ~ "Extension or university ",
      category == "K1041 - Private consultants" ~ "Private consultants ", 
      category == "K1042 - Equipment dealers" ~ "Equipment dealers ",
      category == "K1043 - Irrigation district" ~ "Irrigation district ",
      category == "K1044 - Government" ~ "Government ",
      category == "K1045 - Press" ~ "Press ",
      category == "K1046 - Neighbors" ~ "Neighbors ",
      category == "K1047 - Internet" ~ "Internet"
    )
  )

Irrigation_decision$category <- as.factor(Irrigation_decision$category)
# Irrigation_decision$category <- ordered(Irrigation_decision$category, levels = c("Neighbors (82)", "Extension or university (46)", "Private consultants (32)", "Equipment dealers (23)", "Irrigation district (21)","Government (9)","Internet (4)","Press (1)"))

colnames(Irrigation_decision)[5] <- "Information Source"

row.names(Irrigation_decision) <- NULL

prams <- unique(Irrigation_decision$`Information Source`)
write.csv(Irrigation_decision, "Irrigation_decision.csv")

shape <- readOGR(dsn = "C:/Users/lss284/Downloads/tl_2022_us_county", layer = "tl_2022_us_county")

# save(shape, file="county_shapefile.Rdata")
bbox_new <- st_bbox(shape) 
bbox_new[1] <- -124.736342
bbox_new[2] <- 24.7433195 
bbox_new[3] <- -66.945392
bbox_new[4] <- 49.382808

i=1
for (i in 1:length(prams)) {
  
  
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
  
  pram_data <- Irrigation_decision %>% 
    filter(`Information Source` == prams[i] & proportion >0)
  
  shape2@data <- left_join(shape2@data, pram_data, by = "GEOID")
  
  tmap_mode(mode = "plot")
  
  tm_map <- tm_shape(shape2)+
    tm_polygons(col = "proportion",
                title = paste0("Proportion"),
                # n=5,style="jenks",
                breaks = c(0,10,25,50,75,100),
                textNA = "No Data",
                colorNA="grey",
                id="NAME")
  tm_map <- tm_map +
    tm_layout( main.title =  paste0("Irrigation Source: ", prams[i]))
  
  
  tmap_save(tm_map, paste0("Irrigation decisions", prams[i],".png"),  width = 10, height = 6, units = 'in')
}




#################################################################################################
# Irrigation Scheduling method

Irrigation_method <- readRDS("sched-prop-indyrs.RDS")

keep.states <- c("ARIZONA", "COLORADO", "NEVADA","NEW MEXICO", "UTAH", "CALIFORNIA")

Irrigation_method <- Irrigation_method %>% 
  filter(STATE_ALPHA %in% keep.states)

 write.csv(Irrigation_method, "Irrigation_method.csv")


Irrigation_method <- Irrigation_method %>% filter(YEAR == 2018) %>% select(!c("total")) %>% gather(category, proportion, 5:14) %>% 
  mutate(
    category = case_when(
      category == "K1020" ~ "K1020 - Condition of crop",
      category == "K1021" ~ "K1021 - Feel of the soil",
      category == "K1022" ~ "K1022 - Soil moisture sensor",
      category == "K1023" ~ "K1023 - Plant moisture sensor",
      category == "K1024" ~ "K1024 - Scheduling service",
      category == "K1025" ~ "K1025 - Daily crop-water ET",
      category == "K1026" ~ "K1026 - Water delivered in turn",
      category == "K1027" ~ "K1027 - Personal calendar",
      category == "K1028" ~ "K1028 - Computer simulation models",
      category == "K1029" ~ "K1029 - When neighbors watered"
    )
  )


# rename variables for nicer viz
Irrigation_method <- Irrigation_method %>% 
  mutate(
    category = case_when(
      category == "K1020 - Condition of crop" ~ "Condition of crop",
      category == "K1021 - Feel of the soil" ~ "Feel of the soil",
      category == "K1022 - Soil moisture sensor" ~ "Soil moisture sensor",
      category == "K1023 - Plant moisture sensor" ~ "Plant moisture sensor",
      category == "K1024 - Scheduling service" ~ "Scheduling service",
      category == "K1025 - Daily crop-water ET" ~ "Daily crop-water ET",
      category == "K1026 - Water delivered in turn" ~ "Water delivered in turn",
      category == "K1027 - Personal calendar" ~ "Personal calendar",
      category == "K1028 - Computer simulation models" ~ "Computer simulation models",
      category == "K1029 - When neighbors watered" ~ "When neighbors watered"
    )
  )


prams <- unique(Irrigation_method$category)

# write.csv(Irrigation_method, "Irrigation_methods.csv")

shape <- readOGR(dsn = "C:/Users/lss284/Downloads/tl_2022_us_county", layer = "tl_2022_us_county")

# save(shape, file="county_shapefile.Rdata")
bbox_new <- st_bbox(shape) 
bbox_new[1] <- -124.736342
bbox_new[2] <- 24.7433195 
bbox_new[3] <- -66.945392
bbox_new[4] <- 49.382808

i=1
for (i in 1:length(prams)) {
  
  
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
  
  pram_data <- Irrigation_method %>% 
    filter(category == prams[i] & proportion >0)
  
  shape2@data <- left_join(shape2@data, pram_data, by = "GEOID")
  
  tmap_mode(mode = "plot")
  
  tm_map <- tm_shape(shape2)+
    tm_polygons(col = "proportion",
                title = paste0("Proportion"),
                # n=5,style="jenks",
                breaks = c(0,10,25,50,75,100),
                textNA = "No Data",
                colorNA="grey",
                id="NAME")
  tm_map <- tm_map +
    tm_layout( main.title =  paste0("Irrigation Method: ", prams[i]))
  


# tm_map
  
  tmap_save(tm_map, paste0("Irrigation category", prams[i],".png"),  width = 10, height = 6, units = 'in')
}
