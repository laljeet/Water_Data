library(CropScapeR)
library(tidyverse)
library(tmap)
library(tmaptools)

options(scipen = 9999999)
library(tidyverse)
rm(list = ls(all.names = TRUE))

setwd("C:/Users/lss284/OneDrive - Northern Arizona University/Desktop/Projects/SNOWPACS/data/CDL")


### Load FIPS code csv
fips_csv <- read.csv("FIPS.csv")
keep.states <- c("AZ","CA", "CO", "NV","UT","NM")

fn_CDL_download <- function(Year,keep.states){
  fips_csv_AL <- fips_csv_AL %>%
  data_list <- list()
    
  for (i in 1:nrow(fips_csv_AL)) {
 
  data_list[[i]] <- GetCDLComp(aoi = fips_csv_AL$fips[i], year1 = Year, year2 = Year, type = 'f')
    
  }
}



# Remove Alaska and Hawai

#  fips_csv_AL = fips_csv %>%
#    filter(state != "AK" &
#            state != "HI")
# #
#  keep.states <- c("AZ","CA", "CO", "NV","UT","NM")
# # fips_csv_AL = fips_csv %>%
# #    filter(state == "AL")
# # #
# # # ### Empty list
#  keep.states <- c("CA")
# 
#  fips_csv_AL <- fips_csv_AL %>%
#    filter(state %in% keep.states)
# 
#  data_list <- list()
# 
#  ### Download data for all counties
  Year = 2017
# 
#  for (i in 1:nrow(fips_csv_AL)) {
# 
#    data_list[[i]] <- GetCDLComp(aoi = fips_csv_AL$fips[i], year1 = Year, year2 = Year, type = 'f')
# 
# 
# }
# 
#  names(data_list) <- fips_csv_AL$fips
# 
#  # save the datafile fro future use and easyload
#  data_list_CA <- data_list
# save(data_list_CA, file = paste0("Original_counties_CDL_CA.Rdata"))

rm(list = ls(all.names = TRUE))

load("Original_counties_CDL.Rdata")
load("Original_counties_CDL_CA.Rdata")

data_list <- c(data_list,data_list_CA)
# Convert list to dataframe
AL_counties <- purrr::map_df(data_list, tibble::as_tibble)
AL_counties <- AL_counties[,-1]

# write.csv(AL_counties, paste0("Original_Allcountiesdata",Year,".csv"), row.names =  FALSE)

####################################################################################################
# Look up table for all land uses
Year = 2017
AL_counties <- read.csv(paste0("Original_Allcountiesdata",Year,".csv"))
Lookup_table <- read.csv("Lookup_table.csv")


Remove_LC_codes <- c(81:143,61, 63,64,65,152, 171,190,195) # 61 is fallow

Remove_layers <-subset(Lookup_table, !(LC_code %in% Remove_LC_codes))
  

# Remove the landuses from dataset

AL_county_crops <- subset(AL_counties, (To %in% Remove_layers$LC_type))


#write.csv(AL_county_crops, paste0(keep.states,"AL_county_crops",Year,".csv"))
crops <- AL_county_crops 
unique(crops$To)
by_aoi <- split(crops, crops$aoi)

# Function to get row with max acreage 
get_max_row <- function(df){
  
  # Get index of max acreage row
  max_idx <- which.max(df$Acreage)
  
  # Return full row
  return(df[max_idx,])
  
}

# Apply to each aoi
max_rows <- lapply(by_aoi, get_max_row)

# Bind rows into single dataframe
max_rows_df <- do.call(rbind, max_rows)

Max_crop_county <- max_rows_df

save(Max_crop_county, file ="Max_crop_county.Rdata")


#####################################################################################################
# Function to get rows meeting cutoff 
get_top_rows <- function(df, cutoff = 0.90){
  
  # Calculate total acres
  total_acres <- sum(df$Acreage)
  
  # Sort crops by acreage
  sorted_df <- df[order(-df$Acreage),]
  

  # Cumulative acreage percentage
  sorted_df$cumper <- cumsum(sorted_df$Acreage) / total_acres
  
  first_row_above_0.90 <- which(sorted_df$cumper > cutoff)[1]
  # Filter rows meeting cutoff
  top_rows <- subset_df <- sorted_df[1:first_row_above_0.90, ]
  
  return(top_rows)
  
}

# Apply to each aoi 
top_rows_list <- lapply(by_aoi, get_top_rows)

# Bind into single dataframe
top_rows_df <- do.call(rbind, top_rows_list)
Top_90pct_crop_county <- top_rows_df
rownames(Top_90pct_crop_county) <- NULL

save(Top_90pct_crop_county,file="Top_90pct_crop_countyOct.Rdata")

########################################################################################################
# Plots
rm(list = ls(all.names = TRUE))
load("Max_crop_county.Rdata")


crop_data <- Max_crop_county
 crop_data <- crop_data[,c(1,3,4)]
 
# Label columns
colnames(crop_data) <- c( "Crop", "Acres","fips")
crop_data$fips <- sprintf("%05d", crop_data$fips)



FIPS <- read.csv("https://raw.githubusercontent.com/laljeet/MY_files/main/FIPS2.csv")
FIPS$fips <- sprintf("%05d", FIPS$fips)
FIPS$fips <- as.character(FIPS$fips)
FIPS <- FIPS[,-c(1)]


crop_data <- left_join(crop_data, FIPS , by = "fips")

load("C:/Users/lss284/OneDrive - Northern Arizona University/Desktop/Projects/SNOWPACS/data/Irrigation_data/county_shapefile.Rdata")


states.keep <- c("CALIFORNIA","Nevada" ,              
                 "Utah"  ,
                 "Colorado",     
                 "Arizona",              
                 "New Mexico" )
states.keep2 <- toupper(states.keep)
subset_shape <- subset(shape, Name %in% states.keep2)

subset_shape@data <- left_join(subset_shape@data, crop_data[,-c(6)], by = c("GEOID"= "fips"))

tmap_mode("view")
tmap_mode("plot")
map <- tm_shape(subset_shape) +
  tm_borders() +
  tm_fill("Acres",  
          n=5, style="jenks",
          textNA = "No Data",
          alpha = 0.8, 
          id = "name") +
  tm_text("Crop", size = 1)
map<- map +
    tm_layout( main.title =  "Maximum acreage and respective crop in each county")

map
# tmap_save(map, "Max_acreage.png",  width = 10, height = 8, units = 'in')


tmap_mode("plot")
map <- tm_shape(subset_shape) +
  tm_borders() +
  tm_fill("Crop",  palette = "Paired",
          # n=5, style="jenks",
          textNA = "No Data",
          # alpha = 0.8, 
          id = "name") 
# +
#   tm_text("Crop", size = 1)
map<- map +
  tm_layout( main.title =  "Maximum acreage crop in each county")

map
tmap_save(map, "Max_acreage_Crop.png",  width = 10, height = 8, units = 'in')

# tmap_save(map, "Max_acreage_Crop.html")


##############################################################################################################
rm(list = ls(all.names = TRUE))
# load("Top_90pct_crop_county.Rdata")
load("Top_90pct_crop_countyOct.Rdata")

## Top 90% crops gives the important and high acreage crops WITHIN each county 
#3 Let's get crops between different counties that are common and result in high acreage.

crop_data <- Top_90pct_crop_county
crop_data <- crop_data[,c(1,3,4)]

# Label columns
colnames(crop_data) <- c( "Crop", "Acres","fips")

# Get all unique crops 
all_crops <- unique(crop_data$Crop)

# Count occurrence of each crop
crop_counts <- crop_data %>% 
  count(Crop)

# Arrange by decreasing n to get most repeated crops
top_crops <- crop_counts %>%
  mutate(Pct = round(n/sum(n),2)) %>% 
  arrange(desc(n)) 


get_top_rows2 <- function(df, cutoff = 0.90){
  
  # Calculate total acres
  total_acres <- sum(df$n)
  
  # Sort crops by acreage
  sorted_df <- df[order(-df$n),]
  
  
  # Cumulative acreage percentage
  sorted_df$cumper <- cumsum(sorted_df$n) / total_acres
  
  first_row_above_0.90 <- which(sorted_df$cumper > cutoff)[1]
  # Filter rows meeting cutoff
  top_rows <- subset_df <- sorted_df[1:first_row_above_0.90, ]
  
  return(top_rows)
  
}

top_crops <- get_top_rows2(top_crops)
top_crops$Crop <- gsub("/","_", top_crops$Crop)



top_crops <- top_crops$Crop
top_crops <- gsub("/","_", top_crops)




crop_data$Crop[crop_data$Crop =="Other Hay/Non Alfalfa"] <- c("Other Hay_Non Alfalfa")
crop_data <- crop_data %>% 
  filter(Crop %in% top_crops)


unique(crop_data$Crop)
FIPS <- read.csv("https://raw.githubusercontent.com/laljeet/MY_files/main/FIPS2.csv")
# FIPS$fips <- sprintf("%05d", FIPS$fips)
# FIPS$fips <- as.character(FIPS$fips)
FIPS <- FIPS[,-c(1)]

crop_data <- left_join(crop_data, FIPS , by = "fips")

# write.csv(crop_data, "Crop_data_acerage_oct.csv")
load("C:/Users/lss284/OneDrive - Northern Arizona University/Desktop/Projects/SNOWPACS/data/Irrigation_data/county_shapefile.Rdata")

states.keep <- c("Nevada" ,              
                 "Utah"  ,
                 "Colorado",     
                 "Arizona",              
                 "New Mexico",
                 "California")
states.keep2 <- toupper(states.keep)


crop_data_list <- split(crop_data, f = crop_data$Crop)
pram <- names(crop_data_list)
for (i in 1:length(pram)) {
  subset_shape <- subset(shape, Name %in% states.keep2)
  subset_shape@data <- left_join(subset_shape@data, crop_data_list[[i]][,-c(6)], by = c("GEOID"= "fips"))
  
  tmap_mode("view")
  tmap_mode("plot")
  map <- tm_shape(subset_shape) +
    tm_borders() +
    tm_fill("Acres",  
            n=5, style="jenks",
            textNA = "No Data",
            alpha = 0.8, 
            id = "name") +
    tm_text("Crop", size = 0.5)
  
  map<- map +
    tm_layout( main.title =  paste0(pram[i]," Acerage") )
  
  
  tmap_save(map, paste0(pram[i],"_acreage.png"),  width = 10, height = 8, units = 'in')
  
}

write.csv(crop_data,"Final_crops_data_oct.csv")

save(crop_data,crop_data_list,shape, file= "Rshiny_data.Rdata")




#####################################################
rm(list = ls(all.names = TRUE))
load("Rshiny_data.Rdata")
Crops_max <- unique(crop_data$Crop)
# Crop_demand <- read.csv("demand.csv")
Crop_demand <- read.csv("demand_FRIS.csv")

# df_split <- Crop_demand %>%
#   separate(Demand, into = c("Lower_Bound", "Upper_Bound"), sep = "-", convert = TRUE)
# 
# pattern <- paste(Crops_max, collapse = "|")

# Subset df_split to include only rows with Crop names partially matching Crops_max
# df_matched <- df_split[grep(pattern, df_split$Crop), ]

# df_matched[,c(2,3)] <- round(df_matched[,c(2,3)]*0.00328084,2) # Converted to feet
# rm(df_split,Crop_demand,pattern)

# crop_data2 <- crop_data %>%
#   left_join(df_matched, by = "Crop") %>%
#   mutate(
#     Upper_bound_water = round(Acres * Upper_Bound,0),
#     Lower_bound_water = round(Acres  * Lower_Bound,0),
#     Avg_water_use = (Upper_bound_water + Lower_bound_water) / 2
#   )




crop_data2 <- crop_data %>%
  left_join(Crop_demand, by = "Crop") 

crop_data2$Avg_water_use <- crop_data2$FRIS_Demand*crop_data2$Acres
  write.csv(crop_data2, "water_avaliable_oct.csv")

###############################################################
  # Water_avaliable.csv containes water eamnd based on FAO. Load that file if that is needed.
  # Water_avaliableoct.csv contains water demands from FRIS survey. The values here are higher
  
  
# colnames(crop_data2)[7] <- "Avg_water_use"
load("C:/Users/lss284/OneDrive - Northern Arizona University/Desktop/Projects/SNOWPACS/data/Irrigation_data/county_shapefile.Rdata")

states.keep <- c("Nevada" ,              
                 "Utah"  ,
                 "Colorado",     
                 "Arizona",              
                 "New Mexico" )
states.keep2 <- toupper(states.keep)


crop_data_list <- split(crop_data2, f = crop_data2$Crop)
pram <- names(crop_data_list)

crop_data_df <- bind_rows(crop_data_list)


tmap_list<- list()
tmap_mode("plot")
i = 1
library(sf)
library(viridis)
# Loop through each crop
for (i in 1:length(crop_data_list)) {
  subset_shape <- subset(shape, Name %in% states.keep2)
  subset_shape <- sp::merge(subset_shape, crop_data_list[[i]][,-c(6)], by.x = "GEOID", by.y = "fips")
  # max_value <- max(subset_shape@data[20],na.rm=TRUE)
  # tbreaks <- 10^(0:ceiling(log10(max_value)))
  # tbreaks <- c(0,tbreaks)

  map <- tm_shape(subset_shape) +
    tm_borders() +
    tm_fill("Avg_water_use",  
            title = "Water requirements (Acre-feet)",
             n=7, style="kmeans",
            # breaks = tbreaks,
            textNA = "No Data",
            # alpha = 0.8, 
            id = "name"
    ) +
    # tm_text("Crop", size = 0.5)+
    tm_layout(main.title = paste0(names(crop_data_list)[i]))
  
  
  
  tmap_list[[i]] <- map
}

# Arrange the plots using tmap_arrange
combined_plot <- tmap_arrange(tmap_list[[1]],tmap_list[[2]],tmap_list[[3]],tmap_list[[4]],tmap_list[[5]],tmap_list[[6]],
                              ncol = 3,nrow = 2)

combined_plot
tmap_save(combined_plot,"combined_plot_acreage2.png", width = 12, height = 10, units = 'in')


tmap_save(tmap_list[[1]],"Alfafa.png", width = 8, height = 6, units = 'in')


save(crop_data_df, file = "Acerage_demand_data.Rdata")


####################################################################################################################
# Optimization
#####################################################
rm(list = ls(all.names = TRUE))

setwd("C:/Users/lss284/OneDrive - Northern Arizona University/Desktop/Projects/SNOWPACS/data/CDL")
options(scipen = 9999999)
library(tidyverse)
library(DEoptim)
load("Acerage_demand_data.Rdata")
# crop_data_df <- crop_data_df[-c(7:10)]
crop_data_df$Avg_demand <- (crop_data_df$Upper_Bound+crop_data_df$Lower_Bound)/2
crop_data_df$Allocation <- crop_data_df$Avg_water_use
# Add a new column with the sum of Avg_water_use by fips


# Step 1: Group by fips and calculate the sum of Avg_water_use in each group
crop_data_df <- crop_data_df %>%
  group_by(fips) %>%
  mutate(total_avg_water_use = sum(Avg_water_use))

library(lpSolve)

# Assuming your dataframe is named crop_data_df

# Step 1: Group by fips and calculate the sum of Avg_water_use in each group
crop_data_df <- crop_data_df %>%
  group_by(fips) %>%
  mutate(total_avg_water_use = sum(Avg_water_use))

# Set up the linear programming problem
A <- matrix(rep(1, nrow(crop_data_df)), nrow = nrow(crop_data_df))  # Allocation variables sum to 1
b <- rep(1, nrow(crop_data_df))  # Constraints for allocation variables
dir <- "="  # Equality constraint
rhs <- 0.1 * crop_data_df$total_avg_water_use  # Right-hand side of the constraint

# Solve the linear programming problem
allocation_result <- lp("max", rep(0, nrow(crop_data_df)), A, dir, rhs, all.bin = TRUE, use.rw = TRUE)

# Assign the results back to the dataframe
crop_data_df$Allocation <- allocation_result$solution

# Step 3: Adjust the total Allocation to not exceed 250000
total_allocated <- sum(crop_data_df$Allocation)
excess_allocation <- max(total_allocated - 250000, 0)

# Adjust allocations if the total allocated is over the limit
if (excess_allocation > 0) {
  crop_data_df$Allocation <- crop_data_df$Allocation * (250000 / total_allocated)
}

# Display the updated dataframe
print(crop_data_df)


print(as_tibble(crop_data_df), n=100)
total_budget <- 100000000
Cost <- 400 #$ per acrefeet
Acrefeet_saved <- total_budget/Cost



target_reduction <- 250000

original_data <- crop_data_df$Sum_Avg_water_use
# Define the objective function to minimize
# Define the objective function to minimize
objective <- function(weights) {
  # Calculate the total reduction and penalty
  total_reduction <- sum(pmax(original_data - pmin(0.05 * original_data, weights), 0))
  penalty <- ifelse(total_reduction > target_reduction, abs(total_reduction - target_reduction) * 1e-6, 0)
  
  return(total_reduction + penalty)
}

# Define the bounds for individual water use reductions
upper_bounds <- pmin(0.05 * original_data, target_reduction)

# Define the bounds for individual water use reductions

lower_bounds <- rep(0, nrow(crop_data_df))


# Perform optimization
result <- DEoptim(objective, lower_bounds, upper_bounds,control = DEoptim.control(itermax = 1000))

# Get the optimized weight reductions
crop_data_df$reduction <- pmin(0.05 * original_data, result$optim$bestmem)
crop_data_df$optimized_weights <- pmax(original_data - pmin(0.05 * original_data, result$optim$bestmem), 0)
optimized_weights <- pmax(original_data - pmin(0.05 * original_data, result$optim$bestmem), 0)

cat("Original Avg_water_use values:", crop_data_df$Avg_water_use, "\n")
cat("Optimized weight reductions:", optimized_weights, "\n")
cat("Total reduction achieved:", sum(optimized_weights), "\n")


