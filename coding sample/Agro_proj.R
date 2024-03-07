# Machine Learning project for team8
# Thur Apr 27
library(raster)
library(lubridate)
library(tiff)
library(tidyverse)
library(sf)
library(ggplot2)
library(geosphere)
library(openxlsx)

#####################################################################
# part 1: find out the solar system
# read the solar installation data
solar_install <- read.csv("Fort_Collins_Solar_Installations.csv")
solar_install$Date.of.Service <- mdy_hms(solar_install$Date.of.Service)
solar_install <-solar_install %>% filter(Georeference.System.Address != "")
solar_install$lon <- 0
solar_install$lat <- 0

# clean the lon & lat
for(i in 1:dim(solar_install)[1]){
  formula_str <- solar_install$Georeference.System.Address[i]
  latlon <- strsplit(formula_str, "\n")[[1]][3]
  matches <- regmatches(latlon, gregexpr("-?[0-9]+\\.[0-9]+", latlon))
  solar_install$lat[i] <- as.numeric(matches[[1]][1])
  solar_install$lon[i] <- as.numeric(matches[[1]][2])
}

# delete the variables do not need
solar_install <- solar_install %>% 
  dplyr::select(Date.of.Service, System.Address,System.Capacity.kW.DC, lon,lat)

# create table to save the data
agro_sum <- data.frame(index = numeric(), lon = numeric(), lat = numeric(), value = numeric())
for(year in 2015:2021){
  
  # get the solar system after the year
  solar_aft <- solar_install[year(solar_install$Date.of.Service) >= year,]
  
  # the data range is (-105.0014,-105.1554) and (40.48083, 40.63671)
  # read the tiff data of farmland
  filename = paste0("CDL_", as.character(year), "_08069.tif")
  raster_data <- raster(filename)
  
  # extract the values and coordinates
  df <- data.frame(
    value = as.vector(raster_data[]),
    lon = xyFromCell(raster_data, 1:length(raster_data[]))[,1],
    lat = xyFromCell(raster_data, 1:length(raster_data[]))[,2]
  )
  
  # remove rows with missing values
  df <- na.omit(df)
  
  # filter the pixel values
  p_list = list(0, 142, 152, 176, 121, 122, 111, 123, 195, 190, 37, 131, 141, 124, 112, 143)
  df_crop <- df %>% filter(!value %in% p_list)
  
  # re-project the coordinates
  df_crop <- df_crop %>% 
    st_as_sf(coords = c("lon", "lat"), crs = "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m") %>% 
    st_transform("+proj=longlat +datum=WGS84") %>%
    st_coordinates() %>% 
    as.data.frame() %>% 
    rename(lon = X, lat = Y) %>% 
    cbind(df_crop["value"])
  
  df_crop <- df_crop %>%
    filter(lon < -105.0013 & lon > -105.1554 & lat < 40.637 & lat > 40.48)
  
  # select corresponding columns for solar system
  df_solar <- solar_aft %>%
    dplyr::select(lon, lat) %>%
    mutate(value = 0)
  
  # find out the possible system
  agro_photovoltaic <- data.frame(index = numeric(), lon = numeric(), lat = numeric(), value = numeric())
  dis <- matrix(0, nrow = nrow(df_crop), ncol = nrow(df_solar))
  for (i in 1:nrow(df_solar)){
    dis <- distHaversine(df_solar[i, c("lon", "lat")], df_crop[ ,c("lon", "lat")])
    if(min(dis) < 30){
      agro_photovoltaic <- rbind(agro_photovoltaic, 
                          data.frame(index = i, lon = df_solar[i, "lon"], lat = df_solar[i, "lat"], value = df_crop[which(dis == min(dis)) ,"value"]))
    }
  }
  
  # add other information into the data
  agro_photovoltaic$year <- year
  agro_photovoltaic$date <- solar_aft[agro_photovoltaic$index, "Date.of.Service"]
  agro_photovoltaic$address <- solar_aft[agro_photovoltaic$index, "System.Address"]
  agro_photovoltaic$capacity <- solar_aft[agro_photovoltaic$index, "System.Capacity.kW.DC"]

  # combine the data
  agro_sum <- rbind(agro_sum, agro_photovoltaic)
}

# read the table of pixel values
pixel_val <- read.csv("cdl_2022_08069.csv")

# match the crop information
agro_sum$crop <- pixel_val$Category[match(agro_sum$value, pixel_val$Value)]

# output the data
write.xlsx(agro_sum, "agro_photovoltaic.xlsx")

# code to check the result
# df <- rbind(df_crop, df_solar)
# code to generate the plot to view the points in map
# ggplot(data = df, aes(x = lon, y = lat, color = value)) +
#   geom_point() +
#   scale_color_gradient(low = "white", high = "blue") +
#   labs(title = "Combined Plot", x = "Longitude", y = "Latitude")

################################################################
# part 2: find out the monitors
aqs_monitors <- read.csv("aqs_monitors.csv")
aqs_monitors <- aqs_monitors %>% 
  filter(Longitude < -105.0013 & Longitude > -105.1554 & 
           Latitude < 40.637 & Latitude > 40.48)

agro_sum_lonlat <- unique(agro_sum[, c("lon", "lat")])
monitor_lonlat <- unique(aqs_monitors[, c("Longitude", "Latitude")])

for (i in 1:nrow(agro_sum_lonlat)){
  dis <- distHaversine(agro_sum[i, c("lon", "lat")], aqs_monitors[, c("Longitude", "Latitude")])
  print(min(dis))
}
colnames(monitor_lonlat) <- c("lon", "lat")
agro_sum_lonlat$value = 1
monitor_lonlat$value = 0
df <- rbind(agro_sum_lonlat, monitor_lonlat)

##########################
# part 3: transfer to soil data
library(httr)
library(jsonlite)
library(openxlsx)

# figure out the lon and lat for photovoltaic system
agro_sum_lonlat <- agro_sum %>% 
  filter(value == 24) %>% 
  dplyr::select(lon, lat) %>%
  unique()

# for every single point, we select 3 different points outside the 
scale <- list(0.0025, 0.0035, 0.005)
compare_dat <- data.frame(lon = as.numeric(), lat = as.numeric())
for(i in 1:3){
  df1 <- data.frame(lon = agro_sum_lonlat$lon + scale[[i]],
                    lat = agro_sum_lonlat$lat + scale[[i]])
  df2 <- data.frame(lon = agro_sum_lonlat$lon + scale[[i]],
                    lat = agro_sum_lonlat$lat - scale[[i]])
  df3 <- data.frame(lon = agro_sum_lonlat$lon - scale[[i]],
                    lat = agro_sum_lonlat$lat + scale[[i]])
  df4 <- data.frame(lon = agro_sum_lonlat$lon - scale[[i]],
                    lat = agro_sum_lonlat$lat - scale[[i]])
  compare_dat <- unique(rbind(compare_dat, df1, df2, df3, df4))
}

# filter the compare_dat 
compare_dat_used <- data.frame(ilon = numeric(), lat = numeric())
for (i in 1:nrow(compare_dat)){
  dis <- distHaversine(compare_dat[i, c("lon", "lat")], 
                       agro_sum_lonlat[ ,c("lon", "lat")])
  if(min(dis) > 250){
    compare_dat_used <- rbind(compare_dat_used, compare_dat[i, c("lon", "lat")])
  }
}

compare_dat <- compare_dat_used 
  
data_lonlat_sum <- data.frame(label = as.character(), 
                             values.mean = as.numeric(),
                             lon = as.numeric(),
                             lat = as.numeric(),
                             property = as.character())
for (i in 1:dim(agro_sum_lonlat)[1]){
  # extract solar data
  # Define the URL for the SoilGrids REST API
  lon <- agro_sum_lonlat[i, "lon"]
  lat <- agro_sum_lonlat[i, "lat"]
  properties <- c("bdod", "cec", "cfvo", "clay", "nitrogen", "ocd", "ocs", "phh2o", "sand", "silt", "soc")
  depths <- c("0-5cm", "5-15cm", "30-60cm","100-200cm")
  values <- c("mean")
  url <- paste0("https://rest.isric.org/soilgrids/v2.0/properties/query?",
                "lon=", lon,
                "&lat=", lat,
                "&property=", paste0(properties, collapse = "&property="),
                "&depth=", paste0(depths, collapse = "&depth="),
                "&value=", paste0(values, collapse = "&value="))
  
  # Send a GET request to the API
  response <- GET(url)
  
  # Extract the soil attribute data
  data <- content(response, "text") %>% fromJSON(flatten = TRUE)
  
  data_lonlat <- data.frame(label = as.character(), 
                            values.mean = as.numeric(),
                            lon = as.numeric(),
                            lat = as.numeric(),
                            property = as.character())
  # convert into dataframe
  for (j in 1:10){
    q <- data.frame(data$properties$layers$depths[[j]])
    q <- q %>% dplyr::select(label, values.mean)
    q$lon <- agro_sum_lonlat[i, "lon"]
    q$lat <- agro_sum_lonlat[i, "lat"]
    q$property <- data$properties$layers$name[j]
    data_lonlat <- rbind(data_lonlat, q)
  }
  data_lonlat_sum <- rbind(data_lonlat_sum,data_lonlat)
}

data_lonlat_sum$solar <- 1

data_lonlat_sum2 <- data.frame(label = as.character(), 
                             values.mean = as.numeric(),
                             lon = as.numeric(),
                             lat = as.numeric(),
                             property = as.character())
# get data without solar system
for (i in 1:dim(compare_dat)[1]){
  # extract solar data
  # Define the URL for the SoilGrids REST API
  lon <- compare_dat[i, "lon"]
  lat <- compare_dat[i, "lat"]
  properties <- c("bdod", "cec", "cfvo", "clay", "nitrogen", "ocd", "ocs", "phh2o", "sand", "silt", "soc")
  depths <- c("0-5cm", "5-15cm", "30-60cm","100-200cm")
  values <- c("mean")
  url <- paste0("https://rest.isric.org/soilgrids/v2.0/properties/query?",
                "lon=", lon,
                "&lat=", lat,
                "&property=", paste0(properties, collapse = "&property="),
                "&depth=", paste0(depths, collapse = "&depth="),
                "&value=", paste0(values, collapse = "&value="))
  
  # Send a GET request to the API
  response <- GET(url)
  
  # Extract the soil attribute data
  data <- content(response, "text") %>% fromJSON(flatten = TRUE)
  
  data_lonlat <- data.frame(label = as.character(), 
                            values.mean = as.numeric(),
                            lon = as.numeric(),
                            lat = as.numeric(),
                            property = as.character())
  # convert into dataframe
  for (j in 1:10){
    q <- data.frame(data$properties$layers$depths[[j]])
    q <- q %>% dplyr::select(label, values.mean)
    q$lon <- data$geometry$coordinates[1]
    q$lat <- data$geometry$coordinates[2]
    q$property <- data$properties$layers$name[j]
    data_lonlat <- rbind(data_lonlat, q)
  }
  data_lonlat_sum2 <- rbind(data_lonlat_sum2,data_lonlat)
}

data_lonlat_sum2$solar <- 0

# get final data
fin_data <- unique(rbind(data_lonlat_sum, data_lonlat_sum2))
agro_sum_1 <- agro_sum %>% 
  filter(value == 24) %>% 
  dplyr::select(lon, lat, capacity) %>%
  unique()

df3 <- left_join(fin_data, agro_sum_1, by = c("lon" = "lon", "lat" = "lat"))
df3 <- df3 %>% filter(!is.na(values.mean))
df3_pivot <- df3 %>% pivot_wider(names_from = c(label,property), values_from = values.mean)


# Assume the dataframe is called df
cols_to_check <- 5:44

# Find all rows with duplicate values in the specified columns
duplicates <- df3_pivot[duplicated(df3_pivot[, cols_to_check]) | duplicated(df3_pivot[, cols_to_check], fromLast = TRUE), ]

# Randomly select one row for each unique combination of values in the specified columns
unique_rows <- duplicates %>% group_by(across(all_of(cols_to_check))) %>% sample_n(1)

# Remove the duplicate rows from the original dataframe
df_filtered <- df3_pivot[!duplicated(df3_pivot[, cols_to_check]), ]

# Combine the unique rows with the filtered dataframe to get the final result
result <- rbind(df_filtered, unique_rows)

df3_fin <- semi_join(df3, result, by = c("lon", "lat"))

# output the data
write.xlsx(df3_fin, "soil_solar_data.xlsx")


##########################
# part 4: initial visualization
library(ggplot2)
data <- read.xlsx("soil_solar_data.xlsx")

tmp <- data %>% filter(label == "5-15cm" & property == "cfvo")
# create box plot
ggplot(tmp, aes(group=solar, y=values.mean)) +
  geom_boxplot()

ggplot(data = data, aes(x = lon, y = lat, color = solar)) +
  geom_point() +
  scale_color_gradient(low = "white", high = "blue") +
  labs(title = "Combined Plot", x = "Longitude", y = "Latitude")
