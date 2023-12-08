#Find all stations with a temperature p-value less than 0.05
# Load the necessary library
library(dplyr)

# Initialize an empty data frame to store the combined filtered data
combined_filtered <- data.frame()

# Loop through each file and process
for (i in 1:20) {
  # Construct the file name
  file_name <- paste('test_results/',i, ".csv", sep = "")
  
  # Read the file
  data <- read.csv(file_name)
  
  # Filter rows where mk_temp < 0.05 and append to the combined data frame
  filtered_data <- filter(data, mk_temp < 0.05)
  combined_filtered <- rbind(combined_filtered, filtered_data)
}

# Write the combined filtered data to a new CSV file
write.csv(combined_filtered, 'temincrease.csv', row.names = FALSE)


summary_data <- read.csv('summary.csv')
temincrease_data <- read.csv('temincrease.csv')

merged_data <- merge(temincrease_data,summary_data[,c('STATION','LATITUDE','LONGITUDE')],by = 'STATION')

write.csv(merged_data,'tem.csv', row.names = FALSE)



#Find all stations with precipitation p-value less than 0.05
combined_filtered <- data.frame()

# Loop through each file and process
for (i in 1:20) {
  # Construct the file name
  file_name <- paste('test_results/',i, ".csv", sep = "")
  
  # Read the file
  data <- read.csv(file_name)
  
  # Filter rows where mk_prcp < 0.05 and append to the combined data frame
  filtered_data <- filter(data, mk_prcp < 0.05)
  combined_filtered <- rbind(combined_filtered, filtered_data)
}

# Write the combined filtered data to a new CSV file
write.csv(combined_filtered, 'prcincrease.csv', row.names = FALSE)


summary_data <- read.csv('summary.csv')
temincrease_data <- read.csv('prcincrease.csv')

merged_data <- merge(temincrease_data,summary_data[,c('STATION','LATITUDE','LONGITUDE')],by = 'STATION')

write.csv(merged_data, 'prc.csv', row.names = FALSE)


#Statistics of years with sudden changes in temperature
# Initialize an empty data frame
combined_data <- data.frame()

# Loop through each file
for (i in 1:20) {
  file_name <- paste('test_results/',i, ".csv", sep = "")
  data <- read.csv(file_name)
  
  # Filter out rows where cpm_temp is not NA
  data <- data[!is.na(data$cpm_temp), ]
  
  # Add 1999 to cpm_temp and rename it to years, select only years and STATION
  data$years <- data$cpm_temp + 1999
  selected_data <- data[c('years', 'stations')]
  
  # Combine the data
  combined_data <- rbind(combined_data, selected_data)
}

# Write the combined data to a new CSV file
write.csv(combined_data, "tempyears.csv", row.names = FALSE)



#Statistics on years with sudden changes in precipitation
# Initialize an empty data frame
combined_data <- data.frame()

# Loop through each file
for (i in 1:20) {
  file_name <- paste('test_results/',i, ".csv", sep = "")
  data <- read.csv(file_name)
  
  # Filter out rows where cpm_prcp is not NA
  data <- data[!is.na(data$cpm_prcp), ]
  
  # Add 1999 to cpm_prcp and rename it to years, select only years and STATION
  data$years <- data$cpm_prcp + 1999
  selected_data <- data[c('years', 'stations')]
  
  # Combine the data
  combined_data <- rbind(combined_data, selected_data)
}

# Write the combined data to a new CSV file
write.csv(combined_data, "prcpyears.csv", row.names = FALSE)

#Draw a distribution plot for years with sudden changes in temperature
data <- read.csv('tempyears.csv')
year_range <- max(data$years) - min(data$years)
ggplot(data, aes(x = years)) +
  geom_histogram(bins = year_range + 1, fill = "orange", color = "black") +
  labs(title = "Distribution of Temperature Mutant at Global Stations", 
       x = "Years", 
       y = "Number of Mutant") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.4))
#Draw a distribution plot for years with sudden changes in precipitation
data <- read.csv('prcpyears.csv')
year_range <- max(data$years) - min(data$years)
ggplot(data, aes(x = years)) +
  geom_histogram(bins = year_range + 1, fill = "dodgerblue", color = "black") +
  labs(title = "Distribution of Precipitation Mutant at Global Stations", 
       x = "Years", 
       y = "Number of Mutant") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.4))

#Draw a time series graph of mutation points

summary_data <- read.csv('summary.csv')

#CHM00053898
data <- summary_data[summary_data$STATION == 'CHM00053898', 'AVG_TAVG']
print(data)
plot <-ggplot(df, aes(x =2000:2023, y = data)) +
  geom_line(color = "steelblue", size = 1) +  # 添加蓝色折线
  geom_vline(xintercept = 2013, color = "red", size = 1) +  # 在2013年处添加垂直线
  labs(title = "Annual Average Temperature of Station ANYANG, CH", x = "Year", y = "Temperature") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.47))
plot <-plot + coord_fixed(ratio = 0.3)
ggsave("avgtemp.png", plot, width = 10, height = 6)

#percipotation

#USW00014897
data <- summary_data[summary_data$STATION == 'USW00014897', 'AVG_PRCP']
print(data)
plot <-ggplot(df, aes(x =2000:2023, y = data)) +
  geom_line(color = "steelblue", size = 1) + 
  geom_vline(xintercept = 2012, color = "red", size = 1) +
  labs(title = "Annual Average Precipitation of Station WAUSAU ASOS, WI US", x = "Year", y = "Precipitation") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.47))

plot <-plot + coord_fixed(ratio = 0.8)
plot
ggsave("avgprcp.png", plot, width = 10, height = 6)

###map

library(ggplot2)
library(dplyr)
#Stations with significantly higher temperatures
# Read the datasets
summary_data <- read.csv('summary.csv')
temincrease_data <- read.csv('temincrease.csv')

# Check if stations in summary_data are in temincrease_data
summary_data$in_temincrease <- summary_data$STATION %in% temincrease_data$STATION

# Plotting
summary_data <- summary_data %>%
  mutate(legend = ifelse(in_temincrease, 'Increase', 'Decrease or Unchange'))

ggplot() +
  geom_point(data = summary_data, aes(x = LONGITUDE, y = LATITUDE, color = legend)) +
  labs(title = 'Tempreture Change of Stations', x = 'Longitude', y = 'Latitude') +
  scale_color_manual(values = c('Increase' = 'orange', 'Decrease or Unchange' = 'green4'),
                     labels = c('Increase' = 'Increase', 'Decrease or Unchange' = 'Decrease or Unchange')) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.4),  
        legend.position = c(0.14, 0.14)) 
plot
ggsave("maptemp.png", plot, width = 10, height = 6)

#Stations with significantly higher precipitation
# Read the datasets
summary_data <- read.csv('summary.csv')
temincrease_data <- read.csv('prcincrease.csv')

# Check if stations in summary_data are in temincrease_data
summary_data$in_temincrease <- summary_data$STATION %in% temincrease_data$STATION

# Plotting
summary_data <- summary_data %>%
  mutate(legend = ifelse(in_temincrease, 'Increase', 'Decrease or Unchange'))

plot <-ggplot() +
  geom_point(data = summary_data, aes(x = LONGITUDE, y = LATITUDE, color = legend), alpha = 0.1,size=1) + 
  labs(title = 'Precipitation Change of Stations', x = 'Longitude', y = 'Latitude') +
  scale_color_manual(values = c('Increase' = 'dodgerblue', 'Decrease or Unchange' = 'moccasin'),
                     labels = c('Increase', 'Decrease or Unchange')) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.45),  
        legend.position = c(0.14, 0.14))
plot <-plot + coord_fixed(ratio = 0.8)
plot
ggsave("mapprcp.png", plot, width = 10, height = 6)
