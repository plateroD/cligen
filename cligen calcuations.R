FL1

# Assuming your data frame is named FL1
# Convert the 'mo' column to a factor to ensure it considers all months
FL1$mo <- factor(FL1$mo, levels = 1:12)

# Calculate the mean of 'w.vl' for each month
result <- aggregate(FL1$w.vl, by = list(Month = FL1$mo), FUN = mean)

# Rename the columns in the result data frame
colnames(result) <- c("Month", "Mean_w.vl")

# Print the result
print(result)

NM1<-read.csv("NM .csv")
# Assuming your data frame is named FL1

# Calculate the Maximum Month Mean for Solar Radiation
max_rad_mean <- aggregate(NM1$rad, by = list(Month = NM1$mo), FUN = max)

# Calculate the Minimum Month Mean for Solar Radiation
min_rad_mean <- aggregate(NM1$rad, by = list(Month = NM1$mo), FUN = min)

# Rename the columns in the result data frames
colnames(max_rad_mean) <- c("Month", "Max_Rad_Mean")
colnames(min_rad_mean) <- c("Month", "Min_Rad_Mean")

# Print the results
print(max_rad_mean)
print(min_rad_mean)

# Calculate the mean of Max_Rad_Mean
mean_max_rad <- mean(max_rad_mean$Max_Rad_Mean, na.rm = TRUE)

# Calculate the mean of Min_Rad_Mean
mean_min_rad <- mean(min_rad_mean$Min_Rad_Mean, na.rm = TRUE)

# Print the mean values
print(paste("Mean Max_Rad_Mean:", mean_max_rad))
print(paste("Mean Min_Rad_Mean:", mean_min_rad))

max_wind_vel <- max(FL1$w.vl, na.rm = TRUE)
min_wind_vel <- min(FL1$w.vl, na.rm = TRUE)

# Print the results
print(paste("Maximum Wind Velocity (w.vl):", max_wind_vel))
print(paste("Minimum Wind Velocity (w.vl):", min_wind_vel))

FL1
# Convert the 'mo' column to a factor for grouping by month
FL1$mo <- factor(FL1$mo)

# Create a function to calculate the maximum and minimum values by month
calc_monthly_max_min <- function(x) {
  max_val <- max(x, na.rm = TRUE)
  min_val <- min(x, na.rm = TRUE)
  return(c(Max_Month_Mean = max_val, Min_Month_Mean = min_val))
}

# Calculate maximum and minimum values by month for Temperature, Solar Radiation, and Wind Speed
result <- aggregate(. ~ mo, data = FL1, FUN = calc_monthly_max_min)

# Print the result
print(result)

# Calculate the maximum and minimum averages of the 'rad' column
max_avg_rad <- max(aggregate(rad ~ mo, data = FL1, FUN = mean)$rad)
min_avg_rad <- min(aggregate(rad ~ mo, data = FL1, FUN = mean)$rad)

# Print the results
cat("Maximum Average Radiation (W/m**2):", max_avg_rad, "\n")
cat("Minimum Average Radiation (W/m**2):", min_avg_rad, "\n")

***MD
MD1=read.csv("MD.csv")
# Filter out the rows with valid numeric values for the "rad" column
MD1 <- MD1[complete.cases(MD1$rad), ]

# Calculate maximum, minimum, and average for the "rad" column
max_rad <- max(MD1$rad)
min_rad <- min(MD1$rad)
avg_rad <- mean(MD1$rad)

# Print the results
cat("Maximum Radiation (W/m**2):", max_rad, "\n")
cat("Minimum Radiation (W/m**2):", min_rad, "\n")
cat("Average Radiation (W/m**2):", avg_rad, "\n")

# Filter out the rows with valid numeric values for the "w.vl" column
MD1 <- MD1[complete.cases(MD1$w.vl), ]

# Calculate maximum, minimum, and average for the "w.vl" column
max_wvl <- max(MD1$w.vl)
min_wvl <- min(MD1$w.vl)
avg_wvl <- mean(MD1$w.vl)

# Print the results
cat("Maximum Wind Speed (m/s):", max_wvl, "\n")
cat("Minimum Wind Speed (m/s):", min_wvl, "\n")
cat("Average Wind Speed (m/s):", avg_wvl, "\n")







