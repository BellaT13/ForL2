# Load necessary packages
library(terra)
library(tmap)
library(sf)

# 1. Read and Explore the DEM
# Set your working directory to the location of your files
# Replace with the actual path to your data


# Read the DEM file
dem <- rast("unit2.img")

# Inspect the DEM
dem  # Print DEM properties
plot(dem) #visualize

# 2. Derive Slope and Aspect
# Calculate slope and aspect
slope <- terrain(dem, "slope")
aspect <- terrain(dem, "aspect")

# Visualize slope and aspect
par(mfrow=c(1,2))  #plot side by side
plot(slope, main="Slope")
plot(aspect, main="Aspect")
par(mfrow=c(1,1)) #reset

# 3. Read and Prepare Forest Inventory Data
# Read the summary table from Week 9
inventory_data <- read.csv("sum_u2.csv")

# Read the shapefile of sample plot locations
plot_locations <- st_read("HEE_Overstory_Survey_Points_2017.shp") 


#check projections
st_crs(plot_locations)
crs(dem)

#if they are different, reproject
plot_locations_trans <- st_transform(plot_locations, crs(dem))

# 4. Extract DEM Values at Plot Locations
# Extract DEM values at the plot locations.  Use the transformed plot locations.
dem_values <- extract(dem, plot_locations_trans)

# Combine the extracted DEM values with the inventory data
inventory_data$DEM_value <- dem_values[,1]  # Assuming the first column of extracted values is what you need.  Adjust if necessary.

# 5. Analyze Forest Attributes Across Topography
# Example: Analyze basal area across elevation (DEM value)
# You can perform similar analyses for other attributes (biomass, etc.) and with slope/aspect

# Basic scatterplot of basal area vs. elevation
plot(inventory_data$DEM_value, inventory_data$Basal_Area_m2_ha, 
     xlab = "Elevation (DEM Value)", ylab = "Basal Area (m2/ha)",
     main = "Basal Area vs. Elevation")

# Linear regression to quantify the relationship
model <- lm(Basal_Area_m2_ha ~ DEM_value, data = inventory_data)
abline(model, col = "red")  # Add the regression line to the plot
summary(model) #show model

# You can further analyze how other forest attributes (e.g., Biomass_kg_ha) vary with elevation, slope, or aspect.
# Example: Boxplots to visualize basal area distribution across aspect categories (if you categorize aspect)
# aspect_categories <- cut(aspect_values, breaks = c(0, 90, 180, 270, 360), 
#                         labels = c("North", "East", "South", "West"))
# boxplot(inventory_data$Basal_Area_m2_ha ~ aspect_categories,
#         xlab = "Aspect", ylab = "Basal Area (m2/ha)",
#         main = "Basal Area Distribution Across Aspect")