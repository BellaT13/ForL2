# Load necessary libraries
library(terra)
library(tmap)
library(sf)


filepath <- "/Users/bella/Documents/AGR_333/ForL2/files/"  # Update this path VERY CAREFULLY

# Set the working directory to the location of your data files
setwd(filepath) # Add this line

# Read the DEM file
dem <- rast(paste0(filepath, "unit2.img"))

# Extract slope and aspect
slope <- terrain(dem, v = "slope", unit = "degrees", neighbors = 8)
aspect <- terrain(dem, v = "aspect", unit = "degrees")

# Visualize slope and aspect
ttm()
tm_shape(slope, alpha = 0.5) +
  tm_raster(style = "cont", alpha = 0.6, title = "Slope (deg)")
tm_shape(aspect) +
  tm_raster(style = "cont")


# Reclassify aspect
asp_class <- matrix(c(
  0, 45, 1,
  45, 135, 2,
  135, 225, 3,
  225, 315, 4,
  315, 360, 1
), ncol = 3, byrow = TRUE)
asp <- classify(aspect, asp_class)

# Visualize reclassified aspect
ttm()
tm_shape(asp) +
  tm_raster(style = "cat", palette = c("white", "blue", "green", "yellow", "red"),
            labels = c(NA, "North", "East", "South", "West"), alpha = 0.2)


# Read summary table and shapefile
sum_u2 <- read.csv("sum_u2.csv") # Changed to relative path, relies on setwd()
library(sf)
svy_pts <- st_read(paste0(filepath, "HEE_Overstory_Survey_Points_2017 - Copy.shp"))
svy_pts <- st_transform(svy_pts, 32616)
survey_pts <- subset(svy_pts, Unit == '2')

# Merge summary table with plot locations
sum_u2 <- merge(sum_u2, survey_pts, all.x = TRUE)

# Convert to sf format
sum_u2 <- st_as_sf(sum_u2, coords = c("X", "Y"), crs = 32616)

# Create circular plots
sf_plot <- st_buffer(sum_u2, dist = 17.83)

# Unify coordinate systems
asp_crs <- crs(asp, proj = TRUE)
sf_plot_crs <- st_transform(sf_plot, crs = asp_crs)

# Visualize dominant species by aspect
ttm()
tm_shape(asp, alpha = 0.5) +
  tm_raster(style = "cat", palette = c("white", "blue", "green", "yellow", "red"),
            showNA = FALSE, alpha = 0.2, labels = c(NA, "North", "East", "South", "West")) +
  tm_shape(sf_plot) +
  tm_polygons('Common.name') +
  tm_layout(legend.outside = TRUE, legend.outside.size = 0.2) +
  tm_text("Plot", ymod = -0.9)


# Visualize dominant species by slope
ttm()
tm_shape(slope, alpha = 0.5) +
  tm_raster(style = "cont", alpha = 0.6, title = "Slope (deg)") +
  tm_shape(sf_plot) +
  tm_polygons('Common.name', title = "Dom_Species", alpha = 0.6) +
  tm_layout(title = "Dominant trees by slope",
            legend.outside = TRUE, legend.outside.size = 0.2) +
  tm_text("Plot", ymod = -0.9, size = 1.2)


# Visualize basal area (BA) distribution
ttm()
tm_shape(sf_plot) +
  tm_polygons('BA', title = "Basal Area (sq_ft/acre)", palette = "brewer.spectral") +
  tm_layout(title = "Basal Area Distribution",
            legend.outside = TRUE, legend.outside.size = 0.2) +
  tm_text("Plot", ymod = -1.5, size = 1.2) +
  tm_scale_bar()


# Visualize trees per acre (TPA) distribution
ttm()
tm_shape(sf_plot) +
  tm_polygons('TPA', title = "Trees Per Acre", palette = "brewer.spectral") +
  tm_layout(title = "TPA Distribution",
            legend.outside = TRUE, legend.outside.size = 0.2) +
  tm_text("Plot", ymod = -1.5, size = 1.2) +
  tm_scale_bar()


# Visualize biomass distribution
ttm()
tm_shape(sf_plot) +
  tm_polygons('bm_tonpa', title = "Biomass (tons/ac)", palette = "brewer.spectral") +
  tm_layout(title = "Biomass Distribution",
            legend.outside = TRUE, legend.outside.size = 0.2) +
  tm_text("Plot", ymod = -1.5, size = 1.2) +
  tm_scale_bar()

