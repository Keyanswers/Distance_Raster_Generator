# ================================
# PACKAGES
# ================================
library(terra)

# =======================================================
# 1. Load the river raster /  categorical o binary raster
# ======================================================
rivers <- rast("C:/Users/NorthRiversTest.tif")

# ================================
# 2. Prepare a categorical raster
# (1 = river, NA = non-river)
# ================================
rivers[rivers == 0] <- NA
rivers[!is.na(rivers)] <- 1

# ======================================
# 3. Reproject raster to a CRS in meters
# Example: EPSG:5070 = North America
# Albers Equal Area projection
# ======================================
rivers_m <- project(rivers,
                    "EPSG:5070",
                    method = "near")

# ================================
# 4. Calculate distance to rivers
# Distances are returned in meters
# ================================
river_distance <- distance(rivers_m)

# ================================
# 5. Visualize results
# ================================
plot(river_distance,
     main = "Distance to rivers (m)")

# Add river layer on top of distance raster
plot(rivers_m,
     add = TRUE,
     col = "blue",
     legend = FALSE)

# ================================
# 6. Save distance raster
# ================================
writeRaster(river_distance,
            "C:/Users/RiverDistance.tif",
            overwrite = TRUE)

# ================================
# Example coordinates
# Coordinates must match raster CRS
# ================================
xy <- cbind(c(1000000, 1005000),
            c(500000, 505000))

# ====================================
# Extract distance values from raster
# ====================================
extract(river_distance, xy)

# Example interpretations:
# RIVER LITTLE SUGAR = point located on river
# RIVER CALOOSAHATCHEE = 464.6923 km away

# ===========================================
# Convert coordinate table to sf object
# ===========================================
points <- st_as_sf(values,
                   coords = c("x", "y"),
                   crs = 5070)

# Reproject points to WGS84
points_wgs84 <- st_transform(points,
                             4326)

points_wgs84

# =================================
# Merge river shapefiles
# and standardize river name column
# =================================
Rivers <- bind_rows(
  st_transform(Shf1, crs(Riv)) %>%
    select(name = PNAME),
  
  st_transform(Shf2, crs(Riv)) %>%
    select(name = NOMBRES)
)

# ================================
# Extract raster distance values
# ================================
Dis <- terra::extract(Riv,
                      vect(points))

# Add distance values to point dataset
points$distance_m <- Dis[,2]

# ==================================
# Find nearest river for each point
# ==================================
nearest_idx <- st_nearest_feature(points,
                                  Rivers)

# Store nearest river name
points$nearest_river <- Rivers$name[nearest_idx]

# ================================
# Final result
# ================================
print(points)