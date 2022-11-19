# I'm lazy
library(ggplot2)
library(viridis)

# Data source
dsn <- "C:/Users/Nelson/Documents/Projects/Other/topo_commissions/data/rasters/"

# The raster
elev_raster <- raster::raster(paste0(dsn, "/", "all_san_luis.tif"))

raster::plot(elev_raster)

# Get the extent
frame_extent <- raster::extent(elev_raster)

# Manually change the extent here to look better

# Wide
frame_extent@xmax <- -105.1
frame_extent@xmin <- -106.8
frame_extent@ymax <- 38.8
frame_extent@ymin <- 35.7

# Medium
frame_extent@xmax <- -105.4
frame_extent@xmin <- -106
frame_extent@ymax <- 37
frame_extent@ymin <- 35.7

# Tighter
# frame_extent@xmax <- -105.4
# frame_extent@xmin <- -106.3
# frame_extent@ymax <- 37.5
# frame_extent@ymin <- 34.5

# Aspect ratio
abs(frame_extent@xmax - frame_extent@xmin) / abs(frame_extent@ymax - frame_extent@ymin)


# Crop the raster
elev_raster_crop <- raster::crop(x = elev_raster,
                                 y = frame_extent)

# Check to see that looks right
raster::plot(elev_raster_crop)

# Reduce the resolution
elev_raster_lowres <- raster::aggregate(elev_raster_crop,
                                        fact = 12)

# Convert to a data frame for mapping use
elev_matrix <- raster::as.matrix(elev_raster_lowres)
elev_matrix[is.na(elev_matrix)] <- 0

# Normalize the values
elev_min <- min(elev_matrix)
elev_matrix <- elev_matrix - elev_min
elev_matrix[elev_matrix < 0] <- 0

# Generate the map(s) to save
maps <- list()
line_count <- 300
# This doesn't really do anything. It's vestigial but I haven't removed it yet
tilt_factor <- 75
scale_factor <- 0.6
size <- 0.5
min_height <- 0.0

maps[["white_on_black"]] <- ridgemap(elev_df = NULL,
                                     elev_matrix = elev_matrix,
                                     line_color = "white",
                                     background_color = "gray10",
                                     line_count = line_count,
                                     tilt_factor = tilt_factor,
                                     scale_factor = scale_factor,
                                     size = size,
                                     min_height = min_height)

maps[["white_on_black"]]


# SVG gives the smoothest outputs
filetypes <- c("svg")
width <- 24
height <- 16

for (filetype in filetypes) {
  for (map in names(maps)) {
    filename <- paste0("san_luis",
                       "_", map,
                       # "_lines", line_count,
                       # "_scale", scale_factor,
                       "_width", width,
                       "_height", height,
                       ".", filetype)
    ggsave(filename = filename,
           path = "C:/Users/Nelson/Documents/Projects/Other/topo_commissions/outputs/san_luis",
           plot = maps[[map]],
           device = filetype,
           width = width,
           height = height)
    # And this converts the SVG to a PNG because sometimes you need that too
    system(command = paste0("magick convert -density 300 C:\\Users\\Nelson\\Documents\\Projects\\Other\\topo_commissions\\outputs\\san_luis\\", filename,
                            " C:\\Users\\Nelson\\Documents\\Projects\\Other\\topo_commissions\\outputs\\san_luis\\", gsub(filename, pattern = "svg$", replacement = "png")))
    # And this converts the SVG to a PNG because sometimes you need that too
    system(command = paste0("magick convert -density 300 C:\\Users\\Nelson\\Documents\\Projects\\Other\\topo_commissions\\outputs\\san_luis\\", filename,
                            " C:\\Users\\Nelson\\Documents\\Projects\\Other\\topo_commissions\\outputs\\san_luis\\", gsub(filename, pattern = "svg$", replacement = "tif")))
  }
}









# TESTING BELOW

# Generate a bunch of test maps to try to decide on dimensions, scaling, and line density
line_counts <- 275 + (1:1 * 25)
scales <- 0.5 + (0:5 * 0.05)
x_dimensions <- c(16)
y_dimensions <- c(24)

variants_df <- expand.grid(line_counts, scales, x_dimensions, y_dimensions)
names(variants_df) <- c("line_count", "scale", "x", "y")
# Take out any where the image would be square
variants_df <- variants_df[variants_df[["x"]] != variants_df[["y"]], ]

test_list <- lapply(X = 1:nrow(variants_df),
                    variants_df = variants_df,
                    elev_matrix = elev_matrix,
                    FUN = function(X, variants_df, elev_matrix){
                      current_linecount <- variants_df[X, "line_count"]
                      current_scale <- variants_df[X, "scale"]
                      map <- ridgemap(elev_df = NULL,
                                      elev_matrix = elev_matrix,
                                      line_color = "white",
                                      background_color = "gray10",
                                      line_count = current_linecount,
                                      tilt_factor = 75,
                                      scale_factor = current_scale,
                                      size = 0.5,
                                      min_height = 0)
                      return(map)
                    })

for (variant in 1:nrow(variants_df)) {
  ggsave(filename = paste0("san_luis_santa_fe_medium",
                           "_lines", variants_df[variant, "line_count"],
                           "_scale", variants_df[variant, "scale"],
                           "_width", variants_df[variant, "x"],
                           "_height", variants_df[variant, "y"], ".png"),
         path = "C:/Users/Nelson/Documents/Projects/Other/topo_commissions/outputs/san_luis/testing",
         plot = test_list[[variant]],
         device = "png",
         width = variants_df[variant, "x"],
         height = variants_df[variant, "y"])
}
