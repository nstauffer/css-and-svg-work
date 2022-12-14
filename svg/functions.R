regular_polygon <- function(vertex_count,
                            radius = NULL,
                            side_length = NULL,
                            center_x = 0,
                            center_y = 0,
                            output_type = "path") {
  if (!is.numeric(vertex_count)) {
    stop("vertex_count must be numeric.")
  }
  if (!is.null(side_length) & !is.numeric(side_length)) {
    stop("vertex_count must be numeric or NULL.")
  }
  if (!is.null(radius) & !is.numeric(radius)) {
    stop("radius must be numeric or NULL.")
  }
  if (!(output_type %in% c("coords", "path"))) {
    stop("output_type must be either 'coords' or 'path'.")
  }
  
  if (!is.null(side_length)) {
    if (!is.null(radius)) {
      warning("Both side_length and radius are defined. Using side_length.") 
    }
    radius <- (side_length / 2) / sin(radian_increment / 2)
  } else {
    if (is.null(radius)) {
      stop("Please define either side_length or radius.")
    }
  }
  
  radian_increment <- 2 / vertex_count
  
  # First we generate coordinates on a unit circle
  x_coords <- sapply(X = (0:(vertex_count - 1) * radian_increment),
                     FUN = function(X) {
                       cos(X * pi)
                     })
  
  y_coords <- sapply(X = (0:(vertex_count - 1) * radian_increment),
                     FUN = function(X) {
                       sin(X * pi)
                     })
  
  # Then we adjust the coordinates for the requested center and radius
  coords <- data.frame(x = x_coords * radius + center_x,
                       y = y_coords * radius + center_y)
  
  if (output_type == "coords") {
    coords
  } else {
    # This builds the path for a closed polygon in CSS using the coordinates
    polygon_steps <- sapply(X = 1:vertex_count,
                            coords = coords,
                            FUN = function(X, coords) {
                              if (X == 1) {
                                paste0("M",
                                       round(coords$x,
                                             digits = 2),
                                       " ",
                                       round(coords$y,
                                             digits = 2))
                              } else {
                                paste0("L",
                                       round(coords$x,
                                             digits = 2),
                                       " ",
                                       round(coords$y,
                                             digits = 2))
                              }
                            })
    
    path_d <- paste0(paste0(polygon_steps,
                            collapse = " "),
                     "Z")
    
    path_d
  }
}