source("functions.R")

# Coords for a triangle
regular_polygon(vertex_count = 3,
                radius = 45,
                side_length = NULL,
                center_x = 100,
                center_y = 100,
                output_type = "coords")

# Path for a heptagon
regular_polygon(vertex_count = 7,
                radius = 155,
                side_length = NULL,
                center_x = 500,
                center_y = 500,
                output_type = "path")

# 14 centroids for a ring of rings
centroids <- regular_polygon(vertex_count = 14,
                             radius = 200,
                             side_length = NULL,
                             center_x = 500,
                             center_y = 500,
                             output_type = "coords")

sapply(X = 1:nrow(centroids),
       coords = centroids,
       FUN = function(X, coords) {
         paste0("<circle ",
                "class = 'ring-of-rings' ",
                "cx = '", coords[X, "x"], "' ",
                "cy = '", coords[X, "y"], "' ",
                "r = '", 2, "%' ",
                "stroke-width = '", 0.25, "%' ",
                "stroke = '", "#d4af37", "' ",
                "fill = 'none'",
                "/>")
       })


# This bit makes a ton of radiant paths
inner_coords <- regular_polygon(vertex_count = 50,
                                radius = 200,
                                side_length = NULL,
                                center_x = 500,
                                center_y = 500,
                                output_type = "coords")
outer_coords <- regular_polygon(vertex_count = 50,
                                radius = 350,
                                side_length = NULL,
                                center_x = 500,
                                center_y = 500,
                                output_type = "coords")

sapply(X = 1:nrow(inner_coords),
       inner_coords = inner_coords,
       outer_coords = outer_coords,
       FUN = function(X, inner_coords, outer_coords) {
         paste0("<path ",
                "class = 'radiance' ",
                "stroke = '#d4af37' ",
                "stroke-width = '5' ",
                "d = '",
                paste0("M",
                       inner_coords[X, "x"],
                       " ",
                       inner_coords[X, "y"],
                       " ",
                       "L",
                       outer_coords[X, "x"],
                       " ",
                       outer_coords[X, "y"]),
                "'/>")
       })
