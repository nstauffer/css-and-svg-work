raw_svg <- readLines(con = "las_cruces_white_on_black_width48.9_height10_test.svg")

polyline_indices <- grep(x = raw_svg,
                         pattern = "^<polyline")

polygon_indices <- grep(x = raw_svg,
                        pattern = "^<polygon")

poly_indices <- grep(x = raw_svg,
                     pattern = "^<poly")

poly_strings <- lapply(X = 1:length(poly_indices),
                       indices = poly_indices,
                       raw_svg = raw_svg,
                       FUN = function(X, indices, raw_svg) {
                         poly_string <- raw_svg[indices[X]]
                         
                         poly_type <- stringr::str_extract(poly_string,
                                                           pattern = "(?<=^<)poly.*(?= points)")
                         
                         coords_raw <- stringr::str_extract(string = polyline_string,
                                                            pattern = "(?<=points=').*(?= ')")
                         
                         coords_vector <- stringr::str_split(string = coords_raw,
                                                             pattern = " ",
                                                             simplify = TRUE)
                         
                         x_coords <- sapply(X = coords_vector,
                                            FUN = function(X) {
                                              stringr::str_extract(string = X,
                                                                   pattern = ".*(?=,)")
                                            })
                         y_coords <- sapply(X = coords_vector,
                                            FUN = function(X) {
                                              stringr::str_extract(string = X,
                                                                   pattern = "(?<=,).*")
                                            })
                         
                         coords_vector <- gsub(x = coords_vector,
                                               pattern = ",",
                                               replacement = " ")
                         
                         path_string <- paste0(paste0(c("M",
                                                        rep("L",
                                                            times = length(coords_vector) - 1)),
                                                      coords_vector),
                                               collapse = " ")
                         
                         flat_coord_string <- paste0(paste0(x_coords,
                                                            min(y_coords),
                                                            sep = ","),
                                                     collapse = " ")
                         
                         poly <- paste0("<", poly_type, " class='", poly_type, "-", X, "' points='", coords_raw, "'/>")
                         
                         # For extruding animations
                         if (poly_type == "polygon") {
                           styling <- paste0(".polygon-", X, " {stroke:none;stroke-width:0.1%;fill:#1A1A1A;animation-name:polygon-anim-", X, ";animation-duration:7s;animation-iteration-count:infinite;animation-direction:alternate;animation-timing-function:ease-in-ease-out;}")
                           keyframes <- paste0("@keyframes polygon-anim-", X, "{from{points:'",
                                               coords_raw,
                                               "';}to{points:'",
                                               flat_coord_string,
                                               "';}}")
                         } else {
                           styling <- paste0(".polyline-", X, " {stroke:white;stroke-width:0.1%px;fill:none;animation-name:polyline-anim-", X, ";animation-duration:7s;animation-iteration-count:infinite;animation-direction:alternate;animation-timing-function:ease-in-ease-out;}")
                           keyframes <- paste0("@keyframes polyline-anim-", X, "{from{points:'",
                                               coords_raw,
                                               "';}to{points:'",
                                               flat_coord_string,
                                               "';}}")
                         }
                         
                         # For stroking animations
                         # styling <- paste0(".ridgeline-", X, " {stroke:white;stroke-width:1%;stroke-dasharray:100% 100%;fill:none;animation-name:ridgeline-anim-", X, ";animation-duration:7s;animation-iteration-count:infinite;animation-timing-function:linear;animation-direction:alternate;}")
                         # keyframes <- paste0("@keyframes ridgeline-anim-", X, "{from{stroke-dashoffset:1500px;}to{stroke-dashoffset:0px;}}")
                         
                         list(poly = poly,
                              styling = styling,
                              keyframes = keyframes,
                              original_path_string = path_string,
                              ridgeline_id = paste0(poly_type, "-", X))
                       })

# These are for trying to extrude the paths
polys_string <- paste0(sapply(X = poly_strings,
                              FUN = function(X) {
                                X$poly
                              }),
                       collapse = "")
style_string <- paste0("<style>",
                       paste0(sapply(X = poly_strings,
                                     FUN = function(X) {
                                       X$styling
                                     }),collapse = ""),
                       paste0(sapply(X = poly_strings,
                                     FUN = function(X) {
                                       X$keyframes
                                     }),collapse = ""),
                       "</style>")

# These are for tracing the paths
# paths_string <- paste0(sapply(X = line_strings,
#                               FUN = function(X) {
#                                 paste0("<path class = '", X$ridgeline_id, "' d = '", X$original_path_string, "'/>")
#                               }),
#                        collapse = "")
# style_string <- paste0("<style>",
#                        paste0(sapply(X = line_strings,
#                                      FUN = function(X) {
#                                        X$styling
#                                      }),collapse = ""),
#                        paste0(sapply(X = line_strings,
#                                      FUN = function(X) {
#                                        X$keyframes
#                                      }),collapse = ""),
#                        "</style>")

header <- "<svg xmlns='http://www.w3.org/2000/svg' xmlns:xlink='http://www.w3.org/1999/xlink' class='svglite' width='3520.80pt' height='720.00pt' viewBox='0 0 3520.80 720.00'>
<rect width='100%' height='100%' style='stroke: none; fill: #1A1A1A;'/>"

footer <- "</svg>"

writeLines(text = c(header, polys_string, style_string, footer),
           con = "extrusion_test.svg")
paste0(header, shapes_string, style_string, footer)
