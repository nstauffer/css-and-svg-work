raw_svg <- readLines(con = "steamboat_white_on_black_lines250_width16_height20.svg")

# Trying to save processing power by just using polylines and giving them a fill
# color instead of including the polygons
poly_indices <- grep(x = raw_svg,
                     pattern = "^<poly")

# poly_indices_to_keep <- (1:ceiling(length(poly_indices) / 2) * 2)
# poly_indices_to_keep <- poly_indices_to_keep[poly_indices_to_keep <= length(poly_indices)]

# poly_indices_to_keep <- ceiling(length(poly_indices) / 2):length(poly_indices)

poly_strings_list <- lapply(X = 1:length(poly_indices),
                            indices = poly_indices,
                            raw_svg = raw_svg,
                            FUN = function(X, indices, raw_svg) {
                              # I'm hardcoding this for steamboat_white_on_black_lines250_width16_height20.svg
                              scaling_factor <- 500 / 1152
                              
                              delays <- 6:20 / 10
                              
                              poly_string <- raw_svg[indices[X]]
                              
                              poly_type <- stringr::str_extract(poly_string,
                                                                pattern = "(?<=^<)poly.*(?= points)")
                              
                              coords_raw <- stringr::str_extract(string = poly_string,
                                                                 pattern = "(?<=points=').*(?= ')")
                              
                              coords_vector <- stringr::str_split(string = coords_raw,
                                                                  pattern = " ",
                                                                  simplify = TRUE)
                              
                              x_coords <- sapply(X = coords_vector,
                                                 FUN = function(X) {
                                                   stringr::str_extract(string = X,
                                                                        pattern = ".*(?=,)")
                                                 })
                              x_coords_adjusted <- scaling_factor * as.numeric(x_coords)
                              y_coords <- sapply(X = coords_vector,
                                                 FUN = function(X) {
                                                   stringr::str_extract(string = X,
                                                                        pattern = "(?<=,).*")
                                                 })
                              y_coords_adjusted <- scaling_factor * as.numeric(y_coords)
                              
                              # This reduces the number of vertices to prevent slowdown
                              vertex_indices_to_keep <- (1:ceiling(length(x_coords) / 6) * 6)
                              vertex_indices_to_keep <- vertex_indices_to_keep[vertex_indices_to_keep <= length(x_coords)]
                              
                              x_coords_reduced <- x_coords_adjusted[vertex_indices_to_keep]
                              y_coords_reduced <- y_coords_adjusted[vertex_indices_to_keep]
                              
                              coords_string_reduced <- paste0(paste(x_coords_reduced,
                                                                    y_coords_reduced,
                                                                    sep = ","),
                                                              collapse = " ")
                              
                              poly <- paste0("<", poly_type, " class='", poly_type, "-", X, "' points='", coords_string_reduced, "'/>")
                              
                              # For extruding animations
                              if (poly_type == "polygon") {
                                styling <- paste0(".polygon-", X, " {vector-effect:non-scaling-stroke;stroke:none;stroke-width:0.1%;fill:#1A1A1A;}")
                                keyframes <- paste0("@keyframes polygon-anim-", X, "{from{transform:scaleY(0);to{scaleY(1)}}")
                              } else {
                                styling <- paste0(".polyline-", X, " {vector-effect:non-scaling-stroke;stroke:white;stroke-width:0.1%;stroke-dasharray:120% 120%;fill:none;animation-delay:", sample(x = delays, size = 1),"s;animation-name:trace;animation-duration:20s;animation-iteration-count:infinite;animation-timing-function:linear;animation-direction:normal;}")
                                keyframes <- paste0("@keyframes polyline-anim-", X, "{from{transform:scaleY(0);to{scaleY(1)}}")
                              }
                              
                              list(poly = poly,
                                   styling = styling,
                                   keyframes = keyframes,
                                   ridgeline_id = paste0(poly_type, "-", X))
                            })

poly_strings_vector <- sapply(X = poly_strings_list,
                              FUN = function(X) {
                                X$poly
                              })

style_strings_vector <- c("<style>",
                          sapply(X = poly_strings_list,
                                 FUN = function(X) {
                                   X$styling
                                 }),
                          "@keyframes trace{from{stroke-dashoffset:0%;}to{stroke-dashoffset:-240%;}}",
                          "</style>")

header <- "<svg xmlns='http://www.w3.org/2000/svg' xmlns:xlink='http://www.w3.org/1999/xlink' class='svglite' width='500pt' height='625pt' viewBox='0 0 500.00 625.00'>
<rect width='100%' height='100%' style='stroke: none; fill: #1A1A1A;'/>"

footer <- "</svg>"

writeLines(text = c(header, poly_strings_vector, style_strings_vector, footer),
           con = "valley_trace.svg")
