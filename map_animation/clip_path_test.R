raw_svg <- readLines(con = "las_cruces_white_on_black_width48.9_height10_test.svg")

# Trying to save processing power by just using polylines and giving them a fill
# color instead of including the polygons
poly_indices <- grep(x = raw_svg,
                     pattern = "^<polyline")

poly_indices_to_keep <- (1:ceiling(length(poly_indices) / 2) * 2)
poly_indices_to_keep <- poly_indices_to_keep[poly_indices_to_keep <= length(poly_indices)]

poly_strings_list <- lapply(X = 1:length(poly_indices_to_keep),
                            indices = poly_indices[poly_indices_to_keep],
                            raw_svg = raw_svg,
                            FUN = function(X, indices, raw_svg) {
                              delays <- 6:2.5 / 10
                              
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
                              y_coords <- sapply(X = coords_vector,
                                                 FUN = function(X) {
                                                   stringr::str_extract(string = X,
                                                                        pattern = "(?<=,).*")
                                                 })
                              
                              # This reduces the number of vertices to prevent slowdown
                              vertex_indices_to_keep <- (1:ceiling(length(x_coords) / 3) * 3)
                              vertex_indices_to_keep <- vertex_indices_to_keep[vertex_indices_to_keep <= length(x_coords)]
                              
                              x_coords_reduced <- x_coords[vertex_indices_to_keep]
                              y_coords_reduced <- y_coords[vertex_indices_to_keep]
                              
                              coords_string_reduced <- paste0(paste(x_coords_reduced,
                                                                    y_coords_reduced,
                                                                    sep = ","),
                                                              collapse = " ")
                              
                              poly <- paste0("<", poly_type, " class='", poly_type, "-", X, "' points='", coords_string_reduced, "'/>")
                              
                              delay <- sample(x = delays, size = 1)
                              
                              if (poly_type == "polygon") {
                                styling <- paste0(".polygon-", X, " {vector-effect:non-scaling-stroke;stroke:none;stroke-width:0.1%;fill:#1A1A1A;animation-delay:", delay,"s;animation-name:slide;animation-duration:12s;animation-iteration-count:infinite;animation-direction:normal;animation-timing-function:linear;}")
                                keyframes <- paste0("@keyframes polygon-anim-", X, "{from{transform:scaleY(0);to{scaleY(1)}}")
                              } else {
                                styling <- paste0(".polyline-", X, " {vector-effect:non-scaling-stroke;stroke:white;stroke-width:0.1%;fill:#1A1A1A;animation-delay:", delay,"s;animation-name:slide;animation-duration:12s;animation-iteration-count:infinite;animation-direction:normal;animation-timing-function:linear;}")
                                keyframes <- paste0("@keyframes polyline-anim-", X, "{from{transform:scaleY(0);to{scaleY(1)}}")
                              }
                              
                              list(poly = poly,
                                   styling = styling,
                                   keyframes = keyframes,
                                   ridgeline_id = paste0(poly_type, "-", X))
                            })

poly_strings_vector <- sapply(X = poly_strings_list[3:length(poly_strings_list)],
                              FUN = function(X) {
                                X$poly
                              })

style_strings_vector <- c("<style>",
                          sapply(X = poly_strings_list,
                                 FUN = function(X) {
                                   X$styling
                                 }),
                          "@keyframes slide{0%{clip-path:polygon(0% 0%, 0% 0%, 0% 100%, 0% 100%)}50%{clip-path:polygon(0% 0%, 100% 0%, 100% 100%, 0% 100%)}100%{clip-path:polygon(100% 0%, 100% 0%, 100% 100%, 100% 100%)}}",
                          "</style>")

header <- "<svg xmlns='http://www.w3.org/2000/svg' xmlns:xlink='http://www.w3.org/1999/xlink' class='svglite' width='3520.80pt' height='720.00pt' viewBox='0 0 3520.80 720.00'>
<rect width='100%' height='100%' style='stroke: none; fill: #1A1A1A;'/>"

footer <- "</svg>"

writeLines(text = c(header, poly_strings_vector, style_strings_vector, footer),
           con = "clip_test.svg")
