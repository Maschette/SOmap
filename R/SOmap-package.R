#' @keywords internal
"_PACKAGE"


#' Sea ice
#'
#' Example sea ice concentration data from the Southern Ocean (2018-10-15).
#' (See "data-raw/ice.R").
#' @docType data
#' @name ice
#' @examples
#' xy <- coordinates(spTransform(as(SOmap_data$fronts_orsi, "SpatialPoints"), "+init=epsg:4326"))
#' ## just because you can doesn't mean you should ...
#' SOauto_map(xy[,1], xy[,2], bathy = ice, input_points = FALSE, levels = c(15, 30, 60, 90))
NULL
