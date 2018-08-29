#' Default southern ocean map
#'
#' Provide minimal input information to get a default map. The simplest case is
#' to input locations as `xs` and `ys` values, there must be at least two longitude
#' and latitude values.
#'
#' Try families such as `laea`, `ortho`, `gnomonic` if feeling adventurous.
#' @param xs,ys longitude and latitude values
#' @param centre_lon,centre_lat  optional map centre longitude and latitude
#' @param family optional projection family (default is `stere`ographic)
#' @param dimXY dimensions of background bathmetry (if used) default is 300x300
#' @param bathy,coast optional input bathymetry or coastline data, defaults to `TRUE` set to `FALSE` to ignore or input your own
#' @param input_points,input_lines flag to plot input data as points and / or lines
#' @param graticule flag to add a basic graticule
#'
#' @return the derived target extent in the map projection used, bathymetry, and coastline data
#' @export
#'
#' @examples
#' default_somap(c(0, 50), c(-70, -50))
#' default_somap(runif(10, 130, 200), runif(10, -80, -10))
#' default_somap(runif(10, 130, 200), runif(10, -85, -60))
#' ## save the result to explore later!
#' protomap <- default_somap(runif(10, 60, 160), runif(10, -73, -50), coast = rnaturalearth::ne_coastline())
#' \dontrun{
#' library(lazyraster)
#' ibcso <- as_raster(lazyraster(raadtools::topofile("ibcso")), dim = c(2000, 2000))
#' projection(ibcso) <- projection(raadtools::readtopo("ibcso"))
#' default_somap(runif(10, 30, 160), runif(10, -75, -40), bathy = ibcso)
default_somap <- function(xs, ys, centre_lon = NULL, centre_lat = NULL, family = "stere",
                          dimXY = c(300, 300),
                          bathy = TRUE, coast = TRUE, input_points = TRUE, input_lines = TRUE,
                          graticule = TRUE) {
  xlim <- range(xs)
  ylim <- range(ys)
  if (is.null(centre_lon)) {
    centre_lon <- mean(xlim)
  }
  if (is.null(centre_lat)) {
    centre_lat <- mean(ylim)
  }

  template <- "+proj=%s +lon_0=%f +lat_0=%f +datum=WGS84"
  if (family == "stere") {
    ## won't generalize to northern hemisphere
    template <- "+proj=%s +lon_0=%f +lat_0=%f +lat_ts=-71 +datum=WGS84"
  }
  prj <- sprintf(template, family, centre_lon, centre_lat)

  target <- raster::projectExtent(raster::raster(extent(xlim, ylim), crs = "+init=epsg:4326"),
                                  prj)
  dim(target) <- dimXY
  bathymetry <- coastline <- NULL
  if (isTRUE(bathy)) {            ## insert your local bathy-getter here
    ##topo <- raadtools::readtopo("etopo2")
    bathymetry <- projectRaster(topo, target)
  } else {
    if (inherits(bathy, "BasicRaster")) {
      bathymetry <- projectRaster(bathy[[1]], target, method = "ngb")
      bathy <- TRUE
    }

  }

  if (isTRUE(coast)) {
    ## insert your local coastline getter here
    data("wrld_simpl", package = "maptools")
    coastline <- as(wrld_simpl, "SpatialLinesDataFrame")

    coastline <- raster::crop(sp::spTransform(coastline, prj), extent(target))
  } else {
    if (inherits(coast, "Spatial")) {
      coastline <- sp::spTransform(coast, prj)
      coast <- TRUE
    }

  }

  plot(c(xmin(target), xmax(target)), c(ymin(target), ymax(target)), type = "n", asp = 1, axes = FALSE, xlab = "", ylab = "")
  if (bathy) plot(bathymetry, add = TRUE, col = grey(seq(0, 1, length = 40)))
  if (coast) plot(coastline, add = TRUE)
  if (input_points || input_lines) xy <- rgdal::project(cbind(xs, ys), prj)
  if (input_points) points(xy)
  if (input_lines) lines(xy)

  if (graticule) {
    p <- spex::spex(target)
    rgdal::llgridlines(p, col = "grey")
  }
  invisible(list(bathy = bathymetry, coastline = coastline, target = target))
}







