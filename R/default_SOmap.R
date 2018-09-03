#' Default southern ocean map
#'
#' Provide minimal input information to get a default map. The simplest case is
#' to run the function without any inputs at all and it will provide a random default.
#'
#' To input your data, use input locations as `xs` (longitude) and `ys` (latitude) values, there must be at least
#' two locations.
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
#' @importFrom sf st_graticule st_as_sf
#' @importFrom raster aggregate crop extent projectExtent projectRaster
#' @importFrom rnaturalearth ne_coastline
#' @importFrom rgdal project
#' @examples
#' default_somap(c(0, 50), c(-70, -50))
#' default_somap(runif(10, 130, 200), runif(10, -80, -10))
#' default_somap(runif(10, 130, 200), runif(10, -85, -60))
#' ## save the result to explore later!
#' protomap <- default_somap(runif(10, 60, 160), runif(10, -73, -50), coast = rnaturalearth::ne_coastline())
default_somap <- function(xs, ys, centre_lon = NULL, centre_lat = NULL, family = "stere",
                          dimXY = c(300, 300),
                          bathy = TRUE, coast = TRUE, input_points = TRUE, input_lines = TRUE,
                          graticule = TRUE, buffer=0.05,
                          contours=TRUE, lvs=c(-500, -1000, -2000), addcont=TRUE) {
  if (missing(xs) && missing(ys)) {
    xlim <- sort(runif(2, -180, 180))
    ylim <- sort(runif(2, -89, -20))

    if (diff(xlim) > 160) xlim[1] <- xlim[2] - 160
    xs <- runif(30, xlim[1], xlim[2])
    ys <- runif(30, ylim[1], ylim[2])
  }
  xlim <- range(xs) + c(-buffer, buffer)
  ylim <- range(ys) + c(-buffer, buffer)
    if (ylim[1] < -90) {ylim[1] <- -90}
    if (ylim[2] > 0) {ylim[2] <- 0}

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
  ## do we need to expand xlim/ylim from this target?
  ## obtain vertical xlim and horizontal ylim from

  dim(target) <- dimXY
  bathymetry <- coastline <- NULL
  if (isTRUE(bathy)) {            ## insert your local bathy-getter here
    ##if (!exists("topo")) topo <- raster::aggregate(raadtools::readtopo("etopo2", xylim = extent(-180, 180, -90, 0)), fact = 10)
    bathymetry <- raster::projectRaster(Bathy, target)
  } else {
    if (inherits(bathy, "BasicRaster")) {
      bathymetry <- raster::projectRaster(bathy[[1]], target, method = "ngb")
      bathy <- TRUE
    }

  }

  if (isTRUE(coast)) {
    ## insert your local coastline getter here
   # data("wrld_simpl", package = "maptools")
   #coastline <- as(wrld_simpl, "SpatialLinesDataFrame")

    #coastline <- raster::crop(as(sp::spTransform(land1, prj), "SpatialLinesDataFrame") , extent(target))
    coastline <- sp::spTransform(land1, prj)
  } else {
    if (inherits(coast, "Spatial")) {
      coastline <- sp::spTransform(coast, prj)
      coast <- TRUE
    }

  }
  ramp2<-grDevices::colorRampPalette(c("#54A3D1","#60B3EB","#78C8F0","#98D1F5","#B5DCFF","#BDE1F0","#CDEBFA","#D6EFFF","#EBFAFF","grey92","grey94","grey96", "white"))
  bluepal<-ramp2(45)
  #bk<-c(-10353,-8000,-5000,-4000,-3000,-2000,-1500,-1000,-500,-1,0,1500, 5850)
  #breaks=bk,

#  plot(c(xmin(target), xmax(target)), c(ymin(target), ymax(target)), type = "n", asp = 1, axes = FALSE, xlab = "", ylab = "")
  pp <- aspectplot.default(c(xmin(bathymetry), xmax(bathymetry)), c(ymin(bathymetry), ymax(bathymetry)), asp = 1)
  if (bathy) image(bathymetry, add = FALSE, col = bluepal, axes = FALSE)#grey(seq(0, 1, length = 40)))
  box()

  if (contours) contour(bathymetry, nlevels=1, levels=c(lvs), col="black", add= addcont)
  #op <- par(xpd = FALSE,xaxs="i",yaxs="i")
  if (coast) plot(coastline, add = TRUE)
  #par(op)
  if (input_points || input_lines) xy <- rgdal::project(cbind(xs, ys), prj)
  if (input_points) points(xy)
  if (input_lines) lines(xy)

  if (graticule) {
    #print(target)
    p <- sf::st_as_sf(spex::spex(crs = projection(target)))

    grat <- sf::st_graticule(p)
    plot_graticule(grat)
    #rgdal::llgridlines(p, col = "grey")
  }
  par(pp)
  invisible(list(bathy = bathymetry, coastline = coastline, target = target))
}

## from ?sf::st_graticule
plot_graticule <- function(g) {
  plot(g[1], add = TRUE, col = 'grey')
 # points(g$x_start, g$y_start, col = 'red')
  #points(g$x_end, g$y_end, col = 'blue')

  invisible(lapply(seq_len(nrow(g)), function(i) {
    if (g$type[i] == "N" && g$x_start[i] - min(g$x_start) < 1000)
      text(g[i,"x_start"], g[i,"y_start"], labels = parse(text = g[i,"degree_label"]),
           srt = g$angle_start[i], pos = 2, cex = .7)
    if (g$type[i] == "E" && g$y_start[i] - min(g$y_start) < 1000)
      text(g[i,"x_start"], g[i,"y_start"], labels = parse(text = g[i,"degree_label"]),
           srt = g$angle_start[i] - 90, pos = 1, cex = .7)
    if (g$type[i] == "N" && g$x_end[i] - max(g$x_end) > -1000)
      text(g[i,"x_end"], g[i,"y_end"], labels = parse(text = g[i,"degree_label"]),
           srt = g$angle_end[i], pos = 4, cex = .7)
    if (g$type[i] == "E" && g$y_end[i] - max(g$y_end) > -1000)
      text(g[i,"x_end"], g[i,"y_end"], labels = parse(text = g[i,"degree_label"]),
           srt = g$angle_end[i] - 90, pos = 3, cex = .7)
  }))
  invisible(NULL)
}


aspectplot.default <- function(xlim,ylim,asp, ...) {
  plot.new()
  #plot.window(xlim=xlim,ylim=ylim,xaxs="i",yaxs="i")
  asp <- 1
  r <- abs(diff(ylim)/diff(xlim))
  print(r)

  if(r > 1) {
    recip <- 1/(2 * r)
    figure <- c(0.5 - recip, 0.5 + recip,
                0, 1)
  } else {
    recip <- r/2
    figure <- c(0, 1,
                0.5 - recip, 0.5 + recip)
  }
  print(cbind(xlim, ylim, asp))
  print(figure)
  p <- par(fig = figure, new = TRUE)

  plot.window(xlim=xlim,ylim=ylim,xaxs="i",yaxs="i", asp = asp)
  return(p)
}


