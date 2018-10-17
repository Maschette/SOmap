#' Default southern ocean map
#'
#' Provide minimal input information to get a default map. The simplest case is
#' to run the function without any inputs at all and it will provide a random default.
#'
#' To input your data, use input locations as `xs` (longitude) and `ys` (latitude) values, there must be at least
#' two locations.
#'
#' Try families such as 'lcc', 'laea', 'gnom', 'merc', 'aea' if feeling adventurous.
#'
#' @param xs optional input data longitudes
#' @param ys optional input data latitudes
#' @param centre_lon optional centre longitude (of the map projection, also used to for plot range if `expand = TRUE`)
#' @param centre_lat as per `centre_lon`
#' @param family optional projection family (default is `stere`ographic)
#' @param expand re-compute range of plot to incorporate centre_lon and centre_lat with the data as a natural middle
#' @param dimXY dimensions of background bathmetry (if used) default is 300x300
#' @param bathy optional bathymetry data to use (or `FALSE` for no bathmetry image)
#' @param coast optional coastline data to use (or `FALSE` for no coastline)
#' @param input_points add points to plot (of xs, ys)
#' @param input_lines add lines to plot   (of xs, ys)
#' @param graticule flag to add a basic graticule
#' @param buffer fraction to expand plot range from that calculated (either from data, or from centre_lon/centre_lat _and_ data if `expand = TRUE`)
#' @param contours add contours
#' @param levels contour levels if `contours = TRUE`
#' @param trim_background crop the resulting bathymetry to its margin of valid values
#' @param mask logical, used to mask the raster to the graticule
#'
#' @return the derived target extent and the map projection used, bathymetry, and coastline data
#' @export
#' @importFrom sf st_graticule st_as_sf
#' @importFrom methods as
#' @importFrom raster aggregate contour crop extend extent ncell plot projectExtent projection projectRaster xmin xmax ymin ymax
#' @importFrom rgdal project
#' @importFrom sp plot
#' @importFrom stats na.omit runif
#' @importFrom graphics contour lines par plot plot.new plot.window points text
#' @examples
#' SOauto_map(c(0, 50), c(-70, -50))
#' SOauto_map(runif(10, 130, 200), runif(10, -80, -10))
#' SOauto_map(runif(10, 130, 200), runif(10, -85, -60))
#' ## save the result to explore later!
#' protomap <- SOauto_map(runif(10, 60, 160), runif(10, -73, -50))
#'
#' SOauto_map(runif(50, 40, 180), runif(50, -73, -10), family = "aea", centre_lat = -15,
#'               input_lines = FALSE)
SOauto_map <- function(xs, ys, centre_lon = NULL, centre_lat = NULL, family = "stere",
                          expand = TRUE,
                          dimXY = c(300, 300),
                          bathy = TRUE, coast = TRUE, input_points = TRUE, input_lines = TRUE,
                          graticule = TRUE, buffer=0.05,
                          contours=TRUE, levels=c(-500, -1000, -2000),
                          trim_background = TRUE,
                          mask = TRUE) {
  if (missing(xs) || missing(ys)) {
    xlim <- sort(runif(2, -359, 359))
    ylim <- sort(runif(2, -89, -20))

    xs <- runif(30, xlim[1], xlim[2])
    ys <- runif(30, ylim[1], ylim[2])
    xy <- cbind(xs, ys)
    xy <- xy[order(xy[, 1], xy[,2]), ]
    xs <- xy[,1]
    ys <- xy[,2]
  }
  xs <- na.omit(xs)
  ys <- na.omit(ys)
  stopifnot(length(xs) > 1)
  stopifnot(length(ys) > 1)

  xlim <- range(xs)
  ylim <- range(ys)
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
  if (family %in% c("aea", "lcc")) {
    template <- paste("+proj=%s +lon_0=%f +lat_0=%f +datum=WGS84", sprintf("+lat_0=%f +lat_1=%f", ylim[1], ylim[2]))
  }
  prj <- sprintf(template, family, centre_lon, centre_lat)


  target <- raster::projectExtent(raster::raster(extent(xlim, ylim), crs = "+init=epsg:4326"),
                                  prj)
  dim(target) <- dimXY
  ## extend projected bounds by the buffer
  xxlim <- c(xmin(target), xmax(target))
  xxlim <- xxlim + diff(range(xxlim)) * c(-buffer, buffer)
  yylim <- c(ymin(target), ymax(target))
  yylim <- yylim + diff(range(yylim)) * c(-buffer, buffer)
  target <- extend(target, extent(xxlim, yylim))
  if (expand) {
    centre_line <- rgdal::project(cbind(centre_lon, centre_lat), prj)
    ## we need the largest of the difference from centre to target boundary
    xhalf <- max(abs(centre_line[1] - c(xmin(target), xmax(target))))
    yhalf <- max(abs(centre_line[2] - c(ymin(target), ymax(target))))
    exp_xlim <- centre_line[1] + c(-xhalf, xhalf)
    exp_ylim <- centre_line[2] + c(-yhalf, yhalf)

    target <- extend(target, extent(exp_xlim[1], exp_xlim[2], exp_ylim[1], exp_ylim[2]))
  }
  dim(target) <- dimXY
  bathymetry <- coastline <- NULL
  if (isTRUE(bathy)) {            ## insert your local bathy-getter here
    ##if (!exists("topo")) topo <- raster::aggregate(raadtools::readtopo("etopo2", xylim = extent(-180, 180, -90, 0)), fact = 10)
    bathymetry <- raster::projectRaster(Bathy, target)
    if (trim_background) {
      bathymetry <- raster::trim(bathymetry)
      target <- crop(target, bathymetry)
    }
  } else {
    if (inherits(bathy, "BasicRaster")) {
      bathymetry <- raster::projectRaster(bathy[[1]], target, method = "ngb")
      bathy <- TRUE
      if (trim_background) {
        bathymetry <- raster::trim(bathymetry)
        target <- crop(target, bathymetry)
      }
    }

  }
  # par(pp)
  aspect <- if (raster::isLonLat(target)) 1/cos(mean(c(xmin(target), xmax(target))) * pi/180) else 1
  pp <- aspectplot.default(c(xmin(target), xmax(target)), c(ymin(target), ymax(target)), asp = aspect, mar = par("mar")/2.5)
  newextent <- raster::extent(par("usr"))

  if (isTRUE(coast)) {
    suppressWarnings({

      coastline <- as(sf::st_crop(sf::st_buffer(sf::st_transform(sf::st_as_sf(continent), prj), 0), xmin = xmin(target), xmax = xmax(target), ymin = ymin(target), ymax = ymax(target)), "Spatial")

    })
  } else {
    if (inherits(coast, "Spatial")) {
      coastline <- sp::spTransform(coast, prj)
      coast <- TRUE
    }

  }
  ramp2<-grDevices::colorRampPalette(c("#54A3D1","#60B3EB","#78C8F0","#98D1F5","#B5DCFF","#BDE1F0","#CDEBFA","#D6EFFF","#EBFAFF","grey92","grey94","grey96", "white"))
  bluepal<-ramp2(45)
  #bk<-c(-10353,-8000,-5000,-4000,-3000,-2000,-1500,-1000,-500,-1,0,1500, 5850)

  # if (croptograt){
  # poly <- as(extent(target), "SpatialPolygons")
  # projection(poly) <- projection(target)
  # g <- graticule(xlim, ylim, proj = projection(target),nverts=10, tiles=TRUE)}

  if (mask) {
    gratmask <- graticule::graticule(seq(xlim[1], xlim[2], length = 30),
                                     seq(ylim[1], ylim[2], length = 5), proj = projection(target), tiles = TRUE)
    if (bathy) {
    bathymetry <- fast_mask(bathymetry, gratmask)
    }
    if (coast) {
      suppressWarnings({
      coastline <- as(sf::st_union(sf::st_intersection(sf::st_as_sf(coastline), sf::st_as_sf(gratmask))), "Spatial")
      })
    }
  }
  if (bathy) raster::image(bathymetry, add = TRUE, col = bluepal, axes = FALSE)#grey(seq(0, 1, length = 40)))

  if (contours) contour(bathymetry, nlevels=1, levels=c(levels), col="black", add= TRUE)
  op <- par(xpd = FALSE)
  if (coast) plot(coastline, add = TRUE)
  par(op)
  if (input_points || input_lines) xy <- rgdal::project(cbind(xs, ys), prj)
  if (input_points) points(xy)
  if (input_lines) lines(xy)

  if (graticule) {
    grat <- sf::st_graticule(c(xmin(target), ymin(target), xmax(target), ymax(target)), crs = projection(target))
    op <- par(xpd = NA)
    plot_graticule(grat)
    par(op)
    graticule <- grat
  }

  par(pp)
  # if (croptograt){
  # plot(erase(poly, g), add = TRUE, col = "white")
  # invisible(list(bathy = bathymetry, coastline = coastline, target = target))
  # } else {

  invisible(structure(list(bathy = bathymetry, coastline = coastline, target = target, data = xy, graticule = graticule), class = "SOmap"))

  #}
}

#' Deprecated function
#'
#' Deprecated from SOmap
#' @param ... all arguments passed to new function
#'
#' @export
default_somap <- function(...) {
  .Deprecated("SOauto_map")
}
## from ?sf::st_graticule
plot_graticule <- function(g) {
  plot(sf::st_geometry(g), add = TRUE, col = 'grey')
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
  xlim <- sort(xlim)
  ylim <- sort(ylim)
  r <- asp * abs(diff(ylim)/diff(xlim))
  if(r <= 1) {  # X = 0, 1
    recip <- r / 2
    figure <- c(0, 1,
                0.5 - recip, 0.5 + recip)
  } else {     # Y = 0, 1
    recip <- (1/r) / 2
    figure <- c(0.5 - recip, 0.5 + recip,
                0, 1)
  }

  p <- par(fig = figure, new = FALSE, ...)
  plot.window(xlim=xlim,ylim=ylim,xaxs="i",yaxs="i", asp = asp)
  return(p)
}

fast_mask <- function(ras, poly) {
  cells <- tabularaster::cellnumbers(ras, sf::st_as_sf(poly))
  ras[setdiff(1:ncell(ras), cells$cell_)] <- NA
  ras
}
