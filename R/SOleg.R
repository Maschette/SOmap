
#' SOleg creating rounded legends for SOmap
#'
#' @param x
#' Object to plot (currently not used)
#' @param position
#' Where you want the legend ("topleft","topright", "bottomright")
#' @param col
#' Color pattern to use.
#' @param ticks
#' Number of ticks to include on the legend.
#' @param tlabs
#' Tick labels.
#' @param Trim
#' Trim that was used to create the SOmap.
#' @param label
#' Legend label.
#'
#' @return
#' Creates rounded legends
#' @export
#'
#' @examples
#' SOleg(position="topleft",
#'       col=heat.colors(80),
#'       ticks=4,
#'       tlabs = c("1","10","100","1000"),
#'       Trim=-45,
#'       label="Heat")
#'

SOleg<-function(x,
                position="topright",
                col=heat.colors(80),
                ticks=6,
                tlabs="",
                Trim=-45,
                label="Heat"
){

  if(position=="topleft"){
  bleg<-graticule::graticule(lons = seq(275,355, by=1),lats = c(Trim+3,Trim+5), tiles = TRUE, proj = raster::projection(Bathy))

  btick <- graticule::graticule(lats=c(Trim+4,Trim+7), lons = seq(275,355, by=80/(ticks-1)), proj=raster::projection(Bathy), tiles=F)

  k<-graticule::graticule(lons = seq(274,356, by=1),lats = c(Trim+10,Trim+6.75), tiles = TRUE, proj = raster::projection(Bathy))
  j<-graticule::graticule(lons = seq(274,356, by=1),lats = c(Trim+15,Trim+2), tiles = TRUE, proj = raster::projection(Bathy))
  #Ticks
  df2 <- data.frame(a = tlabs,
                    lon = seq(275,355, by=80/(ticks-1)),
                    lat=rep(Trim+9, ticks))
  sp::coordinates(df2) <- c("lon", "lat")
  raster::projection(df2) <- "+init=epsg:4326"
  lab_pos2 <- sp::spTransform(df2, raster::crs(raster::projection(Bathy)))
  #Label
  SRT<-45
  df3 <- data.frame(a = label,
                    lon = 315,
                    lat=rep(Trim+12.5))
  sp::coordinates(df3) <- c("lon", "lat")
  raster::projection(df3) <- "+init=epsg:4326"
  lab_pos3 <- sp::spTransform(df3, raster::crs(raster::projection(Bathy)))

  }



  if(position=="topright"){
  bleg<-graticule::graticule(lons = seq(5,85, by=1),lats = c(Trim+3,Trim+5), tiles = TRUE, proj = raster::projection(Bathy))

  btick <- graticule::graticule(lats=c(Trim+4,Trim+7), lons = seq(5,85, by=80/(ticks-1)), proj=raster::projection(Bathy), tiles=F)

  k<-graticule::graticule(lons = seq(4,86, by=1),lats = c(Trim+10,Trim+6.75), tiles = TRUE, proj = raster::projection(Bathy))
  j<-graticule::graticule(lons = seq(4,86, by=1),lats = c(Trim+15,Trim+2), tiles = TRUE, proj = raster::projection(Bathy))
  #Ticks
  df2 <- data.frame(a = tlabs,
                    lon = seq(5,85, by=80/(ticks-1)),
                    lat=rep(Trim+9, ticks))
  sp::coordinates(df2) <- c("lon", "lat")
  raster::projection(df2) <- "+init=epsg:4326"
  lab_pos2 <- sp::spTransform(df2, raster::crs(raster::projection(Bathy)))
  #Label
  SRT<--45
  df3 <- data.frame(a = label,
                    lon = 45,
                    lat=rep(Trim+12.5))
  sp::coordinates(df3) <- c("lon", "lat")
  raster::projection(df3) <- "+init=epsg:4326"
  lab_pos3 <- sp::spTransform(df3, raster::crs(raster::projection(Bathy)))
  }



  if(position=="bottomright"){
  bleg<-graticule::graticule(lons = seq(95,175, by=1),lats = c(Trim+3,Trim+5), tiles = TRUE, proj = raster::projection(Bathy))

  btick <- graticule::graticule(lats=c(Trim+4,Trim+7), lons = seq(95,175, by=80/(ticks-1)), proj=raster::projection(Bathy), tiles=F)

  k<-graticule::graticule(lons = seq(94,176, by=1),lats = c(Trim+10,Trim+6.75), tiles = TRUE, proj = raster::projection(Bathy))
  j<-graticule::graticule(lons = seq(94,176, by=1),lats = c(Trim+15,Trim+2), tiles = TRUE, proj = raster::projection(Bathy))
  #Ticks
  df2 <- data.frame(a = tlabs,
                    lon = seq(95,175, by=80/(ticks-1)),
                    lat=rep(Trim+9, ticks))
  sp::coordinates(df2) <- c("lon", "lat")
  raster::projection(df2) <- "+init=epsg:4326"
  lab_pos2 <- sp::spTransform(df2, raster::crs(raster::projection(Bathy)))
  #Label
  SRT<-45
  df3 <- data.frame(a = label,
                    lon = 135,
                    lat=rep(Trim+12.5))
  sp::coordinates(df3) <- c("lon", "lat")
  raster::projection(df3) <- "+init=epsg:4326"
  lab_pos3 <- sp::spTransform(df3, raster::crs(raster::projection(Bathy)))

  }

  ## Plot the legend
raster::plot(j, border=F,col="white", add=T) #White mask
  raster::plot(btick, add=T, col=1)
  raster::plot(bleg, lwd=2, add=T)
  raster::plot(bleg, border=F,  col=col, add=T)
  raster::plot(k, border=F,col="white", add=T)
  text(lab_pos2, labels=lab_pos2$a, cex= 0.75, adj=0.5)
  text(lab_pos3, labels=lab_pos3$a, cex= 1, adj=0.5,srt =SRT)## Need to set SRT during the position if statements.
}

#
#
# SRT<--40
# label="Heat"
# SOmap::SOmap()
#
# ##For bathymetry label
# SRT<--45
# label="Bathymetry"
# df3 <- data.frame(a = label,
#                   lon = 230,
#                   lat=rep(Trim+12.5))
# sp::coordinates(df3) <- c("lon", "lat")
# raster::projection(df3) <- "+init=epsg:4326"
# lab_pos3 <- sp::spTransform(df3, raster::crs(raster::projection(Bathy)))
#
# text(lab_pos3, labels=lab_pos3$a, cex= 1, adj=0.5,srt =SRT)
#
#
#
# ##Testing SOleg
# png(paste(Dat.Dir,'/SOMAP_Layers_6.png', sep=''), width=22, height=20, units='cm', res=300)
# SOmap::SOmap()
# SOleg(position="topleft",
#       col=heat.colors(80),
#       ticks=4,
#       tlabs = c("1","10","100","1000"),
#       Trim=-45,
#       label="Heat")
#
# SOleg(position="topright",
#       col=terrain.colors(80),
#       ticks=5,
#       tlabs = c("1","10","100","1000","10000"),
#       Trim=-45,
#       label="Terrain")
#
# SOleg(position="bottomright",
#       col=rainbow(80),
#       ticks=6,
#       tlabs = c("1","10","100","1000","10000","100000"),
#       Trim=-45,
#       label="rainbow")
# SRT<--45
# label="Bathymetry"
# df3 <- data.frame(a = label,
#                   lon = 230,
#                   lat=rep(Trim+12.5))
# sp::coordinates(df3) <- c("lon", "lat")
# raster::projection(df3) <- "+init=epsg:4326"
# lab_pos3 <- sp::spTransform(df3, raster::crs(raster::projection(Bathy)))
#
# text(lab_pos3, labels=lab_pos3$a, cex= 1, adj=0.5,srt =SRT)
#
# dev.off()
#
# # ## Things to do:
# # 1. add plot of dataset.
# # 2. ability to extract tick labels from datasets.
# #
# # 4. possibility of curved text labels?
