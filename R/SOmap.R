#' Southern Ocean Map
#'
#' @description
#' Function for creating round Southern Ocean maps.
#'
#' @param Bathleg
#' Insert the bathymetry legend.
#' @param Border
#' Insert longitude border.
#' @param Trim
#' Longitude to trim map to.
#' @param Grats
#' Insert graticule grid.
#' @param bordercol
#' Colors for longitude border; Default is c("black","white").
#' @param gratcol
#' Color for graticule grid; Default is grey.
#' @param straight
#' Do you need a blank space on the side for a straight legend.
#' @param land
#' Plot land boundary
#' @param fronts
#' Plot ocean fronts: Subantarctic Front, Polar Front, Southern Antarctic Circumpolar Current Front
#' @param frontcols
#' colors for fronts
#'
#'  @return
#' Produces at the very base a round bathymetry map of the southern hemisphere.
#'
#' @examples
#' \dontrun{
#' Dat.Dir<-getwd()
#' png(paste(Dat.Dir,'/SOmap.png', sep=''), width=22, height=20, units='cm', res=600)
#' SOmap(Trim=-45, Grats=TRUE)
#' dev.off()
#' }
#' @export
#'

SOmap<-function(Bathleg=TRUE,
                Border= TRUE,
                Trim= -45,
                Grats= FALSE,
                straight=FALSE,
                land=TRUE,
                fronts=FALSE,
                frontcols=c("hotpink","orchid","plum"),
                bordercol=c("white","black"),
                gratcol="grey70") {
    ## data
    SOmap_data <- NULL
    data("SOmap_data", package = "SOmap", envir = environment())

    ## Set up color palette for bathy
    ramp2<-grDevices::colorRampPalette(c("#54A3D1", "#60B3EB", "#78C8F0", "#98D1F5", "#B5DCFF", "#BDE1F0", "#CDEBFA", "#D6EFFF", "#EBFAFF", "grey92", "grey94", "grey96", "white"))
    bluepal<-ramp2(68)
    bluepal2<-ramp2(80)
    ## Setup color border #
    bord<-graticule::graticule(lons = seq(-180,180, by=15),lats = c(Trim+2,Trim), tiles = TRUE, proj = raster::projection(Bathy))
    if (Bathleg) {
        ## White Mask #
        j<-graticule::graticule(lons = seq(-180,180, by=1),lats = c(-10,Trim+2), tiles = TRUE, proj = raster::projection(Bathy))
        ## Legend #
        ##Colored legend
        bleg<-graticule::graticule(lons = seq(185,265, by=1),lats = c(Trim+3,Trim+5), tiles = TRUE, proj = raster::projection(Bathy))

        btick <- graticule::graticule(lats=c(Trim+4,Trim+7), lons = seq(190,260, by=11.666), proj=raster::projection(Bathy), tiles=F)

        spud<-graticule::graticule(lons = seq(-180,180, by=1),lats = c(Trim+10,Trim+6.75), tiles = TRUE, proj = raster::projection(Bathy))
        df2 <- data.frame(a = c("-8000","-6000","-4000","-2000","0","2000", "4000"),
                          lon = seq(190,260, by=11.666),
                          lat=rep(Trim+9, 7))
        sp::coordinates(df2) <- c("lon", "lat")
        raster::projection(df2) <- "+init=epsg:4326"
        lab_pos2 <- sp::spTransform(df2, raster::crs(raster::projection(Bathy)))
    }
    ## Graticule dots #
    xx <- c(0,45, 90,135, 180,225, 270,315, 360); yy <- c(-90,-75, -60, -45, Trim)
    grat <- graticule::graticule(xx, yy, proj=raster::projection(Bathy))
    gratlab <- graticule::graticule_labels(lons=180,lats = c(-45,-30,-60,-75), xline=180, yline=-15, proj=raster::projection(Bathy))

    ## Set the Trim value depending on legend yes or no
    ifelse(Bathleg,q<-Trim+13,q<-Trim+2)


    ##Set Par
    op <- graphics::par(mar = rep(0.01, 4), oma= rep(0.0, 4), mai= rep(0.0, 4))
    ## Plot bathymetry
    if (straight==T) {
        potato<-raster::plot
        warning("Straight legends with round plots look terrible.", call. = "FALSE")
        potato(raster::trim(SOmap::latmask(Bathy, latitude = q)), col=bluepal,legend=FALSE, yaxt='n', xaxt='n', asp = 1)
    } else {
        potato<-raster::image
        potato(raster::trim(SOmap::latmask(Bathy, latitude = q)), col=bluepal, yaxt='n', xaxt='n', asp = 1)
    }
    graphics::box(col = "white")
    if (land) {
        plot(SOmap_data$continent, border = 1, add = TRUE)
    }
    ## fronts
    if (fronts) {
        plot(SOmap_data$fronts_orsi, add = TRUE, col = frontcols)
    }
    ## Graticule grid
    if (Grats) {
        raster::plot(grat,add=TRUE, col=gratcol, lty=3)
        text(gratlab, lab= parse(text = gratlab$lab), col="grey70",cex=0.5)
    }
    ## Legend
    if (Bathleg) {raster::plot(j, border=F,col="white", add=T) #White mask
        raster::plot(btick, add=T, col=1)
        raster::plot(bleg, lwd=2, add=T)
        raster::plot(bleg, border=F,  col=bluepal2, add=T)
        raster::plot(spud, border=F,col="white", add=T)
        text(lab_pos2, labels=lab_pos2$a, cex= 0.75, adj=0.5)
    }
    if (Border) {
        raster::plot(bord,  col=bordercol, add=TRUE)
    }
    ## Return Par
    graphics::par(op)
    invisible(NULL)
}
