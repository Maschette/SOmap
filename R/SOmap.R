#' Southern Ocean Map
#'
#' @description
#' Function for creating round Southern Ocean maps with inbuild base layers.
#'
#' @param Bathleg
#' Insert the bathymetry legend.
#' @param CCAMLR
#' Insert the CCAMLR boundaries.
#' @param CCAMLRlab
#' Insert the CCAMLR labels.
#' @param SSRU
#' Insert the CCAMLR small scale research unit boundaries.
#' @param SSRUlab
#' Insert the CCAMLR small scale research unit labels.
#' @param SSMU
#' Insert the CCAMLR small scale management unit boundaries.
#' @param SSMUlab
#' Insert the CCAMLR small scale management unit labels.
#' @param RB
#' Insert the CCAMLR research block boundaries.
#' @param RBlab
#' Insert the CCAMLR research block labels.
#' @param SPRFMORB
#' Insert the SPRFMO toothfish research block boundaries.
#' @param Border
#' Insert longitude border.
#' @param Trim
#' Longitude to trim map to.
#' @param Grats
#' Insert graticule grid.
#' @param EEZ
#' Insert Exclusive Economic Zones.
#' @param EEZlab
#' Insert Exclusive Economic Zone labels.
#' @param MPA
#' Insert CCAMLE Marine Protected Areas.
#' @param MPAlab
#' Insert CCAMLE Marine Protected Area labels.
#' @param IWC
#' Insert International Whaling Commission boundaries.
#' @param IWClab
#' Insert International Whaling Commission labels.
#' @param rbcol
#' Color for CCAMLR research blocks.
#' @param sprfmocol
#' Color for SPRFMO toothfish research blocks
#' @param ccamlrcol
#' Color for CCAMLR boundaries
#' @param ssrucol
#' Color for CCAMLR small scale research units.
#' @param ssmucol
#' Color for CCAMLR small scale management units.
#' @param eezcol
#' Color for Exclusive Economic Zone boundaries; Default is maroon.
#' @param mpacol
#' Color for CCAMLR Marine Protected Areas; Default is yellow.
#' @param bordercol
#' Colors for longitude border; Default is c("black","white").
#' @param gratcol
#' Color for graticule grid; Default is grey.
#' @param iwccol
#' Color for IWC boundaries; Default is blue.
#'
#' @return
#' Produces at the very base a round bathymetry map of the southern hemisphere.
#'
#' @example
#' Dat.Dir<-getwd()
#' png(paste(Dat.Dir,'/SOmap.png', sep=''), width=22, height=20, units='cm', res=600)
#' SOmap(CCAMLRlab= F,CCAMLR=T, ccamlrcol = 1, Trim=-45)
#' dev.off()
#'
#' @export
#'

SOmap<-function(Bathleg=TRUE,
                CCAMLR= FALSE,
                CCAMLRlab= FALSE,
                SSRU= FALSE,
                SSRUlab = FALSE,
                SSMU= FALSE,
                SSMUlab= FALSE,
                RB= FALSE,
                RBlab= FALSE,
                SPRFMORB= FALSE,
                Border= TRUE,
                Trim= -45,
                Grats= FALSE,
                EEZ=FALSE,
                EEZlab=FALSE,
                MPA=FALSE,
                MPAlab=FALSE,
                IWC=FALSE,
                IWClab=FALSE,
                rbcol=3,
                sprfmocol='grey50',
                ccamlrcol=2,
                ssrucol="grey50",
                ssmucol="grey70",
                eezcol="maroon",
                mpacol= "yellow",
                bordercol=c("white","black"),
                gratcol="grey70",
                iwccol="blue"){

#### Set up color palette for bathy #
  ramp2<-grDevices::colorRampPalette(c("#54A3D1","#60B3EB","#78C8F0","#98D1F5","#B5DCFF","#BDE1F0","#CDEBFA","#D6EFFF","#EBFAFF","grey92","grey94","grey96", "white"))
  bluepal<-ramp2(68)
  bluepal2<-ramp2(80)
#### Setup color border #
  bord<-graticule::graticule(lons = seq(-180,180, by=15),lats = c(Trim+2,Trim), tiles = TRUE, proj = raster::projection(Bathy))
  if(Bathleg==TRUE){
 #### White Mask #
  j<-graticule::graticule(lons = seq(-180,180, by=1),lats = c(Trim+15,Trim+2), tiles = TRUE, proj = raster::projection(Bathy))
#### Legend #
  ##Colored legend
  bleg<-graticule::graticule(lons = seq(185,265, by=1),lats = c(Trim+3,Trim+5), tiles = TRUE, proj = raster::projection(Bathy))

  btick <- graticule::graticule(lats=c(Trim+4,Trim+7), lons = seq(190,260, by=11.666), proj=raster::projection(Bathy), tiles=F)

  k<-graticule::graticule(lons = seq(-180,180, by=1),lats = c(Trim+10,Trim+6.75), tiles = TRUE, proj = raster::projection(Bathy))
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
  #### CCAMLR Labels
  cclabs<-c("88.3","48.4","88.2","48.2","48.3","58.4.3a","58.4.3b","58.5.2","48.5","48.6","58.4.1","88.1","58.4.4a","58.7","58.6","58.5.1","58.4.4b")



  ## Set the Trim value depending on legend yes or no
  ifelse(Bathleg==TRUE,q<-Trim+13,q<-Trim+2)
  ##Set Par
  op <- graphics::par(mar = rep(0.01, 4), oma= rep(0.0, 4), mai= rep(0.0, 4))
  ## Plot bathymetry
  raster::image(raster::trim(SOmap::latmask(Bathy, latitude = q)), col=bluepal)#, yaxt='n', xaxt='n',)
  graphics::box(col = 2)
  if(IWC==TRUE){
    # iwc<-graticule::graticule(c(-170,-120,-60,0,70,130,-230), c(-90,Trim+0.5), proj=raster::projection(Bathy))
    # plot(iwc,col=iwccol, add = TRUE)
    graphics::lines(rgdal::project(rbind(c(-170, Trim), c(-170, -78.40)), raster::projection(Bathy)), col=iwccol)
    graphics::lines(rgdal::project(rbind(c(-120, Trim), c(-120, -73.844137)), raster::projection(Bathy)), col=iwccol)
    graphics::lines(rgdal::project(rbind(c(-60, -65.168), c(-60, -75.146206)), raster::projection(Bathy)), col=iwccol)
    graphics::lines(rgdal::project(rbind(c(-60, Trim), c(-60, -62.4505)), raster::projection(Bathy)), col=iwccol)
    graphics::lines(rgdal::project(rbind(c(0, Trim), c(0, -69.596701)), raster::projection(Bathy)), col=iwccol)
    graphics::lines(rgdal::project(rbind(c(70, Trim), c(70, -68.366691)), raster::projection(Bathy)), col=iwccol)
    graphics::lines(rgdal::project(rbind(c(130, Trim), c(130, -66.295027)), raster::projection(Bathy)), col=iwccol)
        }
  if(IWClab==TRUE){
    df3 <- data.frame(a = c("Area VI","Area I","Area II","Area III","Area IV","Area V"),
                      lon = c(-145,-90,-30,35,100,160),
                      lat=rep(-60, 6))
    sp::coordinates(df3) <- c("lon", "lat")
    raster::projection(df3) <- "+init=epsg:4326"
    lab_pos3 <- sp::spTransform(df3, raster::crs(raster::projection(Bathy)))

    text(lab_pos3, labels = lab_pos3$a,col=iwccol, cex = 0.4, pos=1, offset=-0.05)
  }
  #Graticule grid
  if(Grats==TRUE){
    plot(grat,add=TRUE, col=gratcol, lty=3)
    text(gratlab, lab= parse(text = gratlab$lab), col="grey70",cex=0.5)}

  if(RB==TRUE){
    #load("RB.rda")
  plot(RB1, border=rbcol, add=TRUE)}
  if(RBlab==TRUE){
    text(RB1, labels = RB1@data$ShortLabel,col=rbcol, cex = 0.4, pos=4, offset=0.3)}
  if(SPRFMORB==TRUE){
    sprfmoa<-graticule::graticule(lats=c(-59.9,-57.9),lons= c(-155.3333,-150),proj = raster::projection(Bathy))
    plot(sprfmoa, add=TRUE, col=sprfmocol)
    sprfmob<-graticule::graticule(lats=c(-59.0,-60.0),lons= c(-142.1666667,-145.833333),proj = raster::projection(Bathy))
    plot(sprfmob, add=TRUE, col=sprfmocol) }
  if(SSRU==TRUE){
    #load("SSRU.rda")
    plot(SSRU1,border=ssrucol, add = TRUE)}
  if(SSRUlab==TRUE){
    text(SSRU1, labels = SSRU1@data$ShortLabel,col=ssrucol, cex = 0.4, pos=1, offset=-0.05)}
  if(SSMU==TRUE){
    #load("SSMU.rda")
    plot(SSMU1,border=ssmucol, add = TRUE)}
  if(SSMUlab==TRUE){
    text(SSMU1, labels = SSMU1@data$LongLabel,col=ssmucol, cex = 0.5, pos=1, offset=0.6)}
  if(CCAMLR==TRUE){
    #load("CCAMLR.rda")
  plot(CCAMLR1,border=ccamlrcol, add = TRUE)}
  if(CCAMLRlab==TRUE){
    text(CCAMLR1[CCAMLR1$GAR_Long_L!="48.1"&CCAMLR1$GAR_Long_L!="58.4.2",], labels = cclabs, col=ccamlrcol,cex = 0.5, pos=1, offset=-0.3)
    text(CCAMLR1[CCAMLR1$GAR_Long_L=="58.4.2",], labels = "58.4.2", col=ccamlrcol,cex = 0.5, pos=3, offset=0.5)
    text(CCAMLR1[CCAMLR1$GAR_Long_L=="48.1",], labels = "48.1", col=ccamlrcol,cex = 0.5, pos=2, offset=-0.1)}

  # EEZ
  if(EEZ==TRUE){
    #load("EEZ.rda")
    plot(EEZ1,border=eezcol, add = TRUE)}
  if(EEZlab==TRUE){
    text(EEZ1, labels = EEZ1@data$Names, col=eezcol,cex = 0.35, pos=4, offset=0.8)}
  # MPA
  if(MPA==TRUE){
    #load("MPA.rda")
    plot(MPA1,border=mpacol, add = TRUE)
    }
  if(MPAlab==TRUE){
    text(MPA1, labels = MPA1@data$ShortLabel, col=mpacol,cex = 0.35, pos=1, offset=0.2)}
  #Legend
  if(Bathleg==TRUE) {plot(j, border=F,col="white", add=T) #White mask
    plot(btick, add=T, col=1)
    plot(bleg, lwd=2, add=T)
    plot(bleg, border=F,  col=bluepal2, add=T)
    plot(k, border=F,col="white", add=T)
    text(lab_pos2, labels=lab_pos2$a, cex= 0.75, adj=0.5)}
  if(Border==TRUE){
    plot(bord,  col=bordercol, add=TRUE)}
  ## Return Par
  graphics::par(op)
  print("Congratulations, you did a thing!")
}












