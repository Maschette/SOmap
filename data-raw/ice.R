ice <- raadtools::readice(latest = TRUE)
date <- getZ(ice)
psproj <- "+proj=stere +lat_0=-90 +lat_ts=-71 +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
ice <- setZ(raster::projectRaster(ice, crs = psproj), date)
ice[ice < 1] <- NA
usethis::use_data(ice)
