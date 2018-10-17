prj <- "+proj=stere +lat_0=-90 +lat_ts=-71 +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
src <- readAll(raadtools::readtopo("gebco_14"))  ##, xylim = extent(-180, 180, -90, 0))

r <- raster(projectExtent(raster(extent(-180, 180, -90, -5), crs  = "+init=epsg:4326"), prj))
## cleanup and rebuild
r <- raster(spex::buffer_extent(r, 16000), crs = prj)
res(r) <- 16000
Bathy <- projectRaster(src, r)
dataType(Bathy) <- "INT2S"
Bathy <- setValues(Bathy, as.integer(values(Bathy)))

devtools::use_data(Bathy, overwrite = TRUE)
