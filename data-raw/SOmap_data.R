## by default we'll store everything in this projection
psproj <- "+proj=stere +lat_0=-90 +lat_ts=-71 +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"

## CCAMLR reference data
library(sp)
library(raster)

get_unzip_data <- function(this_url) {
    working_dir <- tempfile()
    if (!dir.exists(working_dir)) dir.create(working_dir)
    download.file(this_url, destfile = file.path(working_dir, basename(this_url)))
    unzip(file.path(working_dir, basename(this_url)), exdir = working_dir)
}

## MPAs
files <- get_unzip_data("https://data.ccamlr.org/sites/default/files/mpa-shapefile-WGS84_0.zip") ## "https://data.ccamlr.org/sites/default/files/mpa-shapefile-EPSG102020_0.zip"
MPA1 <- spTransform(raster::shapefile(files[grepl("shp$", files)]), CRS(psproj))

## fix non-ascii to avoid check warnings
MPA1$Descr <- NULL #gsub("\uc2\ub0", "degrees ", MPA1$Descr)
chk <- sapply(names(MPA1), function(z) length(tools::showNonASCII(MPA1[[z]])) > 0)
if (any(chk)) stop("non-ASCII chars in MPA data")

## statistical areas
files <- get_unzip_data("https://data.ccamlr.org/sites/default/files/asd-shapefile-WGS84.zip")
CCAMLR1 <- spTransform(raster::shapefile(files[grepl("shp$", files)]), CRS(psproj))
CCAMLR1$Descr <- NULL #gsub("\uc2\ub0", "degrees ", CCAMLR1$Descr)
chk <- sapply(names(CCAMLR1), function(z) length(tools::showNonASCII(CCAMLR1[[z]])) > 0)
if (any(chk)) stop("non-ASCII chars in CCAMLR1 data")

## research blocks
files <- get_unzip_data("https://data.ccamlr.org/sites/default/files/rb-shapefile-WGS84_0.zip")
RB1 <- spTransform(raster::shapefile(files[grepl("shp$", files)]), CRS(psproj))

## SSRUs
files <- get_unzip_data("https://data.ccamlr.org/sites/default/files/ssru-shapefile-WGS84.zip")
SSRU1 <- spTransform(raster::shapefile(files[grepl("shp$", files)]), CRS(psproj))
SSRU1$Descr <- NULL ## gsub("\uc2\ub0", "degrees ", SSRU1$Descr)
chk <- sapply(names(SSRU1), function(z) length(tools::showNonASCII(SSRU1[[z]])) > 0)
if (any(chk)) stop("non-ASCII chars in SSRU1 data")

## SSMUs
files <- get_unzip_data("https://data.ccamlr.org/sites/default/files/ssmu-shapefile-WGS84.zip")
SSMU1 <- spTransform(raster::shapefile(files[grepl("shp$", files)]), CRS(psproj))
chk <- sapply(names(SSMU1), function(z) length(tools::showNonASCII(SSMU1[[z]])) > 0)
if (any(chk)) stop("non-ASCII chars in SSMU1 data")


## continent (was land1)
library(sf)
library(dplyr)
library(sp)
continent <- rnaturalearth::ne_countries(scale = 50, returnclass = "sp")
continent <- continent[coordinates(continent)[,2] < 20, ]

library(spbabel)
sptable(continent) <- sptable(continent) %>% dplyr::filter(y_ > -85, island_)

continent <- continent %>% st_as_sf() %>%  st_buffer(dist = 0) %>%
  group_by(continent) %>% summarize() %>% st_transform(psproj) %>% as("Spatial")

#plot(st_as_sf(continent[2, ]))


g <- graticule::graticule(seq(-180, 180, by = 5), c(-84, 0), proj = psproj, tiles = TRUE)


chk <- sapply(names(continent), function(z) length(tools::showNonASCII(continent[[z]])) > 0)
if (any(chk)) stop("non-ASCII chars in continent data")

## fronts (was ocean1)
fronts_orsi <- spTransform(orsifronts::orsifronts, CRS(psproj))
chk <- sapply(names(fronts_orsi), function(z) length(tools::showNonASCII(fronts_orsi[[z]])) > 0)
if (any(chk)) stop("non-ASCII chars in fronts_orsi data")

## eez and eez_coast (was EEZ1)
files <- get_unzip_data("https://data.ccamlr.org/sites/default/files/eez-shapefile-WGS84.zip")
EEZ1 <- spTransform(raster::shapefile(files[grepl("shp$", files)]), CRS(psproj))

if (FALSE) {
    ## or to derive from the original files from marineregions.org
    ##devtools::install_github('SymbolixAU/geojsonsf')
    library(geojsonsf)
    eezlist <- lapply(c(25513, 8383, 8385, 8388, 8387, 8384, 8399), function(id) {
  key <- if (id %in% c(25513)) "eez_iho" else "eez"
  this_url <- paste0("http://geo.vliz.be/geoserver/wfs?request=getfeature&service=wfs&version=1.1.0&typename=MarineRegions:", key, "&outputformat=json&filter=%3CPropertyIsEqualTo%3E%3CPropertyName%3Emrgid%3C%2FPropertyName%3E%3CLiteral%3E", id, "%3C%2FLiteral%3E%3C%2FPropertyIsEqualTo%3E") ## geojson

  tf <- tempfile(fileext = ".json")
  download.file(this_url, destfile = tf)
  geojson_sf(tf)
})

    common <- purrr::reduce(purrr::map(eezlist, names), intersect)
    eez_coast <- st_geometry(sf::st_transform(do.call(rbind, purrr::map(eezlist, ~.x[common])), psproj))

    ## kill the coast
    library(dplyr)
    eez <- st_cast(eez_coast, "POLYGON")
    ##g <- st_geometry(eez)
    ##eez <- st_set_geometry(eez, sf::st_sfc(lapply(g, function(x) sf::st_polygon(x[1])), crs = st_crs(g))) %>% dplyr::select(territory1, everything())
    eez <- sf::st_sfc(lapply(eez, function(x) sf::st_polygon(x[1])), crs = st_crs(eez))
    eez_coast <- as(eez_coast, "Spatial")
    EEZ1 <- as(eez, "Spatial")
}


## october and february ice extents
files <- get_unzip_data("ftp://sidads.colorado.edu/DATASETS/NOAA/G02135/south/monthly/shapefiles/shp_median/median_extent_S_10_1981-2010_polyline_v3.0.zip")
seaice_oct <- spTransform(raster::shapefile(files[grepl("shp$", files)]), CRS(psproj))
files <- get_unzip_data("ftp://sidads.colorado.edu/DATASETS/NOAA/G02135/south/monthly/shapefiles/shp_median/median_extent_S_02_1981-2010_polyline_v3.0.zip")
seaice_feb <- spTransform(raster::shapefile(files[grepl("shp$", files)]), CRS(psproj))


## elephant seal tracking
ellie_url <- "https://github.com/ianjonsen/bsam/raw/master/data/ellie.RData"
tfile <- tempfile(fileext = "RData")
curl::curl_download(ellie_url, destfile = tfile)
load(tfile)
mirounga_leonina <- ellie

SOmap_data <- list(CCAMLR_MPA = MPA1, CCAMLR_statistical_areas = CCAMLR1, CCAMLR_research_blocks = RB1,
                   CCAMLR_SSRU = SSRU1, CCAMLR_SSMU = SSMU1,
                   continent = continent, fronts_orsi = fronts_orsi,
                   seaice_feb = seaice_feb, seaice_oct = seaice_oct,
                   mirounga_leonina = mirounga_leonina,
                   EEZ = EEZ1)

devtools::use_data(SOmap_data, overwrite = TRUE, compress = "xz")

