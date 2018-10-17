## by default we'll store everything in this projection
psproj <- "+proj=stere +lat_0=-90 +lat_ts=-71 +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"

## CCAMLR reference data
library(sp)
library(raster)

get_ccamlr_data <- function(this_url) {
    working_dir <- tempfile()
    if (!dir.exists(working_dir)) dir.create(working_dir)
    download.file(this_url, destfile = file.path(working_dir, basename(this_url)))
    unzip(file.path(working_dir, basename(this_url)), exdir = working_dir)
}

## MPAs
files <- get_ccamlr_data("https://data.ccamlr.org/sites/default/files/mpa-shapefile-WGS84_0.zip") ## "https://data.ccamlr.org/sites/default/files/mpa-shapefile-EPSG102020_0.zip"
MPA1 <- spTransform(raster::shapefile(files[grepl("shp$", files)]), CRS(psproj))

## fix non-ascii to avoid check warnings
MPA1$Descr <- gsub("\uc2\ub0", "degrees ", MPA1$Descr)
chk <- sapply(names(MPA1), function(z) length(tools::showNonASCII(MPA1[[z]])) > 0)
if (any(chk)) stop("non-ASCII chars in MPA data")

## statistical areas
files <- get_ccamlr_data("https://data.ccamlr.org/sites/default/files/asd-shapefile-WGS84.zip")
CCAMLR1 <- spTransform(raster::shapefile(files[grepl("shp$", files)]), CRS(psproj))
CCAMLR1$Descr <- gsub("\uc2\ub0", "degrees ", CCAMLR1$Descr)
chk <- sapply(names(CCAMLR1), function(z) length(tools::showNonASCII(CCAMLR1[[z]])) > 0)
if (any(chk)) stop("non-ASCII chars in CCAMLR1 data")


## contintent (was land1)
library(sf)
library(dplyr)
continent <- rnaturalearth::ne_countries(scale = 110, returnclass = "sf") %>%
  st_buffer(dist = 0) %>%
  group_by(continent) %>% summarize()
g <- graticule::graticule(seq(-180, 180, by = 5), c(-90, 0), proj = psproj, tiles = TRUE)
continent <- as(sf::st_intersection(sf::st_buffer(st_transform(continent, psproj), 0), sf::st_as_sf(g) %>% sf::st_union()) %>%
                st_cast("MULTIPOLYGON"),
                "Spatial")


## fronts (was ocean1)
fronts_orsi <- spTransform(orsifronts::orsifronts, CRS(psproj))

SOmap_data <- list(CCAMLR_MPA = MPA1, CCAMLR_statistical_areas = CCAMLR1,
                   continent = continent, fronts_orsi = fronts_orsi)

devtools::use_data(SOmap_data, overwrite = TRUE, compress = "xz")

## clean up
d <- dir(working_dir, full.names = TRUE, recursive = TRUE)
unlink(d)
