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


## continent (was land1)
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



## eez and eez_coast (was EEZ1)
#devtools::install_github('SymbolixAU/geojsonsf')
library(geojsonsf)
eezlist <- lapply(c(25513, 8383, 8385, 8388, 8387, 8384, 8399), function(id) {
  key <- if (id %in% c(25513)) "eez_iho" else "eez"
  this_url <- paste0("http://geo.vliz.be/geoserver/wfs?request=getfeature&service=wfs&version=1.1.0&typename=MarineRegions:", key, "&outputformat=json&filter=%3CPropertyIsEqualTo%3E%3CPropertyName%3Emrgid%3C%2FPropertyName%3E%3CLiteral%3E", id, "%3C%2FLiteral%3E%3C%2FPropertyIsEqualTo%3E") ## geojson

  tf <- tempfile(fileext = ".json")
  download.file(this_url, destfile = tf)
  geojson_sf(tf)
})

common <- purrr::reduce(purrr::map(eezlist, names), intersect)
eez_coast <- sf::st_transform(do.call(rbind, purrr::map(eezlist, ~.x[common])), psproj)

## kill the coast
library(dplyr)
eez <- st_cast(eez_coast, "POLYGON")
g <- st_geometry(eez)
eez <- st_set_geometry(eez, sf::st_sfc(lapply(g, function(x) sf::st_polygon(x[1])), crs = st_crs(g))) %>% dplyr::select(territory1, everything())


eez_coast <- as(eez_coast, "Spatial")
eez <- as(eez, "Spatial")





SOmap_data <- list(CCAMLR_MPA = MPA1, CCAMLR_statistical_areas = CCAMLR1,
                   continent = continent, fronts_orsi = fronts_orsi,
                   eez = eez, eez_coast = eez_coast)

devtools::use_data(SOmap_data, overwrite = TRUE, compress = "xz")
