#' Contextual data for Southern Ocean maps
#'
#' Various spatial datasets that are commonly used on Southern Ocean maps.
#'
#' @docType data
#'
#' @usage data(SOmap_data)
#'
#' @format A list containing the following elements:
#' \itemize{
#'   \item CCAMLR_MPA
#'     \itemize{
#'       \item Description: current marine protected areas
#'       \item Source: CCAMLR
#'       \item URL: https://data.ccamlr.org/dataset/marine-protected-areas
#'       \item License: not specified
#'     }
#'   \item CCAMLR_statistical_areas
#'     \itemize{
#'       \item Description: Statistical areas, subareas and divisions are used globally for the purpose of reporting fishery statistics. CCAMLR's Convention Area in the Southern Ocean is divided, for statistical purposes, into Area 48 (Atlantic Antarctic) between 70W and 30E, Area 58 (Indian Ocean Antarctic) between 30 and 150E, and Area 88 (Pacific Antarctic) between 150E and 70W. These areas, which are further subdivided into subareas and divisions, are managed by CCAMLR.
#'       \item Source: CCAMLR
#'       \item URL: https://data.ccamlr.org/dataset/statistical-areas-subareas-and-divisions
#'       \item License: Public domain
#'     }
#'   \item fronts_orsi
#'     \itemize{
#'       \item Description: Southern Ocean fronts as defined by Orsi et al. 1995
#'       \item Source: orsifronts
#'       \item URL: https://github.com/AustralianAntarcticDivision/orsifronts
#'       \item License: see orsifronts
#'     }
#' }
#' @keywords datasets
"SOmap_data"

#' Sea ice
#'
#' Example sea ice concentration data from the Southern Ocean (2018-10-15).
#' (See "data-raw/ice.R").
#' @docType data
#' @name ice
#' @examples
#' xy <- coordinates(spTransform(as(SOmap_data$fronts_orsi, "SpatialPoints"), "+init=epsg:4326"))
#' ## just because you can doesn't mean you should ...
#' SOauto_map(xy[,1], xy[,2], bathy = ice, input_points = FALSE, levels = c(15, 30, 60, 90))
NULL

#' Bathymetric data for maps
#'
#' Bathymetric data reprocessed from the GEBCO_2014 Grid data set.
#'
#' @docType data
#'
#' @usage data(Bathy)
#'
#' @format An object of class \code{"RasterLayer"}
#'
#' @keywords datasets
#'
#' @references The GEBCO_2014 Grid, version 20150318
#'
#' @source \href{https://www.gebco.net/data_and_products/gridded_bathymetry_data/}{GEBCO}
"Bathy"
