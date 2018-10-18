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
#'   \item CCAMLR_research_blocks
#'     \itemize{
#'       \item Description: A defined spatial area in which research fishing on toothfish is conducted under a research plan agreed by the Commission
#'       \item Source: CCAMLR
#'       \item URL: https://data.ccamlr.org/dataset/research-blocks
#'       \item License: Not specified
#'     }
#'   \item CCAMLR_SSMU
#'     \itemize{
#'       \item Description: Small-scale management units (SSMUs) are designed to be used as a basis for subdividing the precautionary catch limit for krill in Subareas 48.1, 48.2, 48.3 and 48.4, and in developing management procedures for the krill fishery that can adequately account for localised effects on krill predators (SC-CAMLR-XXI, paragraphs 3.16 to 3.18). The boundaries of the SSMUs are based on predator foraging ranges (refer SC-CAMLR-XXI, Annex 4 and Trathan et al, 2008).
#'       \item Source: CCAMLR
#'       \item URL: https://data.ccamlr.org/dataset/small-scale-management-units
#'       \item License: Not specified
#'     }
#'   \item CCAMLR_SSRU
#'     \itemize{
#'       \item Description: Small-scale research units (SSRUs) are designed to be used as a basis for subdividing the precautionary catch limit for toothfish in exploratory fisheries, and in condcuting research fishing and developing stock assessments. The boundaries of the SSRUs are defined in Conservation Measure 41-01 (2013). CCAMLR Secretariat (2013).
#'       \item Source: CCAMLR
#'       \item URL: https://data.ccamlr.org/dataset/small-scale-research-units
#'       \item License: Not specified
#'     }
#'   \item CCAMLR_statistical_areas
#'     \itemize{
#'       \item Description: Statistical areas, subareas and divisions are used globally for the purpose of reporting fishery statistics. CCAMLR's Convention Area in the Southern Ocean is divided, for statistical purposes, into Area 48 (Atlantic Antarctic) between 70W and 30E, Area 58 (Indian Ocean Antarctic) between 30 and 150E, and Area 88 (Pacific Antarctic) between 150E and 70W. These areas, which are further subdivided into subareas and divisions, are managed by CCAMLR.
#'       \item Source: CCAMLR
#'       \item URL: https://data.ccamlr.org/dataset/statistical-areas-subareas-and-divisions
#'       \item License: Public domain
#'     }
#'   \item continent
#'     \itemize{
#'       \item Description: Coastline, details TBA
#'       \item Source: TBA
#'       \item URL: TBA
#'       \item License: TBA
#'     }
#'   \item EEZ
#'     \itemize{
#'       \item Description: An exclusive economic zone (EEZ) is a sea zone prescribed by the United Nations Convention on the Law of the Sea over which a state has special rights regarding the exploration and use of marine resources. Included are the EEZs that intersect the CCAMLR convention area. Some of these are currently in dispute.
#'       \item Source: CCAMLR
#'       \item URL: https://data.ccamlr.org/dataset/exclusive-economic-zones
#'       \item License: Not specified
#'     }
#'   \item fronts_orsi
#'     \itemize{
#'       \item Description: Southern Ocean fronts as defined by Orsi et al. 1995
#'       \item Source: orsifronts
#'       \item URL: https://github.com/AustralianAntarcticDivision/orsifronts
#'       \item License: see orsifronts
#'     }
#'   \item seaice_feb and seaice_oct
#'     \itemize{
#'       \item Description: median October and February sea ice extent
#'       \item Source: Fetterer, F., K. Knowles, W. Meier, M. Savoie, and A. K. Windnagel. 2017, updated daily. Sea Ice Index, Version 3. Boulder, Colorado USA. NSIDC: National Snow and Ice Data Center
#'       \item URL: https://doi.org/10.7265/N5K072F8
#'       \item License: Please cite
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
