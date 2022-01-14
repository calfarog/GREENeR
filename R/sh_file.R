#' @title Sh file
#' @description An sf (simple features) object  that includes attributes and
#' geometries in the form of a data frame. Data frame (or tibble) with rows of
#' features, columns of attributes, and a special geometry column that contains
#' the spatial aspects of the features
#' @format Simple feature collection with 6 features and 14 fields:
#' \describe{
#'   \item{\code{OBJECTID}}{integer Identifier of the spatial object (one for
#'   each catchment)}
#'   \item{\code{HydroID}}{integer Unique catchment identifier}
#'   \item{\code{NextDownID}}{integer Unique identifier of the catchment to
#'   which the catchment goes}
#'   \item{\code{BasinID}}{integer Identifier of the Basin to all  catchment
#'   belong}
#'   \item{\code{JunctionID}}{integer Unique identifier of the catchment to
#'   which the catchment goes. Note that for the outlet JunctionID == -1}
#'   \item{\code{AreaSqkm}}{double Area of each catchment}
#'   \item{\code{DrainAreaS}}{double Drain Area of each catchment}
#'   \item{\code{Shreve}}{integer Indicates Shreve order, i.e. in headwater = 1;
#'   at the outlet Shreve = max(Shreve)}
#'   \item{\code{geometry}}{sfc objects represent the geometry of a single
#'   feature and contains information about the feature’s coordinates,
#'   dimension, and type of geometry}
#'}
#' @keywords internal
"sh_file"
