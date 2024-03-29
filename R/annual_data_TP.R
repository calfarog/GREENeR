#' @title Annual data TP
#' @description Defines the sources of nutrient (phosphorus) for each year and catchments.
#' @format A data frame with 12 variables:
#' \describe{
#'   \item{\code{BasinID}}{integer. The basin unique identifier.}
#'   \item{\code{YearValue}}{integer. The year for which data are defined.}
#'   \item{\code{HydroID}}{integer positive. Unique catchment identifier.}
#'   \item{\code{NextDownID}}{integer. Unique identifier of the catchment to
#'   which the catchment goes.}
#'   \item{\code{Bg}}{double. Annual amount of phosphorus background losses
#'   (ton/yr).}
#'   \item{\code{Min}}{double. Annual amount of phosphorus mineral fertilisers
#'   (ton/yr).}
#'   \item{\code{Man}}{double. Annual amount of phosphorus in manure fertilisers
#'   (ton/yr).}
#'   \item{\code{Sd}}{double. Phosphorus input from scattered dwellings
#'   (ton/yr).}
#'   \item{\code{Ps}}{double. Phosphorus input from point sources (ton/yr).}
#'   \item{\code{YearlyMass}}{double. Observed annual total phosphorus load
#'   (TP ton/yr) from monitoring station data.}
#'   \item{\code{ForestFraction}}{double. Non-agricultural land cover in the
#'   catchment (fraction).}
#'   \item{\code{InvNrmRain}}{double. Inverse of normalized rainfall.}
#'}
"annual_data_TP"
