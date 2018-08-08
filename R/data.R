#' Geolocator data for single flicker individual
#'
#' A dataset containing raw geolocator light levels for several days for a
#' single flicker individual.
#'
#' @format A data frame with 3,600 rows and 6 variables:
#' \describe{
#'   \item{id}{Identity of the flicker}
#'   \item{lon}{Longitude}
#'   \item{lat}{Latitude}
#'   \item{date}{Date}
#'   \item{time}{Time of the measurement}
#'   \item{light}{Maximum light levels (lux) detected within the last two
#'   minutes}
#' }
#' @source \url{https://doi.org/10.1111/ibi.12206}
"flicker"

#' Geolocator data for single white-throated sparrow individual
#'
#' A dataset containing raw geolocator light levels for several days for a
#' single white-throated sparrow individual.
#'
#' @format A data frame with 1,152 rows and 6 variables:
#' \describe{
#'   \item{id}{Identity of the flicker}
#'   \item{lon}{Longitude}
#'   \item{lat}{Latitude}
#'   \item{date}{Date}
#'   \item{time}{Time of the measurement}
#'   \item{light}{Maximum light levels (lux) detected within the last five
#'   minutes}
#' }
#' @source \url{https://doi.org/10.1111/ibi.12206}
"wtsp"

#' Geolocator data for two flicker individuals
#'
#' A dataset containing raw geolocator light levels for several days for two
#' flicker individuals.
#'
#' @format A data frame with 13,680 rows and 6 variables:
#' \describe{
#'   \item{id}{Identity of the flicker}
#'   \item{lon}{Longitude}
#'   \item{lat}{Latitude}
#'   \item{date}{Date}
#'   \item{time}{Time of the measurement}
#'   \item{light}{Maximum light levels (lux) detected within the last two
#'   minutes}
#' }
#' @source \url{https://doi.org/10.1111/ibi.12206}
"flicker_mult"

#' Geolocator data recording sunrise and sunset
#'
#' A dataset containing raw geolocator light levels recording sunset and sunrise
#' events prior to placement on an animal
#'
#' @format A data frame with 2,160 rows and 5 variables:
#' \describe{
#'   \item{lon}{Longitude}
#'   \item{lat}{Latitude}
#'   \item{date}{Date}
#'   \item{time}{Time of the measurement}
#'   \item{light}{Maximum light levels (lux) detected within the last two
#'   minutes}
#' }
#' @source \url{https://doi.org/10.1111/ibi.12206}
"calib"
