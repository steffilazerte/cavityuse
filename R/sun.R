#' Detect patterns of sunrise and sunset
#'
#' Detects patterns of sunrise and sunset in geolocator data.
#'
#' @param data Data frame. Raw light data. Requires two columns: "time" and
#'   "light"
#' @param n Numeric. Number of light observations to use when assessing a
#'   sunrise/sunset event
#' @param range Numeric. Minimum lux difference between minimum and maximum
#'   light levels in sunrise and sunset event
#' @param cutoff Numeric. R-Squared cutoff value to assess the model fit to
#'   sunrise and sunset. Higher values are more stringent, lower values are
#'   less.
#' @param loc Numeric vector. Longitude and Latitude of the observations (if not
#'   in the data, this must be provided).
#' @param local Logical. Restrict data to expect windows of sunrise/sunset based
#'   on coordinates (will speed up the function, but may result in problems if
#'   sunrise/sunset occur close to midnight)
#' @param filter_problems Logical. Remove problematic sunrise/sunset events?
#'   Problematic events are defined as those which are preceded/proceeded by
#'   unexpectedly variable light levels (True sunrise/sunset events should only
#'   be preceded/proceeded by darkness)
#'
#' @details This function looks for sunrise/sunset events by matching \code{n}
#'   points to a cubic polynomial regression. Events with a high degree of model
#'   fit (determined by an R-squared cutoff of \code{cutoff} are returned.
#'
#'   Because this essentially fits models to every point in the data, it can be
#'   very time consuming. \code{local = TRUE} attempts to reduce this time by
#'   first filtering the data to blocks of time which should contain
#'   sunrise/sunset, based on the coordinates of the data.
#'
#'   Only one sunrise and one sunset event are allowed per day. If
#'   \code{filter_problems = TRUE}, then sunrise events that are preceded by
#'   variable light levels and sunset events proceeded by variable light levels
#'   are omitted (before sunrise and after sunset, it should be dark).
#'
#' @return Data frame containing sunrise and sunset events
#' @export
#'
#' @examples
#'
#' # Calibration data should have perfect sunrise/sunset event detection
#' s <- sun_detect(calib)
#' cavity_plot(calib, sun =s)
#'
#' # Sunrise/sunset events missed with extra stringent cutoff values
#' s <- sun_detect(calib, cutoff = 0.9995)
#' cavity_plot(calib, sun = s)
#'
#'
sun_detect <- function(data, n = 10, range = 10, cutoff = 0.95, loc = NULL,
                       local = TRUE, filter_problems = TRUE) {

  check_cols(data, c("time", "light"))
  check_time(data$time)
  check_date(data)
  check_class(data$light, "numeric")
  loc <- check_loc(data, loc)

  # Need to think about range cut off (what is an appropriate value?)

  # angle = 6 Civil
  # angle = 12 Nautical
  # angle = 18 Astronomical
  interval <- stats::median(as.numeric(difftime(data$time,
                                                dplyr::lag(data$time))),
                     na.rm = TRUE)
  s <- data
  # Confine to actual local sunset/sunrise times
  if(local) {
    sun_local <- sun_local(loc = c(loc[1], loc[2]),
                           date = unique(s$date),
                           tz = lubridate::tz(s$time))

    s <- s %>%
      dplyr::left_join(sun_local, by = "date") %>%
      dplyr::mutate(type = dplyr::case_when(
        .data$time >= .data$sunrise & .data$time <=
          .data$sunrise + lubridate::hours(3) ~ "sunrise",
        .data$time <= .data$sunset & .data$time >=
          .data$sunset - lubridate::hours(3) ~ "sunset")) %>%
      dplyr::filter(!is.na(.data$type)) %>%
      dplyr::select(-.data$sunrise, -.data$sunset)
  }

  s <- s %>%
    dplyr::group_by(.data$date) %>%
    dplyr::mutate(forward = n_lag(.data$light, n, dir = "forward",
                                  return = "list")) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(n_na = purrr::map_dbl(.data$forward, ~sum(is.na(.x)))) %>%
    dplyr::filter(.data$n_na == 0) %>%
    dplyr::mutate(n_unique = purrr::map_dbl(.data$forward,
                                            ~length(unique(.x)))) %>%
    dplyr::filter(.data$n_unique > 4) %>%
    dplyr::mutate(n_range = purrr::map_dbl(.data$forward,
                                           ~as.integer(max(.x) - min(.x)))) %>%
    dplyr::filter(.data$n_range >= range) %>%
    dplyr::mutate(n_step = purrr::map_dbl(.data$forward,
                                          ~stats::median(diff(.x)))) %>%
    dplyr::filter(abs(.data$n_step) < 12) %>%
    dplyr::mutate(df = purrr::map(.data$forward,
                                  ~data.frame(y = .x, x = 1:length(.x))),
                  model = purrr::map(.data$df, ~stats::lm(y ~ poly(x, 3), data = .x)),
                  coef = purrr::map(.data$model, ~broom::glance(.x)),
                  dir = purrr::map_dbl(.data$model, ~stats::coef(.x)[2]))

  if(nrow(s) > 0) {
    s <- dplyr::select(s, .data$date, .data$time, .data$light,
                       .data$n_range, .data$coef, .data$dir, .data$type) %>%
      tidyr::unnest(.data$coef) %>%
      dplyr::select(-.data$df, -.data$logLik, -.data$AIC, -.data$BIC,
                    -.data$deviance, -.data$df.residual) %>%
      dplyr::filter(!is.nan(.data$p.value),
                    .data$adj.r.squared > cutoff) %>%
      dplyr::mutate(dir = dplyr::if_else(.data$dir < 0, "sunset", "sunrise"),
                    n = as.integer(n),
                    dur = n * interval) %>%
      # i.e. sunrise must be in sunrise time
      dplyr::filter(.data$dir == .data$type)


    # If more than one in a day keep best performer
    if(nrow(s) > 0) {
      s <- s %>%
        dplyr::group_by(.data$date, .data$dir) %>%
        dplyr::arrange(dplyr::desc(.data$r.squared),
                       dplyr::desc(.data$adj.r.squared),
                       .data$statistic, .data$p.value) %>%
        dplyr::slice(1) %>%
        dplyr::select(.data$date, .data$time, .data$dir,
                      .data$n_range, .data$n, .data$dur) %>%
        dplyr::ungroup()
    }

    # Expect to start or end in dark/light (but not necessarily?)
    # Expect to vary by at least X levels of light

    # If light activity before sunrise or after sunset, not a true sunrise/set detection
    if(filter_problems && nrow(s) > 0) s <- sun_filter(data, s)
  } else s <- dplyr::select(s, .data$date, .data$time, .data$dir,
                            .data$n_range, .data$n)
  s
}

sun_filter <- function(data, times) {

  i <- stats::median(as.numeric(difftime(dplyr::lead(data$time),
                                         data$time, units = "min")),
                     na.rm = TRUE)

  sun_n <- times$n[1]

  data <- times %>%
    dplyr::mutate(id = .data$time) %>%
    dplyr::select(.data$time, .data$dir, .data$id) %>%
    tidyr::spread(.data$dir, .data$id) %>%
    dplyr::right_join(data, by = "time")

  if(!"sunrise" %in% names(data)) data$sunrise <- lubridate::ymd_hms(NA, tz = lubridate::tz(data$time))
  if(!"sunset" %in% names(data)) data$sunset <-  lubridate::ymd_hms(NA, tz = lubridate::tz(data$time))


  # Check for problematic sunrise/sets
  # Look for activity before/after rise/set
  sr <- which(!is.na(data$sunrise))
  ss <- which(!is.na(data$sunset))

  # Mark the two hours before/after sunrise/set
  for(n in 1:(120/i))  {
    data$sunrise[sr-n] <- data$sunrise[sr]
    data$sunset[ss+n] <- data$sunset[ss]
  }

  # Look for activity in these two hours
  problems <- data %>%
    tidyr::gather("dir", "id", .data$sunrise, .data$sunset) %>%
    dplyr::filter(!is.na(.data$id)) %>%
    dplyr::group_by(.data$dir, .data$id) %>%
    dplyr::arrange(.data$time) %>%
    dplyr::mutate(n = 1:length(.data$time)) %>%
    dplyr::filter(!.data$n %in% 1:sun_n) %>%
    dplyr::summarize(n_light = sum(.data$light > 0),
                     max_light = max(.data$light)) %>%
    # No more than 20 min of light events, no more than a max of 32 light
    dplyr::filter(.data$n_light > (20/i) | .data$max_light > 32)

  dplyr::filter(times, !.data$time %in% problems$id)
}


sun_assume <- function(dates, loc) {
  # Function to create sunrise/sunset times for assuming nighttime
  # Or should the user ALWAYS calculate sunset/sunrise?
}


#' Get sunrise/sunset times
#'
#' Calculate times of sunrise and sunset depending on the location and the date.
#'
#' @param loc Vector/Data frame. Longitude and Latitude coordinates for location
#'   of sun rise/set
#' @param date Vector. Date(s) to calculate sun rise/set for.
#' @param tz Timezone of the dates.
#' @param angle Numeric. For type = "riseset" the angle of the sun (6 = civil
#'   twilight)
#' @param interval Character. How to organize the sunrise/sunset times
#' @param type Character. What kind of sunrise/sunset to calculate
#'
sun_times <- function(loc, date, tz, angle = 6,
                      interval = "none", type = "riseset") {
  if(class(loc) == "numeric") loc <- matrix(loc, nrow = 1)
  if(class(loc) %in% c("data.frame", "matrix")) loc <- as.matrix(loc)

  date1 <- as.POSIXct(as.character(date), tz = tz)
  if(interval == "night") date2 <- date1 - lubridate::days(1) else date2 <- date1

  if(type == "riseset") {
    sunrise <- maptools::sunriset(loc, date1,
                                  direction = "sunrise",
                                  POSIXct.out = TRUE)$time
    sunset <- maptools::sunriset(loc, date2,
                                 direction = "sunset",
                                 POSIXct.out = TRUE)$time
  } else if (type == "dawndusk") {
    sunrise <- maptools::crepuscule(loc, date1,
                                    direction = "dawn",
                                    solarDep = angle, POSIXct.out = TRUE)$time
    sunset <- maptools::crepuscule(loc, date2,
                                   direction = "dusk",
                                   solarDep = angle, POSIXct.out = TRUE)$time
  }

  if(interval == "none") s <- data.frame(sunrise = sunrise, sunset = sunset)
  if(interval == "day") s <- lubridate::interval(sunrise, sunset)
  if(interval == "night") s <- lubridate::interval(sunset, sunrise)
  s
}

sun_local <- function(loc, date, tz, type = "dawndusk", angle = 12) {
  temp <- sun_times(loc = c(loc[1], loc[2]), date = date,
                    tz = tz, type = type, angle = angle) %>%
    dplyr::mutate(
      date = date,
      sunrise = replace(.data$sunrise, is.na(.data$sunrise),
                        lubridate::force_tz(
                          lubridate::as_datetime(.data$date[is.na(.data$sunrise)]), tz)),
      sunset = replace(.data$sunset,
                       is.na(.data$sunset),
                       lubridate::force_tz(
                         lubridate::as_datetime(.data$date[is.na(.data$sunset)]), tz) +
                         lubridate::hours(23) + lubridate::minutes(59) +
                         lubridate::seconds(59)))
}
