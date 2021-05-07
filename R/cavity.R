
#' Summarize light into bouts of cavity use
#'
#' @param data Data frame. Raw light data. Requires two columns: "time" and
#'   "light"
#' @param sun Data frame. Detected sunrise and sunset events, output of
#'   \code{\link{sun_detect}}
#' @param loc Numeric vector. Longitude and Latitude of the observations (if not
#'   in the data, this must be provided).
#' @param n Numeric. Number of points before and after a given observation to
#'   use in the calculation of cavity use (similar to a running mean)
#' @param thresh_dark  Numeric. Light threshold for "in" (see details)
#' @param thresh_light Numeric. Light threshold for "out" (see details)
#' @param ambig_dark Numeric. Light threshold for "in_ambig" (see details)
#' @param ambig_light Numeric. Light threshold for "out_ambig" (see details)
#' @param gap_cutoff Numeric. Minimum number of sequential missing observations
#'   defining a "gap" in the data record
#'
#' @details \strong{Thresholds}
#'   This function assigns points to four categories based on their light
#'   intensity: "in", "out", "in_ambig", "out_ambig", "ambig". The thresholds
#'   are used to determine which category a point is assigned to.
#'
#'   \itemize{
#'    \item \strong{in:} Light <= thresh_dark
#'    \item \strong{out:} Light >= thresh_light
#'    \item \strong{in_ambig:} thresh_dark < Light <= ambig_dark
#'    \item \strong{out_ambig:} thresh_light > Light >= ambig_light
#'    \item \strong{ambig:} ambig_dark < Light < ambig_light
#'    }
#'
#'  These points are then smoothed into bouts of cavity use. The argument
#'  \code{n}, determines how many points before and after a given point, should
#'  be used to influence the final cavity use designation. For example, an
#'  "ambig" point, surrounded by "in" points will be assigned as part of an "in"
#'  cavity use bout. However, 3 "ambig" points in a row will result in an
#'  "ambig" bout. How many points to smooth over is affected by \code{n}. The
#'  argument \code{gap_cutoff} determines the maximum number of minutes between
#'  observations allowed before a bout is split and a gap in the data is
#'  introduced.
#'
#'  To detect cavity use in the evening and overnight, this function relies on
#'  detections of sunrise and sunset (detected with \code{\link{sun_detect}}).
#'  If you really don't want to use this, assign an empty data frame, but be
#'  aware that your overnight locations will always be defined as "in", which,
#'  in many cases would be incorrect.
#'  \code{sun = data.frame()}
#'
#' @return A data frame summarizing light into bouts of cavity use, each with a
#'   start, end, and location designation.
#'
#' @export
#'
#' @examples
#'
#' # Single individual
#' s <- sun_detect(wtsp)
#' e <- cavity_detect(wtsp, sun = s)
#'
#' cavity_plot(wtsp, cavity = e, sun = s)
#'
#' # Use map from purrr package for multiple individuals
#' library(dplyr)
#' library(tidyr)
#' library(purrr)
#'
#' d <- flicker_mult %>%
#'   nest(-id, .key = "light_data") %>%
#'   mutate(s = map(light_data, ~sun_detect(.)),
#'          e = map2(light_data, s, ~cavity_detect(.x, sun = .y)))
#' d
#'
#' cavity_plot(d$light_data[[1]], cavity = d$e[[1]], sun = d$s[[1]])
#' cavity_plot(d$light_data[[2]], cavity = d$e[[2]], sun = d$s[[2]])
#'
#'
cavity_detect <- function(data, sun = NULL, loc = NULL, n = 2,
                          thresh_dark = 1, thresh_light = 60,
                          ambig_dark = 10, ambig_light = 25,
                          gap_cutoff = 10) {

  # Input Checks
  check_data(data)
  check_cols(data, c("time", "light"))
  check_class(data$light, "numeric")

  check_time(data$time)

  loc <- check_loc(data, loc)
  tz_offset <- tz_offset(loc[1], loc[2])
  data <- tz_apply_offset(data, tz_offset)
  data <- check_date(data)

  if(!is.null(sun)) {
    check_data(sun)
    check_cols(sun, c("time", "dir", "n_range", "n", "dur"))
    check_time(sun$time)
    sun <- check_date(sun)
  } else {
    sun <- dplyr::select(data, date, time, light) %>%
      dplyr::filter(is.na(light)) %>%
      dplyr::mutate(dir = as.character(NA), n_range = as.numeric(NA),
                    n = as.integer(NA), dur = as.numeric(NA))
  }

  # Ungoup
  if(dplyr::is.grouped_df(data)) data <- dplyr::ungroup(data)
  if(dplyr::is.grouped_df(sun)) sun <- dplyr::ungroup(sun)


  data <- points_sun_times(data, sun = sun,
                           thresh_dark, thresh_light,
                           ambig_dark, ambig_light)

  cavity <- cavity_assign(data, n)
  cavity <- cavity_simplify(cavity, gap_cutoff = gap_cutoff)
  cavity <- cavity_spread(cavity)
  cavity <- cavity_fix(cavity, loc = loc, gap_cutoff = gap_cutoff)
  cavity <- cavity_finalize(cavity)

  cavity <- cavity_split(cavity)

  # Add run details
  dplyr::mutate(cavity,
                lon = loc[['lon']], lat = loc[['lat']],
                thresh_dark = thresh_dark,
                thresh_light = thresh_light,
                ambig_dark = ambig_dark,
                ambig_light = ambig_light)
}


points_sun_times <- function(data, sun,
                            thresh_dark,thresh_light,
                            ambig_dark, ambig_light) {

  if(nrow(sun) == 0) {
    data <- data %>%
      dplyr::mutate(point_type =
                      dplyr::case_when(.data$light >= thresh_light ~ "out",
                                       .data$light <= thresh_dark ~ "in",
                                       .data$light >= ambig_light ~ "out_ambig",
                                       .data$light <= ambig_dark ~ "in_ambig",
                                       TRUE ~ "ambig"))
  } else {

    i <- stats::median(as.numeric(difftime(dplyr::lead(data$time),
                                           data$time, units = "min")),
                       na.rm = TRUE)

    n_sun <- sun$n[1]
    data <- sun %>%
      dplyr::select(-.data$n, -.data$n_range) %>%
      tidyr::complete(dir = c("sunrise", "sunset"),
                      fill = list(date = NA, time = NA, dur = NA)) %>%
      tidyr::spread(.data$dir, .data$time) %>%
      dplyr::right_join(data, by = "date") %>%
      # Note which cavity bouts are during a detected sunrise/sunset
      dplyr::mutate(sunrise_end = .data$sunrise + (i * lubridate::minutes(n_sun)),
                    sunset_end = .data$sunset + (i * lubridate::minutes(n_sun)),
                    point_type = dplyr::case_when(
                      .data$time >= .data$sunrise &
                        .data$time <= .data$sunrise_end ~ "sunrise",
                      .data$time >= .data$sunset &
                        .data$time <= .data$sunset_end ~ "sunset",
                      .data$time <= .data$sunrise |
                        .data$time >= .data$sunset ~ "night",
                      .data$light >= thresh_light ~ "out",
                      .data$light <= thresh_dark ~ "in",
                      .data$light >= ambig_light ~ "out_ambig",
                      .data$light <= ambig_dark ~ "in_ambig",
                      TRUE ~ "ambig"))
  }
}

cavity_assign <- function(data, n) {

  # Create bouts of in/out cavity use based on how many points to skip over.
  # Start with dark then light, parts of bouts which overlap are ambiguous

  if(n > 0) {

    # Get consensus on bout types
    cavity <- data %>%
      dplyr::mutate(forward = cavity_sort(n_lag(.data$point_type, n,
                                                dir = "forward")),
                    backward = cavity_sort(n_lag(.data$point_type, n,
                                                 dir = "backward")))

    # If ambig near sunset/sunrise, become sunset/sunrise marker,
    # unless there are non-ambiguous points between

    cavity <- cavity %>%
      dplyr::mutate(location =
                      dplyr::case_when(
                        stringr::str_detect(.data$point_type, "ambig") &
                          (.data$forward == "sunset" |
                             .data$backward == "sunset") ~
                          "sunset",
                        stringr::str_detect(.data$point_type, "ambig") &
                          (.data$forward == "sunrise" |
                             .data$backward == "sunrise") ~
                          "sunrise",
                        # Never skip over in or out,
                        # but skip over ambig or out/in_ambig,
                        # depending on n and type of bout
                        .data$point_type == "ambig" &
                          .data$forward == .data$backward ~
                          .data$forward,
                        .data$point_type == "in_ambig" &
                          .data$forward == .data$backward &
                          .data$forward == "in" ~ "in",
                        .data$point_type == "out_ambig" &
                          .data$forward == .data$backward &
                          .data$forward == "out" ~ "out",
                        TRUE ~ .data$point_type))
  } else {
    # Don't take into account leading or lagging point types
    cavity <- data %>%
      dplyr::mutate(location = .data$point_type)
  }

  cavity
}


cavity_sort <- function(p) {
  temp <- p %>%
    data.frame(rise = rowSums(. == "sunrise", na.rm = TRUE),
               set = rowSums(. == "sunset", na.rm = TRUE),
               din = rowSums(. == "in" | . == "in_ambig", na.rm = TRUE),
               dout = rowSums(. == "out" | . == "out_ambig", na.rm = TRUE),
               ambig = rowSums(. == "ambig", na.rm = TRUE)) %>%
    dplyr::mutate(location = dplyr::case_when(
      .data$din > 0 & .data$dout == 0 & .data$din > .data$ambig ~ "in",
      .data$din == 0 & .data$dout > 0 & .data$dout > .data$ambig ~ "out",
      .data$rise > 0 ~ "sunrise",
      .data$set > 0 ~ "sunset",
      TRUE ~ "ambig")) %>%
    dplyr::pull(.data$location)
}

cavity_simplify <- function(cavity, gap_cutoff) {
  # Skip over single point cavity bouts
  # If one ambiguous or sunrise/sunset point is surrounded by the same on either side, make it same as on either side

  cavity <- dplyr::mutate(cavity,
                         location = dplyr::if_else(
                           dplyr::lead(.data$location, default = "") ==
                             dplyr::lag(.data$location, default = "") &
                             .data$location == "ambig",
                           dplyr::lead(.data$location), .data$location))

  cavity <- dplyr::mutate(cavity,
                          int = as.numeric(difftime(dplyr::lead(.data$time),
                                                    .data$time, units = "min")))
  # Get overall bout types and times
  cavity <- dplyr::mutate(cavity, type = dplyr::case_when(
    # Single point of a given type
    .data$location != dplyr::lag(.data$location, default = "") &
      .data$location != dplyr::lead(.data$location, default = "") ~ "start/end",
    # Change in previous point type and close to the next
    .data$location != dplyr::lag(.data$location, default = "") &
      .data$int <= gap_cutoff ~ "start",
    # Change in next point type and close to previous
    .data$location != dplyr::lead(.data$location, default = "") &
      dplyr::lag(.data$int) <= gap_cutoff ~ "end",
    # Change in previous point type and too far from next
    .data$location != dplyr::lag(.data$location, default = "") &
      .data$int > gap_cutoff ~ "start/end",
    # Change in next point type and too far from previous
    .data$location != dplyr::lead(.data$location, default = "") &
      dplyr::lag(.data$int) > gap_cutoff ~ "start/end",
    # Isolated point
    dplyr::lag(.data$int) > gap_cutoff & .data$int > gap_cutoff ~ "start/end",
    # Next point too far
    .data$int > gap_cutoff ~ "end",
    # Previous point too far
    dplyr::lag(.data$int) > gap_cutoff ~ "start")
    )

  cavity <- dplyr::select(cavity,
                          -.data$forward, -.data$backward)

  cavity
}

cavity_spread <- function(cavity) {

  # Get numbers for counting points within a bout
  # ....

  # Filter and clean up
  cavity <- cavity %>%
    dplyr::filter(!is.na(.data$type)) %>%
    dplyr::select(.data$time, .data$location, .data$type)

  # Add in extra obs for start/end
  s <- dplyr::filter(cavity, .data$type == "start/end")
  if(nrow(s) > 0) {
    s <- dplyr::bind_rows(s, s) %>%
      dplyr::arrange(.data$time) %>%
      dplyr::mutate(type = rep(c("start", "end"), nrow(.)/2))

    cavity <- dplyr::filter(cavity, .data$type != "start/end") %>%
      dplyr::bind_rows(s) %>%
      dplyr::arrange(.data$time)
  }

  if(length(cavity$type[cavity$type == "start"]) != length(cavity$type[cavity$type == "end"])) {
    stop("Un-equal start and end times...", call. = FALSE)
  } else if(length(cavity$type[cavity$type == "start/end"]) > 0) {
    stop("Start/End times not dealt with...", call. = FALSE)
  } else if(any(cavity$type == dplyr::lead(cavity$type), na.rm = TRUE)) {
    stop("Start and end times not aligned...", call. = FALSE)
  } else if(cavity$type[1] != "start" | cavity$type[nrow(cavity)] != "end") {
    stop("Doesn't start with start or doesn't end with end...", call. = FALSE)
  }

  cavity <- dplyr::arrange(cavity, .data$type, .data$time) %>%
    dplyr::group_by(.data$type) %>%
    dplyr::mutate(index = seq_along(.data$time)) %>%
    tidyr::spread(.data$type, .data$time) %>%
    dplyr::select(.data$location, .data$start, .data$end) %>%
    dplyr::mutate(date = lubridate::as_date(.data$start)) %>%
    dplyr::arrange(.data$start)

  cavity
}

# Fix nighttime cavity bouts after midnight
cavity_fix <- function(cavity, loc, gap_cutoff) {
  sun_local <- sun_local(loc = loc,
                         date = unique(lubridate::as_date(cavity$start)),
                         angle = 6)

  # If
  # - A location is "in"
  # - and starts before sunrise (by default after midnight)
  # - AND occurs right after a "night" location
  # change to "ambig"
  # change previous location from "night" to "ambig"?
  # link to previous location (before midnight)
  #
  # - A location is "in"
  # - and ends after sunset
  # - and occurs right before a "night" location
  # chagne to "ambig"
  # change next to "ambig"


  cavity %>%
    dplyr::arrange(.data$start) %>%
    dplyr::left_join(sun_local, by = "date") %>%
    dplyr::mutate(
      end_next = dplyr::lead(.data$end),
      location_next = dplyr::lead(.data$location),
      location_prev = dplyr::lag(.data$location),
      int = as.numeric(difftime(dplyr::lead(.data$start),
                                .data$end, units = "min")),
      ambig_pm = .data$location == "in" & .data$end > .data$sunset &
        .data$location_next == "night" & .data$int < gap_cutoff,
      ambig_am = .data$location == "in" & .data$start < .data$sunrise &
        .data$location_prev == "night" & dplyr::lag(.data$int) < gap_cutoff,
      ambig_pm = dplyr::lead(.data$ambig_am, default = FALSE) | .data$ambig_pm,
      ambig_am = dplyr::lag(.data$ambig_pm, default = FALSE) | .data$ambig_am,
      ambig_pm = replace(.data$ambig_pm, is.na(.data$ambig_pm), FALSE),
      ambig_am = replace(.data$ambig_am, is.na(.data$ambig_am), FALSE),
      location = replace(.data$location, .data$ambig_pm, "ambig"),
      end = replace(.data$end, .data$ambig_pm,
                    .data$end_next[.data$ambig_pm])) %>%
    dplyr::filter(!.data$ambig_am) %>%
    dplyr::select(-.data$ambig_am, -.data$ambig_pm, -.data$end_next,
                  -.data$sunrise, -.data$sunset, -.data$location_next,
                  -.data$location_prev)
}

cavity_finalize <- function(cavity) {
  cavity %>%
    dplyr::mutate(location =
                    replace(.data$location,
                            .data$location %in% c("sunset", "sunrise", "night"),
                            "out"),
                  length_hrs = as.numeric(difftime(.data$end, .data$start,
                                                   units = "hours"))) %>%
    dplyr::select(.data$date, .data$start, .data$end,
                  .data$length_hrs, .data$location, .data$offset_applied) %>%
    dplyr::arrange(.data$start)
}
