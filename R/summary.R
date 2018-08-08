nights <- function(cavity) {
  cavity %>%
    dplyr::mutate(date_end = lubridate::as_date(.data$end)) %>%
    dplyr::filter(.data$date != .data$date_end) %>%
    dplyr::group_by(.data$location) %>%
    dplyr::summarize(n = dplyr::n(),
                     mean_length = mean(.data$length_hrs),
                     sd_length = stats::sd(.data$length_hrs),
                     min_length = min(.data$length_hrs),
                     max_length = max(.data$length_hrs))
}

days <- function(cavity) {
  cavity %>%
    dplyr::mutate(date_end = lubridate::as_date(.data$end)) %>%
    dplyr::filter(.data$date == .data$date_end) %>%
    dplyr::group_by(.data$location) %>%
    dplyr::summarize(n = dplyr::n(),
                     mean_length = mean(.data$length_hrs),
                     sd_length = stats::sd(.data$length_hrs),
                     min_length = min(.data$length_hrs),
                     max_length = max(.data$length_hrs))
}

#' Split cavity use data by different time frames
#'
#' Split cavity use bouts at midnight, midday, or at sunrise/sunset. The output
#' from cavity_detect is automatically split at midnight.
#'
#' @param cavity Data frame. Bouts of cavity use, output from
#'   \code{\link{cavity_detect}}.
#' @param split Character. One of "midnight" (default), "midday", or "riseset"
#' @param sun Data frame. Detected sunrise and sunset events, output of
#'   \code{\link{sun_detect}}. Required when \code{split = "riseset"}.
#' @param loc Numeric vector. Longitude and Latitude of the observations (if not
#'   in the data, this must be provided when \code{split = "riseset"}).
#'
#' @details Depending on your area of interest, when summarizing cavity use data, you'll want to collapse cavity use according to different time frames.
#'
#' \itemize{
#'   \item \code{split = "riseset"} Split at sunrise and sunset to separate cavity use
#'   during the day from that at night
#'   \item \code{split = "midnight"} Split at midnight to separate cavity use between
#'   date, focusing on daytime patterns
#'   \item \code{split = "midday"} Split at noon to separate cavity use between days,
#'   focusing on nighttime patterns
#' }
#'
#' @return cavity data frame with bouts of cavity use split according to the
#'   specification.
#' @export
#'
#' @examples
#'
#' s <- sun_detect(flicker)
#' e <- cavity_detect(flicker, sun = s)
#' loc <- c(flicker$lon[1], flicker$lat[1])
#'
#' e_day <- cavity_split(e, loc, split = "riseset", sun = s)
#'
#' library(dplyr)
#' library(tidyr)
#' library(ggplot2)
#' e_summary <- e_day %>%
#'   group_by(side, location) %>%
#'   summarize(hrs = sum(length_hrs)) %>%
#'   group_by(side) %>%
#'   mutate(p = hrs/sum(hrs) * 100) %>%
#'   complete(location = unique(e$location), fill = list(p = 0))
#'
#'  ggplot(data = e_summary, aes(x = side, y = p, fill = location)) +
#'    geom_bar(stat = "identity") +
#'    labs(x = "Time of day", y = "% time spent")
#'
cavity_split <- function(cavity, split = c("midnight", "midday", "riseset"),
                         sun = NULL, loc = NULL) {

  arg_match(split)
  split <- split[1]

  if(split == "riseset") {
    loc <- check_loc(cavity, loc)
    if(is.null(sun)) {
      stop("To split cavity bouts by sunrise/sunset need 'sun' data frame",
           call. = FALSE)
    }
    for(s in c("sunrise", "sunset")) {
      cavity <- cavity_split_one(cavity, s, sun, loc)
      cavity[[paste0("side_", s)]] <- cavity$side
    }
    cavity <- dplyr::mutate(cavity,
                            side = dplyr::if_else(.data$side_sunrise ==
                                                    .data$side_sunset,
                                                  "night", "day")) %>%
      dplyr::select(-.data$side_sunrise, -.data$side_sunset)
  } else {
    cavity <- cavity_split_one(cavity, split)
    if(split == "midday") {
      cavity <- dplyr::mutate(cavity, side =
                                dplyr::if_else(.data$side == "before",
                                               .data$date - lubridate::days(1),
                                               .data$date))
    }
    cavity <- dplyr::select(cavity, -.data$side)
  }
  cavity
}

cavity_split_one <- function(cavity, split, sun = NULL, loc = NULL) {
  cavity <- split_calc(cavity, split, sun, loc)
  i <- 0
  while(any(cavity$to_split)) {
    i <- i + 1
    #message("Round ", i)
    s_before <- dplyr::filter(cavity, .data$to_split) %>%
      dplyr::mutate(end = .data$splt - lubridate::seconds(1))

    s_after <- dplyr::filter(cavity, .data$to_split) %>%
      dplyr::mutate(start = .data$splt)

    s <- dplyr::bind_rows(s_before, s_after)

    cavity <- dplyr::filter(cavity, !.data$to_split) %>%
      dplyr::bind_rows(s) %>%
      dplyr::select(-.data$splt) %>%
      dplyr::mutate(date = lubridate::as_date(.data$start))
    cavity <- split_calc(cavity, split, sun, loc)
  }

  dplyr::arrange(cavity, .data$start) %>%
    dplyr::mutate(length_hrs = as.numeric(difftime(.data$end, .data$start,
                                                   units = "hours"))) %>%
    dplyr::select(-.data$splt, -.data$to_split)

}

cavity_combine <- function(cavity) {

}

split_calc <- function(cavity, split, sun = NULL, loc = NULL) {
  if(split == "midnight") {
    cavity <- dplyr::mutate(cavity,
                            splt = lubridate::ymd_hms(
                              paste0(.data$date + lubridate::days(1),
                                     "00:00:00"),
                              tz = lubridate::tz(.data$start)))
  } else if(split == "midday") {
    cavity <- dplyr::mutate(cavity,
                            splt = lubridate::ymd_hms(
                              paste0(.data$date, "12:00:00"),
                              tz = lubridate::tz(.data$start)))
  } else if(split %in% c("sunrise", "sunset")) {
    # Get rise/set from data if exists
    if(!is.null(sun)) {
      cavity <- dplyr::filter(sun, .data$dir == split) %>%
        dplyr::select(.data$date, splt = .data$time) %>%
        dplyr::right_join(cavity, by = "date")
    } else cavity$splt <- NA

    # Get missing sunrise from sunrise functions
    if(!is.null(loc)) {
      cavity_add <- dplyr::filter(cavity, is.na(.data$splt)) %>%
        dplyr::select(-.data$splt)
      cavity_keep <- dplyr::filter(cavity, !is.na(.data$splt))

      if(nrow(cavity_add) > 0) {
        cavity <- sun_times(loc = loc, date = unique(cavity_add$date),
                            type = "dawndusk", angle = 6,
                            tz = lubridate::tz(cavity_add$start))
        names(cavity)[names(cavity) == split] <- "splt"
      }

      cavity <- cavity %>%
        dplyr::mutate(date = lubridate::as_date(.data$splt)) %>%
        dplyr::select(.data$date, .data$splt) %>%
        dplyr::right_join(cavity_add, by = "date") %>%
        dplyr::bind_rows(cavity_keep) %>%
        dplyr::arrange(.data$start)
    }
  }

  dplyr::mutate(cavity,
                int = lubridate::interval(.data$start + lubridate::seconds(1),
                                          .data$end - lubridate::seconds(1)),
                to_split = lubridate::`%within%`(.data$splt, .data$int),
                side = dplyr::if_else(.data$end <= .data$splt,
                                      "before", "after")) %>%
    dplyr::select(-.data$int)
}

