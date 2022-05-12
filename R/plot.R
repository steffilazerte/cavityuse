#' Plot light data
#'
#' Plot light data patterns, optionally with overlayed detected sunrise/sunset
#' events and bouts of cavity use.
#'
#' @param data  Data frame. Raw light data. Requires two columns: "time" and
#'   "light"
#' @param cavity Data frame. Bouts of cavity use, output from
#'   \code{\link{cavity_detect}}.
#' @param sun Data frame. sun Data frame. Detected sunrise and sunset events,
#'   output of \code{\link{sun_detect}}
#' @param loc Numeric vector. Optional Longitude and Latitude of the
#'   observations (if not in the data, this must be provided if \code{show_night
#'   = TRUE}).
#' @param days Numeric. Number of days to plot
#' @param start Character. Start date in "YYYY-MM-DD" format
#' @param tz_apply_offset Logical. Apply a timezone offset to indicate local
#'   time on the x-axis. Only relevant if plotting light data alone.
#' @param nrow Numeric. For multi-day figures, number of plot rows.
#' @param ncol Numeric. For multi-day figures, number of plot cols.
#' @param clip Logical. For geolocator light data >64 lux, clip the data to a
#'   max of 64? This sometimes makes patterns easier to see.
#' @param show_night Logical. Overlay grey shading to indicate nighttime
#'   (defined by local sunrise/sunset times calculated from coordinates).
#'
#' @return A ggplot2 figure showing light patterns overlayed with detected
#'   sunrise/sunset events and/or bouts of cavity use.
#' @export
#'
#' @examples
#'
#' # Light data only
#' cavity_plot(flicker, days = 1)
#'
#' # Don't offset time to show local (i.e. leave as UTC)
#' cavity_plot(flicker, days = 4, tz_apply_offset = FALSE)
#'
#' # Light data + sunrise/sunset
#' s <- sun_detect(flicker)
#' cavity_plot(flicker, sun = s, days = 1)
#'
#' # Light data + sunrise/sunset + cavity use
#' s <- sun_detect(flicker)
#' e <- cavity_detect(flicker, sun = s)
#' cavity_plot(flicker, cavity = e, sun = s, days = 1)
#'
#' # Multi-day plots
#' cavity_plot(flicker, days = 3)
#' cavity_plot(flicker, days = 5, nrow = 1)
#' cavity_plot(flicker, days = 5, ncol = 1)
#'
#' # With clipping
#' cavity_plot(wtsp, days = 1)
#'
#' # Without clipping
#' cavity_plot(wtsp, days = 1, clip = FALSE)
#'
#' # Remove nights
#' cavity_plot(wtsp, days = 1, show_night = FALSE)
#'
#'

cavity_plot <- function(data, cavity = NULL, sun = NULL, loc = NULL,
                        days = 10, start = NULL, tz_apply_offset = TRUE,
                        nrow = NULL, ncol = NULL, clip = TRUE,
                        show_night = TRUE) {

  check_data(data)
  check_cols(data, c("time", "light"))
  check_class(data$light, "numeric")

  check_time(data$time)

  loc <- check_loc(data, loc)

  tz_offset <- tz_find_offset(loc[1], loc[2])
  if(!is.null(cavity) | !is.null(sun) | tz_apply_offset) {
    data <- tz_apply_offset(data, tz_offset)
  }

  if(is.null(start)) {
    start_plot <- lubridate::floor_date(min(data$time), unit = "days")
  } else {
    start_plot <- lubridate::as_date(start)
  }

  if(clip) data$light[data$light > 64] <- 64

  i <- difftime(dplyr::lead(data$time), data$time, units = "sec") %>%
    stats::median(., na.rm = TRUE) %>%
    as.numeric(.)/2

  data <- dplyr::filter(data, .data$time >= start_plot,
                        .data$time < start_plot + lubridate::days(days)) %>%
    dplyr::mutate(date = lubridate::as_date(.data$time))

  if(nrow(data) == 0) {
    message("No data for these dates (",
            start_plot, " - ", start_plot + lubridate::days(days), ")")
    return(invisible())
  }

  if(!is.null(cavity)) {
    cavity <- cavity %>%
      dplyr::filter(.data$start < start_plot + lubridate::days(days),
                    .data$end >= start_plot) %>%
      dplyr::mutate(start = replace(.data$start,
                                    .data$start < start_plot,
                                    start_plot),
                    end = replace(.data$end,
                                  .data$end > start_plot + lubridate::days(days),
                                  start_plot + lubridate::days(days) - 1)) %>%
      cavity_split() %>%
      dplyr::mutate(location = factor(.data$location,
                                      levels = c("in", "in_ambig", "ambig",
                                                 "out_ambig", "out"),
                                      labels = c("In", "In (ambig)", "Ambig",
                                                 "Out (ambig)", "Out")))
  }

  # Compile plot

  g <- ggplot2::ggplot(data) +
    ggplot2::theme_bw() +
    ggplot2::scale_y_reverse()

  if(!is.null(sun)) {
    sun <- dplyr::filter(sun,
                         .data$time >= start_plot,
                         .data$time <= start_plot + lubridate::days(days)) %>%
      dplyr::mutate(time_end = .data$time + lubridate::minutes(.data$dur))
    if(nrow(sun) > 0) {
      g <- g + ggplot2::geom_rect(
        data = sun,
        ggplot2::aes_string(xmin = "time",
                            xmax = "time_end",
                            ymax = 0, ymin = +Inf,
                            alpha = "'Sunrise/Sunset'"),
        fill = "#FFB90F") +
        # Fake legend, so we can have two separate 'fill' scales
        ggplot2::scale_alpha_manual(
          name = "",
          values = 1,
          guide = ggplot2::guide_legend(override.aes = list(fill = "#FFB90F")))
    }
  }

  if(!is.null(cavity)) {
    g <- g +
      ggplot2::geom_rect(data = cavity,
                         ggplot2::aes_string(xmin = "start", xmax = "end",
                                             ymin = 0, ymax = -15,
                                             fill = "location"), alpha = 0.9) +
      ggplot2::scale_fill_viridis_d(drop = FALSE)
  }

  g <- g +
    ggplot2::geom_line(ggplot2::aes_string(x = "time", y = "light")) +
    ggplot2::geom_point(ggplot2::aes_string(x = "time", y = "light")) +
    ggplot2::facet_wrap(~ date, scales = "free_x", nrow = nrow, ncol = ncol) +
    ggplot2::scale_x_datetime(date_labels = "%H:%M", limits = date_limits) +
    ggplot2::labs(x = "Time", y = "Light levels", fill = "Location")

  if(show_night) {
    loc <- check_loc(data, loc)
    sun_t <- sun_times(loc = loc, date = unique(data$date),
                       type = "dawndusk", angle = 6) %>%
      dplyr::mutate(rise_null = lubridate::floor_date(.data$sunrise, "days"),
                    set_null = lubridate::ceiling_date(.data$sunset, "days"),
                    date = lubridate::as_date(.data$rise_null))
    if(!tz_apply_offset) {
      sun_t <- tz_remove_offset(sun_t, cols = c("sunrise", "sunset")) %>%
        mutate(rise_null = dplyr::if_else((lubridate::hour(sunset) + 12) < 24,
                                          sunset - lubridate::days(1),
                                          rise_null),
               sunset = dplyr::if_else(
                 lubridate::as_date(sunset) > lubridate::as_date(sunrise),
                 lubridate::floor_date(sunset, unit = "day"),
                 sunset),
               set_null = dplyr::if_else((lubridate::hour(sunrise) - 12) > 0,
                                         sunrise,
                                         set_null))
    }

    g <- g +
      ggplot2::geom_rect(data = sun_t,
                         ggplot2::aes_string(xmin = "rise_null",
                                             xmax = "sunrise",
                                             ymin = -Inf, ymax = +Inf),
                         alpha = 0.2, inherit.aes = FALSE) +
      ggplot2::geom_rect(data = sun_t,
                         ggplot2::aes_string(xmin = "sunset",
                                             xmax = "set_null",
                                             ymin = -Inf, ymax = +Inf),
                         alpha = 0.2, inherit.aes = FALSE)
  }
  g
}


date_limits <- function(limits) {
  c(lubridate::floor_date(limits[1], unit = "day"),
    lubridate::ceiling_date(limits[2], unit = "day"))
}

