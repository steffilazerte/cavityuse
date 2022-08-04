# Experimental?
light_thresh <- function(data, loc = NULL, night = c(23, 1), day = c(11, 13),
                         quiet = FALSE) {

  # Input Checks
  check_data(data)
  check_cols(data, c("time", "light"))
  check_class(data$light, "numeric")

  check_time(data$time)

  loc <- check_loc(data, loc)
  tz_offset <- tz_find_offset(loc[1], loc[2])
  data <- tz_apply_offset(data, tz_offset)
  data <- check_date(data)


  range <- data %>%
    dplyr::mutate(hour = lubridate::hour(.data$time)) %>%
    dplyr::mutate(side = dplyr::case_when(
      .data$hour >= .data$day[1] & .data$hour <= .data$day[2] ~ "day",
      .data$hour >= .data$night[1] | .data$hour <= .data$night[2] ~ "night")) %>%
    dplyr::filter(!is.na(.data$side)) %>%
    dplyr::group_by(.data$side) %>%
    dplyr::summarize(min = min(.data$light, na.rm = TRUE),
                     median = stats::median(.data$light, na.rm = TRUE),
                     max = max(.data$light, na.rm = TRUE))

  if(!quiet) {
    message(glue::glue("Point resolution: {res(data$time)} s ({round(res(data$time)/60, 2)} min)"))
    message(glue::glue(
      "Day time range: {range$min[1]} - {range$max[1]}, median {range$median[1]}\n",
      "Night range:  {range$min[2]} - {range$max[2]}, median {range$median[2]}"))

    message(glue::glue(
      "Suggested thresholds\n",
      "  thresh_dark: {range$max[2] + 1}\n",
      "  thresh_light: {floor(range$max[1] * 0.95)}\n",
      "  ambig_dark: {range$max[2] + 10}\n",
      "  ambig_light: {floor(range$max[1] * 0.40)}", .trim = FALSE))
  }

  range$max[2] + 1
}
