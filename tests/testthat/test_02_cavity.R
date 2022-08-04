context("cavity_detect")


test_that("cavity_detects works as expected", {
  s <- sun_detect(flicker)
  expect_silent(e <- cavity_detect(flicker, sun = s))

  expect_is(e, "data.frame")
  expect_gt(nrow(e), 10)
  expect_named(e, c("date", "start", "end", "length_hrs", "location",
                    "offset_applied", "lon", "lat",
                    "thresh_dark", "thresh_light", "ambig_dark", "ambig_light"))

  expect_equal(unique(e$date), unique(flicker$date))

  # Need to make comparable by applying offset
  orig_time <- tz_apply_offset(flicker, tz_offset = -8)$time

  expect_true(all(format(e$start[!e$start %in% orig_time], "%H:%M:%S") == "00:00:00"))
  expect_true(all(format(e$end[!e$end %in% orig_time], "%H:%M:%S") == "23:59:59"))

  expect_true(all(e$end >= e$start, na.rm = TRUE))
  expect_true(all(e$end < dplyr::lead(e$start), na.rm = TRUE))

  # check values
  expect_equal(e$start[1:2],
               as.POSIXct(c("2011-06-17 00:00:50", "2011-06-17 03:54:50"),
                          tz = "UTC"))

  expect_equal(e$end[1:2],
               as.POSIXct(c(e$start[2] - lubridate::minutes(2),
                            e$start[3] - lubridate::minutes(2)),
                          tz = "UTC"))

  expect_equal(e$length_hrs[1:3], c(3.86666666, 0, 0.033333333))
  expect_equal(e$location[1:3], c("in", "in_ambig", "ambig"))

  # check patterns
  n <- cavity_split(e, split = "riseset", sun = s,
                    loc = c(flicker$lon[1], flicker$lat[1]))

  p <- n %>%
    dplyr::group_by(side, location) %>%
    dplyr::summarize(l = sum(length_hrs)) %>%
    tidyr::complete(location = unique(n$location), fill = list(l = 0)) %>%
    tidyr::spread(location, l)

  expect_true(p$`in`[p$side == "night"] > p$out[p$side == "night"])
  expect_true(p$out[p$side == "day"] > p$out[p$side == "night"])

})


test_that("cavity_detect fails gracefully on wrong format", {
  expect_error(cavity_detect(data.frame(time = LETTERS,
                                        light = sample(0:64, 26,
                                                       replace = TRUE))),
               "time column is not in time class")
  s <- sun_detect(wtsp)

  expect_error(cavity_detect(dplyr::mutate(wtsp, light = as.character(light)),
                             sun = s),
               "is formated as character, but should be numeric")

  expect_error(cavity_detect(dplyr::select(wtsp, -lat, -lon), sun = s),
               "lon/lat need to be in data frame or supplied as vector")

  expect_silent(cavity_detect(dplyr::select(wtsp[1:50,], -lat, -lon), sun = s,
                              loc = c(wtsp$lon[1], wtsp$lat[1])))
})


test_that("cavity_split works as expected", {
  s <- sun_detect(flicker)
  e <- cavity_detect(flicker, s)
  expect_silent(cavity_split(e, split = "midnight"))
  expect_silent(cavity_split(e, split = "midday"))
  expect_silent(cavity_split(e, split = "riseset",
                             sun = s, loc = c(flicker$lon[1], flicker$lat[1])))
})
