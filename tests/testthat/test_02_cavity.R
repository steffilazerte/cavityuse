context("cavity_detect")


test_that("cavity_detects works as expected", {
  s <- sun_detect(flicker)
  expect_silent(e <- cavity_detect(flicker, s))

  expect_is(e, "data.frame")
  expect_equal(nrow(e), 213)
  expect_equal(ncol(e), 9)

  expect_equal(unique(e$date), unique(flicker$date))
  expect_true(all(format(e$start[!e$start %in% flicker$time], "%H:%M:%S") == "00:00:00"))
  expect_true(all(format(e$end[!e$end %in% flicker$time], "%H:%M:%S") == "23:59:59"))

  expect_true(all(e$end >= e$start, na.rm = TRUE))
  expect_true(all(e$end < dplyr::lead(e$start), na.rm = TRUE))

  # check values
  expect_equal(e$start[1:2],
               as.POSIXct(c("2011-06-17 00:00:50", "2011-06-17 03:40:50"),
                          tz = lubridate::tz(flicker$time)))

  expect_equal(e$end[1:2],
               as.POSIXct(c("2011-06-17 03:38:50", "2011-06-17 04:04:50"),
                          tz = lubridate::tz(flicker$time)))

  expect_equal(e$length_hrs[1:2], c(3.6333333333, 0.4))
  expect_equal(e$location[1:2], c("out", "out"))

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
