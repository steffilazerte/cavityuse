context("sun_detect")

test_that("sun_detect works as expected", {
  expect_silent(s <- sun_detect(calib))
  expect_is(s, "data.frame")
  expect_equal(nrow(s), 6)
  expect_named(s, c("date", "time", "dir", "n_range", "n", "dur", "offset_applied"))

  expect_equal(s$n[1], 10)
  expect_equal(s$dur[1], 20)
  expect_equal(s$n_range, c(24, 32, 63, 62, 32, 62))
  expect_equal(s$time, lubridate::ymd_hms(c("2011-05-07 03:45:57",
                                            "2011-05-07 20:07:57",
                                            "2011-05-08 03:47:57",
                                            "2011-05-08 20:09:57",
                                            "2011-05-09 03:39:56",
                                            "2011-05-09 20:11:56"),
                                          tz = "UTC"))

})


test_that("Fails gracefully on wrong format", {
  expect_error(sun_detect(data.frame(time = LETTERS,
                                     light = sample(0:64, 26, replace = TRUE))),
               "is not in time class. Consider")

  expect_error(sun_detect(dplyr::mutate(wtsp, light = as.character(light))),
               "is formated as character, but should be numeric")

  expect_error(sun_detect(dplyr::select(wtsp, -lat, -lon)),
               "lon/lat need to be in data frame or supplied as vector")
  expect_silent(sun_detect(dplyr::select(wtsp, -lat, -lon),
                           loc = c(wtsp$lon[1], wtsp$lat[1])))
})
