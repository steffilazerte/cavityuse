context("utils")

test_that("check_tz is accurate", {
  expect_silent(expect_equal(check_tz("America/Vancouver"), "America/Vancouver"))
  expect_silent(expect_equal(check_tz("America/Toronto"), "America/Toronto"))

  expect_message(expect_equal(check_tz("america/vancouver"), "America/Vancouver"))
  expect_message(expect_equal(check_tz("america/toronto"), "America/Toronto"))

  expect_error(check_tz("adlkfj/vancouver"))
  expect_error(check_tz("america/tortonto"))
})

test_that("check_dst is correct", {
  expect_true(is_dst("America/Vancouver"))
  expect_true(is_dst("America/Toronto"))
  expect_true(is_dst("Europe/Madrid"))

  # No Dst
  expect_false(is_dst("America/Regina"))
  expect_false(is_dst("Africa/Cairo"))
  expect_false(is_dst("Africa/Timbuktu"))
  expect_false(is_dst("Etc/GMT-2"))
  expect_false(is_dst("Etc/GMT+10"))
})

test_that("check_time", {
  expect_error(check_time(as.POSIXct("2015-01-01", tz = "Etc/GMT+5")))
  expect_error(check_time(as.POSIXct("2015-01-01", tz = "America/Vancouver")))
  expect_silent(check_time(as.POSIXct("2015-01-01", tz = "UTC")))
  expect_error(check_time("2015-01-01 00:00:00"))
})


test_that("tz_find_offset", {

})


test_that("n_lag", {


})


test_that("check_data", {

})


test_that("check_cols", {

})


test_that("check_date", {

})


test_that("check_loc", {

})
