library(testthat)
library(FarsPackageKarol)

test_check("FarsPackageKarol")

test_that("Make filename test", {
  expect_equal(make_filename(2013), "accident_2013.csv.bz2")
})


test_that("Make filename test error", {
  expect_that(make_filename,throws_error(make_filename(qwerty)))
})


