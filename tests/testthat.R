library(testthat)
library(FarsPackageKarol)
library(dplyr)
library(readr)
library(tidyr)
library(magrittr)
library(graphics)
library(maps)

test_check("FarsPackageKarol")

test_that("Make filename test", {
  expect_equal(make_filename(2013), "accident_2013.csv.bz2")
})


test_that("Make filename test", {
  expect_equal(make_filename(2014), "accident_2014.csv.bz2")
})


