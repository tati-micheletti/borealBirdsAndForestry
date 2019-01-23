## Unit tests for defineStudyArea
library(testthat)
source(file.path(getwd(), "inputs", "defineStudyArea.R"))

test_that(desc = "function defineStudyArea does what it is supposed to", code = {

  # testArea = NULL / expect rP = NULL
  expect_message(rp <- defineStudyArea(testArea = NULL, specificTestArea = NULL, mapSubset = NULL))
  expect_null(rp)
  rm(rp)
  
  # testArea = FALSE / expect rP = NULL
  expect_message(rp <- defineStudyArea(testArea = FALSE, specificTestArea = NULL, mapSubset = NULL))
  expect_null(rp)
  rm(rp)
  
  # testArea = NULL + specificTestArea = "something" / expect rP = NULL + warning
  expect_warning(rp <- defineStudyArea(testArea = NULL, specificTestArea = "Something", mapSubset = NULL))
  expect_null(rp)
  rm(rp)
  
  # testArea = FALSE + specificTestArea = "something" / expect rP = NULL + warning
  expect_warning(rp <- defineStudyArea(testArea = FALSE, specificTestArea = "Something", mapSubset = NULL))
  expect_null(rp)
  rm(rp)
  
  # testArea = TRUE + specificTestArea = NULL / expect rP = randomPolygonInSouthOntario
  expect_message(rp <- defineStudyArea(testArea = TRUE, specificTestArea = NULL, mapSubset = NULL))
  expect_is(rp, class = "SpatialPolygons")
  rm(rp)
  
  # testArea = TRUE + specificTestArea = "Alberta" / expect rP = Alberta (NOT cropped to boreal)
  expect_message(rp <- defineStudyArea(testArea = TRUE, specificTestArea = "Alberta", mapSubset = NULL))
  expect_is(rp, class = "SpatialPolygons")
  rm(rp)
  
  # testArea = TRUE + specificTestArea = "boreal" / expect rP = boreal extension (USA + Canada)
  expect_message(rp <- defineStudyArea(testArea = TRUE, specificTestArea = "boreal", mapSubset = NULL))
  expect_is(rp, class = "SpatialPolygons")
  rm(rp)
  
  # testArea = TRUE + specificTestArea = "boreal" + mapSubset = "Alberta" / expect rP = Alberta inside boreal extension
  expect_message(rp <- defineStudyArea(testArea = TRUE, specificTestArea = "boreal", mapSubset = "Alberta"))
  expect_is(rp, class = "SpatialPolygons")
  rm(rp)
  
  # testArea = TRUE + specificTestArea = "boreal" + mapSubset = "Canada" / expect rP = Canadian boreal
  expect_message(rp <- defineStudyArea(testArea = TRUE, specificTestArea = "boreal", mapSubset = "Canada"))
  expect_is(rp, class = "SpatialPolygons")
  rm(rp)
  
  # testArea = TRUE + specificTestArea = "Brazil" / expect error
  expect_error(rp <- defineStudyArea(testArea = TRUE, specificTestArea = "Brazil", mapSubset = NULL))
  expect_error(rp)

  # testArea = TRUE + specificTestArea = "boreal" + mapSubset = "Brazil" / expect error
  expect_error(rp <- defineStudyArea(testArea = TRUE, specificTestArea = "boreal", mapSubset = "Brazil"))
  expect_error(rp)
})


