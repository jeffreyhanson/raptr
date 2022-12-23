context("05-simulate-data")

test_that("simulate planning units functions", {
  # generate planning units
  pus <- sim.pus(n = 225L)
  # check simulated dataset
  expect_true(inherits(pus, "sf"))
  expect_equal(nrow(pus), 225L)
  expect_equal(sf::st_bbox(pus)$xmin[[1]], -7.5)
  expect_equal(sf::st_bbox(pus)$ymin[[1]], -7.5)
  expect_equal(sf::st_bbox(pus)$xmax[[1]], 7.5)
  expect_equal(sf::st_bbox(pus)$ymax[[1]], 7.5)
})

test_that("simulate space functions", {
  # generate planning units
  pus <- sim.pus(n = 225L)
  # generate spaces
  spaces <- sim.space(pus, res = 1, d = 5)
  # check simulated dataset
  expect_true(inherits(spaces, "SpatRaster"))
  expect_equal(terra::nlyr(spaces), 5)
  expect_equal(terra::xmin(spaces), -7.5)
  expect_equal(terra::ymin(spaces), -7.5)
  expect_equal(terra::xmax(spaces), 7.5)
  expect_equal(terra::ymax(spaces), 7.5)
  expect_equal(terra::global(is.na(spaces[[1]]), "sum")[[1]], 0)
})

test_that("simulate species functions", {
  # generate planning units
  pus <- sim.pus(n = 225L)
  # generate spaces
  species <- sim.species(pus, res = 1, n = 5)
  # check simulated dataset
  expect_true(inherits(species, "SpatRaster"))
  expect_equal(terra::nlyr(species), 5)
  expect_equal(terra::xmin(species), -7.5)
  expect_equal(terra::ymin(species), -7.5)
  expect_equal(terra::xmax(species), 7.5)
  expect_equal(terra::ymax(species), 7.5)
  expect_equal(terra::global(is.na(species[[1]]), "sum")[[1]], 0)
})
