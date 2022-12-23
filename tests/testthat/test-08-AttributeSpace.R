context("08-AttributeSpace")

test_that("AttributeSpace generator function", {
  # create object
  pu_coords <- suppressWarnings(sf::st_coordinates(sf::st_centroid(cs_pus)))
  x <- AttributeSpace(
    planning.unit.points = PlanningUnitPoints(
      pu_coords,
      seq_len(nrow(cs_pus))
    ),
    demand.points = make.DemandPoints(
      points = randomPoints(cs_spp[[1]], n = 100, prob = TRUE)
    ),
    species = 1L
  )
  # checks are internal
  suppressMessages(print(x))
  expect_true(methods::validObject(x, test = FALSE))
})
